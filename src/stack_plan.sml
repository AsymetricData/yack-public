structure StackPlan =
struct
  type info =
    { region : bool
    , dest : bool
    , apply : bool
    , calls : (string * bool) list
    }

  type plan =
    { needsRegion : string -> bool
    }

  fun analyze (functions : Ir.funDef list) =
    let
      fun scanExpression expression =
        let
          val region = ref false
          val dest = ref false
          val apply = ref false
          val calls = ref []

          fun walk expression =
            case expression of
              Ir.IntLit _ => ()
            | Ir.FloatLit _ => ()
            | Ir.StringLit _ => ()
            | Ir.BoolLit _ => ()
            | Ir.UnitLit => ()
            | Ir.Var _ => ()
            | Ir.Bin (operator, left, right, _, site) =>
                ( (if operator = "^" then
                     (case !site of
                        Ir.Current => region := true
                      | Ir.Destination => dest := true
                      | Ir.NativeStack => dest := true)
                   else ())
                ; walk left
                ; walk right
                )
            | Ir.Neg (operand, _) => walk operand
            | Ir.Call (name, arguments, _, escapes) =>
                ( calls := (name, !escapes) :: !calls
                ; List.app walk arguments
                )
            | Ir.If (condition, thenBranch, elseBranch, _) =>
                ( walk condition
                ; walk thenBranch
                ; walk elseBranch
                )
            | Ir.Let (bindings, body, _) =>
                ( List.app (fn (_, _, value) => walk value) bindings
                ; walk body
                )
            | Ir.Tuple (values, _) => List.app walk values
            | Ir.Record (fields, _) =>
                List.app (fn (_, value) => walk value) fields
            | Ir.Field (base, _, _) => walk base
            | Ir.TupleIndex (base, _, _) => walk base
            | Ir.RawField (base, _, _) => walk base
            | Ir.Ctor (_, _, argument, _, site) =>
                ( (case !site of
                     Ir.Current => region := true
                   | Ir.Destination => dest := true
                   | Ir.NativeStack => ()  )
                ; (case argument of
                     NONE => ()
                   | SOME value => walk value)
                )
            | Ir.Match (scrutinee, branches, _) =>
                ( walk scrutinee
                ; List.app
                    (fn (_, guard, body, _) =>
                       ( (case guard of
                            NONE => ()
                          | SOME value => walk value)
                       ; walk body
                       ))
                    branches
                )
            | Ir.PrintString (value, _) => walk value
            | Ir.StringBuiltin (name, arguments, _, site) =>
                ( (if LowerBuiltins.stringNeedsRegion name then
                     (case !site of
                        Ir.Current => region := true
                      | Ir.Destination => dest := true
                      | Ir.NativeStack => dest := true)
                   else ())
                ;

                  if LowerBuiltins.isConversionBuiltin name andalso
                     not (LowerBuiltins.stringProduces name)
                  then dest := true
                  else ()
                ; List.app (fn (value, _) => walk value) arguments
                )
            | Ir.BytesBuiltin (name, arguments, _, site) =>
                ( if LowerBuiltins.bytesProducesString name then
                    (case !site of
                       Ir.Current => region := true
                     | Ir.Destination => dest := true
                     | Ir.NativeStack => dest := true)
                  else ()
                ; List.app (fn (value, _) => walk value) arguments
                )
            | Ir.SysBuiltin (name, arguments, _) =>
                (

                  if String.isPrefix "extern:" name then region := true else ()
                ; if name = "sys.scratch_enter" orelse
                     name = "sys.scratch_enter_all" orelse
                     name = "sys.scratch_leave"
                  then region := true
                  else ()
                ; List.app walk arguments
                )
            | Ir.ArrayBuiltin (name, arguments, _, site) =>
                ( (case LowerBuiltins.arrayColor name of
                     SOME LowerBuiltins.ArrayAlloc =>
                       (case !site of
                          Ir.Current => region := true
                        | Ir.Destination => dest := true
                        | Ir.NativeStack => dest := true)
                   | SOME LowerBuiltins.ArrayAllocRooted =>
                       (case !site of
                          Ir.Current => region := true
                        | Ir.Destination => dest := true
                        | Ir.NativeStack => dest := true)
                   | SOME LowerBuiltins.ArrayAllocUninit =>
                       (case !site of
                          Ir.Current => region := true
                        | Ir.Destination => dest := true
                        | Ir.NativeStack => dest := true)
                   | _ => ())
                ; List.app walk arguments
                )
            | Ir.MapBuiltin (name, arguments, _, site) =>
                ( if name = "map_new" orelse name = "map_keys" then
                    (case !site of
                       Ir.Current => region := true
                     | Ir.Destination => dest := true
                     | Ir.NativeStack => dest := true)
                  else ()
                ; List.app (fn (value, _) => walk value) arguments
                )
            | Ir.Copy (value, _) => walk value
            | Ir.Borrow (_, operand, _) => walk operand
            | Ir.Closure _ => ()
            | Ir.ClosureRef _ => ()
            | Ir.Apply (callee, arguments, _) =>
                (

                  apply := true
                ; region := true
                ; walk callee
                ; List.app walk arguments
                )
        in
          walk expression;
          { region = !region
          , dest = !dest
          , apply = !apply
          , calls = !calls
          }
        end

      val infos =
        List.map
          (fn ({ name, body, ... } : Ir.funDef) =>
             (name, scanExpression body))
          functions

      fun findInfo name =
        List.find (fn (candidate, _) => candidate = name) infos

      val writesDest = ref (StringSet.create 32)
      fun destWritten name = StringSet.member (!writesDest, name)

      fun addDest name = StringSet.insert (!writesDest, name)

      fun propagateDest () =
        let
          val changed = ref false
          fun consider (name, { dest, apply, calls, ... } : info) =
            let
              val escapes =
                List.exists
                  (fn (callee, escaping) =>
                     escaping andalso destWritten callee)
                  calls
            in
              if dest orelse apply orelse escapes then
                if addDest name then changed := true else ()
              else ()
            end
        in
          List.app consider infos;
          if !changed then propagateDest () else ()
        end

      val _ = propagateDest ()

      fun destinationWriters () =
        let
          val set = StringSet.create 32
          val _ =
            List.app
              (fn (name, _) =>
                 if destWritten name then
                   ignore (StringSet.insert (set, name))
                 else ())
              infos
        in
          set
        end

      val writers = destinationWriters ()

      fun needs name =
        case findInfo name of
          NONE => false
          | SOME (_, { region, calls, ... }) =>
              region orelse
              List.exists
                (fn (callee, escaping) =>
                   not escaping andalso StringSet.member (writers, callee))
                calls
    in
      { needsRegion = needs }
    end

  fun needsRegion ({ needsRegion, ... } : plan) name = needsRegion name
end
