structure LowerEscape =
struct
  open LowerBuiltins

datatype escapeBinding =
  EscapeBinding of string * escapeBinding list * Ir.expr

fun irPatternNames pattern =
  case pattern of
    Ir.PVar (name, _) => [name]
  | Ir.PWildcard => []
  | Ir.PBool _ => []
  | Ir.PInt _ => []
  | Ir.PString (_, rest, _) =>
      (case rest of
         SOME subpattern => irPatternNames subpattern
       | NONE => [])
  | Ir.PBytes (_, rest, _) =>
      (case rest of
         SOME subpattern => irPatternNames subpattern
       | NONE => [])
  | Ir.PTuple (patterns, _) =>
      List.concat (List.map irPatternNames patterns)
  | Ir.PRecord (fields, _) =>
      List.concat
        (List.map (fn (_, subpattern) =>
          irPatternNames subpattern) fields)
  | Ir.PCtor (_, _, argument, _) =>
      (case argument of
         SOME subpattern => irPatternNames subpattern
       | NONE => [])

fun lookupEscapeBinding (environment, name) =
  List.find
    (fn EscapeBinding (candidate, _, _) => candidate = name)
    environment

fun setSite (siteRef : Ir.allocSite ref) selected =
  case (selected, !siteRef) of
    (Ir.Destination, _) => siteRef := Ir.Destination
  | (_, Ir.Destination) => ()
  | _ => siteRef := selected

fun resultMayEscape ty =
  case ty of
    Ir.Int _ => false
  | Ir.Float _ => false
  | Ir.Bool => false
  | Ir.Unit => false
  | Ir.TToken => false
  | Ir.TResource => false
  | _ => true

fun hasPointers ty =
  case ty of
    Ir.Int _ => false
  | Ir.Float _ => false
  | Ir.Bool => false
  | Ir.Unit => false
  | Ir.TToken => false
  | Ir.TResource => false
  | Ir.TCPtr => false
  | Ir.TFun _ => false
  | Ir.TTuple types => List.exists hasPointers types
  | Ir.TRecord (fields, _) =>
      List.exists (fn (_, fieldTy) => hasPointers fieldTy) fields
  | _ => true

type paramInfo = { retained : bool, depends : bool }
type summary = string * paramInfo vector

val summaries = ref ([] : summary list)

fun summaryOf name =
  List.find (fn (candidate, _) => candidate = name) (!summaries)

fun exprType expression =
  case expression of
    Ir.IntLit (_, ty) => ty
  | Ir.FloatLit (_, ty) => ty
  | Ir.StringLit (_, ty) => ty
  | Ir.BoolLit _ => Ir.Bool
  | Ir.UnitLit => Ir.Unit
  | Ir.Var (_, ty, _) => ty
  | Ir.Bin (_, _, _, ty, _) => ty
  | Ir.Neg (_, ty) => ty
  | Ir.Call (_, _, ty, _) => ty
  | Ir.If (_, _, _, ty) => ty
  | Ir.Let (_, letBody, _) => exprType letBody
  | Ir.Tuple (_, ty) => ty
  | Ir.Record (_, ty) => ty
  | Ir.Field (_, _, ty) => ty
  | Ir.TupleIndex (_, _, ty) => ty
  | Ir.RawField (_, _, ty) => ty
  | Ir.Ctor (_, _, _, ty, _) => ty
  | Ir.Match (_, _, ty) => ty
  | Ir.PrintString _ => Ir.Unit
  | Ir.StringBuiltin (_, _, ty, _) => ty
  | Ir.BytesBuiltin (_, _, ty, _) => ty
  | Ir.SysBuiltin (_, _, ty) => ty
  | Ir.ArrayBuiltin (_, _, ty, _) => ty
  | Ir.MapBuiltin (_, _, ty, _) => ty
  | Ir.Copy (_, ty) => ty
  | Ir.Borrow (_, _, ty) => ty
  | Ir.Closure (_, ty) => ty
  | Ir.ClosureRef (_, ty) => ty
  | Ir.Apply (_, _, ty) => ty

fun summaryParam (name, index) =
  case summaryOf name of
    SOME (_, vector) =>
      if index < Vector.length vector then SOME (Vector.sub (vector, index))
      else NONE
  | NONE => NONE

fun calleeKeeps (name, index) =
  case summaryParam (name, index) of
    SOME info => #retained info orelse #depends info
  | NONE => true

fun calleeDepends (name, index) =
  case summaryParam (name, index) of
    SOME info => #depends info
  | NONE => true

val parameterCallees = ref ([] : (string * (string * string list) list) list)

fun parameterCalleesOf (functionName, parameter) =
  case List.find (fn (name, _) => name = functionName) (!parameterCallees) of
    SOME (_, entries) =>
      (case List.find (fn (candidate, _) => candidate = parameter) entries of
         SOME (_, names) => names
       | NONE => [])
  | NONE => []

fun unionNames (left, right) =
  List.foldl
    (fn (name, acc) =>
       if List.exists (fn candidate => candidate = name) acc
       then acc else name :: acc)
    left right

fun namedCallees (functionName, environment, expression) =
  case expression of
    Ir.Closure (name, _) => [name]
  | Ir.ClosureRef (name, _) => [name]
  | Ir.Var (variable, _, _) =>
      (case List.find (fn (name, _) => name = variable) environment of
         SOME (_, names) => names
       | NONE => parameterCalleesOf (functionName, variable))
  | Ir.If (_, thenBranch, elseBranch, _) =>
      unionNames
        ( namedCallees (functionName, environment, thenBranch)
        , namedCallees (functionName, environment, elseBranch)
        )
  | Ir.Let (bindings, letBody, _) =>
      let
        val extended =
          List.foldl
            (fn ((bound, _, value), environment) =>
               (bound, namedCallees (functionName, environment, value)) ::
               environment)
            environment
            bindings
      in
        namedCallees (functionName, extended, letBody)
      end
  | _ => []

fun computeParameterCallees functions =
  let
    fun initial ({name, params, ...} : Ir.funDef) =
      (name, List.map (fn (parameter, _) => (parameter, [])) params)
    fun scanFunction ({name, body, ...} : Ir.funDef) =
      let
        val collected = ref ([] : (string * int * string list) list)
        fun walk (expression, environment) =
          let
            fun walkAll expressions =
              List.app (fn value => walk (value, environment)) expressions
          in
            case expression of
              Ir.Call (callee, arguments, _, _) =>
                let
                  fun loop (index, remaining) =
                    case remaining of
                      [] => ()
                    | argument :: rest =>
                        let
                          val names =
                            namedCallees (name, environment, argument)
                        in
                          ( if List.null names then ()
                            else
                              collected :=
                                (callee, index, names) :: !collected
                          ; walk (argument, environment)
                          ; loop (index + 1, rest)
                          )
                        end
                in
                  loop (0, arguments)
                end
            | Ir.Apply (callee, arguments, _) =>
                ( walk (callee, environment)
                ; walkAll arguments
                )
            | Ir.Let (bindings, letBody, _) =>
                let
                  val extended =
                    List.foldl
                      (fn ((bound, _, value), environment) =>
                         (bound, namedCallees (name, environment, value)) ::
                         environment)
                      environment
                      bindings
                in
                  List.app
                    (fn (_, _, value) => walk (value, environment))
                    bindings
                ; walk (letBody, extended)
                end
            | Ir.If (condition, thenBranch, elseBranch, _) =>
                ( walk (condition, environment)
                ; walk (thenBranch, environment)
                ; walk (elseBranch, environment)
                )
            | Ir.Match (scrutinee, branches, _) =>
                ( walk (scrutinee, environment)
                ; List.app
                    (fn (_, guard, branchBody, _) =>
                       ( (case guard of
                            SOME condition => walk (condition, environment)
                          | NONE => ())
                       ; walk (branchBody, environment)
                       ))
                    branches
                )
            | Ir.Bin (_, left, right, _, _) =>
                ( walk (left, environment)
                ; walk (right, environment)
                )
            | Ir.Neg (operand, _) => walk (operand, environment)
            | Ir.Tuple (items, _) => walkAll items
            | Ir.Record (fields, _) =>
                List.app (fn (_, value) => walk (value, environment)) fields
            | Ir.Field (operand, _, _) => walk (operand, environment)
            | Ir.TupleIndex (operand, _, _) => walk (operand, environment)
            | Ir.RawField (operand, _, _) => walk (operand, environment)
            | Ir.Ctor (_, _, argument, _, _) =>
                (case argument of
                   SOME value => walk (value, environment)
                 | NONE => ())
            | Ir.PrintString (operand, _) => walk (operand, environment)
            | Ir.StringBuiltin (_, arguments, _, _) =>
                List.app (fn (value, _) => walk (value, environment)) arguments
            | Ir.BytesBuiltin (_, arguments, _, _) =>
                List.app (fn (value, _) => walk (value, environment)) arguments
            | Ir.SysBuiltin (_, arguments, _) => walkAll arguments
            | Ir.ArrayBuiltin (_, arguments, _, _) => walkAll arguments
            | Ir.MapBuiltin (_, arguments, _, _) =>
                List.app (fn (value, _) => walk (value, environment)) arguments
            | Ir.Copy (operand, _) => walk (operand, environment)
            | Ir.Borrow (_, operand, _) => walk (operand, environment)
            | _ => ()
          end
      in
        ( walk (body, [])
        ; !collected
        )
      end
    fun addAt (position, [], _, _) = []
      | addAt (position, (parameter, current) :: rest, index, names) =
          if position <> index then
            (parameter, current) :: addAt (position + 1, rest, index, names)
          else
            (parameter, unionNames (current, names)) :: rest
    fun addEntry ((callee, index, names), table) =
      List.map
        (fn (functionName, entries) =>
           if functionName <> callee then (functionName, entries)
           else (functionName, addAt (0, entries, index, names))
        )
        table
    fun equal (left, right) =
      List.length left = List.length right andalso
      ListPair.all
        (fn ((leftName, leftEntries), (rightName, rightEntries)) =>
           leftName = rightName andalso
           List.length leftEntries = List.length rightEntries andalso
           ListPair.all
             (fn ((leftParameter, leftNames),
                  (rightParameter, rightNames)) =>
                leftParameter = rightParameter andalso
                List.length leftNames = List.length rightNames andalso
                List.all
                  (fn name => List.exists (fn other => other = name) rightNames)
                  leftNames)
             (leftEntries, rightEntries))
        (left, right)
    fun loop (current, fuel) =
      let
        val _ = parameterCallees := current
        val collected = List.concat (List.map scanFunction functions)
        val next = List.foldl addEntry current collected
      in
        if fuel = 0 orelse equal (current, next)
        then next
        else loop (next, fuel - 1)
      end
  in
    loop (List.map initial functions, List.length functions + 4)
  end

fun paramRetained (name, index) =
  case summaryParam (name, index) of
    SOME info => #retained info
  | NONE => true

fun paramDepends (name, index) =
  case summaryParam (name, index) of
    SOME info => #depends info
  | NONE => true

fun applyKeeps (functionName, callee, index) =
  case callee of
    Ir.Closure (name, _) => calleeKeeps (name, index)
  | Ir.ClosureRef (name, _) => calleeKeeps (name, index)
  | Ir.Var (variable, _, _) =>
      let val names = parameterCalleesOf (functionName, variable) in
        if List.null names then true
        else List.exists (fn name => calleeKeeps (name, index)) names
      end
  | _ => true

fun analyzeFunction ({name, params, body, ...} : Ir.funDef) : summary =
  let
    fun analyzeParam parameter =
      let
        val retained = ref false
        fun carries (expression, environment) =
          let
            val value =
              case expression of
                Ir.Var (variable, _, _) => List.exists (fn name => name = variable) environment
              | Ir.Bin (_, left, right, _, _) =>
                  carries (left, environment) orelse carries (right, environment)
              | Ir.Neg (operand, _) => carries (operand, environment)
              | Ir.Field (operand, _, _) => carries (operand, environment)
              | Ir.TupleIndex (operand, _, _) => carries (operand, environment)
              | Ir.RawField (operand, _, _) => carries (operand, environment)
              | Ir.Copy (operand, _) => carries (operand, environment)
              | Ir.Borrow (_, operand, _) => carries (operand, environment)
              | Ir.Ctor (_, _, SOME argument, _, _) =>
                  carries (argument, environment)
              | Ir.Ctor (_, _, NONE, _, _) => false
              | Ir.Tuple (items, _) =>
                  List.exists (fn item => carries (item, environment)) items
              | Ir.Record (fields, _) =>
                  List.exists (fn (_, item) => carries (item, environment)) fields
              | Ir.If (_, thenBranch, elseBranch, _) =>
                  carries (thenBranch, environment) orelse
                  carries (elseBranch, environment)
              | Ir.Match (scrutinee, branches, _) =>
                  let
                    val scrutineeCarries = carries (scrutinee, environment)
                    fun branch (pattern, _, branchBody, _) =
                      carries
                        ( branchBody
                        , if scrutineeCarries
                          then irPatternNames pattern @ environment
                          else environment
                        )
                  in
                    List.exists branch branches
                  end
              | Ir.Let (bindings, letBody, _) =>
                  let
                    val extended =
                      List.foldl
                        (fn ((bound, _, value), environment) =>
                           if carries (value, environment)
                           then bound :: environment
                           else environment)
                        environment
                        bindings
                  in
                    carries (letBody, extended)
                  end
              | Ir.Call (callee, arguments, _, _) =>
                  if isBuiltinCall callee then
                    List.exists
                      (fn argument => carries (argument, environment))
                      arguments
                  else
                    let
                      val flags =
                        List.map (fn argument => carries (argument, environment))
                          arguments
                      val indexed =
                        ListPair.zip
                          (List.tabulate (List.length flags, fn i => i), flags)
                      fun keeps (index, flag) =
                        flag andalso
                        (case summaryParam (callee, index) of
                           SOME info => #retained info
                         | NONE => true)
                      fun dependsOn (index, flag) =
                        flag andalso
                        (case summaryParam (callee, index) of
                           SOME info => #depends info
                         | NONE => true)
                      val _ = if List.exists keeps indexed then retained := true
                              else ()
                    in
                      List.exists dependsOn indexed
                    end
              | Ir.Apply (callee, arguments, _) =>
                  let
                    val names = namedCallees (name, [], callee)
                    val flags =
                      List.map
                        (fn argument => carries (argument, environment))
                        arguments
                    val indexed =
                      ListPair.zip
                        (List.tabulate (List.length flags, fn i => i), flags)
                    fun keeps (index, flag) =
                      flag andalso
                      (if List.null names then true
                       else List.exists
                              (fn calleeName => paramRetained (calleeName, index))
                              names)
                    fun dependsOn (index, flag) =
                      flag andalso
                      (if List.null names then true
                       else List.exists
                              (fn calleeName => paramDepends (calleeName, index))
                              names)
                    val _ =
                      if List.exists keeps indexed then retained := true
                      else ()
                  in
                    List.exists dependsOn indexed
                  end
              | Ir.StringBuiltin (_, arguments, _, _) =>
                  List.exists
                    (fn (argument, _) => carries (argument, environment))
                    arguments
              | Ir.BytesBuiltin (_, arguments, _, _) =>
                  List.exists
                    (fn (argument, _) => carries (argument, environment))
                    arguments
              | Ir.SysBuiltin (_, arguments, _) =>
                  List.exists
                    (fn argument => carries (argument, environment))
                    arguments
              | Ir.ArrayBuiltin (builtin, arguments, _, _) =>
                  let
                    val stored =
                      case LowerBuiltins.arrayStoreIndex builtin of
                        SOME index =>
                          if index < List.length arguments
                          then SOME (List.nth (arguments, index))
                          else NONE
                      | NONE => NONE
                    val _ =
                      case stored of
                        SOME value =>
                          if carries (value, environment) then retained := true
                          else ()
                      | NONE => ()
                  in
                    List.exists
                      (fn argument => carries (argument, environment))
                      arguments
                  end
              | Ir.MapBuiltin (builtin, arguments, _, _) =>
                  let
                    val any =
                      List.exists
                        (fn (argument, _) => carries (argument, environment))
                        arguments
                    val _ =
                      if builtin = "map_set" andalso any then retained := true
                      else ()
                  in
                    any
                  end
              | Ir.PrintString (operand, _) => carries (operand, environment)
              | _ => false
          in
            hasPointers (exprType expression) andalso value
          end
      in
        { retained = !retained
        , depends = carries (body, [parameter])
        }
      end
  in
    (name, Vector.fromList (List.map analyzeParam (List.map #1 params)))
  end

fun summariesEqual (left, right) =
  List.length left = List.length right andalso
  ListPair.all
    (fn ((leftName, leftVector), (rightName, rightVector)) =>
       leftName = rightName andalso
       Vector.length leftVector = Vector.length rightVector andalso
       Vector.foldli
         (fn (index, leftInfo, same) =>
            same andalso
            let val rightInfo = Vector.sub (rightVector, index) in
              #retained leftInfo = #retained rightInfo andalso
              #depends leftInfo = #depends rightInfo
            end)
         true leftVector)
    (left, right)

fun computeSummaries functions =
  let
    fun initial ({name, params, ...} : Ir.funDef) =
      ( name
      , Vector.tabulate
          (List.length params, fn _ => { retained = false, depends = false })
      )
    fun loop (current, fuel) =
      let
        val _ = summaries := current
        val next = List.map analyzeFunction functions
      in
        if fuel = 0 orelse summariesEqual (current, next)
        then next
        else loop (next, fuel - 1)
      end
  in
    loop (List.map initial functions, List.length functions + 4)
  end

val currentEscapeFunction = ref ""

fun escapeCallees (environment, expression) =
  case expression of
    Ir.Closure (name, _) => [name]
  | Ir.ClosureRef (name, _) => [name]
  | Ir.Var (variable, _, _) =>
      (case lookupEscapeBinding (environment, variable) of
         SOME (EscapeBinding (_, definitionEnvironment, definition)) =>
           escapeCallees (definitionEnvironment, definition)
       | NONE => parameterCalleesOf (!currentEscapeFunction, variable))
  | Ir.If (_, thenBranch, elseBranch, _) =>
      unionNames
        ( escapeCallees (environment, thenBranch)
        , escapeCallees (environment, elseBranch)
        )
  | _ => []

fun markSite site markCalls environment expression =
  case expression of
    Ir.Ctor (_, _, argument, _, siteRef) =>
      ( (case site of
           SOME selected => setSite siteRef selected
         | NONE => ())
      ; case argument of
          SOME value => markSite site markCalls environment value
        | NONE => ()
      )
  | Ir.Var (name, ty, _) =>

      if not (hasPointers ty) then ()
      else
        (case lookupEscapeBinding (environment, name) of
           SOME (EscapeBinding (_, definitionEnvironment, definition)) =>
             markSite site markCalls definitionEnvironment definition
         | NONE => ())
  | Ir.Call (name, arguments, resultTy, escapes) =>
      if isBuiltinCall name then
        List.app (markSite NONE false environment) arguments
      else
        let
          val _ = if markCalls then escapes := true else ()

          fun argumentSite index =
            if not markCalls then SOME Ir.Current
            else if calleeKeeps (name, index) then SOME Ir.Destination
            else SOME Ir.Current
          fun loop (index, remaining) =
            case remaining of
              [] => ()
            | argument :: rest =>
                ( markSite (argumentSite index) markCalls environment argument
                ; loop (index + 1, rest)
                )
        in
          loop (0, arguments)
        end
  | Ir.StringBuiltin (name, arguments, _, siteRef) =>
      ( if LowerBuiltins.stringProduces name then
          setSite siteRef
            (if LowerBuiltins.stringProducesEscaping name then
               Ir.Destination
             else
               case site of
                 SOME selected => selected
               | NONE => Ir.Current)
        else ()
      ; List.app
          (fn (argument, _) => markSite NONE false environment argument)
          arguments
      )
  | Ir.BytesBuiltin (name, arguments, _, siteRef) =>
      ( if LowerBuiltins.bytesProducesString name then
          setSite siteRef
            (case site of
               SOME selected => selected
             | NONE => Ir.Current)
        else ()
      ; List.app
          (fn (argument, _) => markSite NONE false environment argument)
          arguments
      )
  | Ir.SysBuiltin (_, arguments, _) =>
      List.app (markSite NONE false environment) arguments
  | Ir.MapBuiltin (name, arguments, _, siteRef) =>
      let
        val allocates = (name = "map_new" orelse name = "map_keys")
        val _ =
          if allocates then
            setSite siteRef
              (case site of
                 SOME selected => selected
               | NONE => Ir.Current)
          else ()
        fun markArgument (index, (argument, _)) =

          if name = "map_set" andalso (index = 1 orelse index = 2) then
            markSite (SOME Ir.Destination) true environment argument
          else
            markSite NONE false environment argument
        fun loop (index, remaining) =
          case remaining of
            [] => ()
          | argument :: rest =>
              ( markArgument (index, argument)
              ; loop (index + 1, rest)
              )
      in
        loop (0, arguments)
      end
  | Ir.ArrayBuiltin (name, arguments, _, siteRef) =>
      let
        val allocates =
          case LowerBuiltins.arrayColor name of
            SOME LowerBuiltins.ArrayAlloc => true
          | SOME LowerBuiltins.ArrayAllocRooted => true
          | SOME LowerBuiltins.ArrayAllocUninit => true
          | _ => false
        val _ =
          if allocates then
            setSite siteRef
              (case site of
                 SOME selected => selected
               | NONE => Ir.Current)
          else ()
        val escapedStore =
          if LowerBuiltins.arrayEscapesStoredValue name then
            LowerBuiltins.arrayStoreIndex name
          else
            NONE
        fun markArgument (index, argument) =
          if escapedStore = SOME index then
            let
              val storeSite =
                if name = "array_make" then
                  (case site of
                     SOME selected => SOME selected
                   | NONE => SOME Ir.Current)
                else SOME Ir.Destination
            in
              markSite storeSite true environment argument
            end
          else
            markSite NONE false environment argument
        fun loop (index, remaining) =
          case remaining of
            [] => ()
          | argument :: rest =>
              ( markArgument (index, argument)
              ; loop (index + 1, rest)
              )
      in
        loop (0, arguments)
      end
  | Ir.Tuple (values, _) =>
      List.app (markSite site markCalls environment) values
  | Ir.Record (fields, _) =>
      List.app
        (fn (_, value) => markSite site markCalls environment value)
        fields
  | Ir.Field (base, _, ty) =>
      if not (hasPointers ty) then () else markSite site markCalls environment base
  | Ir.TupleIndex (base, _, ty) =>
      if not (hasPointers ty) then () else markSite site markCalls environment base
  | Ir.Bin (operator, left, right, ty, siteRef) =>
      if operator = "^" then
        ( setSite siteRef
            (case site of
               SOME selected => selected
             | NONE => Ir.Current)
        ; markSite NONE false environment left
        ; markSite NONE false environment right
        )
      else if not (hasPointers ty) then

        ( markSite NONE false environment left
        ; markSite NONE false environment right
        )
      else
        ( markSite site markCalls environment left
        ; markSite site markCalls environment right
        )
  | Ir.Neg (operand, _) =>
      markSite site markCalls environment operand
  | Ir.If (condition, thenBranch, elseBranch, _) =>
      ( markSite (SOME Ir.Current) false environment condition
      ; markSite site markCalls environment thenBranch
      ; markSite site markCalls environment elseBranch
      )
  | Ir.Let (bindings, body, _) =>
      let
        fun extend (binding, environment) =
          case binding of
            (name, _, value) =>
              let
                val _ = markSite NONE false environment value
              in
                EscapeBinding (name, environment, value) :: environment
              end
        val extended = List.foldl extend environment bindings
      in
        markSite site markCalls extended body
      end
  | Ir.Match (scrutinee, branches, ty) =>
      let

        val carries = hasPointers ty
        val innerSite = if carries then site else NONE
        val innerMark = carries andalso markCalls
      in
        markSite innerSite innerMark environment scrutinee;
        List.app
          (fn (pattern, guard, body, _) =>
            let
              fun addName (name, environment) =
                EscapeBinding (name, environment, scrutinee) ::
                environment
              val extended =
                List.foldl addName environment (irPatternNames pattern)
              val _ =
                case guard of
                  NONE => ()
                | SOME condition =>
                    markSite NONE false extended condition
            in
              markSite innerSite innerMark extended body
            end)
          branches
      end
  | Ir.Borrow (_, operand, _) =>
      markSite site markCalls environment operand
  | Ir.Copy (operand, _) =>
      markSite site markCalls environment operand
  | Ir.Closure _ => ()
  | Ir.ClosureRef _ => ()
  | Ir.Apply (callee, arguments, _) =>

      ( markSite (SOME Ir.Current) false environment callee
      ; let
          val names = escapeCallees (environment, callee)
          fun argumentSite index =
            if List.null names then SOME Ir.Destination
            else if List.exists
                      (fn name => calleeKeeps (name, index)) names
            then SOME Ir.Destination
            else SOME Ir.Current
          fun loop (index, remaining) =
            case remaining of
              [] => ()
            | argument :: rest =>
                ( markSite (argumentSite index) true environment argument
                ; loop (index + 1, rest)
                )
        in
          loop (0, arguments)
        end
      )
  | _ => ()

fun forceEscaping environment expression =
  markSite (SOME Ir.Destination) true environment expression

fun markCurrent environment expression =
  markSite (SOME Ir.Current) false environment expression

fun traverse environment expression =
  markSite NONE false environment expression

fun markFunction ({name, result, body, ...} : Ir.funDef) =
  let val _ = currentEscapeFunction := name in
  case result of
    Ir.Int _ => traverse [] body
  | Ir.Float _ => traverse [] body
  | Ir.Bool => traverse [] body
  | Ir.Unit => traverse [] body
  | Ir.TToken => traverse [] body
  | Ir.TResource => traverse [] body
  | _ => forceEscaping [] body
  end

fun markTailCalls functionName environment expression =
  case expression of
    Ir.Call (name, arguments, _, escapes) =>
      if name = functionName then
        ( escapes := true
        ; List.app
            (markSite (SOME Ir.Destination) true environment)
            arguments
        )
      else ()
  | Ir.Let (bindings, body, _) =>
      let
        fun extend (binding, environment) =
          case binding of
            (name, _, value) =>
              EscapeBinding (name, environment, value) :: environment
        val extended = List.foldl extend environment bindings
      in
        markTailCalls functionName extended body
      end
  | Ir.If (_, thenBranch, elseBranch, _) =>
      ( markTailCalls functionName environment thenBranch
      ; markTailCalls functionName environment elseBranch
      )
  | Ir.Match (scrutinee, branches, _) =>
      List.app
        (fn (pattern, _, body, _) =>
           let
             fun addName (name, environment) =
               EscapeBinding (name, environment, scrutinee) ::
               environment
             val extended =
               List.foldl addName environment (irPatternNames pattern)
           in
             markTailCalls functionName extended body
           end)
        branches
  | _ => ()

fun markFunctionTailCalls ({name, body, ...} : Ir.funDef) =
  markTailCalls name [] body

fun annotate functions =
  ( parameterCallees := computeParameterCallees functions
  ; summaries := computeSummaries functions
  ; List.app markFunction functions
  ; List.app markFunctionTailCalls functions)

end
