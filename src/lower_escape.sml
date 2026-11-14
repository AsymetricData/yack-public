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
  | Ir.Var (name, _, _) =>
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
          val escapingCall = markCalls andalso resultMayEscape resultTy
          val argumentSite =
            if escapingCall then SOME Ir.Destination
            else SOME Ir.Current
        in
          List.app
            (markSite argumentSite escapingCall environment)
            arguments
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
          if name = "map_set" andalso index = 2 then
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
  | Ir.Field (base, _, _) =>
      markSite site markCalls environment base
  | Ir.TupleIndex (base, _, _) =>
      markSite site markCalls environment base
  | Ir.Bin (operator, left, right, _, siteRef) =>
      if operator = "^" then
        ( setSite siteRef
            (case site of
               SOME selected => selected
             | NONE => Ir.Current)
        ; markSite NONE false environment left
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
  | Ir.Match (scrutinee, branches, _) =>
      ( markSite site markCalls environment scrutinee
      ; List.app
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
              markSite site markCalls extended body
            end)
          branches
      )
  | Ir.Borrow (_, operand, _) =>
      markSite site markCalls environment operand
  | Ir.Copy (operand, _) =>
      markSite site markCalls environment operand
  | Ir.Closure _ => ()
  | Ir.ClosureRef _ => ()
  | Ir.Apply (callee, arguments, _) =>

      ( markSite (SOME Ir.Current) false environment callee
      ; List.app
          (markSite (SOME Ir.Destination) true environment)
          arguments
      )
  | _ => ()

fun forceEscaping environment expression =
  markSite (SOME Ir.Destination) true environment expression

fun markCurrent environment expression =
  markSite (SOME Ir.Current) false environment expression

fun traverse environment expression =
  markSite NONE false environment expression

fun markFunction ({result, body, ...} : Ir.funDef) =
  case result of
    Ir.Int _ => traverse [] body
  | Ir.Float _ => traverse [] body
  | Ir.Bool => traverse [] body
  | Ir.Unit => traverse [] body
  | Ir.TToken => traverse [] body
  | Ir.TResource => traverse [] body
  | _ => forceEscaping [] body

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
  (List.app markFunction functions
  ; List.app markFunctionTailCalls functions)

end
