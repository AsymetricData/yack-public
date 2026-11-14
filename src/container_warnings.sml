structure ContainerWarnings =
struct
  type input =
    { program : Ast.program
    , functions : (string * CheckTypes.funSig) list
    , readonlyParams : (Diag.span * string * Types.ty) list
    , ownsSpan : Diag.span -> bool
    }

  type funInfo =
    { name : string
    , nameSpan : Diag.span
    , params : Ast.param list
    , body : Ast.expr
    }

  fun prune ty = Types.prune ty

  fun containerKind ty =
    case prune ty of
      Types.TArray _ => SOME "array"
    | Types.TMap _ => SOME "map"
    | Types.TDataApp (Types.Data {name, property, ...}, _) =>
        if !property = Types.Affine andalso
           name <> "option" andalso name <> "result"
        then SOME (if name = "list" then "list" else name)
        else NONE
    | Types.TData (Types.Data {name, property, ...}) =>
        if !property = Types.Affine andalso
           name <> "option" andalso name <> "result"
        then SOME (if name = "list" then "list" else name)
        else NONE
    | _ => NONE

  fun sameShape (left, right) =
    case (prune left, prune right) of
      (Types.TArray _, Types.TArray _) => true
    | (Types.TMap _, Types.TMap _) => true
    | (Types.TDataApp (Types.Data {name = leftName, ...}, _),
       Types.TDataApp (Types.Data {name = rightName, ...}, _)) =>
        leftName = rightName
    | (Types.TData (Types.Data {name = leftName, ...}),
       Types.TData (Types.Data {name = rightName, ...})) =>
        leftName = rightName
    | _ => false

  fun containsShape shape ty =
    if sameShape (shape, ty) then true
    else
      case prune ty of
        Types.TTuple values => List.exists (containsShape shape) values
      | Types.TRecord fields =>
          List.exists (fn (_, fieldTy) => containsShape shape fieldTy) fields
      | Types.TCon {fields, ...} =>
          List.exists (fn (_, fieldTy) => containsShape shape fieldTy) fields
      | Types.TDataApp (_, arguments) =>
          List.exists (containsShape shape) arguments
      | Types.TConsume inner => containsShape shape inner
      | Types.TBorrow (inner, _, _) => containsShape shape inner
      | _ => false

  fun patternName pattern =
    case pattern of
      Ast.PVar (name, _) => name
    | Ast.PTuple (values, _) =>
        "(" ^ String.concatWith ", " (List.map patternName values) ^ ")"
    | Ast.PRecord (fields, _) =>
        "{" ^
        String.concatWith ", "
          (List.map (fn (name, value) => name ^ " = " ^ patternName value)
             fields) ^ "}"
    | Ast.PConstructor (name, NONE, _) => name
    | Ast.PConstructor (name, SOME value, _) =>
        name ^ " (" ^ patternName value ^ ")"
    | Ast.PString (_, SOME value, _) => "_" ^ patternName value
    | Ast.PBytes (_, SOME value, _) => "_" ^ patternName value
    | _ => "_"

  fun clauseParam (pattern, annot) =
    { name = patternName pattern
    , annot = annot
    , span = Ast.patternSpan pattern
    }

  fun infosOf program =
    let
      fun add (info, infos) =
        case List.find (fn (candidate : funInfo) =>
                          #name candidate = #name info) infos of
          NONE => info :: infos
        | SOME existing =>
            let
              val merged =
                { name = #name existing
                , nameSpan = #nameSpan existing
                , params = #params existing
                , body = #body existing
                }
            in

              merged :: List.filter
                (fn (candidate : funInfo) => #name candidate <> #name info)
                infos
            end
      fun loop (declarations, infos) =
        case declarations of
          [] => List.rev infos
        | Ast.DFun {name, nameSpan, params, body, ...} :: rest =>
            loop (rest,
              add ({ name = name
                   , nameSpan = nameSpan
                   , params = params
                   , body = body
                   }, infos))
        | Ast.DFunClause {name, nameSpan, params, body, ...} :: rest =>
            loop (rest,
              add ({ name = name
                   , nameSpan = nameSpan
                   , params = List.map clauseParam params
                   , body = body
                   }, infos))
        | _ :: rest => loop (rest, infos)
    in
      loop (program, [])
    end

  fun bodiesOf program name =
    let
      fun loop (declarations, bodies) =
        case declarations of
          [] => List.rev bodies
        | Ast.DFun {name = candidate, body, ...} :: rest =>
            if candidate = name then loop (rest, body :: bodies)
            else loop (rest, bodies)
        | Ast.DFunClause {name = candidate, body, ...} :: rest =>
            if candidate = name then loop (rest, body :: bodies)
            else loop (rest, bodies)
        | _ :: rest => loop (rest, bodies)
    in
      loop (program, [])
    end

  fun callsIn expression =
    case expression of
      Ast.ECall (name, arguments, _) =>
        name :: List.concat (List.map callsIn arguments)
    | Ast.EBin (_, left, right, _) =>
        callsIn left @ callsIn right
    | Ast.ENeg (operand, _) => callsIn operand
    | Ast.EIf (condition, thenBranch, elseBranch, _) =>
        callsIn condition @ callsIn thenBranch @ callsIn elseBranch
    | Ast.ELet (bindings, body, _) =>
        List.concat
          (List.map
             (fn Ast.BVal {value, ...} => callsIn value
               | Ast.BPat {value, ...} => callsIn value
               | Ast.BFun _ =>
                   raise Fail "container warnings: desugared local function"
               | Ast.BGuard _ =>
                   raise Fail "container warnings: desugared guard binding")
             bindings) @
        callsIn body
    | Ast.EArena (bindings, body, _) =>
        List.concat
          (List.map
             (fn Ast.BVal {value, ...} => callsIn value
               | Ast.BPat {value, ...} => callsIn value
               | Ast.BFun _ =>
                   raise Fail "container warnings: desugared local function"
               | Ast.BGuard _ =>
                   raise Fail "container warnings: desugared guard binding")
             bindings) @
        callsIn body
    | Ast.ERecord (fields, _) =>
        List.concat (List.map (fn (_, value) => callsIn value) fields)
    | Ast.EField (_, base, _) => callsIn base
    | Ast.ETuple (values, _) => List.concat (List.map callsIn values)
    | Ast.ETupleIndex (_, base, _) => callsIn base
    | Ast.ECase (scrutinee, branches, _) =>
        callsIn scrutinee @
        List.concat
          (List.map
             (fn (_, guard, body) =>
                (case guard of NONE => [] | SOME value => callsIn value) @
                callsIn body)
             branches)
    | Ast.EBorrow (_, _, operand, _) => callsIn operand
    | Ast.EFn (_, _, body, _) => callsIn body
    | _ => []

  fun mentions name expression =
    case expression of
      Ast.EVar (candidate, _) => candidate = name
    | Ast.ECall (_, arguments, _) => List.exists (mentions name) arguments
    | Ast.EBin (_, left, right, _) => mentions name left orelse mentions name right
    | Ast.ENeg (operand, _) => mentions name operand
    | Ast.EIf (condition, thenBranch, elseBranch, _) =>
        mentions name condition orelse mentions name thenBranch orelse
        mentions name elseBranch
    | Ast.ELet (bindings, body, _) =>
        List.exists
          (fn Ast.BVal {value, ...} => mentions name value
            | Ast.BPat {value, ...} => mentions name value
            | Ast.BFun _ =>
                raise Fail "container warnings: desugared local function"
            | Ast.BGuard _ =>
                raise Fail "container warnings: desugared guard binding")
          bindings orelse
        mentions name body
    | Ast.EArena (bindings, body, _) =>
        List.exists
          (fn Ast.BVal {value, ...} => mentions name value
            | Ast.BPat {value, ...} => mentions name value
            | Ast.BFun _ =>
                raise Fail "container warnings: desugared local function"
            | Ast.BGuard _ =>
                raise Fail "container warnings: desugared guard binding")
          bindings orelse
        mentions name body
    | Ast.ERecord (fields, _) =>
        List.exists (fn (_, value) => mentions name value) fields
    | Ast.EField (_, base, _) => mentions name base
    | Ast.ETuple (values, _) => List.exists (mentions name) values
    | Ast.ETupleIndex (_, base, _) => mentions name base
    | Ast.ECase (scrutinee, branches, _) =>
        mentions name scrutinee orelse
        List.exists
          (fn (_, guard, body) =>
             (case guard of NONE => false | SOME value => mentions name value) orelse
             mentions name body)
          branches
    | Ast.EBorrow (_, _, operand, _) => mentions name operand
    | Ast.EFn (_, _, body, _) => mentions name body
    | _ => false

  fun callsName name expression =
    List.exists (fn candidate => candidate = name) (callsIn expression)

  fun constructorNames ty =
    case prune ty of
      Types.TDataApp (Types.Data {constructors, ...}, _) =>
        List.map #1 (!constructors)
    | Types.TData (Types.Data {constructors, ...}) =>
        List.map #1 (!constructors)
    | _ => []

  fun rebuildsContainer (ty, body) =
    case prune ty of
      Types.TArray _ =>
        List.exists (fn name => callsName name body)
          ["array_new", "array_make", "array_make_uninit"]
    | Types.TMap _ => callsName "map_new" body
    | Types.TDataApp (Types.Data {name, ...}, _) =>
        if name = "list" then callsName "Cons" body
        else List.exists (fn constructor => callsName constructor body)
               (constructorNames ty)
    | Types.TData _ =>
        List.exists (fn constructor => callsName constructor body)
          (constructorNames ty)
    | _ => false

  fun check (input : input) =
    let
      val { program, functions, readonlyParams, ownsSpan } = input

      val readonlyWarnings =
        List.map
          (fn (span, name, ty) =>
             let
               val kind =
                 case containerKind ty of
                   SOME value => value
                 | NONE => "container"
             in
               Diag.warning
                 ( "W3102"
                 , "parameter `" ^ name ^ "` is an owned " ^ kind ^
                   " but is only read"
                 , span
                 , [ "the caller transfers ownership, but the body never consumes `" ^
                     name ^ "`"
                   ]
                 , [ "take `borrow " ^ name ^
                     "` so the caller keeps ownership and no move is needed"
                   , "then wrap the argument at each call site with `borrow (...)`"
                   ]
                 )
             end)
          (List.filter (fn (span, _, _) => ownsSpan span) readonlyParams)

      val infos = infosOf program
      val names = List.map #name infos

      fun callsOf name =
        List.concat (List.map callsIn (bodiesOf program name))

      val callGraph =
        List.map (fn name => (name, callsOf name)) names

      fun callsFrom name =
        case List.find (fn (candidate, _) => candidate = name) callGraph of
          SOME (_, calls) => calls
        | NONE => []

      fun isRecursive name =
        let
          val seen = StringSet.create 16
          fun go current =
            List.exists
              (fn callee =>
                 if callee = name then true
                 else if StringSet.member (seen, callee) then false
                 else
                   (ignore (StringSet.insert (seen, callee)); go callee))
              (callsFrom current)
        in
          go name
        end

      val recursiveNames = List.filter isRecursive names

      val hot =
        let
          val set = StringSet.create 32
          fun add name =
            if StringSet.insert (set, name) then
              List.app add (callsFrom name)
            else ()
          val _ = List.app add recursiveNames
        in
          set
        end

      fun callersOf name =
        List.map #1
          (List.filter (fn (_, calls) =>
                          List.exists (fn candidate => candidate = name) calls)
             callGraph)

      fun sigOf name =
        case List.find (fn (candidate, _) => candidate = name) functions of
          SOME (_, sigInfo) => SOME sigInfo
        | NONE => NONE

      fun copyWarnings () =
        List.concat
          (List.map
             (fn (info : funInfo) =>
                if not (StringSet.member (hot, #name info)) then []
                else if not (ownsSpan (#nameSpan info)) then []
                else
                  case sigOf (#name info) of
                    NONE => []
                  | SOME sigInfo =>
                      let
                        val bodies = bodiesOf program (#name info)
                        fun bodyMentions name =
                          List.exists (mentions name) bodies
                        fun bodyRebuilds ty =
                          List.exists (fn body => rebuildsContainer (ty, body))
                            bodies
                        val callers = callersOf (#name info)
                        val callersText =
                          case callers of
                            [] => "reachable from a recursive loop"
                          | _ =>
                              "called from " ^
                              String.concatWith ", "
                                (List.map (fn caller => "`" ^ caller ^ "`") callers) ^
                              ", reachable from a recursive loop"
                        fun warningFor (param, ty) =
                          case containerKind ty of
                            NONE => []
                          | SOME kind =>
                              if not (bodyMentions (#name param)) orelse
                                 not (bodyRebuilds ty) orelse
                                 not (containsShape ty (#result sigInfo))
                              then []
                              else
                                [ Diag.warning
                                    ( "W3101"
                                    , "`" ^ #name info ^ "` copies an owned " ^
                                      kind ^ " on every call"
                                    , #nameSpan info
                                    , [ "parameter `" ^ #name param ^
                                        "` is owned and the body rebuilds a " ^
                                        kind
                                      , callersText
                                      ]
                                    , [ "take `borrow " ^ #name param ^
                                        "`, thread the container, or use an array/ilist of Copy elements"
                                      ]
                                    )
                                ]
                      in
                        List.concat
                          (ListPair.map
                             (fn (param, ty) => warningFor (param, ty))
                             (#params info, #params sigInfo)
                             handle _ => [])
                      end)
             infos)
    in
      readonlyWarnings @ copyWarnings ()
    end
end
