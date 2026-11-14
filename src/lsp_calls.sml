structure LspCalls =
struct
  open LspProtocol
  open LspText
  open LspState

  val functionKind = 12

  type call =
    { caller : string
    , callee : string
    , span : Diag.span
    }

  fun declarationSpan declaration =
    case declaration of
      Ast.DFun {span, ...} => span
    | Ast.DFunClause {span, ...} => span
    | _ => Ast.declSpan declaration

  fun selectionSpan declaration =
    case declaration of
      Ast.DFun {nameSpan, ...} => nameSpan
    | Ast.DFunClause {nameSpan, ...} => nameSpan
    | _ => Ast.declSpan declaration

  fun isLocalName name = String.isPrefix "__yack_local_" name

  fun functionsIn program =
    List.mapPartial
      (fn declaration =>
        case declaration of
          Ast.DFun {name, ...} =>
            if isLocalName name then NONE else SOME (name, declaration)
        | Ast.DFunClause {name, ...} =>
            if isLocalName name then NONE else SOME (name, declaration)
        | _ => NONE)
      program

  fun findFunction checked name =
    let
      val functions = functionsIn (#program checked)
    in
      case List.find (fn (candidate, _) => candidate = name) functions of
        SOME found => SOME found
      | NONE =>
          let
            val short = LspRename.sourceName name
          in
            List.find
              (fn (candidate, _) =>
                LspRename.sourceName candidate = short)
              functions
          end
    end

  fun entryOfSpan checked span =
    entryForFile (#sources checked) (Pos.file (#start span))

  fun itemJson state checked analysisUri (name, declaration) =
    case entryOfSpan checked (declarationSpan declaration) of
      NONE => NONE
      | SOME entry =>
          SOME
            (obj
               [ ("name", str (LspRename.sourceName name))
               , ("kind", int functionKind)
               , ("uri", str (uriOfPath state (#path entry)))
               , ("range", lspRange entry (declarationSpan declaration))
               , ("selectionRange", lspRange entry (selectionSpan declaration))
               , ("data",
                   obj
                     [ ("name", str name)
                     , ("analysis", str analysisUri)
                     ])
               ])

  fun callsOfExpression expression =
    let
      fun walk expression =
        case expression of
          Ast.ECall (name, arguments, span) =>
            (name, span) :: List.concat (List.map walk arguments)
        | Ast.ELet (bindings, body, _) =>
            List.concat
              (List.map
                 (fn binding =>
                   case binding of
                     Ast.BVal {value, ...} => walk value
                   | Ast.BPat {value, ...} => walk value
                   | Ast.BFun {body, ...} => walk body
                   | Ast.BGuard {value, fallback, ...} =>
                       walk value @ walk fallback)
                 bindings
               @ [walk body])
        | Ast.EArena (bindings, body, _) =>
            List.concat
              (List.map
                 (fn binding =>
                   case binding of
                     Ast.BVal {value, ...} => walk value
                   | Ast.BPat {value, ...} => walk value
                   | Ast.BFun {body, ...} => walk body
                   | Ast.BGuard {value, fallback, ...} =>
                       walk value @ walk fallback)
                 bindings
               @ [walk body])
        | Ast.ECase (scrutinee, branches, _) =>
            walk scrutinee @
            List.concat
              (List.map
                 (fn (_, guard, body) =>
                   (case guard of SOME condition => walk condition | NONE => [])
                   @ walk body)
                 branches)
        | Ast.EIf (condition, thenBranch, elseBranch, _) =>
            walk condition @ walk thenBranch @ walk elseBranch
        | Ast.EBin (_, left, right, _) => walk left @ walk right
        | Ast.ENeg (operand, _) => walk operand
        | Ast.ERecord (fields, _) => List.concat (List.map (fn (_, value) => walk value) fields)
        | Ast.EField (_, base, _) => walk base
        | Ast.ETuple (values, _) => List.concat (List.map walk values)
        | Ast.ETupleIndex (_, base, _) => walk base
        | Ast.EBorrow (_, _, operand, _) => walk operand
        | Ast.EFn (_, _, body, _) => walk body
        | _ => []
    in
      walk expression
    end

  fun callsIn program =
    List.concat
      (List.map
         (fn (name, declaration) =>
           let
             val body =
               case declaration of
                 Ast.DFun {body, ...} => body
               | Ast.DFunClause {body, ...} => body
               | _ => Ast.EUnit {start = Pos.start, stop = Pos.start}
           in
             List.map
               (fn (callee, span) => {caller = name, callee = callee, span = span})
               (callsOfExpression body)
           end)
         (functionsIn program))

  fun nameOfItem item =
    let
      val fallback = getOpt (Json.stringField "name" item, "")
    in
      case Json.field "data" item of
        SOME data =>
          (case Json.stringField "name" data of
             SOME name => name
           | NONE => fallback)
      | NONE => fallback
    end

  fun analysisUriOf item =
    let
      val fallback = getOpt (Json.stringField "uri" item, "")
    in
      case Json.field "data" item of
        SOME data =>
          (case Json.stringField "analysis" data of
             SOME uri => uri
           | NONE => fallback)
      | NONE => fallback
    end

  fun prepareCallHierarchy state uri line character =
    case checkedOf state uri of
      NONE => jsonNull
    | SOME checked =>
        let
          val path = uriToPath uri
          val fileId = fileIdOf checked path
        in
          case entryForFile (#sources checked) fileId of
            NONE => jsonNull
          | SOME entry =>
              let
                val cursor = LspRename.cursorOffset entry line character
                val fromBinding =
                  case LspRename.bindingAt (checked, entry, fileId, cursor) of
                    SOME binding => SOME (#name binding)
                  | NONE => NONE
                val fromDeclaration =
                  case fromBinding of
                    SOME _ => NONE
                  | NONE =>
                      List.find
                        (fn (_, declaration) =>
                          let
                            val span = declarationSpan declaration
                          in
                            Pos.offset (#start span) <= cursor andalso
                            cursor <= Pos.offset (#stop span)
                          end)
                        (functionsIn (#program checked))
              in
                case (fromBinding, fromDeclaration) of
                  (SOME name, _) =>
                    (case findFunction checked name of
                       SOME functionEntry =>
                         (case itemJson state checked uri functionEntry of
                            SOME item => arr [item]
                          | NONE => jsonNull)
                     | NONE => jsonNull)
                | (NONE, SOME functionEntry) =>
                    (case itemJson state checked uri functionEntry of
                       SOME item => arr [item]
                     | NONE => jsonNull)
                | _ => jsonNull
              end
        end

  fun groupCalls (keyOf, calls) =
    let
      fun add (call, groups) =
        let
          val key = keyOf call
          fun insert [] = [(key, [call])]
            | insert ((candidate, items) :: rest) =
                if candidate = key then (candidate, call :: items) :: rest
                else (candidate, items) :: insert rest
        in
          insert groups
        end
    in
      List.foldl add [] calls
    end

  fun incomingCalls state item =
    case Json.field "item" item of
      NONE => arr []
    | SOME item =>
        let val analysisUri = analysisUriOf item in
          case checkedOf state analysisUri of
            NONE => arr []
          | SOME checked =>
              let
                val target = nameOfItem item
                val calls =
                  List.filter
                    (fn call => #callee call = target)
                    (callsIn (#program checked))
                val groups = groupCalls (fn call => #caller call, calls)
                fun callerItem (caller, fromCalls) =
                  case List.find (fn (name, _) => name = caller)
                         (functionsIn (#program checked)) of
                    NONE => NONE
                  | SOME functionEntry =>
                      (case itemJson state checked analysisUri functionEntry of
                         NONE => NONE
                       | SOME from =>
                           let
                             val ranges =
                               List.mapPartial
                                 (fn call =>
                                   case entryOfSpan checked (#span call) of
                                     NONE => NONE
                                   | SOME entry =>
                                       SOME (lspRange entry (#span call)))
                                 fromCalls
                           in
                             SOME
                               (obj
                                  [ ("from", from)
                                  , ("fromRanges", arr ranges)
                                  ])
                           end)
              in
                arr (List.mapPartial callerItem groups)
              end
        end

  fun outgoingCalls state item =
    case Json.field "item" item of
      NONE => arr []
    | SOME item =>
        let val analysisUri = analysisUriOf item in
          case checkedOf state analysisUri of
            NONE => arr []
          | SOME checked =>
              let
                val target = nameOfItem item
                val calls =
                  List.filter
                    (fn call => #caller call = target)
                    (callsIn (#program checked))
                val groups = groupCalls (fn call => #callee call, calls)
                fun calleeItem (callee, fromCalls) =
                  case findFunction checked callee of
                    NONE => NONE
                  | SOME functionEntry =>
                      (case itemJson state checked analysisUri functionEntry of
                         NONE => NONE
                       | SOME to =>
                           let
                             val ranges =
                               List.mapPartial
                                 (fn call =>
                                   case entryOfSpan checked (#span call) of
                                     NONE => NONE
                                   | SOME entry =>
                                       SOME (lspRange entry (#span call)))
                                 fromCalls
                           in
                             SOME
                               (obj
                                  [ ("to", to)
                                  , ("fromRanges", arr ranges)
                                  ])
                           end)
              in
                arr (List.mapPartial calleeItem groups)
              end
        end
end
