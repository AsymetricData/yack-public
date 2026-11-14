structure Desugar =
struct
  val arenaCounter = ref 0
  val arenaSpanCounter = ref 0

  fun freshArenaName prefix =
    let
      val index = !arenaCounter
      val _ = arenaCounter := index + 1
    in
      prefix ^ Int.toString index
    end

  fun desugarProgram program =
    let
      fun takeClauses name acc
            ((clause as Ast.DFunClause {name = candidate, ...}) :: rest) =
            if candidate = name then takeClauses name (clause :: acc) rest
            else (List.rev acc, clause :: rest)
        | takeClauses _ acc rest = (List.rev acc, rest)
      fun loop [] = []
        | loop ((first as Ast.DFunClause {name, params, ...}) :: rest) =
            if List.null params then
              desugarClauses name [first] :: loop rest
            else
              let
                val (clauses, remaining) = takeClauses name [first] rest
              in
                desugarClauses name clauses :: loop remaining
              end
        | loop (declaration :: rest) =
            desugarDeclaration declaration :: loop rest
    in
      loop program
    end

  and desugarClauses name clauses =
    let
      fun clauseParts (Ast.DFunClause {nameSpan, params, result, body, span, ...}) =
        (params, result, desugarExpr body, span, nameSpan)
        | clauseParts _ = raise Fail "desugar: expected a function clause"
      val recursive =
        List.exists
          (fn Ast.DFunClause {recursive, ...} => recursive
            | _ => raise Fail "desugar: expected a function clause")
          clauses
      val parts = List.map clauseParts clauses
      val (firstParams, _, _, firstSpan, firstNameSpan) = List.hd parts
      val arity = List.length firstParams
      val direct =
        arity = 0 orelse
        (List.length parts = 1 andalso
         List.all (fn (Ast.PVar _, _) => true | _ => false) firstParams)
      fun resultOf [] = NONE
        | resultOf ((_, result, _, _, _) :: rest) =
            (case result of SOME _ => result | NONE => resultOf rest)
      val result = resultOf parts
      fun annotationAt index params =
        case List.nth (params, index) handle _ => (Ast.PWildcard firstSpan, NONE) of
          (_, SOME ty) => SOME ty
        | (_, NONE) => NONE

      fun specificity pattern =
        case pattern of
          Ast.PWildcard _ => 0
        | Ast.PVar _ => 0
        | Ast.PInt _ => 1
        | Ast.PBool _ => 1
        | Ast.PString (_, NONE, _) => 1
        | Ast.PString (_, SOME rest, _) => 1 + specificity rest
        | Ast.PBytes (_, NONE, _) => 1
        | Ast.PBytes (_, SOME rest, _) => 1 + specificity rest
        | Ast.PConstructor (_, NONE, _) => 1
        | Ast.PConstructor (_, SOME argument, _) =>
            1 + specificity argument
        | Ast.PTuple (patterns, _) =>
            List.foldl
              (fn (pattern, total) => total + specificity pattern)
              0
              patterns
        | Ast.PRecord (fields, _) =>
            List.foldl
              (fn ((_, pattern), total) => total + specificity pattern)
              0
              fields
      fun shifted (position, delta, index) =
        { file = #file position
        , line = #line position
        , column = #column position
        , offset = #offset position + delta + index
        }
      fun bindingSpan index =
        let val start = shifted (#start firstSpan, 100000, index)
        in {start = start, stop = start} end
      fun useSpan index =
        let val start = shifted (#start firstSpan, 200000, index)
        in {start = start, stop = start} end
      fun syntheticSpanAt (base, delta, index) =
        let val start = shifted (#start base, delta, index)
        in {start = start, stop = start} end
      fun syntheticSpan delta index =
        syntheticSpanAt (firstSpan, delta, index)
    in
      if direct then
        let
          val (params, _, body, span, nameSpan) = List.hd parts
          val params' =
            List.map
              (fn (Ast.PVar (paramName, paramSpan), annot) =>
                    {name = paramName, annot = annot, span = paramSpan}
                | (_, _) =>
                    {name = "__arg", annot = NONE, span = firstSpan})
              params
        in
          Ast.DFun
            { name = name
            , nameSpan = nameSpan
            , params = params'
            , result = result
            , recursive = recursive
            , body = body
            , span = span
            }
        end
      else
        let
          val params' =
            List.tabulate
              (arity, fn index =>
                { name = "__arg" ^ Int.toString index
                , annot = annotationAt index firstParams
                , span = bindingSpan index
                })
          val arguments =
            List.tabulate
              (arity, fn index =>
                Ast.EVar ("__arg" ^ Int.toString index, useSpan index))
          val scrutinee =
            case arguments of
              [argument] => argument
            | _ => Ast.ETuple (arguments, syntheticSpan 300000 0)
          fun makeBranch (index, clause as (params, _, body, clauseSpan, _)) =
            let
              val patterns = List.map (fn (pattern, _) => pattern) params

              val base =
                case params of
                  (pattern, _) :: _ => Ast.patternSpan pattern
                | [] => clauseSpan
              val pattern =
                case patterns of
                  [single] => single
                | _ => Ast.PTuple (patterns, syntheticSpanAt (base, 500000, index))
            in
              (pattern, NONE, body)
            end
          fun clauseSpecificity params =
            List.foldl
              (fn ((pattern, _), total) => total + specificity pattern)
              0
              params
          fun insertClause
                (clause as (left, _, _, _, _),
                 (next as (right, _, _, _, _)) :: rest) =
                if clauseSpecificity left > clauseSpecificity right then
                  clause :: next :: rest
                else
                  next :: insertClause (clause, rest)
            | insertClause (clause, []) = [clause]
          val orderedParts =
            List.foldl
              (fn (clause, ordered) => insertClause (clause, ordered))
              []
              parts
          val branches =
            List.map makeBranch
              (ListPair.zip
                 (List.tabulate
                    (List.length orderedParts, fn index => index),
                  orderedParts))
        in
          Ast.DFun
            { name = name
            , nameSpan = firstNameSpan
            , params = params'
            , result = result
            , recursive = recursive
            , body =
                Ast.ECase
                  (scrutinee, branches, syntheticSpan 400000 0)
            , span = firstSpan
            }
        end
    end

  and desugarDeclaration declaration =
    case declaration of
      Ast.DFun { name, nameSpan, params, result, recursive, body, span } =>
        Ast.DFun
          { name = name
          , nameSpan = nameSpan
          , params = params
          , result = result
          , recursive = recursive
          , body = desugarExpr body
          , span = span
          }
    | Ast.DFunClause {name, nameSpan, params, result, recursive, body, span} =>
        desugarClauses name
          [Ast.DFunClause
             {name = name, nameSpan = nameSpan, params = params,
              result = result, recursive = recursive, body = body, span = span}]
    | Ast.DType _ => declaration
    | Ast.DAlias _ => declaration
    | Ast.DDatatype _ => declaration
    | Ast.DImport _ => declaration

  and desugarExpr expression =
    case expression of
      Ast.EInt _ => expression
    | Ast.EFloat _ => expression
    | Ast.EString _ => expression
    | Ast.EBool _ => expression
    | Ast.EUnit _ => expression
    | Ast.EVar _ => expression
    | Ast.ECall (name, arguments, span) =>
        Ast.ECall (name, List.map desugarExpr arguments, span)
    | Ast.EBin ("::", head, tail, span) =>
        let
          val start = #start span
          val synthetic =
            { file = #file start
            , line = #line start
            , column = #column start
            , offset = #offset start + 1000000
            }
          val tupleSpan = {start = synthetic, stop = synthetic}
        in
          Ast.ECall
            ( "Cons"
            , [Ast.ETuple ([desugarExpr head, desugarExpr tail], tupleSpan)]
            , span
            )
        end
    | Ast.EBin ("|>", left, right, span) =>
        desugarPipe (desugarExpr left, desugarExpr right, span)
    | Ast.EBin (operator, left, right, span) =>
        Ast.EBin (operator, desugarExpr left, desugarExpr right, span)
    | Ast.ENeg (operand, span) =>
        Ast.ENeg (desugarExpr operand, span)
    | Ast.EIf (condition, thenBranch, elseBranch, span) =>
        Ast.EIf
          ( desugarExpr condition
          , desugarExpr thenBranch
          , desugarExpr elseBranch
          , span
          )
    | Ast.ELet (bindings, body, _) =>
        desugarBindings bindings body
    | Ast.EArena (bindings, body, span) =>
        let
          val id = !arenaSpanCounter
          val _ = arenaSpanCounter := id + 1
          fun arenaSpan delta =
            let
              val origin = #start span
              val position =
                { file = #file origin
                , line = #line origin
                , column = #column origin
                , offset = #offset origin + 1000000 + id * 8 + delta
                }
            in
              { start = position, stop = position }
            end
          val enterSpan = arenaSpan 0
          val keepSpan = arenaSpan 1
          val leaveSpan = arenaSpan 2
          val enterName = freshArenaName "__yack_arena_enter_"
          val resultName = freshArenaName "__yack_arena_result_"
          val leaveName = freshArenaName "__yack_arena_leave_"
          fun synthBinding name value bindingSpan =
            Ast.BVal
              { name = name
              , annot = NONE
              , value = value
              , span = bindingSpan
              }
          val scoped = desugarBindings bindings body

          val kept =
            Ast.ECall ("arena_keep", [scoped], keepSpan)
          val resultBinding =
            synthBinding resultName kept span
          val leaveBinding =
            synthBinding leaveName
              (Ast.ECall ("sys.scratch_leave", [], leaveSpan))
              leaveSpan
          val returned = Ast.EVar (resultName, span)
          val leaveScope =
            Ast.ELet
              ( [ resultBinding, leaveBinding ]
              , returned
              , span
              )
          val enterBinding =
            synthBinding enterName
              (Ast.ECall ("sys.scratch_enter_all", [], enterSpan))
              enterSpan
        in
          Ast.ELet ([enterBinding], leaveScope, span)
        end
    | Ast.ERecord (fields, span) =>

        let
          fun insert (field, []) = [field]
            | insert (field as (name, _), (other as (otherName, _)) :: rest) =
                if name <= otherName then field :: other :: rest
                else other :: insert (field, rest)
        in
          Ast.ERecord
            (List.foldl insert []
               (List.map (fn (name, value) => (name, desugarExpr value)) fields),
             span)
        end
    | Ast.EField (field, base, span) =>
        Ast.EField (field, desugarExpr base, span)
    | Ast.ETuple (values, span) =>
        Ast.ETuple (List.map desugarExpr values, span)
    | Ast.ETupleIndex (index, base, span) =>
        Ast.ETupleIndex (index, desugarExpr base, span)
    | Ast.ECase (scrutinee, branches, span) =>
        Ast.ECase
          ( desugarExpr scrutinee
          , List.map
              (fn (pattern, guard, body) =>
                ( pattern
                , Option.map desugarExpr guard
                , desugarExpr body
                ))
              branches
          , span
          )
    | Ast.EBorrow (mutable, ampersand, operand, span) =>
        Ast.EBorrow (mutable, ampersand, desugarExpr operand, span)
    | Ast.EFn (params, result, body, span) =>
        Ast.EFn (params, result, desugarExpr body, span)

  and desugarBindings bindings body =
    case bindings of
      [] => desugarExpr body
    | binding :: rest =>
        let val inner = desugarBindings rest body in
          case binding of
            Ast.BVal { name, annot, value, span } =>
              Ast.ELet
                ( [ Ast.BVal
                      { name = name
                      , annot = annot
                      , value = desugarExpr value
                      , span = span
                      }
                  ]
                , inner
                , span
                )
          | Ast.BPat { pattern, value, span } =>
              Ast.ECase
                ( desugarExpr value
                , [ (pattern, NONE, inner) ]
                , span
                )
          | Ast.BFun { name, nameSpan, params, result, body, span } =>
              Ast.ELet
                ( [ Ast.BFun
                      { name = name
                      , nameSpan = nameSpan
                      , params = params
                      , result = result
                      , body = desugarExpr body
                      , span = span
                      }
                  ]
                , inner
                , span
                )
          | Ast.BGuard { pattern = NONE, value, fallback, span } =>
              Ast.EIf
                ( desugarExpr value
                , inner
                , desugarExpr fallback
                , span
                )
          | Ast.BGuard { pattern = SOME pattern, value, fallback, span } =>
              Ast.ECase
                ( desugarExpr value
                , [ (pattern, NONE, inner)
                  , (Ast.PWildcard (Ast.patternSpan pattern),
                     NONE, desugarExpr fallback)
                  ]
                , span
                )
        end

  and desugarPipe (left, right, span) =
    case right of
      Ast.ECall (name, arguments, _) =>
        Ast.ECall (name, left :: arguments, span)
    | Ast.EVar (name, _) =>
        Ast.ECall (name, [left], span)
    | Ast.EField _ =>
        (case qualifiedName right of
           SOME name => Ast.ECall (name, [left], span)
         | NONE => Ast.EBin ("|>", left, right, span))
    | _ => Ast.EBin ("|>", left, right, span)

  and qualifiedName expression =
    case expression of
      Ast.EVar (name, _) => SOME name
    | Ast.EField (field, base, _) =>
        (case qualifiedName base of
           SOME name => SOME (name ^ "." ^ field)
         | NONE => NONE)
    | _ => NONE
end
