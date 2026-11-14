structure LocalFun =
struct
  type info =
    { lifted : string
    , captures : string list option ref
    , span : Diag.span
    }

  type environment =
    { vars : string list
    , funs : (string * info) list
    , captured : string list
    }

  fun liftProgram (program : Ast.program) =
    let
      val lifted = ref []
      val diagnostics = ref []
      val counter = ref 0

      fun report code message span notes hints =
        let val cell = diagnostics in
          cell := Diag.error (code, message, span, notes, hints) :: !cell
        end

      fun freshName name =
        let
          val index = !counter
          val _ = counter := index + 1
        in
          "__yack_local_" ^ Int.toString index ^ "_" ^ name
        end

      fun isIn name names = List.exists (fn candidate => candidate = name) names

      fun addVarAt (environment : environment, name, span) =
        let
          val _ =
            if isIn name (#captured environment) then
              report
                "E0519"
                ("binding `" ^ name ^
                 "` shadows a variable captured by a local function")
                span
                [ "the local function was lambda-lifted with the outer binding"
                , "the call site could no longer name the captured binding unambiguously"
                ]
                [ "rename the new `val`/parameter, or move the local function above the shadowing binding"
                ]
            else if List.exists (fn (candidate, _) => candidate = name)
                      (#funs environment) then
              report
                "E0519"
                ("binding `" ^ name ^
                 "` shadows a local function")
                span
                [ "local functions are lifted by name, so a shadowing binding would hide it"
                , "use a different name to keep the direct call unambiguous"
                ]
                [ "rename the new `val`/parameter"
                , "or rename the local function"
                ]
            else ()
        in
          { vars = name :: #vars environment
          , funs = List.filter (fn (candidate, _) => candidate <> name)
                     (#funs environment)
          , captured = #captured environment
          }
        end

      fun addFun (environment : environment, name, info) =
        let
          val _ =
            if isIn name (#captured environment) then
              report
                "E0519"
                ("local function `" ^ name ^
                 "` shadows a captured variable")
                (#span info)
                [ "local functions are lifted by name and captures are explicit"
                , "a function name that shadows a capture makes calls ambiguous"
                ]
                [ "rename the local function"
                , "or rename the captured variable"
                ]
            else if isIn name (#vars environment) then
              report
                "E0519"
                ("local function `" ^ name ^
                 "` shadows a local variable")
                (#span info)
                [ "local functions and local values share the same lexical scope"
                , "a shadowing function would hide the value from later code"
                ]
                [ "rename the local function"
                , "or move the value to an outer `let`"
                ]
            else if List.exists (fn (candidate, _) => candidate = name)
                      (#funs environment) then
              report
                "E0519"
                ("local function `" ^ name ^
                 "` shadows another local function")
                (#span info)
                [ "local functions are lifted by name"
                , "a duplicate local name makes the intended target ambiguous"
                ]
                [ "rename one of the local functions"
                ]
            else ()
        in
          { vars = List.filter (fn candidate => candidate <> name)
                     (#vars environment)
          , funs = (name, info) ::
                     List.filter (fn (candidate, _) => candidate <> name)
                       (#funs environment)
          , captured = #captured environment
          }
        end

      fun lookupFun (environment : environment, name) =
        if isIn name (#vars environment) then NONE
        else
          case List.find (fn (candidate, _) => candidate = name)
                 (#funs environment) of
            SOME (_, info) => SOME info
          | NONE => NONE

      fun patternNames pattern =
        case pattern of
          Ast.PVar (name, _) => [name]
        | Ast.PWildcard _ => []
        | Ast.PInt _ => []
        | Ast.PBool _ => []
        | Ast.PString (_, rest, _) =>
            (case rest of
               SOME subpattern => patternNames subpattern
             | NONE => [])
        | Ast.PBytes (_, rest, _) =>
            (case rest of
               SOME subpattern => patternNames subpattern
             | NONE => [])
        | Ast.PConstructor (_, argument, _) =>
            (case argument of
               SOME subpattern => patternNames subpattern
             | NONE => [])
        | Ast.PTuple (patterns, _) =>
            List.concat (List.map patternNames patterns)
        | Ast.PRecord (fields, _) =>
            List.concat
              (List.map (fn (_, subpattern) =>
                 patternNames subpattern) fields)

      fun bindingNames binding =
        case binding of
          Ast.BVal {name, ...} => [name]
        | Ast.BPat {pattern, ...} => patternNames pattern
        | Ast.BFun {name, ...} => [name]
        | Ast.BGuard _ => raise Fail "desugared"

      fun shiftedSpan (base : Diag.span, delta, index) =
        let
          val origin = #start base
          val position =
            { file = #file origin
            , line = #line origin
            , column = #column origin
            , offset = #offset origin + delta + index
            }
        in
          {start = position, stop = position}
        end

      fun captureSpan (base, index) = shiftedSpan (base, 1000000, index)
      fun parameterSpan (base, index) = shiftedSpan (base, 2000000, index)

      fun captureArgs (base, captures) =
        List.map
          (fn (index, name) => Ast.EVar (name, captureSpan (base, index)))
          (ListPair.zip
             (List.tabulate (List.length captures, fn index => index), captures))

      fun liftExpression (environment : environment, expression) =
        case expression of
          Ast.EInt _ => expression
        | Ast.EFloat _ => expression
        | Ast.EString _ => expression
        | Ast.EBool _ => expression
        | Ast.EUnit _ => expression
        | Ast.EVar (name, span) =>
            (case lookupFun (environment, name) of
               SOME {lifted = liftedName, captures = cell, ...} =>
                 (case !cell of
                    SOME captures =>
                      if List.null captures then
                        Ast.EVar (liftedName, span)
                      else
                        ( report
                            "E0518"
                            ("local function `" ^ name ^
                             "` captures variables and cannot be used as a closure")
                            span
                            [ "Yack has no capturing closures"
                            , "a local function is lambda-lifted and only supports direct calls"
                            , "a local function without captures can still be used as a value"
                            ]
                            [ "call it directly, e.g. `" ^ name ^ " (...)`"
                            , "or move it to module level and pass the captures explicitly"
                            , "example: `fun " ^ name ^ " (capture, argument) = ...`"
                            ]
                        ; Ast.EVar (liftedName, span)
                        )
                  | NONE => Ast.EVar (name, span))
             | NONE => expression)
        | Ast.ECall (name, arguments, span) =>
            let
              val arguments' =
                List.map (fn argument => liftExpression (environment, argument))
                  arguments
            in
              case lookupFun (environment, name) of
                SOME {lifted = liftedName, captures = cell, ...} =>
                  (case !cell of
                     SOME captures =>
                       Ast.ECall
                         ( liftedName
                         , arguments' @ captureArgs (span, captures)
                         , span
                         )
                   | NONE => Ast.ECall (name, arguments', span))
              | NONE => Ast.ECall (name, arguments', span)
            end
        | Ast.EBin (operator, left, right, span) =>
            Ast.EBin
              ( operator
              , liftExpression (environment, left)
              , liftExpression (environment, right)
              , span
              )
        | Ast.ENeg (operand, span) =>
            Ast.ENeg (liftExpression (environment, operand), span)
        | Ast.EIf (condition, thenBranch, elseBranch, span) =>
            Ast.EIf
              ( liftExpression (environment, condition)
              , liftExpression (environment, thenBranch)
              , liftExpression (environment, elseBranch)
              , span
              )
        | Ast.ELet (bindings, body, span) =>
            liftLet (environment, bindings, body, span, false)
        | Ast.EArena (bindings, body, span) =>
            liftLet (environment, bindings, body, span, true)
        | Ast.ERecord (fields, span) =>
            Ast.ERecord
              ( List.map
                  (fn (name, value) =>
                    (name, liftExpression (environment, value)))
                  fields
              , span
              )
        | Ast.EField (field, base, span) =>
            Ast.EField (field, liftExpression (environment, base), span)
        | Ast.ETuple (values, span) =>
            Ast.ETuple
              (List.map (fn value => liftExpression (environment, value)) values,
               span)
        | Ast.ETupleIndex (index, base, span) =>
            Ast.ETupleIndex (index, liftExpression (environment, base), span)
        | Ast.ECase (scrutinee, branches, span) =>
            Ast.ECase
              ( liftExpression (environment, scrutinee)
              , List.map
                  (fn (pattern, guard, body) =>
                    let
                      val extended =
                        List.foldl
                          (fn (name, environment) =>
                            addVarAt
                              (environment, name, Ast.patternSpan pattern))
                          environment
                          (patternNames pattern)
                    in
                      ( pattern
                      , Option.map
                          (fn condition =>
                            liftExpression (environment, condition))
                          guard
                      , liftExpression (extended, body)
                      )
                    end)
                  branches
              , span
              )
        | Ast.EBorrow (mutable, ampersand, operand, span) =>
            Ast.EBorrow
              (mutable, ampersand, liftExpression (environment, operand), span)
        | Ast.EFn (params, result, body, span) =>
            let
              val extended =
                List.foldl
                  (fn (param, environment) =>
                    addVarAt (environment, #name param, #span param))
                  environment
                  params
            in
              Ast.EFn
                (params, result, liftExpression (extended, body), span)
            end

      and liftLet (environment, bindings, body, span, arena) =
        let
          fun loop (environment, kept, []) =
                (environment, List.rev kept)
            | loop (environment, kept, binding :: rest) =
                (case binding of
                   Ast.BFun {name, nameSpan, params, result, body, span} =>
                     let
                       val environment' =
                         liftOne
                           (environment,
                            { name = name
                            , nameSpan = nameSpan
                            , params = params
                            , result = result
                            , body = body
                            , span = span
                            })
                     in
                       loop (environment', kept, rest)
                     end
                 | Ast.BVal {name, annot, value, span} =>
                     let
                       val value' = liftExpression (environment, value)
                       val environment' =
                         addVarAt (environment, name, span)
                     in
                       loop
                         ( environment'
                         , Ast.BVal
                             {name = name, annot = annot, value = value',
                              span = span} :: kept
                         , rest
                         )
                     end
                 | Ast.BPat {pattern, value, span} =>
                     let
                       val value' = liftExpression (environment, value)
                       val environment' =
                         List.foldl
                           (fn (name, environment) =>
                             addVarAt
                               (environment, name, Ast.patternSpan pattern))
                           environment
                           (patternNames pattern)
                     in
                       loop
                         ( environment'
                         , Ast.BPat
                             {pattern = pattern, value = value', span = span} ::
                             kept
                         , rest
                         )
                     end
                 | Ast.BGuard _ => raise Fail "desugared")
          val (environment', kept) = loop (environment, [], bindings)
          val body' = liftExpression (environment', body)
        in
          case kept of
            [] => body'
          | _ =>
              if arena then
                Ast.EArena (kept, body', span)
              else
                Ast.ELet (kept, body', span)
        end

      and liftOne (environment, declaration as
            { name, nameSpan, params, result, body, span }) =
        let
          val liftedName = freshName name
          val info : info = {lifted = liftedName, captures = ref NONE, span = nameSpan}
          val parameters = List.map (fn param => #name param) params
          val bodyEnvironment =
            addFun
              (List.foldl
                 (fn (param, environment) =>
                   addVarAt (environment, #name param, #span param))
                 environment
                 params,
               name,
               info)
          val body' = liftExpression (bodyEnvironment, body)
          val captures = freeVariables (#vars environment, parameters, body')
          val body'' =
            rewriteSelf
              (name, liftedName, captures, body', span)
          val captureParams =
            List.map
              (fn (index, capture) =>
                { name = capture
                , annot = NONE
                , span = parameterSpan (nameSpan, index)
                })
              (ListPair.zip
                 (List.tabulate (List.length captures, fn index => index),
                  captures))
          val _ = #captures info := SOME captures
          val _ = lifted :=
            Ast.DFun
              { name = liftedName
              , nameSpan = nameSpan
              , params = params @ captureParams
              , result = result
              , recursive = false
              , body = body''
              , span = span
              } :: !lifted
        in
          addFun
            ({ vars = #vars environment
             , funs = #funs environment
             , captured = captures @ #captured environment
             },
             name,
             info)
        end

      and freeVariables (outerVars, parameters, expression) =
        let
          val captures = ref []
          fun add name =
            if isIn name (!captures) then ()
            else captures := name :: !captures
          fun walk (bound, expression) =
            case expression of
              Ast.EInt _ => ()
            | Ast.EFloat _ => ()
            | Ast.EString _ => ()
            | Ast.EBool _ => ()
            | Ast.EUnit _ => ()
            | Ast.EVar (name, _) =>
                if isIn name bound then ()
                else if isIn name outerVars then add name
                else ()
            | Ast.ECall (name, arguments, _) =>
                ( List.app (fn value => walk (bound, value)) arguments
                ; if isIn name bound then ()
                  else if isIn name outerVars then add name
                  else ()
                )
            | Ast.EBin (_, left, right, _) =>
                (walk (bound, left); walk (bound, right))
            | Ast.ENeg (operand, _) => walk (bound, operand)
            | Ast.EIf (condition, thenBranch, elseBranch, _) =>
                ( walk (bound, condition)
                ; walk (bound, thenBranch)
                ; walk (bound, elseBranch)
                )
            | Ast.ELet (bindings, body, _) =>
                let
                  fun bind (binding, bound) =
                    case binding of
                      Ast.BVal {value, ...} =>
                        (walk (bound, value); bindingNames binding @ bound)
                    | Ast.BPat {value, ...} =>
                        (walk (bound, value); bindingNames binding @ bound)
                    | Ast.BFun {params, body = innerBody, ...} =>
                        ( walk
                            (List.map (fn param => #name param) params @ bound,
                             innerBody)
                        ; bindingNames binding @ bound
                        )
                    | Ast.BGuard _ => raise Fail "desugared"
                  val bound' = List.foldl bind bound bindings
                in
                  walk (bound', body)
                end
            | Ast.EArena (bindings, body, _) =>
                let
                  fun bind (binding, bound) =
                    case binding of
                      Ast.BVal {value, ...} =>
                        (walk (bound, value); bindingNames binding @ bound)
                    | Ast.BPat {value, ...} =>
                        (walk (bound, value); bindingNames binding @ bound)
                    | Ast.BFun {params, body = innerBody, ...} =>
                        (walk
                           (List.map (fn param => #name param) params @ bound,
                            innerBody)
                        ; bindingNames binding @ bound)
                    | Ast.BGuard _ => raise Fail "desugared"
                  val bound' = List.foldl bind bound bindings
                in
                  walk (bound', body)
                end
            | Ast.ERecord (fields, _) =>
                List.app (fn (_, value) => walk (bound, value)) fields
            | Ast.EField (_, base, _) => walk (bound, base)
            | Ast.ETuple (values, _) => List.app (fn value => walk (bound, value)) values
            | Ast.ETupleIndex (_, base, _) => walk (bound, base)
            | Ast.ECase (scrutinee, branches, _) =>
                ( walk (bound, scrutinee)
                ; List.app
                    (fn (pattern, guard, body) =>
                      let
                        val bound' =
                          patternNames pattern @ bound
                      in
                        ( case guard of
                            NONE => ()
                          | SOME condition => walk (bound', condition)
                        ; walk (bound', body)
                        )
                      end)
                    branches
                )
            | Ast.EBorrow (_, _, operand, _) => walk (bound, operand)
            | Ast.EFn (params, _, body, _) =>
                walk
                  (List.map (fn param => #name param) params @ bound,
                   body)
        in
          walk (parameters, expression);
          List.rev (!captures)
        end

      and rewriteSelf (self, liftedName, captures, expression, baseSpan) =
        let
          fun walk (bound, expression) =
            case expression of
              Ast.EInt _ => expression
            | Ast.EFloat _ => expression
            | Ast.EString _ => expression
            | Ast.EBool _ => expression
            | Ast.EUnit _ => expression
            | Ast.EVar (name, span) =>
                if name = self andalso not (isIn name bound) then
                  ( if List.null captures then ()
                    else
                      report
                        "E0518"
                        ("local function `" ^ self ^
                         "` captures variables and cannot escape its scope")
                        span
                        [ "Yack has no capturing closures"
                        , "returning or storing it would require a closure environment"
                        , "a local function without captures can escape safely"
                        ]
                        [ "call the function directly instead"
                        , "or move it to module level and pass the captures explicitly"
                        , "example: `fun " ^ self ^ " (capture, argument) = ...`"
                        ]
                  ; Ast.EVar (liftedName, span)
                  )
                else expression
            | Ast.ECall (name, arguments, span) =>
                let
                  val arguments' = List.map (fn argument => walk (bound, argument)) arguments
                in
                  if name = self andalso not (isIn name bound) then
                    Ast.ECall
                      ( liftedName
                      , arguments' @ captureArgs (span, captures)
                      , span
                      )
                  else Ast.ECall (name, arguments', span)
                end
            | Ast.EBin (operator, left, right, span) =>
                Ast.EBin
                  (operator, walk (bound, left), walk (bound, right), span)
            | Ast.ENeg (operand, span) =>
                Ast.ENeg (walk (bound, operand), span)
            | Ast.EIf (condition, thenBranch, elseBranch, span) =>
                Ast.EIf
                  ( walk (bound, condition)
                  , walk (bound, thenBranch)
                  , walk (bound, elseBranch)
                  , span
                  )
            | Ast.ELet (bindings, body, span) =>
                let
                  fun bind (binding, bound) =
                    bindingNames binding @ bound
                  val bindings' =
                    List.map
                      (fn binding =>
                        case binding of
                          Ast.BVal {name, annot, value, span} =>
                            Ast.BVal
                              {name = name, annot = annot,
                               value = walk (bound, value), span = span}
                        | Ast.BPat {pattern, value, span} =>
                            Ast.BPat
                              {pattern = pattern, value = walk (bound, value),
                               span = span}
                        | Ast.BFun {name, nameSpan, params, result, body,
                                    span} =>
                            Ast.BFun
                              {name = name, nameSpan = nameSpan,
                               params = params, result = result,
                               body = walk
                                 (List.map (fn param => #name param) params @
                                  bound,
                                  body),
                               span = span}
                          | Ast.BGuard _ => raise Fail "desugared")
                      bindings
                  val bound' = List.foldl bind bound bindings
                in
                  Ast.ELet (bindings', walk (bound', body), span)
                end
            | Ast.EArena (bindings, body, span) =>
                let
                  fun bind (binding, bound) =
                    bindingNames binding @ bound
                  val bindings' =
                    List.map
                      (fn binding =>
                        case binding of
                          Ast.BVal {name, annot, value, span} =>
                            Ast.BVal
                              {name = name, annot = annot,
                               value = walk (bound, value), span = span}
                        | Ast.BPat {pattern, value, span} =>
                            Ast.BPat
                              {pattern = pattern, value = walk (bound, value),
                               span = span}
                        | Ast.BFun {name, nameSpan, params, result, body,
                                    span} =>
                            Ast.BFun
                              {name = name, nameSpan = nameSpan,
                               params = params, result = result,
                               body = walk
                                 (List.map (fn param => #name param) params @
                                  bound,
                                  body),
                               span = span}
                          | Ast.BGuard _ => raise Fail "desugared")
                      bindings
                  val bound' = List.foldl bind bound bindings
                in
                  Ast.EArena (bindings', walk (bound', body), span)
                end
            | Ast.ERecord (fields, span) =>
                Ast.ERecord
                  (List.map
                     (fn (name, value) => (name, walk (bound, value)))
                     fields,
                   span)
            | Ast.EField (field, base, span) =>
                Ast.EField (field, walk (bound, base), span)
            | Ast.ETuple (values, span) =>
                Ast.ETuple (List.map (fn value => walk (bound, value)) values, span)
            | Ast.ETupleIndex (index, base, span) =>
                Ast.ETupleIndex (index, walk (bound, base), span)
            | Ast.ECase (scrutinee, branches, span) =>
                Ast.ECase
                  ( walk (bound, scrutinee)
                  , List.map
                      (fn (pattern, guard, body) =>
                        let
                          val bound' = patternNames pattern @ bound
                        in
                          ( pattern
                          , Option.map (fn condition => walk (bound', condition)) guard
                          , walk (bound', body)
                          )
                        end)
                      branches
                  , span
                  )
            | Ast.EBorrow (mutable, ampersand, operand, span) =>
                Ast.EBorrow
                  (mutable, ampersand, walk (bound, operand), span)
            | Ast.EFn (params, result, body, span) =>
                Ast.EFn
                  ( params
                  , result
                  , walk
                      (List.map (fn param => #name param) params @ bound,
                       body)
                  , span
                  )
        in
          walk ([], expression)
        end

      fun liftDeclaration declaration =
        case declaration of
          Ast.DFun {name, nameSpan, params, result, recursive, body, span} =>
            let
              val environment =
                { vars = List.map (fn param => #name param) params
                , funs = []
                , captured = []
                }
            in
              Ast.DFun
                { name = name
                , nameSpan = nameSpan
                , params = params
                , result = result
                , recursive = recursive
                , body = liftExpression (environment, body)
                , span = span
                }
            end
        | _ => declaration

      val program' = List.map liftDeclaration program
    in
      { program = program' @ List.rev (!lifted)
      , diagnostics = List.rev (!diagnostics)
      }
    end
end
