structure OwnershipCallbacks =
struct
  open OwnershipTypes

  type env =
    { borrowedParams : int list ref
    , expectedCallback : Types.ty option ref
    , reportedCallbackConflicts : Diag.span list ref
    , bindingTypeById : int -> Types.ty option
    , report : string -> string -> Diag.span -> string list -> string list -> unit
    , bindingName : int -> string
    , placeDescription : Ast.expr -> string
    , expressionTypeAt : Diag.span -> Types.ty option
    , isConsume : Types.ty -> bool
    , hasDropLeaf : Types.ty -> bool
    }

  fun make (env : env) =
    let
      val borrowedParams = #borrowedParams env
      val expectedCallback = #expectedCallback env
      val reportedCallbackConflicts = #reportedCallbackConflicts env
      val bindingTypeById = #bindingTypeById env
      val report = #report env
      val bindingName = #bindingName env
      val placeDescription = #placeDescription env
      val expressionTypeAt = #expressionTypeAt env
      val isConsume = #isConsume env
      val hasDropLeaf = #hasDropLeaf env

      fun isBorrowedParam id =
  List.exists (fn candidate => candidate = id) (!borrowedParams)

      fun reportCallbackMove (subject, id, span) =
  let
    val tyText =
      case bindingTypeById id of
        SOME ty => Types.toString ty
      | NONE => "'a"
    val callbackText =
      case !expectedCallback of
        SOME ty => Types.toString ty
      | NONE => "a callback type"
  in
    report
      "E0512"
      (subject ^ " is borrowed, not owned")
      span
      [ "the callback type here is `" ^ callbackText ^
        "`: its parameters are only lent to the callback"
      , "moving `" ^ bindingName id ^ "` would free a value the caller still owns"
      ]
      [ "if the callback must consume its argument, write `consume " ^
        tyText ^ " -> ...` in the function type, for example `fun apply (f : consume " ^
        tyText ^ " -> unit, ...)`"
      , "otherwise read `" ^ bindingName id ^
        "` instead of passing it to a function that takes ownership"
      , "or pass a named function: named function parameters own their arguments"
      ]
  end

      fun callbackConflict (paramType, argument) =
  case argument of
    Ast.EFn _ => NONE
  | _ =>
      let
        val expected =
          case Types.prune paramType of
            Types.TConsume inner => Types.prune inner
          | other => other
      in
        case expected of
          Types.TFun (expectedParams, _) =>
            (case expressionTypeAt (Ast.exprSpan argument) of
               SOME actual =>
                 (case Types.prune actual of
                    Types.TFun (actualParams, _) =>
                      let
                        fun loop (expected, actual) =
                          case (expected, actual) of
                            (e :: es, a :: rest) =>
                              if not (isConsume e) andalso hasDropLeaf a
                              then SOME a
                              else loop (es, rest)
                          | _ => NONE
                      in
                        loop (expectedParams, actualParams)
                      end
                  | _ => NONE)
             | NONE => NONE)
        | _ => NONE
      end

      fun reportCallbackConflict (expression, actualParamTy, paramType, span) =
  if List.exists (fn candidate => candidate = span)
       (!reportedCallbackConflicts) then ()
  else
    let
      val _ = reportedCallbackConflicts := span :: !reportedCallbackConflicts
      val name = placeDescription expression
    in
      report
        "E0513"
        ("`" ^ name ^ "` owns its parameter, but this callback borrows it")
        span
        [ "the callback type is `" ^ Types.toString paramType ^
          "`; its parameter is lent to the callback"
        , "`" ^ name ^ "` has parameter type `" ^
          Types.toString actualParamTy ^
          "` and would free the argument, while the caller also frees it"
        ]
        [ "mark the callback parameter `consume " ^
          Types.toString actualParamTy ^
          " -> ...` so the caller moves the argument"
        , "or pass a lambda that only reads the value, for example `x => ...`"
        , "or use a named function whose parameter is `borrow " ^
          Types.toString actualParamTy ^ "` (read-only)"
        ]
    end

    in
      (isBorrowedParam, reportCallbackMove, callbackConflict,
       reportCallbackConflict)
    end
end
