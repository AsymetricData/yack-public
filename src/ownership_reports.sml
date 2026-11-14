structure OwnershipReports =
struct
  open OwnershipTypes

  type env =
    { report : string -> string -> Diag.span -> string list -> string list -> unit
    , bindingById : int -> Resolve.binding option
    , bindingTypeById : int -> Types.ty option
    , bindings : Resolve.binding list
    }

  fun make (env : env) =
    let
      val report = #report env
      val bindingById = #bindingById env
      val bindingTypeById = #bindingTypeById env
      val bindings = #bindings env

      fun bindingName id =
  case bindingById id of
    SOME binding => #name binding
  | NONE => "?"

      fun placeDescription expression =
  case expression of
    Ast.EVar (name, _) => name
  | Ast.EField (field, base, _) =>
      placeDescription base ^ "." ^ field
  | Ast.ETupleIndex (index, base, _) =>
      placeDescription base ^ "." ^ Int.toString index
  | _ => "?"

      fun isConstructor name =
  List.exists
    (fn binding =>
      #name binding = name andalso #kind binding = Resolve.Constructor)
    bindings

      fun reportUseAfterMove id span =
  report
    "E0402"
    ("value `" ^ bindingName id ^ "` is used after being moved")
    span
    ["the value was already transferred to another owner"]
    ["move the use before the transfer, or copy the value if its type is Copy"]

      fun linearConsumeHint id =
  case bindingTypeById id of
    SOME ty =>
      (case Types.prune ty of
         Types.TToken =>
           "consume it with `consume_token (" ^ bindingName id ^ ")`"
       | Types.TResource =>
           "release it with `drop_resource (" ^ bindingName id ^ ")`"
       | _ =>
           "pass `" ^ bindingName id ^
           "` to a function that takes ownership, or return it")
  | NONE => "pass it to a function that takes ownership, or return it"

      fun reportPlaceUseAfterMove (expression, id, span) =
  let
    val description = placeDescription expression
  in
    if description = bindingName id then reportUseAfterMove id span
    else
      report
        "E0402"
        ("value `" ^ description ^ "` is used after being moved")
        span
        [ "`" ^ description ^
          "` no longer owns this value: it was moved out earlier"
        , "the owner `" ^ bindingName id ^
          "` still exists, but this place is uninitialized"
        ]
        [ "move this use before the transfer"
        , "or use another field of `" ^ bindingName id ^ "`"
        ]
  end

      fun reportNotConsumed id =
  case bindingById id of
    NONE => ()
  | SOME binding =>
      report
        "E0403"
        ("linear value `" ^ #name binding ^
         "` is never consumed on this path")
        (#span binding)
        [ "values with the Linear property must be consumed exactly once on every path"
        , "leaving it alive is rejected instead of freeing it implicitly"
        ]
        [ linearConsumeHint id
        , "or move the consumption after the `if`/`case` so every branch reaches it"
        ]

      fun reportBranchMismatch id =
  case bindingById id of
    NONE => ()
  | SOME binding =>
      report
        "E0403"
        ("linear value `" ^ #name binding ^
         "` is consumed on only one branch")
        (#span binding)
        [ "the value is still available on the other path"
        , "the Linear property requires exactly one consumption on every path"
        ]
        [ "consume `" ^ #name binding ^
          "` in both branches, for example with `consume_token` or `drop_resource`"
        , "or move the consumption after the `if`/`case` so both paths reach it"
        ]

    in
      (bindingName, placeDescription, isConstructor, reportUseAfterMove,
       linearConsumeHint, reportPlaceUseAfterMove, reportNotConsumed,
       reportBranchMismatch)
    end
end
