structure ParserState =
struct
type state =
  { tokens : Token.t array
  , count : int
  , index : int ref
  , diagnostics : Diag.t list ref
  , allowArrowFun : bool ref
  }

exception Abort

fun current (st : state) =
  let val i = !(#index st) in
    if i < #count st then Array.sub (#tokens st, i)
    else Array.sub (#tokens st, #count st - 1)
  end

fun peek (st : state) offset =
  let val i = !(#index st) + offset in
    if i < #count st then Array.sub (#tokens st, i)
    else Array.sub (#tokens st, #count st - 1)
  end

fun advance (st : state) =
  let val i = !(#index st) in
    if i < #count st - 1 then #index st := i + 1 else ()
  end

fun atEnd st =
  (case #kind (current st) of Token.Eof => true | _ => false)

fun report (st : state) code message span notes hints =
  #diagnostics st :=
    Diag.error (code, message, span, notes, hints) :: !(#diagnostics st)

fun failAt st code message span notes hints =
  (report st code message span notes hints; raise Abort)

fun currentSpan st = #span (current st)

fun previousToken st =
  let val index = !(#index st) in
    if index > 0 then Array.sub (#tokens st, index - 1) else current st
  end

fun expectPunct st punct =
  let val token = current st in
    if Token.isPunct punct token then advance st
    else
      failAt st
        "E2002"
        ("expected `" ^ punct ^ "`, found " ^ Token.describe token)
        (#span token)
        []
        []
  end

fun expectKeyword st keyword =
  let val token = current st in
    if Token.isKeyword keyword token then advance st
    else
      failAt st
        "E2002"
        ("expected `" ^ keyword ^ "`, found " ^ Token.describe token)
        (#span token)
        []
        []
  end

fun expectIdent st context =
  let val token = current st in
    case Token.identOf token of
      SOME name => (advance st; (name, #span token))
    | NONE =>
        failAt st
          "E2002"
          ("expected " ^ context ^ ", found " ^ Token.describe token)
          (#span token)
          []
          []
  end

end
