structure ParserDecl =
struct
  open ParserState
  open ParserType

fun lastSegment name =
  case List.rev (String.fields (fn ch => ch = #".") name) of
    segment :: _ => segment
  | [] => name

fun segmentIsUpper name =
  let val segment = lastSegment name in
    String.size segment > 0 andalso Char.isUpper (String.sub (segment, 0))
  end

fun startsAtom (token : Token.t) =
  case #kind token of
    Token.IntLit _ => true
  | Token.FloatLit _ => true
  | Token.StringLit _ => true
  | Token.Ident _ => true
  | Token.Keyword "true" => true
  | Token.Keyword "false" => true
  | Token.Keyword "case" => true
  | Token.Keyword "borrow" => true
  | Token.Punct "&" => true
  | Token.Punct "(" => true
  | Token.Punct "[" => true
  | Token.Punct "{" => true
  | _ => false

fun parseProperty st =
  let val token = current st in
    case Token.identOf token of
      SOME "Copy" => (advance st; Ast.PCopy)
    | SOME "Affine" => (advance st; Ast.PAffine)
    | SOME "Linear" => (advance st; Ast.PLinear)
    | _ =>
        failAt st
          "E2002"
          ("expected a memory property (Copy, Affine or Linear), found " ^
           Token.describe token)
          (#span token)
          []
          []
  end

fun parseFields st =
  let
    val seen = StringSet.create 64
    fun loop acc =
      let
        val (name, span) = expectIdent st "a field name"
        val _ =
          if StringSet.insert (seen, name) then ()
          else report st "E3013" ("duplicate field `" ^ name ^ "`") span [] []
        val _ = expectPunct st ":"
        val ty = parseTy st
        val field = (name, ty)
      in
        if Token.isPunct "," (current st) then
          (advance st; loop (field :: acc))
        else
          let val close = current st
              val _ = expectPunct st "}"
          in (List.rev (field :: acc), close) end
      end
  in
    loop []
  end

fun parseTypeParams st =
  let
    fun parseParams acc =
      case Token.tyVarOf (current st) of
        SOME (name, equality) =>
          (advance st; parseParams ((name, equality) :: acc))
      | NONE => List.rev acc
    fun parseParenthesizedParams () =
      let
        val _ = expectPunct st "("
        fun loop acc =
          case Token.tyVarOf (current st) of
            SOME (name, equality) =>
              let
                val _ = advance st
                val item = (name, equality)
              in
                if Token.isPunct "," (current st) then
                  (advance st; loop (item :: acc))
                else
                  let val _ = expectPunct st ")"
                  in List.rev (item :: acc) end
              end
          | NONE =>
              ( failAt st
                  "E2002"
                  ("expected a type parameter, found " ^
                   Token.describe (current st))
                  (#span (current st))
                  []
                  []
              ; acc
              )
      in
        loop []
      end
  in
    if Token.isPunct "(" (current st) then parseParenthesizedParams ()
    else parseParams []
  end

fun parseTypeDeclaration st =
  let
    val startToken = current st
    val _ = advance st
    val params = parseTypeParams st
    val (name, _) = expectIdent st "a type name"
    val declared =
      if Token.isPunct ":" (current st) then
        (advance st; SOME (parseProperty st))
      else NONE
    val _ = expectPunct st "="
  in
    if List.null params andalso Token.isPunct "{" (current st) then
      let
        val _ = expectPunct st "{"
        val (fields, close) =
          if Token.isPunct "}" (current st) then
            let val close = current st
                val _ = advance st
            in ([], close) end
          else parseFields st
        val span = {start = Token.startOf startToken, stop = Token.stopOf close}
      in
        Ast.DType
          { name = name
          , declared = declared
          , fields = fields
          , span = span
          }
      end
    else
      let
        val body = parseTy st
        val stop = Token.stopOf (previousToken st)
        val span = {start = Token.startOf startToken, stop = stop}
        val _ =
          case declared of
            SOME _ =>
              report st
                "E2002"
                "a type alias inherits its memory property from its target"
                span
                ["remove the property annotation"]
                ["write `type " ^ name ^ " = ...`"]
          | NONE => ()
      in
        Ast.DAlias
          { name = name
          , params = params
          , body = body
          , span = span
          }
      end
  end

fun parseDatatypeDeclaration st =
  let
    val startToken = current st
    val _ = advance st
    val params = parseTypeParams st
    val (name, _) = expectIdent st "a datatype name"
    val declared =
      if Token.isPunct ":" (current st) then
        (advance st; SOME (parseProperty st))
      else NONE
    val _ = expectPunct st "="
    fun constructors acc =
      let
        val (constructor, constructorSpan) =
          expectIdent st "a constructor name"
        val _ =
          if String.size constructor > 0 andalso
             Char.isUpper (String.sub (constructor, 0))
          then ()
          else
            report st
              "E3023"
              ("constructor `" ^ constructor ^
               "` must start with an uppercase letter")
              constructorSpan
              []
              []
        val (argument, stop) =
          if Token.isKeyword "of" (current st) then
            let
              val _ = advance st
              val argument = parseTy st
              val stop = Token.stopOf (previousToken st)
            in
              (SOME argument, stop)
            end
          else (NONE, #stop constructorSpan)
        val constructorInfo = (constructor, argument)
      in
        if Token.isPunct "|" (current st) then
          (advance st; constructors (constructorInfo :: acc))
        else
          (List.rev (constructorInfo :: acc), stop)
      end
    val (constructorList, stop) = constructors []
    val span = {start = Token.startOf startToken, stop = stop}
  in
    Ast.DDatatype
      { name = name
      , params = params
      , declared = declared
      , constructors = constructorList
      , span = span
      }
  end

end
