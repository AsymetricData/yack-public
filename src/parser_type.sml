structure ParserType =
struct
  open ParserState

fun parseTy st = parseFunTy st

and parseFunTy st =
  let
    fun scan offset depth comma =
      let val tok = peek st offset in
        if Token.isPunct "(" tok then scan (offset + 1) (depth + 1) comma
        else if Token.isPunct ")" tok then
          if depth = 1 then
            (comma orelse (offset = 1), Token.isPunct "->" (peek st (offset + 1)))
          else scan (offset + 1) (depth - 1) comma
        else if Token.isPunct "," tok andalso (depth = 1) then
          scan (offset + 1) depth true
        else if #kind tok = Token.Eof then (false, false)
        else scan (offset + 1) depth comma
      end
    val (multi, arrow) =
      if Token.isPunct "(" (current st) then scan 0 0 false
      else (false, false)
  in
    if multi andalso arrow then
      let
        val _ = advance st
        fun params acc =
          if Token.isPunct ")" (current st) then
            (advance st; List.rev acc)
          else
            let val ty = parseTy st in
              if Token.isPunct "," (current st) then
                (advance st; params (ty :: acc))
              else
                (expectPunct st ")"; List.rev (ty :: acc))
            end
        val parameterTypes = params []
        val _ = expectPunct st "->"
        val result = parseFunTy st
      in
        Ast.TyFun (parameterTypes, result)
      end
    else
      let
        val first = parseTupleTy st
      in
        if Token.isPunct "->" (current st) then
          let
            val _ = advance st
            val result = parseFunTy st
          in
            Ast.TyFun ([first], result)
          end
        else first
      end
  end

and parseTupleTy st =
  let
    val first = parseAppTy st
    fun loop items =
      if Token.isPunct "*" (current st) then
        let val _ = advance st
        in loop (parseAppTy st :: items) end
      else
        case List.rev items of
          [] => first
        | rest => Ast.TyTuple (first :: rest)
  in
    loop []
  end

and parseAppTy st =
  let
    val first = parseAtomTy st
    fun loop ty =
      case Token.identOf (current st) of
        SOME name =>
          let val _ = advance st
          in loop (Ast.TyApp (name, ty)) end
      | NONE => ty
  in
    loop first
  end

and parseAtomTy st =
  let val token = current st in
    case Token.tyVarOf token of
      SOME (name, equality) =>
        (advance st; Ast.TyVar (name, equality))
    | NONE =>
    if (case Token.identOf token of SOME "consume" => true | _ => false)
       orelse Token.isPunct "!" token then
      let
        val bang = Token.isPunct "!" token
        val _ = advance st
        val inner = parseAppTy st
      in Ast.TyConsume (bang, inner) end
    else if Token.isKeyword "borrow" token orelse Token.isPunct "&" token then
      let
        val ampersand = Token.isPunct "&" token
        val _ = advance st
        val mutable =
          if Token.isKeyword "mut" (current st) then
            (advance st; true)
          else false
        val inner = parseAppTy st
        val provenance =
          if Token.isKeyword "from" (current st) then
            let
              val _ = advance st
              val token = current st
              val provenance =
                case #kind token of
                  Token.IntLit text =>
                    (case Int.fromString text of
                       SOME value => Ast.ProvIndex value
                     | NONE =>
                         failAt st "E2002" "invalid borrow provenance"
                           (#span token) [] [])
                | Token.Ident text => Ast.ProvName text
                | _ =>
                    failAt st
                      "E2002"
                      ("expected a parameter name or index after `from`, found " ^
                       Token.describe token)
                      (#span token)
                      []
                      []
              val _ = advance st
            in
              SOME provenance
            end
          else NONE
      in
        Ast.TyBorrow (mutable, ampersand, inner, provenance)
      end
    else if Token.isPunct "(" token then
      let
        val _ = advance st
        val first = parseTupleTy st
        fun commaItems acc =
          if Token.isPunct "," (current st) then
            let val _ = advance st
            in commaItems (parseTupleTy st :: acc) end
          else List.rev acc
        val rest = commaItems []
        val _ = expectPunct st ")"
      in
        case rest of
          [] => first
        | _ => Ast.TyTuple (first :: rest)
      end
    else if Token.isPunct "{" token then
      let
        val _ = advance st
        fun fields acc =
          if Token.isPunct "}" (current st) then
            let val close = current st
                val _ = advance st
            in (List.rev acc, close) end
          else
            let
              val (name, _) = expectIdent st "a record field name"
              val _ = expectPunct st ":"
              val ty = parseTy st
            in
              if Token.isPunct "," (current st) then
                (advance st; fields ((name, ty) :: acc))
              else
                let val close = current st
                    val _ = expectPunct st "}"
                in (List.rev ((name, ty) :: acc), close) end
            end
      in
        let val (fieldList, close) = fields []
            val span =
              { start = Token.startOf token
              , stop = Token.stopOf close
              }
            val _ = span
        in
          Ast.TyRecord fieldList
        end
      end
    else
      let
        val name =
          case Token.identOf token of
            SOME text => text
          | NONE =>
              failAt st
                "E2002"
                ("expected a type name, found " ^ Token.describe token)
                (#span token)
                []
                []
      in
        let
          val _ = advance st
          fun segments (qualified, stop) =
            if Token.isPunct "." (current st) then
              case Token.identOf (peek st 1) of
                SOME segment =>
                  let val segmentToken = peek st 1 in
                    advance st;
                    advance st;
                    segments
                      (qualified ^ "." ^ segment,
                       Token.stopOf segmentToken)
                  end
              | NONE => (qualified, stop)
            else (qualified, stop)
          val (qualifiedName, _) = segments (name, Token.stopOf token)
        in
          case qualifiedName of
            "int" => Ast.TyInt 32
          | "int1" => Ast.TyInt 1
          | "int2" => Ast.TyInt 2
          | "int4" => Ast.TyInt 4
          | "int8" => Ast.TyInt 8
          | "int16" => Ast.TyInt 16
          | "int32" => Ast.TyInt 32
          | "int64" => Ast.TyInt 64
          | "float" => Ast.TyFloat 64
          | "float32" => Ast.TyFloat 32
          | "float64" => Ast.TyFloat 64
          | "bool" => Ast.TyBool
          | "unit" => Ast.TyUnit
          | "token" => Ast.TyToken
          | "resource" => Ast.TyResource
          | "string" => Ast.TyString
          | _ => Ast.TyName qualifiedName
        end
      end
  end

end
