structure ParserBody =
struct
  open ParserState
  open ParserType
  open ParserDecl

fun parseFunctionDeclaration st =
  let
    val startToken = current st
    val leadingRec = Token.isKeyword "rec" startToken
    val _ = advance st

    val declaredRec =
      if leadingRec then true
      else if Token.isKeyword "rec" (current st) then (advance st; true)
      else false
    val (name, nameSpan) = expectIdent st "a function name"
    val seen = StringSet.create 16
    fun patternNames pattern =
      case pattern of
        Ast.PVar (paramName, span) => [(paramName, span)]
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
    fun checkNames pattern =
      List.app
        (fn (paramName, span) =>
          if StringSet.insert (seen, paramName) then ()
          else
            report st
              "E3008"
              ("duplicate parameter `" ^ paramName ^ "`")
              span
              []
              [])
        (patternNames pattern)
    val params =
      if Token.isPunct "(" (current st) then
        let
          val _ = advance st
          fun loop acc =
            let
              val pattern = parsePattern st
              val _ = checkNames pattern
              val annot =
                if Token.isPunct ":" (current st) then
                  (advance st; SOME (parseTy st))
                else NONE
            in
              if Token.isPunct "," (current st) then
                (advance st; loop ((pattern, annot) :: acc))
              else
                (expectPunct st ")"
               ; List.rev ((pattern, annot) :: acc))
            end
        in
          if Token.isPunct ")" (current st) then (advance st; [])
          else loop []
        end
      else
        failAt st
          "E1004"
          ("function declaration requires a parameter list")
          (#span (current st))
          []
          ["write `fun " ^ name ^ " () = ...`; partial application and currying are not supported"]
    val result =
      if Token.isPunct ":" (current st) then (advance st; SOME (parseTy st))
      else NONE
    val _ = expectPunct st "="
    val body = parseExpr st
    val span =
      { start = Token.startOf startToken
      , stop = #stop (Ast.exprSpan body)
      }
  in
    Ast.DFunClause
      { name = name
      , nameSpan = nameSpan
      , params = params
      , result = result
      , recursive = declaredRec
      , body = body
      , span = span
      }
  end

and parseExternDeclaration st =
  let
    val startToken = current st
    val _ = advance st
  in
    if (case Token.identOf (current st) of
          SOME "record" => true
        | _ => false) then
      parseExternRecord st startToken
    else parseExternFunction st startToken
  end

and parseExternFunction st startToken =
  let
    val _ = expectKeyword st "fun"
    val (name, nameSpan) = expectIdent st "an extern function name"
    val _ = expectPunct st "("
    val params = parseParams st
    val _ =
      List.app
        (fn param =>
          case #annot param of
            SOME _ => ()
          | NONE =>
              report st
                "E2002"
                ("extern parameter `" ^ #name param ^
                 "` needs a type annotation")
                (#span param)
                ["the libc signature cannot be inferred from a body"]
                [ "write `" ^ #name param ^ " : int` or `" ^ #name param ^
                  " : int64`" ])
        params
    val _ = expectPunct st ":"
    val result = parseTy st
    val _ = expectPunct st "="
    val symbolToken = current st
    val symbol =
      case #kind symbolToken of
        Token.StringLit text => (advance st; text)
      | _ =>
          failAt st
            "E2002"
            ("expected the C symbol as a string, found " ^
             Token.describe symbolToken)
            (#span symbolToken)
            []
            ["write `= \"clock_gettime\"`"]
    val _ =
      if validCSymbol symbol then ()
      else
        failAt st
          "E2002"
          ("`" ^ symbol ^ "` is not a C symbol")
          (#span symbolToken)
          []
          ["use letters, digits and underscores, starting with a letter or `_`"]
    val span =
      { start = Token.startOf startToken
      , stop = Token.stopOf (previousToken st)
      }
  in
    Ast.DFun
      { name = name
      , nameSpan = nameSpan
      , params = params
      , result = SOME result
      , recursive = false
      , body =
          Ast.EExtern
            { symbol = symbol
            , params = params
            , result = result
            , span = span
            }
      , span = span
      }
  end

and validCSymbol symbol =
  let
    val size = String.size symbol
    fun isStart ch = Char.isAlpha ch orelse ch = #"_"
    fun isPart ch = Char.isAlphaNum ch orelse ch = #"_"
  in
    size > 0 andalso isStart (String.sub (symbol, 0)) andalso
    List.all (fn index => isPart (String.sub (symbol, index)))
      (List.tabulate (size - 1, fn index => index + 1))
  end

and expectIntLiteral st context =
  let val token = current st in
    case #kind token of
      Token.IntLit text =>
        (case Int.fromString text of
           SOME value => (advance st; value)
         | NONE =>
             failAt st
               "E2002"
               ("invalid integer for " ^ context)
               (#span token)
               []
               [])
    | _ =>
        failAt st
          "E2002"
          ("expected an integer for " ^ context ^ ", found " ^
           Token.describe token)
          (#span token)
          []
          []
  end

and parseExternRecord st startToken =
  let
    val (recordName, _) = expectIdent st "`record`"
    val _ =
      if recordName = "record" then ()
      else
        failAt st
          "E2002"
          ("expected `record`, found `" ^ recordName ^ "`")
          (#span startToken)
          []
          ["write `extern record Name (size = 16) { ... }`"]
    val (name, nameSpan) = expectIdent st "an extern record name"
    val _ = expectPunct st "("
    val (sizeName, _) = expectIdent st "the size field"
    val _ =
      if sizeName = "size" then ()
      else
        failAt st
          "E2002"
          ("expected `size` in an extern record, found `" ^ sizeName ^ "`")
          nameSpan
          []
          ["write `extern record Name (size = 16) { ... }`"]
    val _ = expectPunct st "="
    val size = expectIntLiteral st "the record size"
    val _ =
      if size > 0 then ()
      else
        report st
          "E2002"
          "an extern record needs a positive size"
          nameSpan
          []
          []
    val _ = expectPunct st ")"
    val _ = expectPunct st "{"
    val seen = StringSet.create 16
    fun fields acc =
      if Token.isPunct "}" (current st) then
        let val close = current st
            val _ = advance st
        in (List.rev acc, close) end
      else
        let
          val (field, fieldSpan) = expectIdent st "a field name"
          val _ =
            if StringSet.insert (seen, field) then ()
            else report st "E3013" ("duplicate field `" ^ field ^ "`")
                   fieldSpan [] []
          val _ = expectPunct st ":"
          val ty = parseTy st
          val _ = expectPunct st "@"
          val offset = expectIntLiteral st "a field offset"
          val _ =
            if offset >= 0 then ()
            else report st "E2002" "a field offset cannot be negative"
                   fieldSpan [] []
          val item = (field, ty, offset)
        in
          if Token.isPunct "," (current st) then
            (advance st; fields (item :: acc))
          else
            let val close = current st
                val _ = expectPunct st "}"
            in (List.rev (item :: acc), close) end
        end
    val (fieldList, close) = fields []
    val span =
      { start = Token.startOf startToken
      , stop = Token.stopOf close
      }
  in
    Ast.DExternRecord
      { name = name
      , nameSpan = nameSpan
      , size = size
      , fields = fieldList
      , span = span
      }
  end

and parseParams st =
  if Token.isPunct ")" (current st) then (advance st; [])
  else
    let
      val seen = StringSet.create 256
      fun loop acc =
        let
          val (name, nameSpan) = expectIdent st "a parameter name"
          val (annot, annotStop) =
            if Token.isPunct ":" (current st) then
              let
                val _ = advance st
                val typeToken = current st
                val ty = parseTy st
              in
                (SOME ty, SOME (Token.stopOf typeToken))
              end
            else (NONE, NONE)
          val stop =
            case annotStop of
              SOME position => position
            | NONE => #stop nameSpan
          val span = {start = #start nameSpan, stop = stop}
          val _ =
            if StringSet.insert (seen, name) then ()
            else
              report st
                "E3008"
                ("duplicate parameter `" ^ name ^ "`")
                span
                []
                []
          val param = {name = name, annot = annot, span = span}
        in
          if Token.isPunct "," (current st) then (advance st; loop (param :: acc))
          else (expectPunct st ")"; List.rev (param :: acc))
        end
    in
      loop []
    end

and parseExpr st = parsePipe st

and parsePipe st =
  let
    val first = parseCompare st
    fun loop left =
      if Token.isPunct "|>" (current st) then
        let
          val operatorToken = current st
          val _ = advance st
          val right = parseCompare st
          val span =
            { start = #start (Ast.exprSpan left)
            , stop = #stop (Ast.exprSpan right)
            }
          val _ =
            case right of
              Ast.EVar _ => ()
            | Ast.ECall _ => ()
            | Ast.EField _ => ()
            | _ =>
                report st
                  "E2002"
                  ("the right operand of `|>` must be a function call or a function name")
                  (#span operatorToken)
                  []
                  ["write `value |> f (argument)`"]
        in
          loop (Ast.EBin ("|>", left, right, span))
        end
      else left
  in
    loop first
  end

and parseCompare st =
  let
    val first = parseCons st
    fun loop left =
      let val token = current st in
        case #kind token of
          Token.Punct operator =>
            if List.exists (fn candidate => candidate = operator)
                 ["=", "<>", "<", "<=", ">", ">="]
            then
              let
                val _ = advance st
                val right = parseCons st
                val span =
                  { start = #start (Ast.exprSpan left)
                  , stop = #stop (Ast.exprSpan right)
                  }
              in
                loop (Ast.EBin (operator, left, right, span))
              end
            else left
        | _ => left
      end
  in
    loop first
  end

and parseCons st =
  let
    val first = parseAdd st
  in
    if Token.isPunct "::" (current st) then
      let
        val _ = advance st
        val right = parseCons st
        val span =
          { start = #start (Ast.exprSpan first)
          , stop = #stop (Ast.exprSpan right)
          }
      in
        Ast.EBin ("::", first, right, span)
      end
    else first
  end

and parseAdd st =
  let
    val first = parseMul st
    fun loop left =
      let val token = current st in
        case #kind token of
          Token.Punct operator =>
            if operator = "+" orelse operator = "-" orelse operator = "^" then
              let
                val _ = advance st
                val right = parseMul st
                val span =
                  { start = #start (Ast.exprSpan left)
                  , stop = #stop (Ast.exprSpan right)
                  }
              in
                loop (Ast.EBin (operator, left, right, span))
              end
            else left
        | _ => left
      end
  in
    loop first
  end

and parseMul st =
  let
    val first = parseUnary st
    fun loop left =
      let val token = current st in
        case #kind token of
          Token.Punct operator =>
            if operator = "*" orelse operator = "/" orelse operator = "%" then
              let
                val _ = advance st
                val right = parseUnary st
                val span =
                  { start = #start (Ast.exprSpan left)
                  , stop = #stop (Ast.exprSpan right)
                  }
              in
                loop (Ast.EBin (operator, left, right, span))
              end
            else left
        | _ => left
      end
  in
    loop first
  end

and parseUnary st =
  let val token = current st in
    if Token.isPunct "-" token then
      let
        val _ = advance st
        val operand = parseUnary st
        val span =
          { start = Token.startOf token
          , stop = #stop (Ast.exprSpan operand)
          }
      in
        Ast.ENeg (operand, span)
      end
    else parsePostfix st
  end

and parsePostfix st =
  let
    fun loop expression =
      if Token.isPunct "." (current st) then
        let
          val _ = advance st
        in
          case #kind (current st) of
            Token.IntLit text =>
              let
                val indexToken = current st
                val index =
                  case Int.fromString text of
                    SOME value => value
                  | NONE => 0
                val _ = advance st
                val span =
                  { start = #start (Ast.exprSpan expression)
                  , stop = Token.stopOf indexToken
                  }
              in
                loop (Ast.ETupleIndex (index, expression, span))
              end
          | _ =>
              let
                val (field, fieldSpan) = expectIdent st "a field name"
                val span =
                  { start = #start (Ast.exprSpan expression)
                  , stop = #stop fieldSpan
                  }
              in
                loop (Ast.EField (field, expression, span))
              end
        end
      else expression
  in
    loop (parseAtom st)
  end

and parseAtom st =
  let val token = current st in
    case #kind token of
      Token.StringLit text =>
        (advance st; Ast.EString (text, #span token))

    | Token.IntLit text =>
        let
          val _ = advance st
          val value =
            (case Int64.fromString text of
               SOME number => number
             | NONE =>
                 (failAt st "E2002" "integer literal is out of range"
                    (#span token) [] [];
                  Int64.fromInt 0))
            handle Overflow =>
              (failAt st "E2002" "integer literal is out of range"
                 (#span token) [] [];
               Int64.fromInt 0)
        in
          Ast.EInt (value, #span token)
        end

    | Token.FloatLit text =>
        let
          val _ = advance st
          val value =
            case Real.fromString text of
              SOME number => number
            | NONE => 0.0
        in
          Ast.EFloat (value, #span token)
        end

    | Token.Keyword "true" =>
        (advance st; Ast.EBool (true, #span token))

    | Token.Keyword "false" =>
        (advance st; Ast.EBool (false, #span token))

    | Token.Keyword "if" => parseIf st

    | Token.Keyword "let" => parseLet st
    | Token.Keyword "arena" => parseArena st

    | Token.Keyword "case" => parseCase st

    | Token.Keyword "borrow" => parseBorrow st

    | Token.Punct "&" => parseBorrow st

    | Token.Keyword "fn" =>
        failAt st
          "E1001"
          "anonymous functions use `x => body`, not `fn`"
          (#span token)
          []
          ["write `list.map (x => x * 2, values)` or pass a function by name"]

    | Token.Ident name =>
        if !(#allowArrowFun st) andalso Token.isPunct "=>" (peek st 1) then
          let
            val _ = advance st
            val _ = advance st
            val param = {name = name, annot = NONE, span = #span token}
            val body = parseExpr st
            val span =
              { start = Token.startOf token
              , stop = #stop (Ast.exprSpan body)
              }
          in
            Ast.EFn ([param], NONE, body, span)
          end
        else
        let
          val _ = advance st
          fun scanQualified (qualified, offset) =
            if Token.isPunct "." (peek st offset) then
              case Token.identOf (peek st (offset + 1)) of
                SOME segment =>
                  scanQualified (qualified ^ "." ^ segment, offset + 2)
              | NONE => (qualified, offset)
            else (qualified, offset)
          val (qualifiedName, consumed) = scanQualified (name, 0)
          val nextToken = peek st consumed
        in
          if consumed > 0 andalso
             (Token.isPunct "(" nextToken orelse startsAtom nextToken) then
            let
              fun skip 0 = ()
                | skip n = (advance st; skip (n - 1))
              val _ = skip consumed
            in
              parseQualifiedCall st token qualifiedName
            end
          else if consumed > 0 andalso segmentIsUpper qualifiedName then
            let
              val lastToken = peek st (consumed - 1)
              fun skip 0 = ()
                | skip n = (advance st; skip (n - 1))
              val _ = skip consumed
              val span =
                { start = Token.startOf token
                , stop = Token.stopOf lastToken
                }
            in
              Ast.EVar (qualifiedName, span)
            end
          else if Token.isPunct "(" (current st) then
            parseCall st token name
          else if startsAtom (current st) then
            let
              val next = current st
              val span =
                { start = Token.startOf token
                , stop = Token.stopOf next
                }
            in
              failAt st
                "E1003"
                "complete calls are required"
                span
                []
                [ "write `" ^ name ^ " (x)` or `" ^ name ^ " (x, y)`"
                , "partial application and implicit application are not supported"
                ]
            end
          else Ast.EVar (name, #span token)
        end

    | Token.Punct "[" =>
        let
          val startToken = token
          val _ = advance st
          fun elements acc =
            if Token.isPunct "]" (current st) then
              let
                val close = current st
                val _ = advance st
              in (List.rev acc, close) end
            else
              let
                val value = parseExpr st
                val acc = value :: acc
              in
                if Token.isPunct "," (current st) then
                  (advance st; elements acc)
                else
                  let
                    val close = current st
                    val _ = expectPunct st "]"
                  in (List.rev acc, close) end
              end
          val (values, close) = elements []
          val span =
            { start = Token.startOf startToken
            , stop = Token.stopOf close
            }
          val nilSpan =
            { start = Token.startOf close
            , stop = Token.stopOf close
            }
          fun build [] tail = tail
            | build (value :: rest) tail =
                Ast.EBin ("::", value, build rest tail, span)
        in
          build values (Ast.EVar ("Nil", nilSpan))
        end

    | Token.Punct "{" => parseRecord st

    | Token.Punct "(" =>
        let
          fun scan offset depth =
            let val tok = peek st offset in
              if Token.isPunct "(" tok then scan (offset + 1) (depth + 1)
              else if Token.isPunct ")" tok then
                if depth = 1 then Token.isPunct "=>" (peek st (offset + 1))
                else scan (offset + 1) (depth - 1)
              else
                case #kind tok of
                  Token.Eof => false
                | _ => scan (offset + 1) depth
            end
          val arrowParameters = scan 0 0
          val _ = advance st
        in
          if arrowParameters andalso !(#allowArrowFun st) then
            let
              val params = parseParams st
              val _ = expectPunct st "=>"
              val body = parseExpr st
              val span =
                { start = Token.startOf token
                , stop = #stop (Ast.exprSpan body)
                }
            in
              Ast.EFn (params, NONE, body, span)
            end
          else if Token.isPunct ")" (current st) then
            let
              val close = current st
              val _ = advance st
              val span =
                { start = Token.startOf token
                , stop = Token.stopOf close
                }
            in
              Ast.EUnit span
            end
          else
            let
              val first = parseExpr st
              fun loop acc =
                if Token.isPunct "," (current st) then
                  let
                    val _ = advance st
                    val value = parseExpr st
                  in
                    loop (value :: acc)
                  end
                else
                  let
                    val close = current st
                    val _ = expectPunct st ")"
                    val values = first :: List.rev acc
                    val span =
                      { start = Token.startOf token
                      , stop = Token.stopOf close
                      }
                  in
                    case values of
                      [single] => single
                    | _ => Ast.ETuple (values, span)
                  end
            in
              loop []
            end
        end

    | _ =>
        failAt st
          "E2002"
          ("expected an expression, found " ^ Token.describe token)
          (#span token)
          []
          []
  end

and parseBorrow st =
  let
    val startToken = current st
    val ampersand = Token.isPunct "&" startToken
    val _ =
      if Token.isKeyword "borrow" (current st) orelse
         Token.isPunct "&" (current st)
      then advance st
      else
        failAt st
          "E2002"
          ("expected `borrow` or `&`, found " ^ Token.describe (current st))
          (#span (current st))
          []
          []
    val mutable =
      if Token.isKeyword "mut" (current st) then
        (advance st; true)
      else false
    val operand = parsePostfix st
    val span =
      { start = Token.startOf startToken
      , stop = #stop (Ast.exprSpan operand)
      }
  in
    Ast.EBorrow (mutable, ampersand, operand, span)
  end

and parseCase st =
  let
    val startToken = current st
    val _ = expectKeyword st "case"
    val scrutinee = parseExpr st
    val _ = expectKeyword st "of"
    fun branches acc =
      let
        val pattern = parsePattern st
        val guard =
          if Token.isKeyword "if" (current st) then
            let
              val _ = advance st
              val saved = !(#allowArrowFun st)
              val _ = #allowArrowFun st := false
              val condition = parseExpr st
              val _ = #allowArrowFun st := saved
            in
              SOME condition
            end
          else NONE
        val _ = expectPunct st "=>"
        val body = parseExpr st
        val branch = (pattern, guard, body)
      in
        if Token.isPunct "|" (current st) then
          (advance st; branches (branch :: acc))
        else
          (List.rev (branch :: acc), #stop (Ast.exprSpan body))
      end
    val (branchList, stop) = branches []
    val span = {start = Token.startOf startToken, stop = stop}
  in
    Ast.ECase (scrutinee, branchList, span)
  end

and startsPatternAtom token =
  case #kind token of
    Token.IntLit _ => true
  | Token.FloatLit _ => true
  | Token.StringLit _ => true
  | Token.Ident _ => true
  | Token.Keyword "true" => true
  | Token.Keyword "false" => true
  | Token.Punct "(" => true
  | Token.Punct "[" => true
  | Token.Punct "<<" => true
  | _ => false

and parsePattern st = parseConsPattern st

and parseConsPattern st =
  let
    val first = parseStringPattern st
  in
    if Token.isPunct "::" (current st) then
      let
        val _ = advance st
        val rest = parseConsPattern st
        val span =
          { start = #start (Ast.patternSpan first)
          , stop = #stop (Ast.patternSpan rest)
          }
        val pair = Ast.PTuple ([first, rest], span)
      in
        Ast.PConstructor ("Cons", SOME pair, span)
      end
    else first
  end

and parseStringPattern st =
  let
    val first = parsePatternAtom st
  in
    case first of
      Ast.PString (literal, NONE, literalSpan) =>
        if Token.isPunct "^" (current st) then
          let
            val _ = advance st
            val rest = parseStringPattern st
            val span =
              { start = #start literalSpan
              , stop = #stop (Ast.patternSpan rest)
              }
          in
            Ast.PString (literal, SOME rest, span)
          end
        else first
    | _ => first
  end

and parseBytesPattern st =
  let
    val startToken = current st
    val _ = advance st
    fun scan acc =
      case #kind (current st) of
        Token.IntLit text =>
          let
            val value =
              case Int64.fromString text of
                SOME number =>
                  if number < 0 orelse number > 255 then
                    ( failAt st "E2002"
                        "byte pattern literal must be between 0 and 255"
                        (#span (current st)) [] [];
                      Int64.fromInt 0 )
                  else number
              | NONE =>
                  ( failAt st "E2002" "invalid byte pattern literal"
                      (#span (current st)) [] [];
                    Int64.fromInt 0 )
            val _ = advance st
            val character = Char.chr (Int64.toInt value)
          in
            scan (acc ^ String.str character)
          end
      | Token.StringLit text =>
          (advance st; scan (acc ^ text))
      | Token.Punct "," =>
          (advance st; scan acc)
      | _ => acc
    val bytes = scan ""
    val (rest, close) =
      if Token.isPunct ">>" (current st) then
        let val token = current st
            val _ = advance st
        in (NONE, token) end
      else
        let
          val subpattern = parsePattern st
          val token = current st
          val _ = expectPunct st ">>"
        in
          (SOME subpattern, token)
        end
    val span =
      { start = Token.startOf startToken
      , stop = Token.stopOf close
      }
  in
    Ast.PBytes (bytes, rest, span)
  end

and parsePatternAtom st =
  let val token = current st in
    case #kind token of
      Token.Keyword "true" =>
        (advance st; Ast.PBool (true, #span token))
    | Token.Keyword "false" =>
        (advance st; Ast.PBool (false, #span token))
    | Token.IntLit text =>
        let
          val _ = advance st
          val value =
            (case Int64.fromString text of
               SOME number => number
             | NONE =>
                 (failAt st "E2002" "integer literal is out of range"
                    (#span token) [] [];
                  Int64.fromInt 0))
            handle Overflow =>
              (failAt st "E2002" "integer literal is out of range"
                 (#span token) [] [];
               Int64.fromInt 0)
        in
          Ast.PInt (value, #span token)
        end
    | Token.StringLit text =>
        (advance st; Ast.PString (text, NONE, #span token))
    | Token.Punct "<<" => parseBytesPattern st
    | Token.Ident "_" =>
        (advance st; Ast.PWildcard (#span token))
    | Token.Ident name =>
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
          if segmentIsUpper qualifiedName then
            let
              val argument =
                if startsPatternAtom (current st) then
                  SOME (parsePattern st)
                else NONE
              val stop =
                case argument of
                  SOME pattern => #stop (Ast.patternSpan pattern)
                | NONE => #stop (#span token)
              val span = {start = #start (#span token), stop = stop}
            in
              Ast.PConstructor (qualifiedName, argument, span)
            end
          else
            Ast.PVar (qualifiedName, #span token)
        end
    | Token.Punct "[" =>
        let
          val startToken = token
          val _ = advance st
          fun elements acc =
            if Token.isPunct "]" (current st) then
              let
                val close = current st
                val _ = advance st
              in (List.rev acc, close) end
            else
              let
                val value = parsePattern st
                val acc = value :: acc
              in
                if Token.isPunct "," (current st) then
                  (advance st; elements acc)
                else
                  let
                    val close = current st
                    val _ = expectPunct st "]"
                  in (List.rev acc, close) end
              end
          val (values, close) = elements []
          val span =
            { start = Token.startOf startToken
            , stop = Token.stopOf close
            }
          val nilSpan =
            { start = Token.startOf close
            , stop = Token.stopOf close
            }
          fun build [] tail = tail
            | build (value :: rest) tail =
                let val tailPattern = build rest tail in
                  Ast.PConstructor
                    ( "Cons"
                    , SOME (Ast.PTuple ([value, tailPattern], span))
                    , span
                    )
                end
        in
          build values (Ast.PConstructor ("Nil", NONE, nilSpan))
        end
    | Token.Punct "{" =>
        let
          val startToken = current st
          val _ = advance st
          fun fields acc =
            if Token.isPunct "}" (current st) then
              let val close = current st
                  val _ = advance st
              in (List.rev acc, close) end
            else
              let
                val (name, _) = expectIdent st "a record field name"
                val _ = expectPunct st "="
                val subpattern = parsePattern st
                val field = (name, subpattern)
              in
                if Token.isPunct "," (current st) then
                  (advance st; fields (field :: acc))
                else
                  let val close = current st
                      val _ = expectPunct st "}"
                  in (List.rev (field :: acc), close) end
              end
          val (fieldPatterns, close) = fields []
          val span =
            { start = Token.startOf startToken
            , stop = Token.stopOf close
            }
        in
          Ast.PRecord (fieldPatterns, span)
        end

    | Token.Punct "(" =>
        let
          val _ = advance st
          val first = parsePattern st
          fun rest acc =
            if Token.isPunct "," (current st) then
              (advance st; rest (parsePattern st :: acc))
            else List.rev acc
          val restList = rest []
          val close = current st
          val _ = expectPunct st ")"
          val span =
            { start = Token.startOf token
            , stop = Token.stopOf close
            }
        in
          case restList of
            [] => first
          | _ => Ast.PTuple (first :: restList, span)
        end
    | _ =>
        failAt st
          "E2002"
          ("expected a pattern, found " ^ Token.describe token)
          (#span token)
          []
          []
  end

and parseRecord st =
  let
    val startToken = current st
    val _ = expectPunct st "{"
    fun fields acc =
      if Token.isPunct "}" (current st) then
        let val close = current st
            val _ = advance st
        in (List.rev acc, close) end
      else
        let
          val (name, _) = expectIdent st "a field name"
          val _ = expectPunct st "="
          val value = parseExpr st
          val field = (name, value)
        in
          if Token.isPunct "," (current st) then
            (advance st; fields (field :: acc))
          else
            let val close = current st
                val _ = expectPunct st "}"
            in (List.rev (field :: acc), close) end
        end
    val (fields, close) = fields []
    val span =
      { start = Token.startOf startToken
      , stop = Token.stopOf close
      }
  in
    Ast.ERecord (fields, span)
  end

and parseQualifiedCall st nameToken name =
  if Token.isPunct "(" (current st) then
    parseCall st nameToken name
  else
    let
      fun loop acc =
        if startsAtom (current st) then
          let
            val argument = parsePostfix st
            val _ = ignore argument
          in
            loop (argument :: acc)
          end
        else List.rev acc
      val arguments = loop []
      val stop =
        case arguments of
          [] => Token.stopOf nameToken
        | _ => #stop (Ast.exprSpan (List.last arguments))
      val span =
        { start = Token.startOf nameToken
        , stop = stop
        }
    in
      Ast.ECall (name, arguments, span)
    end

and parseCall st nameToken name =
  let
    val _ = expectPunct st "("
    fun parseArgs acc =
      let val argument = parseExpr st in
        if Token.isPunct "," (current st) then
          (advance st; parseArgs (argument :: acc))
        else
          let
            val close = current st
            val _ = expectPunct st ")"
            val span =
              { start = Token.startOf nameToken
              , stop = Token.stopOf close
              }
          in
            Ast.ECall (name, List.rev (argument :: acc), span)
          end
      end
  in
    if Token.isPunct ")" (current st) then
      let
        val close = current st
        val _ = advance st
        val span =
          { start = Token.startOf nameToken
          , stop = Token.stopOf close
          }
      in
        Ast.ECall (name, [], span)
      end
    else parseArgs []
  end

and parseIf st =
  let
    val startToken = current st
    val _ = expectKeyword st "if"
    val condition = parseExpr st
    val _ = expectKeyword st "then"
    val thenBranch = parseExpr st
    val _ = expectKeyword st "else"
    val elseBranch = parseExpr st
    val span =
      { start = Token.startOf startToken
      , stop = #stop (Ast.exprSpan elseBranch)
      }
  in
    Ast.EIf (condition, thenBranch, elseBranch, span)
  end

and parseLet st = parseBlock st "let" false

and parseArena st = parseBlock st "arena" true

and parseBlock st keyword isArena =
  let
    val startToken = current st
    val _ = expectKeyword st keyword
    fun parseBindings acc =
      let val token = current st in
        if Token.isKeyword "guard" token then
          parseBindings (parseGuardBinding st :: acc)
        else if Token.isKeyword "val" token then
          let
            val _ = advance st
            val pattern = parsePattern st
            val annot =
              if Token.isPunct ":" (current st) then (advance st; SOME (parseTy st))
              else NONE
            val _ =
              case (pattern, annot) of
                (Ast.PVar _, _) => ()
              | (_, SOME _) =>
                  report st
                    "E2002"
                    "a type annotation on a destructuring binding is not supported"
                    (Ast.patternSpan pattern)
                    []
                    ["annotate the bound names, or use `val name : type = ...`"]
              | _ => ()
            val _ = expectPunct st "="
            val value = parseExpr st
            val span =
              { start = Token.startOf token
              , stop = #stop (Ast.exprSpan value)
              }
            val binding =
              case pattern of
                Ast.PVar (name, _) =>
                  Ast.BVal
                    { name = name
                    , annot = annot
                    , value = value
                    , span = span
                    }
              | _ =>
                  Ast.BPat
                    { pattern = pattern
                    , value = value
                    , span = span
                    }
          in
            parseBindings (binding :: acc)
          end
        else if Token.isKeyword "fun" token then
          let
            val _ = advance st
            val (name, nameSpan) = expectIdent st "a function name"
            val params =
              if Token.isPunct "(" (current st) then
                (advance st; parseParams st)
              else
                ( failAt st
                    "E1004"
                    "function declaration requires a parameter list"
                    (#span (current st))
                    []
                    ["write `fun " ^ name ^ " () = ...`"]
                ; [] )
            val result =
              if Token.isPunct ":" (current st) then
                (advance st; SOME (parseTy st))
              else NONE
            val _ = expectPunct st "="
            val body = parseExpr st
            val span =
              { start = Token.startOf token
              , stop = #stop (Ast.exprSpan body)
              }
            val binding =
              Ast.BFun
                { name = name
                , nameSpan = nameSpan
                , params = params
                , result = result
                , body = body
                , span = span
                }
          in
            parseBindings (binding :: acc)
          end
        else if null acc then
          failAt st
            "E2002"
            ("an `" ^ keyword ^ "` needs at least one `val` binding")
            (#span token)
            []
            []
        else
          let
            val _ = expectKeyword st "in"
            val body = parseExpr st
            val endToken = current st
            val _ = expectKeyword st "end"
            val span =
              { start = Token.startOf startToken
              , stop = Token.stopOf endToken
              }
          in
            if isArena then
              Ast.EArena (List.rev acc, body, span)
            else
              Ast.ELet (List.rev acc, body, span)
          end
      end
  in
    parseBindings []
  end

and parseGuardBinding st =
  let
    val startToken = current st
    val _ = expectKeyword st "guard"
    val savedIndex = !(#index st)
    val savedDiagnostics = !(#diagnostics st)
    val savedArrowFun = !(#allowArrowFun st)
    fun restore () =
      ( #index st := savedIndex
      ; #diagnostics st := savedDiagnostics
      ; #allowArrowFun st := savedArrowFun
      )
    fun finish (pattern, value) =
      let
        val _ = expectKeyword st "else"
        val fallback = parseExpr st
        val span =
          { start = Token.startOf startToken
          , stop = #stop (Ast.exprSpan fallback)
          }
      in
        Ast.BGuard
          { pattern = pattern
          , value = value
          , fallback = fallback
          , span = span
          }
      end
    fun parseBoolean () =
      let val condition = parseExpr st
      in finish (NONE, condition) end
    val patternAttempt =
      (SOME (parsePattern st)) handle Abort => NONE
  in
    case patternAttempt of
      SOME pattern =>
        if Token.isPunct "<-" (current st) then
          let
            val _ = advance st
            val value = parseExpr st
          in
            finish (SOME pattern, value)
          end
        else
          (restore (); parseBoolean ())
    | NONE =>
        (restore (); parseBoolean ())
  end

and parseImport st =
  let
    val startToken = current st
    val _ = advance st
    val (first, firstSpan) = expectIdent st "a module name"
    fun segments (path, stop) =
      if Token.isPunct "/" (current st) then
        let
          val _ = advance st
          val (segment, segmentSpan) =
            expectIdent st "a module name segment"
        in
          segments (path @ [segment], #stop segmentSpan)
        end
      else (path, stop)
    val (path, stop) = segments ([first], #stop firstSpan)
    fun optionalAlias context stop =
      case Token.identOf (current st) of
        SOME "as" =>
          let
            val _ = advance st
            val (name, nameSpan) = expectIdent st context
          in
            (SOME name, #stop nameSpan)
          end
      | _ => (NONE, stop)
    val (unqualified, stop) =
      if Token.isPunct "." (current st) then
        let
          val _ = advance st
          val _ = expectPunct st "{"
          fun items acc =
            if Token.isPunct "}" (current st) then
              let
                val close = current st
                val _ = advance st
              in
                (List.rev acc, #stop (#span close))
              end
            else
              let
                val importType = Token.isKeyword "type" (current st)
                val _ = if importType then advance st else ()
                val (name, _) = expectIdent st "an imported name"
                val (alias, _) =
                  optionalAlias
                    "an import alias"
                    (#stop (#span (current st)))
                val item =
                  if importType then Ast.ImportType (name, alias)
                  else Ast.ImportValue (name, alias)
                val _ =
                  if Token.isPunct "," (current st) then advance st
                  else if not (Token.isPunct "}" (current st)) then
                    failAt st
                      "E2002"
                      "expected `,` or `}` in an import list"
                      (#span (current st))
                      []
                      []
                  else ()
              in
                items (item :: acc)
              end
          val (items, closeStop) = items []
        in
          (items, closeStop)
        end
      else ([], stop)
    val (alias, stop) = optionalAlias "a module alias" stop
    val _ =
      if Token.isPunct "." (current st) then
        failAt st
          "E2002"
          "`as` must come after the import list"
          (#span (current st))
          []
          ["write `import module.{name} as Alias`"]
      else ()
    val span = { start = Token.startOf startToken, stop = stop }
  in
    Ast.DImport
      { path = path
      , alias = alias
      , unqualified = unqualified
      , span = span
      }
  end

and parseDecl st =
  let val token = current st in
    if Token.isKeyword "fun" token orelse Token.isKeyword "rec" token then
      parseFunctionDeclaration st
    else if Token.isKeyword "type" token then parseTypeDeclaration st
    else if Token.isKeyword "datatype" token then parseDatatypeDeclaration st
    else
      case Token.identOf token of
        SOME "import" => parseImport st
      | SOME "extern" => parseExternDeclaration st
      | SOME "open" =>
          failAt st
            "E2002"
            "`open` is not supported"
            (#span token)
            []
            [ "use `import module.{name}` or a qualified `Module.name`" ]
      | _ =>
          failAt st
            "E2002"
            ("expected a function or type declaration, found " ^
             Token.describe token)
            (#span token)
            []
            ["v0 has module-level `fun` and `type` declarations"]
  end

end
