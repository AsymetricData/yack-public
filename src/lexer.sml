structure Lexer =
struct
  type result =
    { tokens : Token.t list
    , diagnostics : Diag.t list
    , comments : (Diag.span * string) list
    }

  fun isKeyword text =
    case text of
      "fun" => true
    | "rec" => true
    | "val" => true
    | "let" => true
    | "arena" => true
    | "in" => true
    | "end" => true
    | "if" => true
    | "then" => true
    | "else" => true
    | "true" => true
    | "false" => true
    | "fn" => true
    | "type" => true
    | "datatype" => true
    | "case" => true
    | "of" => true
    | "borrow" => true
    | "guard" => true
    | "mut" => true
    | "from" => true
    | _ => false

  fun isPunct2 text =
    case text of
      "<=" => true
    | ">=" => true
    | "<>" => true
    | "=>" => true
    | "->" => true
    | "<-" => true
    | "|>" => true
    | "::" => true
    | "<<" => true
    | ">>" => true
    | _ => false

  fun isPunct1 text =
    String.size text = 1 andalso
    (case String.sub (text, 0) of
       #"(" => true
     | #")" => true
     | #"[" => true
     | #"]" => true
     | #"," => true
     | #"=" => true
     | #"+" => true
     | #"-" => true
     | #"*" => true
     | #"/" => true
     | #"%" => true
     | #"^" => true
     | #"<" => true
     | #">" => true
     | #":" => true
     | #";" => true
     | #"{" => true
     | #"}" => true
     | #"." => true
     | #"|" => true
     | #"&" => true
     | #"!" => true
     | #"@" => true
     | _ => false)

  fun lex source = lexIn (0, source)

  and lexIn (fileId, source) =
    let
      val size = String.size source
      val comments = ref []

      fun charAt i = String.sub (source, i)

      fun isSpace ch =
        ch = #" " orelse ch = #"\t" orelse ch = #"\r" orelse ch = #"\n"

      fun isLetter ch = Char.isAlpha ch orelse ch = #"_"

      fun isIdentChar ch =
        Char.isAlphaNum ch orelse ch = #"_" orelse ch = #"'"

      fun mkPos (_, line, column, offset) =
        {file = fileId, line = line, column = column, offset = offset}

      fun mkSpan (_, l1, c1, o1) (_, l2, c2, o2) =
        { start = {file = fileId, line = l1, column = c1, offset = o1}
        , stop = {file = fileId, line = l2, column = c2, offset = o2}
        }

      fun step (i, line, column, offset) =
        if i >= size then (i, line, column, offset)
        else if charAt i = #"\n" then (i + 1, line + 1, 1, offset + 1)
        else (i + 1, line, column + 1, offset + 1)

      fun scan (state as (i, _, _, _), tokens, diagnostics) =
        if i >= size then
          let
            val point = mkPos state
            val eof = {kind = Token.Eof, span = {start = point, stop = point}}
          in
            { tokens = List.rev (eof :: tokens)
            , diagnostics = List.rev diagnostics
            , comments = List.rev (!comments)
            }
          end
        else
          let
            val ch = charAt i
          in
            if isSpace ch then
              scan (step state, tokens, diagnostics)

            else if ch = #"(" andalso i + 1 < size andalso charAt (i + 1) = #"*" then
              let
                val start = state

                fun skip (state', depth) =
                  let val (j, _, _, _) = state' in
                    if j >= size then
                      let
                        val diagnostic =
                          Diag.error
                            ( "E2001"
                            , "unterminated comment"
                            , mkSpan start state'
                            , []
                            , ["close the comment with `*)`"]
                            )
                      in
                        (state', diagnostic :: diagnostics)
                      end
                    else if charAt j = #"("
                         andalso j + 1 < size
                         andalso charAt (j + 1) = #"*"
                    then
                      skip (step (step state'), depth + 1)
                    else if charAt j = #"*"
                         andalso j + 1 < size
                         andalso charAt (j + 1) = #")"
                    then
                      if depth = 1
                      then (step (step state'), diagnostics)
                      else skip (step (step state'), depth - 1)
                    else
                      skip (step state', depth)
                  end

                val (state', diagnostics') =
                  skip (step (step state), 1)
                val (_, _, _, startOffset) = state
                val (_, _, _, stopOffset) = state'
                val text =
                  String.substring (source, startOffset, stopOffset - startOffset)
                val _ =
                  comments := (mkSpan state state', text) :: !comments
              in
                scan (state', tokens, diagnostics')
              end

            else if ch = #"\"" then
              let
                val start = state
                fun scanString (state', acc, diagnostics) =
                  let val (j, _, _, _) = state' in
                    if j >= size then
                      let
                        val diagnostic =
                          Diag.error
                            ( "E2001"
                            , "unterminated string literal"
                            , mkSpan start state'
                            , []
                            , ["close the string with `\"`"]
                            )
                      in
                        (state', String.implode (List.rev acc),
                         diagnostic :: diagnostics)
                      end
                    else if charAt j = #"\"" then
                      (step state', String.implode (List.rev acc), diagnostics)
                    else if charAt j = #"\\" then
                      let
                        val afterSlash = step state'
                        val (k, _, _, _) = afterSlash
                      in
                        if k >= size then
                          let
                            val diagnostic =
                              Diag.error
                                ( "E2001"
                                , "unterminated escape sequence"
                                , mkSpan start afterSlash
                                , []
                                , []
                                )
                          in
                            (afterSlash, String.implode (List.rev acc),
                             diagnostic :: diagnostics)
                          end
                        else
                          let
                            val escaped = charAt k
                            val afterEscape = step afterSlash
                            val (decoded, diagnostics') =
                              case escaped of
                                #"n" => (#"\n", diagnostics)
                              | #"t" => (#"\t", diagnostics)
                              | #"r" => (#"\r", diagnostics)
                              | #"\\" => (#"\\", diagnostics)
                              | #"\"" => (#"\"", diagnostics)
                              | #"0" => (#"\000", diagnostics)
                              | _ =>
                                  ( escaped
                                  , Diag.error
                                      ( "E2001"
                                      , "unknown string escape `\\" ^
                                        Char.toString escaped ^ "`"
                                      , mkSpan afterSlash afterEscape
                                      , []
                                      , ["use \\n, \\t, \\r, \\\\, \\\" or \\0"]
                                      ) :: diagnostics
                                  )
                          in
                            scanString (afterEscape, decoded :: acc, diagnostics')
                          end
                      end
                    else
                      scanString (step state', charAt j :: acc, diagnostics)
                  end
                val (state', text, diagnostics') = scanString (step state, [], diagnostics)
                val token =
                  { kind = Token.StringLit text
                  , span = mkSpan start state'
                  }
              in
                scan (state', token :: tokens, diagnostics')
              end

            else if Char.isDigit ch then
              let
                fun takeDigits state' =
                  let val (j, _, _, _) = state' in
                    if j < size andalso Char.isDigit (charAt j)
                    then takeDigits (step state')
                    else state'
                  end

                fun takeExponent state' =
                  let val (j, _, _, _) = state' in
                    if j < size andalso
                       (charAt j = #"e" orelse charAt j = #"E") then
                      let
                        val afterE = step state'
                        val (j1, _, _, _) = afterE
                        val afterSign =
                          if j1 < size andalso
                             (charAt j1 = #"+" orelse charAt j1 = #"-")
                          then step afterE
                          else afterE
                        val (j2, _, _, _) = afterSign
                      in
                        if j2 < size andalso Char.isDigit (charAt j2)
                        then takeDigits afterSign
                        else state'
                      end
                    else state'
                  end
                val afterInteger = takeDigits (step state)
                val (integerEnd, _, _, _) = afterInteger
                val hasFraction =
                  integerEnd + 1 < size andalso
                  charAt integerEnd = #"." andalso
                  Char.isDigit (charAt (integerEnd + 1))
                val (state', isFloat) =
                  if hasFraction then
                    let
                      val afterDot = step afterInteger
                      val afterFraction = takeDigits afterDot
                      val afterExponent = takeExponent afterFraction
                    in
                      (afterExponent, true)
                    end
                  else
                    let val afterExponent = takeExponent afterInteger
                    in
                      (afterExponent, afterExponent <> afterInteger)
                    end
                val (j, _, _, _) = state'
                val text = String.substring (source, i, j - i)
                val token =
                  { kind =
                      if isFloat then Token.FloatLit text
                      else Token.IntLit text
                  , span = mkSpan state state'
                  }
              in
                scan (state', token :: tokens, diagnostics)
              end

            else if ch = #"'" then
              let
                val equality =
                  i + 1 < size andalso charAt (i + 1) = #"'"
                val nameIndex = if equality then i + 2 else i + 1
                fun advanceBy (state', count) =
                  if count <= 0 then state'
                  else advanceBy (step state', count - 1)
                val nameStart = advanceBy (state, if equality then 2 else 1)
                fun takeName state' =
                  let val (j, _, _, _) = state' in
                    if j < size andalso isIdentChar (charAt j)
                    then takeName (step state')
                    else state'
                  end
                val state' = takeName nameStart
                val (j, _, _, _) = state'
              in
                if nameIndex < size andalso isLetter (charAt nameIndex) then
                  let
                    val text = String.substring (source, nameIndex, j - nameIndex)
                    val token =
                      { kind = Token.TyVar (text, equality)
                      , span = mkSpan state state'
                      }
                  in
                    scan (state', token :: tokens, diagnostics)
                  end
                else
                  let
                    val diagnostic =
                      Diag.error
                        ( "E2001"
                        , "expected a type variable name after `'`"
                        , mkSpan state state'
                        , []
                        , []
                        )
                  in
                    scan (state', tokens, diagnostic :: diagnostics)
                  end
              end

            else if isLetter ch then
              let
                fun takeIdent state' =
                  let val (j, _, _, _) = state' in
                    if j < size andalso isIdentChar (charAt j)
                    then takeIdent (step state')
                    else state'
                  end

                val state' = takeIdent (step state)
                val (j, _, _, _) = state'
                val text = String.substring (source, i, j - i)
                val kind =
                  if isKeyword text
                  then Token.Keyword text
                  else Token.Ident text
                val token = {kind = kind, span = mkSpan state state'}
              in
                scan (state', token :: tokens, diagnostics)
              end

            else
              let
                val two =
                  if i + 1 < size
                  then String.substring (source, i, 2)
                  else ""
                val one = String.str ch
              in
                if isPunct2 two then
                  let
                    val state' = step (step state)
                    val token =
                      { kind = Token.Punct two
                      , span = mkSpan state state'
                      }
                  in
                    scan (state', token :: tokens, diagnostics)
                  end
                else if isPunct1 one then
                  let
                    val state' = step state
                    val token =
                      { kind = Token.Punct one
                      , span = mkSpan state state'
                      }
                  in
                    scan (state', token :: tokens, diagnostics)
                  end
                else
                  let
                    val state' = step state
                    val diagnostic =
                      Diag.error
                        ( "E2001"
                        , "unexpected character `" ^ one ^ "`"
                        , mkSpan state state'
                        , []
                        , []
                        )
                  in
                    scan (state', tokens, diagnostic :: diagnostics)
                  end
              end
          end
    in
      scan ((0, 1, 1, 0), [], [])
    end
end
