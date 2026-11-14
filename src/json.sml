structure Json =
struct
  datatype t =
      Null
    | Bool of bool
    | Number of string
    | String of string
    | Array of t list
    | Object of (string * t) list

  exception ParseError of string

  fun hexDigit value =
    if value < 10 then Char.chr (Char.ord #"0" + value)
    else Char.chr (Char.ord #"a" + value - 10)

  fun hex2 value =
    String.implode [hexDigit (value div 16), hexDigit (value mod 16)]

  fun sequenceWidth (text, index) =
    let
      val size = String.size text
      val byte = Char.ord (String.sub (text, index))
      val width =
        if byte < 0x80 then 1
        else if byte < 0xC2 then 0
        else if byte <= 0xDF then 2
        else if byte <= 0xEF then 3
        else if byte <= 0xF4 then 4
        else 0
      fun continuation position =
        position < size andalso
        let val byte = Char.ord (String.sub (text, position)) in
          byte >= 0x80 andalso byte <= 0xBF
        end
      fun valid (count, position) =
        count >= width orelse (continuation position andalso valid (count + 1, position + 1))
    in
      if width = 0 orelse not (valid (1, index + 1)) then 0 else width
    end

  fun encodeString text =
    let
      val size = String.size text
      fun loop (index, chunks) =
        if index >= size then String.concat (List.rev chunks)
        else
          let val byte = Char.ord (String.sub (text, index)) in
            if byte = 34 then loop (index + 1, "\\\"" :: chunks)
            else if byte = 92 then loop (index + 1, "\\\\" :: chunks)
            else if byte = 8 then loop (index + 1, "\\b" :: chunks)
            else if byte = 12 then loop (index + 1, "\\f" :: chunks)
            else if byte = 10 then loop (index + 1, "\\n" :: chunks)
            else if byte = 13 then loop (index + 1, "\\r" :: chunks)
            else if byte = 9 then loop (index + 1, "\\t" :: chunks)
            else if byte < 32 then
              loop (index + 1, ("\\u00" ^ hex2 byte) :: chunks)
            else if byte < 0x80 then
              loop (index + 1, String.str (String.sub (text, index)) :: chunks)
            else
              let val width = sequenceWidth (text, index) in
                if width = 0 then
                  loop (index + 1, "\\ufffd" :: chunks)
                else
                  loop
                    ( index + width
                    , String.substring (text, index, width) :: chunks
                    )
              end
          end
    in
      "\"" ^ loop (0, []) ^ "\""
    end

  fun encode value =
    let
      fun go Null = "null"
        | go (Bool true) = "true"
        | go (Bool false) = "false"
        | go (Number text) = text
        | go (String text) = encodeString text
        | go (Array values) =
            "[" ^ String.concatWith "," (List.map go values) ^ "]"
        | go (Object fields) =
            let
              fun field (name, value) =
                encodeString name ^ ":" ^ go value
            in
              "{" ^ String.concatWith "," (List.map field fields) ^ "}"
            end
    in
      go value
    end

  fun decode text =
    let
      val size = String.size text
      val index = ref 0

      fun peek () =
        if !index < size then SOME (String.sub (text, !index)) else NONE

      fun take () =
        case peek () of
          SOME character =>
            ( index := !index + 1; character )
        | NONE => raise ParseError "unexpected end of JSON input"

      fun fail message = raise ParseError message

      fun isSpace character =
        character = #" " orelse character = #"\t" orelse
        character = #"\n" orelse character = #"\r"

      fun skipWhitespace () =
        case peek () of
          SOME character =>
            if isSpace character then
              ( index := !index + 1; skipWhitespace () )
            else ()
        | NONE => ()

      fun expect character =
        if take () = character then ()
        else fail ("expected `" ^ String.str character ^ "`")

      fun isDigit character =
        character >= #"0" andalso character <= #"9"

      fun hexValue character =
        let val value = Char.ord character in
          if value >= Char.ord #"0" andalso value <= Char.ord #"9" then
            value - Char.ord #"0"
          else if value >= Char.ord #"a" andalso value <= Char.ord #"f" then
            value - Char.ord #"a" + 10
          else if value >= Char.ord #"A" andalso value <= Char.ord #"F" then
            value - Char.ord #"A" + 10
          else fail "invalid hexadecimal escape"
        end

      fun readHex4 () =
        let
          fun loop (count, value) =
            if count = 4 then value
            else loop (count + 1, value * 16 + hexValue (take ()))
        in
          loop (0, 0)
        end

      fun utf8 (codepoint, chunks) =
        if codepoint < 0x80 then
          String.str (Char.chr codepoint) :: chunks
        else if codepoint < 0x800 then
          String.implode
            [ Char.chr (0xC0 + codepoint div 0x40)
            , Char.chr (0x80 + codepoint mod 0x40)
            ] :: chunks
        else if codepoint < 0x10000 then
          String.implode
            [ Char.chr (0xE0 + codepoint div 0x1000)
            , Char.chr (0x80 + (codepoint div 0x40) mod 0x40)
            , Char.chr (0x80 + codepoint mod 0x40)
            ] :: chunks
        else
          String.implode
            [ Char.chr (0xF0 + codepoint div 0x40000)
            , Char.chr (0x80 + (codepoint div 0x1000) mod 0x40)
            , Char.chr (0x80 + (codepoint div 0x40) mod 0x40)
            , Char.chr (0x80 + codepoint mod 0x40)
            ] :: chunks

      fun readString () =
        let
          fun loop chunks =
            case take () of
              #"\"" => String.concat (List.rev chunks)
            | #"\\" =>
                (case take () of
                   #"\"" => loop ("\"" :: chunks)
                 | #"\\" => loop ("\\" :: chunks)
                 | #"/" => loop ("/" :: chunks)
                 | #"b" => loop ("\b" :: chunks)
                 | #"f" => loop ("\012" :: chunks)
                 | #"n" => loop ("\n" :: chunks)
                 | #"r" => loop ("\r" :: chunks)
                 | #"t" => loop ("\t" :: chunks)
                 | #"u" =>
                     let
                       val first = readHex4 ()
                       val codepoint =
                         if first >= 0xD800 andalso first <= 0xDBFF then
                           case peek () of
                             SOME #"\\" =>
                               ( index := !index + 1
                               ; if take () = #"u" then
                                   let val second = readHex4 () in
                                     if second >= 0xDC00 andalso second <= 0xDFFF
                                     then
                                       0x10000 + (first - 0xD800) * 0x400 +
                                       (second - 0xDC00)
                                     else 0xFFFD
                                   end
                                 else 0xFFFD
                               )
                           | _ => 0xFFFD
                         else if first >= 0xDC00 andalso first <= 0xDFFF then
                           0xFFFD
                         else first
                     in
                       loop (utf8 (codepoint, chunks))
                     end
                 | _ => fail "invalid string escape")
            | character =>
                if Char.ord character < 32 then
                  fail "control character in string"
                else loop (String.str character :: chunks)
        in
          loop []
        end

      fun readNumber () =
        let
          val start = !index
          fun sign () =
            (case peek () of
               SOME #"-" => ( index := !index + 1; () )
             | _ => ())
          fun digits () =
            case peek () of
              SOME character =>
                if isDigit character then
                  ( index := !index + 1; digits () )
                else ()
            | NONE => ()
          fun fraction () =
            case peek () of
              SOME #"." =>
                ( index := !index + 1
                ; (case peek () of
                     SOME character =>
                       if isDigit character then digits ()
                       else fail "expected digit after decimal point"
                   | NONE => fail "expected digit after decimal point")
                )
            | _ => ()
          fun exponent () =
            case peek () of
              SOME character =>
                if character = #"e" orelse character = #"E" then
                  ( index := !index + 1
                  ; (case peek () of
                       SOME sign =>
                         if sign = #"+" orelse sign = #"-" then
                           index := !index + 1
                         else ()
                     | NONE => ())
                  ; (case peek () of
                       SOME digit =>
                         if isDigit digit then digits ()
                         else fail "expected exponent digits"
                     | NONE => fail "expected exponent digits")
                  )
                else ()
            | NONE => ()
        in
          sign ();
          (case peek () of
             SOME character =>
               if isDigit character then digits ()
               else fail "expected number"
           | NONE => fail "expected number");
          fraction ();
          exponent ();
          String.substring (text, start, !index - start)
        end

      fun readValue () =
        let val _ = skipWhitespace () in
          case peek () of
            NONE => fail "expected a value"
          | SOME #"{" => readObject ()
          | SOME #"[" => readArray ()
          | SOME #"\"" =>
              ( index := !index + 1; String (readString ()) )
          | SOME #"t" =>
              if String.isPrefix "true" (String.extract (text, !index, NONE))
              then ( index := !index + 4; Bool true )
              else fail "invalid literal"
          | SOME #"f" =>
              if String.isPrefix "false" (String.extract (text, !index, NONE))
              then ( index := !index + 5; Bool false )
              else fail "invalid literal"
          | SOME #"n" =>
              if String.isPrefix "null" (String.extract (text, !index, NONE))
              then ( index := !index + 4; Null )
              else fail "invalid literal"
          | SOME character =>
              if character = #"-" orelse isDigit character then
                Number (readNumber ())
              else fail ("unexpected character `" ^ String.str character ^ "`")
        end

      and readArray () =
        let
          val _ = expect #"["
          val _ = skipWhitespace ()
        in
          case peek () of
            SOME #"]" => ( index := !index + 1; Array [] )
          | _ =>
              let
                fun loop values =
                  let
                    val value = readValue ()
                    val _ = skipWhitespace ()
                  in
                    case take () of
                      #"," => loop (value :: values)
                    | #"]" => Array (List.rev (value :: values))
                    | _ => fail "expected `,` or `]` in array"
                  end
              in
                loop []
              end
        end

      and readObject () =
        let
          val _ = expect #"{"
          val _ = skipWhitespace ()
        in
          case peek () of
            SOME #"}" => ( index := !index + 1; Object [] )
          | _ =>
              let
                fun loop fields =
                  let
                    val _ = skipWhitespace ()
                    val _ = expect #"\""
                    val name = readString ()
                    val _ = skipWhitespace ()
                    val _ = expect #":"
                    val value = readValue ()
                    val _ = skipWhitespace ()
                  in
                    case take () of
                      #"," => loop ((name, value) :: fields)
                    | #"}" => Object (List.rev ((name, value) :: fields))
                    | _ => fail "expected `,` or `}` in object"
                  end
              in
                loop []
              end
        end

      val value = readValue ()
      val _ = skipWhitespace ()
      val _ =
        if !index <> size then fail "trailing data after JSON value" else ()
    in
      value
    end

  fun parse text =
    decode text
    handle ParseError message => raise ParseError message

  fun tryParse text =
    SOME (parse text) handle _ => NONE

  fun field name (Object fields) =
        (case List.find (fn (candidate, _) => candidate = name) fields of
           SOME (_, value) => SOME value
         | NONE => NONE)
    | field _ _ = NONE

  fun stringField name value =
    case field name value of
      SOME (String text) => SOME text
    | _ => NONE

  fun intField name value =
    case field name value of
      SOME (Number text) => Int.fromString text
    | _ => NONE

  fun boolField name value =
    case field name value of
      SOME (Bool flag) => SOME flag
    | _ => NONE

  fun objectField name value =
    case field name value of
      SOME (Object fields) => SOME (Object fields)
    | _ => NONE

  fun arrayField name value =
    case field name value of
      SOME (Array values) => SOME values
    | _ => NONE

  fun asString (String text) = SOME text
    | asString _ = NONE

  fun asInt (Number text) = Int.fromString text
    | asInt _ = NONE

  fun asObject (Object fields) = SOME (Object fields)
    | asObject _ = NONE

  fun asArray (Array values) = SOME values
    | asArray _ = NONE
end
