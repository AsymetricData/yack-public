structure LspProtocol =
struct
  fun obj fields = Json.Object fields
  fun arr values = Json.Array values
  fun str text = Json.String text

  fun int value =
    if value < 0 then Json.Number ("-" ^ Int.toString (~ value))
    else Json.Number (Int.toString value)
  fun bool value = Json.Bool value
  val jsonNull = Json.Null

  fun member name (Json.Object fields) =
        (case List.find (fn (candidate, _) => candidate = name) fields of
           SOME (_, value) => SOME value
         | NONE => NONE)
    | member _ _ = NONE

  fun required name value =
    case member name value of
      SOME found => found
    | NONE => raise Fail ("missing field `" ^ name ^ "`")

  fun stringOf value =
    case Json.asString value of
      SOME text => text
    | NONE => raise Fail "expected a JSON string"

  fun intOf value =
    case Json.asInt value of
      SOME number => number
    | NONE => raise Fail "expected a JSON integer"

  fun readByte stream =
    case TextIO.input1 stream of
      NONE => NONE
    | SOME character => SOME (Char.ord character)

  fun readLine stream =
    let
      fun loop bytes =
        case readByte stream of
          NONE => if List.null bytes then NONE else SOME (List.rev bytes)
        | SOME byte =>
            if byte = 10 then
              SOME (List.rev bytes)
            else
              loop (byte :: bytes)
      fun toText bytes =
        let
          val text = String.implode (List.map Char.chr bytes)
        in
          if String.isSuffix "\r" text then
            String.substring (text, 0, String.size text - 1)
          else text
        end
    in
      case loop [] of
        NONE => NONE
      | SOME bytes => SOME (toText bytes)
    end

  fun lowercase text =
    String.map (fn character => Char.toLower character) text

  fun contentLength line =
    let
      val lower = lowercase line
      val prefix = "content-length:"
    in
      if String.isPrefix prefix lower then
        let
          val value =
            String.extract (line, String.size prefix, NONE)
          val value = String.map (fn c => if c = #" " then #" " else c) value
          fun trimLeft text =
            if String.size text > 0 andalso
               (String.sub (text, 0) = #" " orelse
                String.sub (text, 0) = #"\t")
            then trimLeft (String.substring (text, 1, String.size text - 1))
            else text
          val value = trimLeft value
        in
          Int.fromString value
        end
      else NONE
    end

  fun readMessage stream =
    let
      fun headers length =
        case readLine stream of
          NONE => NONE
        | SOME "" =>
            (case length of
               SOME bytes => readBody bytes
             | NONE => raise Fail "missing Content-Length header")
        | SOME line =>
            (case contentLength line of
               SOME bytes => headers (SOME bytes)
             | NONE => headers length)
      and readBody count =
        let
          fun loop (remaining, bytes) =
            if remaining = 0 then String.implode (List.map Char.chr (List.rev bytes))
            else
              case readByte stream of
                NONE => raise Fail "unexpected end of message body"
              | SOME byte => loop (remaining - 1, byte :: bytes)
        in
          SOME (loop (count, []))
        end
    in
      headers NONE
    end

  fun writeMessage value =
    let
      val body = Json.encode value
      val header =
        "Content-Length: " ^ Int.toString (String.size body) ^ "\r\n\r\n"
    in
      TextIO.output (TextIO.stdOut, header ^ body);
      TextIO.flushOut TextIO.stdOut
    end

  fun send value = writeMessage value

  fun respond id result =
    send
      (obj
         [ ("jsonrpc", str "2.0")
         , ("id", id)
         , ("result", result)
         ])

  fun respondError id code message =
    send
      (obj
         [ ("jsonrpc", str "2.0")
         , ("id", id)
         , ("error", obj [ ("code", int code), ("message", str message) ])
         ])

  fun notify method params =
    send
      (obj
         [ ("jsonrpc", str "2.0")
         , ("method", str method)
         , ("params", params)
         ])
end
