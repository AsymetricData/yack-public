structure LspText =
struct
  open LspProtocol

  fun hexValue character =
    let val value = Char.ord character in
      if value >= Char.ord #"0" andalso value <= Char.ord #"9" then
        value - Char.ord #"0"
      else if value >= Char.ord #"a" andalso value <= Char.ord #"f" then
        value - Char.ord #"a" + 10
      else if value >= Char.ord #"A" andalso value <= Char.ord #"F" then
        value - Char.ord #"A" + 10
      else 0
    end

  fun percentDecode text =
    let
      val size = String.size text
      fun loop (index, chunks) =
        if index >= size then String.concat (List.rev chunks)
        else if String.sub (text, index) = #"%" andalso index + 2 < size then
          let
            val high = hexValue (String.sub (text, index + 1))
            val low = hexValue (String.sub (text, index + 2))
          in
            loop (index + 3, String.str (Char.chr (high * 16 + low)) :: chunks)
          end
        else loop (index + 1, String.str (String.sub (text, index)) :: chunks)
    in
      loop (0, [])
    end

  fun uriToPath uri =
    if String.isPrefix "file://" uri then
      percentDecode (String.extract (uri, 7, NONE))
    else uri

  fun isUriSafe character =
    Char.isAlphaNum character orelse
    character = #"/" orelse character = #"-" orelse
    character = #"_" orelse character = #"." orelse
    character = #"~"

  fun hexDigit value =
    if value < 10 then Char.chr (Char.ord #"0" + value)
    else Char.chr (Char.ord #"A" + value - 10)

  fun percentEncode text =
    let
      val size = String.size text
      fun loop (index, chunks) =
        if index >= size then String.concat (List.rev chunks)
        else
          let val character = String.sub (text, index) in
            if isUriSafe character then
              loop (index + 1, String.str character :: chunks)
            else
              let val byte = Char.ord character in
                loop
                  ( index + 1
                  , ("%" ^ String.implode
                       [hexDigit (byte div 16), hexDigit (byte mod 16)])
                    :: chunks
                  )
              end
          end
    in
      loop (0, [])
    end

  fun absolutePath path =
    let
      val path =
        if String.size path > 0 andalso String.sub (path, 0) = #"/" then path
        else OS.Path.concat (OS.FileSys.getDir (), path)
    in
      OS.FileSys.realPath path handle _ => path
    end

  fun utf16Units text limit =
    let
      val size = Int.min (limit, String.size text)
      fun byteAt index = if index < size then Char.ord (String.sub (text, index)) else 0
      fun loop (index, units) =
        if index >= size then units
        else
          let val byte = byteAt index in
            if byte < 0x80 then loop (index + 1, units + 1)
            else if byte < 0xE0 then loop (index + 2, units + 1)
            else if byte < 0xF0 then loop (index + 3, units + 1)
            else loop (index + 4, units + 2)
          end
    in
      loop (0, 0)
    end

  fun byteColumnToCharacter (lineText, column) =
    utf16Units lineText (Int.max (0, column - 1))

  fun characterToByteColumn (lineText, character) =
    let
      val size = String.size lineText
      fun loop (index, units) =
        if index >= size then index
        else if units >= character then index
        else
          let val byte = Char.ord (String.sub (lineText, index)) in
            if byte < 0x80 then loop (index + 1, units + 1)
            else if byte < 0xE0 then loop (index + 2, units + 1)
            else if byte < 0xF0 then loop (index + 3, units + 1)
            else if units + 2 > character then index
            else loop (index + 4, units + 2)
          end
    in
      loop (0, 0)
    end

  fun entryForFile (sources : Driver.sourceEntry list) fileId =
    case List.find (fn entry => #id entry = fileId) sources of
      SOME entry => SOME entry
    | NONE => (case sources of entry :: _ => SOME entry | [] => NONE)

  fun lspPosition (entry : Driver.sourceEntry) (position : Pos.t) =
    let
      val lineText = Source.lineText (#map entry, Pos.line position)
      val character =
        byteColumnToCharacter (lineText, Pos.column position)
    in
      obj
        [ ("line", int (Pos.line position - 1))
        , ("character", int character)
        ]
    end

  fun lspRange (entry : Driver.sourceEntry) (span : Diag.span) =
    obj
      [ ("start", lspPosition entry (#start span))
      , ("end", lspPosition entry (#stop span))
      ]

  fun sameSpan (left : Diag.span, right : Diag.span) =
    Pos.file (#start left) = Pos.file (#start right) andalso
    Pos.offset (#start left) = Pos.offset (#start right) andalso
    Pos.offset (#stop left) = Pos.offset (#stop right)

  fun positionLE ((lineA, columnA), (lineB, columnB)) =
    lineA < lineB orelse (lineA = lineB andalso columnA <= columnB)

  fun spanContains (span : Diag.span) (line, column) =
    let
      val start = #start span
      val stop = #stop span
    in
      positionLE ((Pos.line start, Pos.column start), (line, column)) andalso
      positionLE ((line, column), (Pos.line stop, Pos.column stop))
    end

  fun positionOfOffset (entry : Driver.sourceEntry) offset =
    let
      val text = Source.text (#map entry)
      val size = String.size text
      val target = Int.max (0, Int.min (offset, size))
      fun loop (line, lineStart) =
        let
          val lineText = Source.lineText (#map entry, line)
          val lineEnd = lineStart + String.size lineText
        in
          if target <= lineEnd orelse lineStart >= size then
            ( line
            , byteColumnToCharacter (lineText, target - lineStart + 1)
            )
          else loop (line + 1, lineEnd + 1)
        end
    in
      loop (1, 0)
    end

  fun lspPositionOfOffset (entry : Driver.sourceEntry) offset =
    let val (line, character) = positionOfOffset entry offset in
      obj [ ("line", int (line - 1)), ("character", int character) ]
    end
  fun trim text =
    let
      val size = String.size text
      fun isSpace character = Char.isSpace character
      fun left index =
        if index < size andalso isSpace (String.sub (text, index))
        then left (index + 1) else index
      fun right index =
        if index >= 0 andalso isSpace (String.sub (text, index))
        then right (index - 1) else index
      val first = left 0
      val last = right (size - 1)
    in
      if last < first then ""
      else String.substring (text, first, last - first + 1)
    end

  fun isIdentifierChar character =
    Char.isAlphaNum character orelse character = #"_" orelse character = #"'"

  fun wordBoundsAt (lineText, index) =
    let
      val size = String.size lineText
      val anchor =
        if index < size andalso isIdentifierChar (String.sub (lineText, index))
        then index
        else if index > 0 andalso
                isIdentifierChar (String.sub (lineText, index - 1))
        then index - 1
        else size
      fun left position =
        if position > 0 andalso
           isIdentifierChar (String.sub (lineText, position - 1))
        then left (position - 1) else position
      fun right position =
        if position < size andalso
           isIdentifierChar (String.sub (lineText, position))
        then right (position + 1) else position
    in
      if anchor >= size then NONE
      else
        let val start = left anchor in
          SOME (start, right anchor)
        end
    end

  fun qualifiedWordAt (lineText, index) =
    case wordBoundsAt (lineText, index) of
      NONE => (NONE, "")
    | SOME (start, stop) =>
        let
          val base = String.substring (lineText, start, stop - start)
          fun left position =
            if position > 0 andalso
               isIdentifierChar (String.sub (lineText, position - 1))
            then left (position - 1) else position
          fun loop (position, parts) =
            if position > 0 andalso
               String.sub (lineText, position - 1) = #"." then
              let
                val qualifierStop = position - 1
                val qualifierStart = left qualifierStop
              in
                if qualifierStart = qualifierStop then parts
                else
                  loop
                    ( qualifierStart
                    , String.substring
                        (lineText, qualifierStart, qualifierStop - qualifierStart)
                      :: parts
                    )
              end
            else parts
          val parts = loop (start, [])
        in
          (if List.null parts then NONE
           else SOME (String.concatWith "." parts), base)
        end

  fun offsetOfPosition (text, line, character) =
    let
      val size = String.size text
      fun lineStart (index, current) =
        if current >= line orelse index >= size then index
        else if String.sub (text, index) = #"\n" then
          lineStart (index + 1, current + 1)
        else lineStart (index + 1, current)
      val start = lineStart (0, 0)
      fun lineStop index =
        if index >= size then size
        else if String.sub (text, index) = #"\n" then index
        else lineStop (index + 1)
      val stop = lineStop start
      val lineText = String.substring (text, start, stop - start)
    in
      start + characterToByteColumn (lineText, character)
    end

  fun applyChange (text, change) =
    let
      val replacement = getOpt (Json.stringField "text" change, "")
    in
      case Json.field "range" change of
        NONE => replacement
      | SOME range =>
          let
            fun endOffset key =
              let
                val position =
                  case Json.field key range of
                    SOME value => value
                    | NONE => Json.Object []
                val line = getOpt (Json.intField "line" position, 0)
                val character = getOpt (Json.intField "character" position, 0)
              in
                offsetOfPosition (text, line, character)
              end
            val size = String.size text
            val start = Int.max (0, Int.min (size, endOffset "start"))
            val stop = Int.max (start, Int.min (size, endOffset "end"))
          in
            String.substring (text, 0, start) ^ replacement ^
            String.substring (text, stop, size - stop)
          end
    end

  fun hashText text =
    let
      val size = String.size text
      fun loop (index, hash) =
        if index >= size then hash
        else
          loop
            ( index + 1
            , (hash * 0w16777619 + Word.fromInt (Char.ord (String.sub (text, index))))
            )
    in
      Word.toIntX (loop (0, 0w2166136261))
    end

  fun applyChanges (text, changes) =
    List.foldl (fn (change, current) => applyChange (current, change))
      text changes

end
