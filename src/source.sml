structure Source =
struct
  type t = {text : string, lineStarts : int array}

  fun create text =
    let
      val size = String.size text

      fun countLines (index, count) =
        if index >= size then count
        else if String.sub (text, index) = #"\n" then
          countLines (index + 1, count + 1)
        else
          countLines (index + 1, count)

      val lineCount = countLines (0, 1)
      val starts = Array.array (lineCount, 0)
      val _ = Array.update (starts, 0, 0)

      fun fill (index, line) =
        if index >= size then ()
        else if String.sub (text, index) = #"\n" then
          let
            val _ =
              if line < lineCount then Array.update (starts, line, index + 1)
              else ()
          in
            fill (index + 1, line + 1)
          end
        else
          fill (index + 1, line)

      val _ = fill (0, 1)
    in
      {text = text, lineStarts = starts}
    end

  fun text ({text, ...} : t) = text

  fun lineText ({text, lineStarts} : t, line) =
    let val count = Array.length lineStarts in
      if line < 1 orelse line > count then ""
      else
        let
          val start = Array.sub (lineStarts, line - 1)
          val stop =
            if line < count then Array.sub (lineStarts, line) - 1
            else String.size text
        in
          if stop <= start then ""
          else String.substring (text, start, stop - start)
        end
    end
end
