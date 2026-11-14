structure DriverUi =
struct
  val version = "yack 0.2.0 (frontend + types)"

  val glyphMascot = "\240\159\166\172"
  val glyphCheck = "\226\156\148"
  val glyphCross = "\226\156\150"
  val glyphArrow = "\226\134\146"
  val glyphBullet = "\226\128\162"
  val glyphBranch = "\226\148\156\226\148\128\226\148\128"
  val glyphLast = "\226\148\148\226\148\128\226\148\128"
  val glyphTopLeft = "\226\149\173"
  val glyphTopRight = "\226\149\174"
  val glyphBottomLeft = "\226\149\176"
  val glyphBottomRight = "\226\149\175"
  val glyphHorizontal = "\226\148\128"
  val glyphVertical = "\226\148\130"
  val glyphChevron = "\226\157\175"
  val spinnerUnicode =
    [ "\226\160\139", "\226\160\153", "\226\160\185"
    , "\226\160\184", "\226\160\188", "\226\160\180"
    , "\226\160\166", "\226\160\167", "\226\160\135"
    , "\226\160\143"
    ]

  datatype colorMode = ColorAuto | ColorAlways | ColorNever

  type style =
    { color : bool
    , unicode : bool
    }

  fun contains (haystack, needle) =
    let
      val size = String.size haystack
      val width = String.size needle
      fun search i =
        if i + width > size then false
        else if String.substring (haystack, i, width) = needle then true
        else search (i + 1)
    in
      width = 0 orelse search 0
    end

  fun lower text = String.map Char.toLower text

  fun envEquals name value =
    case OS.Process.getEnv name of
      SOME actual => lower actual = lower value
    | NONE => false

  fun envContains name needle =
    case OS.Process.getEnv name of
      SOME actual => contains (lower actual, lower needle)
    | NONE => false

  fun envPresent name =
    case OS.Process.getEnv name of
      SOME value => value <> ""
    | NONE => false

  fun makeStyle (mode, descriptor) =
    let
      val tty = Posix.ProcEnv.isatty descriptor handle _ => false
      val forced =
        case OS.Process.getEnv "FORCE_COLOR" of
          SOME value =>
            value <> "" andalso value <> "0" andalso lower value <> "false"
        | NONE => false
      val color =
        case mode of
          ColorAlways => true
        | ColorNever => false
        | ColorAuto =>
            if envPresent "NO_COLOR" then false
            else if envEquals "CLICOLOR" "0" then false
            else if envEquals "TERM" "dumb" then false
            else if forced then true
            else tty
      val utf8 =
        envContains "LC_ALL" "utf-8" orelse
        envContains "LC_ALL" "utf8" orelse
        envContains "LC_CTYPE" "utf-8" orelse
        envContains "LC_CTYPE" "utf8" orelse
        envContains "LANG" "utf-8" orelse
        envContains "LANG" "utf8"
      val unicode =
        case OS.Process.getEnv "YACK_UNICODE" of
          SOME "0" => false
        | SOME "1" => true
        | _ => tty andalso utf8
    in
      { color = color
      , unicode = unicode
      }
    end

  fun paint (style : style, code, text) =
    if #color style then "\027[" ^ code ^ "m" ^ text ^ "\027[0m"
    else text

  fun glyph (style : style, unicode, ascii) =
    if #unicode style then unicode else ascii

  fun mascot (style : style) = if #unicode style then glyphMascot ^ "  " else ""

  fun say text = TextIO.output (TextIO.stdOut, text ^ "\n")

  fun complain text = TextIO.output (TextIO.stdErr, text ^ "\n")

  fun newError (style : style) headline details =
    let
      val cross = glyph (style, glyphCross, "x")
      val arrow = glyph (style, glyphArrow, "->")
    in
      complain
        (paint (style, "1;31", cross) ^ " " ^ paint (style, "1", headline));
      List.app
        (fn detail =>
          complain ("  " ^ paint (style, "2;36", arrow) ^ " " ^
                    paint (style, "2", detail)))
        details;
      TextIO.flushOut TextIO.stdErr
    end

  fun progressEnabled descriptor =
    let val tty = Posix.ProcEnv.isatty descriptor handle _ => false in
      case OS.Process.getEnv "YACK_PROGRESS" of
        SOME "0" => false
      | SOME "1" => true
      | _ => tty andalso not (envEquals "TERM" "dumb")
    end

  fun elapsed start = Time.toReal (Time.- (Time.now (), start))

  fun formatDuration seconds =
    let val milliseconds = Real.round (seconds * 1000.0) in
      if milliseconds < 1000 then Int.toString milliseconds ^ "ms"
      else Real.fmt (StringCvt.FIX (SOME 2)) seconds ^ "s"
    end

  fun padRight (width, text) =
    if String.size text >= width then text
    else text ^ String.concat
      (List.tabulate (width - String.size text, fn _ => " "))

  fun stepPlain (style : style, stream, label) =
    ( TextIO.output
        ( stream
        , "  " ^ paint (style, "1;32", glyph (style, glyphCheck, "+")) ^ " " ^
          paint (style, "1", label) ^ "\n"
        )
    ; TextIO.flushOut stream
    )

  fun stepTimed (style : style, stream, label, timing) =
    ( TextIO.output
        ( stream
        , "  " ^ paint (style, "1;32", glyph (style, glyphCheck, "+")) ^ " " ^
          paint (style, "1", padRight (38, label)) ^
          paint (style, "2", timing) ^ "\n"
        )
    ; TextIO.flushOut stream
    )

  fun runningStep (style : style, stream, label) =
    ( TextIO.output
        ( stream
        , "  " ^ paint (style, "1;32", glyph (style, glyphArrow, "->")) ^ " " ^
          paint (style, "1", label) ^ "\n"
        )
    ; TextIO.flushOut stream
    )

  fun groupDigits text =
    let
      val size = String.size text
      fun loop (index, acc) =
        if index <= 0 then acc
        else
          let
            val start = Int.max (0, index - 3)
            val chunk = String.substring (text, start, index - start)
          in
            loop (start, if acc = "" then chunk else chunk ^ "," ^ acc)
          end
    in
      loop (size, "")
    end

  fun formatBytes (bytes : Int64.int) =
    let
      val value = Real.fromInt (Int64.toInt bytes) handle _ => 0.0
      fun scaled (unit, factor) =
        Real.fmt (StringCvt.FIX (SOME 1)) (value / factor) ^ " " ^ unit
    in
      if bytes < 1024 then groupDigits (Int64.toString bytes) ^ " B"
      else if bytes < 1048576 then scaled ("KiB", 1024.0)
      else if bytes < 1073741824 then scaled ("MiB", 1048576.0)
      else scaled ("GiB", 1073741824.0)
    end

  fun fileSizeOpt path =
    (SOME (OS.FileSys.fileSize path)) handle _ => NONE

  fun binaryStep (style : style, stream, path, bytes) =
    ( TextIO.output
        ( stream
        , "  " ^ paint (style, "1;32", glyph (style, glyphCheck, "+")) ^ " " ^
          paint (style, "1", padRight (38, "Binary")) ^
          paint (style, "2", formatBytes bytes) ^
          paint (style, "2", " (" ^ groupDigits (Int64.toString bytes) ^
                             " bytes)") ^
          paint (style, "2", " at " ^ path) ^ "\n"
        )
    ; TextIO.flushOut stream
    )

  fun spinnerFrames (style : style) =
    if #unicode style then spinnerUnicode else ["|", "/", "-", "\\"]

  fun eraseLine stream =
    ( TextIO.output (stream, "\r\027[2K")
    ; TextIO.flushOut stream
    )

  fun spinnerShow (style : style, stream, label, frame) =
    ( eraseLine stream
    ; TextIO.output
        ( stream
        , "  " ^ paint (style, "1;36", frame) ^ " " ^
          paint (style, "1", label)
        )
    ; TextIO.flushOut stream
    )

  fun spinnerDone (style : style, stream, label, timing) =
    ( eraseLine stream
    ; stepTimed (style, stream, label, timing)
    )

  fun spinnerFail (style : style, stream, label) =
    ( eraseLine stream
    ; TextIO.output
        ( stream
        , "  " ^ paint (style, "1;31", glyph (style, glyphCross, "x")) ^ " " ^
          paint (style, "1", label) ^ "\n"
        )
    ; TextIO.flushOut stream
    )

end
