structure DriverFmt =
struct
  open DriverIo
  open DriverUi

  type fmtOptions =
    { check : bool
    , write : bool
    , width : int
    , path : string option
    , bestEffort : bool
    }

  fun fmtError message =
    ( TextIO.output (TextIO.stdErr, "yack fmt: " ^ message ^ "\n")
    ; 1
    )

  fun fmtUsage () =
    let
      val style = makeStyle (ColorAuto, Posix.FileSys.stdout)
      fun heading textValue = paint (style, "1;36", textValue)
      fun command textValue = paint (style, "1;33", textValue)
      fun description textValue = paint (style, "2", textValue)
    in
      say ("  " ^ mascot style ^ paint (style, "1;36", "yack") ^
           paint (style, "2", "  fmt"));
      say "";
      say ("  " ^ description "Format a Yack source file or standard input.");
      say "";
      say ("  " ^ heading "Usage");
      say ("    " ^ command "yack fmt [options] <file|->");
      say ("    " ^ command "yack fmt [options]");
      say "";
      say ("  " ^ heading "Options");
      say ("    " ^ command "--check, -c");
      say ("        " ^ description "exit 1 when the input is not formatted");
      say ("    " ^ command "--write, -w");
      say ("        " ^ description "rewrite the file in place");
      say ("    " ^ command "--best-effort");
      say ("        " ^ description
             "leave invalid input unchanged instead of failing (editors)");
      say ("    " ^ command "--width <n>");
      say ("        " ^ description
             ("target line width (default " ^ Int.toString Fmt.defaultWidth ^ ")"));
      say ("    " ^ command "--help");
      say ("        " ^ description "print this help");
      say "";
      say ("  " ^ heading "Examples");
      say ("    " ^ command "yack fmt main.yack");
      say ("    " ^ command "yack fmt - < main.yack");
      say ("    " ^ command "yack fmt --check main.yack");
      say ("    " ^ command "yack fmt --write main.yack");
      say "";
      TextIO.flushOut TextIO.stdOut
    end

  fun formatInput label source (options : fmtOptions) =
    let
      val result = Fmt.format { source = source, width = #width options }
      val diagnostics = #diagnostics result
    in
      if not (null diagnostics) then
        if #bestEffort options then
          if #check options then 0
          else if #write options then
            (case #path options of
               NONE => 0
             | SOME "-" => 0
             | SOME path =>
                 if DriverIo.writeText path source then 0 else 1)
          else
            (TextIO.output (TextIO.stdOut, source); 0)
        else
          (renderAll label source diagnostics; 1)
      else
        let val output = #output result in
          if #check options then
            if output = source then 0
            else
              ( TextIO.output
                  (TextIO.stdErr, label ^ ": not formatted\n")
              ; 1
              )
          else if #write options then
            (case #path options of
               NONE => fmtError "--write needs a file"
             | SOME "-" => fmtError "--write cannot be used with standard input"
             | SOME path =>
                 if DriverIo.writeText path output then 0
                 else
                   ( TextIO.output
                       (TextIO.stdErr, "yack: cannot write `" ^ path ^ "`\n")
                   ; 1
                   ))
          else
            (TextIO.output (TextIO.stdOut, output); 0)
        end
    end

  fun fmtCommand arguments =
    let
      val check = ref false
      val write = ref false
      val bestEffort = ref false
      val width = ref Fmt.defaultWidth
      val path = ref (NONE : string option)
      exception Help
      exception Invalid of string
      fun setPath value =
        case !path of
          NONE => path := SOME value
        | SOME _ => raise Invalid "expected a single input file"
      fun parseWidth value rest =
        case Int.fromString value of
          SOME number => (width := number; loop rest)
        | NONE => raise Invalid ("invalid width `" ^ value ^ "`")
      and loop rest =
        case rest of
          [] => ()
        | option :: more =>
            if option = "--help" orelse option = "-h" then raise Help
            else if option = "--check" orelse option = "-c" then
              (check := true; loop more)
            else if option = "--write" orelse option = "-w" then
              (write := true; loop more)
            else if option = "--best-effort" then
              (bestEffort := true; loop more)
            else if option = "--width" then
              (case more of
                 value :: remaining => parseWidth value remaining
               | [] => raise Invalid "--width needs a number")
            else if String.isPrefix "--width=" option then
              parseWidth (String.extract (option, 8, NONE)) more
            else if option = "-" then
              (setPath option; loop more)
            else if String.isPrefix "-" option then
              raise Invalid ("unknown option `" ^ option ^ "`")
            else
              (setPath option; loop more)
      fun run () =
        let
          val options : fmtOptions =
            { check = !check
            , write = !write
            , width = !width
            , path = !path
            , bestEffort = !bestEffort
            }
        in
          if #check options andalso #write options then
            fmtError "--check and --write are mutually exclusive"
          else
            case #path options of
              NONE => formatInput "<stdin>" (TextIO.inputAll TextIO.stdIn) options
            | SOME "-" =>
                formatInput "<stdin>" (TextIO.inputAll TextIO.stdIn) options
            | SOME file =>
                (case readFile file of
                   NONE =>
                     ( TextIO.output
                         (TextIO.stdErr, "yack: cannot read `" ^ file ^ "`\n")
                     ; 1
                     )
                 | SOME source => formatInput file source options)
        end
    in
      ( loop arguments
      ; run ()
      )
      handle Help => (fmtUsage (); 0)
           | Invalid message => fmtError message
    end

end
