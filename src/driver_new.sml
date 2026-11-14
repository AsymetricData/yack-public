structure DriverNew =
struct
  open DriverIo
  open DriverUi

  fun trimTrailingSlashes path =
    if String.size path > 1 andalso String.isSuffix "/" path then
      trimTrailingSlashes
        (String.substring (path, 0, String.size path - 1))
    else path

  fun lastSeparator path =
    let
      fun search i =
        if i < 0 then NONE
        else if String.sub (path, i) = #"/" then SOME i
        else search (i - 1)
    in
      search (String.size path - 1)
    end

  fun baseName path =
    case lastSeparator path of
      NONE => path
    | SOME index =>
        String.substring (path, index + 1, String.size path - index - 1)

  fun parentDir path =
    case lastSeparator path of
      NONE => "."
    | SOME 0 => "/"
    | SOME index => String.substring (path, 0, index)

  fun fileExists path =
    OS.FileSys.access (path, []) handle _ => false

  fun isDirectory path =
    OS.FileSys.isDir path handle _ => false

  fun absolutePath path =
    if OS.Path.isAbsolute path then path
    else OS.Path.concat (OS.FileSys.getDir (), path)

  fun canonicalPath path =
    OS.Path.mkCanonical path handle _ => path

  fun findPackageRoot start =
    let
      fun loop dir =
        if fileExists (OS.Path.concat (dir, "yack.toml")) then SOME dir
        else
          let val parent = parentDir dir
          in if parent = dir then NONE else loop parent end
    in
      loop (canonicalPath (absolutePath start))
    end

  fun splitPath path =
    List.filter
      (fn part => part <> "" andalso part <> ".")
      (String.fields (fn character => character = #"/") path)

  fun relativePath (from, to) =
    let
      val fromParts = splitPath (canonicalPath (absolutePath from))
      val toParts = splitPath (canonicalPath (absolutePath to))
      fun stripCommon (left, right) =
        case (left, right) of
          (x :: xs, y :: ys) =>
            if x = y then stripCommon (xs, ys) else (left, right)
        | _ => (left, right)
      val (fromRest, toRest) = stripCommon (fromParts, toParts)
      val parents = List.tabulate (List.length fromRest, fn _ => "..")
      val parts = parents @ toRest
    in
      if null parts then "." else String.concatWith "/" parts
    end

  fun libraryDirectory dir =
    let val lib = OS.Path.concat (dir, "lib")
    in
      if fileExists
           (OS.Path.concat (OS.Path.concat (lib, "yack"), "yack.toml"))
      then SOME (canonicalPath lib)
      else NONE
    end

  fun findLibraryRoot start =
    let
      fun loop dir =
        case libraryDirectory dir of
          SOME lib => SOME lib
        | NONE =>
            let val parent = parentDir dir
            in if parent = dir then NONE else loop parent end
    in
      loop (canonicalPath (absolutePath start))
    end

  fun executableDirectory () =
    let
      val name = CommandLine.name ()
      val hasSeparator =
        List.exists (fn character => character = #"/") (String.explode name)
    in
      if hasSeparator then
        SOME (parentDir (canonicalPath (absolutePath name)))
      else NONE
    end

  fun firstSome candidates =
    case candidates of
      [] => NONE
    | candidate :: rest =>
        (case candidate () of
           SOME value => SOME value
         | NONE => firstSome rest)

  fun libraryRoot target =
    let
      fun fromEnvironment () =
        case OS.Process.getEnv "YACK_LIB" of
          NONE => NONE
        | SOME path =>
            let val candidate = canonicalPath (absolutePath path) in
              if fileExists
                   (OS.Path.concat
                      (OS.Path.concat (candidate, "yack"), "yack.toml"))
              then SOME candidate
              else findLibraryRoot candidate
            end
    in
      firstSome
        [ fromEnvironment
        , fn () => findLibraryRoot target
        , fn () => findLibraryRoot "."
        , fn () => case executableDirectory () of
                     SOME dir => libraryDirectory dir
                   | NONE => NONE
        ]
    end

  fun defaultEntry () =
    let
      val root =
        case findPackageRoot "." of
          SOME directory => directory
        | NONE => canonicalPath (absolutePath ".")
      val candidates =
        [ OS.Path.concat (OS.Path.concat (root, "src"), "main.yack")
        , OS.Path.concat (root, "main.yack")
        ]
    in
      List.find fileExists candidates
    end

  fun directoryIsEmpty path =
    let
      val directory = OS.FileSys.openDir path
      val entry = OS.FileSys.readDir directory
      val _ = OS.FileSys.closeDir directory
    in
      case entry of
        NONE => true
      | SOME _ => false
    end
    handle _ => false

  fun ensureDirectory path =
    if path = "" orelse path = "." then true
    else if fileExists path then isDirectory path
    else
      let
        val parent = parentDir path
        val parentReady = if parent = path then false else ensureDirectory parent
      in
        parentReady andalso
        ((OS.FileSys.mkDir path; true) handle _ => false)
      end

  fun validProjectName name =
    let
      val size = String.size name
      fun validRest index =
        index >= size orelse
        let
          val character = String.sub (name, index)
        in
          ( Char.isAlphaNum character
            orelse character = #"-"
            orelse character = #"_"
          ) andalso validRest (index + 1)
        end
    in
      size > 0 andalso Char.isAlpha (String.sub (name, 0)) andalso validRest 1
    end

  datatype targetState =
      TargetReady of bool
    | TargetNotEmpty
    | TargetIsFile
    | TargetFailed

  fun prepareTarget path =
    if fileExists path then
      if isDirectory path then
        if directoryIsEmpty path then TargetReady false else TargetNotEmpty
      else TargetIsFile
    else
      let
        val parentReady = ensureDirectory (parentDir path)
      in
        if not parentReady then TargetFailed
        else
          let val made = (OS.FileSys.mkDir path; true) handle _ => false in
            if made then TargetReady true else TargetFailed
          end
      end

  fun unlines lines = String.concatWith "\n" lines ^ "\n"

  fun repeatText text count =
    String.concat (List.tabulate (count, fn _ => text))

  fun utf8Width text =
    let
      val size = String.size text
      fun loop index count =
        if index >= size then count
        else
          let val byte = Char.ord (String.sub (text, index))
          in
            if byte >= 128 andalso byte < 192 then loop (index + 1) count
            else loop (index + 1) (count + 1)
          end
    in loop 0 0
    end

  fun spaces count =
    String.implode (List.tabulate (count, fn _ => #" "))

  fun padDisplay (text, width) =
    let val missing = width - utf8Width text
    in text ^ (if missing > 0 then spaces missing else "")
    end

  fun boxTop (title, width) =
    let
      val titleWidth = utf8Width title
      val fill = width - 5 - titleWidth
      val fill = if fill < 0 then 0 else fill
    in
      if title = "" then
        glyphTopLeft ^ repeatText glyphHorizontal (width - 2) ^ glyphTopRight
      else
        glyphTopLeft ^ glyphHorizontal ^ " " ^ title ^ " " ^
        repeatText glyphHorizontal fill ^ glyphTopRight
    end

  fun boxLine (content, width) =
    glyphVertical ^ " " ^ padDisplay (content, width - 4) ^ " " ^ glyphVertical

  fun boxBottom width =
    glyphBottomLeft ^ repeatText glyphHorizontal (width - 2) ^ glyphBottomRight

  fun stripLine text =
    let
      val size = String.size text
      fun isSpace character =
        character = #" " orelse character = #"\t" orelse
        character = #"\r" orelse character = #"\n"
      fun trimEnd index =
        if index > 0 andalso isSpace (String.sub (text, index - 1))
        then trimEnd (index - 1) else index
      fun trimStart index =
        if index < size andalso isSpace (String.sub (text, index))
        then trimStart (index + 1) else index
      val start = trimStart 0
      val stop = trimEnd size
    in
      if stop <= start then ""
      else String.substring (text, start, stop - start)
    end

  fun shellPath path =
    let
      fun needsQuote character =
        Char.isSpace character orelse
        character = #"'" orelse
        character = #"\"" orelse
        character = #"$" orelse
        character = #"`" orelse
        character = #"\\"
    in
      if List.exists needsQuote (String.explode path) then shellQuote path
      else path
    end

  datatype template =
      TemplateHello
    | TemplateCli
    | TemplateWeb
    | TemplateTui

  val templateList =
    [ TemplateHello, TemplateCli, TemplateWeb, TemplateTui ]

  fun templateKey template =
    case template of
      TemplateHello => "hello"
    | TemplateCli => "cli"
    | TemplateWeb => "web"
    | TemplateTui => "tui"

  fun templateDescription template =
    case template of
      TemplateHello => "A minimal Yack program."
    | TemplateCli => "A command-line app with options."
    | TemplateWeb => "An Alpage HTTP server."
    | TemplateTui => "A terminal screen built with yack/tui."

  fun templateSummary template =
    case template of
      TemplateHello => "minimal program"
    | TemplateCli => "command-line app"
    | TemplateWeb => "Alpage HTTP server"
    | TemplateTui => "terminal UI"

  fun templatePackages template =
    case template of
      TemplateHello => []
    | TemplateCli => [ "yack" ]
    | TemplateTui => [ "yack" ]
    | TemplateWeb =>
        [ "alpage", "web", "http_server", "net", "yack" ]

  fun templateFromString text =
    case String.map Char.toLower text of
      "hello" => SOME TemplateHello
    | "cli" => SOME TemplateCli
    | "web" => SOME TemplateWeb
    | "tui" => SOME TemplateTui
    | _ => NONE

  fun mainSource template =
    case template of
      TemplateHello =>
        unlines
          [ "(* Welcome to Yack ! *)"
          , "fun main () ="
          , "    print_string (\"Hello from Yack !\")"
          ]
    | TemplateCli =>
        unlines
          [ "import yack/args"
          , "import yack/cli"
          , "import yack/io"
          , "import yack/option"
          , "import yack/os"
          , ""
          , "(* A tiny CLI: `app --name Ada` prints a greeting. *)"
          , "fun name_option () : args.spec ="
          , "    args.Opt ((\"name\", Some (\"n\"), \"who to greet\", Some (\"Yack\")))"
          , ""
          , "fun specs () : args.spec list = [name_option ()]"
          , ""
          , "(* Both entry points end the process, so they share one shape. *)"
          , "fun usage_error (message : string) : unit ="
          , "    let val _ = cli.print_error (message)"
          , "    in"
          , "        os.exit_usage ()"
          , "    end"
          , ""
          , "(* The base accessor returns the option; `yack/option` owns the default. *)"
          , "fun greet (parsed : args.parsed) : unit ="
          , "    let val (found, _rest) = args.take_option (parsed, \"name\")"
          , "        val name = option.unwrap_or (found, \"Yack\")"
          , "        val greeting = \"Hello from \" ^ name ^ \" !\""
          , "        val _ = io.print_line (borrow greeting)"
          , "    in"
          , "        os.exit_ok ()"
          , "    end"
          , ""
          , "fun main () ="
          , "    case args.parse (specs (), os.command_args ()) of"
          , "        Error message => usage_error (message)"
          , "      | Ok parsed => greet (parsed)"
          ]
    | TemplateWeb =>
        unlines
          [ "import alpage/Alpage"
          , "import alpage/Ctx"
          , "import alpage/Reply"
          , "import alpage/Request"
          , "import alpage/Response"
          , "import yack/io"
          , ""
          , "fun home (ctx : Ctx.t, request : consume Request.t) : Reply.t ="
          , "    Reply.ok (Response.text (\"Hello from Yack !\"))"
          , ""
          , "fun application () : Alpage.t = Alpage.new () |> Alpage.get (\"/\", home)"
          , ""
          , "fun main () ="
          , "    case Alpage.run (application (), Alpage.config (8080)) of"
          , "        Ok _ => ()"
          , "      | Error message => io.print_line (borrow message)"
          ]
    | TemplateTui =>
        unlines
          [ "import yack/io"
          , "import yack/tui"
          , ""
          , "fun main () ="
          , "    let val items = [\"Run\", \"Check\", \"Quit\"]"
          , "        val body ="
          , "            \"Hello from Yack !\\n\\n\" ^ tui.menu (items, 0, 24) ^"
          , "                tui.status_bar (\"ready\", 44)"
          , "        val screen = tui.box (\"yack tui\", body, 52) ^ \"\\n\""
          , "    in"
          , "        io.print_raw (borrow screen)"
          , "    end"
          ]

  fun readmeSource name template =
    let
      val runLines =
        case template of
          TemplateHello =>
            [ "yack run"
            , "yack check src/main.yack"
            , "yack build src/main.yack hello"
            , "./hello"
            ]
        | TemplateCli =>
            [ "yack run"
            , "yack build src/main.yack app"
            , "./app --name Ada"
            ]
        | TemplateWeb =>
            [ "yack run"
            , "# then open http://localhost:8080"
            ]
        | TemplateTui =>
            [ "yack run" ]
    in
      unlines
        ([ "# " ^ name
         , ""
         , templateDescription template
         , ""
         , "Template: `" ^ templateKey template ^ "`"
         , ""
         , "## Run"
         , ""
         , "```sh"
         ]
         @ runLines
         @ [ "```"
           , ""
           , "Happy yacking !"
           ])
    end

  fun escapeToml text =
    String.translate
      (fn #"\\" => "\\\\"
        | #"\"" => "\\\""
        | character => String.str character)
      text

  fun dependencyLine (package, path) =
    package ^ " = { path = \"" ^ escapeToml path ^ "\" }"

  fun manifestSource (name, dependencies) =
    unlines
      ([ "[package]"
       , "name = \"" ^ name ^ "\""
       , "version = \"0.1.0\""
       , ""
       , "# Default memory model for this package:"
       , "#   heap  - normal ownership and free"
       , "#   stack - bump regions, no per-value free"
       , "# The command line flag --memory=... overrides this value."
       , "# memory = \"heap\""
       , ""
       , "[dependencies]"
       ]
       @ (if null dependencies then
            [ "# Add local packages by path:"
            , "# util = { path = \"../util\" }"
            ]
          else List.map dependencyLine dependencies)
       @ [ ""
         , "[memory]"
         , "# Memory model used when no --memory flag is given."
         , "# mode = \"heap\""
         , ""
         , "# Arena scopes are on by default. Set this to \"noop\" to keep the"
         , "# same source but disable arena allocation (useful for A/B tests)."
         , "# arena = \"on\""
         , ""
         , "# Address space reserved by stack mode. The OS commits pages lazily."
         , "# reserve = \"1G\""
         ])

  fun gitignoreSource () =
    unlines
      [ "*.ll"
      , "*.o"
      , "/hello"
      , ".yack/"
      ]

  fun newHeader (style : style) =
    ( say "";
      say ("  " ^ paint (style, "1;36", boxTop ("yack new", 56)));
      say ("  " ^ paint (style, "2", boxLine ("Create a new Yack project.", 56)));
      say ("  " ^ paint (style, "1;36", boxBottom 56));
      say "";
      TextIO.flushOut TextIO.stdOut
    )

  fun nextSteps target template =
    [ "cd " ^ shellPath target ]
    @ (case template of
         TemplateHello => [ "yack run" ]
       | TemplateCli =>
           [ "yack run"
           , "yack build src/main.yack app && ./app --name Ada"
           ]
       | TemplateWeb =>
           [ "yack run"
           , "open http://localhost:8080"
           ]
       | TemplateTui => [ "yack run" ])

  fun newSummary (style : style) target template files =
    let
      val title = target ^ "  -  " ^ templateKey template
    in
      say "";
      say ("  " ^ paint (style, "1;36", boxTop (title, 56)));
      List.app
        (fn file =>
          say ("  " ^ paint (style, "2", boxLine (" " ^ file, 56))))
        files;
      say ("  " ^ paint (style, "1;36", boxBottom 56));
      say "";
      say ("  " ^ paint (style, "1", "Next steps"));
      List.app
        (fn step =>
          say ("    " ^ paint (style, "1;32", glyphChevron) ^ " " ^
               paint (style, "36", step)))
        (nextSteps target template);
      say "";
      say ("  " ^ paint (style, "2", "Happy yacking !"));
      say "";
      TextIO.flushOut TextIO.stdOut
    end

  fun dependenciesFor target template =
    case templatePackages template of
      [] => SOME []
    | packages =>
        (case libraryRoot target of
           NONE => NONE
         | SOME lib =>
             SOME
               (List.map
                  (fn package =>
                    ( package
                    , relativePath
                        (target, OS.Path.concat (lib, package))
                    ))
                  packages))

  fun newProject
        (style : style)
        (errorStyle : style)
        interactive
        template
        target =
    let
      val target = trimTrailingSlashes target
      val name = baseName target
    in
      if not (validProjectName name) then
        ( newError errorStyle
            ("`" ^ name ^ "` is not a valid project name")
            [ "start with a letter"
            , "use only letters, digits, `-` or `_`"
            , "example: yack new hello-world"
            ]
        ; 1
        )
      else
        case dependenciesFor target template of
          NONE =>
            ( newError errorStyle
                ("template `" ^ templateKey template ^
                 "` needs the Yack libraries")
                [ "run `yack new` from the Yack source tree"
                , "or set YACK_LIB to the directory that contains yack/, web/ and alpage/"
                ]
            ; 1
            )
        | SOME dependencies =>
        case prepareTarget target of
          TargetNotEmpty =>
            ( newError errorStyle
                ("`" ^ target ^ "` already exists and is not empty")
                [ "choose another name"
                , "or remove the directory and try again"
                ]
            ; 1
            )
        | TargetIsFile =>
            ( newError errorStyle
                ("`" ^ target ^ "` already exists as a file")
                [ "choose another name" ]
            ; 1
            )
        | TargetFailed =>
            ( newError errorStyle
                ("could not create `" ^ target ^ "`")
                [ "check the parent directory permissions"
                , "and that the path is valid"
                ]
            ; 1
            )
        | TargetReady created =>
            let
              val files =
                [ ("yack.toml", manifestSource (name, dependencies))
                , ("src/main.yack", mainSource template)
                , ("README.md", readmeSource name template)
                , (".gitignore", gitignoreSource ())
                ]
              val written = ref []
              fun writeAll [] = true
                | writeAll ((file, contents) :: rest) =
                    let val path = target ^ "/" ^ file in
                      if writeText path contents then
                        ( written := path :: !written
                        ; if interactive then
                            stepPlain (style, TextIO.stdOut, "Wrote " ^ file)
                          else ()
                        ; writeAll rest
                        )
                      else false
                    end
              val _ =
                if interactive then
                  ( newHeader style
                  ; stepPlain
                      ( style
                      , TextIO.stdOut
                      , (if created then "Created " else "Using ") ^
                        target ^ "/"
                      )
                  )
                else ()
              val sourceReady =
                ensureDirectory (OS.Path.concat (target, "src"))
              val complete = sourceReady andalso writeAll files
              fun cleanup () =
                ( List.app
                    (fn path => OS.FileSys.remove path handle _ => ())
                    (!written)
                ; if created then
                    ( OS.FileSys.rmDir (OS.Path.concat (target, "src"))
                        handle _ => ()
                    ; OS.FileSys.rmDir target handle _ => ()
                    )
                  else ()
                )
            in
              if complete then
                (newSummary style target template (List.map #1 files); 0)
              else
                ( cleanup ()
                ; newError errorStyle
                    ("could not write the project into `" ^ target ^ "`")
                    [ "check the directory permissions and available space" ]
                ; 1
                )
            end
    end

  datatype newArguments =
      NewUsage
    | NewInvalid of string
    | NewMissingName of
        { mode : colorMode
        , template : template option
        }
    | NewValid of
        { name : string
        , mode : colorMode
        , template : template option
        }

  fun parseNewArguments arguments =
    let
      fun withTemplate mode template name value rest =
        case templateFromString value of
          SOME chosen => go mode (SOME chosen) name rest
        | NONE =>
            NewInvalid
              ("unknown template `" ^ value ^
               "` (expected hello, cli, web or tui)")
      and go mode template name remaining =
        case remaining of
          [] =>
            (case name of
               NONE => NewMissingName { mode = mode, template = template }
             | SOME project =>
                 NewValid
                   { name = project
                   , mode = mode
                   , template = template
                   })
        | "--help" :: _ => NewUsage
        | "-h" :: _ => NewUsage
        | "--color" :: rest => go ColorAlways template name rest
        | "--no-color" :: rest => go ColorNever template name rest
        | "--template" :: value :: rest =>
            withTemplate mode template name value rest
        | ["--template"] =>
            NewInvalid "missing value for --template"
        | "-t" :: value :: rest =>
            withTemplate mode template name value rest
        | ["-t"] =>
            NewInvalid "missing value for -t"
        | "--" :: rest =>
            (case (name, rest) of
               (NONE, [project]) => go mode template (SOME project) []
             | _ => NewInvalid "expected a single project name")
        | option :: _ =>
            if String.isPrefix "--template=" option then
              withTemplate mode template name
                (String.extract (option, 11, NONE))
                (List.drop (remaining, 1))
            else if String.isPrefix "-t=" option then
              withTemplate mode template name
                (String.extract (option, 3, NONE))
                (List.drop (remaining, 1))
            else if String.isPrefix "-" option andalso option <> "-" then
              NewInvalid ("unknown option `" ^ option ^ "`")
            else
              (case name of
                 NONE => go mode template (SOME option) (List.drop (remaining, 1))
               | SOME _ => NewInvalid "expected a single project name")
    in
      go ColorAuto NONE NONE arguments
    end

  fun newUsage (style : style) =
    let
      fun heading text = paint (style, "1;36", text)
      fun command text = paint (style, "1;33", text)
      val bullet = glyph (style, glyphBullet, "-")
    in
      say ("  " ^ paint (style, "1;36", boxTop ("yack new", 56)));
      say ("  " ^ paint (style, "2", boxLine ("Create a new Yack project.", 56)));
      say ("  " ^ paint (style, "1;36", boxBottom 56));
      say "";
      say ("  " ^ heading "Usage");
      say ("    " ^ command "yack new <name> [options]");
      say "";
      say ("  " ^ heading "Options");
      say ("    " ^ paint (style, "1", "-h, --help") ^
           paint (style, "2", "          show this help"));
      say ("    " ^ paint (style, "1", "-t, --template <name>") ^
           paint (style, "2", "  hello, cli, web or tui"));
      say ("    " ^ paint (style, "1", "--color") ^
           paint (style, "2", "             force colored output"));
      say ("    " ^ paint (style, "1", "--no-color") ^
           paint (style, "2", "          disable colored output"));
      say "";
      say ("  " ^ heading "Templates");
      say ("    " ^ command "hello" ^
           paint (style, "2", "  minimal program (default)"));
      say ("    " ^ command "cli" ^
           paint (style, "2", "    command-line app with options"));
      say ("    " ^ command "web" ^
           paint (style, "2", "    Alpage HTTP server on port 8080"));
      say ("    " ^ command "tui" ^
           paint (style, "2", "    terminal screen built with yack/tui"));
      say "";
      say ("  " ^ heading "Examples");
      say ("    " ^ command "yack new hello");
      say ("    " ^ command "yack new api --template web");
      say ("    " ^ command "yack new tool -t cli");
      say ("    " ^ command "yack new apps/api-client --template=cli");
      say "";
      say ("  " ^ paint (style, "2", bullet ^ " the scaffold contains yack.toml, src/main.yack, README.md and .gitignore"));
      say ("  " ^ paint (style, "2", bullet ^ " on a TTY, `yack new` asks for the template after the name"));
      say ""
    end

  fun newPrompt (style : style) =
    ( say "";
      say ("  " ^ paint (style, "1", "Project name") ^
           paint (style, "2", " (hello)"));
      TextIO.output
        (TextIO.stdOut, "  " ^ paint (style, "1;36", glyphChevron ^ " "));
      TextIO.flushOut TextIO.stdOut;
      case TextIO.inputLine TextIO.stdIn of
        NONE => NONE
      | SOME line =>
          let val name = stripLine line
          in if name = "" then SOME "hello" else SOME name end
    )

  fun newTemplatePrompt (style : style) (errorStyle : style) =
    let
      fun renderLine (index, template) =
        let
          val selected = index = 0
          val marker =
            if selected then glyph (style, glyphChevron, ">") else " "
          val text =
            " " ^ marker ^ " " ^ Int.toString (index + 1) ^ ". " ^
            templateKey template ^ "  " ^ templateSummary template
          val code = if selected then "1;36" else "2"
        in
          say ("  " ^ paint (style, code, boxLine (text, 56)))
        end
      fun renderLines (templates, index) =
        case templates of
          [] => ()
        | template :: rest =>
            ( renderLine (index, template)
            ; renderLines (rest, index + 1)
            )
      fun choose answer =
        if answer = "" then SOME TemplateHello
        else
          case templateFromString answer of
            SOME template => SOME template
          | NONE =>
              (case Int.fromString answer of
                 SOME number =>
                   if number >= 1 andalso
                      number <= List.length templateList
                   then SOME (List.nth (templateList, number - 1))
                   else NONE
               | NONE => NONE)
    in
      say "";
      say ("  " ^ paint (style, "1;36", boxTop ("template", 56)));
      say ("  " ^ paint (style, "2", boxLine (" Pick a starting point.", 56)));
      say ("  " ^ paint (style, "2", boxLine ("", 56)));
      renderLines (templateList, 0);
      say ("  " ^ paint (style, "1;36", boxBottom 56));
      say "";
      TextIO.output
        ( TextIO.stdOut
        , "  " ^ paint (style, "1", "Template") ^
          paint (style, "2", " (1)") ^ "\n  " ^
          paint (style, "1;36", glyph (style, glyphChevron, ">") ^ " ")
        );
      TextIO.flushOut TextIO.stdOut;
      case TextIO.inputLine TextIO.stdIn of
        NONE => NONE
      | SOME line =>
          let val answer = stripLine line in
            case choose answer of
              SOME template => SOME template
            | NONE =>
                ( newError errorStyle
                    ("unknown template `" ^ answer ^ "`")
                    [ "choose a number from 1 to " ^
                      Int.toString (List.length templateList)
                    , "or a name: " ^
                      String.concatWith
                        ", " (List.map templateKey templateList)
                    ]
                ; NONE
                )
          end
    end

  fun stdinIsTty () =
    Posix.ProcEnv.isatty Posix.FileSys.stdin handle _ => false

  fun chooseTemplate (style, errorStyle, template) =
    case template of
      SOME chosen => SOME chosen
    | NONE =>
        if stdinIsTty () then newTemplatePrompt style errorStyle
        else SOME TemplateHello

  fun newCommand arguments =
    case parseNewArguments arguments of
      NewUsage =>
        ( newUsage (makeStyle (ColorAuto, Posix.FileSys.stdout))
        ; TextIO.flushOut TextIO.stdOut
        ; 0
        )
    | NewInvalid message =>
        let val style = makeStyle (ColorAuto, Posix.FileSys.stderr) in
          newError style message
            [ "run `yack new --help` for usage"
            , "example: yack new hello --template cli"
            ];
          1
        end
    | NewMissingName { mode, template } =>
        let
          val style = makeStyle (mode, Posix.FileSys.stdout)
          val errorStyle = makeStyle (mode, Posix.FileSys.stderr)
        in
          if stdinIsTty () then
            case newPrompt style of
              NONE =>
                ( newError errorStyle "no project name"
                    [ "run `yack new --help` for usage" ]
                ; 1
                )
            | SOME name =>
                if validProjectName name then
                  (case chooseTemplate (style, errorStyle, template) of
                     NONE => 1
                   | SOME chosen =>
                       newProject
                         style
                         errorStyle
                         (progressEnabled Posix.FileSys.stdout)
                         chosen
                         name)
                else
                  ( newError errorStyle
                      ("`" ^ name ^ "` is not a valid project name")
                      [ "start with a letter"
                      , "use only letters, digits, `-` or `_`"
                      ]
                  ; 1
                  )
          else
            ( newUsage style
            ; 1
            )
        end
    | NewValid { name, mode, template } =>
        let
          val style = makeStyle (mode, Posix.FileSys.stdout)
          val errorStyle = makeStyle (mode, Posix.FileSys.stderr)
          val projectName = baseName (trimTrailingSlashes name)
        in
          if not (validProjectName projectName) then
            newProject
              style
              errorStyle
              (progressEnabled Posix.FileSys.stdout)
              TemplateHello
              name
          else
            case chooseTemplate (style, errorStyle, template) of
              NONE => 1
            | SOME chosen =>
                newProject
                  style
                  errorStyle
                  (progressEnabled Posix.FileSys.stdout)
                  chosen
                  name
        end

end
