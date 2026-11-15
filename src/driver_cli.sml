structure DriverCli =
struct
  open DriverTypes
  open DriverUi
  open DriverCommands
  open DriverBuild
  open DriverNew
  open DriverFmt

  fun usage () =
    let
      val style = makeStyle (ColorAuto, Posix.FileSys.stdout)
      fun heading text = paint (style, "1;36", text)
      fun command text = paint (style, "1;33", text)
      fun description text = paint (style, "2", text)
      fun row name text =
        say ("    " ^ command name ^ description
               (String.concat (List.tabulate
                  (Int.max (0, 22 - String.size name), fn _ => " ")) ^ text))
    in
      say ("  " ^ mascot style ^ paint (style, "1;36", "yack") ^
           paint (style, "2", "  " ^ version));
      say "";
      say ("  " ^ description "A small functional language with explicit memory and native code.");
      say "";
      say ("  " ^ heading "Usage");
      say ("    " ^ command "yack <command> [arguments]");
      say "";
      say ("  " ^ heading "Commands");
      row "new <name>" "create a new project";
      row "check <file>" "lex, parse, resolve and type-check";
      row "explain <code>" "explain a diagnostic code";
      row "fmt <file|->" "format a file or standard input";
      row "resolve <file>" "print bindings, ids and uses";
      row "types <file>" "print type properties (declared -> effective)";
      row "parse <file>" "lex and parse, then summarize declarations";
      row "arena <file>" "explain what each arena block allocates and frees";
      row "alloc-map <file>" "list allocation sites and predicted scopes";
      row "tokens <file>" "print the token stream";
      row "ir <file>" "lower to the typed IR";
      row "ssa <file>" "print the CFG in SSA form, last uses marked !";
      row "llvm <file>" "print textual LLVM IR";
      row "build <file> <output>" "compile with clang";
      row "run [file]" "compile and run (default: project entry)";
      row "lsp" "run the language server on stdio";
      row "version" "print the version";
      row "help" "print this help";
      say "";
      say ("  " ^ heading "Build options");
      say ("    " ^ command "--memory=heap|stack" ^
           description "  allocator (default: heap)");
      say ("    " ^ command "--arena-reserve=1G" ^
           description "  stack reservation (default: 1G)");
      say ("    " ^ command "--arena=on|noop" ^
           description "  arena optimisation (default: on)");
      say ("    " ^ command "--arena-debug=poison" ^
           description "  poison closed scopes (needs AddressSanitizer)");
      say ("    " ^ command "--arena-quarantine=strict" ^
           description "  never reuse a closed scope (CI)");
      say ("    " ^ command "--arena-quarantine-limit=256M" ^
           description "  strict quarantine budget");
      say ("    " ^ command "--debug-alloc" ^
           description "  trace allocations and frees on stderr");
      say ("    " ^ command "--trace-alloc" ^
           description "  trace allocations by site (use with alloc-map)");
      say ("    " ^ command "--trim" ^
           description "  optimise for size and strip symbols");
      say "";
      say ("  " ^ heading "Examples");
      say ("    " ^ command "yack new hello");
      say ("    " ^ command "yack check main.yack");
      say ("    " ^ command "yack fmt --write main.yack");
      say ("    " ^ command "yack build main.yack hello");
      say ("    " ^ command "yack run");
      say "";
      TextIO.flushOut TextIO.stdOut
    end

  fun versionLine () =
    let
      val style = makeStyle (ColorAuto, Posix.FileSys.stdout)
    in
      say (paint (style, "1;36", mascot style ^ "yack") ^ " " ^
           paint (style, "2", "0.2.0") ^
           paint (style, "2", " (frontend + types)") ^ " " ^
           paint (style, "2", "commit " ^ BuildInfo.commit));
      TextIO.flushOut TextIO.stdOut
    end

  fun exitWith code =
    if code = 0 then () else OS.Process.exit OS.Process.failure

  fun explainBody body =
    let
      val lines = String.fields (fn ch => ch = #"\n") body
      fun fence line = String.isPrefix "```" line
      fun loop (inside, acc, []) = List.rev acc
        | loop (inside, acc, line :: rest) =
            if fence line then loop (not inside, acc, rest)
            else if inside then loop (inside, ("    " ^ line) :: acc, rest)
            else loop (inside, line :: acc, rest)
    in
      loop (false, [], lines)
    end

  fun explainEntry code =
    let val wanted = String.map Char.toUpper code in
      List.find (fn (candidate, _, _) => candidate = wanted) ErrorDocs.entries
    end

  fun explainCode code =
    case explainEntry code of
      NONE =>
        ( complain ("error: unknown diagnostic code '" ^ code ^ "'")
        ; complain ("= help: run 'yack explain' to list the documented codes")
        ; 1
        )
    | SOME (found, title, body) =>
        let val style = makeStyle (ColorAuto, Posix.FileSys.stdout) in
          say (paint (style, "1", found ^ " — " ^ title));
          say "";
          List.app say (explainBody body);
          0
        end

  fun explainList () =
    ( List.app
        (fn (code, title, _) => say (code ^ "  " ^ title))
        ErrorDocs.entries
    ; 0
    )

  fun explainUsage () =
    let
      val style = makeStyle (ColorAuto, Posix.FileSys.stdout)
      fun heading text = paint (style, "1;36", text)
      fun command text = paint (style, "1;33", text)
      fun description text = paint (style, "2", text)
    in
      say ("  " ^ mascot style ^ paint (style, "1;36", "yack") ^
           paint (style, "2", "  explain"));
      say "";
      say ("  " ^ description "Explain a diagnostic code.");
      say "";
      say ("  " ^ heading "Usage");
      say ("    " ^ command "yack explain <code>");
      say ("    " ^ command "yack explain");
      say ("        " ^ description "list the documented codes");
      say "";
      say ("  " ^ heading "Examples");
      say ("    " ^ command "yack explain E0501");
      say ("    " ^ command "yack explain e3004");
      say "";
      TextIO.flushOut TextIO.stdOut
    end

  datatype parsedMemory =
      MemoryOk of
        Memory.mode * Int64.int * bool * bool *
        Memory.arenaMode * bool * bool * string list
    | MemoryError of string

  fun parseMemory arguments =
    let
      val trim = ref false
      fun reserveOf text =
        case Memory.parseSize text of
          SOME value => if value >= 65536 then SOME value else NONE
        | NONE => NONE
      fun go mode reserve debug explicitMode arena explicitArena acc [] =
            MemoryOk
              (mode, reserve, debug, explicitMode, arena, explicitArena,
               !trim, List.rev acc)
        | go mode reserve debug explicitMode arena explicitArena acc
             ("--trim" :: rest) =
            ( trim := true
            ; go mode reserve debug explicitMode arena explicitArena acc rest
            )
        | go mode reserve debug explicitMode arena explicitArena acc
             ("--no-trim" :: rest) =
            ( trim := false
            ; go mode reserve debug explicitMode arena explicitArena acc rest
            )
        | go mode reserve debug explicitMode arena explicitArena acc
             ("--debug-alloc" :: rest) =
            go mode reserve true explicitMode arena explicitArena acc rest
        | go mode reserve debug explicitMode arena explicitArena acc
             ("--trace-alloc" :: rest) =
            ( Memory.setTrace true
            ; go mode reserve debug explicitMode arena explicitArena acc rest
            )
        | go mode reserve debug explicitMode arena explicitArena acc
             ("--trace" :: rest) =
            ( Memory.setTrace true
            ; go mode reserve true explicitMode arena explicitArena acc rest
            )
        | go mode reserve _ explicitMode arena explicitArena acc
             ("--no-debug-alloc" :: rest) =
            go mode reserve false explicitMode arena explicitArena acc rest
        | go mode reserve debug _ arena explicitArena acc
             ("--memory=heap" :: rest) =
            go Memory.Heap reserve debug true arena explicitArena acc rest
        | go mode reserve debug _ arena explicitArena acc
             ("--memory=stack" :: rest) =
            go Memory.Stack reserve debug true arena explicitArena acc rest
        | go mode reserve debug _ arena explicitArena acc
             ("--memory" :: "heap" :: rest) =
            go Memory.Heap reserve debug true arena explicitArena acc rest
        | go mode reserve debug _ arena explicitArena acc
             ("--memory" :: "stack" :: rest) =
            go Memory.Stack reserve debug true arena explicitArena acc rest
        | go _ _ _ _ _ _ _ ["--memory"] =
            MemoryError "missing value for --memory"
        | go mode reserve debug explicitMode arena explicitArena acc
             ("--arena-reserve" :: value :: rest) =
            (case reserveOf value of
               SOME reserve =>
                 go mode reserve debug explicitMode arena explicitArena acc rest
             | NONE =>
                 MemoryError ("invalid arena reservation `" ^ value ^ "`"))
        | go _ _ _ _ _ _ _ ["--arena-reserve"] =
            MemoryError "missing value for --arena-reserve"
        | go mode reserve debug explicitMode _ _ acc
             ("--arena=on" :: rest) =
            go mode reserve debug explicitMode Memory.ArenaOn true acc rest
        | go mode reserve debug explicitMode _ _ acc
             ("--arena=noop" :: rest) =
            go mode reserve debug explicitMode Memory.ArenaNoop true acc rest
        | go mode reserve debug explicitMode _ _ acc
             ("--arena=off" :: rest) =
            go mode reserve debug explicitMode Memory.ArenaNoop true acc rest
        | go mode reserve debug explicitMode _ _ acc
             ("--arena" :: "on" :: rest) =
            go mode reserve debug explicitMode Memory.ArenaOn true acc rest
        | go mode reserve debug explicitMode _ _ acc
             ("--arena" :: "noop" :: rest) =
            go mode reserve debug explicitMode Memory.ArenaNoop true acc rest
        | go _ _ _ _ _ _ _ ["--arena"] =
            MemoryError "missing value for --arena"
        | go mode reserve debug explicitMode arena explicitArena acc
             ("--arena-debug=poison" :: rest) =
            ( Memory.setArenaDebug Memory.ArenaDebugPoison
            ; go mode reserve debug explicitMode arena explicitArena acc rest
            )
        | go mode reserve debug explicitMode arena explicitArena acc
             ("--arena-debug=off" :: rest) =
            ( Memory.setArenaDebug Memory.ArenaDebugOff
            ; go mode reserve debug explicitMode arena explicitArena acc rest
            )
        | go mode reserve debug explicitMode arena explicitArena acc
             ("--arena-quarantine=strict" :: rest) =
            ( Memory.setQuarantine Memory.QuarantineStrict
            ; go mode reserve debug explicitMode arena explicitArena acc rest
            )
        | go mode reserve debug explicitMode arena explicitArena acc
             ("--arena-quarantine=off" :: rest) =
            ( Memory.setQuarantine Memory.QuarantineOff
            ; go mode reserve debug explicitMode arena explicitArena acc rest
            )
        | go mode reserve debug explicitMode arena explicitArena acc
             (argument :: rest) =
            if String.isPrefix "--memory=" argument then
              MemoryError
                ("unknown memory mode `" ^ argument ^
                 "` (expected --memory=heap or --memory=stack)")
            else if String.isPrefix "--arena-reserve=" argument then
              let
                val prefix = "--arena-reserve="
                val text =
                  String.substring
                    (argument, String.size prefix,
                     String.size argument - String.size prefix)
              in
                case reserveOf text of
                  SOME value =>
                    go mode value debug explicitMode arena explicitArena acc rest
                | NONE =>
                    MemoryError
                      ("invalid arena reservation `" ^ text ^ "`")
              end
            else if String.isPrefix "--arena-quarantine-limit=" argument then
              let
                val prefix = "--arena-quarantine-limit="
                val text =
                  String.substring
                    (argument, String.size prefix,
                     String.size argument - String.size prefix)
              in
                case Memory.parseSize text of
                  SOME value =>
                    if value > 0 then
                      ( Memory.arenaQuarantineLimit := value
                      ; go mode reserve debug explicitMode arena explicitArena
                          acc rest
                      )
                    else
                      MemoryError
                        ("invalid arena quarantine limit `" ^ text ^ "`")
                | NONE =>
                    MemoryError
                      ("invalid arena quarantine limit `" ^ text ^ "`")
              end
            else if String.isPrefix "--arena-debug=" argument then
              MemoryError
                ("unknown arena debug mode `" ^ argument ^
                 "` (expected --arena-debug=off or --arena-debug=poison)")
            else if String.isPrefix "--arena-quarantine=" argument then
              MemoryError
                ("unknown arena quarantine mode `" ^ argument ^
                 "` (expected --arena-quarantine=off or strict)")
            else if String.isPrefix "--arena=" argument then
              let
                val text = String.substring (argument, 8, String.size argument - 8)
              in
                case Memory.arenaFromString text of
                  SOME value =>
                    go mode reserve debug explicitMode value true acc rest
                | NONE =>
                    MemoryError
                      ("unknown arena mode `" ^ text ^
                       "` (expected on or noop)")
              end
            else
              go mode reserve debug explicitMode arena explicitArena
                (argument :: acc) rest
    in
      go Memory.default Memory.defaultArenaReserve false false
        Memory.defaultArena false [] arguments
    end

  fun runCommand mode reserve trim arguments =
    case arguments of
      [file] => runFile mode reserve trim file
    | [] =>
        (case defaultEntry () of
           SOME file => runFile mode reserve trim file
         | NONE =>
             ( TextIO.output
                 ( TextIO.stdErr
                 , "yack: no entry point found\n" ^
                   "  expected `src/main.yack` or `main.yack` " ^
                   "in a project with `yack.toml`\n"
                 )
             ; 1
             ))
    | _ =>
        ( TextIO.output
            (TextIO.stdErr, "yack: run expects at most one file\n")
        ; 1
        )

  fun checkArchitecture () =
    let
      fun field name =
        case List.find (fn (key, _) => key = name)
               (Posix.ProcEnv.uname ()) of
          SOME (_, value) => value
        | NONE => ""
      val sysname = field "sysname"
      val machine = field "machine"
    in
      if sysname = "Linux" andalso
         (machine = "x86_64" orelse machine = "amd64") then ()
      else
        ( TextIO.output
            ( TextIO.stdErr
            , "yack: unsupported platform `" ^ sysname ^ "/" ^ machine ^
              "`\n" ^
              "  the stdlib extern records and getdents64 are Linux x86-64 only\n"
            )
        ; OS.Process.exit OS.Process.failure
        )
    end

  fun main () =
    let
      val stderrStyle = makeStyle (ColorAuto, Posix.FileSys.stderr)
      val _ = diagnosticStyle := { color = #color stderrStyle }
    in
    case parseMemory (CommandLine.arguments ()) of
      MemoryError message =>
        ( TextIO.output (TextIO.stdErr, "yack: " ^ message ^ "\n")
        ; OS.Process.exit OS.Process.failure
        )
    | MemoryOk (mode0, reserve0, debug, explicit, arena0, explicitArena,
                trim, arguments) =>
        let
          val target =
            case List.find (fn argument => String.isSuffix ".yack" argument)
                   arguments of
              SOME file => file
            | NONE =>
                (case defaultEntry () of
                   SOME file => file
                 | NONE => OS.FileSys.getDir ())
          val settings = manifestSettingsFor target
          val mode =
            if explicit then mode0
            else
              case #mode settings of
                SOME value => value
              | NONE => mode0
          val reserve =
            case #reserve settings of
              SOME value => value
            | NONE => reserve0
          val arena =
            if explicitArena then arena0
            else
              case #arena settings of
                SOME value => value
              | NONE => arena0
          val _ =
            case (mode, arena) of
              (Memory.Stack, Memory.ArenaNoop) =>
                ( TextIO.output
                    ( TextIO.stdErr
                    , "yack: --arena=noop is not supported with " ^
                      "--memory=stack; stack mode requires a request arena\n"
                    )
                ; OS.Process.exit OS.Process.failure
                )
            | _ => ()
          val _ = Memory.setArena arena
          val _ = Llvm.setDebugAlloc debug
          val _ =
            InternalContext.set
              (case arguments of
                 command :: _ => "running `yack " ^ command ^ "`"
               | [] => "running yack")
        in
        case arguments of
          [] => (usage (); OS.Process.exit OS.Process.failure)
        | ["version"] => versionLine ()
        | ["help"] => usage ()
        | ["help", "new"] => exitWith (newCommand ["--help"])
        | ["help", "fmt"] => exitWith (fmtCommand ["--help"])
        | ["help", "explain"] => explainUsage ()
        | ["explain"] => exitWith (explainList ())
        | ["explain", code] => exitWith (explainCode code)
        | "new" :: newArguments => exitWith (newCommand newArguments)
        | "fmt" :: fmtArguments => exitWith (fmtCommand fmtArguments)
        | ["check", file] =>
            let val status = DriverCommands.checkLowered mode file
            in DriverIo.explainHint (); exitWith status end
        | ["resolve", file] => exitWith (resolveOnly file)
        | ["types", file] => exitWith (typesOnly file)
        | ["parse", file] => exitWith (parseOnly file)
        | ["arena", file] => exitWith (arenaOnly mode file)
        | ["alloc-map", file] =>
            ( Memory.setTrace true
            ; exitWith (allocMap mode false file)
            )
        | ["alloc-map", "--json", file] =>
            ( Memory.setTrace true
            ; exitWith (allocMap mode true file)
            )
        | ["tokens", file] => exitWith (showTokens file)
        | ["ir", file] => exitWith (irOnly file)
        | ["ssa", file] => exitWith (ssaOnly false file)
        | ["ssa", "--loans", file] => exitWith (ssaOnly true file)
        | ["ssa-fmt", file] => exitWith (ssaFormat file)
        | ["ssa-check", file] => exitWith (ssaCheck file)
        | ["llvm", file] =>
            (checkArchitecture (); exitWith (llvmOnly mode reserve file))
        | ["build", file, output] =>
            (checkArchitecture ();
             let val status = build mode reserve trim file output
             in DriverIo.explainHint (); exitWith status end)
        | ["run", file] =>
            (checkArchitecture ();
             let val status = runFile mode reserve trim file
             in DriverIo.explainHint (); exitWith status end)
        | "run" :: runArguments =>
            (checkArchitecture ();
             let val status = runCommand mode reserve trim runArguments
             in DriverIo.explainHint (); exitWith status end)
        | _ => (usage (); OS.Process.exit OS.Process.failure)
        end
    end
end
