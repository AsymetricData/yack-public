structure DriverCommands =
struct
  open DriverLoad
  open DriverIo

  fun declarationSummary (Ast.DFun {name, params, ...}) =
    "fun " ^ name ^ "/" ^ Int.toString (List.length params)
    | declarationSummary (Ast.DFunClause {name, params, ...}) =
    "fun " ^ name ^ "/" ^ Int.toString (List.length params)
    | declarationSummary (Ast.DType {name, ...}) =
    "type " ^ name
    | declarationSummary (Ast.DAlias {name, ...}) =
    "type " ^ name
    | declarationSummary (Ast.DDatatype {name, ...}) =
    "datatype " ^ name
    | declarationSummary (Ast.DExternRecord {name, ...}) =
    "extern record " ^ name
    | declarationSummary (Ast.DImport {path, ...}) =
    "import " ^ String.concatWith "/" path

  fun parseOnly file =
    case readFile file of
      NONE =>
        ( TextIO.output (TextIO.stdErr, "yack: cannot read `" ^ file ^ "`\n")
        ; 1
        )
    | SOME source =>
        let
          val lexed = Lexer.lex source
          val lexicalDiagnostics = #diagnostics lexed
        in
          if not (null lexicalDiagnostics) then
            (renderAll file source lexicalDiagnostics; 1)
          else
            let
              val parsed = Parser.parse (#tokens lexed)
              val syntaxDiagnostics = #diagnostics parsed
              val _ = renderAll file source syntaxDiagnostics
            in
              if null syntaxDiagnostics then
                ( List.app
                    (fn declaration =>
                      print (declarationSummary declaration ^ "\n"))
                    (#program parsed)
                ; 0
                )
              else 1
            end
        end

  fun showTokens file =
    case readFile file of
      NONE =>
        ( TextIO.output (TextIO.stdErr, "yack: cannot read `" ^ file ^ "`\n")
        ; 1
        )
    | SOME source =>
        let
          val lexed = Lexer.lex source
          val diagnostics = #diagnostics lexed
          val _ = renderAll file source diagnostics
          val _ =
            List.app
              (fn token =>
                print
                  (Pos.toString (Token.startOf token) ^ "  " ^
                   Token.describe token ^ "\n"))
              (#tokens lexed)
        in
          if null diagnostics then 0 else 1
        end

  fun resolveOnly file =
    case readFile file of
      NONE =>
        ( TextIO.output (TextIO.stdErr, "yack: cannot read `" ^ file ^ "`\n")
        ; 1
        )
    | SOME source =>
        let
          val lexed = Lexer.lex source
          val lexicalDiagnostics = #diagnostics lexed
        in
          if not (null lexicalDiagnostics) then
            (renderAll file source lexicalDiagnostics; 1)
          else
            let
              val parsed = Parser.parse (#tokens lexed)
              val syntaxDiagnostics = #diagnostics parsed
            in
              if not (null syntaxDiagnostics) then
                (renderAll file source syntaxDiagnostics; 1)
              else
                let
                  val (program, liftDiagnostics) =
                    prepareProgram (#program parsed)
                  val _ = renderAll file source liftDiagnostics
                  val resolved = Resolve.resolve program
                  val resolutionDiagnostics = #diagnostics resolved
                  val _ = renderAll file source resolutionDiagnostics
                in
                  if null liftDiagnostics andalso null resolutionDiagnostics then
                    ( List.app
                        (fn binding =>
                          print (Resolve.bindingSummary binding ^ "\n"))
                        (#bindings resolved)
                    ; List.app
                        (fn use => print (Resolve.useSummary use ^ "\n"))
                        (#uses resolved)
                    ; 0
                    )
                  else 1
                end
            end
        end

  fun allocMap mode json file =
    case loadWithMode mode file of
      NONE => 1
    | SOME checked =>
        let
          val lowered =
            Lower.lower
              { program = #program checked
              , mode = mode
              , functions = #functions checked
              , datatypes = #datatypes checked
              , expressionTypes = #expressionTypes checked
              , bindingTypes = #bindingTypes checked
              , calls = #calls checked
              , functionValues = #functionValues checked
              , applications = #applications checked
              , drops = #drops checked
              , partialDrops = #partialDrops checked
              , earlyDrops = #earlyDrops checked
              , borrowedLambdaParams = #borrowedLambdaParams checked
              }
          val diagnostics = #diagnostics lowered
          val _ = renderDiags (#sources checked) diagnostics
          val sites = #sites lowered
          val rootSites : Lower.traceSite list =
            List.filter
              (fn s => #allocates s andalso #destination s = "root")
              sites
          val rootWarnings =
            if mode = Memory.Stack then
              List.map
                (fn s =>
                  Diag.warning
                    ( "W3103"
                    , "allocation in the root region outside an arena"
                    , #span s
                    , [ "destination=root; this site repeats its allocation if the function runs per request"
                      , "function `" ^ #function s ^ "`, site " ^
                        Int.toString (#line s) ^ ":" ^
                        Int.toString (#column s)
                      ]
                    , [ "move the allocation into an `arena` block"
                      , "take a `borrow` of the container instead of rebuilding it"
                      , "or freeze the value once at startup"
                      ]
                    ))
                rootSites
            else []
          val _ = renderDiags (#sources checked) rootWarnings
        in
          if not (null diagnostics) then 1
          else if json then
            ( print "{\"sites\":["
            ; print
                (String.concatWith ","
                   (List.map
                      (fn s =>
                        "{\"id\":" ^ Int.toString (#id s) ^
                        ",\"function\":\"" ^ #function s ^ "\"" ^
                        ",\"line\":" ^ Int.toString (#line s) ^
                        ",\"column\":" ^ Int.toString (#column s) ^
                        ",\"kind\":\"" ^ #kind s ^ "\"" ^
                        ",\"scope\":\"" ^ #scope s ^ "\"" ^
                        ",\"allocates\":" ^
                        (if #allocates s then "true" else "false") ^
                        ",\"destination\":\"" ^ #destination s ^ "\"" ^
                        ",\"reason\":\"" ^ #reason s ^ "\"}")
                      sites))
            ; let
                fun count destination =
                  List.length
                    (List.filter
                       (fn s => #destination s = destination)
                       sites)
                val destinations =
                  [ "read", "heap", "activation", "destination", "arena",
                    "owner", "root", "stack" ]
                val fields =
                  List.map
                    (fn destination =>
                       "\"" ^ destination ^ "\":" ^
                       Int.toString (count destination))
                    destinations
              in
                print ("],\"summary\":{" ^
                       String.concatWith "," fields ^ "}}\n")
              end
            ; 0
            )
          else
            ( print
                "id\tfunction\tsite\tscope\tdestination\tallocates\tkind\treason\n"
            ; List.app
                (fn s =>
                  print
                    (Int.toString (#id s) ^ "\t" ^ #function s ^ "\t" ^
                     Int.toString (#line s) ^ ":" ^
                     Int.toString (#column s) ^ "\t" ^ #scope s ^ "\t" ^
                     #destination s ^ "\t" ^
                     (if #allocates s then "yes" else "no") ^ "\t" ^
                     #kind s ^ "\t" ^ #reason s ^ "\n"))
                sites
            ; let
                val rootSites =
                  List.filter
                    (fn s => #allocates s andalso #destination s = "root")
                    sites
              in
                if mode = Memory.Stack andalso not (List.null rootSites) then
                  print
                    ("\nAttention: " ^
                     Int.toString (List.length rootSites) ^
                     " site(s) allouent en racine hors arena en mode stack.\n" ^
                     "Vérifier qu'ils ne sont pas répétés dans une boucle de requête.\n")
                else ()
              end
            ; let
                fun count destination =
                  List.length
                    (List.filter
                       (fn s => #destination s = destination)
                       sites)
                val destinations =
                  [ "read", "heap", "activation", "destination", "arena",
                    "owner", "root", "stack" ]
                val fields =
                  List.map
                    (fn destination =>
                       destination ^ "=" ^
                       Int.toString (count destination))
                    destinations
              in
                print ("\nRésumé: " ^
                       String.concatWith " " fields ^ "\n")
              end
            ; 0
            )
        end

  fun typesOnly file =
    case load file of
      NONE => 1
    | SOME {types, aliases, datatypes, ...} =>
        let
          fun showAlias (info : Check.declaredAlias) =
            print
              ("type " ^ #name info ^ " : " ^
               Types.propertyToString (#property info) ^ " -> " ^
               Types.propertyToString (#property info) ^
               " = " ^ #body info ^ "\n")

          fun showDatatype (info : Check.declaredData) =
            let
              val params =
                case #params info of
                  [] => ""
                | params =>
                    " " ^ String.concatWith " "
                      (List.map
                         (fn (name, equality) =>
                           (if equality then "''" else "'") ^ name)
                         params)
              val constructors =
                String.concatWith " | "
                  (List.map
                     (fn (constructor, argument) =>
                       case argument of
                         NONE => constructor
                       | SOME ty => constructor ^ " of " ^ Types.toString ty)
                     (#constructors info))
            in
              print
                ("datatype" ^ params ^ " " ^ #name info ^ " : " ^
                 Types.propertyToString (#declared info) ^ " -> " ^
                 Types.propertyToString (#effective info) ^
                 " = " ^ constructors ^ "\n")
            end
          fun showType (info : Check.declaredType) =
            let
              val fields =
                String.concatWith ", "
                  (List.map
                     (fn (field, ty) => field ^ " : " ^ Types.toString ty)
                     (#fields info))
            in
              print
                ("type " ^ #name info ^ " : " ^
                 Types.propertyToString (#declared info) ^ " -> " ^
                 Types.propertyToString (#effective info) ^
                 " = { " ^ fields ^ " }\n")
            end
        in
          List.app showType types;
          List.app showAlias aliases;
          List.app showDatatype datatypes;
          0
        end

  fun lowerRaw mode checked = lowerFrom ["main"] mode checked

  and lowerFrom roots mode checked =
        Lower.lowerFrom roots
          { program = #program checked
          , mode = mode
          , functions = #functions checked
          , datatypes = #datatypes checked
          , expressionTypes = #expressionTypes checked
          , bindingTypes = #bindingTypes checked
          , calls = #calls checked
          , functionValues = #functionValues checked
          , applications = #applications checked
          , drops = #drops checked
          , partialDrops = #partialDrops checked
          , earlyDrops = #earlyDrops checked
          , borrowedLambdaParams = #borrowedLambdaParams checked
          }

  fun lowerRootsFor file (checked : DriverTypes.checked) =
    let
      fun canonical path =
        OS.Path.mkCanonical
          (if OS.Path.isAbsolute path then path
           else OS.Path.concat (OS.FileSys.getDir (), path))
      val target = canonical file
      val fileId =
        Option.map #id
          (List.find
             (fn (entry : DriverTypes.sourceEntry) => canonical (#path entry) = target)
             (#sources checked))
      val hasMain =
        List.exists
          (fn Ast.DFun {name = "main", ...} => true | _ => false)
          (#program checked)

      val everyFunction =
        OS.Process.getEnv "YACK_BORROW_BYPASS" = SOME "1"
    in
      if hasMain andalso not everyFunction then ["main"]
      else
        case fileId of
          NONE => []
        | SOME id =>
            List.mapPartial
              (fn Ast.DFun {name, span, ...} =>
                    if Pos.file (#start span) = id then SOME name else NONE
                | _ => NONE)
              (#program checked)
    end

  val loanCheckEnabled = OS.Process.getEnv "YACK_NO_VALIDATE" <> SOME "1"
  val loanNote = "the frontend accepted a borrow the SSA graph cannot prove sound"

  fun lowerForCheck file mode checked =
    let
      val lowered = lowerFrom (lowerRootsFor file checked) mode checked
      fun functionSpan ssaName =
        let
          val name =
            if String.isPrefix "yack_" ssaName
            then String.extract (ssaName, 5, NONE)
            else ssaName
          fun declared candidate =
            List.find
              (fn Ast.DFun {name = declaredName, ...} => declaredName = candidate
                | _ => false)
              (#program checked)

          fun cut index =
            if index + 1 >= String.size name then name
            else if String.sub (name, index) = #"_"
                    andalso String.sub (name, index + 1) = #"_"
            then String.substring (name, 0, index)
            else cut (index + 1)
          val base = cut 0
        in
          case (declared name, declared base) of
            (SOME (Ast.DFun {span, ...}), _) => span
          | (_, SOME (Ast.DFun {span, ...})) => span
          | _ => Lower.builtinSpan
        end
      val findings =
        if not (null (#diagnostics lowered))
           orelse OS.Process.getEnv "YACK_NO_VALIDATE" = SOME "1"
        then []
        else SsaOwn.checkProgram (SsaOpt.optimize (#program lowered))
      val loanFindings =
        if not (null (#diagnostics lowered)) orelse not loanCheckEnabled then []
        else SsaLoan.checkProgram (SsaOpt.optimize (#program lowered))
      val validation =
        List.map
          (fn finding =>
            Diag.error
              ( "E0900"
              , "internal error: ownership validation: " ^ #message finding
              , case #site finding of
                  SOME span => span
                | NONE => functionSpan (#function finding)
              , (case #note finding of
                   SOME span => ["the other side is at " ^ Pos.toString (#start span)]
                 | NONE =>
                     ["the frontend accepted a program its own lowering cannot free correctly"])
              , ["this is a compiler bug: please report it with the file (see `yack ssa`)"] ))
          findings
        @ List.map
          (fn finding =>
            Diag.error
              ( "E0901"
              , "internal error: borrow validation: " ^ #message finding
              , case #site finding of
                  SOME span => span
                | NONE => functionSpan (#function finding)
              , (case #note finding of
                   SOME span => ["the other side is at " ^ Pos.toString (#start span)]
                 | NONE => [loanNote])
              , ["this is a compiler bug: please report it with the file (see `yack ssa --loans`)"] ))
          loanFindings
    in
      { program = #program lowered
      , diagnostics = #diagnostics lowered @ validation
      , sites = #sites lowered
      }
    end

  fun lowerDiagnostics file mode checked =
    ( InternalContext.set ("lowering " ^ file)
    ; #diagnostics (lowerForCheck file mode checked) )

  fun arenaBlocks { file, text, mode, checked : DriverTypes.checked, lowered : Lower.result } =
    let
      fun canonical path =
        OS.Path.mkCanonical
          (if OS.Path.isAbsolute path then path
           else OS.Path.concat (OS.FileSys.getDir (), path))
      val fileId =
        case List.find
               (fn (entry : DriverTypes.sourceEntry) =>
                 canonical (#path entry) = canonical file)
               (#sources checked) of
          SOME entry => #id entry
        | NONE => ~1
      val program =
        let val lexed = Lexer.lex text in
          if List.null (#tokens lexed) then []
          else #program (Parser.parse (#tokens lexed))
        end
        handle _ => []

      val preludeIds =
        List.map #id
          (List.filter
             (fn (entry : DriverTypes.sourceEntry) => #path entry = "<prelude>")
             (#sources checked))
      val userFunctions =
        List.mapPartial
          (fn Ast.DFun {name, span, ...} =>
                if List.exists (fn id => id = Pos.file (#start span)) preludeIds
                   orelse String.isPrefix "__yack" name
                then NONE
                else SOME name
            | _ => NONE)
          (#program checked)
      fun lineColumn (span : Diag.span) =
        (Pos.line (#start span), Pos.column (#start span),
         Pos.line (#stop span), Pos.column (#stop span))
      fun typeAt span =
        Option.map #2
          (List.find
             (fn (candidate : Diag.span, _) =>
               Pos.file (#start candidate) = fileId andalso
               lineColumn candidate = lineColumn span)
             (#expressionTypes checked))
    in
      ArenaReport.scan
        { program = program
        , fileId = fileId
        , mode = mode
        , sites = #sites lowered
        , lowered = List.map #sourceName (#functions (#program lowered))
        , functions = userFunctions
        , typeAt = typeAt
        }
    end

  fun arenaOnly mode file =
    case loadForCheck mode file of
      NONE => 1
    | SOME checked =>
        let
          val lowered = lowerForCheck file mode checked
          val diagnostics = #diagnostics lowered
          val _ = renderDiags (#sources checked) diagnostics
        in
          if Diag.hasErrors diagnostics then 1
          else
            case readFile file of
              NONE => 1
            | SOME text =>
                ( print
                    (ArenaReport.render mode
                       (arenaBlocks
                          { file = file, text = text, mode = mode
                          , checked = checked, lowered = lowered }) ^ "\n")
                ; 0 )
        end

  fun lowerUnvalidated mode checked = lowerUnvalidatedFrom ["main"] mode checked

  and lowerUnvalidatedFrom roots mode checked =
    let
      val _ = InternalContext.set "lowering"
      val lowered = lowerFrom roots mode checked
      val diagnostics = #diagnostics lowered
      val _ = renderDiags (#sources checked) diagnostics
    in
      if null diagnostics then SOME (SsaOpt.optimize (#program lowered))
      else NONE
    end

  fun lowerChecked mode file checked =
    case lowerUnvalidated mode checked of
      NONE => NONE
    | SOME program =>
          (case (if OS.Process.getEnv "YACK_NO_VALIDATE" = SOME "1" then []
                 else SsaOwn.checkProgram program) of
            [] =>
              (case (if loanCheckEnabled then SsaLoan.checkProgram program else []) of
                 [] => SOME program
               | findings =>
                   ( List.app
                       (fn finding =>
                         TextIO.output (TextIO.stdErr,
                           "internal error[E0901]: borrow validation: " ^
                           #message finding ^ "\n"))
                       findings
                   ; TextIO.output (TextIO.stdErr,
                       "  = note: " ^ loanNote ^ "; this is a compiler bug (see `yack ssa --loans`)\n")
                   ; NONE ))
          | findings =>
              ( List.app
                  (fn finding =>
                    TextIO.output (TextIO.stdErr,
                      "internal error[E0900]: ownership validation: " ^
                      #message finding ^ "\n"))
                  findings
              ; TextIO.output (TextIO.stdErr,
                  "  = note: the frontend accepted a program its own lowering cannot free \
                  \correctly; this is a compiler bug (see `yack ssa`)\n")
              ; NONE ))

  fun checkLowered mode file =
    case loadForCheck mode file of
      NONE => 1
    | SOME checked =>
        let
          val diagnostics = lowerDiagnostics file mode checked
          val _ = renderDiags (#sources checked) diagnostics
        in
          if Diag.hasErrors diagnostics then 1 else 0
        end

  fun ssaOnly loans file =
    case load file of
      NONE => 1
    | SOME checked =>
        (case lowerUnvalidatedFrom (lowerRootsFor file checked) Memory.Heap checked of
           NONE => 1
         | SOME program =>
             let
               fun isLinearData name =
                 List.exists (fn (d : Ir.dataDef) => #name d = name andalso #linear d)
                   (#datatypes program)
               val funcs = Ssa.buildProgram program
             in
               if loans then
                 print (Ssa.headers isLinearData funcs ^
                        SsaLoan.programText funcs)
               else print (Ssa.programToString isLinearData funcs);
               0
             end)

  fun ssaFormat file =
    case Option.map SsaParse.parse (readFile file) of
      NONE => (TextIO.output (TextIO.stdErr, file ^ ": cannot read\n"); 1)
    | SOME (SsaParse.Failed message) =>
        (TextIO.output (TextIO.stdErr, file ^ ":" ^ message ^ "\n"); 1)
    | SOME (SsaParse.Parsed {linear, funcs}) =>
        ( print (Ssa.programToString
                   (fn name => List.exists (fn x => x = name) linear) funcs)
        ; 0 )

  fun ssaCheck file =
    case Option.map SsaParse.parse (readFile file) of
      NONE => (TextIO.output (TextIO.stdErr, file ^ ": cannot read\n"); 1)
    | SOME (SsaParse.Failed message) =>
        (TextIO.output (TextIO.stdErr, file ^ ":" ^ message ^ "\n"); 1)
    | SOME (SsaParse.Parsed {linear, funcs}) =>
        let
          fun isLinearData name = List.exists (fn x => x = name) linear
          val findings = List.concat (List.map (SsaOwn.check isLinearData) funcs)
          val loanFindings = SsaLoan.checkFuncs funcs
          val suffix = fn finding =>
            (case #site finding of
               SOME span => "  # at " ^ Pos.toString (#start span)
             | NONE => "") ^
            (case #note finding of
               SOME span => "  (other side at " ^ Pos.toString (#start span) ^ ")"
             | NONE => "")
        in
          List.app
            (fn finding =>
              TextIO.output (TextIO.stdErr,
                "internal error[E0900]: ownership validation: " ^
                #message finding ^ suffix finding ^ "\n"))
            findings;
          List.app
            (fn finding =>
              TextIO.output (TextIO.stdErr,
                "internal error[E0901]: borrow validation: " ^
                #message finding ^ suffix finding ^ "\n"))
            loanFindings;
          if null findings andalso null loanFindings then 0 else 1
        end

  fun irOnly file =
    case load file of
      NONE => 1
    | SOME checked =>
        (case lowerChecked Memory.Heap file checked of
           NONE => 1
         | SOME program =>
             ( List.app
                 (fn definition => print (Ir.dataToString definition ^ "\n"))
                 (#datatypes program)
             ; List.app
                 (fn definition => print (Ir.funToString definition ^ "\n"))
                 (#functions program)
             ; 0
             ))

  fun llvmOnly mode reserve file =
    case loadWithMode mode file of
      NONE => 1
    | SOME checked =>
        (case lowerChecked mode file checked of
           NONE => 1
         | SOME program =>
             ( print (Llvm.generate (mode, reserve, program))
             ; 0
             ))
end
