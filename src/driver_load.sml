structure DriverLoad =
struct
  exception ModuleAbort

  fun testFailure phase =
    case OS.Process.getEnv "YACK_INTERNAL_ERROR_TEST" of
      SOME requested =>
        if requested = phase then raise Fail "internal error test" else ()
    | NONE => ()

  fun resolveDependency (root, path) =
    if OS.Path.isAbsolute path then OS.Path.mkCanonical path
    else OS.Path.concat (root, path)

  val preludeText =
    "fun arena_keep (value : 'a) : 'a = value\n" ^
    "datatype 'a option = None | Some of 'a\n" ^
    "datatype 'a result = Ok of 'a | Error of string\n" ^
    "datatype 'a list = Nil | Cons of 'a * 'a list\n" ^
    "\n" ^
    "fun list_length (values : 'a list) : int =\n" ^
    "    case values of\n" ^
    "        Nil => 0\n" ^
    "      | Cons (_, rest) => 1 + list_length (rest)\n" ^
    "\n" ^
    "fun list_tail (values : 'a list) : 'a list =\n" ^
    "    case values of\n" ^
    "        Nil => Nil\n" ^
    "      | Cons (_, rest) => rest\n" ^
    "\n" ^
    "fun list_head (values : 'a list) : 'a option =\n" ^
    "    case values of\n" ^
    "        Nil => None\n" ^
    "      | Cons (head, _) => Some (head)\n" ^
    "\n" ^
    "fun option_is_some (value : 'a option) : bool =\n" ^
    "    case value of\n" ^
    "        None => false\n" ^
    "      | Some (_) => true\n" ^
    "\n" ^
    "fun result_is_ok (value : 'a result) : bool =\n" ^
    "    case value of\n" ^
    "        Ok (_) => true\n" ^
    "      | Error (_) => false\n" ^
    "\n" ^
    "fun int32_to_string (value : int) : string = int_to_string (value)\n" ^
    "\n" ^
    "fun string_to_int32 (text : borrow string) : int option =\n" ^
    "    string_to_int (text)\n"

  fun prepareProgram program =
    let val lifted = LocalFun.liftProgram (Desugar.desugarProgram program) in
      (#program lifted, #diagnostics lifted)
    end

  fun trim text =
    let
      fun isSpace ch = Char.isSpace ch
      val size = String.size text
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

  fun unquote text =
    let
      val text = trim text
      val size = String.size text
    in
      if size >= 2 andalso String.sub (text, 0) = #"\"" andalso
         String.sub (text, size - 1) = #"\"" then
        String.substring (text, 1, size - 2)
      else text
    end

  val defaultManifest =
    { mode = NONE : Memory.mode option
    , reserve = NONE : Int64.int option
    , arena = NONE : Memory.arenaMode option
    }

  fun readManifestSettings manifest =
    case DriverIo.readFile manifest of
      NONE => defaultManifest
    | SOME contents =>
        let
          val mode = ref NONE
          val reserve = ref NONE
          val arena = ref NONE
          val section = ref ""
          fun setMode text =
            case Memory.fromString
                   (String.map Char.toLower (unquote text)) of
              SOME value => mode := SOME value
            | NONE => ()
          fun setArena text =
            case Memory.arenaFromString
                   (String.map Char.toLower (unquote text)) of
              SOME value => arena := SOME value
            | NONE => ()
          fun setReserve text =
            case Memory.parseSize (unquote text) of
              SOME value => reserve := SOME value
            | NONE => ()
          fun parseLine line =
            let val line = trim line in
              if line = "" orelse String.isPrefix "#" line then ()
              else if String.isPrefix "[" line andalso
                      String.isSuffix "]" line then
                section :=
                  String.substring (line, 1, String.size line - 2)
              else
                case String.fields (fn ch => ch = #"=") line of
                  [] => ()
                | key :: rest =>
                    let
                      val key = String.map Char.toLower (trim key)
                      val value = String.concatWith "=" rest
                    in
                      case (!section, key) of
                        ("", "memory") => setMode value
                      | ("package", "memory") => setMode value
                      | ("", "arena") => setArena value
                      | ("package", "arena") => setArena value
                      | ("memory", "mode") => setMode value
                      | ("memory", "arena") => setArena value
                      | ("memory", "reserve") => setReserve value
                      | ("memory", "size") => setReserve value
                      | _ => ()
                    end
            end
          val _ =
            List.app parseLine
              (String.fields (fn ch => ch = #"\n") contents)
        in
          { mode = !mode, reserve = !reserve, arena = !arena }
        end

  fun findManifest start =
    let
      fun absolute path =
        if OS.Path.isAbsolute path then path
        else OS.Path.concat (OS.FileSys.getDir (), path)
      fun parent path =
        let val parent = OS.Path.dir path
        in if parent = path then path else parent end
      fun loop dir =
        let val candidate = OS.Path.concat (dir, "yack.toml") in
          if OS.FileSys.access (candidate, [OS.FileSys.A_READ]) then
            SOME candidate
          else
            let val next = parent dir in
              if next = dir then NONE else loop next
            end
        end
      val start =
        if OS.FileSys.isDir start handle _ => false then start
        else OS.Path.dir start
    in
      loop (OS.Path.mkCanonical (absolute start))
    end

  fun manifestSettingsFor path =
    case findManifest path of
      SOME manifest => readManifestSettings manifest
    | NONE => defaultManifest

  fun scratchCalls program =
    let
      val calls = ref []
      fun walk expression =
        case expression of
          Ast.ECall (name, arguments, span) =>
            ( if String.isPrefix "sys.scratch_" name then
                calls := (span, name) :: !calls
              else ()
            ; List.app walk arguments
            )
        | Ast.EBin (_, left, right, _) => (walk left; walk right)
        | Ast.ENeg (operand, _) => walk operand
        | Ast.EIf (condition, thenBranch, elseBranch, _) =>
            (walk condition; walk thenBranch; walk elseBranch)
        | Ast.ELet (bindings, body, _) =>
            (List.app
               (fn binding =>
                 case binding of
                   Ast.BVal {value, ...} => walk value
                 | Ast.BPat {value, ...} => walk value
                 | Ast.BFun {body, ...} => walk body
                 | Ast.BGuard {value, fallback, ...} =>
                     (walk value; walk fallback))
               bindings;
             walk body)
        | Ast.EArena (bindings, body, _) =>
            (List.app
               (fn binding =>
                 case binding of
                   Ast.BVal {value, ...} => walk value
                 | Ast.BPat {value, ...} => walk value
                 | Ast.BFun {body, ...} => walk body
                 | Ast.BGuard {value, fallback, ...} =>
                     (walk value; walk fallback))
               bindings;
             walk body)
        | Ast.ERecord (fields, _) =>
            List.app (fn (_, value) => walk value) fields
        | Ast.EField (_, base, _) => walk base
        | Ast.ETuple (values, _) => List.app walk values
        | Ast.ETupleIndex (_, base, _) => walk base
        | Ast.ECase (scrutinee, branches, _) =>
            (walk scrutinee;
             List.app
               (fn (_, guard, body) =>
                 (case guard of SOME value => walk value | NONE => ();
                  walk body))
               branches)
        | Ast.EBorrow (_, _, operand, _) => walk operand
        | Ast.EFn (_, _, body, _) => walk body
        | _ => ()
      val _ =
        List.app
          (fn declaration =>
            case declaration of
              Ast.DFun {body, ...} => walk body
            | Ast.DFunClause {body, ...} => walk body
            | _ => ())
          program
    in
      List.rev (!calls)
    end

  fun loadWith (options : DriverTypes.loadOptions) file : DriverTypes.checked option =
    (case #read options file of
      NONE =>
        ( #note options ("cannot read `" ^ file ^ "`")
        ; NONE
        )
    | SOME source =>
        let
          val _ = InternalContext.set ("parsing " ^ file)
          val _ = testFailure "parse"
          val sources =
            ref
              [ { id = 0
                , path = file
                , map = Source.create source
                } ]
          val nextFileId = ref 1
          fun registerSource (path, text) =
            let
              val id = !nextFileId
              val _ = nextFileId := id + 1
              val _ = sources :=
                { id = id, path = path, map = Source.create text } :: !sources
            in
              id
            end
          val lexed = Lexer.lexIn (0, source)
          val lexicalDiagnostics = #diagnostics lexed
        in
          if not (null lexicalDiagnostics) then
            (#report options (!sources, lexicalDiagnostics); NONE)
          else
            let
              val parsed = Parser.parse (#tokens lexed)
              val syntaxDiagnostics = #diagnostics parsed

              val parseFailedNames = #failed parsed
              val _ = #report options (!sources, syntaxDiagnostics)
              val scratchDiagnostics =
                List.map
                  (fn (span, _) =>
                    Diag.error
                      ( "E0708"
                      , "mémoire temporaire utilisée sans bloc `arena`"
                      , span
                      , [ "les appels de mémoire temporaire sont internes au compilateur" ]
                      , [ "remplacez cet appel par `arena val ... in ... end`"
                        , "tout ce qui est alloué dans le bloc est libéré d'un coup à `end`"
                        ]
                      ))
                  (scratchCalls (#program parsed))
            in
              if not (null scratchDiagnostics) then
                (#report options (!sources, scratchDiagnostics); NONE)
              else
                let
                  val moduleFailed = ref false
                  fun failModule message =
                    ( moduleFailed := true
                    ; #note options message
                    )
                  val preludeFileId = registerSource ("<prelude>", preludeText)
                  val preludeLexed = Lexer.lexIn (preludeFileId, preludeText)
                  val preludeParsed = Parser.parse (#tokens preludeLexed)
                  val preludeDiagnostics =
                    #diagnostics preludeLexed @ #diagnostics preludeParsed
                  val _ = #report options (!sources, preludeDiagnostics)
                  val _ =
                    if not (null preludeDiagnostics) then
                      failModule "invalid prelude"
                    else ()
                  val (prelude, preludeLiftDiagnostics) =
                    prepareProgram (#program preludeParsed)
                  val _ =
                    #report options (!sources, preludeLiftDiagnostics)
                  val _ =
                    if not (null preludeLiftDiagnostics) then
                      failModule "invalid prelude local function"
                    else ()
                  val _ = if !moduleFailed then raise ModuleAbort else ()
                  fun stripImports program =
                    List.filter
                      (fn Ast.DImport _ => false | _ => true)
                      program
                  fun importsOf program =
                    List.mapPartial
                      (fn Ast.DImport directive => SOME directive | _ => NONE)
                      program
                  fun moduleKey path = String.concatWith "." path
                  fun modulePrefix path = "$" ^ moduleKey path
                  fun internalName path name = modulePrefix path ^ "." ^ name
                  fun pathText path = String.concatWith "/" path
                  val directory =
                    let val value = OS.Path.dir file
                    in if value = "" then "." else value end
                  fun pathExists path =
                    OS.FileSys.access (path, [OS.FileSys.A_READ])
                  fun parentDirectory path =
                    let val parent = OS.Path.dir path
                    in if parent = path then path else parent end

                  fun absoluteDirectory path =
                    let
                      val path =
                        if OS.Path.isAbsolute path then path
                        else OS.Path.concat (OS.FileSys.getDir (), path)
                    in
                      OS.Path.mkCanonical path
                    end
                  fun findPackageRoot path =
                    let
                      fun loop dir =
                        if pathExists (OS.Path.concat (dir, "yack.toml")) then
                          SOME dir
                        else
                          let val parent = parentDirectory dir
                          in if parent = dir then NONE else loop parent end
                    in
                      loop (absoluteDirectory path)
                    end

                  val implicitYackRoot =
                    case StdlibRoot.find directory of
                      NONE => NONE
                    | SOME packageRoot =>
                        let
                          val candidate = OS.Path.concat (packageRoot, "src")
                        in
                          SOME
                            (if OS.FileSys.isDir candidate handle _ => false then
                               candidate
                             else packageRoot)
                        end
                  fun trim text =
                    let
                      fun isSpace ch = Char.isSpace ch
                      val size = String.size text
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
                  fun quoted text =
                    let
                      val size = String.size text
                      fun openAt index =
                        if index >= size then ""
                        else if String.sub (text, index) = #"\"" then
                          let
                            fun closeAt stop =
                              if stop >= size then ""
                              else if String.sub (text, stop) = #"\"" then
                                String.substring (text, index + 1, stop - index - 1)
                              else closeAt (stop + 1)
                          in
                            closeAt (index + 1)
                          end
                        else openAt (index + 1)
                    in
                      openAt 0
                    end
                  fun readDependencies manifest =
                    case DriverIo.readFile manifest of
                      NONE => []
                    | SOME contents =>
                        let
                          val dependencies = ref []
                          val section = ref ""
                          fun parseLine line =
                            let val line = trim line in
                              if line = "" orelse String.isPrefix "#" line then ()
                              else if String.isPrefix "[" line andalso
                                      String.isSuffix "]" line then
                                section :=
                                  String.substring
                                    (line, 1, String.size line - 2)
                              else if !section = "dependencies" then
                                case String.fields (fn ch => ch = #"=") line of
                                  [] => ()
                                | key :: rest =>
                                    let val path = quoted (String.concatWith "=" rest) in
                                      if path = "" then ()
                                      else
                                        dependencies := (trim key, path) :: !dependencies
                                    end
                              else ()
                            end
                          val _ =
                            List.app parseLine
                              (String.fields (fn ch => ch = #"\n") contents)
                        in
                          List.rev (!dependencies)
                        end
                  val root =
                    case findPackageRoot directory of
                      SOME packageRoot => packageRoot
                    | NONE => directory
                  val sourceRoot =
                    let val candidate = OS.Path.concat (root, "src")
                    in
                      if OS.FileSys.isDir candidate handle _ => false then
                        candidate
                      else root
                    end
                  val dependencies =
                    case findPackageRoot directory of
                      SOME packageRoot =>
                        readDependencies (OS.Path.concat (packageRoot, "yack.toml"))
                    | NONE => []
                  val hasImplicitYack =
                    case implicitYackRoot of SOME _ => true | NONE => false
                  fun dependencyRoot alias =
                    case List.find (fn (name, _) => name = alias) dependencies of
                      NONE =>
                        (case implicitYackRoot of
                           SOME source => if alias = "yack" then source else sourceRoot
                         | NONE => sourceRoot)
                    | SOME (_, path) =>
                        let
                          val dependency = resolveDependency (root, path)
                          val candidate = OS.Path.concat (dependency, "src")
                        in
                          if OS.FileSys.isDir candidate handle _ => false then
                            candidate
                          else dependency
                        end
                  fun modulePath path =
                    let
                      val segments =
                        case path of
                          [] => []
                        | first :: rest =>
                            if List.exists
                                 (fn (alias, _) => alias = first)
                                 dependencies orelse
                               (first = "yack" andalso hasImplicitYack) then
                              dependencyRoot first :: rest
                            else sourceRoot :: path
                      val relative =
                        String.concatWith "/" segments ^ ".yack"
                    in

                      if String.isPrefix "/" relative then relative
                      else OS.Path.concat ("", relative)
                    end
                  fun missingStdlibHint path =
                    case (implicitYackRoot, path) of
                      (NONE, "yack" :: _) =>
                        if List.exists
                             (fn (alias, _) => alias = "yack") dependencies
                        then NONE
                        else
                          SOME
                            ("hint: set YACK_LIB to the directory that " ^
                             "contains yack/yack.toml")
                    | _ => NONE
                  val seenModules = StringSet.create 64
                  val loadedModules = ref []
                  fun loadModule (path, stack) =
                    let val key = moduleKey path in
                      if List.exists (fn candidate => candidate = key) stack then
                        failModule
                          ("cyclic import: " ^
                           String.concatWith " -> " (List.rev (key :: stack)))
                      else
                        let val filePath = modulePath path in
                          if StringSet.member (seenModules, filePath) then ()
                          else
                            case #read options filePath of
                              NONE =>
                                ( failModule
                                    ("cannot find module " ^ pathText path ^
                                     " at " ^ filePath)
                                ; case missingStdlibHint path of
                                    SOME hint => #note options hint
                                  | NONE => ()
                                )
                            | SOME moduleSource =>
                                let
                                  val moduleFileId =
                                    registerSource (filePath, moduleSource)
                                  val lexedModule =
                                    Lexer.lexIn (moduleFileId, moduleSource)
                                  val parsedModule = Parser.parse (#tokens lexedModule)
                                  val moduleDiagnostics =
                                    #diagnostics lexedModule @
                                    #diagnostics parsedModule
                                  val _ = #report options (!sources, moduleDiagnostics)
                                in
                                  if not (null moduleDiagnostics) then
                                    failModule
                                      ("parse error in module " ^ pathText path)
                                  else
                                    let
                                      val (moduleProgram, moduleLiftDiagnostics) =
                                        prepareProgram (#program parsedModule)
                                      val _ =
                                        #report options
                                          (!sources, moduleLiftDiagnostics)
                                      val _ =
                                        if not (null moduleLiftDiagnostics) then
                                          failModule
                                            ("local function error in module " ^
                                             pathText path)
                                        else ()
                                      val _ =
                                        List.app
                                          (fn directive =>
                                            loadModule
                                              (#path directive, key :: stack))
                                          (importsOf moduleProgram)
                                      val _ = StringSet.insert (seenModules, filePath)
                                      val _ =
                                        loadedModules :=
                                          (path, moduleProgram) :: !loadedModules
                                    in
                                      ()
                                    end
                                end
                        end
                    end
                  val (mainProgram, mainLiftDiagnostics) =
                    prepareProgram (#program parsed)
                  val _ = #report options (!sources, mainLiftDiagnostics)
                  val _ =
                    if not (null mainLiftDiagnostics) then
                      failModule "local function error"
                    else ()
                  val _ =
                    List.app
                      (fn directive => loadModule (#path directive, []))
                      (importsOf mainProgram)
                  val _ = if !moduleFailed then raise ModuleAbort else ()
                  val loaded = List.rev (!loadedModules)

                  type exports =
                    { functions : string list
                    , externs : string list
                    , types : string list
                    , constructors : string list
                    }

                  val emptyExports =
                    { functions = [], externs = [], types = [], constructors = [] }

                  fun member name names =
                    List.exists (fn candidate => candidate = name) names

                  fun isExternBody body =
                    case body of
                      Ast.EExtern _ => true
                    | _ => false

                  fun exportsOf program =
                    List.foldl
                      (fn (declaration, exports) =>
                        case declaration of
                          Ast.DFun {name, body, ...} =>
                            { functions = name :: #functions exports
                            , externs =
                                if isExternBody body then
                                  name :: #externs exports
                                else #externs exports
                            , types = #types exports
                            , constructors = #constructors exports
                            }
                        | Ast.DType {name, ...} =>
                            { functions = #functions exports
                            , externs = #externs exports
                            , types = name :: #types exports
                            , constructors = #constructors exports
                            }
                        | Ast.DAlias {name, ...} =>
                            { functions = #functions exports
                            , externs = #externs exports
                            , types = name :: #types exports
                            , constructors = #constructors exports
                            }
                        | Ast.DExternRecord {name, ...} =>
                            { functions = #functions exports
                            , externs = #externs exports
                            , types = name :: #types exports
                            , constructors = #constructors exports
                            }
                        | Ast.DDatatype {name, constructors, ...} =>
                            { functions = #functions exports
                            , externs = #externs exports
                            , types = name :: #types exports
                            , constructors =
                                List.map
                                  (fn (constructor, _) => constructor)
                                  constructors
                                @ #constructors exports
                            }
                        | _ => exports)
                      emptyExports
                      program

                  val preludeExports = exportsOf prelude
                  val protectedNames =
                    Check.builtins @
                    (#functions preludeExports @
                     #types preludeExports @
                     #constructors preludeExports) @
                    [ "int", "int1", "int2", "int4", "int8", "int16"
                    , "int32", "int64", "float", "float32", "float64"
                    , "bool", "unit", "token", "resource", "string"
                    , "array", "map", "io_result"
                    ]

                  val loadedWithExports =
                    List.map
                      (fn (path, program) => (moduleKey path, exportsOf program))
                      loaded

                  fun exportsFor key =
                    case List.find (fn (candidate, _) => candidate = key)
                             loadedWithExports of
                      SOME (_, exports) => SOME exports
                    | NONE => NONE

                  val qualifyDiagnostics = ref []

                  fun reportQualify (code, message, span, notes, hints) =
                    qualifyDiagnostics :=
                      Diag.error (code, message, span, notes, hints) ::
                      !qualifyDiagnostics

                  type imports =
                    { qualifiers : (string * string) list
                    , values : (string * string) list
                    , types : (string * string) list
                    }

                  val emptyImports =
                    { qualifiers = [], values = [], types = [] }

                  fun importEnvironment program =
                    let
                      val own = exportsOf program
                    in
                    List.foldl
                      (fn (directive, imports) =>
                        case directive of
                          { path, alias, unqualified, span } =>
                            let
                              val key = moduleKey path
                              val prefix = modulePrefix path
                              val qualifier =
                                case alias of
                                  SOME name => name
                                | NONE =>
                                    (case List.rev path of
                                       segment :: _ => segment
                                     | [] => key)
                              val exports =
                                case exportsFor key of
                                  SOME exports => exports
                                | NONE => emptyExports
                              val imports =
                                if alias = SOME "_" then imports
                                else
                                  (case List.find
                                          (fn (candidate, _) =>
                                            candidate = qualifier)
                                          (#qualifiers imports) of
                                     SOME (_, previous) =>
                                       if previous = prefix then imports
                                       else
                                         ( reportQualify
                                             ( "E3030"
                                             , "module qualifier `" ^ qualifier ^
                                               "` is used by more than one import"
                                             , span
                                             , []
                                             , ["use `as` to give one import a distinct name"]
                                             )
                                         ; imports )
                                   | NONE =>
                                       { qualifiers =
                                           (qualifier, prefix) ::
                                           #qualifiers imports
                                       , values = #values imports
                                       , types = #types imports
                                       })
                              fun addValue (name, alias, imports) =
                                if member name (#externs exports) then
                                  ( reportQualify
                                      ( "E3043"
                                      , "`" ^ name ^ "` is an extern private to module `" ^
                                        pathText path ^ "`"
                                      , span
                                      , ["an extern is not exported, even inside the standard library"]
                                      , ["write a public wrapper in `" ^
                                         pathText path ^ "` and import that"] )
                                  ; imports )
                                else if not (member name (#functions exports) orelse
                                        member name (#constructors exports)) then
                                  ( reportQualify
                                      ( "E3031"
                                      , "module `" ^ pathText path ^
                                        "` has no value `" ^ name ^ "`"
                                      , span, [], [] )
                                  ; imports )
                                else if alias = SOME "_" then imports
                                else
                                  let
                                    val bound =
                                      case alias of
                                        SOME name' => name'
                                      | NONE => name
                                  in
                                    if member bound protectedNames then
                                      ( reportQualify
                                          ( "E3030"
                                          , "importing `" ^ bound ^
                                            "` unqualified would shadow a prelude name"
                                          , span, []
                                          , [ "use the qualified name `" ^ qualifier ^
                                              "." ^ name ^ "` instead" ] )
                                      ; imports )
                                    else if member bound (#functions own) orelse
                                            member bound (#constructors own) then
                                      ( reportQualify
                                          ( "E3030"
                                          , "`" ^ bound ^
                                            "` is imported unqualified but already defined in this module"
                                          , span, []
                                          , [ "rename the import with `as`" ] )
                                      ; imports )
                                    else
                                      (case List.find
                                              (fn (candidate, _) => candidate = bound)
                                              (#values imports) of
                                         SOME (_, target) =>
                                           if target = internalName path name then imports
                                           else
                                             ( reportQualify
                                                 ( "E3030"
                                                 , "name `" ^ bound ^
                                                   "` is imported unqualified more than once"
                                                 , span, []
                                                 , ["use `as` to give one import a distinct name"] )
                                             ; imports )
                                       | NONE =>
                                           { qualifiers = #qualifiers imports
                                           , values =
                                               (bound, internalName path name) ::
                                               #values imports
                                           , types = #types imports
                                           })
                                  end
                              fun addType (name, alias, imports) =
                                if not (member name (#types exports)) then
                                  ( reportQualify
                                      ( "E3031"
                                      , "module `" ^ pathText path ^
                                        "` has no type `" ^ name ^ "`"
                                      , span, [], [] )
                                  ; imports )
                                else if alias = SOME "_" then imports
                                else
                                  let
                                    val bound =
                                      case alias of
                                        SOME name' => name'
                                      | NONE => name
                                  in
                                    if member bound protectedNames then
                                      ( reportQualify
                                          ( "E3030"
                                          , "importing type `" ^ bound ^
                                            "` unqualified would shadow a prelude name"
                                          , span, []
                                          , [ "use the qualified name `" ^ qualifier ^
                                              "." ^ name ^ "` instead" ] )
                                      ; imports )
                                    else if member bound (#types own) then
                                      ( reportQualify
                                          ( "E3030"
                                          , "type `" ^ bound ^
                                            "` is imported unqualified but already defined in this module"
                                          , span, []
                                          , [ "rename the import with `as`" ] )
                                      ; imports )
                                    else
                                      (case List.find
                                              (fn (candidate, _) => candidate = bound)
                                              (#types imports) of
                                         SOME (_, target) =>
                                           if target = internalName path name then imports
                                           else
                                             ( reportQualify
                                                 ( "E3030"
                                                 , "type `" ^ bound ^
                                                   "` is imported unqualified more than once"
                                                 , span, []
                                                 , ["use `as` to give one import a distinct name"] )
                                             ; imports )
                                       | NONE =>
                                           { qualifiers = #qualifiers imports
                                           , values = #values imports
                                           , types =
                                               (bound, internalName path name) ::
                                               #types imports
                                           })
                                  end
                            in
                              List.foldl
                                (fn (item, imports) =>
                                  case item of
                                    Ast.ImportValue (name, alias) =>
                                      addValue (name, alias, imports)
                                  | Ast.ImportType (name, alias) =>
                                      addType (name, alias, imports))
                                imports
                                unqualified
                            end)
                      emptyImports
                      (importsOf program)
                    end

                  fun splitQualified name =
                    case String.fields (fn ch => ch = #".") name of
                      [] => (NONE, name)
                    | [_] => (NONE, name)
                    | first :: rest =>
                        (SOME first, String.concatWith "." rest)

                  fun rewriteValueName (ownExpr, imports, qualifier, name) =
                    case splitQualified name of
                      (SOME candidate, memberName) =>
                        (case List.find
                                (fn (candidate', _) => candidate' = candidate)
                                (#qualifiers imports) of
                           SOME (_, prefix) => prefix ^ "." ^ memberName
                         | NONE => name)
                    | (NONE, _) =>
                        if member name ownExpr then
                          (case qualifier of
                             SOME prefix => prefix ^ "." ^ name
                           | NONE => name)
                        else
                          (case List.find
                                  (fn (alias, _) => alias = name)
                                  (#values imports) of
                             SOME (_, target) => target
                           | NONE => name)

                  fun rewriteTypeName (ownTypes, imports, qualifier, name) =
                    case splitQualified name of
                      (SOME candidate, memberName) =>
                        (case List.find
                                (fn (candidate', _) => candidate' = candidate)
                                (#qualifiers imports) of
                           SOME (_, prefix) => prefix ^ "." ^ memberName
                         | NONE => name)
                    | (NONE, _) =>
                        if member name ownTypes then
                          (case qualifier of
                             SOME prefix => prefix ^ "." ^ name
                           | NONE => name)
                        else
                          (case List.find
                                  (fn (alias, _) => alias = name)
                                  (#types imports) of
                             SOME (_, target) => target
                           | NONE => name)

                  fun qualifyDeclName (qualifier, name) =
                    case qualifier of
                      SOME prefix => prefix ^ "." ^ name
                    | NONE => name

                  fun rewriteType (ownTypes, imports, qualifier, syntax) =
                    case syntax of
                      Ast.TyName name =>
                        Ast.TyName
                          (rewriteTypeName (ownTypes, imports, qualifier, name))
                    | Ast.TyTuple types =>
                        Ast.TyTuple
                          (List.map
                             (fn ty =>
                               rewriteType (ownTypes, imports, qualifier, ty))
                             types)
                    | Ast.TyRecord fields =>
                        Ast.TyRecord
                          (List.map
                             (fn (field, ty) =>
                               ( field
                               , rewriteType
                                   (ownTypes, imports, qualifier, ty)))
                             fields)
                    | Ast.TyBorrow (mutable, ampersand, inner, provenance) =>
                        Ast.TyBorrow
                          ( mutable
                          , ampersand
                          , rewriteType (ownTypes, imports, qualifier, inner)
                          , provenance
                          )
                    | Ast.TyApp (name, argument) =>
                        Ast.TyApp
                          ( rewriteTypeName (ownTypes, imports, qualifier, name)
                          , rewriteType (ownTypes, imports, qualifier, argument)
                          )
                    | Ast.TyFun (params, result) =>
                        Ast.TyFun
                          ( List.map
                              (fn ty =>
                                rewriteType (ownTypes, imports, qualifier, ty))
                              params
                          , rewriteType (ownTypes, imports, qualifier, result)
                          )
                    | Ast.TyConsume (bang, inner) =>
                        Ast.TyConsume
                          ( bang
                          , rewriteType (ownTypes, imports, qualifier, inner)
                          )
                    | _ => syntax

                  fun rewriteExpr
                        (environment, ownExpr, ownTypes, imports, qualifier,
                         expression) =
                    let
                      fun recurse expression' =
                        rewriteExpr
                          (environment, ownExpr, ownTypes, imports, qualifier,
                           expression')
                      val rewriteValueNameHere = fn name =>
                        rewriteValueName (ownExpr, imports, qualifier, name)
                      val rewriteTypeHere = fn syntax =>
                        rewriteType (ownTypes, imports, qualifier, syntax)
                    in
                      case expression of
                        Ast.EInt _ => expression
                      | Ast.EFloat _ => expression
                      | Ast.EString _ => expression
                      | Ast.EBool _ => expression
                      | Ast.EUnit _ => expression
                      | Ast.EVar (name, span) =>
                          if member name environment then expression
                          else Ast.EVar (rewriteValueNameHere name, span)
                      | Ast.ECall (name, arguments, span) =>
                          let
                            val name' =
                              if member name environment then name
                              else rewriteValueNameHere name
                          in
                            Ast.ECall (name', List.map recurse arguments, span)
                          end
                      | Ast.EBin (operator, left, right, span) =>
                          Ast.EBin (operator, recurse left, recurse right, span)
                      | Ast.ENeg (operand, span) =>
                          Ast.ENeg (recurse operand, span)
                      | Ast.EIf (condition, thenBranch, elseBranch, span) =>
                          Ast.EIf
                            ( recurse condition
                            , recurse thenBranch
                            , recurse elseBranch
                            , span
                            )
                      | Ast.ELet (bindings, body, span) =>
                          let
                            val (bindings', environment') =
                              List.foldl
                                (fn (binding, (bindings, environment)) =>
                                  case binding of
                                    Ast.BVal {name, annot, value, span} =>
                                      let
                                        val value' =
                                          rewriteExpr
                                            ( environment, ownExpr, ownTypes
                                            , imports, qualifier, value
                                            )
                                        val annot' =
                                          Option.map rewriteTypeHere annot
                                      in
                                        ( Ast.BVal
                                            { name = name
                                            , annot = annot'
                                            , value = value'
                                            , span = span
                                            } :: bindings
                                        , name :: environment
                                        )
                                      end
                                  | Ast.BPat {pattern, value, span} =>
                                      let
                                        val value' =
                                          rewriteExpr
                                            ( environment, ownExpr, ownTypes
                                            , imports, qualifier, value
                                            )
                                        val (pattern', environment') =
                                          rewritePattern
                                            ( environment, ownExpr, imports
                                            , qualifier, pattern
                                            )
                                      in
                                        ( Ast.BPat
                                            { pattern = pattern'
                                            , value = value'
                                            , span = span
                                            } :: bindings
                                        , environment'
                                        )
                                      end
                                  | Ast.BFun {name, nameSpan, params, result,
                                              body, span} =>
                                      let
                                        val body' =
                                          rewriteExpr
                                            ( environment, ownExpr, ownTypes
                                            , imports, qualifier, body
                                            )
                                      in
                                        ( Ast.BFun
                                            { name = name
                                            , nameSpan = nameSpan
                                            , params = params
                                            , result = result
                                            , body = body'
                                            , span = span
                                            } :: bindings
                                        , environment
                                        )
                                      end
                                  | Ast.BGuard _ =>
                                      raise Fail
                                        "driver: desugared guard binding")
                                ([], environment)
                                bindings
                            val body' =
                              rewriteExpr
                                ( environment', ownExpr, ownTypes, imports
                                , qualifier, body
                                )
                          in
                            Ast.ELet (List.rev bindings', body', span)
                          end
                      | Ast.EArena (bindings, body, span) =>
                          let
                            val (bindings', environment') =
                              List.foldl
                                (fn (binding, (bindings, environment)) =>
                                  case binding of
                                    Ast.BVal {name, annot, value, span} =>
                                      let
                                        val value' =
                                          rewriteExpr
                                            ( environment, ownExpr, ownTypes
                                            , imports, qualifier, value
                                            )
                                        val annot' =
                                          Option.map rewriteTypeHere annot
                                      in
                                        ( Ast.BVal
                                            { name = name
                                            , annot = annot'
                                            , value = value'
                                            , span = span
                                            } :: bindings
                                        , name :: environment
                                        )
                                      end
                                  | Ast.BPat {pattern, value, span} =>
                                      let
                                        val value' =
                                          rewriteExpr
                                            ( environment, ownExpr, ownTypes
                                            , imports, qualifier, value
                                            )
                                        val (pattern', environment') =
                                          rewritePattern
                                            ( environment, ownExpr, imports
                                            , qualifier, pattern
                                            )
                                      in
                                        ( Ast.BPat
                                            { pattern = pattern'
                                            , value = value'
                                            , span = span
                                            } :: bindings
                                        , environment'
                                        )
                                      end
                                  | Ast.BFun {name, nameSpan, params, result,
                                              body, span} =>
                                      let
                                        val body' =
                                          rewriteExpr
                                            ( environment, ownExpr, ownTypes
                                            , imports, qualifier, body
                                            )
                                      in
                                        ( Ast.BFun
                                            { name = name
                                            , nameSpan = nameSpan
                                            , params = params
                                            , result = result
                                            , body = body'
                                            , span = span
                                            } :: bindings
                                        , environment
                                        )
                                      end
                                  | Ast.BGuard _ =>
                                      raise Fail
                                        "driver: desugared guard binding")
                                ([], environment)
                                bindings
                            val body' =
                              rewriteExpr
                                ( environment', ownExpr, ownTypes, imports
                                , qualifier, body
                                )
                          in
                            Ast.EArena (List.rev bindings', body', span)
                          end
                      | Ast.ERecord (fields, span) =>
                          Ast.ERecord
                            (List.map
                               (fn (field, value) => (field, recurse value))
                               fields,
                             span)
                      | Ast.EField (field, base, span) =>
                          (case base of

                             Ast.EVar (name, _) =>
                               if member name environment orelse
                                  member name ownExpr then
                                 Ast.EField (field, recurse base, span)
                               else
                                 (case List.find
                                         (fn (candidate, _) => candidate = name)
                                         (#qualifiers imports) of
                                    SOME (_, prefix) =>
                                      Ast.EVar (prefix ^ "." ^ field, span)
                                  | NONE => Ast.EField (field, recurse base, span))
                           | _ => Ast.EField (field, recurse base, span))
                      | Ast.ETuple (values, span) =>
                          Ast.ETuple (List.map recurse values, span)
                      | Ast.ETupleIndex (index, base, span) =>
                          Ast.ETupleIndex (index, recurse base, span)
                      | Ast.ECase (scrutinee, branches, span) =>
                          let
                            val scrutinee' = recurse scrutinee
                            fun rewriteBranch (pattern, guard, body) =
                              let
                                val (pattern', environment') =
                                  rewritePattern
                                    ( environment, ownExpr, imports, qualifier
                                    , pattern
                                    )
                                val guard' =
                                  Option.map
                                    (fn condition =>
                                      rewriteExpr
                                        ( environment', ownExpr, ownTypes
                                        , imports, qualifier, condition
                                        ))
                                    guard
                              in
                                ( pattern'
                                , guard'
                                , rewriteExpr
                                    ( environment', ownExpr, ownTypes, imports
                                    , qualifier, body
                                    )
                                )
                              end
                          in
                            Ast.ECase
                              (scrutinee', List.map rewriteBranch branches, span)
                          end
                      | Ast.EBorrow (mutable, ampersand, operand, span) =>
                          Ast.EBorrow (mutable, ampersand, recurse operand, span)
                      | Ast.EFn (params, result, body, span) =>
                          let
                            val params' =
                              List.map
                                (fn param =>
                                  { name = #name param
                                  , annot =
                                      Option.map rewriteTypeHere (#annot param)
                                  , span = #span param
                                  })
                                params
                            val environment' =
                              List.map (fn param => #name param) params @
                              environment
                          in
                            Ast.EFn
                              ( params'
                              , Option.map rewriteTypeHere result
                              , rewriteExpr
                                  ( environment', ownExpr, ownTypes, imports
                                  , qualifier, body
                                  )
                              , span
                              )
                          end
                      | Ast.EExtern _ => expression
                    end

                  and rewritePattern
                        (environment, ownExpr, imports, qualifier, pattern) =
                    let
                      val rewriteValueNameHere = fn name =>
                        if member name environment then name
                        else rewriteValueName (ownExpr, imports, qualifier, name)
                    in
                      case pattern of
                        Ast.PVar (name, span) =>
                          (pattern, name :: environment)
                      | Ast.PWildcard _ => (pattern, environment)
                      | Ast.PInt _ => (pattern, environment)
                      | Ast.PBool _ => (pattern, environment)
                      | Ast.PString (literal, rest, span) =>
                          let
                            val (rest', environment') =
                              case rest of
                                SOME subpattern =>
                                  let
                                    val (subpattern', environment') =
                                      rewritePattern
                                        ( environment, ownExpr, imports
                                        , qualifier, subpattern
                                        )
                                  in
                                    (SOME subpattern', environment')
                                  end
                              | NONE => (NONE, environment)
                          in
                            (Ast.PString (literal, rest', span), environment')
                          end
                      | Ast.PBytes (bytes, rest, span) =>
                          let
                            val (rest', environment') =
                              case rest of
                                SOME subpattern =>
                                  let
                                    val (subpattern', environment') =
                                      rewritePattern
                                        ( environment, ownExpr, imports
                                        , qualifier, subpattern
                                        )
                                  in
                                    (SOME subpattern', environment')
                                  end
                              | NONE => (NONE, environment)
                          in
                            (Ast.PBytes (bytes, rest', span), environment')
                          end
                      | Ast.PConstructor (name, argument, span) =>
                          let
                            val name' = rewriteValueNameHere name
                            val (argument', environment') =
                              case argument of
                                SOME subpattern =>
                                  let
                                    val (subpattern', environment') =
                                      rewritePattern
                                        ( environment, ownExpr, imports
                                        , qualifier, subpattern
                                        )
                                  in
                                    (SOME subpattern', environment')
                                  end
                              | NONE => (NONE, environment)
                          in
                            ( Ast.PConstructor (name', argument', span)
                            , environment'
                            )
                          end
                      | Ast.PTuple (patterns, span) =>
                          let
                            val (patterns', environment') =
                              List.foldl
                                (fn (subpattern, (patterns, environment)) =>
                                  let
                                    val (subpattern', environment') =
                                      rewritePattern
                                        ( environment, ownExpr, imports
                                        , qualifier, subpattern
                                        )
                                  in
                                    (subpattern' :: patterns, environment')
                                  end)
                                ([], environment)
                                patterns
                          in
                            (Ast.PTuple (List.rev patterns', span), environment')
                          end
                      | Ast.PRecord (fields, span) =>
                          let
                            val (fields', environment') =
                              List.foldl
                                (fn ((field, subpattern),
                                     (fields, environment)) =>
                                  let
                                    val (subpattern', environment') =
                                      rewritePattern
                                        ( environment, ownExpr, imports
                                        , qualifier, subpattern
                                        )
                                  in
                                    ((field, subpattern') :: fields,
                                     environment')
                                  end)
                                ([], environment)
                                fields
                          in
                            (Ast.PRecord (List.rev fields', span), environment')
                          end
                    end

                  fun rewriteDeclaration (qualifier, imports, own, declaration) =
                    let
                      val ownExpr = #functions own @ #constructors own
                      val ownTypes = #types own
                      val rewriteTypeHere = fn syntax =>
                        rewriteType (ownTypes, imports, qualifier, syntax)
                      val rewriteExprHere = fn (environment, expression) =>
                        rewriteExpr
                          ( environment, ownExpr, ownTypes, imports, qualifier
                          , expression
                          )
                      val qualify = fn name =>
                        qualifyDeclName (qualifier, name)
                    in
                      case declaration of
                        Ast.DFun
                          { name, nameSpan, params, result, recursive, body
                          , span } =>
                            let
                              val params' =
                                List.map
                                  (fn {name, annot, span} =>
                                    { name = name
                                    , annot = Option.map rewriteTypeHere annot
                                    , span = span
                                    })
                                  params
                              val environment = List.map (fn p => #name p) params
                            in
                              Ast.DFun
                                { name = qualify name
                                , nameSpan = nameSpan
                                , params = params'
                                , result = Option.map rewriteTypeHere result
                                , recursive = recursive
                                , body = rewriteExprHere (environment, body)
                                , span = span
                                }
                            end
                      | Ast.DType {name, declared, fields, span} =>
                          Ast.DType
                            { name = qualify name
                            , declared = declared
                            , fields =
                                List.map
                                  (fn (field, ty) =>
                                    (field, rewriteTypeHere ty))
                                  fields
                            , span = span
                            }
                      | Ast.DAlias {name, params, body, span} =>
                          Ast.DAlias
                            { name = qualify name
                            , params = params
                            , body = rewriteTypeHere body
                            , span = span
                            }
                      | Ast.DDatatype
                          { name, params, declared, constructors, span } =>
                          Ast.DDatatype
                            { name = qualify name
                            , params = params
                            , declared = declared
                            , constructors =
                                List.map
                                  (fn (constructor, argument) =>
                                    ( qualify constructor
                                    , Option.map rewriteTypeHere argument
                                    ))
                                  constructors
                            , span = span
                            }
                      | Ast.DExternRecord {name, nameSpan, size, fields, span} =>
                          Ast.DExternRecord
                            { name = qualify name
                            , nameSpan = nameSpan
                            , size = size
                            , fields =
                                List.map
                                  (fn (field, ty, offset) =>
                                    (field, rewriteTypeHere ty, offset))
                                  fields
                            , span = span
                            }
                      | _ => declaration
                    end

                  val qualifiedModules =
                    List.map
                      (fn (path, moduleProgram) =>
                        let
                          val own = exportsOf moduleProgram
                          val imports = importEnvironment moduleProgram
                          val qualifier = SOME (modulePrefix path)
                        in
                          ( path
                          , List.map
                              (fn declaration =>
                                rewriteDeclaration
                                  (qualifier, imports, own, declaration))
                              moduleProgram
                          )
                        end)
                      loaded

                  val mainOwn = exportsOf mainProgram
                  val mainImports = importEnvironment mainProgram
                  val mainRewritten =
                    List.map
                      (fn declaration =>
                        rewriteDeclaration
                          (NONE, mainImports, mainOwn, declaration))
                      mainProgram

                  val modulePrograms =
                    List.concat
                      (List.map
                         (fn (_, moduleProgram) =>
                           stripImports moduleProgram)
                         qualifiedModules)

                  val program =
                    prelude @ modulePrograms @ stripImports mainRewritten
                  val ownershipProgram = program
                  val resolved = Resolve.resolve program
                  val resolutionDiagnostics = #diagnostics resolved

                  fun realPathOf path =
                    let
                      val absolute =
                        (if OS.Path.isAbsolute path then path
                         else OS.Path.concat (OS.FileSys.getDir (), path))
                        handle _ => path
                    in
                      OS.FileSys.fullPath absolute
                      handle _ =>
                        (OS.Path.mkCanonical absolute handle _ => absolute)
                    end

                  fun underRoot (root, path) =
                    let
                      fun parts text =
                        let val {isAbs, vol, arcs} = OS.Path.fromString text
                        in (isAbs, vol, List.filter (fn arc => arc <> "") arcs)
                        end
                      val (rootAbs, rootVol, rootArcs) = parts root
                      val (pathAbs, pathVol, pathArcs) = parts path
                      fun prefix ([], _) = true
                        | prefix (_, []) = false
                        | prefix (r :: rs, p :: ps) =
                            r = p andalso prefix (rs, ps)
                    in
                      rootAbs = pathAbs andalso rootVol = pathVol andalso
                      prefix (rootArcs, pathArcs)
                    end

                  val sourceRealPaths =
                    List.map
                      (fn entry =>
                        ( #id entry
                        , if String.isPrefix "<" (#path entry) then NONE
                          else SOME (realPathOf (#path entry))
                        ))
                      (!sources)
                  fun sourceRealPath fileId =
                    case List.find (fn (id, _) => id = fileId) sourceRealPaths of
                      SOME (_, path) => path
                    | NONE => NONE
                  val excludedRoots =
                    List.map realPathOf
                      ((case implicitYackRoot of SOME root => [root] | NONE => []) @
                       List.map
                         (fn (alias, _) => dependencyRoot alias)
                         dependencies)
                  fun userSourcePath fileId =
                    case sourceRealPath fileId of
                      SOME path =>
                        if List.exists
                             (fn root => underRoot (root, path))
                             excludedRoots
                        then NONE
                        else SOME path
                    | NONE => NONE
                  fun userOwnsBinding (binding : Resolve.binding) =
                    case userSourcePath (Pos.file (#start (#span binding))) of
                      SOME _ => true
                    | NONE => false
                  fun userOwnsSpan span =
                    case userSourcePath (Pos.file (#start span)) of
                      SOME _ => true
                    | NONE => false

                  val kernelTrusted =
                    if List.exists (fn (alias, _) => alias = "yack") dependencies
                    then SOME (realPathOf (dependencyRoot "yack"))
                    else Option.map realPathOf implicitYackRoot
                  fun kernelAllowed fileId =
                    fileId = preludeFileId orelse
                    (case (kernelTrusted, sourceRealPath fileId) of
                       (SOME trusted, SOME path) => underRoot (trusted, path)
                     | _ => false)
                  val kernelBindings =
                    List.foldl
                      (fn (binding : Resolve.binding, found) =>
                        case #kind binding of
                          Resolve.Builtin =>
                            if LowerBuiltins.isReservedArrayKernel (#name binding)
                               orelse LowerBuiltins.isReservedExternView (#name binding)
                               orelse LowerBuiltins.isReservedBytes (#name binding)
                            then (#id binding, #name binding) :: found
                            else found
                        | _ => found)
                      []
                      (#bindings resolved)
                  fun kernelNameOf bindingId =
                    case List.find (fn (id, _) => id = bindingId) kernelBindings of
                      SOME (_, name) => SOME name
                    | NONE => NONE
                  val kernelDiagnostics =
                    List.mapPartial
                      (fn (use : Resolve.use) =>
                        case kernelNameOf (#binding use) of
                          NONE => NONE
                        | SOME name =>
                            if not (kernelAllowed (Pos.file (#start (#span use))))
                            then
                              let
                                val isView = LowerBuiltins.isReservedExternView name
                                val isBytes = LowerBuiltins.isReservedBytes name
                                val isScratch = LowerBuiltins.isReservedScratch name
                              in
                                SOME
                                  (Diag.error
                                     ( "E3042"
                                     , "`" ^ name ^ "` is " ^
                                       (if isView then "an extern record primitive"
                                        else if isBytes then "an internal byte primitive"
                                        else if isScratch then "an internal arena primitive"
                                        else "an internal array primitive")
                                     , #span use
                                     , [ if isView then
                                           "it can reinterpret any byte buffer as a typed C struct"
                                         else if isBytes then
                                           "it writes raw bytes with an absolute limit and no bounds check"
                                         else if isScratch then
                                           "it manipulates the arena scope markers directly"
                                         else
                                           "it reads or writes a slot without testing the index" ]
                                     , [ if isView then
                                           "write the view in the standard library, next to the extern record declaration"
                                         else if isBytes then
                                           "use `yack/bytes` (`to_string_range`, `write_string`, `write_int`)"
                                         else if isScratch then
                                           "use an `arena` block"
                                         else
                                           "use the checked functions of `yack/array` (`array.get`, `array.set`, `array.push`)" ]
                                     ))
                              end
                            else NONE)
                      (#uses resolved)

                  val externDeclarations =
                    List.mapPartial
                      (fn declaration =>
                        case declaration of
                          Ast.DFun {name, nameSpan, body = Ast.EExtern _, ...} =>
                            SOME (name, nameSpan)
                        | _ => NONE)
                      program
                  val externRecordDeclarations =
                    List.mapPartial
                      (fn declaration =>
                        case declaration of
                          Ast.DExternRecord {name, nameSpan, ...} =>
                            SOME (name, nameSpan)
                        | _ => NONE)
                      program
                  val externRecordDiagnostics =
                    List.mapPartial
                      (fn (name, nameSpan) =>
                        if kernelAllowed (Pos.file (#start nameSpan)) then NONE
                        else
                          SOME
                            (Diag.error
                               ( "E3042"
                               , "`" ^ Diag.displayName name ^
                                 "` is an extern record declared outside the standard library"
                               , nameSpan
                               , ["extern records describe raw C memory"]
                               , [ "move the declaration to the resolved standard library" ])))
                      externRecordDeclarations
                  val externFiles =
                    List.map
                      (fn (name, nameSpan) =>
                        (name, Pos.file (#start nameSpan)))
                      externDeclarations
                  fun externFileOf bindingId =
                    case List.find
                           (fn (binding : Resolve.binding) =>
                             #id binding = bindingId)
                           (#bindings resolved) of
                      NONE => NONE
                    | SOME binding =>
                        (case List.find
                                (fn (name, file) => name = #name binding)
                                externFiles of
                           SOME (_, file) => SOME (#name binding, file)
                         | NONE => NONE)
                  val externDeclarationDiagnostics =
                    List.mapPartial
                      (fn (name, nameSpan) =>
                        if kernelAllowed (Pos.file (#start nameSpan)) then NONE
                        else
                          SOME
                            (Diag.error
                               ( "E3042"
                               , "`" ^ Diag.displayName name ^
                                 "` is an extern declared outside the standard library"
                               , nameSpan
                               , ["extern exposes libc without a checked wrapper"]
                               , [ "move the declaration to the resolved standard library"
                                 , "user code calls the typed functions of `yack/time`, `yack/fs`, ..."
                                 ]
                               )))
                      externDeclarations
                  val externUseDiagnostics =
                    List.mapPartial
                      (fn (use : Resolve.use) =>
                        case externFileOf (#binding use) of
                          NONE => NONE
                        | SOME (name, file) =>
                            if Pos.file (#start (#span use)) = file then NONE
                            else
                              SOME
                                (Diag.error
                                   ( "E3043"
                                   , "`" ^ Diag.displayName name ^
                                     "` is an extern private to its module"
                                   , #span use
                                   , ["an extern is never exported by its module"]
                                   , [ "import the public wrapper of that module instead" ])))
                      (#uses resolved)
                  val unusedWarnings =
                    List.map
                      (fn binding : Resolve.binding =>
                        let val name = Diag.displayName (#name binding) in
                          Diag.warning
                            ( "W3001"
                            , "function `" ^ name ^ "` is never used"
                            , #span binding
                            , []
                            , [ "remove the definition of `" ^ name ^
                                "`, or call it from the code that needs it" ]
                            )
                        end)
                      (List.filter
                         (fn binding : Resolve.binding =>
                           #name binding <> "main" andalso
                           not (String.isPrefix "__yack_local_" (#name binding))
                           andalso userOwnsBinding binding)
                         (Resolve.unusedFunctions resolved))
                  val resolutionDiagnostics =
                    resolutionDiagnostics @ unusedWarnings @ kernelDiagnostics @
                    externDeclarationDiagnostics @ externUseDiagnostics @
                    externRecordDiagnostics
                  val qualifyDiagnostics' = List.rev (!qualifyDiagnostics)

                  val reportedKeys = ref ([] : (string * Diag.span) list)
                  fun reportPhase diagnostics =
                    let
                      fun fresh diagnostic =
                        List.exists
                          (fn (code, span) =>
                            code = #code diagnostic andalso
                            span = #span diagnostic)
                          (!reportedKeys)
                      val kept = List.filter (fn d => not (fresh d)) diagnostics
                      val _ =
                        reportedKeys :=
                          List.map
                            (fn d => (#code d, #span d))
                            kept @ !reportedKeys
                    in
                      if List.null kept then ()
                      else #report options (!sources, kept)
                    end

                  val unknownNameCodes = ["E3001", "E3002", "E3012", "E3027"]
                  fun targetsFailedName (diagnostic : Diag.t) =
                    List.exists
                      (fn code => code = #code diagnostic)
                      unknownNameCodes andalso
                    List.exists
                      (fn name =>
                        String.isSubstring ("`" ^ name ^ "`")
                          (#message diagnostic))
                      parseFailedNames

                  fun spanContains (outer : Diag.span, inner : Diag.span) =
                    let
                      val a = #start outer
                      val b = #stop outer
                      val p = #start inner
                    in
                      Pos.file a = Pos.file p andalso
                      Pos.offset a <= Pos.offset p andalso
                      Pos.offset p <= Pos.offset b
                    end
                  fun occursIn span (diagnostic : Diag.t) =
                    spanContains (span, #span diagnostic)
                  val declarationSpans = List.map Ast.declSpan program
                  fun reportLater earlier diagnostics =
                    let
                      val tainted =
                        List.filter
                          (fn span =>
                            List.exists
                              (fn diagnostic =>
                                #severity diagnostic = Diag.Error andalso
                                occursIn span diagnostic)
                              earlier)
                          declarationSpans
                      val kept =
                        List.filter
                          (fn diagnostic =>
                            not (List.exists
                                   (fn span => occursIn span diagnostic)
                                   tainted))
                          diagnostics
                    in
                      reportPhase kept
                    end
                in
                  reportPhase qualifyDiagnostics';
                  if Diag.hasErrors qualifyDiagnostics' then NONE
                  else
                    let
                      val _ = InternalContext.set ("typing " ^ file)
                      val _ = testFailure "typing"
                      val checked =
                        Check.checkWithPrelude
                          (preludeFileId, #requireMain options, program)
                      val typeDiagnostics = #diagnostics checked

                      val resolutionReported =
                        List.filter
                          (fn diagnostic => not (targetsFailedName diagnostic))
                          resolutionDiagnostics
                      val typeReported =
                        List.filter
                          (fn diagnostic => not (targetsFailedName diagnostic))
                          typeDiagnostics
                      val _ = reportPhase resolutionReported
                      val _ = reportPhase typeReported
                      val earlier =
                        syntaxDiagnostics @ qualifyDiagnostics' @
                        resolutionReported @ typeReported
                      val hardErrors = Diag.hasErrors earlier
                      val _ =
                        ( InternalContext.set ("checking ownership in " ^ file)
                        ; testFailure "ownership" )

                      val ownershipResult =
                        SOME
                          (Ownership.check
                             { program = ownershipProgram
                             , resolution = resolved
                             , functions = #functions checked
                             , bindingTypes = #bindingTypes checked
                             , expressionTypes = #expressionTypes checked
                             })
                        handle error =>
                          if #tolerant options orelse hardErrors then NONE
                          else raise error
                      val ownershipDiagnostics =
                        case ownershipResult of
                          SOME ownership => #diagnostics ownership
                        | NONE => []
                      val _ = reportLater earlier ownershipDiagnostics
                      val containerDiagnostics =
                        case ownershipResult of
                          SOME ownership =>
                            ((ContainerWarnings.check
                                { program = ownershipProgram
                                , functions = #functions checked
                                , readonlyParams = #readonlyParams ownership
                                , ownsSpan = userOwnsSpan
                                })
                             handle error =>
                               if #tolerant options orelse hardErrors then []
                               else raise error)
                        | NONE => []
                      val _ = reportLater earlier containerDiagnostics
                      val ownershipErrors =
                        Diag.hasErrors ownershipDiagnostics orelse
                        not (isSome ownershipResult)
                      fun ownershipField pick fallback =
                        case ownershipResult of
                          SOME ownership => pick ownership
                        | NONE => fallback
                    in

                      if #tolerant options
                         orelse (not hardErrors andalso not ownershipErrors)
                         orelse
                           (not hardErrors andalso
                            isSome ownershipResult andalso
                            OS.Process.getEnv "YACK_BORROW_BYPASS" = SOME "1")
                      then
                        SOME
                          { source = source
                          , sources = !sources
                          , program = program
                          , resolution = resolved
                          , functions = #functions checked
                          , bindingTypes = #bindingTypes checked
                          , expressionTypes = #expressionTypes checked
                          , calls = #calls checked
                          , functionValues = #functionValues checked
                          , applications = #applications checked
                          , drops = ownershipField #drops []
                          , partialDrops = ownershipField #partialDrops []
                          , earlyDrops = ownershipField #earlyDrops []
                          , borrowedLambdaParams =
                              ownershipField #borrowedLambdaParams []
                          , types = #types checked
                          , aliases = #aliases checked
                          , datatypes = #datatypes checked
                          }
                      else NONE
                    end
                end
            end
        end)
    handle ModuleAbort => NONE
         | OS.Path.Path => (#note options "invalid dependency path"; NONE)
         | OS.Path.InvalidArc => (#note options "invalid dependency path"; NONE)

  fun loadWithMode mode file =
    loadWith
      { read = DriverIo.readFile
      , requireMain = true
      , memory = mode
      , tolerant = false
      , report = fn (sources, diagnostics) =>
          DriverIo.renderDiags sources diagnostics
      , note = fn message =>
          TextIO.output (TextIO.stdErr, "error: " ^ message ^ "\n")
      }
      file

  fun requiresMain path =
    OS.Path.file path = "main.yack" orelse not (isSome (findManifest path))

  fun loadForCheck mode file =
    loadWith
      { read = DriverIo.readFile
      , requireMain = requiresMain file
      , memory = mode
      , tolerant = false
      , report = fn (sources, diagnostics) =>
          DriverIo.renderDiags sources diagnostics
      , note = fn message =>
          TextIO.output (TextIO.stdErr, "error: " ^ message ^ "\n")
      }
      file

  fun load file = loadWithMode Memory.Heap file

  fun checkWithMode mode file =
    case loadWithMode mode file of
      NONE => 1
    | SOME _ => 0

  fun check file = checkWithMode Memory.Heap file
end
