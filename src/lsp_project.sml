structure LspProject =
struct
  open LspText
  open LspAst

  type doc =
    { uri : string
    , path : string
    , text : string
    , version : int
    }

  type report =
    { sources : Driver.sourceEntry list
    , diagnostics : Diag.t list
    }

  type analysis =
    { checked : Driver.checked option
    , reports : report list
    , notes : string list
    , mode : Memory.mode
    , arenas : ArenaReport.block list
    }

  type indexModule =
    { key : string
    , path : string
    , exports : (string * string) list
    , program : Ast.program
    , text : string
    }

  fun analyze read path =
    let
      val reports = ref []
      val notes = ref []
      val settings = Driver.manifestSettingsFor path
      val memory =
        case #mode settings of
          SOME mode => mode
        | NONE => Memory.Heap
      val checked =
        Driver.loadWith
          { read = read
          , requireMain = Driver.requiresMain path
          , memory = memory
          , tolerant = true
          , report = fn (sources, diagnostics) =>
              reports := { sources = sources, diagnostics = diagnostics } :: !reports
          , note = fn message => notes := message :: !notes
          }
          path

      val hasErrors =
        List.exists
          (fn { diagnostics, ... } => Diag.hasErrors diagnostics)
          (!reports)

      val arenas =
        if hasErrors then []
        else
        case checked of
          SOME c =>
            (let
               val lowered = Driver.lowerForCheck path memory c
               val diagnostics = #diagnostics lowered
               val _ =
                 if null diagnostics then ()
                 else
                   reports :=
                     { sources = #sources c, diagnostics = diagnostics } :: !reports
             in
               case read path of
                 SOME text =>
                   Driver.arenaBlocks
                     { file = path, text = text, mode = memory
                     , checked = c, lowered = lowered }
               | NONE => []
             end
             handle _ => [])
        | NONE => []
    in
      { checked = checked
      , reports = List.rev (!reports)
      , notes = List.rev (!notes)
      , mode = memory
      , arenas = arenas
      }
    end

  fun pathExists path = OS.FileSys.access (path, [OS.FileSys.A_READ])
  fun isDirectory path = OS.FileSys.isDir path handle _ => false

  fun parentDir path =
    let val parent = OS.Path.dir path in
      if parent = path then path else parent
    end

  fun absoluteDirectory path =
    let
      val path =
        if OS.Path.isAbsolute path then path
        else OS.Path.concat (OS.FileSys.getDir (), path)
    in
      OS.Path.mkCanonical path
    end

  fun findPackageRoot start =
    let
      val start = absoluteDirectory start
      fun loop dir =
        if pathExists (OS.Path.concat (dir, "yack.toml")) then dir
        else
          let val parent = parentDir dir in
            if parent = dir then start else loop parent
          end
    in
      loop start
    end

  fun manifestPath path =
    let
      val root = findPackageRoot (OS.Path.dir path)
      val manifest = OS.Path.concat (root, "yack.toml")
    in
      if pathExists manifest then SOME manifest else NONE
    end

  fun manifestStamp path =
    case manifestPath path of
      NONE => ""
    | SOME manifest =>
        (Time.toString (OS.FileSys.modTime manifest)) handle _ => ""

  fun indexKey path = findPackageRoot (OS.Path.dir path)

  val findStdlibRoot = StdlibRoot.find

  fun stdlibSource root =
    let val candidate = OS.Path.concat (root, "src") in
      if isDirectory candidate then candidate else root
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
    case Driver.readFile manifest of
      NONE => []
    | SOME contents =>
        let
          val dependencies = ref []
          val section = ref ""
          fun parseLine line =
            let val line = trim line in
              if line = "" orelse String.isPrefix "#" line then ()
              else if String.isPrefix "[" line andalso String.isSuffix "]" line
              then section := String.substring (line, 1, String.size line - 2)
              else if !section = "dependencies" then
                case String.fields (fn character => character = #"=") line of
                  [] => ()
                | key :: rest =>
                    let val path = quoted (String.concatWith "=" rest) in
                      if path = "" then ()
                      else dependencies := (trim key, path) :: !dependencies
                    end
              else ()
            end
        in
          List.app parseLine
            (String.fields (fn character => character = #"\n") contents);
          List.rev (!dependencies)
        end

  fun withoutExtension name =
    if String.isSuffix ".yack" name then
      String.substring (name, 0, String.size name - 5)
    else name

  val skipDirectories =
    [ ".git", ".trail", ".commandcode", ".yack", "node_modules", "bench", "test" ]

  fun scanRoot depth root =
    let
      val modules = ref []
      fun walk (remaining, directory, segments) =
        let
          val directoryHandle = OS.FileSys.openDir directory
          fun loop () =
            case OS.FileSys.readDir directoryHandle of
              NONE => ()
            | SOME name =>
                let
                  val path = OS.Path.concat (directory, name)
                  val _ =
                    if isDirectory path then
                      if remaining <= 0 then ()
                      else if List.exists (fn skip => skip = name) skipDirectories then ()
                      else walk (remaining - 1, path, segments @ [name])
                    else if String.isSuffix ".yack" name then
                      case Driver.readFile path of
                        NONE => ()
                      | SOME text =>
                          (case desugarText text of
                             NONE => ()
                           | SOME program =>
                               let
                                 val key =
                                   String.concatWith "/"
                                     (segments @ [withoutExtension name])
                               in
                                 modules :=
                                   { key = key
                                   , path = path
                                   , exports = exportsOf program
                                   , program = program
                                   , text = text
                                   } :: !modules
                               end)
                    else ()
                in
                  loop ()
                end
          val _ = (loop ()) handle _ => ()
          val _ = (OS.FileSys.closeDir directoryHandle) handle _ => ()
        in
          ()
        end
    in
      walk (depth, root, []);
      !modules
    end

  fun buildIndex path =
    let
      val root = findPackageRoot (OS.Path.dir path)
      val sourceRoot =
        let val candidate = OS.Path.concat (root, "src") in
          if isDirectory candidate then candidate else root
        end
      val manifest = OS.Path.concat (root, "yack.toml")
      val hasManifest = pathExists manifest
      val dependencies = if hasManifest then readDependencies manifest else []
      val depth = if hasManifest then 12 else 0
      val implicitRoots =
        if List.exists (fn (alias, _) => alias = "yack") dependencies then []
        else
          case findStdlibRoot (OS.Path.dir path) of
            NONE => []
          | SOME stdlib => [("yack", stdlibSource stdlib, 12)]
      val roots =
        ("", sourceRoot, depth) ::
        List.map
          (fn (alias, relative) =>
            let
              val dependency = DriverLoad.resolveDependency (root, relative)
              val candidate = OS.Path.concat (dependency, "src")
            in
              (alias, if isDirectory candidate then candidate else dependency, depth)
            end)
          dependencies @
        implicitRoots
      val scanned =
        List.concat
          (List.map
             (fn (prefix, directory, maximum) =>
               List.map
                 (fn module =>
                   if prefix = "" then module
                   else { key = prefix ^ "/" ^ #key module
                        , path = #path module
                        , exports = #exports module
                        , program = #program module
                        , text = #text module
                        })
                 (scanRoot maximum directory))
             roots)
      val prelude =
        case desugarText Driver.preludeText of
          NONE => []
        | SOME program =>
            [ { key = "<prelude>"
              , path = "<prelude>"
              , exports = exportsOf program
              , program = program
              , text = Driver.preludeText
              }
            ]
      val builtins =
        [ { key = "<builtins>"
          , path = "<builtins>"
          , exports = List.map (fn name => ("function", name)) Check.builtins
          , program = []
          , text = ""
          }
        ]
      val modules = prelude @ builtins @ scanned
    in
      modules
    end
end
