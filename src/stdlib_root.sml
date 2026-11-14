structure StdlibRoot =
struct
  fun pathExists path =
    OS.FileSys.access (path, [OS.FileSys.A_READ]) handle _ => false

  fun parentDir path =
    let val parent = OS.Path.dir path in
      if parent = path then path else parent
    end

  fun absolutePath path =
    if OS.Path.isAbsolute path then path
    else OS.Path.concat (OS.FileSys.getDir (), path)

  fun canonicalPath path =
    OS.Path.mkCanonical path handle _ => path

  fun absoluteDirectory path = canonicalPath (absolutePath path)

  fun stdlibAt dir =
    let
      val candidate = OS.Path.concat (OS.Path.concat (dir, "lib"), "yack")
    in
      if pathExists (OS.Path.concat (candidate, "yack.toml"))
      then SOME candidate
      else NONE
    end

  fun walkUp start =
    let
      fun loop dir =
        case stdlibAt dir of
          SOME root => SOME root
        | NONE =>
            let val parent = parentDir dir in
              if parent = dir then NONE else loop parent
            end
    in
      loop (absoluteDirectory start)
    end

  fun fromEnvironment () =
    case OS.Process.getEnv "YACK_LIB" of
      NONE => NONE
    | SOME path =>
        let
          val dir = absoluteDirectory path
          val direct = OS.Path.concat (dir, "yack")
        in
          if pathExists (OS.Path.concat (direct, "yack.toml"))
          then SOME direct
          else walkUp dir
        end

  fun executableDirectory () =
    let
      val name = CommandLine.name ()
      val hasSeparator =
        List.exists (fn character => character = #"/") (String.explode name)
    in
      if hasSeparator
      then SOME (parentDir (absoluteDirectory name))
      else NONE
    end

  fun fromExecutable () =
    case executableDirectory () of
      NONE => NONE
    | SOME dir =>
        let
          val candidate = OS.Path.concat (OS.Path.concat (dir, "lib"), "yack")
        in
          if pathExists (OS.Path.concat (candidate, "yack.toml"))
          then SOME candidate
          else NONE
        end

  fun firstSome candidates =
    case candidates of
      [] => NONE
    | candidate :: rest =>
        (case candidate () of
           SOME value => SOME value
         | NONE => firstSome rest)

  fun find start =
    firstSome
      [ fn () => walkUp start
      , fromEnvironment
      , fromExecutable
      ]
end
