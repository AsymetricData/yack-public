structure DriverBuild =
struct
  open DriverCommands
  open DriverUi
  open DriverProcess
  open DriverIo
  open DriverLoad

  fun usesSqlite text =
    let
      fun hasSymbol line = String.isSubstring "@sqlite3_" line
      fun isDeclare line =
        case String.tokens Char.isSpace line of
          ("declare" :: _) => true
        | _ => false
      fun loop [] = false
        | loop (line :: rest) =
            (not (isDeclare line) andalso hasSymbol line) orelse loop rest
    in
      loop (String.tokens (fn c => c = #"\n") text)
    end

  fun compileProject (style : style) interactive dev mode reserve trim file outputPath display =
    let
      val checkStart = Time.now ()
      val writtenLabel =
        case display of
          SOME name => "Wrote " ^ name ^ ".ll"
        | NONE => "Wrote LLVM IR"
      val compilingLabel =
        case display of
          SOME name => "Compiling " ^ name
        | NONE => "Compiling program"
      fun lower checked =
        let val lowerStart = Time.now () in
          case lowerChecked mode file checked of
            NONE => false
          | SOME program =>
              let
                val emitStart = Time.now ()
                val llvmPath = outputPath ^ ".ll"
                val llvmText = Llvm.generate (mode, reserve, program)
                val _ =
                  if interactive then
                    stepTimed
                      ( style, TextIO.stdErr, "Lowered to typed IR"
                      , formatDuration (elapsed lowerStart)
                      )
                  else ()
              in
                if not (writeText llvmPath llvmText) then
                  ( TextIO.output
                      ( TextIO.stdErr
                      , "yack: cannot write `" ^ llvmPath ^ "`\n"
                      )
                  ; false
                  )
                else
                  let
                    val logPath = outputPath ^ ".clang.log"
                    val sanitize =
                      if Memory.poisonEnabled () then
                        "-fsanitize=address -g -fno-omit-frame-pointer "
                      else ""
                    val optimize =
                      if trim then
                        "-Os -ffunction-sections -fdata-sections " ^
                        "-Wl,--gc-sections "
                      else if dev then
                        "-O0 "
                      else
                        "-O2 "

                    val strip =
                      if trim andalso not (Memory.poisonEnabled ()) then
                        "-Wl,--strip-all "
                      else
                        ""
                    val sqlite =
                      if usesSqlite llvmText then " -lsqlite3" else ""
                    val command =
                      "clang " ^ optimize ^ strip ^ sanitize ^
                      "-Wno-override-module -o " ^
                      shellQuote outputPath ^ " " ^
                      shellQuote llvmPath ^ sqlite ^ " > " ^
                      shellQuote logPath ^ " 2>&1"
                    val _ =
                      if interactive then
                        stepTimed
                          ( style, TextIO.stdErr, writtenLabel
                          , formatDuration (elapsed emitStart)
                          )
                      else ()
                  in
                    let
                      val succeeded =
                        runCaptured
                          ( style, TextIO.stdErr, interactive
                          , compilingLabel
                          , command, logPath
                          )
                    in
                      if succeeded andalso interactive then
                        (case fileSizeOpt outputPath of
                           SOME bytes =>
                             binaryStep
                               (style, TextIO.stdErr, outputPath, bytes)
                         | NONE => ())
                      else ();
                      succeeded
                    end
                  end
              end
        end
    in
      case loadWithMode mode file of
        NONE => false
      | SOME checked =>
          ( if interactive then
              stepTimed
                ( style, TextIO.stdErr, "Checked " ^ file
                , formatDuration (elapsed checkStart)
                )
            else ()
          ; lower checked
          )
    end

  fun build mode reserve trim file outputPath =
    let
      val style = makeStyle (ColorAuto, Posix.FileSys.stderr)
      val interactive = progressEnabled Posix.FileSys.stderr
    in
      if compileProject style interactive false mode reserve trim file outputPath
           (SOME outputPath) then
        0
      else 1
    end

  fun runFile mode reserve trim file =
    let
      val style = makeStyle (ColorAuto, Posix.FileSys.stderr)
      val interactive = progressEnabled Posix.FileSys.stderr
      val outputPath = OS.FileSys.tmpName ()
      val llvmPath = outputPath ^ ".ll"
      val logPath = outputPath ^ ".clang.log"
      fun cleanup () =
        ( OS.FileSys.remove outputPath handle _ => ()
        ; OS.FileSys.remove llvmPath handle _ => ()
        ; OS.FileSys.remove logPath handle _ => ()
        )
    in
      if not (compileProject style interactive true mode reserve trim file outputPath NONE) then
        ( cleanup (); 1 )
      else
        let
          val _ =
            if interactive then
              runningStep (style, TextIO.stdErr, "Running " ^ file)
            else ()
          val code = runProgram outputPath
          val _ = cleanup ()
        in
          code
        end
    end
end
