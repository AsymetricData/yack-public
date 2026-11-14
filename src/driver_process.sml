structure DriverProcess =
struct
  open DriverUi
  open DriverIo

  datatype forkResult =
      ForkChild
    | ForkParent of Posix.Process.pid
    | ForkUnavailable

  fun forkProcess () =
    (case Posix.Process.fork () of
       NONE => ForkChild
     | SOME pid => ForkParent pid)
    handle _ => ForkUnavailable

  fun exitSucceeded Posix.Process.W_EXITED = true
    | exitSucceeded (Posix.Process.W_EXITSTATUS code) = Word8.toInt code = 0
    | exitSucceeded _ = false

  fun exitCode Posix.Process.W_EXITED = 0
    | exitCode (Posix.Process.W_EXITSTATUS code) = Word8.toInt code
    | exitCode _ = 130

  fun waitBlocking pid =
    let val (_, status) =
          Posix.Process.waitpid (Posix.Process.W_CHILD pid, [])
    in status end

  fun waitWithSpinner (style : style, stream, label, pid) =
    let
      val frames = spinnerFrames style
      val count = List.length frames
      fun loop index =
        case Posix.Process.waitpid_nh (Posix.Process.W_CHILD pid, []) of
          SOME (_, status) => status
        | NONE =>
            ( spinnerShow
                (style, stream, label, List.nth (frames, index mod count))
            ; Posix.Process.sleep (Time.fromMilliseconds 70)
            ; loop (index + 1)
            )
    in
      loop 0
    end

  fun printCaptured stream path =
    case readFile path of
      SOME contents =>
        if contents <> "" then TextIO.output (stream, contents) else ()
    | NONE => ()

  fun runCaptured (style : style, stream, interactive, label, command, logPath) =
    let
      val started = Time.now ()
      val _ = TextIO.flushOut TextIO.stdOut
      val _ = TextIO.flushOut TextIO.stdErr
      val status =
        case forkProcess () of
          ForkChild =>
            ( Posix.Process.execp ("/bin/sh", ["sh", "-c", command])
              handle _ => Posix.Process.exit (Word8.fromInt 127)
            )
        | ForkParent pid =>
            if interactive then waitWithSpinner (style, stream, label, pid)
            else waitBlocking pid
        | ForkUnavailable =>
            let val status = OS.Process.system command in
              if OS.Process.isSuccess status then Posix.Process.W_EXITED
              else Posix.Process.W_EXITSTATUS (Word8.fromInt 1)
            end
      val succeeded = exitSucceeded status
      val _ =
        if interactive then
          if succeeded then
            spinnerDone
              (style, stream, label, formatDuration (elapsed started))
          else spinnerFail (style, stream, "Compilation failed")
        else ()
      val _ = printCaptured stream logPath
      val _ = OS.FileSys.remove logPath handle _ => ()
    in
      succeeded
    end

  fun runProgram path =
    let
      val _ = TextIO.flushOut TextIO.stdOut
      val _ = TextIO.flushOut TextIO.stdErr
      val status =
        case forkProcess () of
          ForkChild =>
            ( Posix.Process.execp (path, [path])
              handle _ => Posix.Process.exit (Word8.fromInt 127)
            )
        | ForkParent pid => waitBlocking pid
        | ForkUnavailable =>
            let val status = OS.Process.system (shellQuote path) in
              if OS.Process.isSuccess status then Posix.Process.W_EXITED
              else Posix.Process.W_EXITSTATUS (Word8.fromInt 1)
            end
    in
      exitCode status
    end
end
