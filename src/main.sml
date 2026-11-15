val issuesUrl = "https://github.com/AsymetricData/yack-public/issues"

fun errorLine text = TextIO.output (TextIO.stdErr, text ^ "\n")

fun messageOf error =
  let
    val name = General.exnName error
    val message = General.exnMessage error
  in
    if String.isPrefix (name ^ ": ") message orelse message = name then message
    else name ^ ": " ^ message
  end

fun isSystemError error =
  case error of
    IO.Io _ => true
  | OS.SysErr _ => true
  | _ => false

fun reportSystem error =
  ( errorLine
      (case error of
         IO.Io {name, function, cause} =>
           "error: " ^ function ^ " failed on `" ^ name ^ "`: " ^
           General.exnMessage cause
       | OS.SysErr (message, _) => "error: " ^ message
       | _ => "error: " ^ messageOf error)
  ; TextIO.flushOut TextIO.stdErr
  ; OS.Process.exit OS.Process.failure
  )

fun reportInternal error =
  ( errorLine ("internal error[E0902]: " ^ messageOf error)
  ; errorLine ("  = note: while " ^ InternalContext.get ())
  ; errorLine "  = note: this is a bug in the Yack compiler, not in your program"
  ; errorLine
      ("  = help: please report it at " ^ issuesUrl ^
       " with the command and the file above (" ^ DriverUi.version ^ ")")

  ; TextIO.flushOut TextIO.stdOut
  ; TextIO.flushOut TextIO.stdErr
  ; Posix.Process.exit (Word8.fromInt 70)
  )

val main =
  let val arguments = CommandLine.arguments () in
    if arguments = ["lsp"] orelse arguments = ["lsp", "--stdio"] then Lsp.main ()
    else (Driver.main (); 0)
         handle error =>
           if isSystemError error then reportSystem error
           else reportInternal error
  end
