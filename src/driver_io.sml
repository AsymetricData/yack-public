structure DriverIo =
struct
  open DriverTypes

  val explainCode : string option ref = ref NONE

  fun documented code =
    List.exists
      (fn (candidate, _, _) => candidate = code)
      ErrorDocs.entries

  fun noteDiagnostic (diagnostic : Diag.t) =
    case !explainCode of
      SOME _ => ()
    | NONE =>
        let val code = #code diagnostic in
          if code <> "E0902" andalso documented code
          then explainCode := SOME code
          else ()
        end

  fun explainHint () =
    case !explainCode of
      NONE => ()
    | SOME code =>
        ( TextIO.output
            ( TextIO.stdErr
            , "For more information about an error, try 'yack explain " ^
              code ^ "'.\n"
            )
        ; explainCode := NONE
        ; TextIO.flushOut TextIO.stdErr
        )

  fun readFile path =
    ( let
        val input = TextIO.openIn path
        val contents = TextIO.inputAll input
        val _ = TextIO.closeIn input
      in
        SOME contents
      end
      handle _ => NONE )

  fun renderAll file source diagnostics =
    let val sourceMap = Source.create source in
      List.app
        (fn diagnostic =>
          ( noteDiagnostic diagnostic
          ; TextIO.output
              ( TextIO.stdErr
              , Diag.renderWith
                  (!diagnosticStyle,
                   { source = sourceMap
                   , file = file
                   , severity = #severity diagnostic
                   })
                  diagnostic
              )
          ))
        diagnostics
    end

  fun renderDiags (sources : sourceEntry list) diagnostics =
    case sources of
      [] => ()
    | fallback :: _ =>
        let

          val seen = ref []
          fun duplicate diagnostic =
            let
              val key =
                ( #code diagnostic
                , #message diagnostic
                , #start (#span diagnostic)
                , #stop (#span diagnostic)
                )
              val already = List.exists (fn k => k = key) (!seen)
              val _ = if already then () else seen := key :: !seen
            in
              already
            end
          fun render (diagnostic) =
            let
              val fileId = Pos.file (#start (#span diagnostic))
              val entry =
                case List.find (fn entry => #id entry = fileId) sources of
                  SOME entry => entry
                | NONE => fallback
            in
              noteDiagnostic diagnostic;
              TextIO.output
                ( TextIO.stdErr
                , Diag.renderWith
                    (!diagnosticStyle,
                     { source = #map entry
                     , file = #path entry
                     , severity = #severity diagnostic
                     })
                    diagnostic
                )
            end
        in
          List.app
            (fn diagnostic =>
              if duplicate diagnostic then () else render diagnostic)
            diagnostics
        end

  fun shellQuote text =
    "'" ^
    String.translate
      (fn #"'" => "'\\''"
        | ch => Char.toString ch)
      text ^ "'"

  fun writeText path text =
    let
      val output = TextIO.openOut path
      val _ = TextIO.output (output, text)
      val _ = TextIO.closeOut output
    in
      true
    end
    handle _ => false
end
