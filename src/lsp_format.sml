structure LspFormat =
struct
  open LspProtocol
  open LspText
  open LspState

  fun documentText state uri path =
    case findDoc state uri of
      SOME document => #text document
    | NONE => getOpt (Driver.readFile path, "")

  fun formatDocument state uri =
    let
      val path = uriToPath uri
      val text = documentText state uri path
      val result = Fmt.format { source = text, width = Fmt.defaultWidth }
    in
      if not (List.null (#diagnostics result)) then arr []
      else
        let val output = #output result in
          if output = text then arr []
          else
            let
              val source = Source.create text
              fun endPosition offset =
                let
                  val (line, character) = positionOfOffset
                    { id = 0, path = path, map = source } offset
                in
                  obj [ ("line", int (line - 1)), ("character", int character) ]
                end
              val range =
                obj
                  [ ("start", obj [ ("line", int 0), ("character", int 0) ])
                  , ("end", endPosition (String.size text))
                  ]
            in
              arr
                [ obj
                    [ ("range", range)
                    , ("newText", str output)
                    ]
                ]
            end
        end
    end

  fun changedSpan (text, output) =
    let
      val size = String.size text
      val outSize = String.size output
      fun prefix index =
        if index < size andalso index < outSize andalso
           String.sub (text, index) = String.sub (output, index)
        then prefix (index + 1) else index
      val start = prefix 0
      fun suffix (count) =
        if count < size - start andalso count < outSize - start andalso
           String.sub (text, size - count - 1) =
             String.sub (output, outSize - count - 1)
        then suffix (count + 1) else count
      val suffixLength = suffix 0
      val oldStop = size - suffixLength
      val newStop = outSize - suffixLength
    in
      if oldStop <= start andalso newStop <= start then NONE
      else
        SOME
          ( start
          , oldStop
          , String.substring (output, start, newStop - start)
          )
    end

  fun formatRange state uri startLine startCharacter endLine endCharacter =
    let
      val path = uriToPath uri
      val text = documentText state uri path
      val result = Fmt.format { source = text, width = Fmt.defaultWidth }
    in
      if not (List.null (#diagnostics result)) then arr []
      else
        case changedSpan (text, #output result) of
          NONE => arr []
        | SOME (start, stop, newText) =>
            let
              val requestedStart =
                offsetOfPosition (text, startLine, startCharacter)
              val requestedStop =
                offsetOfPosition (text, endLine, endCharacter)
            in
              if stop < requestedStart orelse start > requestedStop then
                arr []
              else
                let
                  val source = Source.create text
                  val entry = { id = 0, path = path, map = source }
                in
                  arr
                    [ obj
                        [ ("range",
                            obj
                              [ ("start", lspPositionOfOffset entry start)
                              , ("end", lspPositionOfOffset entry stop)
                              ])
                        , ("newText", str newText)
                        ]
                    ]
                end
            end
    end
end
