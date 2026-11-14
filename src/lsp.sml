structure Lsp =
struct
  val serverVersion = "0.2.0"

  open LspProtocol
  open LspText
  open LspState
  open LspDiag
  open LspCompletion
  open LspHints
  open LspActions
  open LspRename
  open LspTokens
  open LspCalls
  open LspSignature
  open LspNav
  open LspSymbol
  open LspFormat

  fun capabilities () =
    obj
      [ ("textDocumentSync",
          obj
            [ ("openClose", bool true)
            , ("change", int 2)
            , ("save", obj [ ("includeText", bool false) ])
            ])
      , ("completionProvider",
          obj
            [ ("triggerCharacters", arr [str ".", str "/"])
            , ("resolveProvider", bool true)
            ])
      , ("signatureHelpProvider",
          obj
            [ ("triggerCharacters", arr [str "(", str ","])
            , ("retriggerCharacters", arr [str ","])
            ])
      , ("inlayHintProvider", bool true)
      , ("semanticTokensProvider",
          obj
            [ ("legend",
                obj
                  [ ("tokenTypes", arr (List.map str LspTokens.tokenTypes))
                  , ("tokenModifiers", arr [])
                  ])
            , ("full", obj [ ("delta", bool true) ])
            , ("range", bool true)
            ])
      , ("renameProvider", obj [ ("prepareProvider", bool true) ])
      , ("documentSymbolProvider", bool true)
      , ("foldingRangeProvider", bool true)
      , ("documentFormattingProvider", bool true)
      , ("documentRangeFormattingProvider", bool true)
      , ("definitionProvider", bool true)
      , ("declarationProvider", bool true)
      , ("typeDefinitionProvider", bool true)
      , ("referencesProvider", bool true)
      , ("documentHighlightProvider", bool true)
      , ("documentLinkProvider", bool true)
      , ("workspaceSymbolProvider", bool true)
      , ("callHierarchyProvider", bool true)
      , ("codeActionProvider",
          obj [ ("codeActionKinds", arr [str "quickfix", str "refactor.rewrite"]) ])
      , ("hoverProvider", bool true)
      ]

  fun workspaceRootOf params =
    let
      val fallback = OS.FileSys.getDir ()
      fun pathOfUri uri =
        let val path = uriToPath uri in
          if path = uri then fallback else path
        end
    in
      case Json.stringField "rootUri" params of
        SOME uri => if uri = "" then fallback else pathOfUri uri
      | NONE =>
          (case Json.stringField "rootPath" params of
             SOME path => if path = "" then fallback else path
           | NONE =>
               (case Json.arrayField "workspaceFolders" params of
                  SOME (folder :: _) =>
                    (case Json.stringField "uri" folder of
                       SOME uri => pathOfUri uri
                     | NONE => fallback)
                | _ => fallback))
    end

  fun handleMessage state message =
    let
      val method =
        case Json.stringField "method" message of
          SOME name => name
        | NONE => ""
      val id = Json.field "id" message
      val params =
        case Json.field "params" message of
          SOME value => value
        | NONE => obj []
      fun documentUri () =
        let val document = required "textDocument" params in
          stringOf (required "uri" document)
        end
      fun updateFromOpen () =
        let
          val document = required "textDocument" params
          val uri = stringOf (required "uri" document)
          val text = stringOf (required "text" document)
          val version =
            case Json.intField "version" document of
              SOME value => value
            | NONE => 0
          val path = uriToPath uri
        in
          updateDoc state { uri = uri, path = path, text = text, version = version };
          path
        end
      fun updateFromChange () =
        let
          val document = required "textDocument" params
          val uri = stringOf (required "uri" document)
          val changes =
            case Json.arrayField "contentChanges" params of
              SOME values => values
            | NONE => []
          val existing = findDoc state uri
          val base =
            case existing of
              SOME doc => #text doc
            | NONE => getOpt (Driver.readFile (uriToPath uri), "")
          val text = applyChanges (base, changes)
          val version =
            case (existing, Json.intField "version" document) of
              (SOME doc, _) => #version doc
            | (NONE, SOME value) => value
            | _ => 0
          val path = uriToPath uri
        in
          updateDoc state { uri = uri, path = path, text = text, version = version };
          path
        end
      fun reanalyze path =
        let val analysis = analyze state path in
          publishAnalysis state path analysis
        end
    in
      (case method of
        "initialize" =>
          (case id of
             SOME id =>
               let val _ = (#workspaceRoot state) := workspaceRootOf params in
                 respond id
                   (obj
                      [ ("capabilities", capabilities ())
                      , ("serverInfo",
                          obj [ ("name", str "yack"), ("version", str serverVersion) ])
                      ])
               end
           | NONE => ())
      | "initialized" => ()
      | "shutdown" => (case id of SOME id => respond id jsonNull | NONE => ())
      | "exit" => ()
      | "textDocument/didOpen" =>
          let val path = updateFromOpen () in reanalyze path end
      | "textDocument/didChange" =>
          let val path = updateFromChange () in reanalyze path end
      | "textDocument/didSave" =>
          let val path = uriToPath (documentUri ()) in
            #index state := [];
            reanalyze path
          end
      | "textDocument/didClose" =>
          let
            val uri = documentUri ()
            val _ = removeDoc state uri
            val _ = forgetSemanticTokens state uri
            val _ = publish state uri []
          in
            ()
          end
      | "textDocument/inlayHint" =>
          (case id of
             SOME id =>
               let
                 val uri = documentUri ()
                 val range = Json.field "range" params
                 val (startLine, startCharacter, endLine, endCharacter) =
                   case range of
                     SOME range =>
                       let
                         val start = required "start" range
                         val stop = required "end" range
                       in
                         (intOf (required "line" start),
                          intOf (required "character" start),
                          intOf (required "line" stop),
                          intOf (required "character" stop))
                       end
                   | NONE => (0, 0, 1000000, 0)
                 val hints =
                   hintsFor state uri startLine startCharacter endLine endCharacter
               in
                 respond id hints
               end
           | NONE => ())
      | "textDocument/signatureHelp" =>
          (case id of
             SOME id =>
               let
                 val uri = documentUri ()
                 val position = required "position" params
                 val line = intOf (required "line" position)
                 val character = intOf (required "character" position)
               in
                 respond id (signatureHelpAt state uri line character)
               end
           | NONE => ())
      | "textDocument/semanticTokens/full" =>
          (case id of
             SOME id => respond id (semanticTokensAt state (documentUri ()))
           | NONE => ())
      | "textDocument/semanticTokens/full/delta" =>
          (case id of
             SOME id =>
               let
                 val uri = documentUri ()
                 val previous =
                   getOpt (Json.stringField "previousResultId" params, "")
               in
                 respond id (semanticDeltaAt state uri previous)
               end
           | NONE => ())
      | "textDocument/semanticTokens/range" =>
          (case id of
             SOME id =>
               let
                 val uri = documentUri ()
                 val range = required "range" params
                 val start = required "start" range
                 val stop = required "end" range
                 val startLine = intOf (required "line" start)
                 val endLine = intOf (required "line" stop)
               in
                 respond id
                   (semanticTokensRangeAt state uri startLine endLine)
               end
           | NONE => ())
      | "textDocument/documentSymbol" =>          (case id of
             SOME id => respond id (documentSymbolsAt state (documentUri ()))
           | NONE => ())
      | "textDocument/foldingRange" =>
          (case id of
             SOME id => respond id (foldingRangesAt state (documentUri ()))
           | NONE => ())
      | "textDocument/formatting" =>
          (case id of
             SOME id => respond id (formatDocument state (documentUri ()))
           | NONE => ())
      | "textDocument/rangeFormatting" =>
          (case id of
             SOME id =>
               let
                 val uri = documentUri ()
                 val range = required "range" params
                 val start = required "start" range
                 val stop = required "end" range
               in
                 respond id
                   (formatRange state uri
                      (intOf (required "line" start))
                      (intOf (required "character" start))
                      (intOf (required "line" stop))
                      (intOf (required "character" stop)))
               end
           | NONE => ())
      | "textDocument/definition" =>
          (case id of
             SOME id =>
               let
                 val uri = documentUri ()
                 val position = required "position" params
                 val line = intOf (required "line" position)
                 val character = intOf (required "character" position)
               in
                 respond id (definitionAt state uri line character)
               end
           | NONE => ())
      | "textDocument/declaration" =>
          (case id of
             SOME id =>
               let
                 val uri = documentUri ()
                 val position = required "position" params
                 val line = intOf (required "line" position)
                 val character = intOf (required "character" position)
               in
                 respond id (definitionAt state uri line character)
               end
           | NONE => ())
      | "textDocument/typeDefinition" =>
          (case id of
             SOME id =>
               let
                 val uri = documentUri ()
                 val position = required "position" params
                 val line = intOf (required "line" position)
                 val character = intOf (required "character" position)
               in
                 respond id (typeDefinitionAt state uri line character)
               end
           | NONE => ())
      | "textDocument/documentLink" =>
          (case id of
             SOME id => respond id (documentLinksAt state (documentUri ()))
           | NONE => ())
      | "textDocument/references" =>
          (case id of
             SOME id =>
               let
                 val uri = documentUri ()
                 val position = required "position" params
                 val line = intOf (required "line" position)
                 val character = intOf (required "character" position)
                 val includeDeclaration =
                   case Json.field "context" params of
                     SOME context => getOpt (Json.boolField "includeDeclaration" context, true)
                   | NONE => true
               in
                 respond id
                   (referencesAt state uri line character includeDeclaration)
               end
           | NONE => ())
      | "textDocument/documentHighlight" =>
          (case id of
             SOME id =>
               let
                 val uri = documentUri ()
                 val position = required "position" params
                 val line = intOf (required "line" position)
                 val character = intOf (required "character" position)
               in
                 respond id (highlightsAt state uri line character)
               end
           | NONE => ())
      | "workspace/symbol" =>
          (case id of
             SOME id =>
               respond id
                 (workspaceSymbolsAt state
                    (getOpt (Json.stringField "query" params, "")))
           | NONE => ())
      | "textDocument/prepareCallHierarchy" =>
          (case id of
             SOME id =>
               let
                 val uri = documentUri ()
                 val position = required "position" params
                 val line = intOf (required "line" position)
                 val character = intOf (required "character" position)
               in
                 respond id (prepareCallHierarchy state uri line character)
               end
           | NONE => ())
      | "callHierarchy/incomingCalls" =>
          (case id of
             SOME id => respond id (incomingCalls state params)
           | NONE => ())
      | "callHierarchy/outgoingCalls" =>
          (case id of
             SOME id => respond id (outgoingCalls state params)
           | NONE => ())
      | "completionItem/resolve" =>
          (case id of
             SOME id => respond id (resolveCompletionItem state params)
           | NONE => ())
      | "textDocument/completion" =>
          (case id of
             SOME id =>
               let
                 val uri = documentUri ()
                 val position = required "position" params
                 val line = intOf (required "line" position)
                 val character = intOf (required "character" position)
                 val items = completionAt state uri line character
               in
                 respond id
                   (obj [ ("isIncomplete", bool false), ("items", items) ])
               end
           | NONE => ())
      | "textDocument/codeAction" =>
          (case id of
             SOME id =>
               let
                 val uri = documentUri ()
                 val context = Json.field "context" params
                 val diagnostics =
                   case context of
                     SOME value =>
                       (case Json.arrayField "diagnostics" value of
                          SOME values => values
                        | NONE => [])
                   | NONE => []
                 val actions = codeActionsAt state uri
                   (case Json.field "range" params of SOME value => value | NONE => jsonNull)
                   diagnostics
               in
                 respond id (arr actions)
               end
           | NONE => ())
      | "textDocument/prepareRename" =>
          (case id of
             SOME id =>
               let
                 val uri = documentUri ()
                 val position = required "position" params
                 val line = intOf (required "line" position)
                 val character = intOf (required "character" position)
                 val answer = prepareRenameAt state uri line character
               in
                 respond id answer
               end
           | NONE => ())
      | "textDocument/rename" =>
          (case id of
             SOME id =>
               let
                 val uri = documentUri ()
                 val position = required "position" params
                 val line = intOf (required "line" position)
                 val character = intOf (required "character" position)
                 val newName = stringOf (required "newName" params)
                 val answer = renameAt state uri line character newName
               in
                 respond id answer
               end
           | NONE => ())
      | "textDocument/hover" =>
          (case id of
             SOME id =>
               let
                 val uri = documentUri ()
                 val position = required "position" params
                 val line = intOf (required "line" position)
                 val character = intOf (required "character" position)
                 val answer = hoverAt state uri line character
               in
                 respond id answer
               end
           | NONE => ())
      | _ =>
          (case id of
             SOME id => respondError id (~32601)
               ("method not found: " ^ method)
           | NONE => ()));
      method <> "exit"
    end

  fun main () =
    let
      val state : LspState.state =
        { docs = ref []
        , cache = ref []
        , published = ref []
        , index = ref []
        , parsed = ref []
        , workspaceRoot = ref ""
        , lastChecked = ref []
        , semanticCache = ref []
        , nextSemanticId = ref 1
        }
      fun logError message =
        ( TextIO.output (TextIO.stdErr, "yack lsp: " ^ message ^ "\n")
        ; TextIO.flushOut TextIO.stdErr
        )

      fun failRequest message error =
        case Json.field "id" message of
          SOME id => respondError id (~32603) ("yack lsp: " ^ exnMessage error)
        | NONE => ()
      fun loop () =
        let
          val keepGoing =
            case readMessage TextIO.stdIn of
              NONE => false
            | SOME text =>
                case Json.tryParse text of
                  NONE => true
                | SOME message =>
                    handleMessage state message
                    handle error =>
                      ( logError (exnMessage error)
                      ; failRequest message error
                      ; true
                      )
        in
          if keepGoing then loop () else ()
        end
    in
      loop ();
      0
    end
end
