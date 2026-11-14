structure LspDiag =
struct
  open LspProtocol
  open LspText
  open LspState

  fun severityOf Diag.Error = 1
    | severityOf Diag.Warning = 2

  fun diagnosticKey (diagnostic : Diag.t) =
    ( #code diagnostic
    , #message diagnostic
    , Pos.file (#start (#span diagnostic))
    , Pos.offset (#start (#span diagnostic))
    , Pos.offset (#stop (#span diagnostic))
    )

  fun diagnosticText (diagnostic : Diag.t) =
    let
      val extras =
        List.map (fn note => "\n• " ^ note) (#notes diagnostic) @
        List.map (fn hint => "\n→ " ^ hint) (#hints diagnostic)
    in
      #message diagnostic ^ String.concat extras
    end

  fun diagnosticJson entry diagnostic =
    obj
      [ ("range", lspRange entry (#span diagnostic))
      , ("severity", int (severityOf (#severity diagnostic)))
      , ("code", str (#code diagnostic))
      , ("source", str "yack")
      , ("message", str (diagnosticText diagnostic))
      ]

  fun publish (state : state) uri (diagnostics : Json.t list) =
    notify
      "textDocument/publishDiagnostics"
      (obj [ ("uri", str uri), ("diagnostics", arr diagnostics) ])

  fun groupsOf (state : state) (analysis : LspProject.analysis) =
    let
      val seen = ref []
      fun add (uri, diagnostic) =
        let val key = (uri, diagnosticKey diagnostic) in
          if List.exists (fn candidate => candidate = key) (!seen) then ()
          else seen := key :: !seen
        end
      val groups = ref []
      fun addGroup (uri, diagnostics) =
        case List.find (fn (candidate, _) => candidate = uri) (!groups) of
          SOME (_, existing) =>
            groups :=
              (uri, existing @ diagnostics) ::
              List.filter (fn (candidate, _) => candidate <> uri) (!groups)
        | NONE => groups := (uri, diagnostics) :: !groups
      val _ =
        List.app
          (fn {sources, diagnostics} =>
            List.app
              (fn diagnostic =>
                case entryForFile sources (Pos.file (#start (#span diagnostic))) of
                  SOME entry =>
                    if String.isPrefix "<" (#path entry) then ()
                    else
                      let
                        val uri = uriOfPath state (#path entry)
                        val key = (uri, diagnosticKey diagnostic)
                      in
                        if List.exists (fn candidate => candidate = key) (!seen) then ()
                        else
                          let
                            val _ = seen := key :: !seen
                            val json = diagnosticJson entry diagnostic
                          in
                            addGroup (uri, [json])
                          end
                      end
                | NONE => ())
              diagnostics)
          (#reports analysis)
    in
      !groups
    end

  fun publishAnalysis (state : state) path (analysis : LspProject.analysis) =
    let
      val groups = groupsOf state analysis
      val currentUris = List.map (fn (uri, _) => uri) groups
      val analyzedUri = uriOfPath state path
      val previous =
        case List.find (fn (p, _) => p = path) (!(#published state)) of
          SOME (_, uris) => uris
        | NONE => []
      val stale = List.filter (fn uri => not (List.exists (fn u => u = uri) currentUris)) previous
      val _ = List.app (fn uri => publish state uri []) stale
      val _ =
        if List.exists (fn uri => uri = analyzedUri) currentUris then ()
        else publish state analyzedUri []
      val _ = List.app (fn (uri, diagnostics) => publish state uri diagnostics) groups
      val published =
        (path, analyzedUri :: currentUris) ::
        List.filter (fn (p, _) => p <> path) (!(#published state))
    in
      #published state := published
    end
end
