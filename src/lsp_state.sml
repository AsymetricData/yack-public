structure LspState =
struct
  open LspText

  type state =
    { docs : LspProject.doc list ref
    , cache : (string * LspProject.analysis) list ref
    , published : (string * string list) list ref
    , index : (string * LspProject.indexModule list) list ref

    , parsed : (string * string * Ast.program * LspDocs.comment list) list ref

    , workspaceRoot : string ref

    , lastChecked : (string * Driver.checked) list ref

    , semanticCache : (string * int * int list) list ref
    , nextSemanticId : int ref
    }

  fun uriOfPath (state : state) path =
    let
      val path = absolutePath path
      val openDoc =
        List.find (fn document => #path document = path) (!(#docs state))
    in
      case openDoc of
        SOME document => #uri document
      | NONE => "file://" ^ percentEncode path
    end

  fun findDoc (state : state) uri =
    List.find (fn document => #uri document = uri) (!(#docs state))

  fun updateDoc (state : state) (document : LspProject.doc) =
    let
      val docs =
        document :: List.filter (fn d => #uri d <> #uri document) (!(#docs state))
    in
      #docs state := docs
    end

  fun removeDoc (state : state) uri =
    #docs state := List.filter (fn d => #uri d <> uri) (!(#docs state))

  fun readOverlay (state : state) path =
    case List.find (fn document => #path document = path) (!(#docs state)) of
      SOME document => SOME (#text document)
    | NONE => Driver.readFile path

  fun analyze (state : state) path =
    let
      val result = LspProject.analyze (readOverlay state) path
      val cache =
        (path, result) :: List.filter (fn (p, _) => p <> path) (!(#cache state))
      val _ =
        case #checked result of
          SOME checked =>
            #lastChecked state :=
              (path, checked) ::
              List.filter (fn (candidate, _) => candidate <> path)
                (!(#lastChecked state))
        | NONE => ()
    in
      #cache state := cache;
      result
    end

  fun analysisFor (state : state) uri =
    let val path = uriToPath uri in
      case List.find (fn (p, _) => p = path) (!(#cache state)) of
        SOME (_, result) => result
      | NONE => analyze state path
    end

  fun checkedOf (state : state) uri =
    #checked (analysisFor state uri)

  fun cachedSemanticTokens (state : state) uri =
    List.find (fn (candidate, _, _) => candidate = uri)
      (!(#semanticCache state))

  fun storeSemanticTokens (state : state) uri resultId data =
    #semanticCache state :=
      (uri, resultId, data) ::
      List.filter (fn (candidate, _, _) => candidate <> uri)
        (!(#semanticCache state))

  fun forgetSemanticTokens (state : state) uri =
    #semanticCache state :=
      List.filter (fn (candidate, _, _) => candidate <> uri)
        (!(#semanticCache state))

  fun freshSemanticId (state : state) =
    let
      val id = !(#nextSemanticId state)
      val _ = #nextSemanticId state := id + 1
    in
      id
    end

  fun lastCheckedOf (state : state) uri =
    let val path = uriToPath uri in
      case List.find (fn (candidate, _) => candidate = path)
             (!(#lastChecked state)) of
        SOME (_, checked) => SOME checked
      | NONE => NONE
    end

  fun fileIdOf (checked : Driver.checked) path =
    case List.find (fn entry => #path entry = path) (#sources checked) of
      SOME entry => #id entry
    | NONE => 0

  fun indexFor (state : state) path =
    let

      val key = LspProject.indexKey path ^ "@" ^ LspProject.manifestStamp path
    in
      case List.find (fn (candidate, _) => candidate = key) (!(#index state)) of
        SOME (_, modules) => modules
      | NONE =>
          let val modules = LspProject.buildIndex path in
            #index state := (key, modules) :: !(#index state);
            modules
          end
    end

  fun parsedOf (state : state) uri text =
    let
      fun matches (candidateUri, candidateText, _, _) =
        candidateUri = uri andalso candidateText = text
    in
      case List.find matches (!(#parsed state)) of
        SOME (_, _, program, comments) => (program, comments)
      | NONE =>
          let
            val program = LspAst.programOfText text
            val comments = LspDocs.commentsOf text
            val _ =
              #parsed state :=
                (uri, text, program, comments) ::
                List.filter
                  (fn (candidateUri, _, _, _) => candidateUri <> uri)
                  (!(#parsed state))
          in
            (program, comments)
          end
    end
end
