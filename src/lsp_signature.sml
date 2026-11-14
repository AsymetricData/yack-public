structure LspSignature =
struct
  open LspProtocol
  open LspText
  open LspState

  type candidate =
    { program : Ast.program
    , text : string
    , declaration : Ast.decl
    }

  fun candidateOf (program, text, declaration) =
    { program = program, text = text, declaration = declaration }

  fun findInProgram (program, text, name) =
    case LspDocs.findDeclIn (LspDocs.indexOf program, name) of
      SOME declaration => SOME (candidateOf (program, text, declaration))
    | NONE => NONE

  fun qualifierOf name =
    case List.rev (String.tokens (fn character => character = #".") name) of
      _ :: qualifier :: _ => SOME qualifier
    | _ => NONE

  fun findCandidate state path text program name =
    let
      val base = LspCompletion.baseName name
      val imports = LspCompletion.importsFromText text
      fun fromModule module =
        case LspDocs.findDeclIn
               (LspDocs.indexOf (#program module), base) of
          SOME declaration =>
            SOME (candidateOf (#program module, #text module, declaration))
        | NONE => NONE
      fun fromKey key =
        case LspCompletion.moduleFor state path key of
          SOME module => fromModule module
        | NONE => NONE
      fun searchModules [] = NONE
        | searchModules (module :: rest) =
            (case fromModule module of
               SOME candidate => SOME candidate
             | NONE => searchModules rest)
    in
      case qualifierOf name of
        SOME qualifier =>
          (case List.find (fn (candidate, _) => candidate = qualifier) imports of
             SOME (_, key) => fromKey key
           | NONE =>
               (case List.find
                       (fn module =>
                         LspCompletion.moduleQualifier (#key module) = qualifier)
                       (indexFor state path) of
                  SOME module => fromModule module
                | NONE => NONE))
      | NONE =>
          (case findInProgram (program, text, base) of
             SOME candidate => SOME candidate
           | NONE =>
               searchModules
                 (List.mapPartial
                    (fn (key, _) => LspCompletion.moduleFor state path key)
                    imports))
    end

  fun documentation (text, program, name) =
    case LspDocs.docByNameWith
           (text, LspDocs.commentsOf text, program, name) of
      NONE => []
    | SOME value =>
        [ ("documentation",
            obj
              [ ("kind", str "markdown")
              , ("value", str value)
              ])
        ]

  fun signatureHelpAt state uri line character =
    let
      val path = uriToPath uri
      val text =
        case findDoc state uri of
          SOME document => #text document
        | NONE => getOpt (Driver.readFile path, "")
      val source = Source.create text
      val lineText = Source.lineText (source, line + 1)
      val byteColumn = characterToByteColumn (lineText, character)
      val beforeCursor = String.substring (lineText, 0, byteColumn)
      val program = #1 (parsedOf state uri text)
    in
      case LspCompletion.lastOpenParen beforeCursor of
        NONE => jsonNull
      | SOME openIndex =>
          let val name = LspCompletion.callNameBefore beforeCursor openIndex in
            if name = "" then jsonNull
            else
              let
                val fromAst =
                  case findCandidate state path text program name of
                    NONE => NONE
                  | SOME candidate =>
                      let
                        val base = LspCompletion.baseName name
                        val index = LspDocs.indexOf (#program candidate)
                      in
                        case LspDocs.signatureIn (index, base) of
                          NONE => NONE
                        | SOME label =>
                            SOME
                              ( label
                              , LspDocs.parameterLabels (#declaration candidate)
                              , documentation
                                  (#text candidate, #program candidate, base)
                              )
                      end

                val data =
                  case fromAst of
                    SOME value => SOME value
                  | NONE =>
                      (case (case checkedOf state uri of
                               SOME checked => SOME checked
                             | NONE => lastCheckedOf state uri) of
                         NONE => NONE
                       | SOME checked =>
                           (case LspCompletion.signatureIndex (SOME checked) name of
                              NONE => NONE
                            | SOME (_, sigInfo) =>
                                SOME
                                  ( LspCompletion.checkedSignatureText sigInfo
                                  , LspCompletion.checkedParameterLabels sigInfo
                                  , []
                                  )))
              in
                case data of
                  NONE => jsonNull
                | SOME (label, parameters, docs) =>
                    let
                      val active =
                        LspCompletion.argumentIndexAt beforeCursor openIndex
                      val active =
                        if List.null parameters then 0
                        else Int.min (active, List.length parameters - 1)
                      val signatureItem =
                        obj
                          ([ ("label", str label)
                           , ("parameters",
                               arr
                                 (List.map
                                    (fn parameter =>
                                      obj [ ("label", str parameter) ])
                                    parameters))
                           ] @ docs)
                    in
                      obj
                        [ ("signatures", arr [signatureItem])
                        , ("activeSignature", int 0)
                        , ("activeParameter", int active)
                        ]
                    end
              end
          end
    end
end
