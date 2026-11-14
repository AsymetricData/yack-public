structure LspNav =
struct
  open LspProtocol
  open LspText
  open LspState

  fun sourceEntryOf (path, text) : Driver.sourceEntry =
    { id = 0, path = path, map = Source.create text }

  fun documentText state uri path =
    case findDoc state uri of
      SOME document => #text document
    | NONE => getOpt (Driver.readFile path, "")

  fun offsetsLocation uri entry (start, stop) =
    obj
      [ ("uri", str uri)
      , ("range",
          obj
            [ ("start", lspPositionOfOffset entry start)
            , ("end", lspPositionOfOffset entry stop)
            ])
      ]

  fun rangeLocation uri entry span =
    obj [ ("uri", str uri), ("range", lspRange entry span) ]

  fun declarationSpan declaration =
    case declaration of
      Ast.DFun {nameSpan, ...} => nameSpan
    | Ast.DFunClause {nameSpan, ...} => nameSpan
    | _ => Ast.declSpan declaration

  fun localLocation checked uri entry fileId cursor =
    let
      val bindings = #bindings (#resolution checked)
      val uses = #uses (#resolution checked)
      fun inFile span = Pos.file (#start span) = fileId
      fun bindingOffsets binding = LspRename.bindingOffsets entry binding
      fun usable binding =
        inFile (#span binding) andalso #kind binding <> Resolve.Builtin
      fun atBinding binding =
        if not (usable binding) then NONE
        else
          let val offsets = bindingOffsets binding in
            if LspRename.containsOffset cursor offsets
            then SOME (offsetsLocation uri entry offsets)
            else NONE
          end
      fun atUse use =
        if not (inFile (#span use)) then NONE
        else
          case List.find (fn binding => #id binding = #binding use) bindings of
            NONE => NONE
          | SOME binding =>
              if not (usable binding) then NONE
              else
                let
                  val offsets = LspRename.useOffsets entry binding use
                in
                  if LspRename.containsOffset cursor offsets
                  then SOME (offsetsLocation uri entry (bindingOffsets binding))
                  else NONE
                end
      fun firstSome [] = NONE
        | firstSome (value :: rest) =
            (case value of SOME found => SOME found | NONE => firstSome rest)
    in
      firstSome
        (List.map atBinding bindings @
         List.map atUse uses)
    end

  fun moduleLocation state module declaration =
    let
      val entry = sourceEntryOf (#path module, #text module)
    in
      rangeLocation
        (uriOfPath state (#path module)) entry (declarationSpan declaration)
    end

  fun moduleDefinition state path text qualifier base =
    let
      val imports = LspCompletion.importsFromText text
      fun fromModule module =
        if String.isPrefix "<" (#key module) then NONE
        else
          case LspDocs.findDeclIn
                 (LspDocs.indexOf (#program module), base) of
            SOME declaration => SOME (moduleLocation state module declaration)
          | NONE => NONE
      fun fromKey key =
        case LspCompletion.moduleFor state path key of
          SOME module => fromModule module
        | NONE => NONE
      fun suffixModule qualifier =
        List.find
          (fn module =>
            LspCompletion.moduleQualifier (#key module) = qualifier)
          (indexFor state path)
    in
      case qualifier of
        SOME name =>
          (case List.find (fn (candidate, _) => candidate = name) imports of
             SOME (_, key) => fromKey key
           | NONE =>
               (case suffixModule name of
                  SOME module => fromModule module
                | NONE => NONE))
      | NONE =>
          let
            fun search [] = NONE
              | search (module :: rest) =
                  (case fromModule module of
                     SOME location => SOME location
                   | NONE => search rest)
            val imported =
              List.mapPartial
                (fn (key, _) => LspCompletion.moduleFor state path key)
                imports
            val others =
              List.filter
                (fn module =>
                  not (List.exists
                         (fn candidate => #key candidate = #key module)
                         imported))
                (indexFor state path)
          in
            search (imported @ others)
          end
    end

  fun definitionAt state uri line character =
    let
      val path = uriToPath uri
      val text = documentText state uri path
      val source = Source.create text
      val lineText = Source.lineText (source, line + 1)
      val index = characterToByteColumn (lineText, character)
      val program = #1 (parsedOf state uri text)
      val entry = sourceEntryOf (path, text)
      val cursor = LspRename.cursorOffset entry line character
      val (qualifier, base) = qualifiedWordAt (lineText, index)
      fun findSubstring (text, from, limit, needle) =
        let
          val width = String.size needle
          fun loop index =
            if index + width > limit then NONE
            else if String.substring (text, index, width) = needle then
              SOME index
            else loop (index + 1)
        in
          if width = 0 then NONE else loop from
        end
      fun firstSome [] = NONE
        | firstSome (value :: rest) =
            (case value of SOME found => SOME found | NONE => firstSome rest)
      fun targetLocation (sourceText, sourcePath, sourceUri, declaration) =
        let
          val entry = sourceEntryOf (sourcePath, sourceText)
        in
          rangeLocation sourceUri entry (declarationSpan declaration)
        end
      fun moduleStart module =
        obj
          [ ("uri", str (uriOfPath state (#path module)))
          , ("range",
              obj
                [ ("start", obj [ ("line", int 0), ("character", int 0) ])
                , ("end", obj [ ("line", int 0), ("character", int 0) ])
                ])
          ]
      fun moduleByName name =
        let
          val imports = LspCompletion.importsFromText text
          fun byKey key =
            case LspCompletion.moduleFor state path key of
              SOME module => SOME module
            | NONE => NONE
        in
          case List.find (fn (candidate, _) => candidate = name) imports of
            SOME (_, key) => byKey key
          | NONE =>
              List.find
                (fn module =>
                  LspCompletion.moduleQualifier (#key module) = name)
                (indexFor state path)
        end
      fun importsAndOthers () =
        let
          val imports = LspCompletion.importsFromText text
          val imported =
            List.mapPartial
              (fn (key, _) => LspCompletion.moduleFor state path key)
              imports
          val others =
            List.filter
              (fn module =>
                not (List.exists
                       (fn candidate => #key candidate = #key module)
                       imported))
              (indexFor state path)
        in
          imported @ others
        end
      fun declarationTargetIn (sourceText, sourcePath, sourceUri, program) base =
        case LspDocs.findDecl program base of
          SOME declaration =>
            SOME (sourceText, sourcePath, sourceUri, declaration)
        | NONE => NONE
      fun moduleTarget module base =
        if String.isPrefix "<" (#key module) then NONE
        else
          case LspDocs.findDeclIn
                 (LspDocs.indexOf (#program module), base) of
            SOME declaration =>
              SOME
                ( #text module
                , #path module
                , uriOfPath state (#path module)
                , declaration
                )
          | NONE => NONE
      fun findTarget base =
        case declarationTargetIn (text, path, uri, program) base of
          SOME target => SOME target
        | NONE => firstSome (List.map (fn module => moduleTarget module base)
                               (importsAndOthers ()))
      fun isTypeDeclaration declaration =
        case declaration of
          Ast.DType _ => true
        | Ast.DAlias _ => true
        | Ast.DDatatype _ => true
        | _ => false
      fun typeTargetIn (sourceText, sourcePath, sourceUri, program) base =
        let
          fun candidate declaration =
            case LspDocs.declName declaration of
              SOME name => name = base andalso isTypeDeclaration declaration
            | NONE => false
        in
          case List.find candidate program of
            SOME declaration =>
              SOME (sourceText, sourcePath, sourceUri, declaration)
          | NONE => NONE
        end
      fun findTypeTarget base =
        case typeTargetIn (text, path, uri, program) base of
          SOME target => SOME target
        | NONE =>
            firstSome
              (List.map
                 (fn module =>
                   typeTargetIn
                     (#text module, #path module,
                      uriOfPath state (#path module), #program module) base)
                 (importsAndOthers ()))
      fun constructorTarget base =
        let
          fun inProgram (sourceText, sourcePath, sourceUri, program) =
            let
              fun fromDatatype declaration =
                case declaration of
                  Ast.DDatatype {constructors, ...} =>
                    if List.exists (fn (name, _) => name = base) constructors
                    then
                      let
                        val span = Ast.declSpan declaration
                        val from = Pos.offset (#start span)
                        val limit = Pos.offset (#stop span)
                      in
                        SOME
                          (case findSubstring (sourceText, from, limit, base) of
                             SOME offset =>
                               offsetsLocation sourceUri
                                 (sourceEntryOf (sourcePath, sourceText))
                                 (offset, offset + String.size base)
                           | NONE =>
                               targetLocation
                                 (sourceText, sourcePath, sourceUri, declaration))
                      end
                    else NONE
                | _ => NONE
            in
              firstSome (List.map fromDatatype program)
            end
          val localConstructor = inProgram (text, path, uri, program)
        in
          case localConstructor of
            SOME location => SOME location
          | NONE =>
              firstSome
                (List.map
                   (fn module =>
                     inProgram
                       (#text module, #path module,
                        uriOfPath state (#path module), #program module))
                   (importsAndOthers ()))
        end
      fun typeNameOf ty =
        case Types.prune ty of
          Types.TCon {name, ...} => SOME name
        | Types.TData (Types.Data {name, ...}) => SOME name
        | Types.TDataApp (Types.Data {name, ...}, _) => SOME name
        | Types.TBorrow (inner, _, _) => typeNameOf inner
        | Types.TConsume inner => typeNameOf inner
        | _ => NONE
      fun lastSegment name =
        case List.rev
               (String.tokens
                  (fn character =>
                    character = #"." orelse character = #"/") name) of
          segment :: _ => segment
        | [] => name
      fun fieldTarget qualifier base =
        case checkedOf state uri of
          NONE => NONE
        | SOME info =>
            (case LspCompletion.bindingTypeOfName info qualifier of
               NONE => NONE
             | SOME ty =>
                 (case typeNameOf ty of
                    NONE => NONE
                  | SOME name =>
                      (case findTypeTarget (lastSegment name) of
                         NONE => NONE
                       | SOME (sourceText, sourcePath, sourceUri, declaration) =>
                           let
                             val span = Ast.declSpan declaration
                             val from = Pos.offset (#start span)
                             val limit = Pos.offset (#stop span)
                           in
                             case findSubstring (sourceText, from, limit, base) of
                               SOME offset =>
                                 SOME
                                   (offsetsLocation sourceUri
                                      (sourceEntryOf (sourcePath, sourceText))
                                      (offset, offset + String.size base))
                             | NONE =>
                                 SOME
                                   (targetLocation
                                      (sourceText, sourcePath, sourceUri,
                                       declaration))
                           end)))
      fun importTarget () =
        let
          fun declaration decl =
            case decl of
              Ast.DImport {path = segments, span, ...} =>
                if Pos.offset (#start span) <= cursor andalso
                   cursor <= Pos.offset (#stop span)
                then
                  let val key = String.concatWith "/" segments in
                    case LspCompletion.moduleFor state path key of
                      SOME module =>
                        if String.isPrefix "<" (#key module) then NONE
                        else SOME (moduleStart module)
                    | NONE => NONE
                  end
                else NONE
            | _ => NONE
        in
          firstSome (List.map declaration program)
        end
      fun qualifierTarget () =
        case wordBoundsAt (lineText, index) of
          NONE => NONE
        | SOME (_, stop) =>
            if stop < String.size lineText andalso
               String.sub (lineText, stop) = #"." then
              (case moduleByName base of
                 SOME module => SOME (moduleStart module)
               | NONE => NONE)
            else NONE
      val checked = checkedOf state uri
      val localTarget =
        case checked of
          NONE => NONE
        | SOME info =>
            (case entryForFile (#sources info) (fileIdOf info path) of
               NONE => NONE
             | SOME fileEntry =>
                 localLocation info uri fileEntry (fileIdOf info path)
                   (LspRename.cursorOffset fileEntry line character))
    in
      if base = "" then jsonNull
      else
        case importTarget () of
          SOME location => location
        | NONE =>
            (case qualifierTarget () of
               SOME location => location
             | NONE =>
                 (case localTarget of
                    SOME location => location
                  | NONE =>
                      (case qualifier of
                         SOME name =>
                           (case moduleByName name of
                              SOME module =>
                                (case moduleTarget module base of
                                   SOME target => targetLocation target
                                 | NONE => jsonNull)
                            | NONE =>
                                (case fieldTarget name base of
                                   SOME location => location
                                 | NONE => jsonNull))
                       | NONE =>
                           (case constructorTarget base of
                              SOME location => location
                            | NONE =>
                                (case findTarget base of
                                   SOME target => targetLocation target
                                 | NONE => jsonNull)))))
    end

  fun referencesAt state uri line character includeDeclaration =
    case checkedOf state uri of
      NONE => arr []
    | SOME checked =>
        let
          val path = uriToPath uri
          val fileId = fileIdOf checked path
        in
          case entryForFile (#sources checked) fileId of
            NONE => arr []
          | SOME entry =>
              (case LspRename.targetAt checked entry fileId line character of
                 NONE => arr []
               | SOME target =>
                   let
                     val occurrences =
                       if includeDeclaration then #occurrences target
                       else
                         List.filter
                           (fn occurrence =>
                             not (#declaration occurrence))
                           (#occurrences target)
                   in
                     arr
                       (List.map
                          (fn occurrence =>
                            let
                              val entry = #entry occurrence
                              val uri = uriOfPath state (#path entry)
                            in
                              offsetsLocation uri entry
                                (#start occurrence, #stop occurrence)
                            end)
                          occurrences)
                   end)
        end

  fun highlightsAt state uri line character =
    case checkedOf state uri of
      NONE => arr []
    | SOME checked =>
        let
          val path = uriToPath uri
          val fileId = fileIdOf checked path
        in
          case entryForFile (#sources checked) fileId of
            NONE => arr []
          | SOME entry =>
              (case LspRename.targetAt checked entry fileId line character of
                 NONE => arr []
               | SOME target =>
                   let
                     val occurrences =
                       List.filter
                         (fn occurrence =>
                           #path (#entry occurrence) = path)
                         (#occurrences target)
                   in
                     arr
                       (List.map
                          (fn occurrence =>
                            obj
                              [ ("range",
                                  obj
                                    [ ("start",
                                        lspPositionOfOffset entry
                                          (#start occurrence))
                                    , ("end",
                                        lspPositionOfOffset entry
                                          (#stop occurrence))
                                    ])
                              , ("kind", int 1)
                              ])
                          occurrences)
                   end)
        end

  fun typeNameOf ty =
    case Types.prune ty of
      Types.TCon {name, ...} => SOME name
    | Types.TData (Types.Data {name, ...}) => SOME name
    | Types.TDataApp (Types.Data {name, ...}, _) => SOME name
    | Types.TBorrow (inner, _, _) => typeNameOf inner
    | Types.TConsume inner => typeNameOf inner
    | _ => NONE

  fun lastSegment name =
    case List.rev (String.tokens (fn character => character = #"." orelse
                                               character = #"/") name) of
      segment :: _ => segment
    | [] => name

  fun findDeclarationLocation state uri path text program base =
    let
      val entry = sourceEntryOf (path, text)
      fun isTypeDeclaration declaration =
        case declaration of
          Ast.DType _ => true
        | Ast.DAlias _ => true
        | Ast.DDatatype _ => true
        | _ => false
      fun named base declaration =
        case LspDocs.declName declaration of
          SOME name => name = base
        | NONE => false
      fun findTypeDecl declarations =
        List.find
          (fn declaration =>
            named base declaration andalso isTypeDeclaration declaration)
          declarations
      val localTarget =
        case findTypeDecl program of
          SOME declaration =>
            SOME (rangeLocation uri entry (declarationSpan declaration))
        | NONE =>
            (case LspDocs.findDecl program base of
               SOME declaration =>
                 SOME (rangeLocation uri entry (declarationSpan declaration))
             | NONE => NONE)
      fun fromModule module =
        if String.isPrefix "<" (#key module) then NONE
        else
          case findTypeDecl (#program module) of
            SOME declaration =>
              SOME (moduleLocation state module declaration)
          | NONE =>
              (case LspDocs.findDeclIn
                     (LspDocs.indexOf (#program module), base) of
                SOME declaration =>
                  SOME (moduleLocation state module declaration)
              | NONE => NONE)
      val imports = LspCompletion.importsFromText text
      val imported =
        List.mapPartial
          (fn (key, _) => LspCompletion.moduleFor state path key)
          imports
      val others =
        List.filter
          (fn module =>
            not (List.exists
                   (fn candidate => #key candidate = #key module)
                   imported))
          (indexFor state path)
      fun search [] = NONE
        | search (module :: rest) =
            (case fromModule module of
               SOME location => SOME location
             | NONE => search rest)
    in
      case localTarget of
        SOME location => SOME location
      | NONE => search (imported @ others)
    end

  fun typeDefinitionAt state uri line character =
    let
      val path = uriToPath uri
      val text = documentText state uri path
      val source = Source.create text
      val lineText = Source.lineText (source, line + 1)
      val index = characterToByteColumn (lineText, character)
      val program = #1 (parsedOf state uri text)
      val (_, word) = qualifiedWordAt (lineText, index)
      val fromWord =
        if word = "" then NONE
        else
          findDeclarationLocation state uri path text program word
      val checked = checkedOf state uri
      val fromType =
        case checked of
          NONE => NONE
        | SOME info =>
            let
              val fileId = fileIdOf info path
              val entry =
                case entryForFile (#sources info) fileId of
                  SOME entry => entry
                | NONE => sourceEntryOf (path, text)
              val cursor = LspRename.cursorOffset entry line character
              fun contains span =
                Pos.file (#start span) = fileId andalso
                Pos.offset (#start span) <= cursor andalso
                cursor <= Pos.offset (#stop span)
              val candidates =
                List.filter (fn (span, _) => contains span)
                  (#bindingTypes info @ #expressionTypes info)
              fun width (span, _) =
                Pos.offset (#stop span) - Pos.offset (#start span)
              val best =
                case candidates of
                  [] => NONE
                  | first :: rest =>
                      SOME (List.foldl
                        (fn (candidate, best) =>
                          if width candidate < width best then candidate else best)
                        first rest)
            in
              case best of
                NONE => NONE
              | SOME (_, ty) =>
                  (case typeNameOf ty of
                     NONE => NONE
                   | SOME name =>
                       findDeclarationLocation state uri path text program
                         (lastSegment name))
            end
    in
      case fromType of
        SOME location => location
      | NONE =>
          (case fromWord of
             SOME location => location
           | NONE => jsonNull)
    end

  fun findSubstring (text, from, limit, needle) =
    let
      val width = String.size needle
      fun loop index =
        if index + width > limit then NONE
        else if String.substring (text, index, width) = needle then SOME index
        else loop (index + 1)
    in
      if width = 0 then NONE else loop from
    end

  fun documentLinksAt state uri =
    let
      val path = uriToPath uri
      val text = documentText state uri path
      val program = #1 (parsedOf state uri text)
      val entry = sourceEntryOf (path, text)
      fun link declaration =
        case declaration of
          Ast.DImport {path = segments, span, ...} =>
            let
              val key = String.concatWith "/" segments
              val from = Pos.offset (#start span)
              val limit = Pos.offset (#stop span)
            in
              if key = "" then NONE
              else
                case LspCompletion.moduleFor state path key of
                  SOME module =>
                    if String.isPrefix "<" (#key module) then NONE
                    else
                      (case findSubstring (text, from, limit, key) of
                         SOME offset =>
                           SOME
                             (obj
                                [ ("range",
                                    obj
                                      [ ("start", lspPositionOfOffset entry offset)
                                      , ("end",
                                          lspPositionOfOffset entry
                                            (offset + String.size key))
                                      ])
                                , ("target", str (uriOfPath state (#path module)))
                                ])
                       | NONE => NONE)
                | NONE => NONE
            end
        | _ => NONE
    in
      arr (List.mapPartial link program)
    end
end
