structure LspSymbol =
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

  fun symbolKind declaration =
    case declaration of
      Ast.DFun _ => 12
    | Ast.DFunClause _ => 12
    | Ast.DType _ => 23
    | Ast.DAlias _ => 5
    | Ast.DDatatype _ => 10
    | Ast.DExternRecord _ => 23
    | Ast.DImport _ => 2

  fun selectionSpan declaration =
    case declaration of
      Ast.DFun {nameSpan, ...} => nameSpan
    | Ast.DFunClause {nameSpan, ...} => nameSpan
    | _ => Ast.declSpan declaration

  fun childrenOf (program, entry, declaration) =
    let
      fun child (name, detail, kind, span) =
        obj
          [ ("name", str name)
          , ("detail", str detail)
          , ("kind", int kind)
          , ("range", lspRange entry span)
          , ("selectionRange", lspRange entry span)
          ]
    in
      case declaration of
        Ast.DType {fields, span, ...} =>
          List.map
            (fn (field, ty) =>
              child (field, Ast.tyToString ty, 8, span))
            fields
      | Ast.DDatatype {name, constructors, span, ...} =>
          List.map
            (fn (constructor, argument) =>
              child
                ( constructor
                , (case argument of
                     NONE => name
                   | SOME ty => "of " ^ Ast.tyToString ty)
                , 22
                , span
                ))
            constructors
      | Ast.DExternRecord {fields, span, ...} =>
          List.map
            (fn (field, ty, offset) =>
              child
                ( field
                , Ast.tyToString ty ^ " @" ^ Int.toString offset
                , 8
                , span
                ))
            fields
      | _ => []
    end

  fun documentSymbolsAt state uri =
    let
      val path = uriToPath uri
      val text = documentText state uri path
      val entry = sourceEntryOf (path, text)
      val program = #1 (parsedOf state uri text)
      fun symbol declaration =
        let
          val name = getOpt (LspDocs.declName declaration, "")
          val range = Ast.declSpan declaration
          val detail =
            case LspDocs.signatureOfDeclIn (program, declaration) of
              SOME value => value
            | NONE => name
          val children =
            childrenOf (program, entry, declaration)
        in
          obj
            ([ ("name", str name)
             , ("detail", str detail)
             , ("kind", int (symbolKind declaration))
             , ("range", lspRange entry range)
             , ("selectionRange", lspRange entry (selectionSpan declaration))
             ] @
             (if List.null children then []
              else [("children", arr children)]))
        end
      val symbols =
        List.mapPartial
          (fn declaration =>
            case LspDocs.declName declaration of
              NONE => NONE
            | SOME "" => NONE
            | SOME _ => SOME (symbol declaration))
          program
    in
      arr symbols
    end

  fun foldingRangesAt state uri =
    let
      val path = uriToPath uri
      val text = documentText state uri path
      val entry = sourceEntryOf (path, text)
      fun lineOfOffset offset =
        let val (line, _) = positionOfOffset entry offset in
          line - 1
        end
      fun fold (startLine, endLine) =
        if endLine <= startLine then NONE
        else
          SOME
            (obj
               [ ("startLine", int startLine)
               , ("endLine", int endLine)
               , ("kind", str "region")
               ])
      val program = #1 (parsedOf state uri text)
      val declarationFolds =
        List.mapPartial
          (fn declaration =>
            let
              val span = Ast.declSpan declaration
              val startLine = Pos.line (#start span) - 1
              val stopOffset =
                Int.max (Pos.offset (#start span), Pos.offset (#stop span) - 1)
            in
              fold (startLine, lineOfOffset stopOffset)
            end)
          program
      val commentFolds =
        List.mapPartial
          (fn comment =>
            let
              val startLine = lineOfOffset (#start comment)
              val endLine =
                lineOfOffset (Int.max (#start comment, #stop comment - 1))
            in
              fold (startLine, endLine)
            end)
          (LspDocs.commentsOf text)
    in
      arr (declarationFolds @ commentFolds)
    end

  fun take (count, values) =
    if count <= 0 then []
    else
      case values of
        [] => []
      | value :: rest => value :: take (count - 1, rest)

  fun contains (text, needle) =
    let
      val size = String.size text
      val width = String.size needle
      fun loop index =
        if width = 0 then true
        else if index + width > size then false
        else if String.substring (text, index, width) = needle then true
        else loop (index + 1)
    in
      loop 0
    end

  fun lower text = String.map Char.toLower text

  fun workspaceSymbolsAt state query =
    let
      val root = !(#workspaceRoot state)
      val root = if root = "" then OS.FileSys.getDir () else root
      val diskModules = indexFor state (OS.Path.concat (root, "workspace.yack"))
      val openDocs = !(#docs state)
      fun keyOf path =
        let
          val canonical = absolutePath path
          val prefix = if String.isSuffix "/" root then root else root ^ "/"
          val relative =
            if String.isPrefix prefix canonical then
              String.extract (canonical, String.size prefix, NONE)
            else OS.Path.file canonical
        in
          if String.isSuffix ".yack" relative then
            String.substring (relative, 0, String.size relative - 5)
          else relative
        end
      val openModules =
        List.map
          (fn document =>
            let
              val program = #1 (parsedOf state (#uri document) (#text document))
            in
              { key = keyOf (#path document)
              , path = #path document
              , exports = LspAst.exportsOf program
              , program = program
              , text = #text document
              }
            end)
          openDocs
      val openPaths = List.map (fn document => absolutePath (#path document)) openDocs
      val diskModules =
        List.filter
          (fn module =>
            not (List.exists
                   (fn path => path = absolutePath (#path module))
                   openPaths))
          diskModules
      val modules = openModules @ diskModules
      val needle = lower query
      fun matches name = contains (lower name, needle)
      fun entryOf module = sourceEntryOf (#path module, #text module)
      fun locationOf module declaration =
        obj
          [ ("uri", str (uriOfPath state (#path module)))
          , ("range", lspRange (entryOf module) (Ast.declSpan declaration))
          ]
      fun symbol (module, name, kind, location) =
        obj
          [ ("name", str name)
          , ("kind", int kind)
          , ("location", location)
          , ("containerName", str (#key module))
          ]
      fun declarationSymbols module declaration =
        case LspDocs.declName declaration of
          NONE => []
        | SOME "" => []
        | SOME name =>
            if not (matches name) then []
            else
              let
                val location = locationOf module declaration
              in
                symbol (module, name, symbolKind declaration, location)
                ::
                (case declaration of
                   Ast.DDatatype {constructors, ...} =>
                     List.mapPartial
                       (fn (constructor, _) =>
                         if matches constructor then
                           SOME (symbol (module, constructor, 22, location))
                         else NONE)
                       constructors
                 | _ => [])
              end
      val symbols =
        List.concat
          (List.map
             (fn module =>
               if String.isPrefix "<" (#key module) then []
               else
                 List.concat
                   (List.map
                      (declarationSymbols module)
                      (#program module)))
             modules)
    in
      arr (take (300, symbols))
    end
end
