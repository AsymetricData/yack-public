structure LspCompletion =
struct
  open LspProtocol
  open LspText
  open LspAst
  open LspState

  fun completionKind kind =
    case kind of
      "function" => 3
    | "type" => 22
    | "constructor" => 4
    | "module" => 9
    | "keyword" => 14
    | "snippet" => 15
    | _ => 6

  fun completionItem label kind detail =
    obj
      [ ("label", str label)
      , ("kind", int (completionKind kind))
      , ("detail", str detail)
      , ("insertText", str label)
      ]

  fun completionItemText label kind detail insert =
    obj
      [ ("label", str label)
      , ("kind", int (completionKind kind))
      , ("detail", str detail)
      , ("insertText", str insert)
      ]

  fun replaceField key value fields =
    let
      fun loop [] = [(key, value)]
        | loop ((name, current) :: rest) =
            if name = key then (name, value) :: rest
            else (name, current) :: loop rest
    in
      loop fields
    end

  fun withSnippet item text =
    case item of
      Json.Object fields =>
        Json.Object
          (replaceField "insertTextFormat" (int 2)
             (replaceField "insertText" (str text) fields))
    | _ => item

  fun addField name value item =
    case item of
      Json.Object fields => Json.Object (fields @ [(name, value)])
    | other => other

  fun documentationMarkdown (signatureText, doc) =
    let
      val parts =
        (case signatureText of
           SOME text => ["```yack\n" ^ text ^ "\n```"]
         | NONE => []) @
        (case doc of
           SOME text => [text]
         | NONE => [])
    in
      if List.null parts then NONE
      else
        SOME
          (obj
             [ ("kind", str "markdown")
             , ("value", str (String.concatWith "\n\n" parts))
             ])
    end

  fun documentedItemAs (text, comments, index) kind label lookupName fallbackDetail =
    let
      val program = #program index
      val signatureText =
        case kind of
          "constructor" => LspDocs.constructorSignature program lookupName
        | _ => LspDocs.signatureIn (index, lookupName)
      val doc =
        case kind of
          "constructor" =>
            LspDocs.constructorDocWith (text, comments, program, lookupName)
        | _ => LspDocs.docInWith (text, comments, index, lookupName)
      val detail =
        case signatureText of
          SOME value => value
        | NONE => fallbackDetail
      val item = completionItem label kind detail
    in
      case documentationMarkdown (signatureText, doc) of
        NONE => item
      | SOME value => addField "documentation" value item
    end

  fun documentedItem (text, comments, index) kind name fallbackDetail =
    documentedItemAs (text, comments, index) kind name name fallbackDetail

  fun letSnippetItem () =
    withSnippet
      (completionItem "let" "snippet" "let … in … end")
      "let val ${1:name} = ${2:value}\n    in ${3:}\n    end"

  fun keywordItems () =
    letSnippetItem () ::
    List.map
      (fn keyword => completionItem keyword "keyword" "keyword")
      [ "fun", "val", "in", "end", "case", "of", "if", "then", "else"
      , "import", "as", "type", "datatype", "Copy", "Affine", "Linear"
      , "borrow", "mut", "from", "true", "false"
      ]

  fun importCompletion state path prefix =
    let
      val modules = indexFor state path
      val keys = List.map (fn module => #key module) modules
      val keys = List.filter (fn key => not (String.isPrefix "<" key)) keys
      val matching =
        List.filter
          (fn key =>
            prefix = "" orelse String.isPrefix prefix key orelse
            String.isPrefix (key ^ "/") prefix)
          keys
      val moduleItems =
        List.map (fn key => completionItem key "module" "module") matching
    in
      moduleItems
    end

  fun currentImports program =
    List.map
      (fn {path, alias, unqualified, span} =>
        let
          val key = String.concatWith "/" path
          val qualifier =
            case alias of
              SOME name => name
            | NONE =>
                (case List.rev path of
                   segment :: _ => segment
                 | [] => key)
        in
          (qualifier, key)
        end)
      (importsOf program)

  fun importsFromText text =
    let
      fun takeWord value =
        let
          val size = String.size value
          fun loop index =
            if index < size then
              let val character = String.sub (value, index) in
                if Char.isAlphaNum character orelse character = #"_" orelse
                   character = #"/" orelse character = #"-"
                then loop (index + 1)
                else index
              end
            else size
          val stop = loop 0
        in
          (String.substring (value, 0, stop),
           String.extract (value, stop, NONE))
        end
      fun parseLine line =
        let
          val line = trim line
        in
          if String.isPrefix "import " line then
            let
              val rest = trim (String.extract (line, 7, NONE))
              val (pathText, afterPath) = takeWord rest
              val afterPath = trim afterPath
              val alias =
                if String.isPrefix "as " afterPath then
                  let
                    val (aliasText, _) =
                      takeWord (trim (String.extract (afterPath, 3, NONE)))
                  in
                    if aliasText = "" then NONE else SOME aliasText
                  end
                else NONE
              val qualifier =
                case alias of
                  SOME name => name
                | NONE =>
                    (case List.rev (String.fields (fn c => c = #"/") pathText) of
                       segment :: _ => segment
                     | [] => pathText)
            in
              if pathText = "" then NONE else SOME (qualifier, pathText)
            end
          else NONE
        end
    in
      List.mapPartial parseLine
        (String.fields (fn c => c = #"\n") text)
    end

  fun moduleFor state path key =
    case List.find (fn module => #key module = key) (indexFor state path) of
      SOME module => SOME module
    | NONE => NONE

  fun qualifiedCompletions state path imports qualifier prefix =
    let
      val key =
        case List.find (fn (candidate, _) => candidate = qualifier) imports of
          SOME (_, key) => SOME key
        | NONE => NONE
    in
      case key of
        NONE => []
      | SOME key =>
          (case moduleFor state path key of
             NONE => []
           | SOME module =>
               let
                 val comments = LspDocs.commentsOf (#text module)
                 val index = LspDocs.indexOf (#program module)
               in
                 List.mapPartial
                   (fn (kind, name) =>
                     if prefix = "" orelse String.isPrefix prefix name then
                       SOME
                         (documentedItem
                            (#text module, comments, index)
                            kind name ("from " ^ key))
                     else NONE)
                   (#exports module)
               end)
    end

  fun moduleQualifier key =
    case List.rev (String.fields (fn c => c = #"/") key) of
      segment :: _ => segment
    | [] => key

  fun hasImportedKey imports key =
    List.exists (fn (_, candidate) => candidate = key) imports

  fun importTextEdit text key =
    let
      val lines = String.fields (fn c => c = #"\n") text
      fun scan (index, last, []) = last
        | scan (index, last, line :: rest) =
            scan
              ( index + 1
              , if String.isPrefix "import " (trim line) then SOME (index + 1)
                else last
              , rest
              )
      val insertAt =
        case scan (0, NONE, lines) of
          SOME line => line
        | NONE => 0
    in
      obj
        [ ("range",
            obj
              [ ("start", obj [ ("line", int insertAt), ("character", int 0) ])
              , ("end", obj [ ("line", int insertAt), ("character", int 0) ])
              ])
        , ("newText", str ("import " ^ key ^ "\n"))
        ]
    end

  fun autoImportedMembers state path text imports qualifier prefix =
    let
      val candidates =
        List.filter
          (fn module =>
            not (String.isPrefix "<" (#key module)) andalso
            moduleQualifier (#key module) = qualifier andalso
            not (hasImportedKey imports (#key module)))
          (indexFor state path)
    in
      List.concat
        (List.map
           (fn module =>
             let
               val edit = importTextEdit text (#key module)
               val comments = LspDocs.commentsOf (#text module)
               val index = LspDocs.indexOf (#program module)
               fun item (kind, name) =
                 if prefix = "" orelse String.isPrefix prefix name then
                   SOME
                     (addField "additionalTextEdits" (arr [edit])
                        (documentedItem
                           (#text module, comments, index)
                           kind name ("from " ^ #key module ^ " (auto import)")))
                 else NONE
             in
               List.mapPartial item (#exports module)
             end)
           candidates)
    end

  fun autoImportModuleItems state path text imports word =
    if String.size word = 0 then []
    else
      List.map
        (fn module =>
          let val qualifier = moduleQualifier (#key module) in
            addField "additionalTextEdits" (arr [importTextEdit text (#key module)])
              (completionItem qualifier "module"
                 ("module " ^ #key module ^ " (auto import)"))
          end)
        (List.filter
           (fn module =>
             not (String.isPrefix "<" (#key module)) andalso
             not (hasImportedKey imports (#key module)) andalso
             let val qualifier = moduleQualifier (#key module) in
               String.isPrefix word qualifier
             end)
           (indexFor state path))

  fun wordPrefix text =
    let
      val size = String.size text
      fun loop index =
        if index > 0 then
          let
            val character = String.sub (text, index - 1)
          in
            if Char.isAlphaNum character orelse character = #"_" then loop (index - 1)
            else index
          end
        else 0
      val start = loop size
    in
      String.substring (text, start, size - start)
    end

  fun contains text needle =
    let
      val size = String.size text
      val width = String.size needle
      fun loop index =
        if index + width > size then false
        else if String.substring (text, index, width) = needle then true
        else loop (index + 1)
    in
      width = 0 orelse loop 0
    end

  val rankCallback = 100
  val rankLocalVariable = 1000
  val rankLocalFunction = 2000
  val rankFileFunction = 3000
  val rankFileConstructor = 3100
  val rankFileType = 3200
  val rankImported = 4000
  val rankModule = 5000
  val rankAutoImportModule = 5500
  val rankMember = 1000
  val rankBuiltin = 6000
  val rankPrelude = 6500
  val rankKeyword = 9000

  fun labelOf item =
    case member "label" item of
      SOME (Json.String label) => label
    | _ => ""

  fun matchRank prefix label =
    if prefix = "" then 1
    else if label = prefix then 0
    else if String.isPrefix prefix label then 1
    else if contains label prefix then 2
    else 3

  fun depthScore depth =
    Int.max (0, 40 - Int.min (40, Int.max (0, depth))) * 10

  fun withSortText text item =
    case item of
      Json.Object fields =>
        Json.Object (replaceField "sortText" (str text) fields)
    | other => other

  fun rankItem prefix base depth item =
    let
      val score = base + depthScore depth + matchRank prefix (labelOf item)
    in
      withSortText (StringCvt.padLeft #"0" 5 (Int.toString score)) item
    end

  fun itemSortText item =
    case item of
      Json.Object _ =>
        (case member "sortText" item of
           SOME (Json.String text) => text
         | _ => "")
    | _ => ""

  fun sortBySortText items =
    let
      val indexed =
        ListPair.zip
          (List.tabulate (List.length items, fn index => index), items)
      fun precedes ((indexA, itemA), (indexB, itemB)) =
        case String.compare (itemSortText itemA, itemSortText itemB) of
          LESS => true
        | GREATER => false
        | EQUAL => indexA <= indexB
      fun merge ([], right) = right
        | merge (left, []) = left
        | merge (left as entry :: restLeft, right as other :: restRight) =
            if precedes (entry, other) then
              entry :: merge (restLeft, right)
            else other :: merge (left, restRight)
      fun split [] = ([], [])
        | split [item] = ([item], [])
        | split (first :: second :: rest) =
            let val (left, right) = split rest
            in (first :: left, second :: right) end
      fun sort [] = []
        | sort [item] = [item]
        | sort unsorted =
            let val (left, right) = split unsorted
            in merge (sort left, sort right) end
    in
      List.map (fn (_, item) => item) (sort indexed)
    end

  fun trimEnd text =
    let
      val size = String.size text
      fun loop index =
        if index > 0 andalso Char.isSpace (String.sub (text, index - 1))
        then loop (index - 1) else index
      val stop = loop size
    in
      String.substring (text, 0, stop)
    end

  fun isWordChar character =
    Char.isAlphaNum character orelse character = #"_"

  fun lastWordIndex text word =
    let
      val size = String.size text
      val width = String.size word
      fun loop index =
        if index < 0 then NONE
        else if String.substring (text, index, width) = word then
          let
            val pre =
              index = 0 orelse not (isWordChar (String.sub (text, index - 1)))
            val post =
              index + width >= size orelse
              not (isWordChar (String.sub (text, index + width)))
          in
            if pre andalso post then SOME index else loop (index - 1)
          end
        else loop (index - 1)
    in
      if width = 0 then NONE else loop (size - width)
    end

  fun lastCharIndex text character =
    let
      fun loop index =
        if index < 0 then NONE
        else if String.sub (text, index) = character then SOME index
        else loop (index - 1)
    in
      loop (String.size text - 1)
    end

  fun lastOpenBrace text =
    let
      val size = String.size text
      fun loop (index, depth, last) =
        if index >= size then last
        else case String.sub (text, index) of
          #"{" => loop (index + 1, depth + 1, SOME index)
        | #"}" => loop (index + 1, Int.max (0, depth - 1), last)
        | _ => loop (index + 1, depth, last)
    in
      loop (0, 0, NONE)
    end

  fun endsWith text suffix =
    let
      val size = String.size text
      val width = String.size suffix
    in
      width <= size andalso
      String.substring (text, size - width, width) = suffix
    end

  fun wordBeforeEnd text =
    let
      val size = String.size text
      fun skip index =
        if index > 0 andalso Char.isSpace (String.sub (text, index - 1))
        then skip (index - 1) else index
      fun collect (index, chars) =
        if index > 0 andalso isWordChar (String.sub (text, index - 1))
        then collect (index - 1, String.sub (text, index - 1) :: chars)
        else String.implode chars
      val stop = skip size
    in
      collect (stop, [])
    end

  fun fieldItems (fields : LspTypes.field list) (present, typeContext) =
    List.mapPartial
      (fn (field as {name, ty}) =>
        if List.exists (fn candidate => candidate = name) present then NONE
        else if typeContext then
          SOME (completionItemText name "field" (name ^ " : " ^ ty)
                  (name ^ " : "))
        else
          SOME (completionItemText name "field" (name ^ " : " ^ ty)
                  (name ^ " = ")))
      fields

  fun expectedRecordFields beforeBrace (db : LspTypes.db) =
    let val prefix = trimEnd beforeBrace in
      if endsWith prefix "(" then
        let
          val constructor = wordBeforeEnd (String.substring (prefix, 0, String.size prefix - 1))
        in
          case LspTypes.findConstructor db constructor of
            SOME found =>
              (case LspTypes.recordFieldsOf db found of
                 SOME fields => SOME fields
               | NONE => NONE)
          | _ => NONE
        end
      else if endsWith prefix "=" then
        let
          val beforeEquals =
            trimEnd (String.substring (prefix, 0, String.size prefix - 1))
        in
          case lastCharIndex beforeEquals #":" of
            NONE => NONE
          | SOME colon =>
              let
                val annotation =
                  trim (String.substring (beforeEquals, colon + 1,
                    String.size beforeEquals - colon - 1))
                val typeName = wordPrefix annotation
              in
                LspTypes.findRecord db typeName
              end
        end
      else NONE
    end

  fun recordCompletion beforeCursor (db : LspTypes.db) =
    case lastOpenBrace beforeCursor of
      NONE => []
    | SOME openIndex =>
        let
          val inner =
            String.substring (beforeCursor, openIndex + 1,
              String.size beforeCursor - openIndex - 1)
          val segment =
            case lastCharIndex inner #"," of
              SOME comma =>
                String.substring (inner, comma + 1,
                  String.size inner - comma - 1)
            | NONE => inner
          val beforeBrace = String.substring (beforeCursor, 0, openIndex)
          val linePrefix =
            case lastCharIndex beforeBrace #"\n" of
              SOME newline =>
                String.substring (beforeBrace, newline + 1,
                  String.size beforeBrace - newline - 1)
            | NONE => beforeBrace
          val trimmedLine = trim linePrefix
          val typeContext =
            String.isPrefix "type " trimmedLine orelse
            String.isPrefix "datatype " trimmedLine orelse
            endsWith trimmedLine " of"
        in
          if contains segment "=" orelse typeContext then []
          else
            let
              val present =
                List.map
                  (fn part =>
                    case String.fields (fn character => character = #"=") part of
                      [] => ""
                    | first :: _ => trim first)
                  (String.fields (fn character => character = #",") inner)
              val fields =
                case expectedRecordFields beforeBrace db of
                  SOME expected => expected
                | NONE => LspTypes.allFields db
            in
              fieldItems fields (present, false)
            end
        end

  fun usedConstructors afterOf =
    List.mapPartial
      (fn part =>
        let val word = wordPrefix (trim part) in
          if String.size word > 0 andalso Char.isUpper (String.sub (word, 0))
          then SOME word else NONE
        end)
      (String.fields (fn character => character = #"|") afterOf)

  fun annotationType annotations scrutinee =
    let
      val name =
        case List.rev (String.fields (fn character => character = #".") scrutinee) of
          segment :: _ => segment
        | [] => scrutinee
    in
      case List.find (fn (candidate, _) => candidate = name) annotations of
        SOME (_, annotation) =>
          let val typeName = wordPrefix (trim annotation) in
            if typeName = "" then NONE else SOME typeName
          end
      | NONE => NONE
    end

  fun caseCompletion beforeCursor (db : LspTypes.db) annotations =
    case (lastWordIndex beforeCursor "case", lastWordIndex beforeCursor "of") of
      (SOME caseIndex, SOME ofIndex) =>
        if caseIndex >= ofIndex then []
        else
          let
            val afterOf =
              String.substring (beforeCursor, ofIndex + 2,
                String.size beforeCursor - ofIndex - 2)
            val lastArrow =
              let
                fun loop index =
                  if index < 0 then NONE
                  else if index + 2 <= String.size afterOf andalso
                          String.substring (afterOf, index, 2) = "=>"
                  then SOME index else loop (index - 1)
              in
                loop (String.size afterOf - 2)
              end
            val lastPipe = lastCharIndex afterOf #"|"
            val patternPosition =
              case lastArrow of
                NONE => true
              | SOME arrow =>
                  (case lastPipe of
                     SOME pipe => pipe > arrow
                   | NONE => false)
          in
            if not patternPosition then []
            else
              let
                val scrutinee =
                  trim (String.substring (beforeCursor, caseIndex + 4,
                    ofIndex - caseIndex - 4))
                val typeName = annotationType annotations scrutinee
                val constructors =
                  case typeName of
                    SOME name =>
                      (case LspTypes.findDatatype db name of
                         SOME constructors => constructors
                       | NONE => LspTypes.allConstructors db)
                  | NONE => LspTypes.allConstructors db
                val used = usedConstructors afterOf
                val available =
                  List.filter
                    (fn constructor =>
                      not (List.exists (fn candidate =>
                        candidate = #name constructor) used))
                    constructors
                val spacer =
                  let val size = String.size beforeCursor in
                    if size > 0 andalso
                       not (Char.isSpace (String.sub (beforeCursor, size - 1))) andalso
                       (String.isSuffix "of" beforeCursor orelse
                        String.isSuffix "|" beforeCursor)
                    then " " else ""
                  end
              in
                List.map
                  (fn constructor =>
                    completionItemText
                      (#name constructor)
                      "constructor"
                      ("case " ^ #typeName constructor)
                      (spacer ^ LspTypes.patternTextWith db constructor ^ " => "))
                  available
              end
          end
      | _ => []

  fun lastOpenParen text =
    let
      val size = String.size text
      fun loop index depth =
        if index < 0 then NONE
        else
          case String.sub (text, index) of
            #")" => loop (index - 1) (depth + 1)
          | #"(" =>
              if depth = 0 then SOME index
              else loop (index - 1) (depth - 1)
          | _ => loop (index - 1) depth
    in
      loop (size - 1) 0
    end

  fun callNameBefore text openIndex =
    let
      fun isNameChar character =
        Char.isAlphaNum character orelse
        character = #"_" orelse character = #"." orelse character = #"'"
      fun skipSpace index =
        if index >= 0 andalso Char.isSpace (String.sub (text, index)) then
          skipSpace (index - 1)
        else index
      fun nameStart index =
        if index >= 0 andalso isNameChar (String.sub (text, index)) then
          nameStart (index - 1)
        else index + 1
      val last = skipSpace (openIndex - 1)
      val start = nameStart last
    in
      if last < 0 then ""
      else String.substring (text, start, last - start + 1)
    end

  fun argumentIndexAt text openIndex =
    let
      val size = String.size text
      fun loop index depth count =
        if index >= size then count
        else
          case String.sub (text, index) of
            #"(" => loop (index + 1) (depth + 1) count
          | #"[" => loop (index + 1) (depth + 1) count
          | #"{" => loop (index + 1) (depth + 1) count
          | #")" => loop (index + 1) (Int.max (0, depth - 1)) count
          | #"]" => loop (index + 1) (Int.max (0, depth - 1)) count
          | #"}" => loop (index + 1) (Int.max (0, depth - 1)) count
          | #"," => loop (index + 1) depth (if depth = 0 then count + 1 else count)
          | _ => loop (index + 1) depth count
    in
      loop (openIndex + 1) 0 0
    end

  fun baseName name =
    case List.rev (String.tokens (fn character => character = #".") name) of
      last :: _ => last
    | [] => name

  fun functionParameters ty =
    case Types.prune ty of
      Types.TFun (params, _) => SOME params
    | Types.TConsume inner => functionParameters inner
    | _ => NONE

  fun dataExample name =
    if name = "list" then "Nil"
    else if name = "option" then "None"
    else if name = "result" then "Error (\"\")"
    else ""

  fun exampleFor ty =
    case Types.prune ty of
      Types.TInt _ => "0"
    | Types.TIntLit _ => "0"
    | Types.TFloat _ => "0.0"
    | Types.TFloatLit _ => "0.0"
    | Types.TBool => "false"
    | Types.TUnit => "()"
    | Types.TString => "\"\""
    | Types.TArray _ => "array_new ()"
    | Types.TMap _ => "map_new ()"
    | Types.TData (Types.Data {name, ...}) => dataExample name
    | Types.TDataApp (Types.Data {name, ...}, _) => dataExample name
    | Types.TTuple values =>
        "(" ^ String.concatWith ", " (List.map exampleFor values) ^ ")"
    | Types.TFun ([], _) => "() => ()"
    | Types.TFun ([_], _) => "x => x"
    | Types.TFun _ => "(x, y) => x"
    | Types.TConsume inner => exampleFor inner
    | _ => ""

  fun shapeNames count =
    let
      val preferred = ["x", "y", "z", "a", "b", "c"]
      fun nameAt index =
        if index < List.length preferred then List.nth (preferred, index)
        else "x" ^ Int.toString (index + 1)
    in
      List.tabulate (count, nameAt)
    end

  fun inlineShape count =
    case count of
      0 => ("() => …", "() => ")
    | 1 => ("x => …", "x => ")
    | _ =>
        let val names = String.concatWith ", " (shapeNames count)
        in ("(" ^ names ^ ") => …", "(" ^ names ^ ") => ")
        end

  fun exampleForSyntax syntax =
    case syntax of
      Ast.TyInt _ => "0"
    | Ast.TyFloat _ => "0.0"
    | Ast.TyBool => "false"
    | Ast.TyUnit => "()"
    | Ast.TyString => "\"\""
    | Ast.TyName name => dataExample name
    | Ast.TyApp ("list", _) => "Nil"
    | Ast.TyApp ("option", _) => "None"
    | Ast.TyApp ("result", _) => "Error (\"\")"
    | Ast.TyApp ("array", _) => "array_new ()"
    | Ast.TyApp ("map", _) => "map_new ()"
    | Ast.TyTuple values =>
        "(" ^ String.concatWith ", " (List.map exampleForSyntax values) ^ ")"
    | Ast.TyFun (params, _) =>
        let val (_, insert) = inlineShape (List.length params)
        in trim insert end
    | Ast.TyConsume (_, inner) => exampleForSyntax inner
    | Ast.TyRecord [] => "{}"
    | _ => ""

  fun functionArityOfSyntax syntax =
    case syntax of
      Ast.TyFun (params, _) => SOME (List.length params)
    | Ast.TyConsume (_, inner) => functionArityOfSyntax inner
    | _ => NONE

  fun checkedParameterLabels (sigInfo : Check.funSig) =
    let
      val types = #params sigInfo
      val names = CheckBuiltins.parameterNames (baseName (#name sigInfo))
      val nameCount = List.length names
      fun labelAt index =
        let val ty = Types.toString (List.nth (types, index)) in
          if index < nameCount then List.nth (names, index) ^ " : " ^ ty
          else ty
        end
    in
      List.tabulate (List.length types, labelAt)
    end

  fun checkedSignatureText (sigInfo : Check.funSig) =
    "fun " ^ #name sigInfo ^ " (" ^
    String.concatWith ", " (checkedParameterLabels sigInfo) ^
    ") : " ^ Types.toString (#result sigInfo)

  type parameterInfo =
    { name : string
    , example : string
    , callbackArity : int option
    }

  fun parameterInfo (name, annot) =
    { name = name
    , example =
        (case annot of
           NONE => ""
         | SOME syntax => exampleForSyntax syntax)
    , callbackArity =
        (case annot of
           NONE => NONE
         | SOME syntax => functionArityOfSyntax syntax)
    }

  fun placeholder (index, info : parameterInfo) =
    let
      val label =
        if #name info <> "" then #name info else #example info
    in
      if label = "" then "$" ^ Int.toString index
      else "${" ^ Int.toString index ^ ":" ^ label ^ "}"
    end

  fun callSnippetParts name infos =
    if List.null infos then name ^ " ()"
    else
      name ^ " (" ^
      String.concatWith ", "
        (List.map placeholder
           (ListPair.zip
              (List.tabulate (List.length infos, fn index => index + 1),
               infos))) ^
      ")"

  fun callSnippet name infos = callSnippetParts name infos

  fun pipeCallSnippet name infos =
    case infos of
      _ :: rest =>
        if List.null rest then name
        else callSnippetParts name rest
    | [] => name

  fun declarationParams declaration =
    case declaration of
      Ast.DFun {params, ...} =>
        SOME (List.map (fn param => parameterInfo (#name param, #annot param)) params)
    | Ast.DFunClause {params, ...} =>
        SOME
          (List.map
             (fn (pattern, annot) =>
               let
                 val name =
                   case LspAst.patternNames pattern of
                     name :: _ => name
                   | [] => ""
               in
                 parameterInfo (name, annot)
               end)
             params)
    | _ => NONE

  fun signatureMatches name (candidate, _) =
    let val parts = String.tokens (fn character => character = #".") name in
      candidate = name orelse
      (case List.rev parts of
         member :: qualifier :: _ =>
           String.isSuffix ("." ^ qualifier ^ "." ^ member) candidate
       | [member] => String.isSuffix ("." ^ member) candidate
       | [] => false)
    end

  fun signatureIndex (checked : Driver.checked option) =
    case checked of
      NONE => (fn _ => NONE)
    | SOME info =>
        let
          val functions = #functions info
          val size = Int.max (64, 2 * List.length functions)
          val buckets = Array.array (size, [])
          fun insert entry =
            let
              val index = LspDocs.hashName size (baseName (#1 entry))
              val bucket = Array.sub (buckets, index)
            in
              Array.update (buckets, index, entry :: bucket)
            end
          val _ = List.app insert functions
        in
          fn name =>
            let
              val bucket =
                Array.sub (buckets, LspDocs.hashName size (baseName name))
            in
              case List.find (fn entry => signatureMatches name entry) bucket of
                SOME entry => SOME entry
              | NONE => NONE
            end
        end

  fun isFunctionDeclaration declaration =
    case declaration of
      Ast.DFun _ => true
    | Ast.DFunClause _ => true
    | _ => false

  fun declarationIndex modules program =
    let
      val current = LspDocs.indexOf program
      val moduleIndexes =
        List.map
          (fn module => (#key module, LspDocs.indexOf (#program module)))
          modules
      fun qualifierOf name =
        case List.rev (String.tokens (fn character => character = #".") name) of
          _ :: candidate :: _ => SOME candidate
        | _ => NONE
      fun findIn (qualifier, wanted) =
        let
          fun currentResult () =
            case LspDocs.findDeclIn (current, wanted) of
              SOME declaration =>
                if isFunctionDeclaration declaration then SOME declaration
                else NONE
            | NONE => NONE
          fun moduleMatches key =
            case qualifier of
              NONE => true
            | SOME value =>
                key = value orelse String.isSuffix ("/" ^ value) key
          fun search [] = NONE
            | search ((key, index) :: rest) =
                if moduleMatches key then
                  (case LspDocs.findDeclIn (index, wanted) of
                     SOME declaration =>
                       if isFunctionDeclaration declaration then
                         SOME declaration
                       else search rest
                   | NONE => search rest)
                else search rest
        in
          case currentResult () of
            SOME declaration => SOME declaration
          | NONE => search moduleIndexes
        end
    in
      fn name => findIn (qualifierOf name, baseName name)
    end

  type parameterLookup = string -> parameterInfo list option

  fun parameterLookupFor modules program (checked : Driver.checked option) =
    let
      val signatureOf = signatureIndex checked
      val declarationOf = declarationIndex modules program
      fun namesOf name =
        case declarationOf name of
          SOME declaration =>
            (case declarationParams declaration of
               SOME infos => List.map #name infos
             | NONE => [])
        | NONE => []
      fun fromChecked name =
        case signatureOf name of
          NONE => NONE
        | SOME (_, sigInfo) =>
            let
              val declared = namesOf name
              val names =
                if List.null declared then
                  CheckBuiltins.parameterNames (baseName name)
                else declared
              val nameCount = List.length names
              fun at index =
                if index < nameCount then List.nth (names, index) else ""
              val params = #params sigInfo
            in
              SOME
                (List.tabulate
                   (List.length params, fn index =>
                     let val ty = List.nth (params, index) in
                       { name = at index
                       , example = exampleFor ty
                       , callbackArity =
                           Option.map List.length (functionParameters ty)
                       }
                     end))
            end
      fun fromAst name =
        case declarationOf name of
          NONE => NONE
        | SOME declaration => declarationParams declaration
    in
      fn name =>
        case fromChecked name of
          SOME infos => SOME infos
        | NONE => fromAst name
    end

  fun callContext lookup beforeCursor =
    case lastOpenParen beforeCursor of
      NONE => NONE
    | SOME openIndex =>
        let
          val name = callNameBefore beforeCursor openIndex
        in
          if name = "" then NONE
          else
            case lookup name of
              NONE => NONE
              | SOME params =>
                  let val index = argumentIndexAt beforeCursor openIndex in
                    if index < List.length params then
                      SOME (name, index, List.nth (params, index))
                    else NONE
                  end
        end

  fun callbackCompletion lookup beforeCursor =
    case callContext lookup beforeCursor of
      NONE => []
    | SOME (_, _, { callbackArity, ... }) =>
        (case callbackArity of
           NONE => []
         | SOME count =>
             let
               val (label, insert) = inlineShape count
             in
               [completionItemText
                  label "function"
                  ("inline function with " ^ Int.toString count ^ " argument(s)")
                  insert]
             end)

  fun applyCallSnippet lookup expectedCallback inPipe qualifier item =
    case item of
      Json.Object _ =>
        (case member "label" item of
           SOME (Json.String label) =>
             if expectedCallback then item
             else
               let
                 val lookupName =
                   case qualifier of
                     SOME prefix => prefix ^ "." ^ label
                   | NONE => label
               in
                 case lookup lookupName of
                   NONE => item
                 | SOME infos =>
                     withSnippet item
                       (if inPipe then pipeCallSnippet label infos
                        else callSnippet label infos)
               end
         | _ => item)
    | _ => item

  datatype fieldType =
      NamedFieldType of string
    | RecordFieldType of LspTypes.field list
    | ListFieldType of fieldType
    | TupleFieldType of fieldType list
    | UnknownFieldType

  fun fieldTypeOfText text =
    let val text = trim text in
      if text = "" then UnknownFieldType
      else if String.isSuffix " list" text then
        ListFieldType
          (fieldTypeOfText
             (String.substring (text, 0, String.size text - 5)))
      else if String.isPrefix "{" text andalso String.isSuffix "}" text then
        let
          val inner =
            trim (String.substring (text, 1, String.size text - 2))
          fun parseField part =
            case String.fields (fn character => character = #":") part of
              [] => { name = "", ty = "" }
            | name :: rest =>
                { name = trim name
                , ty = trim (String.concatWith ":" rest)
                }
          val fields =
            if inner = "" then []
            else List.map parseField
                   (String.fields (fn character => character = #",") inner)
        in
          RecordFieldType fields
        end
      else if String.isPrefix "(" text andalso String.isSuffix ")" text then
        let
          val inner =
            trim (String.substring (text, 1, String.size text - 2))
          val parts = String.fields (fn character => character = #"*") inner
        in
          TupleFieldType (List.map (fieldTypeOfText o trim) parts)
        end
      else NamedFieldType text
    end

  fun fieldTypeOfArgument argument =
    case argument of
      LspTypes.NoArgument => UnknownFieldType
    | LspTypes.RecordArgument fields => RecordFieldType fields
    | LspTypes.TupleArgument types =>
        TupleFieldType (List.map fieldTypeOfText types)
    | LspTypes.OtherArgument name => fieldTypeOfText name

  fun fieldsOfValueType db valueType =
    case valueType of
      RecordFieldType fields => SOME fields
    | NamedFieldType name =>
        (case LspTypes.findRecord db name of
           SOME fields => SOME fields
         | NONE =>
             (case LspTypes.findDatatype db name of
                SOME constructors => commonFieldsOfConstructors db constructors
              | NONE => NONE))
    | _ => NONE

  and commonFieldsOfConstructors db constructors =
    let
      fun loop [] = NONE
        | loop (constructor :: rest) =
            (case LspTypes.recordFieldsOf db constructor of
               NONE => NONE
             | SOME fields =>
                 (case loop rest of
                    NONE => SOME fields
                  | SOME other =>
                      SOME
                        (List.filter
                           (fn field =>
                             List.exists
                               (fn candidate => #name candidate = #name field)
                               other)
                           fields)))
    in
      loop constructors
    end

  fun lookupFieldType env name =
    case List.find (fn (candidate, _) => candidate = name) env of
      SOME (_, ty) => ty
    | NONE => UnknownFieldType

  fun inferValueType db env expression =
    case expression of
      Ast.EVar (name, _) => lookupFieldType env name
    | Ast.EString _ => NamedFieldType "string"
    | Ast.EInt _ => NamedFieldType "int"
    | Ast.EFloat _ => NamedFieldType "float"
    | Ast.EBool _ => NamedFieldType "bool"
    | Ast.EUnit _ => NamedFieldType "unit"
    | Ast.ECall (name, _, _) =>
        (case LspTypes.findConstructor db name of
           SOME constructor => NamedFieldType (#typeName constructor)
         | NONE => UnknownFieldType)
    | Ast.EBin ("::", head, _, _) =>
        ListFieldType (inferValueType db env head)
    | Ast.EIf (_, thenBranch, _, _) => inferValueType db env thenBranch
    | Ast.ECase (_, (_, _, body) :: _, _) => inferValueType db env body
    | Ast.EField (field, base, _) =>
        (case fieldsOfValueType db (inferValueType db env base) of
           SOME fields =>
             (case List.find (fn candidate => #name candidate = field) fields of
                SOME found => fieldTypeOfText (#ty found)
              | NONE => UnknownFieldType)
         | NONE => UnknownFieldType)
    | Ast.ETuple (values, _) =>
        TupleFieldType (List.map (inferValueType db env) values)
    | Ast.ETupleIndex (index, base, _) =>
        (case inferValueType db env base of
           TupleFieldType types =>
             if index >= 0 andalso index < List.length types then
               List.nth (types, index)
             else UnknownFieldType
         | _ => UnknownFieldType)
    | Ast.EBorrow (_, _, operand, _) => inferValueType db env operand
    | _ => UnknownFieldType

  fun patternBindings db scrutinee pattern =
    case pattern of
      Ast.PVar (name, _) => [(name, scrutinee)]
    | Ast.PWildcard _ => []
    | Ast.PString (_, SOME rest, _) =>
        patternBindings db (NamedFieldType "string") rest
    | Ast.PString (_, NONE, _) => []
    | Ast.PBytes (_, SOME rest, _) =>
        patternBindings db (NamedFieldType "int8 array") rest
    | Ast.PBytes (_, NONE, _) => []
    | Ast.PConstructor ("Cons", SOME (Ast.PTuple ([head, tail], _)), _) =>
        (case scrutinee of
           ListFieldType element =>
             patternBindings db element head @
             patternBindings db scrutinee tail
         | _ => [])
    | Ast.PConstructor (name, SOME inner, _) =>
        (case LspTypes.findConstructor db name of
           SOME constructor =>
             patternBindings db (fieldTypeOfArgument (#argument constructor))
               inner
         | NONE => [])
    | Ast.PTuple (patterns, _) =>
        (case scrutinee of
           TupleFieldType types =>
             let
               fun zip ([], _) = []
                 | zip (_, []) = []
                 | zip (pattern :: patterns, ty :: types) =
                     patternBindings db ty pattern @ zip (patterns, types)
             in
               zip (patterns, types)
             end
         | _ => [])
    | Ast.PRecord (fields, _) =>
        (case scrutinee of
           RecordFieldType recordFields =>
             List.concat
               (List.map
                  (fn (name, subpattern) =>
                    case List.find
                           (fn candidate => #name candidate = name)
                           recordFields of
                      SOME found =>
                        patternBindings db (fieldTypeOfText (#ty found))
                          subpattern
                    | NONE => [])
                  fields)
         | _ => [])
    | _ => []

  exception FoundField of fieldType

  fun fieldsForName (db, annotations, program, qualifier) =
    (let
      val initialEnv =
        List.map (fn (name, ty) => (name, fieldTypeOfText ty)) annotations
      fun checkBindings bindings =
        case List.find (fn (name, _) => name = qualifier) bindings of
          SOME (_, ty) => raise FoundField ty
        | NONE => ()
      fun walk env expression =
        case expression of
          Ast.ELet (bindings, body, _) =>
            let
              fun add (binding, env) =
                case binding of
                  Ast.BVal {name, annot, value, ...} =>
                    let
                      val ty =
                        case annot of
                          SOME syntax => fieldTypeOfText (Ast.tyToString syntax)
                        | NONE => inferValueType db env value
                      val _ =
                        if name = qualifier then raise FoundField ty else ()
                    in
                      (name, ty) :: env
                    end
                | Ast.BPat {pattern, value, ...} =>
                    let
                      val ty = inferValueType db env value
                      val bound = patternBindings db ty pattern
                      val _ = checkBindings bound
                    in
                      bound @ env
                    end
                | Ast.BFun {body, ...} => (walk env body; env)
                | Ast.BGuard {pattern = NONE, ...} => env
                | Ast.BGuard {pattern = SOME pattern, value, ...} =>
                    let
                      val ty = inferValueType db env value
                      val bound = patternBindings db ty pattern
                      val _ = checkBindings bound
                    in
                      bound @ env
                    end
              val extended = List.foldl add env bindings
            in
              walk extended body
            end
        | Ast.EArena (bindings, body, _) =>
            let
              fun add (binding, env) =
                case binding of
                  Ast.BVal {name, annot, value, ...} =>
                    let
                      val ty =
                        case annot of
                          SOME syntax => fieldTypeOfText (Ast.tyToString syntax)
                        | NONE => inferValueType db env value
                      val _ =
                        if name = qualifier then raise FoundField ty else ()
                    in
                      (name, ty) :: env
                    end
                | Ast.BPat {pattern, value, ...} =>
                    let
                      val ty = inferValueType db env value
                      val bound = patternBindings db ty pattern
                      val _ = checkBindings bound
                    in
                      bound @ env
                    end
                | Ast.BFun {body, ...} => (walk env body; env)
                | Ast.BGuard {pattern = NONE, ...} => env
                | Ast.BGuard {pattern = SOME pattern, value, ...} =>
                    let
                      val ty = inferValueType db env value
                      val bound = patternBindings db ty pattern
                      val _ = checkBindings bound
                    in
                      bound @ env
                    end
              val extended = List.foldl add env bindings
            in
              walk extended body
            end
        | Ast.ECase (scrutinee, branches, _) =>
            let
              val scrutineeType = inferValueType db env scrutinee
              val _ =
                List.app
                  (fn (pattern, guard, body) =>
                    let
                      val bound = patternBindings db scrutineeType pattern
                      val _ = checkBindings bound
                      val env' = bound @ env
                      val _ =
                        case guard of
                          SOME condition => walk env' condition
                        | NONE => ()
                    in
                      walk env' body
                    end)
                  branches
            in
              ()
            end
        | Ast.EIf (condition, thenBranch, elseBranch, _) =>
            (walk env condition; walk env thenBranch; walk env elseBranch)
        | Ast.EBin (_, left, right, _) =>
            (walk env left; walk env right)
        | Ast.ENeg (operand, _) => walk env operand
        | Ast.ECall (_, arguments, _) => List.app (walk env) arguments
        | Ast.ERecord (fields, _) =>
            List.app (fn (_, value) => walk env value) fields
        | Ast.EField (_, base, _) => walk env base
        | Ast.ETuple (values, _) => List.app (walk env) values
        | Ast.ETupleIndex (_, base, _) => walk env base
        | Ast.EBorrow (_, _, operand, _) => walk env operand
        | Ast.EFn (params, _, body, _) =>
            let
              fun addParam (param, env) =
                (#name param,
                 case #annot param of
                   SOME syntax => fieldTypeOfText (Ast.tyToString syntax)
                 | NONE => UnknownFieldType) :: env
              val env' = List.foldl addParam env params
              val _ =
                List.app
                  (fn param =>
                    if #name param = qualifier then
                      raise FoundField (lookupFieldType env' (#name param))
                    else ())
                  params
            in
              walk env' body
            end
        | _ => ()
      val _ =
        List.app
          (fn declaration =>
            case declaration of
              Ast.DFun {params, body, ...} =>
                let
                  fun addParam (param, env) =
                    (#name param,
                     case #annot param of
                       SOME syntax => fieldTypeOfText (Ast.tyToString syntax)
                     | NONE => UnknownFieldType) :: env
                  val env = List.foldl addParam initialEnv params
                  val _ =
                    List.app
                      (fn param =>
                        if #name param = qualifier then
                          raise FoundField (lookupFieldType env (#name param))
                        else ())
                      params
                in
                  walk env body
                end
            | Ast.DFunClause {params, body, ...} =>
                let
                  fun addParam ((pattern, annot), env) =
                    let
                      val ty =
                        case annot of
                          SOME syntax => fieldTypeOfText (Ast.tyToString syntax)
                        | NONE => UnknownFieldType
                      val bound = patternBindings db ty pattern
                      val _ = checkBindings bound
                    in
                      bound @ env
                    end
                  val env = List.foldl addParam initialEnv params
                in
                  walk env body
                end
            | _ => ())
          program
    in
      NONE
    end) handle FoundField ty => SOME ty

  fun bindingNameSpans program =
    let
      val names = ref []
      fun add (span, name) = names := (annotatedStart span, name) :: !names
      fun addPattern pattern =
        case pattern of
          Ast.PVar (name, span) => add (span, name)
        | Ast.PString (_, SOME inner, _) => addPattern inner
        | Ast.PBytes (_, SOME inner, _) => addPattern inner
        | Ast.PConstructor (_, SOME inner, _) => addPattern inner
        | Ast.PTuple (patterns, _) => List.app addPattern patterns
        | Ast.PRecord (fields, _) =>
            List.app (fn (_, subpattern) => addPattern subpattern) fields
        | _ => ()
      fun walk expression =
        case expression of
          Ast.ELet (bindings, body, _) =>
            (List.app
               (fn binding =>
                 case binding of
                   Ast.BVal {name, annot, span, value} =>
                     (add (span, name); walk value)
                 | Ast.BPat {pattern, value, span} =>
                     (names := (annotatedStart span, "") :: !names;
                      addPattern pattern;
                      walk value)
                 | Ast.BFun {name, span, body, ...} =>
                     (add (span, name); walk body)
                 | Ast.BGuard {pattern, value, fallback, span} =>
                     (names := (annotatedStart span, "") :: !names;
                      (case pattern of
                         SOME pattern => addPattern pattern
                       | NONE => ());
                      walk value;
                      walk fallback))
               bindings;
             walk body)
        | Ast.EArena (bindings, body, _) =>
            (List.app
               (fn binding =>
                 case binding of
                   Ast.BVal {name, annot, span, value} =>
                     (add (span, name); walk value)
                 | Ast.BPat {pattern, value, span} =>
                     (names := (annotatedStart span, "") :: !names;
                      addPattern pattern;
                      walk value)
                 | Ast.BFun {name, span, body, ...} =>
                     (add (span, name); walk body)
                 | Ast.BGuard {pattern, value, fallback, span} =>
                     (names := (annotatedStart span, "") :: !names;
                      (case pattern of
                         SOME pattern => addPattern pattern
                       | NONE => ());
                      walk value;
                      walk fallback))
               bindings;
             walk body)
        | Ast.ECase (scrutinee, branches, _) =>
            (walk scrutinee;
             List.app
               (fn (pattern, guard, body) =>
                 (addPattern pattern;
                  (case guard of SOME condition => walk condition | NONE => ());
                  walk body))
               branches)
        | Ast.EIf (condition, thenBranch, elseBranch, _) =>
            (walk condition; walk thenBranch; walk elseBranch)
        | Ast.EBin (_, left, right, _) => (walk left; walk right)
        | Ast.ENeg (operand, _) => walk operand
        | Ast.ECall (_, arguments, _) => List.app walk arguments
        | Ast.ERecord (fields, _) => List.app (fn (_, value) => walk value) fields
        | Ast.EField (_, base, _) => walk base
        | Ast.ETuple (values, _) => List.app walk values
        | Ast.ETupleIndex (_, base, _) => walk base
        | Ast.EBorrow (_, _, operand, _) => walk operand
        | _ => ()
    in
      List.app
        (fn declaration =>
          case declaration of
            Ast.DFun {params, body, ...} =>
              (List.app (fn {name, annot, span} => add (span, name)) params;
               walk body)
          | Ast.DFunClause {params, body, ...} =>
              (List.app
                 (fn (pattern, _) =>
                   List.app
                     (fn name => add (Ast.patternSpan pattern, name))
                     (LspAst.patternNames pattern))
                 params;
               walk body)
          | _ => ())
        program;
      !names
    end

  fun bindingTypeOfName (checked : Driver.checked) name =
    let
      val spans = bindingNameSpans (#program checked)
    in
      case List.find (fn (_, candidate) => candidate = name) spans of
        NONE => NONE
      | SOME (key, _) =>
          (case List.find
                  (fn (span, _) => annotatedStart span = key)
                  (#bindingTypes checked) of
             SOME (_, ty) => SOME ty
           | NONE => NONE)
    end

  fun fieldsOfCheckedType ty =
    case Types.prune ty of
      Types.TCon {fields, ...} =>
        SOME
          (List.map
             (fn (name, fieldTy) =>
               { name = name, ty = Types.toString fieldTy })
             fields)
    | Types.TRecord fields =>
        SOME
          (List.map
             (fn (name, fieldTy) =>
               { name = name, ty = Types.toString fieldTy })
             fields)
    | Types.TData data =>
        (case Types.dataCommonFields data [] of
           SOME fields =>
             SOME
               (List.map
                  (fn (name, fieldTy) =>
                    { name = name, ty = Types.toString fieldTy })
                  fields)
         | NONE => NONE)
    | Types.TDataApp (data, arguments) =>
        (case Types.dataCommonFields data arguments of
           SOME fields =>
             SOME
               (List.map
                  (fn (name, fieldTy) =>
                    { name = name, ty = Types.toString fieldTy })
                  fields)
         | NONE => NONE)
    | Types.TBorrow (inner, _, _) => fieldsOfCheckedType inner
    | _ => NONE

  fun kindName kind =
    case kind of
      3 => "function"
    | 4 => "constructor"
    | 9 => "module"
    | 14 => "keyword"
    | 15 => "snippet"
    | 22 => "type"
    | _ => "variable"

  fun resolveCompletionItem state item =
    case Json.field "documentation" item of
      SOME _ => item
    | NONE =>
        (case Json.field "data" item of
           NONE => item
         | SOME data =>
             let
               val uri = getOpt (Json.stringField "uri" data, "")
               val name = getOpt (Json.stringField "name" data, "")
               val kind = kindName (getOpt (Json.intField "kind" data, 6))
             in
               if uri = "" orelse name = "" then item
               else if kind = "module" orelse kind = "keyword" orelse
                       kind = "snippet" then item
               else
                 let
                   val path = uriToPath uri
                   val text =
                     case findDoc state uri of
                       SOME document => #text document
                     | NONE => getOpt (Driver.readFile path, "")
                   val (program, comments) = parsedOf state uri text
                   val currentIndex = LspDocs.indexOf program
                   val isConstructor = kind = "constructor"
                   fun lookup (index, moduleText) =
                     let
                       val signatureText =
                         if isConstructor then
                           LspDocs.constructorSignature (#program index) name
                         else LspDocs.signatureIn (index, name)
                       val doc =
                         if isConstructor then
                           LspDocs.constructorDocWith
                             (moduleText, LspDocs.commentsOf moduleText,
                              #program index, name)
                         else
                           LspDocs.docInWith
                             (moduleText, LspDocs.commentsOf moduleText, index,
                              name)
                     in
                       case signatureText of
                         NONE => NONE
                       | SOME value => SOME (value, doc)
                     end
                   val imported =
                     List.mapPartial
                       (fn (key, _) => moduleFor state path key)
                       (importsFromText text)
                   val others =
                     List.filter
                       (fn module =>
                         not (List.exists
                                (fn candidate => #key candidate = #key module)
                                imported))
                       (indexFor state path)
                   fun firstSome [] = NONE
                     | firstSome (value :: rest) =
                         (case value of
                            SOME found => SOME found
                          | NONE => firstSome rest)
                   val found =
                     case lookup (currentIndex, text) of
                       SOME value => SOME value
                     | NONE =>
                         firstSome
                           (List.map
                              (fn module =>
                                lookup (LspDocs.indexOf (#program module),
                                        #text module))
                              (imported @ others))
                   val found =
                     case found of
                       SOME value => SOME value
                     | NONE =>
                         let
                           val checked =
                             case checkedOf state uri of
                               SOME info => SOME info
                             | NONE => lastCheckedOf state uri
                         in
                           case checked of
                             NONE => NONE
                           | SOME info =>
                               (case signatureIndex (SOME info) name of
                                  SOME (_, sigInfo) =>
                                    SOME (checkedSignatureText sigInfo, NONE)
                                | NONE => NONE)
                         end
                 in
                   case found of
                     NONE => item
                   | SOME (signatureText, doc) =>
                       let
                         val item =
                           case documentationMarkdown (SOME signatureText, doc) of
                             SOME value => addField "documentation" value item
                           | NONE => item
                       in
                         case member "detail" item of
                           SOME (Json.String detail) =>
                             if detail = "in this file" orelse
                                detail = "builtin"
                             then
                               (case item of
                                  Json.Object fields =>
                                    Json.Object
                                      (replaceField "detail"
                                         (str signatureText) fields)
                                | _ => item)
                             else item
                         | _ => item
                       end
                 end
             end)

  fun completionAt state uri line character =
    let
      val path = uriToPath uri
      val document = findDoc state uri
      val text = case document of SOME doc => #text doc | NONE => ""
      val source = Source.create text
      val lineText = Source.lineText (source, line + 1)
      val byteColumn = characterToByteColumn (lineText, character)
      val beforeCursor = String.substring (lineText, 0, byteColumn)
      val lineStart =
        let
          fun loop (index, offset) =
            if index >= line then offset
            else
              loop (index + 1,
                offset + String.size (Source.lineText (source, index + 1)) + 1)
        in
          loop (0, 0)
        end
      val plainWord = wordPrefix beforeCursor
      val cursorOffset = lineStart + byteColumn
      val beforeDocument =
        let val stop = Int.min (String.size text, lineStart + byteColumn) in
          String.substring (text, 0, stop)
        end
      val modules = indexFor state path
      val (tolerantProgram, currentComments) = parsedOf state uri text
      val program =
        case desugarText text of
          SOME program => program
        | NONE => tolerantProgram
      val db = LspTypes.dbOf modules tolerantProgram

      fun applyConstructorSnippet item =
        case (member "label" item, member "insertText" item) of
          (SOME (Json.String label), SOME (Json.String insert)) =>
            if insert = label then
              (case LspTypes.findConstructor db label of
                 SOME constructor =>
                   (case LspTypes.recordFieldsOf db constructor of
                      SOME _ =>
                        withSnippet item
                          (LspTypes.constructorSnippet db constructor)
                    | NONE => item)
               | NONE => item)
            else item
        | _ => item
      val annotations = annotationsFromText text @ annotationsOf tolerantProgram
      val currentIndex = LspDocs.indexOf tolerantProgram
      val checked = checkedOf state uri
      val localBindings =
        case LspAst.bindingsAt (tolerantProgram, cursorOffset) of
          SOME bindings => bindings
        | NONE => LspAst.localBindings tolerantProgram

      fun localDetail (name, kind, span) =
        if kind = "function" then "in this file"
        else
          case checked of
            NONE => "in this file"
          | SOME info =>
              let
                fun textOf ty =
                  if List.null (Types.freeVars ty) then Types.toString ty
                  else "in this file"
                fun typeAt candidate =
                  List.find
                    (fn (bindingSpan, _) =>
                      LspAst.annotatedStart bindingSpan =
                        LspAst.annotatedStart candidate)
                    (#bindingTypes info)
              in
                case Option.mapPartial
                       (fn candidate => typeAt candidate)
                       span of
                  SOME (_, ty) => textOf ty
                | NONE =>
                    (case bindingTypeOfName info name of
                       SOME ty => textOf ty
                     | NONE => "in this file")
              end
      val localItems =
        List.map
          (fn {name, kind, depth, span} =>
            rankItem plainWord
              (if kind = "function" then rankLocalFunction
               else rankLocalVariable)
              depth
              (completionItem name kind (localDetail (name, kind, span))))
          localBindings @
        List.map
          (fn (kind, name) =>
            rankItem plainWord
              (case kind of
                 "function" => rankFileFunction
               | "constructor" => rankFileConstructor
               | _ => rankFileType)
              0
              (documentedItem (text, currentComments, currentIndex)
                 kind name "in this file"))
          (exportsOf program)
      val imports =
        let val fromText = importsFromText text in
          if List.null fromText then currentImports program else fromText
        end
      val unqualifiedItems =
        List.concat
          (List.map
             (fn directive =>
               case moduleFor state path (String.concatWith "/" (#path directive)) of
                 NONE => []
               | SOME module =>
                   let val comments = LspDocs.commentsOf (#text module) in
                     List.mapPartial
                       (fn item =>
                         let
                           val (name, alias) =
                             case item of
                               Ast.ImportValue (name, alias) => (name, alias)
                             | Ast.ImportType (name, alias) => (name, alias)
                         in
                           case List.find (fn (_, candidate) => candidate = name)
                                  (#exports module) of
                             NONE => NONE
                           | SOME (kind, _) =>
                               let
                                 val bound =
                                   case alias of SOME value => value | NONE => name
                                 val index = LspDocs.indexOf (#program module)
                               in
                                 SOME
                                   (rankItem plainWord rankImported 0
                                      (documentedItemAs
                                         (#text module, comments, index)
                                         kind bound name
                                         ("imported from " ^ #key module)))
                               end
                         end)
                       (#unqualified directive)
                   end)
             (importsOf program))
      val memberDot =
        let
          val size = String.size beforeCursor
          fun findDot index =
            if index < 0 then NONE
            else if String.sub (beforeCursor, index) = #"." then SOME index
            else findDot (index - 1)
        in
          case findDot (size - 1) of
            NONE => NONE
          | SOME dot =>
              (case lastOpenParen beforeCursor of
                 SOME openIndex => if openIndex > dot then NONE else SOME dot
               | NONE => SOME dot)
        end

      val inPipe =
        let
          fun lastPipe index =
            if index < 0 then NONE
            else if String.substring (beforeCursor, index, 2) = "|>"
            then SOME index
            else lastPipe (index - 1)
        in
          case lastPipe (String.size beforeCursor - 2) of
            NONE => false
          | SOME pipe =>
              (case lastOpenParen beforeCursor of
                 SOME openParen => pipe > openParen
               | NONE => true)
        end
      val parameterLookup = parameterLookupFor modules tolerantProgram checked
      val expectedCallback =
        case callContext parameterLookup beforeCursor of
          SOME (_, _, { callbackArity = arity, ... }) => Option.isSome arity
        | NONE => false
      val callbackItems =
        if plainWord = "" then
          List.map (rankItem plainWord rankCallback 0)
            (callbackCompletion parameterLookup beforeCursor)
        else []
      val memberPrefix =
        case memberDot of
          SOME dot => wordPrefix (String.extract (beforeCursor, dot + 1, NONE))
        | NONE => ""
      val fieldMembers =
        case memberDot of
          NONE => []
        | SOME dot =>
            let
              val qualifier =
                wordPrefix (String.substring (beforeCursor, 0, dot))
              val checkedFields =
                case checked of
                  SOME info => bindingTypeOfName info qualifier
                | NONE => NONE
              val fields =
                case checkedFields of
                  SOME ty => fieldsOfCheckedType ty
                | NONE =>

                    let
                      val probe = "__field_probe"
                      val offset =
                        Int.min (String.size text, lineStart + byteColumn)
                      val repaired =
                        String.substring (text, 0, offset) ^ probe ^
                        String.substring
                          (text, offset, String.size text - offset)
                      val probeProgram = programOfText repaired
                      val probeAnnotations = annotationsOf probeProgram
                    in
                      case
                        fieldsForName
                          (db, probeAnnotations, probeProgram, qualifier) of
                        SOME valueType => fieldsOfValueType db valueType
                      | NONE => NONE
                    end
            in
              if qualifier = "" then []
              else
                case fields of
                  NONE => []
                | SOME fields =>
                    List.map
                      (fn field =>
                        completionItem (#name field) "field"
                          (#name field ^ " : " ^ #ty field))
                      (List.filter
                         (fn field =>
                           memberPrefix = "" orelse
                           String.isPrefix memberPrefix (#name field))
                         fields)
            end
      val members =
        case memberDot of
          NONE => []
        | SOME dot =>
            let
              val qualifier =
                wordPrefix (String.substring (beforeCursor, 0, dot))
            in
              if qualifier = "" then []
              else
                qualifiedCompletions state path imports qualifier memberPrefix @
                autoImportedMembers state path text imports qualifier memberPrefix @
                fieldMembers
            end
      val members = List.map (rankItem memberPrefix rankMember 0) members
      val isImport = String.isPrefix "import" (trim beforeCursor)
      val caseItems = caseCompletion beforeDocument db annotations
      val recordItems = recordCompletion beforeDocument db
      val caseStartByte =
        let
          val ofIndex = lastWordIndex beforeCursor "of"
          val pipeIndex = lastCharIndex beforeCursor #"|"
          val separator =
            case (ofIndex, pipeIndex) of
              (SOME left, SOME right) => SOME (Int.max (left, right))
            | (SOME index, NONE) => SOME index
            | (NONE, SOME index) => SOME index
            | (NONE, NONE) => NONE
          val word =
            case separator of
              SOME index =>
                wordPrefix
                  (String.substring (beforeCursor, index + 1,
                    String.size beforeCursor - index - 1))
            | NONE => plainWord
        in
          Int.max (0, byteColumn - String.size word)
        end
      val importPrefix =
        if isImport then
          let val trimmed = trim beforeCursor in
            if String.isPrefix "import" trimmed then
              trim (String.extract (trimmed, 6, NONE))
            else trimmed
          end
        else ""
      val items =
        if isImport then
          List.map (rankItem importPrefix rankModule 0)
            (importCompletion state path importPrefix)
        else
          case memberDot of
            SOME _ => members
          | NONE =>
          if not (List.null caseItems) then caseItems
          else if not (List.null recordItems) then recordItems
          else
          callbackItems @
          localItems @
          unqualifiedItems @
          List.map (rankItem plainWord rankAutoImportModule 0)
            (autoImportModuleItems state path text imports plainWord) @
          (case List.find (fn module => #key module = "<builtins>") modules of
             SOME module =>
               let
                 val comments = LspDocs.commentsOf (#text module)
                 val index = LspDocs.indexOf (#program module)
               in
                 List.map
                   (fn (kind, name) =>
                     let
                       val fallback =
                         case (case checked of
                                 SOME info => SOME info
                               | NONE => lastCheckedOf state uri) of
                           NONE => "builtin"
                         | SOME info =>
                             (case signatureIndex (SOME info) name of
                                SOME (_, sigInfo) =>
                                  checkedSignatureText sigInfo
                              | NONE => "builtin")
                     in
                       rankItem plainWord rankBuiltin 0
                         (documentedItem
                            (#text module, comments, index)
                            kind name fallback)
                     end)
                   (#exports module)
               end
           | NONE => []) @
          (case List.find (fn module => #key module = "<prelude>") modules of
             SOME module =>
               let
                 val comments = LspDocs.commentsOf (#text module)
                 val index = LspDocs.indexOf (#program module)
               in
                 List.map
                   (fn (kind, name) =>
                     rankItem plainWord rankPrelude 0
                       (documentedItem
                          (#text module, comments, index)
                          kind name "prelude"))
                   (#exports module)
               end
           | NONE => []) @
          List.map
            (fn (qualifier, key) =>
              rankItem plainWord rankModule 0
                (completionItem qualifier "module" ("module " ^ key)))
            imports @
          List.map (rankItem plainWord rankKeyword 0) (keywordItems ())
      val memberQualifier =
        case memberDot of
          NONE => NONE
        | SOME dot =>
            let
              val qualifier =
                wordPrefix (String.substring (beforeCursor, 0, dot))
            in
              if qualifier = "" then NONE else SOME qualifier
            end
      val items =
        List.map
          (fn item =>
            applyConstructorSnippet
              (applyCallSnippet parameterLookup expectedCallback inPipe
                 memberQualifier item))
          items
      val replaceStartByte =
        if isImport then Int.max (0, byteColumn - String.size importPrefix)
        else if not (List.null caseItems) then caseStartByte
        else
          case memberDot of
            SOME dot => dot + 1
          | NONE => Int.max (0, byteColumn - String.size plainWord)
      val startCharacter = utf16Units lineText replaceStartByte
      fun withTextEdit item =
        case item of
          Json.Object fields =>
            let
              val label =
                case member "label" item of
                  SOME (Json.String name) => name
                | _ => ""
              val newText =
                case member "insertText" item of
                  SOME (Json.String text) => text
                | _ => label
              val edit =
                obj
                  [ ("range",
                      obj
                        [ ("start",
                            obj [ ("line", int line)
                                , ("character", int startCharacter) ])
                        , ("end",
                            obj [ ("line", int line)
                                , ("character", int character) ])
                        ])
                  , ("newText", str newText)
                  ]
              val kind =
                case member "kind" item of
                  SOME (Json.Number value) => value
                | _ => "6"
              val data =
                obj
                  [ ("uri", str uri)
                  , ("name", str label)
                  , ("kind", Json.Number kind)
                  ]
            in
              Json.Object (fields @ [("textEdit", edit), ("data", data)])
            end
        | _ => item
      val items = sortBySortText items
      val seen = ref []
      val items =
        List.filter
          (fn item =>
            let val label = case member "label" item of SOME (Json.String name) => name | _ => "" in
              if List.exists (fn candidate => candidate = label) (!seen) then false
              else (seen := label :: !seen; true)
            end)
          items
    in
      arr (List.map withTextEdit items)
    end
end
