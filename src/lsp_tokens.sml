structure LspTokens =
struct
  open LspProtocol
  open LspText
  open LspState

  val namespaceT = 0
  val typeT = 1
  val enumT = 2
  val structT = 3
  val typeParameterT = 4
  val parameterT = 5
  val variableT = 6
  val propertyT = 7
  val enumMemberT = 8
  val functionT = 9
  val keywordT = 10
  val commentT = 11
  val stringT = 12
  val numberT = 13
  val operatorT = 14

  val tokenTypes =
    [ "namespace", "type", "enum", "struct", "typeParameter", "parameter"
    , "variable", "property", "enumMember", "function", "keyword", "comment"
    , "string", "number", "operator"
    ]

  type nameTable = { size : int, buckets : (string * int) list array }

  fun makeNameTable entries =
    let
      val size = Int.max (64, 2 * List.length entries)
      val buckets = Array.array (size, [])
      fun insert (name, kind) =
        let val index = LspDocs.hashName size name
            val bucket = Array.sub (buckets, index)
        in
          Array.update (buckets, index, (name, kind) :: bucket)
        end
      val _ = List.app insert entries
    in
      { size = size, buckets = buckets }
    end

  fun findName (table : nameTable) name =
    let
      val { size, buckets } = table
      val bucket = Array.sub (buckets, LspDocs.hashName size name)
    in
      case List.find (fn (candidate, _) => candidate = name) bucket of
        SOME (_, kind) => SOME kind
      | NONE => NONE
    end

  type offsetTable = { size : int, buckets : (int * int) list array }

  fun makeOffsetTable entries =
    let
      val size = Int.max (64, 2 * List.length entries)
      val buckets = Array.array (size, [])
      fun insert (offset, kind) =
        let
          val index =
            Word.toInt
              (Word.mod (Word.fromInt offset, Word.fromInt size))
          val bucket = Array.sub (buckets, index)
        in
          Array.update (buckets, index, (offset, kind) :: bucket)
        end
      val _ = List.app insert entries
    in
      { size = size, buckets = buckets }
    end

  fun findOffset (table : offsetTable) offset =
    let
      val { size, buckets } = table
      val index =
        Word.toInt (Word.mod (Word.fromInt offset, Word.fromInt size))
      val bucket = Array.sub (buckets, index)
    in
      case List.find (fn (candidate, _) => candidate = offset) bucket of
        SOME (_, kind) => SOME kind
      | NONE => NONE
    end

  fun fieldKinds fields =
    List.map (fn (name, _) => (name, propertyT)) fields

  fun declarationKinds declaration =
    case declaration of
      Ast.DFun {name, ...} => [(name, functionT)]
    | Ast.DFunClause {name, ...} => [(name, functionT)]
    | Ast.DType {name, fields, ...} =>
        (name, structT) :: fieldKinds fields
    | Ast.DAlias {name, ...} => [(name, typeT)]
    | Ast.DDatatype {name, constructors, ...} =>
        (name, typeT) ::
        List.concat
          (List.map
             (fn (constructor, argument) =>
               (constructor, enumMemberT) ::
               (case argument of
                  SOME (Ast.TyRecord fields) => fieldKinds fields
                | _ => []))
             constructors)
    | Ast.DImport _ => []

  fun programKinds program =
    List.concat (List.map declarationKinds program)

  fun kindOfBinding kind =
    case kind of
      Resolve.Function => functionT
    | Resolve.Parameter => parameterT
    | Resolve.Constructor => enumMemberT
    | Resolve.Builtin => functionT
    | Resolve.Local => variableT
    | Resolve.Pattern => variableT

  fun callMemberOffset (text, span) =
    let
      val size = String.size text
      val start = Pos.offset (#start span)
      val stop = Int.min (size, Pos.offset (#stop span))
      fun isIdent character =
        Char.isAlphaNum character orelse character = #"_" orelse
        character = #"'"
      fun loop (index, last) =
        if index >= stop then last
        else
          case String.sub (text, index) of
            #"(" => last
          | character =>
              if isIdent character andalso
                 (index = start orelse
                  not (isIdent (String.sub (text, index - 1))))
              then loop (index + 1, SOME index)
              else loop (index + 1, last)
    in
      loop (start, NONE)
    end

  fun overrides checked path =
    case checked of
      NONE => []
    | SOME info =>
        let
          val fileId = fileIdOf info path
          val entry =
            case entryForFile (#sources info) fileId of
              SOME entry => entry
            | NONE => { id = 0, path = path, map = Source.create "" }
          val text = Source.text (#map entry)
          val bindings = #bindings (#resolution info)
          val uses = #uses (#resolution info)
            fun inFile span = Pos.file (#start span) = fileId
            fun bindingOffset binding =
              SOME (LspRename.bindingOffsets entry binding)
            val declarationEntries =
              List.mapPartial
                (fn binding =>
                  if not (inFile (#span binding)) then NONE
                  else if #kind binding = Resolve.Builtin orelse
                          #kind binding = Resolve.Constructor then NONE
                  else
                    case bindingOffset binding of
                      NONE => NONE
                    | SOME (start, _) =>
                        SOME (start, kindOfBinding (#kind binding)))
                bindings
            val useEntries =
              List.concat
                (List.map
                   (fn use =>
                     if not (inFile (#span use)) then []
                     else
                       case List.find
                              (fn binding => #id binding = #binding use)
                              bindings of
                         NONE => []
                       | SOME binding =>
                           let
                             val kind = kindOfBinding (#kind binding)
                             val offset =
                               case #kind use of
                                 Resolve.Call => callMemberOffset (text, #span use)
                               | Resolve.Read =>
                                   (case LspRename.useOffsets entry binding use of
                                      (start, _) => SOME start)
                           in
                             case offset of
                               SOME value => [(value, kind)]
                             | NONE => []
                           end)
                   uses)
        in
          declarationEntries @ useEntries
        end

  fun qualifierNames text =
    List.map #1 (LspCompletion.importsFromText text)

  fun importedModules state path text =
    List.mapPartial
      (fn (_, key) => LspCompletion.moduleFor state path key)
      (LspCompletion.importsFromText text)

  val builtinTypes =
    [ "int", "int1", "int2", "int4", "int8", "int16", "int32", "int64"
    , "float", "float32", "float64", "bool", "unit", "token", "resource"
    , "string", "array", "map"
    ]

  fun isBuiltinType name =
    List.exists (fn candidate => candidate = name) builtinTypes

  fun nameKinds state path program text =
    let
      val prelude =
        case LspCompletion.moduleFor state path "<prelude>" of
          SOME module => [module]
        | NONE => []
      val imported = prelude @ importedModules state path text
      val importedKinds =
        List.concat
          (List.map (fn module => programKinds (#program module)) imported)
      val builtinKinds =
        List.map (fn name => (name, functionT)) Check.builtins
    in
      makeNameTable (programKinds program @ importedKinds @ builtinKinds)
    end

  type item =
    { line : int
    , char : int
    , length : int
    , kind : int
    }

  fun mergeSort (compare : item * item -> bool) (items : item list) : item list =
    let
      fun merge ([], right) = right
        | merge (left, []) = left
        | merge (left as first :: rest, right as second :: more) =
            if compare (first, second) then
              first :: merge (rest, right)
            else second :: merge (left, more)
      fun split (left, right, []) = (left, right)
        | split (left, right, [item]) = (item :: left, right)
        | split (left, right, first :: second :: rest) =
            split (first :: left, second :: right, rest)
      fun sort [] = []
        | sort [item] = [item]
        | sort items =
            let val (left, right) = split ([], [], items)
            in merge (sort left, sort right) end
    in
      sort items
    end

  fun itemBefore (left : item, right : item) =
    #line left < #line right orelse
    (#line left = #line right andalso #char left <= #char right)

  fun lineText source line = Source.lineText (source, line + 1)

  fun spanItems (source, span, kind) : item list =
    let
      val startLine = Pos.line (#start span)
      val stopLine = Pos.line (#stop span)
      val startColumn = Pos.column (#start span)
      val stopColumn = Pos.column (#stop span)
      fun itemAt (line, byteStart, byteStop) =
        let
          val text = lineText source (line - 1)
          val startChar = byteColumnToCharacter (text, byteStart + 1)
          val stopChar = byteColumnToCharacter (text, byteStop + 1)
          val length = stopChar - startChar
        in
          if length <= 0 then NONE
          else
            SOME
              { line = line - 1
              , char = startChar
              , length = length
              , kind = kind
              }
        end
      fun loop (line, items) =
        if line > stopLine then List.rev items
        else
          let
            val text = lineText source (line - 1)
            val byteStart = if line = startLine then startColumn - 1 else 0
            val byteStop =
              if line = stopLine then
                Int.min (String.size text, Int.max (0, stopColumn - 1))
              else String.size text
          in
            case itemAt (line, byteStart, byteStop) of
              SOME item => loop (line + 1, item :: items)
            | NONE => loop (line + 1, items)
          end
    in
      loop (startLine, [])
    end

  fun tokenItem (source, span, kind) : item list =
    case spanItems (source, span, kind) of
      [item] => [item]
    | items => items

  fun punctuationKind text =
    case text of
      "+" => SOME operatorT
    | "-" => SOME operatorT
    | "*" => SOME operatorT
    | "/" => SOME operatorT
    | "%" => SOME operatorT
    | "^" => SOME operatorT
    | "<" => SOME operatorT
    | ">" => SOME operatorT
    | "=" => SOME operatorT
    | "<=" => SOME operatorT
    | ">=" => SOME operatorT
    | "<>" => SOME operatorT
    | "=>" => SOME operatorT
    | "->" => SOME operatorT
    | "|>" => SOME operatorT
    | "::" => SOME operatorT
    | "<<" => SOME operatorT
    | ">>" => SOME operatorT
    | "|" => SOME operatorT
    | "&" => SOME operatorT
    | "!" => SOME operatorT
    | _ => NONE

  fun isManualKeyword name =
    name = "import" orelse name = "as" orelse name = "Copy" orelse
    name = "Affine" orelse name = "Linear"

  fun itemsAt state uri =
    let
      val path = uriToPath uri
      val text =
        case findDoc state uri of
          SOME document => #text document
        | NONE => getOpt (Driver.readFile path, "")
      val source = Source.create text
      val lexed = Lexer.lex text
      val tokens = #tokens lexed
      val program = #1 (parsedOf state uri text)
      val checked = checkedOf state uri
      val entries = overrides checked path
      val offsets = makeOffsetTable entries
      val names = nameKinds state path program text
      val qualifiers = qualifierNames text
      fun isQualifier name =
        List.exists (fn candidate => candidate = name) qualifiers
      fun nextIsDot next =
        case next of
          SOME token =>
            (case #kind token of
               Token.Punct "." => true
             | _ => false)
        | NONE => false
      fun identKind (token, next) =
        let val name = case #kind token of Token.Ident value => value | _ => "" in
          case findOffset offsets (Pos.offset (#start (#span token))) of
            SOME kind => kind
          | NONE =>
              if isManualKeyword name then keywordT
              else if isQualifier name andalso nextIsDot next then namespaceT
              else if isBuiltinType name then typeT
              else
                (case findName names name of
                   SOME kind => kind
                 | NONE => variableT)
        end
      fun tokenKind (token, next) =
        case #kind token of
          Token.Keyword _ => SOME keywordT
        | Token.StringLit _ => SOME stringT
        | Token.IntLit _ => SOME numberT
        | Token.FloatLit _ => SOME numberT
        | Token.TyVar _ => SOME typeParameterT
        | Token.Ident _ => SOME (identKind (token, next))
        | Token.Punct value => punctuationKind value
        | Token.Eof => NONE
      fun loop (remaining, items) =
        case remaining of
          [] => items
        | token :: rest =>
            let
              val next =
                case rest of
                  candidate :: _ => SOME candidate
                | [] => NONE
              val items =
                case tokenKind (token, next) of
                  NONE => items
                | SOME kind =>
                    List.revAppend
                      (tokenItem (source, #span token, kind), items)
            in
              loop (rest, items)
            end
      val tokenItems = loop (tokens, [])
      val commentItems =
        List.concat
          (List.map
             (fn (span, _) => spanItems (source, span, commentT))
             (#comments lexed))
    in
      mergeSort itemBefore (tokenItems @ commentItems)
    end

  fun dataOfItems items =
    let
      val data = ref []
      fun emit (previousLine, previousChar, items : item list) =
        case items of
          [] => ()
        | item :: rest =>
            let
              val deltaLine = #line item - previousLine
              val deltaChar =
                if deltaLine = 0 then #char item - previousChar
                else #char item
              val _ =
                data :=
                  [deltaLine, deltaChar, #length item, #kind item, 0] :: !data
            in
              emit (#line item, #char item, rest)
            end
      val _ = emit (0, 0, items)
    in
      List.concat (List.rev (!data))
    end

  fun dataJson data = arr (List.map int data)

  fun fullResponse state uri =
    let
      val data = dataOfItems (itemsAt state uri)
      val resultId = freshSemanticId state
      val _ = storeSemanticTokens state uri resultId data
    in
      obj
        [ ("resultId", str (Int.toString resultId))
        , ("data", dataJson data)
        ]
    end

  fun semanticTokensAt state uri = fullResponse state uri

  fun semanticTokensRangeAt state uri startLine endLine =
    let
      val items =
        List.filter
          (fn item => #line item >= startLine andalso #line item <= endLine)
          (itemsAt state uri)
    in
      obj [ ("data", dataJson (dataOfItems items)) ]
    end

  fun groupCount data = List.length data div 5

  fun groupAt (data, index) =
    List.take (List.drop (data, index * 5), 5)

  fun sameGroup (left, right, index) =
    groupAt (left, index) = groupAt (right, index)

  fun commonPrefix (left, right, count, limit, index) =
    if index >= limit then count
    else if sameGroup (left, right, index) then
      commonPrefix (left, right, count + 1, limit, index + 1)
    else count

  fun commonSuffix (left, right, leftCount, rightCount, limit, index) =
    if index >= limit then index
    else if sameGroup (left, right, leftCount - index - 1) andalso
            sameGroup (left, right, rightCount - index - 1)
    then commonSuffix (left, right, leftCount, rightCount, limit, index + 1)
    else index

  fun deltaEdits (oldData, newData) =
    let
      val oldCount = groupCount oldData
      val newCount = groupCount newData
      val limit = Int.min (oldCount, newCount)
      val prefix = commonPrefix (oldData, newData, 0, limit, 0)
      val suffix =
        commonSuffix (oldData, newData, oldCount, newCount,
          limit - prefix, 0)
      val start = prefix * 5
      val deleteCount = (oldCount - prefix - suffix) * 5
      val inserted =
        List.take
          (List.drop (newData, prefix * 5), (newCount - prefix - suffix) * 5)
    in
      if deleteCount = 0 andalso List.null inserted then []
      else
        [ obj
            [ ("start", int start)
            , ("deleteCount", int deleteCount)
            , ("data", dataJson inserted)
            ]
        ]
    end

  fun semanticDeltaAt state uri previous =
    let val data = dataOfItems (itemsAt state uri) in
      case cachedSemanticTokens state uri of
        SOME (_, resultId, oldData) =>
          if Int.toString resultId <> previous then
            let
              val newId = freshSemanticId state
              val _ = storeSemanticTokens state uri newId data
            in
              obj
                [ ("resultId", str (Int.toString newId))
                , ("data", dataJson data)
                ]
            end
          else
            let val edits = deltaEdits (oldData, data) in
              if List.null edits then
                obj
                  [ ("resultId", str (Int.toString resultId))
                  , ("edits", arr [])
                  ]
              else
                let
                  val newId = freshSemanticId state
                  val _ = storeSemanticTokens state uri newId data
                in
                  obj
                    [ ("resultId", str (Int.toString newId))
                    , ("edits", arr edits)
                    ]
                end
            end
      | NONE =>
          let
            val newId = freshSemanticId state
            val _ = storeSemanticTokens state uri newId data
          in
            obj
              [ ("resultId", str (Int.toString newId))
              , ("data", dataJson data)
              ]
          end
    end
end
