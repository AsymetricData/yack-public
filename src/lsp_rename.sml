structure LspRename =
struct
  open LspProtocol
  open LspText
  open LspState

  type occurrence =
    { entry : Driver.sourceEntry
    , start : int
    , stop : int
    , declaration : bool
    }

  type target =
    { name : string
    , occurrences : occurrence list
    , at : int * int
    , crossFile : bool
    , modulePath : string option
    }

  fun isWordChar character =
    Char.isAlphaNum character orelse character = #"_" orelse character = #"'"

  fun spanOffsets (span : Diag.span) =
    (Pos.offset (#start span), Pos.offset (#stop span))

  fun cursorOffset (entry : Driver.sourceEntry) line character =
    let
      val text = Source.text (#map entry)
      val size = String.size text
      val lineText = Source.lineText (#map entry, line + 1)
      val byteColumn = characterToByteColumn (lineText, character)
      fun lineStart index currentLine =
        if currentLine >= line orelse index >= size then index
        else if String.sub (text, index) = #"\n" then
          lineStart (index + 1) (currentLine + 1)
        else lineStart (index + 1) currentLine
    in
      lineStart 0 0 + byteColumn
    end

  fun containsOffset cursor (start, stop) =
    start <= cursor andalso cursor <= stop

  fun findWordOffset (text, start, limit, word) =
    let
      val width = String.size word
      val size = String.size text
      val start = Int.max (0, start)
      val limit = Int.min (size, limit)
      fun boundaryBefore index =
        index = 0 orelse not (isWordChar (String.sub (text, index - 1)))
      fun boundaryAfter index =
        index + width >= size orelse not (isWordChar (String.sub (text, index + width)))
      fun loop index =
        if width = 0 orelse index + width > limit then NONE
        else if String.substring (text, index, width) = word andalso
                boundaryBefore index andalso boundaryAfter index then
          SOME index
        else loop (index + 1)
    in
      loop start
    end

  fun textOf entry = Source.text (#map entry)

  fun syntheticEntry entry = String.isPrefix "<" (#path entry)

  fun sourceName name =
    let
      fun lastAfter separators text =
        List.foldl
          (fn (separator, current) =>
            case List.rev (String.tokens (fn c => c = separator) current) of
              last :: _ => last
            | [] => current)
          text separators
    in
      lastAfter [#".", #"/"] name
    end

  fun identifierEnd (text, start) =
    let
      val size = String.size text
      fun loop index =
        if index < size andalso isWordChar (String.sub (text, index))
        then loop (index + 1) else index
    in
      loop start
    end

  fun lastIdentifierStart (text, from, limit, stopAtParen) =
    let
      val size = String.size text
      val limit = Int.min (size, limit)
      fun loop (index, last) =
        if index >= limit then last
        else
          case String.sub (text, index) of
            #"(" => if stopAtParen then last else loop (index + 1, last)
          | character =>
              if isWordChar character andalso
                 (index = from orelse
                  not (isWordChar (String.sub (text, index - 1))))
              then loop (index + 1, SOME index)
              else loop (index + 1, last)
    in
      loop (from, NONE)
    end

  fun sourceOffsetsOfBinding (entry, binding) =
    let
      val text = textOf entry
      val (start, stop) = spanOffsets (#span binding)
      val name = sourceName (#name binding)
      val width = String.size name
    in
      case findWordOffset (text, start, stop, name) of
        SOME offset => (offset, offset + width)
      | NONE => (start, Int.min (stop, start + width))
    end

  fun sourceOffsetsOfUse (entry, binding, use) =
    let
      val text = textOf entry
      val (start, stop) = spanOffsets (#span use)
      val member =
        case #kind use of
          Resolve.Call => lastIdentifierStart (text, start, stop, true)
        | Resolve.Read => lastIdentifierStart (text, start, stop, false)
      val start = case member of SOME offset => offset | NONE => start
    in
      (start, identifierEnd (text, start))
    end

  fun bindingOffsets entry binding = sourceOffsetsOfBinding (entry, binding)
  fun useOffsets entry binding use = sourceOffsetsOfUse (entry, binding, use)

  fun entryForSpan (checked : Driver.checked) span =
    entryForFile (#sources checked) (Pos.file (#start span))

  fun occurrencesOfBinding (checked : Driver.checked, binding) =
    let
      val id = #id binding
      val declaration =
        case entryForSpan checked (#span binding) of
          NONE => []
        | SOME entry =>
            if syntheticEntry entry then []
            else
              let val (start, stop) = sourceOffsetsOfBinding (entry, binding)
              in [{entry = entry, start = start, stop = stop, declaration = true}]
              end
      val uses =
        List.mapPartial
          (fn use =>
            if #binding use <> id then NONE
            else
              case entryForSpan checked (#span use) of
                NONE => NONE
              | SOME entry =>
                  if syntheticEntry entry then NONE
                  else
                    let
                      val (start, stop) =
                        sourceOffsetsOfUse (entry, binding, use)
                    in
                      SOME
                        { entry = entry
                        , start = start
                        , stop = stop
                        , declaration = false
                        }
                    end)
          (#uses (#resolution checked))
    in
      declaration @ uses
    end

  fun bindingAt (checked : Driver.checked, entry, fileId, cursor) =
    let
      val bindings = #bindings (#resolution checked)
      val uses = #uses (#resolution checked)
      fun inFile span = Pos.file (#start span) = fileId
      fun editable binding =
        case entryForSpan checked (#span binding) of
          SOME entry => not (syntheticEntry entry)
        | NONE => false
      fun atBinding binding =
        if not (inFile (#span binding)) orelse
           not (editable binding)
        then NONE
        else
          let val offsets = sourceOffsetsOfBinding (entry, binding) in
            if containsOffset cursor offsets then SOME binding else NONE
          end
      fun atUse use =
        if not (inFile (#span use)) then NONE
        else
          case List.find (fn binding => #id binding = #binding use) bindings of
            NONE => NONE
          | SOME binding =>
              if not (editable binding) then NONE
              else
                let
                  val offsets = sourceOffsetsOfUse (entry, binding, use)
                in
                  if containsOffset cursor offsets then SOME binding else NONE
                end
      fun firstSome [] = NONE
        | firstSome (value :: rest) =
            (case value of SOME found => SOME found | NONE => firstSome rest)
    in
      firstSome (List.map atBinding bindings @ List.map atUse uses)
    end

  fun modulePathOfBinding binding =
    let
      val parts = String.tokens (fn character => character = #".") (#name binding)
    in
      case parts of
        first :: _ =>
          if String.isPrefix "$" first then
            let
              val keyParts = List.take (parts, List.length parts - 1)
              val key = String.concatWith "." keyParts
              val key =
                String.extract (key, 1, NONE)
                handle _ => key
            in
              SOME (String.map (fn #"." => #"/" | character => character) key)
            end
          else NONE
      | [] => NONE
    end

  fun valueTarget (checked, entry, fileId, cursor) =
    case bindingAt (checked, entry, fileId, cursor) of
      NONE => NONE
      | SOME binding =>
          let
            val occurrences = occurrencesOfBinding (checked, binding)
            val path = #path entry
            val current =
              List.filter
                (fn occurrence => #path (#entry occurrence) = path)
                occurrences
            fun containing (occurrence : occurrence) =
              containsOffset cursor
                (#start occurrence, #stop occurrence)
            val at =
              case List.find containing current of
                SOME occurrence => (#start occurrence, #stop occurrence)
              | NONE =>
                  (case current of
                     occurrence :: _ =>
                       (#start occurrence, #stop occurrence)
                   | [] => (cursor, cursor))
            val name =
              case occurrences of
                occurrence :: _ =>
                  let
                    val text = textOf (#entry occurrence)
                    val width = #stop occurrence - #start occurrence
                  in
                    String.substring (text, #start occurrence, width)
                  end
              | [] => sourceName (#name binding)
          in
            SOME
              { name = name
              , occurrences = occurrences
              , at = at
              , crossFile = true
              , modulePath = modulePathOfBinding binding
              }
          end

  fun declaredTypeNames program fileId =
    List.foldl
      (fn (declaration, names) =>
        case declaration of
          Ast.DType {name, span, ...} =>
            if Pos.file (#start span) = fileId then name :: names else names
        | Ast.DAlias {name, span, ...} =>
            if Pos.file (#start span) = fileId then name :: names else names
        | Ast.DDatatype {name, span, ...} =>
            if Pos.file (#start span) = fileId then name :: names else names
        | _ => names)
      []
      program

  fun identifierTokens text =
    let val lexed = Lexer.lex text in
      List.mapPartial
        (fn token =>
          case #kind token of
            Token.Ident name => SOME (name, spanOffsets (#span token))
          | _ => NONE)
        (#tokens lexed)
    end

  fun typeTarget checked entry fileId cursor =
    let
      val program = #program checked
      val names = declaredTypeNames program fileId
      val tokens = identifierTokens (textOf entry)
      fun isTypeName name = List.exists (fn candidate => candidate = name) names
      fun cursorToken () =
        List.find
          (fn (name, offsets) =>
            isTypeName name andalso containsOffset cursor offsets)
          tokens
    in
      case cursorToken () of
        NONE => NONE
      | SOME (name, at) =>
          let
            val bindings = #bindings (#resolution checked)
            val namedBindings =
              List.filter
                (fn binding =>
                  #name binding = name andalso
                  Pos.file (#start (#span binding)) = fileId)
                bindings
            val excluded =
              List.map (fn binding => sourceOffsetsOfBinding (entry, binding)) namedBindings @
              List.map
                (fn use =>
                  case List.find
                         (fn binding => #id binding = #binding use)
                         namedBindings of
                    SOME binding => sourceOffsetsOfUse (entry, binding, use)
                  | NONE => (0, 0))
                (List.filter
                   (fn use => Pos.file (#start (#span use)) = fileId)
                   (#uses (#resolution checked)))
            fun overlaps (start, stop) =
              List.exists
                (fn (otherStart, otherStop) =>
                  start < otherStop andalso otherStart < stop)
                excluded
            val spans =
              List.mapPartial
                (fn (candidate, offsets) =>
                  if candidate = name andalso not (overlaps offsets)
                  then SOME offsets else NONE)
                tokens
          in
            if List.null spans then NONE
            else
              let
                val occurrences =
                  List.map
                    (fn (start, stop) =>
                      { entry = entry
                      , start = start
                      , stop = stop
                      , declaration = false
                      })
                    (spansOf spans)
              in
                SOME
                  { name = name
                  , occurrences = occurrences
                  , at = at
                  , crossFile = false
                  , modulePath = NONE
                  }
              end
          end
    end

  and spansOf spans = dedupe (sortSpans spans)

  and sortSpans spans =
    let
      fun insert ((start, stop), []) = [(start, stop)]
        | insert ((start, stop), (other as (otherStart, _)) :: rest) =
            if start <= otherStart then (start, stop) :: other :: rest
            else other :: insert ((start, stop), rest)
    in
      List.foldl (fn (span, sorted) => insert (span, sorted)) [] spans
    end

  and dedupe [] = []
    | dedupe [span] = [span]
    | dedupe (first :: second :: rest) =
        if first = second then dedupe (second :: rest)
        else first :: dedupe (second :: rest)

  fun targetAt checked entry fileId line character =
    let val cursor = cursorOffset entry line character in
      case valueTarget (checked, entry, fileId, cursor) of
        SOME target => SOME target
      | NONE => typeTarget checked entry fileId cursor
    end

  fun validName name =
    let val size = String.size name in
      size > 0 andalso
      (Char.isAlpha (String.sub (name, 0)) orelse
       String.sub (name, 0) = #"_") andalso
      CharVector.all isWordChar name
    end

  fun textEdit entry ((start, stop), newName) =
    obj
      [ ("range",
          obj
            [ ("start", lspPositionOfOffset entry start)
            , ("end", lspPositionOfOffset entry stop)
            ])
      , ("newText", str newName)
      ]

  fun prepareRenameAt state uri line character =
    case checkedOf state uri of
      NONE => jsonNull
    | SOME checked =>
        let
          val path = uriToPath uri
          val fileId = fileIdOf checked path
        in
          case entryForFile (#sources checked) fileId of
            NONE => jsonNull
          | SOME entry =>
              (case targetAt checked entry fileId line character of
                 NONE => jsonNull
               | SOME target =>
                   let val (start, stop) = #at target in
                     obj
                       [ ("range",
                           obj
                             [ ("start", lspPositionOfOffset entry start)
                             , ("end", lspPositionOfOffset entry stop)
                             ])
                       , ("placeholder", str (#name target))
                       ]
                   end)
        end

  fun importLines text =
    let
      val size = String.size text
      fun loop (start, acc) =
        if start >= size then List.rev acc
        else
          let
            fun newline index =
              if index >= size then size
              else if String.sub (text, index) = #"\n" then index
              else newline (index + 1)
            val stop = newline start
            val line = String.substring (text, start, stop - start)
          in
            loop (stop + 1, (start, line) :: acc)
          end
    in
      loop (0, [])
    end

  fun trimSubstring (text, start, stop) =
    let
      fun left index =
        if index < stop andalso Char.isSpace (String.sub (text, index))
        then left (index + 1) else index
      fun right index =
        if index >= start andalso Char.isSpace (String.sub (text, index))
        then right (index - 1) else index
      val start = left start
      val stop = right (stop - 1) + 1
    in
      if stop <= start then "" else String.substring (text, start, stop - start)
    end

  fun importPathAndItems line =
    let
      val text = trim line
    in
      if not (String.isPrefix "import " text) then NONE
      else
        let
          val rest = trim (String.extract (text, 7, NONE))
          val size = String.size rest
          fun stopAt index =
            if index >= size then index
            else
              let val character = String.sub (rest, index) in
                if Char.isSpace character orelse character = #"{"
                then index else stopAt (index + 1)
              end
          val stop = stopAt 0
          val path = String.substring (rest, 0, stop)
          val path =
            if String.isSuffix "." path then
              String.substring (path, 0, String.size path - 1)
            else path
          fun find index character =
            if index >= size then NONE
            else if String.sub (rest, index) = character then SOME index
            else find (index + 1) character
        in
          if path = "" then NONE
          else
            case find 0 #"{" of
              NONE => NONE
            | SOME openIndex =>
                (case find (openIndex + 1) #"}" of
                   NONE => NONE
                 | SOME closeIndex =>
                     SOME (path, rest, openIndex + 1, closeIndex))
        end
    end

  fun importItemOffset (text, name, expected) =
    let
      fun inLine (lineOffset, line) =
        case importPathAndItems line of
          NONE => NONE
        | SOME (path, rest, openIndex, closeIndex) =>
            if (case expected of SOME value => value <> path | NONE => false)
            then NONE
            else
              let
                fun itemOffset (start, stop) =
                  let
                    val item = trimSubstring (rest, start, stop)
                    val words = String.tokens Char.isSpace item
                    val original =
                      case words of
                        first :: _ => first
                      | [] => ""
                  in
                    if original <> name then NONE
                    else
                      let
                        fun leading index =
                          if index < stop andalso
                             Char.isSpace (String.sub (rest, index))
                          then leading (index + 1) else index
                        val start = leading start
                      in
                        SOME (lineOffset + openIndex + start)
                      end
                  end
                fun loop (start, acc) =
                  let
                    fun findComma index =
                      if index >= closeIndex then NONE
                      else if String.sub (rest, index) = #","
                      then SOME index
                      else findComma (index + 1)
                  in
                    case findComma start of
                      NONE =>
                        (case acc of
                           SOME offset => SOME offset
                         | NONE => itemOffset (start, closeIndex))
                    | SOME comma =>
                        (case acc of
                           SOME offset => SOME offset
                         | NONE => loop (comma + 1, itemOffset (start, comma)))
                  end
              in
                loop (openIndex, NONE)
              end
    in
      let
        fun walk [] = NONE
          | walk ((offset, line) :: rest) =
              (case inLine (offset, line) of
                 SOME found => SOME found
               | NONE => walk rest)
      in
        walk (importLines text)
      end
    end

  fun selectiveAlias (text, name) =
    let
      fun inLine line =
        case importPathAndItems line of
          NONE => NONE
        | SOME (_, rest, openIndex, closeIndex) =>
            let
              val inner =
                String.substring (rest, openIndex, closeIndex - openIndex)
              val items =
                String.fields (fn character => character = #",") inner
              fun aliasOf item =
                case String.tokens Char.isSpace item of
                  [original] => if original = name then SOME original else NONE
                | original :: "as" :: alias :: _ =>
                    if original = name then SOME alias else NONE
                | _ => NONE
            in
              case List.find Option.isSome (List.map aliasOf items) of
                SOME (SOME alias) => SOME alias
              | _ => NONE
            end
    in
      case List.find Option.isSome
             (List.map inLine
                (String.fields (fn character => character = #"\n") text)) of
        SOME (SOME alias) => SOME alias
      | _ => NONE
    end

  fun importEditsFor (checked : Driver.checked, target : target, name) =
    let
      val expected = #modulePath target
      val usePaths =
        List.foldl
          (fn (occurrence : occurrence, paths) =>
            let val path = #path (#entry occurrence) in
              if List.exists (fn candidate => candidate = path) paths then paths
              else path :: paths
            end)
          []
          (#occurrences target)
      fun allowed entry =
        if syntheticEntry entry then false
        else
          case expected of
            SOME _ => true
            | NONE =>
                List.exists (fn path => path = #path entry) usePaths
      val edits =
        List.foldl
          (fn (entry, acc) =>
            if not (allowed entry) then acc
            else
              case importItemOffset (textOf entry, name, expected) of
                SOME offset =>
                  (#path entry, offset, offset + String.size name) :: acc
              | NONE => acc)
          []
          (#sources checked)
    in
      edits
    end

  fun renameAt state uri line character newName =
    if not (validName newName) then jsonNull
    else
      case checkedOf state uri of
        NONE => jsonNull
      | SOME checked =>
          let
            val path = uriToPath uri
            val fileId = fileIdOf checked path
          in
            case entryForFile (#sources checked) fileId of
              NONE => jsonNull
            | SOME entry =>
                (case targetAt checked entry fileId line character of
                   NONE => jsonNull
                 | SOME target =>
                     let
                       val name = #name target
                       fun tokenOf occurrence =
                         let
                           val text = textOf (#entry occurrence)
                           val width = #stop occurrence - #start occurrence
                         in
                           String.substring (text, #start occurrence, width)
                         end
                       fun keepAlias occurrence =
                         if #declaration occurrence then false
                         else
                           let
                             val text = textOf (#entry occurrence)
                             val token = tokenOf occurrence
                           in
                             case selectiveAlias (text, name) of
                               SOME alias => alias <> name andalso token = alias
                             | NONE => false
                           end
                       val occurrenceEdits =
                         List.foldl
                           (fn (occurrence : occurrence, edits) =>
                             if keepAlias occurrence then edits
                             else
                               (#path (#entry occurrence),
                                #start occurrence,
                                #stop occurrence) :: edits)
                           []
                           (#occurrences target)
                       val importEdits =
                         if #crossFile target then
                           importEditsFor (checked, target, name)
                         else []
                       val edits = occurrenceEdits @ importEdits
                       fun dedupe [] = []
                         | dedupe (edit :: rest) =
                             let
                               fun same (path, start) =
                                 let val (otherPath, otherStart, _) = edit in
                                   path = otherPath andalso start = otherStart
                                 end
                               val (path, start, _) = edit
                             in
                               edit ::
                               dedupe
                                 (List.filter
                                    (fn candidate => not (same
                                       (let val (p, s, _) = candidate in (p, s) end)))
                                    rest)
                             end
                       val edits = dedupe edits
                       fun entryOfPath candidate =
                         List.find (fn entry => #path entry = candidate)
                           (#sources checked)
                       fun group (edits, groups) =
                         case edits of
                           [] => groups
                         | (editPath, start, stop) :: rest =>
                             let
                               fun add [] = [(editPath, [(start, stop)])]
                                 | add ((candidate, items) :: others) =
                                     if candidate = editPath then
                                       (candidate, (start, stop) :: items) ::
                                       others
                                     else
                                       (candidate, items) :: add others
                             in
                               group (rest, add groups)
                             end
                       val groups = group (edits, [])
                       val changes =
                         List.mapPartial
                           (fn (groupPath, items) =>
                             case entryOfPath groupPath of
                               NONE => NONE
                               | SOME sourceEntry =>
                                   SOME
                                     ( uriOfPath state groupPath
                                     , arr
                                         (List.map
                                            (fn offsets =>
                                              textEdit sourceEntry
                                                (offsets, newName))
                                            items)
                                     ))
                           groups
                     in
                       obj [ ("changes", obj changes) ]
                     end)
          end

end
