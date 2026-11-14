structure LspActions =
struct
  open LspProtocol
  open LspText
  open LspAst
  open LspState

  datatype bindingShape =
      TupleShape of int
    | RecordShape of string list

  fun indentAt entry (position : Pos.t) =
    let
      val text = Source.lineText (#map entry, Pos.line position)
      val size = String.size text
      fun loop index =
        if index < size andalso
           (String.sub (text, index) = #" " orelse String.sub (text, index) = #"\t")
        then loop (index + 1)
        else index
      val count = loop 0
    in
      String.substring (text, 0, count)
    end

  fun typeAtSpan checked span =
    case List.find (fn (candidate, _) => sameSpan (candidate, span))
             (#expressionTypes checked) of
      SOME (_, ty) => SOME ty
    | NONE => NONE

  fun placeholderFor ty =
    case ty of
      NONE => "()"
    | SOME value =>
        let val text = Types.toString value in
          if text = "unit" then "()"
          else if text = "bool" then "false"
          else if String.isPrefix "int" text then "0"
          else if String.isPrefix "float" text then "0.0"
          else if text = "string" then "\"\""
          else if String.isSuffix " option" text then "None"
          else if String.isSuffix " list" text then "Nil"
          else "()"
        end

  fun textEdit entry startOffset stopOffset newText =
    obj
      [ ("range",
          obj
            [ ("start", lspPositionOfOffset entry startOffset)
            , ("end", lspPositionOfOffset entry stopOffset)
            ])
      , ("newText", str newText)
      ]

  fun replaceInSpan entry span needle replacement =
    let
      val text = Source.text (#map entry)
      val startOffset = Pos.offset (#start span)
      val stopOffset = Pos.offset (#stop span)
      val available = Int.max (0, Int.min (String.size text, stopOffset) - startOffset)
      val chunk = String.substring (text, startOffset, available)
      fun search index =
        if index + String.size needle > String.size chunk then NONE
        else if String.substring (chunk, index, String.size needle) = needle then
          SOME index
        else search (index + 1)
    in
      case search 0 of
        NONE => NONE
      | SOME index =>
          SOME (textEdit entry (startOffset + index)
                  (startOffset + index + String.size needle) replacement)
    end

  fun quotedName message =
    let
      val size = String.size message
      fun close start index =
        if index >= size then NONE
        else if String.sub (message, index) = #"`" then
          SOME (String.substring (message, start, index - start))
        else close start (index + 1)
      fun find index =
        if index >= size then NONE
        else if String.sub (message, index) = #"`" then
          close (index + 1) (index + 1)
        else find (index + 1)
    in
      find 0
    end

  fun replaceAll (text, needle, replacement) =
    let
      val size = String.size text
      val width = String.size needle
      fun loop (index, chunks) =
        if index + width > size then
          String.concat (List.rev (String.extract (text, index, NONE) :: chunks))
        else if String.substring (text, index, width) = needle then
          loop (index + width, replacement :: chunks)
        else
          loop (index + 1,
                String.substring (text, index, 1) :: chunks)
    in
      if width = 0 then text else loop (0, [])
    end

  fun editDistance (left, right) =
    let
      val leftSize = String.size left
      val rightSize = String.size right
      val previous = Array.tabulate (rightSize + 1, fn index => index)
      fun row (line, previous) =
        if line > leftSize then previous
        else
          let
            val current = Array.array (rightSize + 1, 0)
            val _ = Array.update (current, 0, line)
            val leftChar = String.sub (left, line - 1)
            fun column (index, previous) =
              if index > rightSize then current
              else
                let
                  val cost =
                    if leftChar = String.sub (right, index - 1) then 0 else 1
                  val deletion = Array.sub (previous, index) + 1
                  val insertion = Array.sub (current, index - 1) + 1
                  val substitution = Array.sub (previous, index - 1) + cost
                  val _ = Array.update
                    (current, index,
                     Int.min (deletion, Int.min (insertion, substitution)))
                in
                  column (index + 1, previous)
                end
            val current = column (1, previous)
          in
            row (line + 1, current)
          end
      val result = row (1, previous)
    in
      Array.sub (result, rightSize)
    end

  fun lastImportOffset text =
    let
      val size = String.size text
      fun loop (start, last) =
        if start >= size then last
        else
          let
            fun newline index =
              if index >= size then size
              else if String.sub (text, index) = #"\n" then index
              else newline (index + 1)
            val stop = newline start
            val line = String.substring (text, start, stop - start)
            val trimmed = trim line
            val last =
              if String.isPrefix "import " trimmed then
                Int.min (size, stop + 1)
              else last
          in
            loop (stop + 1, last)
          end
    in
      loop (0, 0)
    end

  fun qualifierUsable (text, key) =
    let
      fun lineInfo line =
        let val trimmed = trim line in
          if not (String.isPrefix "import " trimmed) then NONE
          else
            let
              val rest = trim (String.extract (trimmed, 7, NONE))
              val size = String.size rest
              fun pathEnd index =
                if index >= size then index
                else
                  let val character = String.sub (rest, index) in
                    if Char.isSpace character orelse character = #"{"
                    then index else pathEnd (index + 1)
                  end
              val stop = pathEnd 0
              val path = String.substring (rest, 0, stop)
            in
              if path = key then
                let
                  fun hasChar character =
                    let
                      fun loop index =
                        if index >= size then false
                        else if String.sub (rest, index) = character then true
                        else loop (index + 1)
                    in
                      loop 0
                    end
                  val aliasPresent =
                    let
                      fun loop index =
                        if index + 4 > size then false
                        else if String.substring (rest, index, 4) = " as "
                        then true
                        else loop (index + 1)
                    in
                      loop 0
                    end
                in
                  SOME (not (hasChar #"{") orelse aliasPresent)
                end
              else NONE
            end
        end
      fun firstSome [] = NONE
        | firstSome (value :: rest) =
            (case value of SOME found => SOME found | NONE => firstSome rest)
    in
      case firstSome
             (List.map lineInfo
                (String.fields (fn character => character = #"\n") text)) of
        SOME usable => usable
      | NONE => false
    end

  fun wordInSpan (text, span : Diag.span, word) =
    let
      val size = String.size text
      val width = String.size word
      val start = Int.max (0, Pos.offset (#start span))
      val limit = Int.min (size, Pos.offset (#stop span))
      fun isWord character =
        Char.isAlphaNum character orelse character = #"_" orelse
        character = #"'"
      fun boundaryBefore index =
        index = 0 orelse not (isWord (String.sub (text, index - 1)))
      fun boundaryAfter index =
        index + width >= size orelse
        not (isWord (String.sub (text, index + width)))
      fun loop index =
        if width = 0 orelse index + width > limit then NONE
        else if String.substring (text, index, width) = word andalso
                boundaryBefore index andalso boundaryAfter index
        then SOME index else loop (index + 1)
    in
      loop start
    end

  fun declarationAt program fileId line column =
    List.find
      (fn declaration =>
        let val span = Ast.declSpan declaration in
          Pos.file (#start span) = fileId andalso
          spanContains span (line, column)
        end)
      program

  fun callbackCallAt program fileId line column code =
    let
      fun inFile span = Pos.file (#start span) = fileId
      fun argumentMatches argument =
        case code of
          "E0512" =>
            (case argument of
               Ast.EFn (_, _, _, span) => spanContains span (line, column)
             | _ => false)
        | _ => spanContains (Ast.exprSpan argument) (line, column)
      fun findArgument (index, arguments) =
        case arguments of
          [] => NONE
        | argument :: rest =>
            if argumentMatches argument then SOME index
            else findArgument (index + 1, rest)
      fun firstOf values =
        case List.find Option.isSome values of
          SOME (SOME found) => SOME found
        | _ => NONE
      fun walk expression =
        let val span = Ast.exprSpan expression in
          if inFile span andalso spanContains span (line, column) then
            case expression of
              Ast.ECall (name, arguments, _) =>
                (case walkChildren expression of
                   SOME found => SOME found
                 | NONE =>
                     (case findArgument (0, arguments) of
                        SOME index => SOME (name, index)
                      | NONE => NONE))
            | _ => walkChildren expression
          else NONE
        end
      and walkChildren expression =
        case expression of
          Ast.ELet (bindings, body, _) =>
            firstOf
              (List.map
                 (fn binding =>
                   case binding of
                     Ast.BVal {value, ...} => walk value
                   | Ast.BPat {value, ...} => walk value
                   | Ast.BFun {body, ...} => walk body
                   | Ast.BGuard {value, fallback, ...} =>
                       firstOf [walk value, walk fallback])
                 bindings @ [walk body])
        | Ast.EArena (bindings, body, _) =>
            firstOf
              (List.map
                 (fn binding =>
                   case binding of
                     Ast.BVal {value, ...} => walk value
                   | Ast.BPat {value, ...} => walk value
                   | Ast.BFun {body, ...} => walk body
                   | Ast.BGuard {value, fallback, ...} =>
                       firstOf [walk value, walk fallback])
                 bindings @ [walk body])
        | Ast.ECase (scrutinee, branches, _) =>
            firstOf
              (walk scrutinee ::
               List.map
                 (fn (_, guard, body) =>
                   case guard of
                     SOME condition =>
                       (case walk condition of
                          SOME found => SOME found
                        | NONE => walk body)
                   | NONE => walk body)
                 branches)
        | Ast.EIf (condition, thenBranch, elseBranch, _) =>
            firstOf [walk condition, walk thenBranch, walk elseBranch]
        | Ast.EBin (_, left, right, _) => firstOf [walk left, walk right]
        | Ast.ENeg (operand, _) => walk operand
        | Ast.ECall (_, arguments, _) => firstOf (List.map walk arguments)
        | Ast.ERecord (fields, _) =>
            firstOf (List.map (fn (_, value) => walk value) fields)
        | Ast.EField (_, base, _) => walk base
        | Ast.ETuple (values, _) => firstOf (List.map walk values)
        | Ast.ETupleIndex (_, base, _) => walk base
        | Ast.EBorrow (_, _, operand, _) => walk operand
        | Ast.EFn (_, _, body, _) => walk body
        | _ => NONE
    in
      firstOf
        (List.map
           (fn declaration =>
             case declaration of
               Ast.DFun {body, ...} => walk body
             | Ast.DFunClause {body, ...} => walk body
             | _ => NONE)
           program)
    end

  fun findFunction program name =
    List.find
      (fn declaration =>
        case declaration of
          Ast.DFun {name = candidate, ...} => candidate = name
        | _ => false)
      program

  fun consumeEdit entry declaration index =
    case declaration of
      Ast.DFun {params, body, span, ...} =>
        if index < 0 orelse index >= List.length params then NONE
        else
          let
            val {name, annot, ...} = List.nth (params, index)
            val needsConsume =
              case annot of
                SOME (Ast.TyFun (first :: _, _)) =>
                  (case first of
                     Ast.TyConsume (_, _) => false
                   | _ => true)
              | _ => false
          in
            if not needsConsume then NONE
            else
              let
                val text = Source.text (#map entry)
                val headerStart = Pos.offset (#start span)
                val headerStop =
                  Int.min
                    (Pos.offset (#start (Ast.exprSpan body)),
                     Pos.offset (#stop span))
                val nameSize = String.size name
                fun isWordChar character =
                  Char.isAlphaNum character orelse character = #"_" orelse
                  character = #"'"
                fun boundaryBefore offset =
                  offset = 0 orelse not (isWordChar (String.sub (text, offset - 1)))
                fun boundaryAfter offset =
                  offset >= String.size text orelse
                  not (isWordChar (String.sub (text, offset)))
                fun skipSpace offset =
                  if offset < headerStop andalso
                     Char.isSpace (String.sub (text, offset))
                  then skipSpace (offset + 1)
                  else offset
                fun search offset =
                  if offset + nameSize > headerStop then NONE
                  else if String.substring (text, offset, nameSize) = name andalso
                          boundaryBefore offset andalso
                          boundaryAfter (offset + nameSize) then
                    let
                      val colon = skipSpace (offset + nameSize)
                    in
                      if colon < headerStop andalso
                         String.sub (text, colon) = #":" then
                        let
                          val after = skipSpace (colon + 1)
                          val typeStart =
                            if after < headerStop andalso
                               String.sub (text, after) = #"("
                            then after + 1
                            else after
                        in
                          SOME typeStart
                        end
                      else search (offset + 1)
                    end
                  else search (offset + 1)
              in
                case search headerStart of
                  SOME offset => SOME (textEdit entry offset offset "consume ")
                | NONE => NONE
              end
          end
    | _ => NONE

  fun codeActionsAt state uri range contextDiagnostics =
    let
      val path = uriToPath uri
      val checkedOption = checkedOf state uri
      val document = findDoc state uri
      val program =
        case checkedOption of
          SOME checked => #program checked
        | NONE =>
            (case document of
               SOME doc =>
                 (case desugarText (#text doc) of
                    SOME program => program
                  | NONE => [])
             | NONE => [])
      val modules = indexFor state path
      val documentText =
        case document of SOME doc => #text doc | NONE => ""
      val tolerantProgram = programOfText documentText
      val db = LspTypes.dbOf modules tolerantProgram
      val fileId =
        case checkedOption of
          SOME checked => fileIdOf checked path
        | NONE => 0
      val sourceEntry =
        case checkedOption of
          SOME checked => entryForFile (#sources checked) fileId
        | NONE =>
            (case document of
               SOME doc =>
                 SOME { id = 0, path = path, map = Source.create (#text doc) }
             | NONE => NONE)
      fun rangeStart range =
        case member "start" range of
          SOME start =>
            (case (member "line" start, member "character" start) of
               (SOME line, SOME character) => (intOf line, intOf character)
             | _ => (0, 0))
        | NONE => (0, 0)
      fun diagnosticCode diagnostic =
        case member "code" diagnostic of
          SOME (Json.String code) => code
          | SOME (Json.Number code) => code
          | _ => ""
      fun diagnosticMessage diagnostic =
        case member "message" diagnostic of
          SOME (Json.String text) => text
        | _ => ""
      fun findSubstring (needle, text) =
        let
          val size = String.size text
          val width = String.size needle
          fun loop index =
            if index + width > size then NONE
            else if String.substring (text, index, width) = needle then SOME index
            else loop (index + 1)
        in
          if width = 0 then SOME 0 else loop 0
        end
      fun missingConstructors diagnostic =
        let
          val message = diagnosticMessage diagnostic
          val marker = "missing constructors: "
        in
          case findSubstring (marker, message) of
            NONE => []
          | SOME index =>
              let
                val rest =
                  String.extract (message, index + String.size marker, NONE)
                val stop =
                  case findSubstring ("\n", rest) of
                    SOME newline => newline
                  | NONE => String.size rest
                val names = String.substring (rest, 0, stop)
              in
                List.map trim
                  (List.filter (fn name => name <> "")
                     (String.fields (fn character => character = #",") names))
              end
        end
      fun codeActionMany title diagnostic edits =
        obj
          [ ("title", str title)
          , ("kind", str "quickfix")
          , ("diagnostics", arr [diagnostic])
          , ("edit", obj [ ("changes", obj [ (uri, arr edits) ]) ])
          ]
      fun codeAction title diagnostic edit =
        codeActionMany title diagnostic [edit]
      fun literalPlaceholder expression =
        case expression of
          Ast.EInt _ => SOME "0"
        | Ast.ENeg (Ast.EInt _, _) => SOME "0"
        | Ast.EFloat _ => SOME "0.0"
        | Ast.EString _ => SOME "\"\""
        | Ast.EBool _ => SOME "false"
        | Ast.EUnit _ => SOME "()"
        | _ => NONE
      fun casePlaceholder expression =
        case (checkedOption, expression) of
          (SOME checked, Ast.ECase (_, _, span)) =>
            placeholderFor (typeAtSpan checked span)
        | (NONE, Ast.ECase (_, branches, _)) =>
            (case List.mapPartial (fn (_, _, body) => literalPlaceholder body) branches of
               [] => "()"
             | first :: rest =>
                 if List.all (fn value => value = first) rest then first else "()")
        | _ => "()"
      fun actionsFor entry diagnostic =
        let
          val code = diagnosticCode diagnostic
          val (line0, character0) =
            case member "range" diagnostic of
              SOME value => rangeStart value
            | NONE => (0, 0)
          val line = line0 + 1
          val lineText = Source.lineText (#map entry, line)
          val column = characterToByteColumn (lineText, character0) + 1
      val cursor = offsetOfPosition (documentText, line0, character0)
      fun diagnosticRange diagnostic =
        case member "range" diagnostic of
          SOME value =>
            let
              fun positionOf field fallback =
                case member field value of
                  SOME position =>
                    (case (member "line" position, member "character" position) of
                       (SOME line, SOME character) => (intOf line, intOf character)
                     | _ => fallback)
                  | NONE => fallback
              val (startLine, startCharacter) = positionOf "start" (0, 0)
              val (endLine, endCharacter) =
                positionOf "end" (startLine, startCharacter)
            in
              ( offsetOfPosition (documentText, startLine, startCharacter)
              , offsetOfPosition (documentText, endLine, endCharacter)
              )
            end
        | NONE => (0, 0)
      fun rangeText diagnostic =
        let
          val (start, stop) = diagnosticRange diagnostic
          val size = String.size documentText
          val stop = Int.max (start, Int.min (size, stop))
        in
          String.substring (documentText, start, stop - start)
        end
      fun nameOfDiagnostic diagnostic =
        case quotedName (diagnosticMessage diagnostic) of
          SOME name => name
        | NONE => trim (rangeText diagnostic)
      fun lower text = String.map Char.toLower text
      fun take count values =
        if count <= 0 then []
        else
          case values of
            [] => []
          | value :: rest => value :: take (count - 1) rest
      fun sortPairs [] = []
        | sortPairs (pair :: rest) =
            let
              fun insert (value, []) = [value]
                | insert ((distance, name), (other as (otherDistance, otherName)) :: tail) =
                    if distance < otherDistance orelse
                       (distance = otherDistance andalso name <= otherName)
                    then (distance, name) :: other :: tail
                    else other :: insert ((distance, name), tail)
            in
              insert (pair, sortPairs rest)
            end
      fun candidateNames () =
        let
          val locals =
            case LspAst.bindingsAt (tolerantProgram, cursor) of
              SOME bindings => List.map #name bindings
            | NONE => []
          val own = List.map #2 (exportsOf program)
          val prelude =
            case LspCompletion.moduleFor state path "<prelude>" of
              SOME module => List.map #2 (#exports module)
            | NONE => []
          val imported =
            List.concat
              (List.map
                 (fn (_, key) =>
                   case LspCompletion.moduleFor state path key of
                     SOME module => List.map #2 (#exports module)
                   | NONE => [])
                 (LspCompletion.importsFromText documentText))
          val all = locals @ own @ prelude @ imported @ Check.builtins
          fun member name [] = false
            | member name (candidate :: rest) =
                name = candidate orelse member name rest
          fun dedupe [] = []
            | dedupe (name :: rest) =
                if member name rest then dedupe rest else name :: dedupe rest
        in
          dedupe all
        end
      fun typoActions () =
        let
          val name = nameOfDiagnostic diagnostic
          val (start, stop) = diagnosticRange diagnostic
          val candidates = candidateNames ()
          val scored =
            List.map
              (fn candidate =>
                (editDistance (lower name, lower candidate), candidate))
              candidates
          val close =
            List.filter
              (fn (distance, candidate) =>
                distance > 0 andalso distance <= 2 andalso candidate <> name)
              scored
          val chosen = take 3 (sortPairs close)
        in
          List.map
            (fn (_, candidate) =>
              codeAction ("Change to `" ^ candidate ^ "`") diagnostic
                (textEdit entry start stop candidate))
            chosen
        end
      fun importActions () =
        let
          val name = nameOfDiagnostic diagnostic
          val (start, stop) = diagnosticRange diagnostic
          fun matchesExport (kind, candidate) =
            candidate = name andalso
            (case code of
               "E3012" => kind = "type"
             | "E3027" => kind = "constructor"
             | _ => kind = "function" orelse kind = "constructor")
          val imports = LspCompletion.importsFromText documentText
          val importedKeys = List.map #2 imports
          fun exportsName module =
            List.exists matchesExport (#exports module)
          val qualifies =
            List.mapPartial
              (fn (qualifier, key) =>
                case LspCompletion.moduleFor state path key of
                  SOME module =>
                    if exportsName module andalso
                       qualifierUsable (documentText, key)
                    then
                      SOME
                        (codeAction
                           ("Qualify as `" ^ qualifier ^ "." ^ name ^ "`")
                           diagnostic
                           (textEdit entry start stop
                              (qualifier ^ "." ^ name)))
                    else NONE
                | NONE => NONE)
              imports
          val unimported =
            List.filter
              (fn module =>
                not (List.exists (fn key => key = #key module) importedKeys))
              modules
          val adds =
            List.mapPartial
              (fn module =>
                if String.isPrefix "<" (#key module) then NONE
                else if not (exportsName module) then NONE
                else
                  let
                    val insert = lastImportOffset documentText
                    val item =
                      if code = "E3012" then "type " ^ name else name
                    val newText =
                      "import " ^ #key module ^ ".{" ^ item ^ "}\n"
                  in
                    SOME
                      (codeAction
                         ("Import `" ^ name ^ "` from `" ^ #key module ^ "`")
                         diagnostic
                         (textEdit entry insert insert newText))
                  end)
              unimported
        in
          qualifies @ take 3 adds
        end
      fun baseName name =
        case List.rev (String.tokens (fn character => character = #".") name) of
          last :: _ => last
        | [] => name
      fun containsSpan span =
        Pos.file (#start span) = fileId andalso
        Pos.offset (#start span) <= cursor andalso
        cursor <= Pos.offset (#stop span)

      fun enclosingArgument () =
        let
          fun walk expression =
            case expression of
              Ast.ECall (name, arguments, _) =>
                let
                  fun search (index, remaining) =
                    case remaining of
                      [] => NONE
                    | argument :: rest =>
                        if containsSpan (Ast.exprSpan argument) then
                          (case walk argument of
                             SOME found => SOME found
                           | NONE => SOME (name, index))
                        else search (index + 1, rest)
                in
                  search (0, arguments)
                end
              | Ast.ELet (bindings, body, _) =>
                  firstOf
                    (List.map
                       (fn binding =>
                         case binding of
                           Ast.BVal {value, ...} => walk value
                         | Ast.BPat {value, ...} => walk value
                         | Ast.BFun {body, ...} => walk body
                         | Ast.BGuard {value, fallback, ...} =>
                             (case walk value of
                                SOME found => SOME found
                              | NONE => walk fallback))
                       bindings @ [walk body])
              | Ast.EArena (bindings, body, _) =>
                  firstOf
                    (List.map
                       (fn binding =>
                         case binding of
                           Ast.BVal {value, ...} => walk value
                         | Ast.BPat {value, ...} => walk value
                         | Ast.BFun {body, ...} => walk body
                         | Ast.BGuard {value, fallback, ...} =>
                             (case walk value of
                                SOME found => SOME found
                              | NONE => walk fallback))
                       bindings @ [walk body])
              | Ast.ECase (scrutinee, branches, _) =>
                  firstOf
                    (walk scrutinee ::
                     List.map
                       (fn (_, guard, body) =>
                         (case guard of
                            SOME condition =>
                              (case walk condition of
                                 SOME found => SOME found
                               | NONE => walk body)
                          | NONE => walk body))
                       branches)
              | Ast.EIf (condition, thenBranch, elseBranch, _) =>
                  firstOf [walk condition, walk thenBranch, walk elseBranch]
              | Ast.EBin (_, left, right, _) => firstOf [walk left, walk right]
              | Ast.ENeg (operand, _) => walk operand
              | Ast.ERecord (fields, _) =>
                  firstOf (List.map (fn (_, value) => walk value) fields)
              | Ast.EField (_, base, _) => walk base
              | Ast.ETuple (values, _) => firstOf (List.map walk values)
              | Ast.ETupleIndex (_, base, _) => walk base
              | Ast.EBorrow (_, _, operand, _) => walk operand
              | Ast.EFn (_, _, body, _) => walk body
              | _ => NONE
          and firstOf [] = NONE
            | firstOf (value :: rest) =
                (case value of SOME found => SOME found | NONE => firstOf rest)
        in
          firstOf
            (List.map
               (fn declaration =>
                 case declaration of
                   Ast.DFun {body, ...} => walk body
                 | Ast.DFunClause {body, ...} => walk body
                 | _ => NONE)
               program)
        end
      fun functionShape ty =
        case Types.prune ty of
          Types.TFun (params, result) => SOME (params, result)
        | Types.TConsume inner => functionShape inner
        | _ => NONE
      fun prettyShape (params, result) =
        let
          val values = params @ [result]
          val freeVars =
            List.concat (List.map Types.freeVars values)
          val vars = List.rev (Types.dedupVars freeVars)
          val indexed =
            ListPair.zip (List.tabulate (List.length vars, fn index => index), vars)
          fun varName index =
            let
              val letter = Char.chr (Char.ord #"a" + (index mod 26))
              val suffix = if index < 26 then "" else Int.toString (index div 26)
            in
              "'" ^ String.str letter ^ suffix
            end
          fun marker (id, kind) =
            (case kind of
               Types.Numeric => "'n"
             | Types.Equality => "''t"
             | Types.Ordinary => "'t") ^ Int.toString id
          fun pretty ty =
            List.foldl
              (fn ((index, (id, kind)), text) =>
                replaceAll (text, marker (id, kind), varName index))
              (Types.toString ty)
              indexed
        in
          (List.map pretty params, pretty result)
        end
      fun astFunctionShape syntax =
        case syntax of
          Ast.TyFun (params, result) =>
            SOME (List.map Ast.tyToString params, Ast.tyToString result)
        | Ast.TyConsume (_, inner) => astFunctionShape inner
        | _ => NONE
      fun expectedFunction (callName, index) =
        let
          val found =
            case checkedOption of
              SOME checked =>
                (case LspCompletion.signatureIndex (SOME checked) callName of
                   SOME (_, sigInfo) =>
                     if index < List.length (#params sigInfo) then
                       Option.map prettyShape
                         (functionShape (List.nth (#params sigInfo, index)))
                     else NONE
                 | NONE => NONE)
            | NONE => NONE
        in
          case found of
            SOME value => SOME value
          | NONE =>
              let
                val wanted = baseName callName
                fun fromProgram program =
                  case List.find
                         (fn declaration =>
                           case declaration of
                             Ast.DFun {name, ...} => name = wanted
                           | Ast.DFunClause {name, ...} => name = wanted
                           | _ => false)
                         program of
                    SOME (Ast.DFun {params, ...}) =>
                      if index < List.length params then
                        (case #annot (List.nth (params, index)) of
                           SOME syntax => astFunctionShape syntax
                         | NONE => NONE)
                      else NONE
                  | SOME (Ast.DFunClause {params, ...}) =>
                      if index < List.length params then
                        (case #2 (List.nth (params, index)) of
                           SOME syntax => astFunctionShape syntax
                         | NONE => NONE)
                      else NONE
                  | _ => NONE
                fun firstSome [] = NONE
                  | firstSome (value :: rest) =
                      (case value of
                         SOME found => SOME found
                       | NONE => firstSome rest)
              in
                case fromProgram program of
                  SOME value => SOME value
                | NONE =>
                    firstSome
                      (List.map
                         (fn module => fromProgram (#program module))
                         modules)
              end
        end
      fun placeholderFromText text =
        if text = "unit" then "()"
        else if text = "bool" then "false"
        else if String.isPrefix "int" text then "0"
        else if String.isPrefix "float" text then "0.0"
        else if text = "string" then "\"\""
        else if String.isSuffix " option" text then "None"
        else if String.isSuffix " list" text then "Nil"
        else "()"
      fun createFunction () =
        case enclosingArgument () of
          NONE => []
        | SOME (callName, index) =>
            (case expectedFunction (callName, index) of
               NONE => []
             | SOME (params, result) =>
                 let
                   val name = nameOfDiagnostic diagnostic
                   val names =
                     [ "x", "y", "z", "a", "b", "c" ]
                   fun paramName position =
                     if position < List.length names then
                       List.nth (names, position)
                     else "arg" ^ Int.toString (position + 1)
                   val labelled =
                     ListPair.map
                       (fn (position, ty) =>
                         paramName position ^ " : " ^ ty)
                       (List.tabulate (List.length params, fn index => index),
                        params)
                   val functionSignature =
                     name ^ " (" ^ String.concatWith ", " labelled ^ ")" ^
                     (if result = "unit" then "" else " : " ^ result)
                   val insert = String.size documentText
                   val newText =
                     "\nfun " ^ functionSignature ^ " =\n    " ^
                     placeholderFromText result ^ "\n"
                 in
                   [codeAction ("Create function `" ^ functionSignature ^ "`")
                      diagnostic (textEdit entry insert insert newText)]
                 end)
      fun removeUnused () =
        case declarationAt program fileId line column of
          NONE => []
        | SOME declaration =>
            let
              val span = Ast.declSpan declaration
              val start = Pos.offset (#start span)
              val size = String.size documentText
              val stop = Int.min (size, Pos.offset (#stop span))
              val stop =
                if stop < size andalso String.sub (documentText, stop) = #"\n"
                then stop + 1 else stop
              val name = nameOfDiagnostic diagnostic
              val title =
                if name = "" then "Remove unused function"
                else "Remove unused function `" ^ name ^ "`"
            in
              [codeAction title diagnostic (textEdit entry start stop "")]
            end
      fun wrapBorrow () =
        let
          val (start, stop) = diagnosticRange diagnostic
          val chunk =
            String.substring (documentText, start,
              Int.max (0, Int.min (String.size documentText, stop) - start))
        in
          if code = "E0517" then
            [codeAction "Wrap the argument in `borrow (...)`" diagnostic
               (textEdit entry start stop ("borrow (" ^ chunk ^ ")"))]
          else []
        end

      fun bindingTypeAtSpan span =
        case checkedOption of
          SOME checked =>
            (case List.find (fn (candidate, _) => sameSpan (candidate, span))
                     (#bindingTypes checked) of
               SOME (_, ty) => SOME ty
             | NONE => NONE)
        | NONE => NONE

      fun isContainerTy ty =
        case Types.prune ty of
          Types.TArray _ => true
        | Types.TMap _ => true
        | Types.TDataApp (Types.Data {name, property, ...}, _) =>
            !property = Types.Affine andalso
            name <> "option" andalso name <> "result"
        | Types.TData (Types.Data {name, property, ...}) =>
            !property = Types.Affine andalso
            name <> "option" andalso name <> "result"
        | _ => false

      fun paramType (param : Ast.param) =
        case #annot param of
          SOME ty => SOME (Ast.tyToString ty)
        | NONE =>
            (case bindingTypeAtSpan (#span param) of
               SOME ty => SOME (Types.toString ty)
             | NONE => NONE)

      fun paramBorrowEdit entry declaration param =
        case declaration of
          Ast.DFun {params, body, span, ...} =>
            if not (List.exists
                      (fn candidate =>
                         #name candidate = #name param) params) then NONE
            else
              let
                val text = Source.text (#map entry)
                val headerStart = Pos.offset (#start span)
                val headerStop =
                  Int.min
                    (Pos.offset (#start (Ast.exprSpan body)),
                     Pos.offset (#stop span))
                val name = #name param
                val nameSize = String.size name
                fun isWordChar character =
                  Char.isAlphaNum character orelse character = #"_" orelse
                  character = #"'"
                fun boundaryBefore offset =
                  offset = 0 orelse
                  not (isWordChar (String.sub (text, offset - 1)))
                fun boundaryAfter offset =
                  offset >= String.size text orelse
                  not (isWordChar (String.sub (text, offset)))
                fun skipSpace offset =
                  if offset < headerStop andalso
                     Char.isSpace (String.sub (text, offset))
                  then skipSpace (offset + 1)
                  else offset
                fun typeStop start =
                  let
                    fun loop (index, depth) =
                      if index >= headerStop then index
                      else
                        case String.sub (text, index) of
                          #"(" => loop (index + 1, depth + 1)
                        | #")" =>
                            if depth = 0 then index
                            else loop (index + 1, depth - 1)
                        | #"," =>
                            if depth = 0 then index
                            else loop (index + 1, depth)
                        | #"[" => loop (index + 1, depth + 1)
                        | #"]" =>
                            if depth = 0 then index
                            else loop (index + 1, depth - 1)
                        | #"{" => loop (index + 1, depth + 1)
                        | #"}" =>
                            if depth = 0 then index
                            else loop (index + 1, depth - 1)
                        | _ => loop (index + 1, depth)
                  in
                    loop (start, 0)
                  end
                fun insertBorrow nameOffset =
                  case paramType param of
                    SOME ty =>
                      SOME (textEdit entry nameOffset nameOffset
                              (" : borrow (" ^ ty ^ ")"))
                  | NONE => NONE
                fun search offset =
                  if offset + nameSize > headerStop then NONE
                  else if String.substring (text, offset, nameSize) = name andalso
                          boundaryBefore offset andalso
                          boundaryAfter (offset + nameSize) then
                    let
                      val colon = skipSpace (offset + nameSize)
                    in
                      if colon < headerStop andalso
                         String.sub (text, colon) = #":" then
                        let
                          val typeStart = skipSpace (colon + 1)
                          val stop = typeStop typeStart
                          val original =
                            String.substring (text, typeStart,
                                              Int.max (0, stop - typeStart))
                          val trimmed =
                            trim original
                        in
                          if typeStart >= stop then insertBorrow offset
                          else if String.isPrefix "borrow" trimmed then NONE
                          else
                            SOME (textEdit entry typeStart stop
                                    ("borrow (" ^ original ^ ")"))
                        end
                      else insertBorrow offset
                    end
                  else search (offset + 1)
              in
                search headerStart
              end
        | _ => NONE

      fun paramNameFromMessage diagnostic =
        let
          val message = diagnosticMessage diagnostic
          val marker = "parameter `"
          val size = String.size message
          val markerSize = String.size marker
          fun find index =
            if index + markerSize > size then NONE
            else if String.substring (message, index, markerSize) = marker then
              let
                fun close stop =
                  if stop >= size then NONE
                  else if String.sub (message, stop) = #"`" then
                    SOME (String.substring (message, index + markerSize,
                                            stop - index - markerSize))
                  else close (stop + 1)
              in
                close (index + markerSize)
              end
            else find (index + 1)
        in
          find 0
        end

      fun callsTo fileId name =
        let
          val found = ref []
          fun inFile span = Pos.file (#start span) = fileId
          fun walk expression =
            (case expression of
               Ast.ECall (candidate, arguments, span) =>
                 (if candidate = name andalso inFile span then
                   List.app
                     (fn (index, argument) =>
                        case argument of
                          Ast.EVar _ =>
                            found := (index, argument) :: !found
                        | Ast.EField _ =>
                            found := (index, argument) :: !found
                        | Ast.ETupleIndex _ =>
                            found := (index, argument) :: !found
                        | _ => ())
                     (ListPair.zip
                        (List.tabulate (List.length arguments, fn i => i),
                         arguments))
                  else ();
                  List.app walk arguments)
             | Ast.EBin (_, left, right, _) => (walk left; walk right)
             | Ast.ENeg (operand, _) => walk operand
             | Ast.EIf (condition, thenBranch, elseBranch, _) =>
                 (walk condition; walk thenBranch; walk elseBranch)
             | Ast.ELet (bindings, body, _) =>
                 (List.app
                    (fn Ast.BVal {value, ...} => walk value
                      | Ast.BPat {value, ...} => walk value
                      | Ast.BFun {body, ...} => walk body
                      | Ast.BGuard {value, fallback, ...} =>
                          (walk value; walk fallback))
                    bindings;
                  walk body)
             | Ast.EArena (bindings, body, _) =>
                 (List.app
                    (fn Ast.BVal {value, ...} => walk value
                      | Ast.BPat {value, ...} => walk value
                      | Ast.BFun {body, ...} => walk body
                      | Ast.BGuard {value, fallback, ...} =>
                          (walk value; walk fallback))
                    bindings;
                  walk body)
             | Ast.ERecord (fields, _) =>
                 List.app (fn (_, value) => walk value) fields
             | Ast.EField (_, base, _) => walk base
             | Ast.ETuple (values, _) => List.app walk values
             | Ast.ETupleIndex (_, base, _) => walk base
             | Ast.ECase (scrutinee, branches, _) =>
                 (walk scrutinee;
                  List.app
                    (fn (_, guard, body) =>
                       ((case guard of SOME value => walk value | NONE => ());
                        walk body))
                    branches)
             | Ast.EBorrow (_, _, operand, _) => walk operand
             | Ast.EFn (_, _, body, _) => walk body
             | _ => ())
        in
          List.app
            (fn declaration =>
               case declaration of
                 Ast.DFun {body, ...} => walk body
               | Ast.DFunClause {body, ...} => walk body
               | _ => ())
            program;
          List.rev (!found)
        end

      fun borrowAction () =
        case declarationAt program fileId line column of
          NONE => []
        | SOME declaration =>
            (case declaration of
               Ast.DFun {name, params, ...} =>
                 let
                   fun paramForDiagnostic () =
                     case code of
                       "W3102" =>
                         List.find
                           (fn param => spanContains (#span param) (line, column))
                           params
                     | "W3101" =>
                         (case paramNameFromMessage diagnostic of
                            SOME wanted =>
                              List.find (fn param => #name param = wanted) params
                          | NONE => NONE)
                     | _ => NONE
                   val chosen =
                     case paramForDiagnostic () of
                       SOME param => SOME param
                     | NONE =>
                         List.find
                           (fn param =>
                              case bindingTypeAtSpan (#span param) of
                                SOME ty => isContainerTy ty
                              | NONE => false)
                           params
                 in
                   case chosen of
                     NONE => []
                   | SOME param =>
                       (case paramBorrowEdit entry declaration param of
                          NONE => []
                        | SOME edit =>
                            let
                              val index =
                                let
                                  fun loop (remaining, position) =
                                    case remaining of
                                      [] => 0
                                    | candidate :: rest =>
                                        if sameSpan (#span candidate,
                                                    #span param) then position
                                        else loop (rest, position + 1)
                                in
                                  loop (params, 0)
                                end
                              val callEdits =
                                List.map
                                  (fn (_, argument) =>
                                     let
                                       val span = Ast.exprSpan argument
                                       val start = Pos.offset (#start span)
                                       val stop = Pos.offset (#stop span)
                                       val text =
                                         Source.text (#map entry)
                                       val size =
                                         Int.max
                                           (0, Int.min (String.size text, stop) -
                                               start)
                                       val chunk =
                                         String.substring (text, start, size)
                                     in
                                       textEdit entry start stop
                                         ("borrow (" ^ chunk ^ ")")
                                     end)
                                  (List.filter
                                     (fn (position, _) => position = index)
                                     (callsTo fileId name))
                              val edits = edit :: callEdits
                              val title =
                                if List.null callEdits then
                                  "Add `borrow` to parameter `" ^ #name param ^ "`"
                                else
                                  "Add `borrow` to parameter `" ^ #name param ^
                                  "` and call sites"
                            in
                              [codeActionMany title diagnostic edits]
                            end)
                 end
             | _ => [])

          fun wildcardCase () =
            case findCaseAt program fileId line column of
              NONE => []
            | SOME (expression as Ast.ECase (_, branches, _)) =>
                (case List.rev branches of
                   [] => []
                 | (pattern, _, body) :: _ =>
                     let
                       val bodyEnd = #stop (Ast.exprSpan body)
                       val indent = indentAt entry (#start (Ast.patternSpan pattern))
                       val edit =
                         textEdit entry (Pos.offset bodyEnd) (Pos.offset bodyEnd)
                           ("\n" ^ indent ^ "| _ => " ^ casePlaceholder expression)
                     in
                       [codeAction "Add wildcard branch `_ =>`" diagnostic edit]
                     end)
            | _ => []
          fun completeCaseBranches () =
            let
              val names = missingConstructors diagnostic
              val constructors =
                List.mapPartial (fn name => LspTypes.findConstructor db name) names
            in
              if List.null constructors then []
              else
                case findCaseAt program fileId line column of
                  NONE => []
                | SOME (expression as Ast.ECase (_, branches, _)) =>
                    (case List.rev branches of
                       [] => []
                     | (pattern, _, body) :: _ =>
                         let
                           val bodyEnd = #stop (Ast.exprSpan body)
                           val indent =
                             indentAt entry (#start (Ast.patternSpan pattern))
                           val placeholder = casePlaceholder expression
                           val newText =
                             String.concat
                               (List.map
                                  (fn constructor =>
                                    "\n" ^ indent ^ "| " ^
                                    LspTypes.patternText constructor ^
                                    " => " ^ placeholder)
                                  constructors)
                           val edit =
                             textEdit entry (Pos.offset bodyEnd)
                               (Pos.offset bodyEnd) newText
                         in
                           [codeAction "Add missing case branches" diagnostic edit]
                         end)
                | _ => []
            end
          fun removeBranch () =
            case findCaseAt program fileId line column of
              NONE => []
            | SOME (Ast.ECase (_, branches, _)) =>
                (case List.find
                        (fn (pattern, _, _) =>
                          spanContains (Ast.patternSpan pattern) (line, column))
                        branches of
                   NONE => []
                 | SOME (pattern, _, body) =>
                     let
                       val patternStart = #start (Ast.patternSpan pattern)
                       val bodyStop = #stop (Ast.exprSpan body)
                       val patternLine =
                         Source.lineText (#map entry, Pos.line patternStart)
                       val prefix =
                         String.substring (patternLine, 0, Pos.column patternStart - 1)
                       fun findPipe index =
                         if index < 0 then NONE
                         else if String.sub (prefix, index) = #"|" then SOME index
                         else findPipe (index - 1)
                       val startOffset =
                         case findPipe (String.size prefix - 1) of
                           SOME index =>
                             Pos.offset patternStart - (String.size prefix - index)
                         | NONE => Pos.offset patternStart
                       val edit = textEdit entry startOffset (Pos.offset bodyStop) ""
                     in
                       [codeAction "Remove unreachable branch" diagnostic edit]
                     end)
            | _ => []
          fun declarationEdit title needle replacement =
            case declarationAt program fileId line column of
              NONE => []
            | SOME declaration =>
                (case replaceInSpan entry (Ast.declSpan declaration) needle replacement of
                   SOME edit => [codeAction title diagnostic edit]
                 | NONE => [])
          fun addConsume () =
            case callbackCallAt program fileId line column code of
              NONE => []
            | SOME (callName, index) =>
                (case findFunction program callName of
                   NONE => []
                 | SOME declaration =>
                     (case consumeEdit entry declaration index of
                        SOME edit =>
                          [codeAction
                             "Mark the callback parameter as `consume`"
                             diagnostic edit]
                      | NONE => []))
          fun addMain () =
            let
              val message =
                case member "message" diagnostic of
                  SOME (Json.String text) => text
                | _ => ""
            in
              if String.isPrefix "missing `main`" message then
                let
                  val endOffset = String.size (Source.text (#map entry))
                  val edit =
                    textEdit entry endOffset endOffset "\nfun main () =\n    ()\n"
                in
                  [codeAction "Add `main` function" diagnostic edit]
                end
              else []
            end
      fun messageValue marker =
        let
          val message = diagnosticMessage diagnostic
        in
          case findSubstring (marker, message) of
            NONE => NONE
          | SOME index =>
              let
                val rest =
                  String.extract (message, index + String.size marker, NONE)
                val stop =
                  case findSubstring ("\n", rest) of
                    SOME newline => newline
                  | NONE => String.size rest
                val value = trim (String.substring (rest, 0, stop))
              in
                if value = "" then NONE else SOME value
              end
        end
      fun parseIntText text =
        let
          val size = String.size text
          fun loop (index, digits) =
            if index < size andalso Char.isDigit (String.sub (text, index))
            then loop (index + 1, String.substring (text, index, 1) :: digits)
            else Int.fromString (String.concat (List.rev digits))
        in
          loop (0, [])
        end
      fun messageInt marker = Option.mapPartial parseIntText (messageValue marker)

      fun callAt () =
        let
          fun firstOf [] = NONE
            | firstOf (value :: rest) =
                (case value of SOME found => SOME found | NONE => firstOf rest)
          fun walk expression =
            case expression of
              Ast.ECall (name, arguments, span) =>
                if containsSpan span then
                  (case firstOf
                          (List.map
                             (fn argument =>
                               if containsSpan (Ast.exprSpan argument) then
                                 walk argument
                               else NONE)
                             arguments) of
                     SOME found => SOME found
                   | NONE => SOME (name, arguments, span))
                else NONE
              | Ast.ELet (bindings, body, _) =>
                  firstOf
                    (List.map
                       (fn binding =>
                         case binding of
                           Ast.BVal {value, ...} => walk value
                         | Ast.BPat {value, ...} => walk value
                         | Ast.BFun {body, ...} => walk body
                         | Ast.BGuard {value, fallback, ...} =>
                             (case walk value of
                                SOME found => SOME found
                              | NONE => walk fallback))
                       bindings @ [walk body])
              | Ast.EArena (bindings, body, _) =>
                  firstOf
                    (List.map
                       (fn binding =>
                         case binding of
                           Ast.BVal {value, ...} => walk value
                         | Ast.BPat {value, ...} => walk value
                         | Ast.BFun {body, ...} => walk body
                         | Ast.BGuard {value, fallback, ...} =>
                             (case walk value of
                                SOME found => SOME found
                              | NONE => walk fallback))
                       bindings @ [walk body])
              | Ast.ECase (scrutinee, branches, _) =>
                  firstOf
                    (walk scrutinee ::
                     List.map
                       (fn (_, guard, body) =>
                         (case guard of
                            SOME condition =>
                              (case walk condition of
                                 SOME found => SOME found
                               | NONE => walk body)
                          | NONE => walk body))
                       branches)
              | Ast.EIf (condition, thenBranch, elseBranch, _) =>
                  firstOf [walk condition, walk thenBranch, walk elseBranch]
              | Ast.EBin (_, left, right, _) => firstOf [walk left, walk right]
              | Ast.ENeg (operand, _) => walk operand
              | Ast.ERecord (fields, _) =>
                  firstOf (List.map (fn (_, value) => walk value) fields)
              | Ast.EField (_, base, _) => walk base
              | Ast.ETuple (values, _) => firstOf (List.map walk values)
              | Ast.ETupleIndex (_, base, _) => walk base
              | Ast.EBorrow (_, _, operand, _) => walk operand
              | Ast.EFn (_, _, body, _) => walk body
              | _ => NONE
          val target =
            case checkedOption of
              SOME checked => #program checked
            | NONE => tolerantProgram
        in
          firstOf
            (List.map
               (fn declaration =>
                 case declaration of
                   Ast.DFun {body, ...} => walk body
                 | Ast.DFunClause {body, ...} => walk body
                 | _ => NONE)
               target)
        end
      fun callOpenOffset span =
        let
          val start = Pos.offset (#start span)
          val stop = Int.min (String.size documentText, Pos.offset (#stop span))
          fun loop index =
            if index >= stop then NONE
            else if String.sub (documentText, index) = #"(" then SOME index
            else loop (index + 1)
        in
          loop start
        end
      fun arityAction () =
        case (messageInt "expects ", messageInt ", but got ") of
          (SOME expected, SOME got) =>
            (case callAt () of
               SOME (name, arguments, span) =>
                 if got < expected then
                   let
                     val missing = expected - got
                     val placeholders =
                       case checkedOption of
                         SOME checked =>
                           (case LspCompletion.signatureIndex
                                   (SOME checked) name of
                              SOME (_, sigInfo) =>
                                let
                                  val types = List.drop (#params sigInfo, got)
                                in
                                  if List.length types = missing then
                                    List.map
                                      (fn ty =>
                                        placeholderFromText
                                          (Types.toString ty))
                                      types
                                  else List.tabulate (missing, fn _ => "()")
                                end
                            | NONE => List.tabulate (missing, fn _ => "()"))
                       | NONE => List.tabulate (missing, fn _ => "()")
                     val insert =
                       case callOpenOffset span of
                         SOME offset => offset + 1
                       | NONE => Pos.offset (#stop span)
                     val separator = if got = 0 then "" else ", "
                     val text = separator ^ String.concatWith ", " placeholders
                     val title =
                       "Add " ^ Int.toString missing ^ " missing argument" ^
                       (if missing = 1 then "" else "s")
                   in
                     [codeAction title diagnostic
                        (textEdit entry insert insert text)]
                   end
                 else if got > expected then
                   let
                     val afterArgument =
                       case List.drop (arguments, expected) of
                         argument :: _ => Pos.offset (#stop (Ast.exprSpan argument))
                       | [] => Pos.offset (#stop span)
                     val beforeClose =
                       Int.max (afterArgument, Pos.offset (#stop span) - 1)
                     val removeStart =
                       if expected = 0 then
                         (case callOpenOffset span of
                            SOME offset => offset + 1
                          | NONE => afterArgument)
                       else afterArgument
                     val title =
                       "Remove " ^ Int.toString (got - expected) ^
                       " extra argument" ^
                       (if got - expected = 1 then "" else "s")
                   in
                     [codeAction title diagnostic
                        (textEdit entry removeStart beforeClose "")]
                   end
                 else []
             | _ => [])
        | _ => []
      fun argumentSpan () =
        case messageInt "argument " of
          SOME index =>
            (case callAt () of
               SOME (_, arguments, _) =>
                 if index >= 1 andalso index <= List.length arguments then
                   SOME (Ast.exprSpan (List.nth (arguments, index - 1)))
                 else NONE
             | NONE => NONE)
        | NONE => NONE
      fun typeMismatchActions () =
        let
          val (start, stop) =
            case argumentSpan () of
              SOME span =>
                (Pos.offset (#start span), Pos.offset (#stop span))
            | NONE => diagnosticRange diagnostic
          val chunk =
            String.substring (documentText, start,
              Int.max (0, Int.min (String.size documentText, stop) - start))
          fun wrapAction title wrapper =
            codeAction title diagnostic
              (textEdit entry start stop (wrapper ^ " (" ^ chunk ^ ")"))
          val conversion =
            case (messageValue "expected: ", messageValue "found: ") of
              (SOME "string", SOME found) =>
                (case found of
                   "int" => SOME "int_to_string"
                 | "int32" => SOME "int32_to_string"
                 | "int64" => SOME "int64_to_string"
                 | "float" => SOME "float_to_string"
                 | "float32" => SOME "float_to_string"
                 | "float64" => SOME "float_to_string"
                 | _ => NONE)
            | _ => NONE
          val conversions =
            case conversion of
              SOME name =>
                [wrapAction ("Convert with `" ^ name ^ "`") name]
            | NONE => []
          val borrows =
            case messageValue "expected: " of
              SOME expected =>
                if String.isPrefix "borrow " expected then
                  [wrapAction "Wrap in `borrow (...)`" "borrow"]
                else []
            | NONE => []
        in
          conversions @ borrows
        end
      fun constructorArityAction () =
        case quotedName (diagnosticMessage diagnostic) of
          NONE => []
        | SOME name =>
            let
              val (start, stop) = diagnosticRange diagnostic
              val message = diagnosticMessage diagnostic
            in
              if findSubstring ("expects no argument", message) <> NONE then
                [codeAction "Remove the constructor argument" diagnostic
                   (textEdit entry start stop name)]
              else if findSubstring ("expects an argument", message) <> NONE then
                [codeAction "Add the constructor argument" diagnostic
                   (textEdit entry stop stop " ()")]
              else []
            end
      fun tuplePatternAction () =
        case (messageInt "expected: ", messageInt "found: ") of
          (SOME expected, SOME found) =>
            if found < expected then
              let
                val (_, stop) = diagnosticRange diagnostic
                val missing = expected - found
                val text =
                  String.concat
                    (List.tabulate (missing, fn _ => ", _"))
              in
                [codeAction
                   ("Add " ^ Int.toString missing ^ " missing tuple element" ^
                    (if missing = 1 then "" else "s"))
                   diagnostic
                   (textEdit entry (Int.max (0, stop - 1))
                      (Int.max (0, stop - 1)) text)]
              end
            else []
        | _ => []
      fun borrowGetterAction () =
        case quotedName (diagnosticMessage diagnostic) of
          NONE => []
        | SOME old =>
            let
              val replacement =
                case old of
                  "array_get" => SOME "array_get_borrow"
                | "array_get_unchecked" => SOME "array_get_borrow"
                | "map_get" => SOME "map_get_borrow"
                | _ => NONE
            in
              case replacement of
                NONE => []
              | SOME new =>
                  (case callAt () of
                     SOME (_, _, span) =>
                       (case replaceInSpan entry span old new of
                          SOME edit =>
                            [codeAction ("Use `" ^ new ^ "`") diagnostic edit]
                        | NONE => [])
                   | _ => [])
            end
      fun recordFieldsFromText text =
        let
          val text = trim text
          val inner =
            if String.isPrefix "{" text andalso String.isSuffix "}" text then
              trim (String.substring (text, 1, String.size text - 2))
            else ""
          fun splitFields depth (index, start, acc) =
            if index >= String.size inner then
              List.rev (String.substring (inner, start, index - start) :: acc)
            else
              case String.sub (inner, index) of
                #"{" => splitFields (depth + 1) (index + 1, start, acc)
              | #"(" => splitFields (depth + 1) (index + 1, start, acc)
              | #"[" => splitFields (depth + 1) (index + 1, start, acc)
              | #"}" => splitFields (depth - 1) (index + 1, start, acc)
              | #")" => splitFields (depth - 1) (index + 1, start, acc)
              | #"]" => splitFields (depth - 1) (index + 1, start, acc)
              | #"," =>
                  if depth = 0 then
                    splitFields depth
                      (index + 1, index + 1,
                       String.substring (inner, start, index - start) :: acc)
                  else splitFields depth (index + 1, start, acc)
              | _ => splitFields depth (index + 1, start, acc)
          val parts =
            if inner = "" then []
            else splitFields 0 (0, 0, [])
          fun field part =
            case String.fields (fn character => character = #":") part of
              name :: rest =>
                SOME (trim name, trim (String.concatWith ":" rest))
            | [] => NONE
        in
          List.mapPartial field parts
        end
      fun recordAction () =
        let
          val expected = messageValue "expected: "
          val expectedFields =
            case expected of
              SOME text =>
                if String.isPrefix "{" (trim text) then
                  recordFieldsFromText text
                else
                  (case LspTypes.findRecord db (trim text) of
                     SOME fields =>
                       List.map
                         (fn field => (#name field, #ty field))
                         fields
                   | NONE => [])
            | NONE => []
          fun recordAt () =
            let
              val (rangeStart, rangeStop) = diagnosticRange diagnostic
              fun overlaps (span : Diag.span) =
                Pos.offset (#start span) < rangeStop andalso
                rangeStart < Pos.offset (#stop span)
              fun firstOf [] = NONE
                | firstOf (value :: rest) =
                    (case value of SOME found => SOME found | NONE => firstOf rest)
              fun walk expression =
                case expression of
                  Ast.ERecord (fields, span) =>
                    if overlaps span then SOME (fields, span)
                    else firstOf
                      (List.map (fn (_, value) => walk value) fields)
                  | Ast.ELet (bindings, body, _) =>
                      firstOf
                        (List.map
                           (fn binding =>
                             case binding of
                               Ast.BVal {value, ...} => walk value
                             | Ast.BPat {value, ...} => walk value
                             | Ast.BFun {body, ...} => walk body
                             | Ast.BGuard {value, fallback, ...} =>
                                 (case walk value of
                                    SOME found => SOME found
                                  | NONE => walk fallback))
                           bindings @ [walk body])
                  | Ast.EArena (bindings, body, _) =>
                      firstOf
                        (List.map
                           (fn binding =>
                             case binding of
                               Ast.BVal {value, ...} => walk value
                             | Ast.BPat {value, ...} => walk value
                             | Ast.BFun {body, ...} => walk body
                             | Ast.BGuard {value, fallback, ...} =>
                                 (case walk value of
                                    SOME found => SOME found
                                  | NONE => walk fallback))
                           bindings @ [walk body])
                  | Ast.ECall (_, arguments, _) =>
                      firstOf (List.map walk arguments)
                  | Ast.EIf (condition, thenBranch, elseBranch, _) =>
                      firstOf [walk condition, walk thenBranch, walk elseBranch]
                  | Ast.EBin (_, left, right, _) => firstOf [walk left, walk right]
                  | Ast.ENeg (operand, _) => walk operand
                  | Ast.EField (_, base, _) => walk base
                  | Ast.ETuple (values, _) => firstOf (List.map walk values)
                  | Ast.ETupleIndex (_, base, _) => walk base
                  | Ast.EBorrow (_, _, operand, _) => walk operand
                  | Ast.EFn (_, _, body, _) => walk body
                  | _ => NONE
              val target =
                case checkedOption of
                  SOME checked => #program checked
                | NONE => tolerantProgram
            in
              firstOf
                (List.map
                   (fn declaration =>
                     case declaration of
                       Ast.DFun {body, ...} => walk body
                     | Ast.DFunClause {body, ...} => walk body
                     | _ => NONE)
                   target)
            end
        in
          if List.null expectedFields then []
          else
            case recordAt () of
              NONE => []
            | SOME (fields, span) =>
                let
                  fun present name =
                    List.exists (fn (candidate, _) => candidate = name) fields
                  val missing =
                    List.filter (fn (name, _) => not (present name))
                      expectedFields
                in
                  if List.null missing then []
                  else
                    let
                      val stop = Pos.offset (#stop span)
                      val insert = Int.max (0, stop - 1)
                      val separator =
                        if List.null fields then "" else ", "
                      fun fieldText (name, ty) =
                        name ^ " = " ^ placeholderFromText ty
                      val names =
                        String.concatWith ", "
                          (List.map (fn (name, _) => name) missing)
                      val text =
                        separator ^
                        String.concatWith ", "
                          (List.map fieldText missing)
                    in
                      [codeAction
                         ("Add missing record field" ^
                          (if List.length missing = 1 then " `" else "s `") ^
                          names ^ "`")
                         diagnostic
                         (textEdit entry insert insert text)]
                    end
                end
        end

        in
          case code of
            "E3001" => typoActions () @ importActions () @ createFunction ()
          | "E3002" => typoActions () @ importActions () @ createFunction ()
          | "E3012" => typoActions () @ importActions ()
          | "E3027" => typoActions () @ importActions ()
          | "E3003" => arityAction ()
          | "E3004" => typeMismatchActions () @ recordAction ()
          | "E3019" => tuplePatternAction ()
          | "E3026" => constructorArityAction ()
          | "E3041" => borrowGetterAction ()
          | "E3044" => borrowGetterAction ()
          | "W3001" => removeUnused ()
          | "E0517" => wrapBorrow ()
          | "E3021" => completeCaseBranches () @ wildcardCase ()
          | "E3022" => removeBranch ()
          | "E3015" => declarationEdit "Remove `Copy` annotation" " : Copy" ""
          | "E3016" => declarationEdit "Declare the type `Linear`" "Affine" "Linear"
          | "E3010" => addMain ()
          | "E0512" => addConsume ()
          | "E0513" => addConsume ()
          | "W3101" => borrowAction ()
          | "W3102" => borrowAction ()
          | _ => []
        end
      fun refactorActions entry =
        let
          val (line0, character0) = rangeStart range
          val cursor = offsetOfPosition (documentText, line0, character0)
          val text = Source.text (#map entry)
          val size = String.size text
          fun refactorAction title edits =
            obj
              [ ("title", str title)
              , ("kind", str "refactor.rewrite")
              , ("edit",
                  obj [ ("changes", obj [ (uri, arr edits) ]) ])
              ]
          fun lastSegment name =
            case List.rev
                   (String.tokens
                      (fn character =>
                        character = #"." orelse character = #"/") name) of
              segment :: _ => segment
            | [] => name
          fun dataNameOf ty =
            case Types.prune ty of
              Types.TData (Types.Data {name, ...}) => SOME name
            | Types.TDataApp (Types.Data {name, ...}, _) => SOME name
            | Types.TBorrow (inner, _, _) => dataNameOf inner
            | Types.TConsume inner => dataNameOf inner
            | _ => NONE
          fun indentOf start =
            let
              val lineText = Source.lineText (#map entry, Pos.line start)
              fun loop index =
                if index < String.size lineText andalso
                   (String.sub (lineText, index) = #" " orelse
                    String.sub (lineText, index) = #"\t")
                then loop (index + 1) else index
            in
              String.substring (lineText, 0, loop 0)
            end
          fun atLineStart start =
            Pos.column start - 1 = String.size (indentOf start)
          fun sameLine span =
            Pos.line (#start span) = Pos.line (#stop span)
          fun contains span =
            Pos.file (#start span) = fileId andalso
            Pos.offset (#start span) <= cursor andalso
            cursor <= Pos.offset (#stop span)
          fun smallest pairs =
            case pairs of
              [] => NONE
              | first :: rest =>
                  SOME
                    (List.foldl
                       (fn (candidate, best) =>
                         let
                           fun width (span, _) =
                             Pos.offset (#stop span) - Pos.offset (#start span)
                         in
                           if width candidate < width best then candidate
                           else best
                         end)
                       first rest)
          val patternAction =
            case checkedOption of
              NONE => []
            | SOME checked =>
                (case smallest
                        (List.filter (fn (span, _) => contains span)
                           (#expressionTypes checked)) of
                   NONE => []
                 | SOME (span, ty) =>
                     if not (sameLine span) orelse
                        not (atLineStart (#start span)) then []
                     else
                       (case dataNameOf ty of
                          NONE => []
                        | SOME name =>
                            (case LspTypes.findDatatype db (lastSegment name) of
                               NONE => []
                             | SOME constructors =>
                                 if List.null constructors then []
                                 else
                                   let
                                     val start = Pos.offset (#start span)
                                     val stop = Pos.offset (#stop span)
                                     val expression =
                                       String.substring (text, start,
                                         Int.max (0, Int.min (size, stop) - start))
                                     val indent = indentOf (#start span)
                                     val branchIndent = indent ^ "    "
                                     val branches =
                                       List.map
                                         (fn constructor =>
                                           LspTypes.patternTextWith db constructor ^
                                           " => ()")
                                         constructors
                                     val newText =
                                       case branches of
                                         [] => ""
                                       | first :: rest =>
                                           "case " ^ expression ^ " of\n" ^
                                           branchIndent ^ first ^
                                           String.concat
                                             (List.map
                                                (fn branch =>
                                                  "\n" ^ branchIndent ^ "| " ^
                                                  branch)
                                                rest)
                                   in
                                     [refactorAction
                                        "Pattern match on this value"
                                        [textEdit entry start stop newText]]
                                   end)))
          fun bindingDeclAt program =
            let
              fun contains (span : Diag.span) =
                Pos.offset (#start span) <= cursor andalso
                cursor <= Pos.offset (#stop span)
              fun search expression =
                case expression of
                  Ast.ELet (bindings, body, _) =>
                    searchBindings (bindings, body)
                | Ast.EArena (bindings, body, _) =>
                    searchBindings (bindings, body)
                | Ast.ECase (scrutinee, branches, _) =>
                    (case search scrutinee of
                       SOME found => SOME found
                     | NONE =>
                         firstOf
                           (List.map
                              (fn (_, guard, body) =>
                                case guard of
                                  SOME condition =>
                                    (case search condition of
                                       SOME found => SOME found
                                     | NONE => search body)
                                | NONE => search body)
                              branches))
                | Ast.EIf (condition, thenBranch, elseBranch, _) =>
                    firstOf [search condition, search thenBranch, search elseBranch]
                | Ast.EBin (_, left, right, _) =>
                    firstOf [search left, search right]
                | Ast.ENeg (operand, _) => search operand
                | Ast.ECall (_, arguments, _) => firstOf (List.map search arguments)
                | Ast.ERecord (fields, _) =>
                    firstOf (List.map (fn (_, value) => search value) fields)
                | Ast.EField (_, base, _) => search base
                | Ast.ETuple (values, _) => firstOf (List.map search values)
                | Ast.ETupleIndex (_, base, _) => search base
                | Ast.EBorrow (_, _, operand, _) => search operand
                | Ast.EFn (_, _, body, _) => search body
                | _ => NONE
              and searchBindings (bindings, body) =
                case bindings of
                  [] => search body
                | binding :: rest =>
                    (case searchBinding binding of
                       SOME found => SOME found
                     | NONE => searchBindings (rest, body))
              and searchBinding binding =
                case binding of
                  Ast.BVal {name, annot, value, span} =>
                    (case wordInSpan (text, span, name) of
                       SOME offset =>
                         if offset <= cursor andalso
                            cursor <= offset + String.size name
                         then SOME (name, span, offset, annot)
                         else search value
                     | NONE => search value)
                | Ast.BPat {value, ...} => search value
                | Ast.BFun {body, ...} => search body
                | Ast.BGuard {value, fallback, ...} =>
                    (case search value of
                       SOME found => SOME found
                     | NONE => search fallback)
              and firstOf [] = NONE
                | firstOf (value :: rest) =
                    (case value of
                       SOME found => SOME found
                     | NONE => firstOf rest)
            in
              firstOf
                (List.map
                   (fn declaration =>
                     case declaration of
                       Ast.DFun {body, ...} => search body
                     | Ast.DFunClause {body, ...} => search body
                     | _ => NONE)
                   program)
            end
          val annotationAction =
            case checkedOption of
              NONE => []
            | SOME checked =>
                (case bindingDeclAt tolerantProgram of
                   NONE => []
                 | SOME (name, span, offset, annot) =>
                     if Option.isSome annot then []
                     else
                     let
                       val insertion = offset + String.size name
                       fun skipSpace index =
                         if index < size andalso
                            Char.isSpace (String.sub (text, index))
                         then skipSpace (index + 1) else index
                       val after = skipSpace insertion
                       val annotated =
                         after < size andalso
                         String.sub (text, after) = #":"
                       val ty =
                         List.find
                           (fn (candidate, _) =>
                             annotatedStart candidate = annotatedStart span)
                           (#bindingTypes checked)
                     in
                       case ty of
                         SOME (_, value) =>
                           if annotated orelse
                              not (List.null (Types.freeVars value))
                           then []
                           else
                             let
                               val typeText = Types.toString value
                             in
                               [refactorAction
                                  ("Add type annotation `: " ^ typeText ^ "`")
                                  [textEdit entry insertion insertion
                                     (" : " ^ typeText)]]
                             end
                       | NONE => []
                     end)
          val destructureAction =
            case bindingDeclAt tolerantProgram of
              NONE => []
            | SOME (name, span, offset, annot) =>
                let
                  fun shapeOfTy ty =
                    case Types.prune ty of
                      Types.TTuple values =>
                        SOME (TupleShape (List.length values))
                    | Types.TRecord fields =>
                        SOME (RecordShape (List.map #1 fields))
                    | Types.TCon {fields, ...} =>
                        if List.null fields then NONE
                        else SOME (RecordShape (List.map #1 fields))
                    | Types.TData data =>
                        (case Types.dataCommonFields data [] of
                           SOME fields =>
                             SOME (RecordShape (List.map #1 fields))
                         | NONE => NONE)
                    | Types.TDataApp (data, arguments) =>
                        (case Types.dataCommonFields data arguments of
                           SOME fields =>
                             SOME (RecordShape (List.map #1 fields))
                         | NONE => NONE)
                    | _ => NONE
                  fun shapeOfSyntax syntax =
                    case syntax of
                      Ast.TyTuple values =>
                        SOME (TupleShape (List.length values))
                    | Ast.TyRecord fields =>
                        SOME (RecordShape (List.map #1 fields))
                    | Ast.TyName typeName =>
                        (case LspTypes.findRecord db (lastSegment typeName) of
                           SOME fields =>
                             SOME (RecordShape (List.map #name fields))
                         | NONE => NONE)
                    | _ => NONE
                  val shape =
                    case checkedOption of
                      SOME checked =>
                        (case List.find
                                (fn (candidate, _) =>
                                  annotatedStart candidate = annotatedStart span)
                                (#bindingTypes checked) of
                           SOME (_, ty) => shapeOfTy ty
                         | NONE =>
                             (case annot of
                                SOME syntax => shapeOfSyntax syntax
                              | NONE => NONE))
                    | NONE =>
                        (case annot of
                           SOME syntax => shapeOfSyntax syntax
                         | NONE => NONE)
                  fun tupleName index =
                    let
                      val names = [ "x", "y", "z", "a", "b", "c" ]
                    in
                      if index < List.length names then List.nth (names, index)
                      else "value" ^ Int.toString (index + 1)
                    end
                  val pattern =
                    case shape of
                      SOME (TupleShape count) =>
                        SOME
                          ("(" ^
                           String.concatWith ", "
                             (List.tabulate (count, tupleName)) ^ ")")
                    | SOME (RecordShape fields) =>
                        SOME
                          ("{ " ^
                           String.concatWith ", "
                             (List.map (fn field => field ^ " = " ^ field)
                                fields) ^ " }")
                    | NONE => NONE
                in
                  case pattern of
                    NONE => []
                  | SOME text =>
                      [refactorAction
                         ("Destructure into `" ^ text ^ "`")
                         [textEdit entry offset
                            (offset + String.size name) text]]
                end
        in
          patternAction @ annotationAction @ destructureAction
        end
    in
      case sourceEntry of
        NONE => []
      | SOME entry =>
          refactorActions entry @
          List.concat
            (List.map (fn diagnostic => actionsFor entry diagnostic)
               contextDiagnostics)
    end
end
