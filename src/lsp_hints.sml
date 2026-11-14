structure LspHints =
struct
  open LspProtocol
  open LspText
  open LspAst
  open LspState

  fun typeTextWith visited ty =
    let
      fun fieldText (field, fieldTy) =
        field ^ " : " ^ typeTextWith visited fieldTy
      fun recordText fields =
        "{ " ^ String.concatWith ", " (List.map fieldText fields) ^ " }"
    in
      case Types.prune ty of
        Types.TCon {name, fields, ...} =>
          if List.null fields then name
          else if List.exists (fn candidate => candidate = name) visited then name
          else
            name ^ " = " ^
            typeTextWith (name :: visited) (Types.TRecord fields)
      | Types.TRecord fields => recordText fields
      | Types.TTuple values =>
          String.concatWith " * " (List.map (typeTextWith visited) values)
      | Types.TConsume inner => "consume " ^ typeTextWith visited inner
      | Types.TBorrow (inner, mutable, provenance) =>
          "borrow " ^ (if mutable then "mut " else "") ^
          typeTextWith visited inner ^
          (case provenance of
             NONE => ""
           | SOME index => " from " ^ Int.toString index)
      | _ => Types.toString ty
    end

  fun typeText ty = typeTextWith [] ty

  fun bindingNames program =
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
                 | Ast.BFun {body, ...} => walk body
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
                 | Ast.BFun {body, ...} => walk body
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
            Ast.DFun {name, params, result, body, ...} =>
              (List.app
                 (fn {name, annot, span} => add (span, name))
                 params;
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

  fun positionAfterName entry (span : Diag.span, name) =
    let
      val start = #start span
      val line = Pos.line start
      val lineText = Source.lineText (#map entry, line)
      val width = String.size name
      fun search index =
        if width = 0 then NONE
        else if index + width > String.size lineText then NONE
        else if String.substring (lineText, index, width) = name then SOME index
        else search (index + 1)
      val found = search (Pos.column start - 1)
    in
      case found of
        SOME index =>
          obj
            [ ("line", int (line - 1))
            , ("character", int (byteColumnToCharacter (lineText, index + width + 1)))
            ]
      | NONE => lspPosition entry start
    end

  fun paramsEndOffset entry nameSpan limit =
    let
      val text = Source.text (#map entry)
      val size = String.size text
      val start = Pos.offset (#start nameSpan)
      val stop = Int.min (size, Pos.offset limit)
      fun findOpen index =
        if index >= stop then NONE
        else if String.sub (text, index) = #"(" then SOME index
        else findOpen (index + 1)
      fun matchClose index depth =
        if index >= size then NONE
        else
          let val character = String.sub (text, index) in
            if character = #"(" then matchClose (index + 1) (depth + 1)
            else if character = #")" then
              if depth = 1 then SOME (index + 1)
              else matchClose (index + 1) (depth - 1)
            else matchClose (index + 1) depth
          end
    in
      case findOpen start of
        NONE => NONE
      | SOME openIndex => matchClose openIndex 0
    end

  fun nameLookup names =
    let
      val size = Int.max (64, 2 * List.length names)
      val buckets = Array.array (size, [])
      fun hashKey (file, line, column) =
        let
          val modulus = Word.fromInt size
          val key =
            Word.mod
              (Word.fromInt file * 0w1000003 + Word.fromInt line, modulus)
        in
          Word.toInt
            (Word.mod (key * 0w1009 + Word.fromInt column, modulus))
        end
      fun insert (start, name) =
        let
          val index = hashKey start
          val bucket = Array.sub (buckets, index)
        in
          Array.update (buckets, index, (start, name) :: bucket)
        end
      val _ = List.app insert names
    in
      fn span =>
        let
          val start = annotatedStart span
          val bucket = Array.sub (buckets, hashKey start)
        in
          case List.find (fn (candidate, _) => candidate = start) bucket of
            SOME (_, name) => name
          | NONE => ""
        end
    end

  fun arenaMarkdown uri mode block =
    String.concatWith "\n"
      (ArenaReport.details
         ( fn (line, column) =>
             "[" ^ Int.toString line ^ ":" ^ Int.toString column ^ "](" ^ uri ^
             "#L" ^ Int.toString line ^ "," ^ Int.toString column ^ ")"
         , fn text => "**" ^ text ^ "**" )
         mode block)

  fun arenaHints state uri text startLine endLine =
    let
      val analysis = analysisFor state uri
      val source = Source.create text
      fun hint (block : ArenaReport.block) =
        let
          val line = Pos.line (#start (#span block))
          val column = Pos.column (#start (#span block))
          val lineText = Source.lineText (source, line)
        in
          if line - 1 < startLine orelse line - 1 > endLine then NONE
          else
            SOME
              (obj
                 [ ("position",
                     obj
                       [ ("line", int (line - 1))
                       , ("character",
                           int (byteColumnToCharacter (lineText, column + 5)))
                       ])
                 , ("label", str (ArenaReport.headline block))
                 , ("paddingLeft", bool true)
                 , ("tooltip",
                     obj
                       [ ("kind", str "markdown")
                       , ("value", str (arenaMarkdown uri (#mode analysis) block))
                       ])
                 ])
        end
    in
      List.mapPartial hint (#arenas analysis)
    end

  fun argumentHints state uri text checked entry startLine endLine =
    let
      val path = #path entry
      val fileId = #id entry
      val modules = indexFor state path
      val program = #1 (parsedOf state uri text)
      val lookup = LspCompletion.parameterLookupFor modules program (SOME checked)
      val constructors = LspTypes.dbOf modules program
      val seen = ref []
      val hints = ref []
      fun add (argument, info) =
        let
          val name = #name info
          val start = #start (Ast.exprSpan argument)
          val line = Pos.line start - 1
        in
          if name = "" orelse Pos.file start <> fileId then ()
          else if line < startLine orelse line > endLine then ()
          else
            let
              val position = lspPosition entry start
              val character =
                case member "character" position of
                  SOME value => intOf value
                | NONE => 0
              val key = (line, character, name)
            in
              if List.exists (fn candidate => candidate = key) (!seen) then ()
              else
                ( seen := key :: !seen
                ; hints :=
                    obj
                      [ ("position", position)
                      , ("label", str (name ^ ":"))
                      , ("kind", int 2)
                      , ("paddingRight", bool true)
                      ] :: !hints
                )
            end
        end
      fun walkBinding binding =
        case binding of
          Ast.BVal {value, ...} => walk value
        | Ast.BPat {value, ...} => walk value
        | Ast.BFun {body, ...} => walk body
        | Ast.BGuard {value, fallback, ...} =>
            (walk value; walk fallback)
      and walk expression =
        case expression of
          Ast.ECall (name, arguments, _) =>
            let
              val isConstructor =
                Option.isSome
                  (LspTypes.findConstructor constructors
                     (LspCompletion.baseName name))
            in
              if isConstructor then ()
              else
                (case lookup name of
                   SOME infos =>
                     if List.length infos = List.length arguments then
                       ListPair.app
                         (fn (argument, info) => add (argument, info))
                         (arguments, infos)
                     else ()
                 | NONE => ());
              List.app walk arguments
            end
        | Ast.ELet (bindings, body, _) =>
            (List.app walkBinding bindings; walk body)
        | Ast.EArena (bindings, body, _) =>
            (List.app walkBinding bindings; walk body)
        | Ast.ECase (scrutinee, branches, _) =>
            (walk scrutinee;
             List.app
               (fn (_, guard, body) =>
                 ((case guard of SOME condition => walk condition | NONE => ());
                  walk body))
               branches)
        | Ast.EIf (condition, thenBranch, elseBranch, _) =>
            (walk condition; walk thenBranch; walk elseBranch)
        | Ast.EBin (_, left, right, _) => (walk left; walk right)
        | Ast.ENeg (operand, _) => walk operand
        | Ast.ERecord (fields, _) =>
            List.app (fn (_, value) => walk value) fields
        | Ast.EField (_, base, _) => walk base
        | Ast.ETuple (values, _) => List.app walk values
        | Ast.ETupleIndex (_, base, _) => walk base
        | Ast.EBorrow (_, _, operand, _) => walk operand
        | Ast.EFn (_, _, body, _) => walk body
        | _ => ()
    in
      List.app
        (fn declaration =>
          case declaration of
            Ast.DFun {body, ...} => walk body
          | Ast.DFunClause {body, ...} => walk body
          | _ => ())
        program;
      List.rev (!hints)
    end

  fun hintsFor state uri startLine _ endLine _ =
    let
      val path = uriToPath uri
      val empty = arr []
    in
      case checkedOf state uri of
        NONE => empty
      | SOME checked =>
          (case entryForFile (#sources checked) (fileIdOf checked path) of
             NONE => empty
           | SOME entry =>
               let
                 val fileId = fileIdOf checked path
                 val annotated = annotatedBindings (#program checked)
                 val names = bindingNames (#program checked)
                 val seen = ref []
                 val nameOf = nameLookup names
                 fun add (span, ty) =
                   let val start = #start span in
                     if Pos.file start <> fileId then NONE
                     else if nameOf span = "" then NONE
                     else if List.exists (fn candidate => candidate = annotatedStart span) annotated then NONE
                     else if not (List.null (Types.freeVars ty)) then NONE
                     else
                       let
                         val line = Pos.line start - 1
                         val text = typeText ty
                         val character =
                           case member "character" (positionAfterName entry (span, nameOf span)) of
                             SOME value => intOf value
                           | NONE => 0
                         val key = (line, character, text)
                       in
                         if line < startLine orelse line > endLine then NONE
                         else if List.exists (fn candidate => candidate = key) (!seen) then NONE
                         else
                           ( seen := key :: !seen
                           ; SOME
                               (obj
                                  [ ("position",
                                      obj [ ("line", int line), ("character", int character) ])
                                  , ("label", str (": " ^ text))
                                  , ("kind", int 1)
                                  , ("paddingLeft", bool false)
                                  ])
                           )
                       end
                   end

                  fun addReturn declaration =
                    case declaration of
                      Ast.DFun {name, nameSpan, result = NONE, span, ...} =>
                        if Pos.file (#start nameSpan) <> fileId then NONE
                        else
                          (case List.find
                                  (fn (candidate, _) => candidate = name)
                                  (#functions checked) of
                             NONE => NONE
                           | SOME (_, funSignature) =>
                               if not (List.null (Types.freeVars (#result funSignature)))
                               then NONE
                               else
                               (case paramsEndOffset entry nameSpan (#stop span) of
                                  NONE => NONE
                                | SOME offset =>
                                    let
                                      val (line, character) =
                                        positionOfOffset entry offset
                                      val line = line - 1
                                      val text = typeText (#result funSignature)
                                      val key = (line, character, text)
                                    in
                                      if line < startLine orelse line > endLine then NONE
                                      else if List.exists
                                                (fn candidate => candidate = key)
                                                (!seen) then NONE
                                      else
                                        ( seen := key :: !seen
                                        ; SOME
                                            (obj
                                               [ ("position",
                                                   obj
                                                     [ ("line", int line)
                                                     , ("character", int character)
                                                     ])
                                               , ("label", str (": " ^ text))
                                               , ("kind", int 1)
                                               , ("paddingLeft", bool false)
                                               ])
                                        )
                                    end))
                    | _ => NONE
                  val text =
                    case findDoc state uri of
                      SOME doc => #text doc
                    | NONE => getOpt (Driver.readFile path, "")
                  val raw = List.mapPartial add (#bindingTypes checked) @
                            List.mapPartial addReturn (#program checked) @
                            argumentHints state uri text checked entry
                              startLine endLine @
                            arenaHints state uri text startLine endLine
               in
                 arr raw
               end)
    end

  fun declarationHover (text, comments, program, word) markdown =
    let
      val signatureText =
        case LspDocs.signatureByName (text, program, word) of
          SOME value => SOME value
        | NONE => LspDocs.constructorSignature program word
      val doc =
        case LspDocs.docByNameWith (text, comments, program, word) of
          SOME value => SOME value
        | NONE => LspDocs.constructorDocWith (text, comments, program, word)
    in
      case signatureText of
        NONE => NONE
      | SOME value =>
          let
            val parts =
              ["```yack\n" ^ value ^ "\n```"] @
              (case doc of SOME body => [body] | NONE => [])
          in
            SOME
              (obj
                 [ ("contents",
                     obj
                       [ ("kind", str "markdown")
                       , ("value", str (String.concatWith "\n\n" parts))
                       ])
                 ])
          end
    end

  fun hoverAt state uri line0 character0 =
    let
      val path = uriToPath uri
      val document = findDoc state uri
      val text = case document of SOME doc => #text doc | NONE => ""
      val source = Source.create text
      val lineText = Source.lineText (source, line0 + 1)
      val index = characterToByteColumn (lineText, character0)
      val (qualifier, word) = qualifiedWordAt (lineText, index)
      val modules = indexFor state path
      val (currentProgram, currentComments) = parsedOf state uri text
      fun arenaHover () =
        if word <> "arena" then NONE
        else
          let val analysis = analysisFor state uri in
            case ArenaReport.at (#arenas analysis, line0 + 1, index + 1) of
              NONE => NONE
            | SOME block =>
                SOME
                  (obj
                     [ ("contents",
                         obj
                           [ ("kind", str "markdown")
                           , ("value", str (arenaMarkdown uri (#mode analysis) block))
                           ])
                     ])
          end
      fun borrowHover () =
        if word <> "borrow" then NONE
        else
          SOME
            (obj
               [ ("contents",
                   obj
                     [ ("kind", str "markdown")
                     , ("value",
                         str ("`borrow T` is a shared borrow: the callee reads `T` " ^
                              "without taking ownership, cannot store it, and the " ^
                              "caller keeps the value alive for the call."))
                     ])
               ])
      fun current () =
        declarationHover (text, currentComments, currentProgram, word) (str "")
      fun moduleHover module =
        let
          val comments = LspDocs.commentsOf (#text module)
        in
          declarationHover (#text module, comments, #program module, word) (str "")
        end

      fun qualifiedModule () =
        case qualifier of
          NONE => NONE
        | SOME name =>
            (case List.find
                    (fn (candidate, _) => candidate = name)
                    (LspCompletion.importsFromText text) of
               SOME (_, key) =>
                 (case LspCompletion.moduleFor state path key of
                    SOME module => moduleHover module
                  | NONE => NONE)
             | NONE =>
                 (case List.find
                         (fn module =>
                           LspCompletion.moduleQualifier (#key module) = name)
                         modules of
                    SOME module => moduleHover module
                  | NONE => NONE))
      fun fromModules () =
        let
          fun loop remaining =
            case remaining of
              [] => NONE
            | module :: rest =>
                (case moduleHover module of
                   SOME answer => SOME answer
                 | NONE => loop rest)
        in
          loop modules
        end
      fun declaration () =
        case qualifier of
          SOME _ => qualifiedModule ()
        | NONE =>
            (case current () of
               SOME answer => SOME answer
             | NONE => fromModules ())

      fun builtin () =
        case qualifier of
          SOME _ => NONE
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
                  (case LspCompletion.signatureIndex (SOME info) word of
                     NONE => NONE
                   | SOME (_, sigInfo) =>
                       SOME
                         (obj
                            [ ("contents",
                                obj
                                  [ ("kind", str "markdown")
                                  , ("value",
                                      str
                                        ("```yack\\n" ^
                                         LspCompletion.checkedSignatureText sigInfo ^
                                         "\\n```"))
                                  ])
                            ]))
            end
    in
      if word = "" then jsonNull
      else
        case arenaHover () of
          SOME answer => answer
        | NONE =>
        case borrowHover () of
          SOME answer => answer
        | NONE =>
        case declaration () of
          SOME answer => answer
        | NONE =>
        case builtin () of
          SOME answer => answer
        | NONE =>
            (case checkedOf state uri of
                    NONE => jsonNull
                  | SOME checked =>
                      let
                        val fileId = fileIdOf checked path
                        val entry =
                          case entryForFile (#sources checked) fileId of
                            SOME entry => entry
                          | NONE => raise Fail "no source for the current file"
                        val line = line0 + 1
                        val column = index + 1
                        fun width span =
                          Pos.offset (#stop span) - Pos.offset (#start span)
                        fun candidates pairs =
                          List.mapPartial
                            (fn (span, ty) =>
                              if Pos.file (#start span) = fileId andalso
                                 spanContains span (line, column)
                              then SOME (span, ty) else NONE)
                            pairs
                        val found =
                          case candidates (#bindingTypes checked) @
                               candidates (#expressionTypes checked) of
                            [] => NONE
                          | values =>
                              SOME (List.foldl
                                (fn ((span, ty), best as (bestSpan, _)) =>
                                  if width span < width bestSpan then (span, ty) else best)
                                (hd values) (tl values))
                      in
                        case found of
                          NONE => jsonNull
                        | SOME (span, ty) =>
                            obj
                              [ ("contents",
                                  obj
                                    [ ("kind", str "markdown")
                                    , ("value",
                                        str ("```yack\n" ^ typeText ty ^ "\n```"))
                                    ])
                              , ("range", lspRange entry span)
                              ]
                      end)
    end

end
