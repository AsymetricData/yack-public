structure LspAst =
struct
  open LspText

  fun patternNames pattern =
    case pattern of
      Ast.PVar (name, _) => [name]
    | Ast.PWildcard _ => []
    | Ast.PInt _ => []
    | Ast.PBool _ => []
    | Ast.PString (_, rest, _) =>
        (case rest of
           SOME inner => patternNames inner
         | NONE => [])
    | Ast.PBytes (_, rest, _) =>
        (case rest of
           SOME inner => patternNames inner
         | NONE => [])
    | Ast.PConstructor (_, NONE, _) => []
    | Ast.PConstructor (_, SOME inner, _) => patternNames inner
    | Ast.PTuple (patterns, _) => List.concat (List.map patternNames patterns)
    | Ast.PRecord (fields, _) =>
        List.concat (List.map (fn (_, subpattern) => patternNames subpattern) fields)

  fun programOfText text =
    let val lexed = Lexer.lex text in
      if List.null (#tokens lexed) then []
      else #program (Parser.parse (#tokens lexed))
    end

  fun annotationsFromText text =
    let
      fun isWordChar character =
        Char.isAlphaNum character orelse character = #"_" orelse
        character = #"'"
      fun wordBefore line index =
        let
          fun skip position =
            if position > 0 andalso Char.isSpace (String.sub (line, position - 1))
            then skip (position - 1) else position
          fun collect (position, chars) =
            if position > 0 andalso isWordChar (String.sub (line, position - 1))
            then collect (position - 1, String.sub (line, position - 1) :: chars)
            else String.implode (List.rev chars)
          val stop = skip index
        in
          collect (stop, [])
        end
      fun textAfter line index =
        let
          val size = String.size line
          fun stop position =
            if position >= size then size
            else case String.sub (line, position) of
              #")" => position
            | #"," => position
            | #"=" => position
            | #"{" => position
            | #"}" => position
            | #"|" => position
            | _ => stop (position + 1)
          fun skip position =
            if position < size andalso Char.isSpace (String.sub (line, position))
            then skip (position + 1) else position
          val start = skip (index + 1)
        in
          String.substring (line, start, stop start - start)
        end
      fun lastWord value =
        let
          val value = trim value
          val size = String.size value
          fun loop index =
            if index > 0 andalso isWordChar (String.sub (value, index - 1))
            then loop (index - 1) else index
          val start = loop size
        in
          String.substring (value, start, size - start)
        end
      fun containsValue haystack needle =
        let
          val size = String.size haystack
          val width = String.size needle
          fun loop index =
            if index + width > size then false
            else if String.substring (haystack, index, width) = needle then true
            else loop (index + 1)
        in
          loop 0
        end
      fun parseLine line =
        let val line = trim line in
          if String.isPrefix "fun " line orelse
             String.isPrefix "val " line orelse
             String.isPrefix "let val " line orelse
             containsValue line " val "
          then
            let
              val size = String.size line
              fun scan (index, acc) =
                if index >= size then List.rev acc
                else if String.sub (line, index) = #":" then
                  let
                    val name = wordBefore line index
                    val ty = lastWord (textAfter line index)
                  in
                    if name = "" orelse ty = "" then scan (index + 1, acc)
                    else scan (index + 1, (name, ty) :: acc)
                  end
                else scan (index + 1, acc)
            in
              scan (0, [])
            end
          else []
        end
    in
      List.concat
        (List.map parseLine (String.fields (fn c => c = #"\n") text))
    end

  fun annotationsOf program =
    let
      val annotations = ref []
      fun add (name, ty) = annotations := (name, Ast.tyToString ty) :: !annotations
      fun walk expression =
        case expression of
          Ast.ELet (bindings, body, _) =>
            (List.app
               (fn binding =>
                 case binding of
                   Ast.BVal {name, annot = SOME ty, value, span} =>
                     (add (name, ty); walk value)
                 | Ast.BVal {name, annot = NONE, value, span} => walk value
                 | Ast.BPat {pattern, value, span} =>
                     (walkPattern pattern; walk value)
                 | Ast.BFun {body, ...} => walk body
                 | Ast.BGuard {pattern, value, fallback, span} =>
                     (case pattern of
                        SOME pattern => walkPattern pattern
                      | NONE => ();
                      walk value;
                      walk fallback))
               bindings;
             walk body)
        | Ast.EArena (bindings, body, _) =>
            (List.app
               (fn binding =>
                 case binding of
                   Ast.BVal {name, annot = SOME ty, value, span} =>
                     (add (name, ty); walk value)
                 | Ast.BVal {name, annot = NONE, value, span} => walk value
                 | Ast.BPat {pattern, value, span} =>
                     (walkPattern pattern; walk value)
                 | Ast.BFun {body, ...} => walk body
                 | Ast.BGuard {pattern, value, fallback, span} =>
                     (case pattern of
                        SOME pattern => walkPattern pattern
                      | NONE => ();
                      walk value;
                      walk fallback))
               bindings;
             walk body)
        | Ast.ECase (scrutinee, branches, _) =>
            (walk scrutinee;
             List.app
               (fn (pattern, guard, body) =>
                 (walkPattern pattern;
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
      and walkPattern pattern =
        case pattern of
          Ast.PString (_, SOME inner, _) => walkPattern inner
        | Ast.PBytes (_, SOME inner, _) => walkPattern inner
        | Ast.PConstructor (_, SOME inner, _) => walkPattern inner
        | Ast.PTuple (patterns, _) => List.app walkPattern patterns
        | Ast.PRecord (fields, _) =>
            List.app (fn (_, subpattern) => walkPattern subpattern) fields
        | _ => ()
    in
      List.app
        (fn declaration =>
          case declaration of
            Ast.DFun {name, params, result, body, ...} =>
              (List.app
                 (fn {name, annot = SOME ty, span} => add (name, ty)
                   | _ => ())
                 params;
               walk body)
          | Ast.DFunClause {params, body, ...} =>
              (List.app
                 (fn (pattern, annot) =>
                   case annot of
                     NONE => ()
                   | SOME ty =>
                       List.app (fn name => add (name, ty))
                         (patternNames pattern))
                 params;
               walk body)
          | _ => ())
        program;
      !annotations
    end

  fun exportsOf program =
    List.foldl
      (fn (declaration, exports) =>
        case declaration of
          Ast.DFun {name, params, result, body, ...} => ("function", name) :: exports
        | Ast.DFunClause {name, ...} => ("function", name) :: exports
        | Ast.DType {name, declared, fields, span} => ("type", name) :: exports
        | Ast.DAlias {name, params, body, span} => ("type", name) :: exports
        | Ast.DDatatype {name, params, declared, constructors, span} =>
            ("type", name) ::
            List.map (fn (constructor, _) => ("constructor", constructor)) constructors
            @ exports
        | _ => exports)
      []
      program

  fun importsOf program =
    List.mapPartial
      (fn Ast.DImport directive => SOME directive | _ => NONE)
      program

  fun desugarText text =
    let val lexed = Lexer.lex text in
      if not (List.null (#diagnostics lexed)) then NONE
      else
        let val parsed = Parser.parse (#tokens lexed) in
          if not (List.null (#diagnostics parsed)) then NONE
          else SOME (Desugar.desugarProgram (#program parsed))
        end
    end

  type localBinding =
    { name : string
    , kind : string
    , depth : int
    , span : Diag.span option
    }

  fun localBindings program =
    let
      val names = ref []
      fun add (name, kind, span) =
        names :=
          ({ name = name, kind = kind, depth = 1, span = span }
           : localBinding) :: !names
      fun addPattern pattern =
        List.app
          (fn name => add (name, "variable", SOME (Ast.patternSpan pattern)))
          (patternNames pattern)
      fun walk expression =
        case expression of
          Ast.ELet (bindings, body, _) =>
            (List.app
               (fn binding =>
                 case binding of
                   Ast.BVal {name, value, span, ...} =>
                     (add (name, "variable", SOME span); walk value)
                 | Ast.BPat {pattern, value, ...} =>
                     (addPattern pattern; walk value)
                 | Ast.BFun {name, body, span, ...} =>
                     (add (name, "function", SOME span); walk body)
                 | Ast.BGuard {pattern, value, fallback, ...} =>
                     (case pattern of
                        SOME pattern => addPattern pattern
                      | NONE => ();
                      walk value;
                      walk fallback))
               bindings;
             walk body)
        | Ast.EArena (bindings, body, _) =>
            (List.app
               (fn binding =>
                 case binding of
                   Ast.BVal {name, value, span, ...} =>
                     (add (name, "variable", SOME span); walk value)
                 | Ast.BPat {pattern, value, ...} =>
                     (addPattern pattern; walk value)
                 | Ast.BFun {name, body, span, ...} =>
                     (add (name, "function", SOME span); walk body)
                 | Ast.BGuard {pattern, value, fallback, ...} =>
                     (case pattern of
                        SOME pattern => addPattern pattern
                      | NONE => ();
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
        | Ast.ERecord (fields, _) =>
            List.app (fn (_, value) => walk value) fields
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
              (List.app
                 (fn {name, span, ...} => add (name, "variable", SOME span))
                 params;
               walk body)
          | Ast.DFunClause {params, body, ...} =>
              (List.app (fn (pattern, _) => addPattern pattern) params;
               walk body)
          | _ => ())
        program;
      !names
    end

  fun bindingsAt (program, offset) =
    let
      val found = ref false
      fun contains span =
        Pos.offset (#start span) <= offset andalso
        offset <= Pos.offset (#stop span)
      fun bindingSpan binding =
        case binding of
          Ast.BVal {span, ...} => span
        | Ast.BPat {span, ...} => span
        | Ast.BFun {span, ...} => span
        | Ast.BGuard {span, ...} => span
      fun add (depth, name, kind, span, acc) =
        if name = "" then acc
        else
          ({ name = name, kind = kind, depth = depth, span = SOME span }
           : localBinding) :: acc
      fun addPattern (depth, pattern, acc) =
        List.foldl
          (fn (name, acc) =>
             add (depth, name, "variable", Ast.patternSpan pattern, acc))
          acc (patternNames pattern)
      fun addParams (depth, params, acc) =
        List.foldl
          (fn ({name, span, ...}, acc) =>
             add (depth, name, "variable", span, acc))
          acc params
      fun addClauseParams (depth, params, acc) =
        List.foldl
          (fn ((pattern, _), acc) => addPattern (depth, pattern, acc))
          acc params
      fun addBinding (depth, binding, acc) =
        case binding of
          Ast.BVal {name, span, ...} =>
            add (depth, name, "variable", span, acc)
        | Ast.BPat {pattern, ...} => addPattern (depth, pattern, acc)
        | Ast.BFun {name, span, ...} =>
            add (depth, name, "function", span, acc)
        | Ast.BGuard {pattern, ...} =>
            (case pattern of
               SOME pattern => addPattern (depth, pattern, acc)
             | NONE => acc)
      fun walk (expression, depth, acc) =
        if not (contains (Ast.exprSpan expression)) then acc
        else
          case expression of
            Ast.ELet (bindings, body, _) =>
              walkLet (bindings, body, depth, acc)
          | Ast.EArena (bindings, body, _) =>
              walkLet (bindings, body, depth, acc)
          | Ast.ECase (scrutinee, branches, _) =>
              if contains (Ast.exprSpan scrutinee) then
                walk (scrutinee, depth, acc)
              else
                List.foldl
                  (fn ((pattern, guard, body), acc) =>
                     let
                       val branchDepth = depth + 1
                       val withPattern =
                         addPattern (branchDepth, pattern, acc)
                     in
                       case guard of
                         SOME condition =>
                           if contains (Ast.exprSpan condition) then
                             walk (condition, branchDepth, withPattern)
                           else if contains (Ast.exprSpan body) then
                             walk (body, branchDepth, withPattern)
                           else acc
                       | NONE =>
                           if contains (Ast.exprSpan body) then
                             walk (body, branchDepth, withPattern)
                           else acc
                     end)
                  acc branches
          | Ast.EIf (condition, thenBranch, elseBranch, _) =>
              if contains (Ast.exprSpan condition) then
                walk (condition, depth, acc)
              else if contains (Ast.exprSpan thenBranch) then
                walk (thenBranch, depth, acc)
              else if contains (Ast.exprSpan elseBranch) then
                walk (elseBranch, depth, acc)
              else acc
          | Ast.EBin (_, left, right, _) =>
              if contains (Ast.exprSpan left) then walk (left, depth, acc)
              else if contains (Ast.exprSpan right) then walk (right, depth, acc)
              else acc
          | Ast.ENeg (operand, _) => walk (operand, depth, acc)
          | Ast.ECall (_, arguments, _) =>
              List.foldl
                (fn (argument, acc) =>
                   if contains (Ast.exprSpan argument) then
                     walk (argument, depth, acc)
                   else acc)
                acc arguments
          | Ast.ERecord (fields, _) =>
              List.foldl
                (fn ((_, value), acc) =>
                   if contains (Ast.exprSpan value) then
                     walk (value, depth, acc)
                   else acc)
                acc fields
          | Ast.EField (_, base, _) => walk (base, depth, acc)
          | Ast.ETuple (values, _) =>
              List.foldl
                (fn (value, acc) =>
                   if contains (Ast.exprSpan value) then
                     walk (value, depth, acc)
                   else acc)
                acc values
          | Ast.ETupleIndex (_, base, _) => walk (base, depth, acc)
          | Ast.EBorrow (_, _, operand, _) => walk (operand, depth, acc)
          | Ast.EFn (params, _, body, _) =>
              if contains (Ast.exprSpan body) then
                walk (body, depth + 1,
                  addParams (depth + 1, params, acc))
              else acc
          | _ => acc
      and walkLet (bindings, body, depth, acc) =
        if contains (Ast.exprSpan body) then
          let
            val innerDepth = depth + 1
            val withBindings =
              List.foldl
                (fn (binding, acc) =>
                   addBinding (innerDepth, binding, acc))
                acc bindings
          in
            walk (body, innerDepth, withBindings)
          end
        else walkBindings (bindings, depth, acc)
      and walkBindings ([], _, acc) = acc
        | walkBindings (binding :: rest, depth, acc) =
            let val innerDepth = depth + 1 in
              if Pos.offset (#start (bindingSpan binding)) > offset then acc
              else if contains (bindingSpan binding) then
                (case binding of
                   Ast.BVal {value, ...} => walk (value, depth, acc)
                 | Ast.BPat {value, ...} => walk (value, depth, acc)
                 | Ast.BFun {params, body, ...} =>
                     walk (body, innerDepth,
                       addParams (innerDepth, params,
                         addBinding (innerDepth, binding, acc)))
                 | Ast.BGuard {value, fallback, ...} =>
                     if contains (Ast.exprSpan fallback) then
                       walk (fallback, depth, acc)
                     else walk (value, depth, acc))
              else
                walkBindings (rest, depth,
                  addBinding (innerDepth, binding, acc))
            end
      fun localOf declaration =
        case declaration of
          Ast.DFun {params, body, span, ...} =>
            if contains span then
              (found := true;
               walk (body, 1, addParams (1, params, [])))
            else []
        | Ast.DFunClause {params, body, span, ...} =>
            if contains span then
              (found := true;
               walk (body, 1, addClauseParams (1, params, [])))
            else []
        | _ => []
      val bindings = List.concat (List.map localOf program)
    in
      if !found then SOME bindings else NONE
    end

  fun annotatedStart (span : Diag.span) =
    (Pos.file (#start span), Pos.line (#start span), Pos.column (#start span))

  fun annotatedBindings program =
    let
      val spans = ref []
      fun add span = spans := annotatedStart span :: !spans
      fun walk expression =
        case expression of
          Ast.ELet (bindings, body, _) =>
            (List.app
               (fn binding =>
                 case binding of
                   Ast.BVal {annot = SOME _, span, value, name} =>
                     (add span; walk value)
                 | Ast.BVal {name, annot, value, span} => walk value
                 | Ast.BPat {pattern, value, span} => walk value
                 | Ast.BFun {body, ...} => walk body
                 | Ast.BGuard {value, fallback, ...} =>
                     (walk value; walk fallback))
               bindings;
             walk body)
        | Ast.EArena (bindings, body, _) =>
            (List.app
               (fn binding =>
                 case binding of
                   Ast.BVal {annot = SOME _, span, value, name} =>
                     (add span; walk value)
                 | Ast.BVal {name, annot, value, span} => walk value
                 | Ast.BPat {pattern, value, span} => walk value
                 | Ast.BFun {body, ...} => walk body
                 | Ast.BGuard {value, fallback, ...} =>
                     (walk value; walk fallback))
               bindings;
             walk body)
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
        | Ast.ECall (_, arguments, _) => List.app walk arguments
        | Ast.ERecord (fields, _) =>
            List.app (fn (_, value) => walk value) fields
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
                 (fn {name, annot = SOME _, span} => add span | _ => ())
                 params;
               walk body)
          | Ast.DFunClause {params, body, ...} =>
              (List.app
                 (fn (pattern, annot) =>
                   case annot of
                     SOME _ =>
                       List.app (fn _ => add (Ast.patternSpan pattern))
                         (patternNames pattern)
                   | NONE => ())
                 params;
               walk body)
          | _ => ())
        program;
      !spans
    end

  fun findCaseAt program fileId line column =
    let
      fun inFile span = Pos.file (#start span) = fileId
      fun firstOf values =
        case List.find Option.isSome values of
          SOME (SOME found) => SOME found
        | _ => NONE
      fun walk expression =
        let val span = Ast.exprSpan expression in
          if inFile span andalso spanContains span (line, column) then
            case expression of
              Ast.ECase _ => SOME expression
            | _ => walkChildren expression
          else walkChildren expression
        end
      and walkChildren expression =
        case expression of
          Ast.ELet (bindings, body, _) =>
            firstOf
              (List.map
                 (fn binding =>
                   case binding of
                     Ast.BVal {name, annot, value, span} => walk value
                   | Ast.BPat {pattern, value, span} => walk value
                   | Ast.BFun {body, ...} => walk body
                   | Ast.BGuard {value, fallback, ...} =>
                       firstOf [walk value, walk fallback])
                 bindings @ [walk body])
        | Ast.EArena (bindings, body, _) =>
            firstOf
              (List.map
                 (fn binding =>
                   case binding of
                     Ast.BVal {name, annot, value, span} => walk value
                   | Ast.BPat {pattern, value, span} => walk value
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
        | _ => NONE
    in
      firstOf
        (List.map
           (fn declaration =>
             case declaration of
               Ast.DFun {name, params, result, body, ...} => walk body
             | Ast.DFunClause {body, ...} => walk body
             | _ => NONE)
           program)
    end

end
