structure LspDocs =
struct
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

  fun allSpace text =
    let
      val size = String.size text
      fun loop index =
        if index >= size then true
        else if Char.isSpace (String.sub (text, index)) then loop (index + 1)
        else false
    in
      loop 0
    end

  fun adjacentWhitespace text =
    allSpace text andalso
    not (contains text "\n\n") andalso
    not (contains text "\r\n\r\n")

  fun stripPrefix prefix text =
    if String.isPrefix prefix text then
      String.extract (text, String.size prefix, NONE)
    else text

  fun stripSuffix suffix text =
    if String.isSuffix suffix text then
      String.substring (text, 0, String.size text - String.size suffix)
    else text

  fun trimLine text =
    let
      val text = LspText.trim text
    in
      if String.isPrefix "* " text then
        LspText.trim (String.extract (text, 2, NONE))
      else if String.isPrefix "*" text then
        LspText.trim (String.extract (text, 1, NONE))
      else text
    end

  fun cleanComment text =
    let
      val inner = stripSuffix "*)" (stripPrefix "(*" text)
      val lines = List.map trimLine (String.fields (fn c => c = #"\n") inner)
      fun dropEmptyFront values =
        case values of
          "" :: rest => dropEmptyFront rest
        | _ => values
      val lines = dropEmptyFront lines
      val lines = List.rev (dropEmptyFront (List.rev lines))
    in
      String.concatWith "\n" lines
    end

  type comment =
    { start : int
    , stop : int
    , text : string
    }

  fun commentsOf text =
    let val lexed = Lexer.lex text in
      List.map
        (fn (span, value) =>
          { start = Pos.offset (#start span)
          , stop = Pos.offset (#stop span)
          , text = value
          })
        (#comments lexed)
    end

  fun sortByStart comments =
    let
      fun insert (comment, []) = [comment]
        | insert (comment, other :: rest) =
            if #start comment <= #start other
            then comment :: other :: rest
            else other :: insert (comment, rest)
    in
      List.foldl (fn (comment, sorted) => insert (comment, sorted)) [] comments
    end

  fun lastAdjacent (text, comments : comment list, offset) =
    let
      fun loop (best, remaining) =
        case remaining of
          [] => best
        | comment :: rest =>
            if #stop comment <= offset andalso
               adjacentWhitespace
                 (String.substring (text, #stop comment, offset - #stop comment))
            then loop (SOME comment, rest)
            else loop (best, rest)
    in
      loop (NONE, comments)
    end

  fun decorative text =
    let
      fun allowed character =
        Char.isSpace character orelse
        character = #"-" orelse character = #"=" orelse
        character = #"*" orelse character = #"_" orelse
        character = #"~" orelse character = #"#" orelse
        character = #"/"
      val size = String.size text
      fun loop index =
        if index >= size then true
        else if allowed (String.sub (text, index)) then loop (index + 1)
        else false
    in
      loop 0
    end

  fun docBeforeWith (text, rawComments : comment list, offset) =
    let
      val comments = sortByStart rawComments
      val last = lastAdjacent (text, comments, offset)
    in
      case last of
        NONE => NONE
      | SOME comment =>
          let val cleaned = cleanComment (#text comment) in
            if cleaned = "" orelse decorative cleaned then NONE else SOME cleaned
          end
    end

  fun docBefore (text, offset) =
    docBeforeWith (text, commentsOf text, offset)

  fun declName declaration =
    case declaration of
      Ast.DFun {name, ...} => SOME name
    | Ast.DFunClause {name, ...} => SOME name
    | Ast.DType {name, ...} => SOME name
    | Ast.DAlias {name, ...} => SOME name
    | Ast.DDatatype {name, ...} => SOME name
    | _ => NONE

  fun paramsText params =
    String.concatWith ", "
      (List.map
         (fn {name, annot, span} =>
           case annot of
             NONE => name
           | SOME ty => name ^ " : " ^ Ast.tyToString ty)
         params)

  fun patternText pattern =
    case pattern of
      Ast.PVar (name, _) => name
    | Ast.PWildcard _ => "_"
    | Ast.PInt (value, _) => Int64.toString value
    | Ast.PBool (value, _) => if value then "true" else "false"
    | Ast.PString (literal, NONE, _) => "\"" ^ literal ^ "\""
    | Ast.PString (literal, SOME rest, _) =>
        "\"" ^ literal ^ "\" ^ " ^ patternText rest
    | Ast.PBytes (bytes, NONE, _) =>
        "<<" ^ Int.toString (String.size bytes) ^ " bytes>>"
    | Ast.PBytes (bytes, SOME rest, _) =>
        "<<" ^ Int.toString (String.size bytes) ^ " bytes, " ^
        patternText rest ^ ">>"
    | Ast.PConstructor (name, argument, _) =>
        (case argument of
           NONE => name
         | SOME subpattern => name ^ " (" ^ patternText subpattern ^ ")")
    | Ast.PTuple (patterns, _) =>
        "(" ^ String.concatWith ", " (List.map patternText patterns) ^ ")"
    | Ast.PRecord (fields, _) =>
        "{ " ^
        String.concatWith ", "
          (List.map (fn (field, subpattern) =>
             field ^ " = " ^ patternText subpattern) fields) ^
        " }"

  fun patternParamsText params =
    String.concatWith ", "
      (List.map
         (fn (pattern, annot) =>
           patternText pattern ^
           (case annot of
              NONE => ""
            | SOME ty => " : " ^ Ast.tyToString ty))
         params)

  fun parameterLabels declaration =
    let
      fun annotated annot =
        case annot of
          NONE => ""
        | SOME ty => " : " ^ Ast.tyToString ty
    in
      case declaration of
        Ast.DFun {params, ...} =>
          List.map (fn {name, annot, ...} => name ^ annotated annot) params
      | Ast.DFunClause {params, ...} =>
          List.map
            (fn (pattern, annot) => patternText pattern ^ annotated annot)
            params
      | _ => []
    end

  fun propertySuffix declared =
    case declared of
      NONE => ""
    | SOME property => " : " ^ Ast.propertyToString property

  fun datatypeParams params =
    case params of
      [] => ""
    | _ =>
        let
          fun item (name, equality) =
            (if equality then "''" else "'") ^ name
        in
          (case params of
             [single] => item single ^ " "
           | _ => "(" ^ String.concatWith ", " (List.map item params) ^ ") ")
        end

  fun declStart declaration =
    Pos.offset (#start (Ast.declSpan declaration))

  fun docForDecl (text, declaration) =
    docBefore (text, declStart declaration)

  fun docForDeclWith (text, comments, declaration) =
    docBeforeWith (text, comments, declStart declaration)

  fun findDecl program name =
    List.find
      (fn declaration =>
        case declName declaration of
          SOME candidate => candidate = name
        | NONE => false)
      program

  fun expandType (program, visited) syntax =
    let
      fun fieldText (field, ty) =
        field ^ " : " ^ expandType (program, visited) ty
      fun recordText fields =
        if List.null fields then "{}"
        else "{ " ^ String.concatWith ", " (List.map fieldText fields) ^ " }"
      fun argumentText argument =
        let val text = expandType (program, visited) argument in
          case argument of
            Ast.TyTuple _ => "(" ^ text ^ ")"
          | Ast.TyApp _ => "(" ^ text ^ ")"
          | _ => text
        end
    in
      case syntax of
        Ast.TyName name =>
          if List.exists (fn candidate => candidate = name) visited then name
          else
            (case findDecl program name of
               SOME (Ast.DType {fields, ...}) =>
                 expandType (program, name :: visited) (Ast.TyRecord fields)
             | SOME (Ast.DAlias {params = [], body, ...}) =>
                 expandType (program, name :: visited) body
             | _ => name)
      | Ast.TyRecord fields => recordText fields
      | Ast.TyTuple types =>
          String.concatWith " * "
            (List.map (fn ty => expandType (program, visited) ty) types)
      | Ast.TyApp (name, argument) =>
          argumentText argument ^ " " ^ name
      | Ast.TyBorrow (mutable, ampersand, inner, provenance) =>
          (if ampersand then "&" else "borrow ") ^
          (if mutable then "mut " else "") ^
          expandType (program, visited) inner ^
          (case provenance of
             NONE => ""
           | SOME (Ast.ProvIndex index) => " from " ^ Int.toString index
           | SOME (Ast.ProvName name) => " from " ^ name)
      | Ast.TyConsume (bang, inner) =>
          (if bang then "!" else "consume ") ^
          expandType (program, visited) inner
      | Ast.TyFun (params, result) =>
          (case params of
             [param] => expandType (program, visited) param
           | _ =>
               "(" ^
               String.concatWith ", "
                 (List.map (fn ty => expandType (program, visited) ty) params) ^
               ")") ^
          " -> " ^ expandType (program, visited) result
      | _ => Ast.tyToString syntax
    end

  fun signatureOfDeclIn (program, declaration) =
    case declaration of
      Ast.DFun {name, params, result, ...} =>
        SOME
          ("fun " ^ name ^ " (" ^ paramsText params ^ ")" ^
           (case result of
              NONE => ""
            | SOME ty => " : " ^ expandType (program, []) ty))
    | Ast.DFunClause {name, params, result, ...} =>
        SOME
          ("fun " ^ name ^ " (" ^ patternParamsText params ^ ")" ^
           (case result of
              NONE => ""
            | SOME ty => " : " ^ expandType (program, []) ty))
    | Ast.DType {name, declared, fields, ...} =>
        SOME
          ("type " ^ name ^ propertySuffix declared ^ " = " ^
           expandType (program, [name]) (Ast.TyRecord fields))
    | Ast.DAlias {name, params, body, ...} =>
        SOME
          ("type " ^ datatypeParams params ^ name ^ " = " ^
           expandType (program, [name]) body)
    | Ast.DDatatype {name, params, declared, constructors, ...} =>
        let
          fun constructor (constructorName, argument) =
            case argument of
              NONE => constructorName
            | SOME ty =>
                constructorName ^ " of " ^
                expandType (program, [name]) ty
        in
          SOME
            ("datatype " ^ datatypeParams params ^ name ^ propertySuffix declared ^
             " = " ^ String.concatWith " | " (List.map constructor constructors))
        end
    | _ => NONE

  fun signatureByName (text, program, name) =
    case findDecl program name of
      SOME declaration => signatureOfDeclIn (program, declaration)
    | NONE => NONE

  type declIndex =
    { program : Ast.program
    , size : int
    , buckets : (string * Ast.decl) list array
    }

  fun hashName size name =
    let
      val hash =
        CharVector.foldl
          (fn (character, value) =>
            (value * 33 + Char.ord character) mod size)
          0 name
    in
      if hash < 0 then hash + size else hash
    end

  fun indexOf program =
    let
      val size = Int.max (64, 2 * List.length program)
      val buckets = Array.array (size, [])
      fun insert declaration =
        case declName declaration of
          NONE => ()
        | SOME name =>
            let
              val index = hashName size name
              val bucket = Array.sub (buckets, index)
            in
              Array.update (buckets, index, (name, declaration) :: bucket)
            end
      val _ = List.app insert program
    in
      { program = program, size = size, buckets = buckets }
    end

  fun findDeclIn (index : declIndex, name) =
    let
      val {size, buckets, ...} = index
      val bucket = Array.sub (buckets, hashName size name)
    in
      case List.find (fn (candidate, _) => candidate = name) bucket of
        NONE => NONE
      | SOME (_, declaration) => SOME declaration
    end

  fun signatureIn (index : declIndex, name) =
    case findDeclIn (index, name) of
      SOME declaration => signatureOfDeclIn (#program index, declaration)
    | NONE => NONE

  fun docInWith (text, comments, index : declIndex, name) =
    case findDeclIn (index, name) of
      SOME declaration => docForDeclWith (text, comments, declaration)
    | NONE => NONE

  fun docByName (text, program, name) =
    case findDecl program name of
      SOME declaration => docForDecl (text, declaration)
    | NONE => NONE

  fun docByNameWith (text, comments, program, name) =
    case findDecl program name of
      SOME declaration => docForDeclWith (text, comments, declaration)
    | NONE => NONE

  fun firstSome values =
    case List.find (fn value => Option.isSome value) values of
      SOME (SOME value) => SOME value
    | _ => NONE

  fun constructorSignature program name =
    firstSome
      (List.map
         (fn declaration =>
           case declaration of
             Ast.DDatatype {constructors, ...} =>
               (case List.find (fn (candidate, _) => candidate = name) constructors of
                  NONE => NONE
                | SOME (_, argument) =>
                    (case argument of
                       NONE => SOME name
                     | SOME ty => SOME (name ^ " of " ^ Ast.tyToString ty)))
           | _ => NONE)
         program)

  fun constructorDoc (text, program, name) =
    firstSome
      (List.map
         (fn declaration =>
           case declaration of
             Ast.DDatatype {constructors, ...} =>
               if List.exists (fn (candidate, _) => candidate = name) constructors
               then docForDecl (text, declaration)
               else NONE
           | _ => NONE)
         program)

  fun constructorDocWith (text, comments, program, name) =
    firstSome
      (List.map
         (fn declaration =>
           case declaration of
             Ast.DDatatype {constructors, ...} =>
               if List.exists (fn (candidate, _) => candidate = name) constructors
               then docForDeclWith (text, comments, declaration)
               else NONE
           | _ => NONE)
         program)
end
