structure Fmt =
struct
  val defaultWidth = 88

  datatype doc =
      Empty
    | Text of string * int
    | Line
    | SoftLine
    | HardLine
    | Cat of doc * doc * int
    | Nest of int * doc
    | Group of doc * int

  val maxWidth = 1000000000

  fun docWidth Empty = 0
    | docWidth (Text (_, width)) = width
    | docWidth Line = 1
    | docWidth SoftLine = 0
    | docWidth HardLine = maxWidth
    | docWidth (Cat (_, _, width)) = width
    | docWidth (Nest (_, doc)) = docWidth doc
    | docWidth (Group (_, width)) = width

  fun text value = Text (value, String.size value)
  val empty = Empty
  val line = Line
  val soft = SoftLine
  val hard = HardLine

  fun cat left right =
    Cat (left, right, Int.min (maxWidth, docWidth left + docWidth right))

  fun nest amount doc = if amount = 0 then doc else Nest (amount, doc)
  fun group doc = Group (doc, docWidth doc)

  infix 6 <+>
  fun left <+> right = cat left right

  fun join separator docs =
    let
      fun loop acc [] = acc
        | loop acc [doc] = cat acc doc
        | loop acc (doc :: rest) =
            loop (cat (cat acc doc) separator) rest
    in
      loop Empty docs
    end

  fun mapInOrder f items =
    let
      fun loop acc [] = List.rev acc
        | loop acc (item :: rest) =
            let val doc = f item
            in loop (doc :: acc) rest end
    in
      loop [] items
    end

  fun spaces count =
    if count <= 0 then ""
    else String.implode (List.tabulate (count, fn _ => #" "))

  fun render target doc =
    let
      fun loop column stack acc =
        case stack of
          [] => List.rev acc
        | (indent, flat, current) :: rest =>
            case current of
              Empty => loop column rest acc
            | Text (value, _) =>
                loop (column + String.size value) rest (value :: acc)
            | Line =>
                if flat then
                  loop (column + 1) rest (" " :: acc)
                else
                  loop indent rest (spaces indent :: "\n" :: acc)
            | SoftLine =>
                if flat then
                  loop column rest acc
                else
                  loop indent rest (spaces indent :: "\n" :: acc)
            | HardLine =>
                loop indent rest (spaces indent :: "\n" :: acc)
            | Cat (left, right, _) =>
                loop column
                  ((indent, flat, left) :: (indent, flat, right) :: rest)
                  acc
            | Nest (amount, inner) =>
                loop column ((indent + amount, flat, inner) :: rest) acc
            | Group (inner, width) =>
                let
                  val flat' = flat orelse column + width <= target
                in
                  loop column ((indent, flat', inner) :: rest) acc
                end
    in
      String.concat (loop 0 [(0, false, doc)] [])
    end

  fun splitLines value =
    let
      val size = String.size value
      fun loop (index, start, acc) =
        if index >= size then
          List.rev (String.substring (value, start, index - start) :: acc)
        else if String.sub (value, index) = #"\n" then
          loop (index + 1, index + 1,
                String.substring (value, start, index - start) :: acc)
        else
          loop (index + 1, start, acc)
    in
      loop (0, 0, [])
    end

  fun trimStart value =
    let
      val size = String.size value
      fun loop index =
        if index < size andalso Char.isSpace (String.sub (value, index))
        then loop (index + 1)
        else index
      val start = loop 0
    in
      String.substring (value, start, size - start)
    end

  fun commentDoc value =
    case splitLines value of
      [] => Empty
    | first :: rest =>
        join hard
          (text first :: List.map (fn part => text (trimStart part)) rest)

  type state =
    { pending : (int * string) list ref
    , source : string
    }

  fun takeBefore (state : state) offset =
    let
      val pending = !(#pending state)
      fun loop acc rest =
        case rest of
          [] => (List.rev acc, [])
        | (entry as (start, _)) :: tail =>
            if start < offset then loop (entry :: acc) tail
            else (List.rev acc, rest)
      val (taken, remaining) = loop [] pending
    in
      #pending state := remaining;
      taken
    end

  fun wrapComments comments body =
    case comments of
      [] => body
    | _ =>
        join hard (List.map (fn (_, value) => commentDoc value) comments)
        <+> hard <+> body

  fun flushComments (state : state) =
    let val comments = !(#pending state)
        val _ = #pending state := []
    in
      join hard (List.map (fn (_, value) => commentDoc value) comments)
    end

  fun flushTail state =
    let
      val comments = !(#pending state)
      val _ = #pending state := []
    in
      case comments of
        [] => Empty
      | _ =>
          hard <+>
          join hard (List.map (fn (_, value) => commentDoc value) comments)
    end

  fun startOffset span = Pos.offset (#start span)

  fun sourceSlice (state : state) span =
    let
      val { start, stop } = span
      val first = Pos.offset start
      val last = Pos.offset stop
    in
      String.substring (#source state, first, last - first)
    end

  fun ppTyItem ty =
    case ty of
      Ast.TyTuple _ => text "(" <+> ppTy ty <+> text ")"
    | Ast.TyFun _ => text "(" <+> ppTy ty <+> text ")"
    | _ => ppTy ty

  and ppTyAtom ty =
    case ty of
      Ast.TyTuple _ => text "(" <+> ppTy ty <+> text ")"
    | Ast.TyFun _ => text "(" <+> ppTy ty <+> text ")"
    | Ast.TyBorrow _ => text "(" <+> ppTy ty <+> text ")"
    | Ast.TyConsume (_, _) => text "(" <+> ppTy ty <+> text ")"
    | _ => ppTy ty

  and ppTy ty =
    case ty of
      Ast.TyInt width =>
        text (if width = 32 then "int" else "int" ^ Int.toString width)
    | Ast.TyFloat width =>
        text (if width = 64 then "float" else "float" ^ Int.toString width)
    | Ast.TyBool => text "bool"
    | Ast.TyUnit => text "unit"
    | Ast.TyToken => text "token"
    | Ast.TyResource => text "resource"
    | Ast.TyString => text "string"
    | Ast.TyName name => text name
    | Ast.TyTuple types =>
        join (text " * ") (List.map ppTyItem types)
    | Ast.TyRecord fields =>
        if null fields then text "{}"
        else
          let
            val fieldDocs =
              List.map
                (fn (field, ty) =>
                  text field <+> text " : " <+> ppTy ty)
                fields
          in
            if List.length fields >= 3 then
              text "{" <+>
              nest 4 (hard <+> join (text "," <+> hard) fieldDocs) <+>
              hard <+> text "}"
            else
              group (
                text "{" <+>
                nest 4 (line <+> join (text "," <+> line) fieldDocs) <+>
                line <+> text "}"
              )
          end
    | Ast.TyConsume (bang, inner) =>
        (if bang then text "!" else text "consume ") <+> ppTyAtom inner
    | Ast.TyBorrow (mutable, ampersand, inner, provenance) =>
        (if ampersand then text "&" else text "borrow ") <+>
        (if mutable then text "mut " else empty) <+>
        ppTyAtom inner <+>
        (case provenance of
           NONE => empty
         | SOME (Ast.ProvIndex index) =>
             text " from " <+> text (Int.toString index)
         | SOME (Ast.ProvName name) =>
             text " from " <+> text name)
    | Ast.TyApp (name, Ast.TyTuple arguments) =>
        text "(" <+>
        join (text ", ") (List.map ppTyItem arguments) <+>
        text ") " <+> text name
    | Ast.TyApp (name, argument) =>
        ppTyAtom argument <+> text " " <+> text name
    | Ast.TyVar (name, equality) =>
        text ((if equality then "''" else "'") ^ name)
    | Ast.TyFun (params, result) =>
        (case params of
           [param] => ppTyItem param
         | _ =>
             text "(" <+>
             join (text ", ") (List.map ppTyItem params) <+>
             text ")") <+>
        text " -> " <+> ppTy result

  fun escapeString value =
    String.translate
      (fn #"\n" => "\\n"
        | #"\t" => "\\t"
        | #"\r" => "\\r"
        | #"\\" => "\\\\"
        | #"\"" => "\\\""
        | character =>
            if Char.ord character = 0 then "\\0"
            else String.str character)
      value

  fun patternConsChain pattern =
    case pattern of
      Ast.PConstructor ("Cons", SOME (Ast.PTuple ([head, tail], _)), _) =>
        let val (heads, ending) = patternConsChain tail
        in (head :: heads, ending) end
    | Ast.PConstructor ("Nil", NONE, _) => ([], NONE)
    | _ => ([], SOME pattern)

  fun ppPattern state pattern =
    let
      val span = Ast.patternSpan pattern
      val prefix = takeBefore state (startOffset span)
      val core =
        case pattern of
          Ast.PVar (name, _) => text name
        | Ast.PWildcard _ => text "_"
        | Ast.PInt (value, _) => text (Int64.toString value)
        | Ast.PBool (value, _) => text (if value then "true" else "false")
        | Ast.PString (literal, rest, _) =>
            let
              val literalDoc = text ("\"" ^ escapeString literal ^ "\"")
            in
              case rest of
                NONE => literalDoc
              | SOME subpattern =>
                  literalDoc <+> text " ^ " <+> ppPattern state subpattern
            end
        | Ast.PBytes (bytes, rest, _) =>
            let
              fun printable character =
                let val code = Char.ord character in
                  code >= 32 andalso code <= 126
                end
              val literalDoc =
                if List.all printable (String.explode bytes) then
                  text ("\"" ^ escapeString bytes ^ "\"")
                else
                  text
                    (String.concatWith ", "
                       (List.map
                          (fn character => Int.toString (Char.ord character))
                          (String.explode bytes)))
              val restDoc =
                case rest of
                  NONE => empty
                | SOME subpattern =>
                    text ", " <+> ppPattern state subpattern
            in
              group (text "<<" <+> literalDoc <+> restDoc <+> text ">>")
            end
        | Ast.PConstructor ("Nil", NONE, _) => text "[]"
        | Ast.PConstructor ("Cons", SOME (Ast.PTuple (_, _)), _) =>
            let
              val (heads, ending) = patternConsChain pattern
              val headDocs = mapInOrder (ppPattern state) heads
            in
              case ending of
                NONE =>
                  group (
                    text "[" <+>
                    nest 4 (soft <+>
                      join (text "," <+> line) headDocs) <+>
                    soft <+> text "]"
                  )
              | SOME tail =>
                  group (
                    join (text " ::" <+> line)
                      (headDocs @ [ppPattern state tail])
                  )
            end
        | Ast.PConstructor (name, argument, _) =>
            text name <+>
            (case argument of
               NONE => empty
             | SOME (inner as Ast.PRecord _) =>
                 text " " <+> text "(" <+> ppPattern state inner <+> text ")"
             | SOME inner => text " " <+> ppPattern state inner)
        | Ast.PTuple (items, _) =>
            group (
              text "(" <+>
              nest 4 (soft <+>
                join (text "," <+> line)
                  (mapInOrder (ppPattern state) items)) <+>
              soft <+> text ")"
            )
        | Ast.PRecord (fields, _) =>
            if null fields then text "{}"
            else
              group (
                text "{" <+>
                nest 4 (line <+>
                  join (text "," <+> line)
                    (mapInOrder
                       (fn (name, value) =>
                         text name <+> text " = " <+> ppPattern state value)
                       fields)) <+>
                line <+> text "}"
              )
    in
      wrapComments prefix core
    end

  fun exprPrec expression =
    case expression of
      Ast.ENeg _ => 5
    | Ast.EBorrow _ => 5
    | Ast.EFn _ => 0
    | Ast.EField _ => 6
    | Ast.ETupleIndex _ => 6
    | Ast.EBin (operator, _, _, _) => operatorPrec operator
    | Ast.EIf _ => 0
    | Ast.ELet _ => 0
    | Ast.EArena _ => 0
    | Ast.ECase _ => 0
    | _ => 7

  and pipeChain expression =
    case expression of
      Ast.EBin ("|>", left, right, _) =>
        let val (head, steps) = pipeChain left
        in (head, steps @ [right]) end
    | _ => (expression, [])

  and consChain expression =
    case expression of
      Ast.EBin ("::", head, tail, _) =>
        let val (heads, ending) = consChain tail
        in (head :: heads, ending) end
    | Ast.EVar ("Nil", _) => ([], NONE)
    | _ => ([], SOME expression)

  and operatorPrec operator =
    if operator = "|>" then 0
    else if operator = "=" orelse operator = "<>" orelse operator = "<"
       orelse operator = "<=" orelse operator = ">" orelse operator = ">="
    then 1
    else if operator = "::" then 2
    else if operator = "+" orelse operator = "-" orelse operator = "^"
    then 3
    else 4

  fun ppExpr state expression =
    let
      val span = Ast.exprSpan expression
      val prefix = takeBefore state (startOffset span)
      val core =
        case expression of
          Ast.EInt (value, _) => text (Int64.toString value)
        | Ast.EFloat (_, literalSpan) => text (sourceSlice state literalSpan)
        | Ast.EString (value, _) => text ("\"" ^ escapeString value ^ "\"")
        | Ast.EBool (value, _) => text (if value then "true" else "false")
        | Ast.EUnit _ => text "()"
        | Ast.EVar ("Nil", _) => text "[]"
        | Ast.EVar (name, _) => text name
        | Ast.ECall (name, arguments, _) =>
            let
              val argumentsDoc =
                case arguments of
                  [] => text "()"
                | _ =>
                    group (
                      text "(" <+>
                      nest 4 (soft <+>
                        join (text "," <+> line)
                          (mapInOrder (ppExpr state) arguments)) <+>
                      soft <+> text ")"
                    )
            in
              text name <+> text " " <+> argumentsDoc
            end
        | Ast.EBin ("::", _, _, _) =>
            let
              val (heads, ending) = consChain expression
              val headDocs = mapInOrder (ppExpr state) heads
            in
              case ending of
                NONE =>
                  group (
                    text "[" <+>
                    nest 4 (soft <+>
                      join (text "," <+> line) headDocs) <+>
                    soft <+> text "]"
                  )
              | SOME tail =>
                  group (
                    join (text " ::" <+> line)
                      (headDocs @ [ppExpr state tail])
                  )
            end
        | Ast.EBin (operator, left, right, _) =>
            if operator = "|>" then
              let
                val (head, steps) = pipeChain expression
              in
                if List.length steps >= 2 then

                  ppAt state 0 head <+>
                  hard <+>
                  join hard
                    (List.map
                       (fn step => text "|> " <+> ppExpr state step)
                       steps)
                else
                  let
                    val precedence = operatorPrec operator
                    val leftDoc = ppAt state precedence left
                    val rightDoc = ppAt state (precedence + 1) right
                  in
                    group (
                      leftDoc <+> text " " <+> text operator <+>
                      nest 4 (line <+> rightDoc)
                    )
                  end
              end
            else
              let
                val precedence = operatorPrec operator
                val leftDoc =
                  ppAt state
                    (if operator = "::" then precedence + 1 else precedence)
                    left
                val rightDoc =
                  ppAt state
                    (if operator = "::" then precedence else precedence + 1)
                    right
              in
                group (
                  leftDoc <+> text " " <+> text operator <+>
                  nest 4 (line <+> rightDoc)
                )
              end
        | Ast.ENeg (operand, _) =>
            text "-" <+> ppAt state 5 operand
        | Ast.EIf (condition, thenBranch, elseBranch, _) =>
            let
              val conditionDoc = ppAt state 1 condition
              val thenDoc = ppExpr state thenBranch
              val elseDoc = ppExpr state elseBranch
            in
              group (
                text "if " <+> conditionDoc <+> text " then" <+>
                nest 4 (line <+> thenDoc) <+>
                (case elseBranch of
                   Ast.EIf _ => line <+> text "else " <+> elseDoc
                 | _ =>
                     line <+> text "else" <+>
                     nest 4 (line <+> elseDoc))
              )
            end
        | Ast.ELet (bindings, body, _) =>
            let
              val bindingDocs = mapInOrder (ppBinding state) bindings
              val bodyDoc = ppExpr state body

              val bindingsDoc =
                case bindingDocs of
                  [] => empty
                | first :: rest =>
                    group (nest 4 (line <+> first)) <+>
                    (if null rest then empty
                     else nest 4 (hard <+> join hard rest))
            in
              text "let" <+> bindingsDoc <+>
              hard <+> text "in" <+>
              nest 4 (hard <+> bodyDoc) <+>
              hard <+> text "end"
            end
        | Ast.EArena (bindings, body, _) =>
            let
              val bindingDocs = mapInOrder (ppBinding state) bindings
              val bodyDoc = ppExpr state body
              val bindingsDoc =
                case bindingDocs of
                  [] => empty
                | first :: rest =>
                    group (nest 4 (line <+> first)) <+>
                    (if null rest then empty
                     else nest 4 (hard <+> join hard rest))
            in
              text "arena" <+> bindingsDoc <+>
              hard <+> text "in" <+>
              nest 4 (hard <+> bodyDoc) <+>
              hard <+> text "end"
            end
        | Ast.ERecord (fields, _) =>
            if null fields then text "{}"
            else
              group (
                text "{" <+>
                nest 4 (line <+>
                  join (text "," <+> line)
                    (mapInOrder
                       (fn (name, value) =>
                         text name <+> text " = " <+> ppExpr state value)
                       fields)) <+>
                line <+> text "}"
              )
        | Ast.EField (field, base, _) =>
            ppAt state 7 base <+> text "." <+> text field
        | Ast.ETuple (items, _) =>
            group (
              text "(" <+>
              nest 4 (soft <+>
                join (text "," <+> line)
                  (mapInOrder (ppExpr state) items)) <+>
              soft <+> text ")"
            )
        | Ast.ETupleIndex (index, base, _) =>
            ppAt state 7 base <+> text "." <+> text (Int.toString index)
        | Ast.ECase (scrutinee, branches, _) =>
            let
              val scrutineeDoc = ppAt state 1 scrutinee
              val branchDocs = mapInOrder (ppBranch state) branches
            in
              case branchDocs of
                [] => text "case " <+> scrutineeDoc <+> text " of"
              | first :: rest =>
                  text "case " <+> scrutineeDoc <+> text " of" <+>
                  nest 4 (hard <+> first) <+>
                  (case rest of
                     [] => empty
                   | _ =>
                       nest 2 (hard <+> join hard
                         (List.map
                            (fn branch => text "| " <+> branch)
                            rest)))
            end
        | Ast.EBorrow (mutable, ampersand, operand, _) =>
            (if ampersand then text "&" else text "borrow ") <+>
            (if mutable then text "mut " else empty) <+>
            ppAt state 6 operand
        | Ast.EExtern _ =>
            text "extern"

        | Ast.EFn (params, result, body, _) =>
            let
              val paramDocs =
                List.map
                  (fn param =>
                    text (#name param) <+>
                    (case #annot param of
                       NONE => empty
                     | SOME ty => text " : " <+> ppTy ty))
                  params
              val paramsDoc =
                case params of
                  [param] =>
                    text (#name param) <+>
                    (case #annot param of
                       NONE => empty
                     | SOME ty => text " : " <+> ppTy ty)
                | _ =>
                    group (
                      text "(" <+>
                      nest 4 (soft <+>
                        join (text "," <+> line) paramDocs) <+>
                      soft <+> text ")"
                    )
              val resultDoc =
                case result of
                  NONE => empty
                | SOME ty => text " : " <+> ppTy ty
            in
              group (
                paramsDoc <+> resultDoc <+> text " =>" <+>
                nest 4 (line <+> ppExpr state body)
              )
            end
    in
      wrapComments prefix core
    end

  and ppAt state minimum expression =
    let val doc = ppExpr state expression in
      if exprPrec expression < minimum
      then text "(" <+> doc <+> text ")"
      else doc
    end

  and ppBinding state binding =
    case binding of
      Ast.BVal { name, annot, value, span } =>
        let
          val prefix = takeBefore state (startOffset span)
          val annotation =
            case annot of
              NONE => empty
            | SOME ty => text " : " <+> ppTy ty
          val valueDoc = ppExpr state value
        in
          wrapComments prefix (
            group (
              text "val " <+> text name <+> annotation <+> text " =" <+>
              nest 4 (line <+> valueDoc)
            )
          )
        end
    | Ast.BPat { pattern, value, span } =>
        let
          val prefix = takeBefore state (startOffset span)
          val valueDoc = ppExpr state value
        in
          wrapComments prefix (
            group (
              text "val " <+> ppPattern state pattern <+> text " =" <+>
              nest 4 (line <+> valueDoc)
            )
          )
        end
    | Ast.BFun { name, params, result, body, span, ... } =>
        let
          val prefix = takeBefore state (startOffset span)
          val bodyDoc = ppExpr state body
          val paramDocs =
            List.map
              (fn param =>
                text (#name param) <+>
                (case #annot param of
                   NONE => empty
                 | SOME ty => text " : " <+> ppTy ty))
              params
          val paramsDoc =
            if null params then text "()"
            else
              group (
                text "(" <+>
                nest 4 (soft <+>
                  join (text "," <+> line) paramDocs) <+>
                soft <+> text ")"
              )
        in
          wrapComments prefix (
            group (
              text "fun " <+> text name <+> text " " <+>
              paramsDoc <+>
              (case result of
                 NONE => empty
               | SOME ty => text " : " <+> ppTy ty) <+>
              text " =" <+>
              nest 4 (line <+> bodyDoc)
            )
          )
        end
    | Ast.BGuard { pattern, value, fallback, span } =>
        let
          val prefix = takeBefore state (startOffset span)
          val head =
            case pattern of
              NONE => ppExpr state value
            | SOME pattern =>
                ppPattern state pattern <+> text " <- " <+> ppExpr state value
          val fallbackDoc = ppExpr state fallback
        in
          wrapComments prefix (
            group (
              text "guard " <+> head <+>
              nest 4 (line <+> text "else" <+> line <+> fallbackDoc)
            )
          )
        end

  and ppBranch state (pattern, guard, body) =
    let

      val bodyDoc =
        case body of
          Ast.ECase _ => text "(" <+> ppExpr state body <+> text ")"
        | _ => ppExpr state body
      val guardDoc =
        case guard of
          NONE => empty
        | SOME condition => text " if " <+> ppAt state 1 condition
    in
      group (
        ppPattern state pattern <+> guardDoc <+> text " =>" <+>
        nest 4 (line <+> bodyDoc)
      )
    end

  fun ppParam param =
    text (#name param) <+>
    (case #annot param of
       NONE => empty
     | SOME ty => text " : " <+> ppTy ty)

  fun ppParams params =
    if null params then text "()"
    else
      group (
        text "(" <+>
        nest 4 (soft <+>
          join (text "," <+> line) (List.map ppParam params)) <+>
        soft <+> text ")"
      )

  fun ppParamsPrefix params =
    case params of
      [] => empty
    | [(name, equality)] =>
        text ((if equality then "''" else "'") ^ name ^ " ")
    | _ =>
        text "(" <+>
        text (String.concatWith ", "
          (List.map
             (fn (name, equality) =>
               (if equality then "''" else "'") ^ name)
             params)) <+>
        text ") "

  fun ppConstructor (name, argument) =
    text name <+>
    (case argument of
       NONE => empty
     | SOME ty => text " of " <+> ppTy ty)

  fun ppDecl state declaration =
    let
      val span = Ast.declSpan declaration
      val prefix = takeBefore state (startOffset span)
      val core =
        case declaration of
          Ast.DFun { name, params, result, recursive, body, ... } =>
            (case body of
               Ast.EExtern {symbol, ...} =>
                 group (
                   text "extern fun " <+> text name <+> text " " <+>
                   ppParams params <+>
                   (case result of
                      NONE => empty
                    | SOME ty => text " : " <+> ppTy ty) <+>
                   text " = " <+> text ("\"" ^ symbol ^ "\"")
                 )
             | _ =>
                 let val bodyDoc = ppExpr state body in
                   group (
                     text (if recursive then "rec " else "fun ") <+>
                     text name <+> text " " <+>
                     ppParams params <+>
                     (case result of
                        NONE => empty
                      | SOME ty => text " : " <+> ppTy ty) <+>
                     text " =" <+>
                     nest 4 (line <+> bodyDoc)
                   )
                 end)
        | Ast.DFunClause { name, params, result, recursive, body, ... } =>
            let
              val paramDocs =
                List.map
                  (fn (pattern, annot) =>
                    ppPattern state pattern <+>
                    (case annot of
                       NONE => empty
                     | SOME ty => text " : " <+> ppTy ty))
                  params
              val paramsDoc =
                if null params then text "()"
                else
                  group (
                    text "(" <+>
                    nest 4 (soft <+>
                      join (text "," <+> line) paramDocs) <+>
                    soft <+> text ")"
                  )
              val bodyDoc = ppExpr state body
            in
              group (
                text (if recursive then "rec " else "fun ") <+>
                text name <+> text " " <+> paramsDoc <+>
                (case result of
                   NONE => empty
                 | SOME ty => text " : " <+> ppTy ty) <+>
                text " =" <+>
                nest 4 (line <+> bodyDoc)
              )
            end
        | Ast.DType { name, declared, fields, ... } =>
            let
              val fieldDocs =
                List.map
                  (fn (field, ty) =>
                    text field <+> text " : " <+> ppTy ty)
                  fields
              val fieldsDoc =
                if null fields then text "{}"
                else if List.length fields >= 3 then

                  text "{" <+>
                  nest 4 (hard <+> join (text "," <+> hard) fieldDocs) <+>
                  hard <+> text "}"
                else
                  group (
                    text "{" <+>
                    nest 4 (line <+> join (text "," <+> line) fieldDocs) <+>
                    line <+> text "}"
                  )
            in
              text "type " <+> text name <+>
              (case declared of
                 NONE => empty
               | SOME property =>
                   text " : " <+> text (Ast.propertyToString property)) <+>
              text " = " <+> fieldsDoc
            end
        | Ast.DAlias { name, params, body, ... } =>
            text "type " <+> ppParamsPrefix params <+> text name <+>
            text " = " <+> ppTy body
        | Ast.DExternRecord { name, size, fields, ... } =>
            let
              val fieldDocs =
                List.map
                  (fn (field, ty, offset) =>
                    text field <+> text " : " <+> ppTy ty <+>
                    text " @" <+> text (Int.toString offset))
                  fields
            in
              group (
                text "extern record " <+> text name <+>
                text " (size = " <+> text (Int.toString size) <+> text ") {" <+>
                nest 4 (line <+> join (text "," <+> line) fieldDocs) <+>
                line <+> text "}"
              )
            end
        | Ast.DDatatype { name, params, declared, constructors, ... } =>
            let
              val constructorDocs = List.map ppConstructor constructors
            in
              text "datatype " <+> ppParamsPrefix params <+> text name <+>
              (case declared of
                 NONE => empty
               | SOME property =>
                   text " : " <+> text (Ast.propertyToString property)) <+>
              text " =" <+>
              (case constructorDocs of
                 [] => empty
               | first :: rest =>
                   group (
                     nest 4 (line <+> first) <+>
                     (case rest of
                        [] => empty
                      | _ =>
                          nest 2 (hard <+> join hard
                            (List.map
                               (fn constructor =>
                                 text "| " <+> constructor)
                               rest)))
                   ))
            end
        | Ast.DImport { path, alias, unqualified, ... } =>
            let
              fun ppImportItem item =
                case item of
                  Ast.ImportValue (name, itemAlias) =>
                    text name <+>
                    (case itemAlias of
                       NONE => empty
                     | SOME value => text " as " <+> text value)
                | Ast.ImportType (name, itemAlias) =>
                    text "type " <+> text name <+>
                    (case itemAlias of
                       NONE => empty
                     | SOME value => text " as " <+> text value)
              val items =
                case unqualified of
                  [] => empty
                | _ =>
                    text ".{" <+>
                    join (text ", ")
                      (List.map ppImportItem unqualified) <+>
                    text "}"
            in
              text "import " <+> text (String.concatWith "/" path) <+>
              items <+>
              (case alias of
                 NONE => empty
               | SOME value => text " as " <+> text value)
            end
    in
      wrapComments prefix core
    end

  fun ppProgram state program =
    let
      val declarations = ListPair.zip (program, mapInOrder (ppDecl state) program)
      fun isImport declaration =
        case declaration of
          Ast.DImport _ => true
        | _ => false

      fun isSameFunction declaration nextDeclaration =
        case (declaration, nextDeclaration) of
          ( Ast.DFunClause {name = name, ...}
          , Ast.DFunClause {name = nextName, ...} ) => name = nextName
        | _ => false
      fun joinDeclarations [] = empty
        | joinDeclarations [(_, doc)] = doc
        | joinDeclarations
            ((declaration, doc) :: (nextDeclaration, nextDoc) :: rest) =
            doc <+>

            (if (isImport declaration andalso isImport nextDeclaration) orelse
                isSameFunction declaration nextDeclaration
             then hard
             else hard <+> hard) <+>
            joinDeclarations ((nextDeclaration, nextDoc) :: rest)
    in
      case declarations of
        [] => flushComments state
      | _ => joinDeclarations declarations <+> flushTail state
    end

  type options =
    { source : string
    , width : int
    }

  type result =
    { output : string
    , diagnostics : Diag.t list
    }

  fun format {source, width} =
    let
      val lexed = Lexer.lex source
      val lexicalDiagnostics = #diagnostics lexed
    in
      if not (null lexicalDiagnostics) then
        { output = "", diagnostics = lexicalDiagnostics }
      else
        let
          val parsed = Parser.parse (#tokens lexed)
          val syntaxDiagnostics = #diagnostics parsed
        in
          if not (null syntaxDiagnostics) then
            { output = "", diagnostics = syntaxDiagnostics }
          else
            let
              val target = if width <= 20 then defaultWidth else width
              val pending =
                ref (List.map
                       (fn (span, value) =>
                         (Pos.offset (#start span), value))
                       (#comments lexed))
              val state : state = { pending = pending, source = source }
              val document = ppProgram state (#program parsed)
              val rendered = render target document
              val output =
                if rendered = "" then "" else rendered ^ "\n"
            in
              { output = output, diagnostics = [] }
            end
        end
    end
end
