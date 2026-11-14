structure LowerSprintf =
struct
  type env =
    { report : string -> string -> Diag.span -> string list -> string list -> unit
    , exprType : Ir.expr -> Ir.ty
    }

  fun make (env : env) =
    let
      val report = #report env
      val exprType = #exprType env

      datatype sprintfPiece =
    SprintfText of string
  | SprintfValue

      fun literalString expression =
  case expression of
    Ast.EString (text, _) => SOME text
  | Ast.EBorrow (_, _, inner, _) => literalString inner
  | _ => NONE

      fun parseSprintfFormat (span, format) =
  let
    val size = String.size format
    fun flush (start, stop, acc) =
      if stop > start then
        SprintfText (String.substring (format, start, stop - start)) :: acc
      else acc
    fun unknown (index, next) =
      let
        val text = String.substring (format, index, next - index)
      in
        report
          "E0610"
          ("unknown `sprintf` directive `" ^ text ^ "`")
          span
          ["supported directives: %d, %i, %s, %b, %f, %c, %ld, %lld and %%"]
          ["use `%%` for a literal percent sign"];
        (next, SprintfText text)
      end
    fun loop (index, start, acc) =
      if index >= size then
        List.rev (flush (start, index, acc))
      else if String.sub (format, index) = #"%" then
        let
          val acc = flush (start, index, acc)
          val next = index + 1
          val (after, piece) =
            if next >= size then unknown (index, next)
            else
              let val c = String.sub (format, next) in
                if c = #"%" then (next + 1, SprintfText "%")
                else if c = #"d" orelse c = #"i" orelse c = #"u" orelse
                        c = #"s" orelse c = #"b" orelse c = #"f" orelse
                        c = #"c" then
                  (next + 1, SprintfValue)
                else if c = #"l" andalso next + 1 < size then
                  let val d = String.sub (format, next + 1) in
                    if d = #"d" orelse d = #"u" then
                      (next + 2, SprintfValue)
                    else if d = #"f" then
                      (next + 2, SprintfValue)
                    else if d = #"l" andalso next + 2 < size then
                      let val e = String.sub (format, next + 2) in
                        if e = #"d" orelse e = #"u" then
                          (next + 3, SprintfValue)
                        else unknown (index, next + 3)
                      end
                    else unknown (index, next + 2)
                  end
                else unknown (index, next + 1)
              end
        in
          loop (after, after, piece :: acc)
        end
      else
        loop (index + 1, start, acc)
  in
    loop (0, 0, [])
  end

      fun mapi function values =
  let
    fun loop (index, []) = []
      | loop (index, value :: rest) =
          function (index, value) :: loop (index + 1, rest)
  in
    loop (0, values)
  end

      fun flattenSprintfValue value =
  case (value, exprType value) of
    (Ir.Tuple (elements, _), _) => elements
  | (_, Ir.TTuple fields) =>
      mapi
        (fn (index, fieldTy) => Ir.TupleIndex (value, index, fieldTy))
        fields
  | (_, Ir.Unit) => []
  | _ => [value]

      fun sprintfValueText span value =
  case exprType value of
    Ir.Int 32 =>
      Ir.StringBuiltin ("string_of_int", [(value, false)], Ir.TString, ref Ir.Current)
  | Ir.Int width =>
      Ir.StringBuiltin
        ( "int" ^ Int.toString width ^ "_to_string"
        , [(value, false)]
        , Ir.TString
        , ref Ir.Current
        )
  | Ir.Float 64 =>
      Ir.StringBuiltin ("float_to_string", [(value, false)], Ir.TString, ref Ir.Current)
  | Ir.Bool =>
      Ir.If
        ( value
        , Ir.StringLit ("true", Ir.TString)
        , Ir.StringLit ("false", Ir.TString)
        , Ir.TString
        )
  | Ir.TString =>
      Ir.StringBuiltin ("string_copy", [(value, false)], Ir.TString, ref Ir.Current)
  | Ir.TBorrow (Ir.TString, _) =>
      Ir.StringBuiltin ("string_copy", [(value, false)], Ir.TString, ref Ir.Current)
  | ty =>
      ( report
          "E0610"
          ("`sprintf` cannot format a value of type `" ^
           Ir.tyToString ty ^ "`")
          span
          ["supported values: int, int64, float, bool and string"]
          ["convert the value before formatting it"];
        Ir.StringLit ("", Ir.TString)
      )

      fun concatStrings pieces =
  case pieces of
    [] => Ir.StringLit ("", Ir.TString)
  | [single] => single
  | first :: rest =>
      List.foldl
        (fn (piece, acc) => Ir.Bin ("^", acc, piece, Ir.TString, ref Ir.Current))
        first
        rest

      fun lowerSprintf (span, arguments, loweredArguments) =
  case (arguments, loweredArguments) of
    (formatArg :: _ :: _, _ :: value :: _) =>
      (case literalString formatArg of
         NONE =>
           ( report
               "E0610"
               "`sprintf` requires a literal format string"
               span
               ["the format is analysed when the program is compiled"]
               ["use a string literal, or build the result with `^` and `int_to_string`"]
           ; Ir.StringLit ("", Ir.TString)
           )
       | SOME format =>
           let
             val pieces = parseSprintfFormat (span, format)

             val (values, wrap) =
               case value of
                 Ir.Tuple (elements, _) =>
                   (elements, fn expression => expression)
               | Ir.Var _ =>
                   (flattenSprintfValue value,
                    fn expression => expression)
               | _ =>
                   let
                     val name =
                       "sprintf.args." ^ Pos.toString (#start span)
                     val ty = exprType value
                     val argument = Ir.Var (name, ty, NONE)
                   in
                     ( flattenSprintfValue argument
                     , fn expression =>
                         Ir.Let ([(name, ty, value)], expression, [])
                     )
                   end
             fun render (remainingPieces, remainingValues, acc) =
               case remainingPieces of
                 [] =>
                   (case remainingValues of
                      [] => List.rev acc
                    | _ =>
                        ( report
                            "E0610"
                            "`sprintf` received more values than format directives"
                            span
                            ["the extra value is ignored"]
                            ["add a matching directive to the format string"]
                        ; List.rev acc
                        ))
               | SprintfText text :: rest =>
                   render
                     (rest, remainingValues,
                      Ir.StringLit (text, Ir.TString) :: acc)
               | SprintfValue :: rest =>
                   (case remainingValues of
                      [] =>
                        ( report
                            "E0610"
                            "`sprintf` format has more directives than values"
                            span
                            ["missing value for this directive"]
                            ["pass one more value, or remove the directive"]
                        ; render (rest, [], acc)
                        )
                    | nextValue :: moreValues =>
                        render
                          (rest, moreValues,
                           sprintfValueText span nextValue :: acc))
           in
             wrap (concatStrings (render (pieces, values, [])))
           end)
  | _ => Ir.StringLit ("", Ir.TString)

    in
      lowerSprintf
    end
end
