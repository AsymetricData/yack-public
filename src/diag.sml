structure Diag =
struct
  datatype severity = Error | Warning

  type span = {start : Pos.t, stop : Pos.t}

  type t =
    { code : string
    , severity : severity
    , message : string
    , span : span
    , notes : string list
    , hints : string list
    }

  type style = { color : bool }

  val plain : style = { color = false }

  fun fallbackHint code =
    case code of
      "E3001" => "check the spelling, or define the name with `val`/`fun` before using it"
    | "E3002" => "check the spelling, or define the function with `fun name (...) = ...` at module level"
    | "E3003" => "pass all arguments at once: `f (a, b)`"
    | "E3004" => "make the intended type explicit with a type annotation, or convert the value"
    | "E3008" => "rename one of the parameters"
    | "E3009" => "rename one definition, or call the existing one"
    | "E3010" => "define `fun main () : unit = ...`"
    | "E3011" => "rename the function; builtins are reserved"
    | "E3012" => "check the spelling, or declare the type with `type`/`datatype`"
    | "E3013" => "rename one of the declarations"
    | "E3014" => "use one of the field names shown above, or declare the missing field"
    | "E3015" => "remove the `Copy` annotation, or make every field Copy"
    | "E3016" => "remove the Linear field, or declare the containing type Linear"
    | "E3017" => "add a type annotation so the shape of the value is known"
    | "E3018" => "remove the duplicate field"
    | "E3019" => "use an index or pattern that matches the tuple's arity"
    | "E3020" => "rename one pattern variable; a name may bind only once per pattern"
    | "E3021" => "add a wildcard branch (`_ => ...`) or cover the missing constructors"
    | "E3022" => "remove the unreachable branch, or reorder the branches"
    | "E3023" => "constructors must start with an uppercase letter"
    | "E3024" => "rename one constructor; constructors share a single namespace"
    | "E3025" => "rename the function so it does not collide with a constructor"
    | "E3026" => "match the constructor's arity: `Name` without an argument, `Name (value)` with one"
    | "E3027" => "check the spelling, or declare the constructor in a `datatype`"
    | "E3028" => "remove the type variables; type declarations are monomorphic for now"
    | "E3029" => "supply the number of type arguments shown above"
    | "E3030" => "rename the import with `as`, or use a qualified name"
    | "E3031" => "check the module path/name, or export the value from that module"
    | "E3040" => "use a Copy element type, or build the array with `array_new` and `array_push`"
    | "E3041" => "use `array_get_borrow` to read the element without copying it"
    | "E3042" => "use the checked functions of `yack/array`; the kernel is for the standard library"
    | "E3043" => "import the public wrapper of that module instead of its private extern"
    | "E3045" => "extern accepts `int`, `int64`, `borrow string` and `borrow int8 array` parameters"
    | "E3046" => "extern returns `int`, `int64` or `unit`; fill a buffer for anything else"
    | "E3047" => "check the field offset and the record `size`; only fixed-width fields and extern records are allowed"
    | "E3044" => "use `map_get_borrow` to read the value without taking ownership"
    | "E0402" => "move the use before the transfer, or copy the value if its type is Copy"
    | "E0403" => "consume the value on every path before its scope ends"
    | "E0500" => "borrow a local variable or a field of one: `borrow user` or `borrow user.name`"
    | "E0501" => "end the borrow before moving the owner, or move the value before borrowing it"
    | "E0502" => "end the conflicting borrow first; shared reads and a mutable borrow cannot overlap"
    | "E0503" => "add `from <parameter>` to the return type, or return an owned value"
    | "E0504" => "use a `from` index between 1 and the number of parameters"
    | "E0505" => "name a borrow parameter in `from`, or make the parameter a `borrow` parameter"
    | "E0506" => "return the parameter named in `from`, or update `from` to match the returned borrow"
    | "E0507" => "declare the owner in an outer `let`, or keep the borrow local"
    | "E0508" => "copy a Copy field, or access the field through an owned value"
    | "E0514" =>
        "inline the lambda where the expected callback type can make its parameter owned, or pass a function that consumes its argument"
    | "E0517" =>
        "inline the lambda so the callback type decides ownership, or pass the value to a named function"
    | "E0600" => "use a type that the backend supports"
    | "E0601" => "add annotations so every type variable is resolved before lowering"
    | "E0603" => "this is a compiler bug: simplify the expression and report it"
    | "E0604" => "call a module-level function directly; function values are not lowered yet"
    | "E0605" => "check the datatype name, or redeclare it in this module"
    | "E0606" => "add a numeric type annotation to the literal"
    | "E0607" => "check the constructor name, or declare it in a `datatype`"
    | "E0608" => "write `Name (value)`; this constructor needs an argument"
    | "E0609" => "use a field name declared by the record type"
    | "E0701" =>
        "build the value in the destination region, or root it before it escapes"
    | "E0702" => "store a freshly constructed or root-allocated value, not a reference to the current region"
    | "E0704" =>
        "construct the value in the destination region; a stack alloca cannot escape"
    | "E0610" => "use a literal format string and pass one value per directive"
    | "E0518" => "call the local function directly; a capturing local function cannot become a closure value"
    | "E0519" => "rename the shadowing binding, or move the local function below it"
    | "E1001" => "write `x => ...` or pass a module-level function by name"
    | "E1003" => "write `name (x)` or `name (x, y)`; partial application is not supported"
    | "E1004" => "write `fun name () = ...`; currying and implicit parameters are not supported"
    | "E2001" => "check the token around the caret"
    | "E2002" => "check the syntax around the caret; `yack fmt` can show the expected shape"
    | _ => "check the value and its type, then adjust the code around the caret"

  fun hintFor (d : t) =
    case #hints d of
      [] => [fallbackHint (#code d)]
    | hints => hints

  fun displayName name =
    if String.size name > 0 andalso String.sub (name, 0) = #"$" then
      let
        val body = String.substring (name, 1, String.size name - 1)
        val parts = String.fields (fn ch => ch = #".") body
        fun lastTwo revParts =
          case revParts of
            member :: moduleName :: _ => moduleName ^ "." ^ member
          | [member] => member
          | [] => body
      in
        lastTwo (List.rev parts)
      end
    else name

  fun demangleText text =
    let
      val size = String.size text
      fun isNameChar ch =
        Char.isAlphaNum ch orelse ch = #"_" orelse ch = #"."
      fun endOf index =
        if index < size andalso isNameChar (String.sub (text, index))
        then endOf (index + 1)
        else index
      fun loop (index, start, acc) =
        if index >= size then
          List.rev (String.substring (text, start, index - start) :: acc)
        else if String.sub (text, index) = #"$" andalso
                index + 1 < size andalso
                isNameChar (String.sub (text, index + 1)) then
          let
            val stop = endOf (index + 1)
            val prefixText = String.substring (text, start, index - start)
            val name = String.substring (text, index, stop - index)
          in
            loop (stop, stop, displayName name :: prefixText :: acc)
          end
        else
          loop (index + 1, start, acc)
    in
      String.concat (loop (0, 0, []))
    end

  fun demangle (d : t) : t =
    { code = #code d
    , severity = #severity d
    , message = demangleText (#message d)
    , span = #span d
    , notes = List.map demangleText (#notes d)
    , hints = List.map demangleText (#hints d)
    }

  fun error (code, message, span, notes, hints) : t =
    { code = code
    , severity = Error
    , message = message
    , span = span
    , notes = notes
    , hints = hints
    }

  fun warning (code, message, span, notes, hints) : t =
    { code = code
    , severity = Warning
    , message = message
    , span = span
    , notes = notes
    , hints = hints
    }

  fun hasErrors (diagnostics : t list) =
    List.exists (fn diagnostic => #severity diagnostic = Error) diagnostics

  fun spanOf start stop : span = {start = start, stop = stop}

  fun pointSpan (p : Pos.t) : span = {start = p, stop = p}

  fun repeat (count, text) =
    if count <= 0 then ""
    else String.concat (List.tabulate (count, fn _ => text))

  fun paint ({color} : style, code, text) =
    if color then "\027[" ^ code ^ "m" ^ text ^ "\027[0m"
    else text

  fun severityPaint (style, Error) = paint (style, "1;31", "error")
    | severityPaint (style, Warning) = paint (style, "1;33", "warning")

  fun renderWith (style : style, {source, file, severity}) (d : t) =
    let
      val {start, stop} = #span d
      val line = Pos.line start
      val column = Pos.column start
      val text = Source.lineText (source, line)
      val width =
        if Pos.line stop = line andalso Pos.column stop > column
        then Pos.column stop - column
        else if Pos.line stop = line andalso Pos.column stop = column
        then 1
        else Int.max (1, String.size text - column + 1)
      val caretChar = repeat (Int.max (0, column - 1), " ") ^ repeat (width, "^")
      val caretColor = case severity of Error => "1;31" | Warning => "1;33"
      val gutter = paint (style, "2", "|")
      val gutterIndent = paint (style, "2", "  |")
      val lineNumber = paint (style, "1;34", Int.toString line)
      val location =
        paint (style, "1;34", file ^ ":" ^ Int.toString line ^ ":" ^
               Int.toString column)
      val notes =
        List.map
          (fn n =>
            paint (style, "2", "   = ") ^
            paint (style, "1;36", "note") ^
            paint (style, "2", ": ") ^ n ^ "\n")
          (#notes d)
      val hints =
        List.map
          (fn h =>
            paint (style, "2", "   = ") ^
            paint (style, "1;32", "hint") ^
            paint (style, "2", ": ") ^ h ^ "\n")
          (hintFor d)
      val code =
        paint (style, "2", "[" ^ #code d ^ "]")
    in
      severityPaint (style, severity) ^ code ^
      paint (style, "1", ": ") ^ #message d ^ "\n" ^
      paint (style, "2", "  ") ^ paint (style, "1;34", "-->") ^
      " " ^ location ^ "\n" ^
      gutterIndent ^ "\n" ^
      " " ^ lineNumber ^ " " ^ gutter ^ " " ^ text ^ "\n" ^
      gutterIndent ^ " " ^ paint (style, caretColor, caretChar) ^ "\n" ^
      String.concat notes ^
      String.concat hints
    end

  fun render args =
    renderWith (plain, args)
end
