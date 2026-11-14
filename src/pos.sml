structure Pos =
struct

  type t = {file : int, line : int, column : int, offset : int}

  val start : t = {file = 0, line = 1, column = 1, offset = 0}

  fun startIn file : t = {file = file, line = 1, column = 1, offset = 0}

  fun file ({file, ...} : t) = file
  fun line ({line, ...} : t) = line
  fun column ({column, ...} : t) = column
  fun offset ({offset, ...} : t) = offset

  fun toString ({line, column, ...} : t) =
    Int.toString line ^ ":" ^ Int.toString column

  fun advance (p : t, ch : char) =
    if ch = #"\n"
    then {file = #file p, line = #line p + 1, column = 1, offset = #offset p + 1}
    else {file = #file p, line = #line p, column = #column p + 1,
          offset = #offset p + 1}
end
