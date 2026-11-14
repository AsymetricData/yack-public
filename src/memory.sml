structure Memory =
struct
  datatype mode =
      Heap
    | Stack

  datatype arenaMode =
      ArenaOn
    | ArenaNoop

  val defaultArena = ArenaOn
  val currentArena = ref ArenaOn

  val traceAlloc = ref false

  fun traceEnabled () = !traceAlloc
  fun setTrace value = traceAlloc := value

  fun arenaEnabled () = !currentArena = ArenaOn

  fun setArena mode = currentArena := mode

  datatype arenaDebug =
      ArenaDebugOff
    | ArenaDebugPoison

  datatype quarantine =
      QuarantineOff
    | QuarantineStrict

  val arenaDebug = ref ArenaDebugOff
  val arenaQuarantine = ref QuarantineOff
  val arenaQuarantineLimit : Int64.int ref = ref (256 * 1024 * 1024)

  fun poisonEnabled () = !arenaDebug = ArenaDebugPoison
  fun strictQuarantine () = !arenaQuarantine = QuarantineStrict

  fun setArenaDebug mode = arenaDebug := mode
  fun setQuarantine mode = arenaQuarantine := mode

  fun debugFromString "off" = SOME ArenaDebugOff
    | debugFromString "poison" = SOME ArenaDebugPoison
    | debugFromString _ = NONE

  fun quarantineFromString "off" = SOME QuarantineOff
    | quarantineFromString "strict" = SOME QuarantineStrict
    | quarantineFromString _ = NONE

  fun arenaFromString "on" = SOME ArenaOn
    | arenaFromString "noop" = SOME ArenaNoop
    | arenaFromString _ = NONE

  fun arenaToString ArenaOn = "on"
    | arenaToString ArenaNoop = "noop"

  val default = Heap

  val defaultArenaReserve : Int64.int = 1073741824

  fun toString Heap = "heap"
    | toString Stack = "stack"

  fun fromString "heap" = SOME Heap
    | fromString "stack" = SOME Stack
    | fromString _ = NONE

  fun parseSize text =
    let
      val length = String.size text
      val (digits, multiplier) =
        if length = 0 then ("", 1)
        else
          case Char.toUpper (String.sub (text, length - 1)) of
            #"K" => (String.substring (text, 0, length - 1), 1024)
          | #"M" => (String.substring (text, 0, length - 1), 1024 * 1024)
          | #"G" => (String.substring (text, 0, length - 1), 1024 * 1024 * 1024)
          | #"T" =>
              (String.substring (text, 0, length - 1),
               1024 * 1024 * 1024 * 1024)
          | _ => (text, 1)
      val valid =
        String.size digits > 0 andalso
        List.all Char.isDigit (String.explode digits)
    in
      if not valid then NONE
      else
        case Int64.fromString digits of
          NONE => NONE
        | SOME value => SOME (value * Int64.fromInt multiplier)
    end
end
