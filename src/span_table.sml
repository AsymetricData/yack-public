structure SpanTable =
struct
  type key = Diag.span
  type 'a table = 'a list array

  val bucketCount = 512

  fun hash ({start, stop} : key) =
    let
      val a = Word.fromInt (Pos.offset start)
      val b = Word.fromInt (Pos.offset stop)
      val f = Word.fromInt (Pos.file start)
      val mixed = a * 0wx1F + b * 0wx11 + f
    in
      Word.toInt (mixed mod (Word.fromInt bucketCount))
    end

  fun add (table, key, value) =
    let
      val index = hash key
      val bucket = Array.sub (table, index)
    in
      Array.update (table, index, (key, value) :: bucket)
    end

  fun make entries =
    let
      val table = Array.array (bucketCount, [])

      val _ =
        List.app (fn (key, value) => add (table, key, value))
          (List.rev entries)
    in
      table
    end

  fun lookup (table, key) =
    let
      val bucket = Array.sub (table, hash key)
      fun find [] = NONE
        | find ((candidate, value) :: rest) =
            if candidate = key then SOME value else find rest
    in
      find bucket
    end

  fun contains (table, key) =
    case lookup (table, key) of
      SOME _ => true
    | NONE => false
end
