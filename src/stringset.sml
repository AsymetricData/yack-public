structure StringSet =
struct
  type t = {buckets : string list array, size : int}

  fun create size =
    let val size = Int.max (17, size)
    in {buckets = Array.array (size, []), size = size} end

  fun hash (size, name) =
    let
      val length = String.size name
      fun loop (index, accumulator) =
        if index >= length then accumulator
        else
          loop
            ( index + 1
            , (accumulator * 31 + Char.ord (String.sub (name, index))) mod size
            )
    in
      loop (0, 0)
    end

  fun member ({buckets, size} : t, name) =
    let val bucket = Array.sub (buckets, hash (size, name))
    in List.exists (fn candidate => candidate = name) bucket end

  fun insert ({buckets, size} : t, name) =
    let
      val index = hash (size, name)
      val bucket = Array.sub (buckets, index)
    in
      if List.exists (fn candidate => candidate = name) bucket then false
      else
        let val _ = Array.update (buckets, index, name :: bucket)
        in true end
    end
end
