structure OwnershipLiveness =
struct
  open OwnershipTypes

  type t =
    { carry : int list ref array
    , pin : int list ref
    , remaining : int array
    , dead : bool array
    , exhausted : int list ref
    , holders : int array ref
    , journal : (unit -> unit) list ref
    , journalLength : int ref
    , touched : int list ref
    , active : int ref
    , sortedUses : (Diag.span * int) vector
    }

  fun log (t : t, undo) =
    if !(#active t) = 0 then ()
    else
      ( (#journal t) := undo :: !(#journal t)
      ; (#journalLength t) := !(#journalLength t) + 1 )

  fun setHolder (t : t, id, value) =
    let val old = Array.sub (!(#holders t), id) in
      ( log (t, fn () => Array.update (!(#holders t), id, old))
      ; Array.update (!(#holders t), id, value) )
    end

  fun setArray (t : t, array, id, value) =
    let val old = Array.sub (array, id) in
      ( log (t, fn () => Array.update (array, id, old))
      ; Array.update (array, id, value) )
    end

  fun setRef (t : t, cell, value) =
    let val old = !cell in
      ( log (t, fn () => cell := old)
      ; cell := value )
    end

  fun posLess (a : Pos.t, b : Pos.t) =
    #file a < #file b orelse (#file a = #file b andalso #offset a < #offset b)

  fun sortUses uses =
    let
      fun less ((a : Diag.span, _), (b : Diag.span, _)) = posLess (#start a, #start b)
      fun merge ([], ys) = ys
        | merge (xs, []) = xs
        | merge (x :: xs, y :: ys) =
            if less (y, x) then y :: merge (x :: xs, ys)
            else x :: merge (xs, y :: ys)
      fun split xs =
        let
          fun go ([], a, b) = (a, b)
            | go ([x], a, b) = (x :: a, b)
            | go (x :: y :: rest, a, b) = go (rest, x :: a, y :: b)
        in
          go (xs, [], [])
        end
      fun sort [] = []
        | sort [x] = [x]
        | sort xs = let val (a, b) = split xs in merge (sort a, sort b) end
    in
      Vector.fromList (sort uses)
    end

  fun bigger size needed =
    let
      fun loop current = if current > needed then current else loop (current * 2)
    in
      loop (Int.max (64, size))
    end

  fun make { bindings : Resolve.binding list
           , uses : Resolve.use list
           } =
    let
      val bindingCount =
        List.foldl (fn (binding, size) => Int.max (#id binding + 1, size)) 1
          bindings
      val carry = Array.tabulate (bindingCount, fn _ => ref [])
      val remaining = Array.array (bindingCount, 0)
      val dead = Array.array (bindingCount, false)
      fun addUse (use : Resolve.use) =
        let val id = #binding use in
          if id >= 0 andalso id < bindingCount then
            Array.update (remaining, id, Array.sub (remaining, id) + 1)
          else ()
        end
      val _ = List.app addUse uses
      val sortedUses =
        sortUses
          (List.mapPartial
             (fn (use : Resolve.use) =>
               if #binding use >= 0 andalso #binding use < bindingCount
               then SOME (#span use, #binding use)
               else NONE)
             uses)
    in
      { carry = carry
      , pin = ref []
      , remaining = remaining
      , dead = dead
      , exhausted = ref []
      , holders = ref (Array.array (64, 0))
      , journal = ref []
      , journalLength = ref 0
      , touched = ref []
      , active = ref 0
      , sortedUses = sortedUses
      }
    end

  fun usesLeft (t : t, id) =
    let val remaining = #remaining t in
      if id >= 0 andalso id < Array.length remaining then
        Array.sub (remaining, id)
      else 0
    end

  fun isDead (t : t, id) =
    let val dead = #dead t in
      id >= 0 andalso id < Array.length dead andalso Array.sub (dead, id)
    end

  fun markDead (t : t, id) =
    let val dead = #dead t in
      if id >= 0 andalso id < Array.length dead then
        setArray (t, dead, id, true)
      else ()
    end

  fun noteUse (t : t, id) =
    let
      val remaining = #remaining t
      val current = usesLeft (t, id)
    in
      if current > 0 andalso id < Array.length remaining then
        let
          val next = current - 1
          val _ = setArray (t, remaining, id, next)
          val _ =
            if next = 0 andalso not (isDead (t, id)) then
              ( markDead (t, id)
              ; setRef (t, #exhausted t, id :: !(#exhausted t)) )
            else ()
        in
          ()
        end
      else if current = 0 andalso not (isDead (t, id)) then

        ( markDead (t, id)
        ; setRef (t, #exhausted t, id :: !(#exhausted t)) )
      else ()
    end

  fun ensureHolders (t : t, id) =
    let
      val cell = #holders t
      val current = !cell
      val size = Array.length current
    in
      if id < size then ()
      else
        let
          val grown = Array.array (bigger size (id + 1), 0)
          val _ = Array.copy { src = current, dst = grown, di = 0 }
        in
          cell := grown
        end
    end

  fun bumpHolder (t : t, id) =
    let val _ = ensureHolders (t, id) in
      setHolder (t, id, Array.sub (!(#holders t), id) + 1)
    end

  fun dropHolder (t : t, id) =
    let val holders = !(#holders t) in
      if id >= 0 andalso id < Array.length holders then
        setHolder (t, id, Int.max (0, Array.sub (holders, id) - 1))
      else ()
    end

  fun holderCount (t : t, id) =
    let val holders = !(#holders t) in
      if id >= 0 andalso id < Array.length holders then Array.sub (holders, id)
      else 0
    end

  fun carriedBy (t : t, id) =
    let val carry = #carry t in
      if id >= 0 andalso id < Array.length carry then !(Array.sub (carry, id))
      else []
    end

  fun addToCell (t : t, cell, loans) =
    if null loans then ()
    else
      let
        val existing = !cell
        val added =
          List.foldl
            (fn (loan, acc) =>
              if List.exists (fn other => other = loan) existing orelse
                 List.exists (fn other => other = loan) acc
              then acc
              else loan :: acc)
            []
            loans
        val _ = List.app (fn loan => bumpHolder (t, loan)) added
      in
        setRef (t, cell, added @ existing)
      end

  fun carry (t : t, id, loans) =
    let val carry = #carry t in
      if usesLeft (t, id) = 0 then ()
      else if id < 0 orelse id >= Array.length carry then ()
      else
        ( addToCell (t, Array.sub (carry, id), loans)
        ; setRef (t, #touched t, id :: !(#touched t)) )
    end

  fun releaseCarried (t : t, cell) =
    let
      val held = !cell
      val _ = List.app (fn loan => dropHolder (t, loan)) held
    in
      setRef (t, cell, [])
    end

  fun release (t : t, id) =
    let val carry = #carry t in
      if id >= 0 andalso id < Array.length carry then
        releaseCarried (t, Array.sub (carry, id))
      else ()
    end

  fun pin (t : t, loans) = addToCell (t, #pin t, loans)

  fun flush (t : t, loans : loan list) =
    if List.null loans then loans
    else
      let
        val _ = List.app (fn id => release (t, id)) (!(#exhausted t))
        val _ = setRef (t, #exhausted t, [])
        val _ = releaseCarried (t, #pin t)
        fun keep (loan : loan) = holderCount (t, #id loan) > 0
      in
        List.filter keep loans
      end

  fun usesIn (t : t, span : Diag.span) =
    let
      val uses = #sortedUses t
      val count = Vector.length uses
      fun startOf i = #start (#1 (Vector.sub (uses, i)))

      fun search (low, high) =
        if low >= high then low
        else
          let val middle = (low + high) div 2 in
            if posLess (startOf middle, #start span) then search (middle + 1, high)
            else search (low, middle)
          end
      fun collect (i, acc) =
        if i >= count then acc
        else
          let val (useSpan, id) = Vector.sub (uses, i) in
            if posLess (#start useSpan, #stop span) then
              collect (i + 1,
                if posLess (#stop span, #stop useSpan) then acc else id :: acc)
            else acc
          end
    in
      collect (search (0, count), [])
    end

  fun rollback (t : t, length) =
    if !(#journalLength t) <= length then ()
    else
      case !(#journal t) of
        [] => ()
      | undo :: rest =>
          ( (#journal t) := rest
          ; (#journalLength t) := !(#journalLength t) - 1
          ; undo ()
          ; rollback (t, length) )

  fun branches (t : t, arms : (Diag.span list * (unit -> 'a)) list) : 'a list =
    let
      val _ = (#active t) := !(#active t) + 1
      val start = !(#journalLength t)
      val armUses =
        List.map (fn (spans, _) => List.concat (List.map (fn span => usesIn (t, span)) spans))
          arms
      fun others index =
        List.concat
          (List.rev
             (#2 (List.foldl
                    (fn (ids, (j, acc)) => (j + 1, if j = index then acc else ids :: acc))
                    (0, [])
                    armUses)))
      fun runArm ((_, analyze), (index, acc)) =
        let
          val _ = rollback (t, start)
          val _ = setRef (t, #touched t, [])
          val _ = List.app (fn id => noteUse (t, id)) (others index)
          val result = analyze ()
          val kept =
            List.mapPartial
              (fn id => if isDead (t, id) then NONE else SOME (id, carriedBy (t, id)))
              (!(#touched t))
          val pinned = !(#pin t)
        in
          (index + 1, (result, kept, pinned) :: acc)
        end
      val (_, outcomes) = List.foldl runArm (0, []) arms
      val outcomes = List.rev outcomes
      val _ = rollback (t, start)
      val _ = List.app (fn ids => List.app (fn id => noteUse (t, id)) ids) armUses
      val _ =
        List.app
          (fn (_, kept, pinned) =>
            ( List.app
                (fn (id, loans) =>
                  if isDead (t, id) then ()
                  else
                    ( addToCell (t, Array.sub (#carry t, id), loans)
                    ; setRef (t, #touched t, id :: !(#touched t)) ))
                kept
            ; addToCell (t, #pin t, pinned) ))
          outcomes

      val _ = (#active t) := !(#active t) - 1
      val _ =
        if !(#active t) = 0 then ((#journal t) := []; (#journalLength t) := 0)
        else ()
    in
      List.map #1 outcomes
    end
end
