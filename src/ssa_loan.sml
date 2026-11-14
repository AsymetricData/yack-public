structure SsaLoan =
struct
  type place = Ssa.value * int list

  type loan = { id : Ssa.value, owners : place list, mutable : bool }

  structure IS = Ssa.IS

  fun loanable ty = not (Ssa.isCopy ty) andalso not (Ssa.isBorrow ty)

  fun hasBorrow ty =
    case ty of
      Ir.TBorrow _ => true
    | Ir.TTuple types => List.exists hasBorrow types
    | Ir.TRecord (fields, _) => List.exists (hasBorrow o #2) fields
    | Ir.TData (_, parameters, _) => List.exists hasBorrow parameters
    | Ir.TArray inner => hasBorrow inner
    | Ir.TMap inner => hasBorrow inner
    | _ => false

  fun mayCarry ty = hasBorrow ty orelse not (Ssa.isCopy ty)

  fun isPrefix ([], _) = true
    | isPrefix (_, []) = false
    | isPrefix (x :: xs, y :: ys) = x = y andalso isPrefix (xs, ys)

  fun overlap (p, q) = isPrefix (p, q) orelse isPrefix (q, p)

  fun placeLess ((v, p), (w, q)) =
    v < w orelse (v = w andalso List.collate Int.compare (p, q) = LESS)

  fun insertPlace (x, []) = [x]
    | insertPlace (x, y :: ys) =
        if x = y then y :: ys
        else if placeLess (x, y) then x :: y :: ys
        else y :: insertPlace (x, ys)

  fun unionPlaces (xs, ys) = List.foldl insertPlace ys xs

  fun viewPath kind =
    let
      fun parse text =
        List.foldr
          (fn (t, SOME acc) => Option.map (fn n => n :: acc) (Int.fromString t)
            | (_, NONE) => NONE)
          (SOME []) (String.tokens (fn c => c = #".") text)
      fun after prefix =
        if String.isPrefix prefix kind then
          parse (String.extract (kind, String.size prefix, NONE))
        else NONE
    in
      case after "field " of
        SOME p => p
      | NONE =>
          case after "index " of
            SOME p => p
          | NONE => (case after "bind " of SOME p => p | NONE => [])
    end

  type summaries = string -> int list option

  type result =
    { loans : loan list

    , carried : Ssa.value -> int list

    , liveAfter : (int list * int list list * int list) vector
    }

  fun callee kind =
    if String.isPrefix "call " kind then SOME (String.extract (kind, 5, NONE))
    else NONE

  fun builtinName kind =
    if CharVector.exists Char.isSpace kind then NONE
    else
      case CharVector.findi (fn (_, c) => c = #".") kind of
        SOME (i, _) => SOME (String.extract (kind, i + 1, NONE))
      | NONE => NONE

  fun mutator kind =
    case builtinName kind of
      SOME name =>
        LowerBuiltins.arrayMovesStoredValue name orelse
        LowerBuiltins.arrayMutatesHeader name orelse
        name = "array_take"
        orelse name = "map_set" orelse name = "map_remove"
    | NONE => false

  fun kindArgs inst =
    case inst of
      Ssa.Def {kind, args, ...} => SOME (kind, args)
    | Ssa.Effect {kind, args} => SOME (kind, args)
    | _ => NONE

  fun loanUses inst =
    case inst of
      Ssa.Drop _ => []
    | Ssa.Destructure _ => []
    | _ => Ssa.instUses inst

  fun liveness (f : Ssa.func) =
    let
      val n = Vector.length (#blocks f)
      val liveIn = Array.array (n, [] : int list)
      fun liveOut (blk : Ssa.block) =
        List.foldl (fn (s, acc) => IS.union (Array.sub (liveIn, s), acc)) []
          (Ssa.successors blk)
      fun step (i, live) =
        let
          val live =
            case Ssa.instDef i of
              SOME d => IS.remove (d, live)
            | NONE => live
        in
          List.foldl (fn ((v, _), acc) => IS.insert (v, acc)) live (loanUses i)
        end
      fun transfer (blk : Ssa.block) =
        let
          val afterTerm =
            List.foldl (fn ((v, _), acc) => IS.insert (v, acc))
              (liveOut blk) (Ssa.termUses (#term blk))
          val atEntry = List.foldr step afterTerm (#insts blk)
        in
          List.foldl (fn ((p, _), acc) => IS.remove (p, acc)) atEntry (#params blk)
        end
      fun iterate () =
        let
          val changed = ref false
          val _ =
            Vector.app
              (fn blk =>
                let val next = transfer blk in
                  if next <> Array.sub (liveIn, #id blk) then
                    (Array.update (liveIn, #id blk, next); changed := true)
                  else ()
                end)
              (#blocks f)
        in
          if !changed then iterate () else ()
        end
    in
      iterate ();
      { liveOut = fn id => liveOut (Vector.sub (#blocks f, id)), step = step }
    end

  fun analyzeWith (summaries : summaries) (seedParams : bool) (f : Ssa.func) : result =
    let
      val blocks = #blocks f
      val maxValue =
        List.foldl (fn ((v, _), m) => Int.max (v, m)) 0 (#types f)
      val carried = Array.array (maxValue + 1, [] : int list)
      val types = Array.array (maxValue + 1, NONE : Ir.ty option)
      val () = List.app (fn (v, ty) => Array.update (types, v, SOME ty)) (#types f)
      fun inRange v = v >= 0 andalso v <= maxValue
      fun get v = if inRange v then Array.sub (carried, v) else []
      val entryParams =
        if Vector.length blocks = 0 then [] else #params (Vector.sub (blocks, 0))
      val () =
        if seedParams then
          ignore (List.foldl
            (fn ((p, ty), i) =>
              ( if hasBorrow ty andalso inRange p then Array.update (carried, p, [~(i + 1)])
                else ()
              ; i + 1 ))
            0 entryParams)
        else ()
      val loans = ref ([] : loan list)
      fun addLoan ({id, owners, mutable} : loan) =
        case List.partition (fn (l : loan) => #id l = id) (!loans) of
          ([old], rest) =>
            loans := {id = id, owners = unionPlaces (owners, #owners old), mutable = mutable} :: rest
        | _ => loans := {id = id, owners = owners, mutable = mutable} :: !loans
      fun placesOf ids =
        List.foldl
          (fn (l : loan, acc) =>
            if IS.member (#id l, ids) then unionPlaces (#owners l, acc) else acc)
          [] (!loans)
      fun isCarrier v = List.exists (fn id => id >= 0) (get v)
      fun borrowValue v =
        case (if inRange v then Array.sub (types, v) else NONE) of
          SOME ty => Ssa.isBorrow ty
        | NONE => false
      fun ownedValue v =
        case (if inRange v then Array.sub (types, v) else NONE) of
          SOME ty => loanable ty
        | NONE => false

      fun defLoans (dst, ty, kind, args, owns) =
        let
          val inheritFrom =
            case Option.mapPartial summaries (callee kind) of
              SOME positions =>
                List.foldl
                  (fn (i, acc) =>
                    if i < List.length args then List.nth (args, i) :: acc else acc)
                  [] positions
            | NONE => args
          val inherited =
            List.foldl (fn ((v, _), acc) => IS.union (get v, acc)) [] inheritFrom
          val explicit =
            if kind = "borrow" orelse kind = "borrow mut" then
              List.mapPartial
                (fn (v, Ssa.BorrowOf) =>
                      if isCarrier v then
                        ( addLoan {id = dst,
                                   owners = placesOf (get v),
                                   mutable = kind = "borrow mut"}
                        ; SOME dst )
                      else if ownedValue v then
                        ( addLoan {id = dst, owners = [(v, [])],
                                   mutable = kind = "borrow mut"}
                        ; SOME dst )
                      else NONE
                  | _ => NONE)
                args
            else []
          val implicit =

            if Ssa.isBorrow ty andalso isSome (builtinName kind) then

              List.mapPartial
                (fn (v, Ssa.Read) =>
                      if not (isCarrier v) andalso ownedValue v then
                        ( addLoan {id = dst, owners = [(v, [])], mutable = false}
                        ; SOME dst )
                      else NONE
                  | _ => NONE)
                args
            else if owns orelse Ssa.isCopy ty then []
            else
              List.mapPartial
                (fn (v, _) =>
                  if not (isCarrier v) andalso ownedValue v then
                    ( addLoan {id = dst, owners = [(v, viewPath kind)], mutable = false}
                    ; SOME dst )
                  else NONE)
                args
          val own = IS.union (explicit, implicit)
        in
          if not (null own) then IS.union (own, inherited)
          else if mayCarry ty andalso (hasBorrow ty orelse not owns) then inherited
          else []
        end

      fun pass () =
        let
          val changed = ref false
          fun set (v, s) =
            if not (inRange v) then ()
            else
              let val old = get v
                  val next = IS.union (s, old)
              in
                if next <> old then (Array.update (carried, v, next); changed := true)
                else ()
              end

          fun store (kind, args) =
            case args of
              (container, _) :: rest =>
                if not (mutator kind) then ()
                else
                  let
                    val stored =
                      List.foldl (fn ((v, _), acc) => IS.union (get v, acc)) [] rest
                    val stored = List.filter (fn id => id >= 0) stored
                  in
                    if null stored then ()
                    else if borrowValue container then
                      List.app (fn (w, _) => set (w, stored)) (placesOf (get container))
                    else set (container, stored)
                  end
            | [] => ()
          fun inst i =
            ( case i of
                Ssa.Def {dst, ty, kind, args, owns, site} =>
                  set (dst, defLoans (dst, ty, kind, args, owns))
              | _ => ()
            ; Option.app store (kindArgs i) )
          fun term (Ssa.Jump (target, args)) =
                let val params = #params (Vector.sub (blocks, target)) in
                  ListPair.app (fn ((p, _), (v, _)) => set (p, get v)) (params, args)
                end
            | term _ = ()
        in
          Vector.app (fn (blk : Ssa.block) => (List.app inst (#insts blk); term (#term blk)))
            blocks;
          !changed
        end
      fun fix () = if pass () then fix () else ()
      val () = fix ()

      val {liveOut, step} = liveness f
      fun loansOf live =
        List.filter (fn id => id >= 0)
          (List.foldl (fn (v, acc) => IS.union (get v, acc)) [] live)
      val liveAfter =
        Vector.map
          (fn (blk : Ssa.block) =>
            let
              val out = liveOut (#id blk)
              val beforeTerm =
                List.foldl (fn ((v, _), acc) => IS.insert (v, acc)) out
                  (Ssa.termUses (#term blk))
              val (entry, afters) =
                List.foldr
                  (fn (i, (live, acc)) => (step (i, live), loansOf live :: acc))
                  (beforeTerm, [])
                  (#insts blk)
            in
              (loansOf entry, afters, loansOf out)
            end)
          blocks
      fun insert (x : loan, []) = [x]
        | insert (x, y :: ys) =
            if #id x < #id y then x :: y :: ys else y :: insert (x, ys)
    in
      { loans = List.foldl insert [] (!loans)
      , carried = get
      , liveAfter = liveAfter
      }
    end

  fun summarize (funcs : Ssa.func list) : summaries =
    let
      val table = ref (List.map (fn (f : Ssa.func) => (#name f, [] : int list)) funcs)
      fun lookup name =
        case List.find (fn (n, _) => n = name) (!table) of
          SOME (_, positions) => SOME positions
        | NONE => NONE
      fun returns (f : Ssa.func) =
        let
          val {carried, ...} = analyzeWith lookup true f
        in
          Vector.foldl
            (fn (blk : Ssa.block, acc) =>
              case #term blk of
                Ssa.Return (SOME (v, _)) =>
                  IS.union
                    (List.mapPartial (fn id => if id < 0 then SOME (~id - 1) else NONE)
                       (carried v),
                     acc)
              | _ => acc)
            [] (#blocks f)
        end
      fun round () =
        let
          val next = List.map (fn f => (#name f, returns f)) funcs
          val changed = next <> !table
        in
          table := next;
          if changed then round () else ()
        end
    in
      round ();
      lookup
    end

  fun analyze f = analyzeWith (fn _ => NONE) false f

  fun placeText name (v, []) = name v
    | placeText name (v, path) = name v ^ "/" ^ Ir.pathToString path

  fun checkWith (summaries : summaries) (f : Ssa.func) : Ssa.finding list =
    let
      val {loans, carried, liveAfter} = analyzeWith summaries false f
      fun loanOf id = List.find (fn (l : loan) => #id l = id) loans
      fun name v =
        case List.find (fn (x, _) => x = v) (#names f) of
          SOME (_, n) => "`" ^ n ^ "` (%" ^ Int.toString v ^ ")"
        | NONE => "%" ^ Int.toString v
      fun loanText (l : loan) =
        (if #mutable l then "mutable borrow " else "borrow ") ^ name (#id l) ^
        " of " ^ String.concatWith ", " (List.map (placeText name) (#owners l))
      val errors = ref ([] : Ssa.finding list)
      fun reportWith (blockId, site, note, message) =
        let val text = #name f ^ " b" ^ Int.toString blockId ^ ": " ^ message in
          if List.exists (fn e => #message e = text) (!errors) then ()
          else errors :=
                 { function = #name f, message = text, site = site, note = note } ::
                 !errors
        end
      fun report (blockId, site, message) =
        reportWith (blockId, site, NONE, message)
      val definedAt =
        List.concat
          (List.map
             (fn blk =>
               List.mapPartial
                 (fn Ssa.Def {dst, site, ...} => SOME (dst, site)
                   | _ => NONE)
                 (#insts blk))
             (Vector.foldr (fn (blk, acc) => blk :: acc) [] (#blocks f)))
      fun loanSite (l : loan) =
        case List.find (fn (v, _) => v = #id l) definedAt of
          SOME (_, site) => site
        | NONE => NONE
      fun usedLoans uses =
        List.foldl (fn ((v, _), acc) => IS.union (carried v, acc)) [] uses
      fun inForce (after, uses, excluded) =
        List.mapPartial loanOf
          (List.filter (fn id => not (IS.member (id, excluded)))
             (IS.union (after, usedLoans uses)))

      fun touches (inst, (v, mode)) =
        case (inst, mode) of
          (SOME (Ssa.Drop {skip, ...}), _) => (true, [], skip)
        | (_, Ssa.Move) => (true, [], [])
        | (_, Ssa.Part path) => (true, path, [])
        | (SOME (Ssa.Def {kind, owns = false, ...}), _) => (false, viewPath kind, [])
        | _ => (false, [], [])
      fun hits (v, path, skip) (l : loan) =
        List.exists
          (fn (w, q) =>
            w = v andalso overlap (path, q)
            andalso not (List.exists (fn s => isPrefix (s, q)) skip))
          (#owners l)
      fun verb (SOME (Ssa.Drop _)) = "drops "
        | verb (SOME (Ssa.Destructure _)) = "destructures "
        | verb _ = "moves "
      fun checkUses (blockId, site, inst, uses, active) =
        List.app
          (fn (use as (v, _)) =>
            let val (consumes, path, skip) = touches (inst, use) in
              List.app
                (fn (l : loan) =>
                  if not (hits (v, path, skip) l) then ()
                  else if consumes then
                    reportWith
                      (blockId, site, loanSite l,
                       verb inst ^ placeText name (v, path) ^
                       " while its " ^ loanText l ^
                       " is live: the borrow outlives its owner")
                  else if #mutable l then
                    reportWith
                      (blockId, site, loanSite l,
                       "uses " ^ placeText name (v, path) ^
                       " while its " ^ loanText l ^
                       " is live: aliasing conflict")
                  else ())
                active
            end)
          uses
      fun checkInst blockId (i, after) =
        let
          val site =
            case i of
              Ssa.Def {site, ...} => site
            | Ssa.Drop {site, ...} => site
            | Ssa.Destructure {site, ...} => site
            | Ssa.Effect _ => NONE
          val uses = Ssa.instUses i
          val excluded =
            case i of
              Ssa.Def {dst, kind, args, ...} =>
                IS.union
                  ( if isSome (loanOf dst) then [dst] else []
                  , if kind = "borrow" orelse kind = "borrow mut" then
                      List.foldl
                        (fn ((v, Ssa.BorrowOf), acc) => IS.union (carried v, acc)
                          | (_, acc) => acc)
                        [] args
                    else [] )
            | _ => []
          val active = inForce (after, uses, excluded)
        in
          checkUses (blockId, site, SOME i, uses, active);
          (case kindArgs i of
             SOME (kind, (container, _) :: _) =>
               if not (mutator kind) then ()
               else
                 let
                   val own = carried container
                   val isBorrowValue =
                     case List.find (fn (x, _) => x = container) (#types f) of
                       SOME (_, ty) => Ssa.isBorrow ty
                     | NONE => false
                   val targets =
                     if isBorrowValue then
                       List.concat
                         (List.map (fn (l : loan) => #owners l)
                            (List.mapPartial loanOf (List.filter (fn id => id >= 0) own)))
                     else [(container, [])]
                 in
                   List.app
                     (fn (l : loan) =>
                       if IS.member (#id l, own) then ()
                       else if List.exists (fn (v, p) => hits (v, p, []) l) targets then
                         report (blockId, site, "mutates " ^
                                          String.concatWith ", " (List.map (placeText name) targets) ^
                                          " while its " ^ loanText l ^
                                          " is live: aliasing conflict")
                       else ())
                     active
                 end
           | _ => ());
          case i of
            Ssa.Def {dst, kind = "borrow mut", ...} =>
              (case loanOf dst of
                 SOME mine =>
                   List.app
                     (fn (l : loan) =>
                       if List.exists (fn (v, p) => hits (v, p, []) l) (#owners mine)
                       then
                         reportWith (blockId, site, loanSite mine, "takes " ^ loanText mine ^
                                          " while its " ^ loanText l ^
                                          " is live: aliasing conflict")
                       else ())
                     active
               | NONE => ())
          | _ => ()
        end
      fun checkBlock (blk : Ssa.block) =
        let
          val (_, afters, out) = Vector.sub (liveAfter, #id blk)
          val id = #id blk
        in
          ListPair.app (checkInst id) (#insts blk, afters);
          case #term blk of
            Ssa.Return (SOME (v, _)) =>
              List.app
                (fn l =>
                  report (id, NONE, "returns " ^ name v ^ ", which carries the " ^
                              loanText l ^ ": the borrow escapes its owner"))
                (List.mapPartial loanOf (List.filter (fn x => x >= 0) (carried v)))
          | term =>
              let val uses = Ssa.termUses term in
                checkUses (id, NONE, NONE, uses, inForce (out, uses, []))
              end
        end
    in
      Vector.app checkBlock (#blocks f);
      List.rev (!errors)
    end

  fun checkFuncs (funcs : Ssa.func list) : Ssa.finding list =
    let val summaries = summarize funcs in
      List.concat (List.map (checkWith summaries) funcs)
    end

  fun checkProgram (program : Ir.program) : Ssa.finding list =
    checkFuncs (Ssa.buildProgram program)

  fun toStringWith (summaries : summaries) (f : Ssa.func) =
    let
      val {loans, liveAfter, ...} = analyzeWith summaries false f
      val name = Ssa.valueName f
      fun loanText ({id, owners, mutable} : loan) =
        "  # loan " ^ name id ^ " = " ^ (if mutable then "&mut " else "&") ^
        String.concatWith ", " (List.map (placeText name) owners)
      fun setText [] = "none"
        | setText ids = String.concatWith ", " (List.map name ids)
      val lines = String.fields (fn c => c = #"\n") (Ssa.toString f)

      fun emitBlock (blk : Ssa.block, rest, acc) =
        let
          val (entry, afters, out) = Vector.sub (liveAfter, #id blk)
          val (header, rest) = (hd rest, tl rest)
          val acc = header :: acc
          val acc = if null entry then acc else ("    # in: " ^ setText entry) :: acc
          fun insts ([], rest, _, acc) = (rest, acc)
            | insts (live :: more, rest, prev, acc) =
                let val acc = hd rest :: acc
                    val acc = if live = prev then acc
                              else ("    # live: " ^ setText live) :: acc
                in insts (more, tl rest, live, acc) end
          val (rest, acc) = insts (afters, rest, entry, acc)
          val prev = case List.rev afters of last :: _ => last | [] => entry
          val acc = hd rest :: acc
          val acc = if out = prev then acc else ("    # out: " ^ setText out) :: acc
        in
          (tl rest, acc)
        end
      val first = hd lines
      val (rest, acc) =
        Vector.foldl (fn (blk, (rest, acc)) => emitBlock (blk, rest, acc))
          (tl lines, List.rev (List.map loanText loans) @ [first])
          (#blocks f)
    in
      String.concatWith "\n" (List.rev acc @ rest)
    end

  fun programText (funcs : Ssa.func list) =
    let val summaries = summarize funcs in
      String.concat (List.map (fn f => toStringWith summaries f ^ "\n") funcs)
    end
end
