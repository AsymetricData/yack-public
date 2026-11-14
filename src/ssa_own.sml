structure SsaOwn =
struct
  type path = int list

  type state = (Ssa.value * path list) list

  fun isPrefix ([], _) = true
    | isPrefix (_, []) = false
    | isPrefix (x :: xs, y :: ys) = x = y andalso isPrefix (xs, ys)

  fun pathLess ([], []) = false
    | pathLess ([], _) = true
    | pathLess (_, []) = false
    | pathLess (x :: xs, y :: ys) = x < y orelse (x = y andalso pathLess (xs, ys))

  fun sortPaths paths =
    List.foldr
      (fn (p, acc) =>
        let
          fun insert [] = [p]
            | insert (q :: qs) =
                if p = q then q :: qs
                else if pathLess (p, q) then p :: q :: qs
                else q :: insert qs
        in
          insert acc
        end)
      [] paths

  fun tracked ty = not (Ssa.isCopy ty) andalso not (Ssa.isBorrow ty)

  fun holdsMemory ty =
    case ty of
      Ir.TResource => false
    | Ir.TToken => false
    | Ir.TTuple types => List.exists holdsMemory types
    | Ir.TRecord (fields, _) => List.exists (holdsMemory o #2) fields
    | _ => tracked ty

  fun needsRelease isLinearData ty =
    tracked ty andalso
    (holdsMemory ty orelse not (null (Ir.linearPaths isLinearData (ty, []))))

  fun leaf (Ir.TRecord (_, Ir.DLinear)) = true
    | leaf _ = false

  fun covers isLinearData (ty, consumed) =
    let
      fun walk (ty, path) =
        not (needsRelease isLinearData ty) orelse
        List.exists (fn q => isPrefix (q, path)) consumed orelse
        ((not (leaf ty) orelse
          List.exists (fn q => q <> path andalso isPrefix (path, q)) consumed)
         andalso
         (case ty of
           Ir.TTuple types =>
             List.all (fn x => x)
               (List.tabulate (List.length types,
                  fn i => walk (List.nth (types, i), path @ [i])))
         | Ir.TRecord (fields, _) =>
             List.all (fn x => x)
               (List.tabulate (List.length fields,
                  fn i => walk (#2 (List.nth (fields, i)), path @ [i])))
         | _ => false))
    in
      walk (ty, [])
    end

  fun typeAt (ty, []) = SOME ty
    | typeAt (Ir.TTuple types, i :: rest) =
        if i < List.length types then typeAt (List.nth (types, i), rest) else NONE
    | typeAt (Ir.TRecord (fields, _), i :: rest) =
        if i < List.length fields then typeAt (#2 (List.nth (fields, i)), rest)
        else NONE
    | typeAt _ = NONE

  fun released (ty, skip) =
    let
      fun walk (ty, path) =
        if not (tracked ty) orelse List.exists (fn s => isPrefix (s, path)) skip then []
        else if not (List.exists (fn s => isPrefix (path, s)) skip) then [path]
        else
          case ty of
            Ir.TTuple types =>
              List.concat (List.tabulate (List.length types, fn i =>
                walk (List.nth (types, i), path @ [i])))
          | Ir.TRecord (fields, _) =>
              List.concat (List.tabulate (List.length fields, fn i =>
                walk (#2 (List.nth (fields, i)), path @ [i])))
          | _ => []
    in
      walk (ty, [])
    end

  fun pathText [] = "the whole value"
    | pathText path = "path ." ^ Ir.pathToString path

  fun check isLinearData (f : Ssa.func) : Ssa.finding list =
    let
      val count =
        List.foldl (fn ((v, _), m) => Int.max (v + 1, m)) 0 (#types f)
      val types = Array.array (count, Ir.Unit)
      val _ = List.app (fn (v, ty) => Array.update (types, v, ty)) (#types f)
      val views = Array.array (count, false)
      val statics = Array.array (count, false)
      val _ =
        Vector.app
          (fn (blk : Ssa.block) =>
            List.app
              (fn Ssa.Def {dst, owns = false, kind, ...} =>
                    ( Array.update (views, dst, true)
                    ; if String.isPrefix "const" kind then
                        Array.update (statics, dst, true)
                      else () )
                | _ => ())
              (#insts blk))
          (#blocks f)
      fun tyOf v = Array.sub (types, v)
      fun owner v = tracked (tyOf v) andalso not (Array.sub (views, v))
      fun name v =
        case List.find (fn (x, _) => x = v) (#names f) of
          SOME (_, n) => "`" ^ n ^ "` (%" ^ Int.toString v ^ ")"
        | NONE => "%" ^ Int.toString v ^ " : " ^ Ir.tyToString (tyOf v)

      val errors = ref ([] : Ssa.finding list)
      fun report (blockId, site, message) =
        let
          val text = #name f ^ " b" ^ Int.toString blockId ^ ": " ^ message
        in
          if List.exists (fn e => #message e = text) (!errors) then ()
          else errors :=
                 { function = #name f, message = text, site = site, note = NONE } ::
                 !errors
        end

      fun get (state : state) v =
        Option.map #2 (List.find (fn (x, _) => x = v) state)
      fun set (state : state) (v, paths) =
        (v, paths) :: List.filter (fn (x, _) => x <> v) state

      fun full (v, consumed) = covers isLinearData (tyOf v, consumed)
      fun whole consumed = List.exists null consumed

      fun useOperand (blockId, site) (state, (v, mode)) =
        if not (owner v) then
          (case mode of
             Ssa.Part _ => state
           | Ssa.Move =>
               ( if Array.sub (views, v) andalso tracked (tyOf v)
                    andalso not (Array.sub (statics, v)) then
                   report (blockId, site, "moves " ^ name v ^ ", a view it does not own")
                 else ()
               ; state )
           | _ => state)
        else
          case get state v of
            NONE => state
          | SOME consumed =>
              case mode of
                Ssa.Read =>
                  ( if whole consumed then
                      report (blockId, site, "reads " ^ name v ^ " after it was consumed")
                    else ()
                  ; state )
              | Ssa.BorrowOf =>
                  ( if not (null consumed) then
                      report (blockId, site, "borrows " ^ name v ^ " after " ^
                              (if full (v, consumed) then "it was consumed"
                               else "a partial move"))
                    else ()
                  ; state )
              | Ssa.Move =>
                  ( if not (null consumed) then
                      report (blockId, site, "consumes " ^ name v ^ " twice (" ^
                              (if full (v, consumed) then "already consumed"
                               else "after moving " ^
                                    String.concatWith ", " (List.map pathText consumed)) ^
                              ")")
                    else ()
                  ; set state (v, [[]]) )
              | Ssa.Part path =>

                  if (case typeAt (tyOf v, path) of
                        SOME sub => not (tracked sub)
                      | NONE => false)
                  then state
                  else
                    ( case List.find (fn q => isPrefix (q, path) orelse isPrefix (path, q)) consumed of
                        SOME q =>
                          report (blockId, site, "moves " ^ pathText path ^ " of " ^ name v ^
                                  " after " ^ pathText q ^ " was consumed")
                      | NONE => ()
                    ; set state (v, path :: consumed) )

      fun define state v =
        if owner v then set state (v, []) else state

      fun instruction blockId (inst, state) =
        let
          val site =
            case inst of
              Ssa.Def {site, ...} => site
            | Ssa.Drop {site, ...} => site
            | Ssa.Destructure {site, ...} => site
            | Ssa.Effect _ => NONE
        in
        case inst of
          Ssa.Def {dst, args, ...} =>
            define (List.foldl (fn (u, st) => useOperand (blockId, site) (st, u)) state args) dst
        | Ssa.Effect {args, ...} =>
            List.foldl (fn (u, st) => useOperand (blockId, site) (st, u)) state args
        | Ssa.Drop {value, ty, skip, site} =>
            let
              val _ =
                if Array.sub (views, value) andalso tracked ty
                   andalso not (Array.sub (statics, value)) then
                  report (blockId, site, "drops " ^ name value ^
                          ", a view it does not own")
                else ()
              val _ =
                case Ir.linearPaths isLinearData (ty, skip) of
                  [] => ()
                | path :: _ =>
                    report (blockId, site, "drops " ^ name value ^
                            ", discarding the Linear value at " ^ pathText path)
            in
              if not (owner value) then state
              else
                case get state value of
                  NONE => state
                | SOME consumed =>
                    let

                      val freed = released (ty, skip)
                      val _ =
                        if whole consumed then
                          report (blockId, site, "drops " ^ name value ^
                                  " after it was consumed (double free)")
                        else
                          List.app
                            (fn q =>
                              if List.exists (fn s => isPrefix (s, q)) skip then ()
                              else
                                report (blockId, site, "drops " ^ name value ^
                                        " but " ^ pathText q ^
                                        " was already moved out (double free)"))
                            consumed
                    in
                      set state (value, sortPaths (consumed @ freed))
                    end
            end
        | Ssa.Destructure {value, site, ...} =>
            if not (owner value) then state
            else
              (case get state value of
                 NONE => state
               | SOME consumed =>
                   ( if whole consumed then
                       report (blockId, site, "destructures " ^ name value ^
                               " after it was consumed")
                     else ()
                   ; set state (value, [[]]) ))
        end

      fun leaks blockId (state : state) what =
        List.app
          (fn (v, consumed) =>
            if full (v, consumed) then ()
            else
              report (blockId, NONE, name v ^ " is not consumed " ^ what ^
                      (if null consumed then ""
                       else " (only " ^
                            String.concatWith ", " (List.map pathText consumed) ^
                            " moved out)") ^ ": leak"))
          state

      val blocks = #blocks f
      val n = Vector.length blocks
      val preds = Array.array (n, [] : int list)
      val _ =
        Vector.app
          (fn blk =>
            List.app (fn s => Array.update (preds, s, #id blk :: Array.sub (preds, s)))
              (Ssa.successors blk))
          blocks
      val outState = Array.array (n, NONE : state option)

      fun merge id =
        let
          val incoming =
            List.mapPartial (fn p => Array.sub (outState, p)) (Array.sub (preds, id))
          val values =
            List.foldl
              (fn (st, acc) =>
                List.foldl (fn ((v, _), acc) =>
                  if List.exists (fn x => x = v) acc then acc else v :: acc) acc st)
              [] incoming
        in
          List.foldl
            (fn (v, acc) =>
              let
                val seen = List.map (fn st => get st v) incoming
                val present = List.mapPartial (fn x => x) seen
              in
                if List.length present = List.length incoming then
                  let val shapes = List.map sortPaths present in
                    if List.all (fn s => s = hd shapes) shapes then
                      (v, hd shapes) :: acc
                    else if List.all (fn c => full (v, c)) present then
                      (v, [[]]) :: acc
                    else
                      ( report (id, NONE, name v ^
                                " is consumed on some incoming paths and not on others")
                      ; (v, [[]]) :: acc )
                  end
                else
                  ( if List.all (fn c => full (v, c)) present then ()
                    else
                      report (id, NONE, name v ^
                              " reaches the join unconsumed from a branch that defined it: leak")
                  ; acc )
              end)
            [] values
        end

      fun runBlock id =
        let
          val blk = Vector.sub (blocks, id)
          val entry =
            if id = 0 then
              let
                fun nameOf value =
                  case List.find (fn (v, _) => v = value) (#names f) of
                    SOME (_, name) => name
                  | NONE => ""
                fun isBorrowed value =
                  List.exists
                    (fn name => name = nameOf value)
                    (#borrowed f)
              in
                List.foldl
                  (fn ((v, _), st) => if isBorrowed v then st else define st v)
                  []
                  (#params f)
              end
            else merge id
          val entry = List.foldl (fn ((v, _), st) => define st v) entry
                        (if id = 0 then [] else #params blk)
          val afterInsts = List.foldl (instruction id) entry (#insts blk)
          val afterTerm =
            List.foldl (fn (u, st) => useOperand (id, NONE) (st, u)) afterInsts
              (Ssa.termUses (#term blk))
        in
          case #term blk of
            Ssa.Return _ => leaks id afterTerm "on the path to return"
          | _ => ();
          Array.update (outState, id, SOME afterTerm)
        end

      val reachable = Array.array (n, false)
      fun mark id =
        if Array.sub (reachable, id) then ()
        else
          ( Array.update (reachable, id, true)
          ; List.app mark (Ssa.successors (Vector.sub (blocks, id))) )
      val _ = if n > 0 then mark 0 else ()
      val pending =
        Array.tabulate (n, fn id =>
          List.length (List.filter (fn p => Array.sub (reachable, p))
                         (Array.sub (preds, id))))
      fun visit [] = ()
        | visit (id :: rest) =
            ( runBlock id
            ; visit
                (List.foldl
                   (fn (s, acc) =>
                     ( Array.update (pending, s, Array.sub (pending, s) - 1)
                     ; if Array.sub (pending, s) = 0 then acc @ [s] else acc ))
                   rest
                   (Ssa.successors (Vector.sub (blocks, id)))) )
    in
      if n > 0 then visit [0] else ();
      List.rev (!errors)
    end

  fun checkProgram (program : Ir.program) : Ssa.finding list =
    let
      fun isLinearData name =
        List.exists (fn (d : Ir.dataDef) => #name d = name andalso #linear d)
          (#datatypes program)
    in
      List.concat (List.map (check isLinearData) (Ssa.buildProgram program))
    end
end
