structure Ssa =
struct
  type value = int

  type finding =
    { function : string
    , message : string
    , site : Diag.span option
    , note : Diag.span option
    }

  datatype mode =
      Move
    | Read
    | BorrowOf

    | Part of int list

  datatype inst =
      Def of
        { dst : value
        , ty : Ir.ty
        , kind : string
        , args : (value * mode) list
        , owns : bool
        , site : Diag.span option
        }
    | Drop of
        { value : value
        , ty : Ir.ty
        , skip : int list list
        , site : Diag.span option
        }

    | Destructure of
        { value : value
        , ty : Ir.ty
        , site : Diag.span option
        }

    | Effect of { kind : string, args : (value * mode) list }

  datatype terminator =
      Jump of int * (value * mode) list
    | Branch of value * int * int
    | Return of (value * mode) option
    | Unreachable

  type block =
    { id : int
    , params : (value * Ir.ty) list
    , insts : inst list
    , term : terminator
    }

  type func =
    { name : string
    , params : (value * Ir.ty) list
    , blocks : block vector
    , names : (value * string) list
    , types : (value * Ir.ty) list

    , borrowed : string list
    }

  fun isCopy ty =
    case ty of
      Ir.Int _ => true
    | Ir.Float _ => true
    | Ir.Bool => true
    | Ir.Unit => true
    | Ir.TBorrow (_, false) => true
    | Ir.TCPtr => true
    | Ir.TFun _ => true
    | Ir.TData (_, _, copy) => copy
    | Ir.TTuple types => List.all isCopy types

    | Ir.TRecord (_, discipline) => discipline = Ir.DCopy
    | _ => false

  fun isBorrow (Ir.TBorrow _) = true
    | isBorrow _ = false

  fun passMode ty = if isCopy ty orelse isBorrow ty then Read else Move

  type builder =
    { next : int ref
    , blocks : block list ref
    , current : (int * (value * Ir.ty) list * inst list) option ref
    , nextBlock : int ref
    , names : (value * string) list ref
    , types : (value * Ir.ty) list ref
    }

  fun newBuilder () : builder =
    { next = ref 0
    , blocks = ref []
    , current = ref NONE
    , nextBlock = ref 0
    , names = ref []
    , types = ref []
    }

  fun freshValue (b : builder) ty =
    let val v = !(#next b) in
      #next b := v + 1;
      #types b := (v, ty) :: !(#types b);
      v
    end

  fun reserveBlock (b : builder) =
    let val id = !(#nextBlock b) in #nextBlock b := id + 1; id end

  fun startBlock (b : builder) (id, params) =
    #current b := SOME (id, params, [])

  fun emit (b : builder) inst =
    case !(#current b) of
      SOME (id, params, insts) => #current b := SOME (id, params, inst :: insts)
    | NONE => ()

  fun terminate (b : builder) term =
    case !(#current b) of
      SOME (id, params, insts) =>
        ( #blocks b :=
            { id = id, params = params, insts = List.rev insts, term = term }
            :: !(#blocks b)
        ; #current b := NONE )
    | NONE => ()

  fun live (b : builder) = isSome (!(#current b))

  fun lookup env name =
    case List.find (fn (n, _) => n = name) env of
      SOME (_, v) => SOME v
    | NONE => NONE

  fun tyOfExpr expression =
    case expression of
      Ir.IntLit (_, ty) => ty
    | Ir.FloatLit (_, ty) => ty
    | Ir.StringLit (_, ty) => ty
    | Ir.BoolLit _ => Ir.Bool
    | Ir.UnitLit => Ir.Unit
    | Ir.Var (_, ty, _) => ty
    | Ir.Bin (_, _, _, ty, _) => ty
    | Ir.Neg (_, ty) => ty
    | Ir.Call (_, _, ty, _) => ty
    | Ir.If (_, _, _, ty) => ty
    | Ir.Let (_, body, _) => tyOfExpr body
    | Ir.Tuple (_, ty) => ty
    | Ir.Record (_, ty) => ty
    | Ir.Field (_, _, ty) => ty
    | Ir.TupleIndex (_, _, ty) => ty
    | Ir.RawField (_, _, ty) => ty
    | Ir.Ctor (_, _, _, ty, _) => ty
    | Ir.Match (_, _, ty) => ty
    | Ir.PrintString _ => Ir.Unit
    | Ir.StringBuiltin (_, _, ty, _) => ty
    | Ir.BytesBuiltin (_, _, ty, _) => ty
    | Ir.SysBuiltin (_, _, ty) => ty
    | Ir.ArrayBuiltin (_, _, ty, _) => ty
    | Ir.MapBuiltin (_, _, ty, _) => ty
    | Ir.Copy (_, ty) => ty
    | Ir.Borrow (_, _, ty) => ty
    | Ir.Closure (_, ty) => ty
    | Ir.ClosureRef (_, ty) => ty
    | Ir.Apply (_, _, ty) => ty

  fun place expression =
    case expression of
      Ir.Var (name, _, _) => SOME (name, [])
    | Ir.Field (base, index, _) =>
        Option.map (fn (n, p) => (n, p @ [index])) (place base)
    | Ir.TupleIndex (base, index, _) =>
        Option.map (fn (n, p) => (n, p @ [index])) (place base)
    | _ => NONE

  fun findData (datatypes : Ir.dataDef list) name =
    List.find (fn (d : Ir.dataDef) => #name d = name) datatypes

  fun isLinearIn datatypes name =
    case findData datatypes name of
      SOME d => #linear d
    | NONE => false

fun siteOf expression =
  let
    fun first [] = NONE
      | first (candidate :: rest) =
          (case siteOf candidate of
             SOME site => SOME site
           | NONE => first rest)
  in
    case expression of
      Ir.Var (_, _, site) => site
    | Ir.Field (base, _, _) => siteOf base
    | Ir.TupleIndex (base, _, _) => siteOf base
    | Ir.RawField (base, _, _) => siteOf base
    | Ir.Borrow (_, operand, _) => siteOf operand
    | Ir.Copy (value, _) => siteOf value
    | Ir.Bin (_, left, right, _, _) =>
        (case siteOf left of
           SOME site => SOME site
         | NONE => siteOf right)
    | Ir.Neg (operand, _) => siteOf operand
    | Ir.Call (_, arguments, _, _) => first arguments
    | Ir.Apply (callee, arguments, _) =>
        (case siteOf callee of
           SOME site => SOME site
         | NONE => first arguments)
    | Ir.Ctor (_, _, SOME value, _, _) => siteOf value
    | Ir.Tuple (values, _) => first values
    | Ir.Record (fields, _) =>
        first (List.map (fn (_, value) => value) fields)
    | Ir.StringBuiltin (_, arguments, _, _) =>
        first (List.map #1 arguments)
    | Ir.BytesBuiltin (_, arguments, _, _) =>
        first (List.map #1 arguments)
    | Ir.MapBuiltin (_, arguments, _, _) =>
        first (List.map #1 arguments)
    | Ir.SysBuiltin (_, arguments, _) => first arguments
    | Ir.ArrayBuiltin (_, arguments, _, _) => first arguments
    | Ir.Match (scrutinee, _, _) => siteOf scrutinee
    | Ir.PrintString (value, _) => siteOf value
    | Ir.Let (_, body, _) => siteOf body
    | _ => NONE
  end

  fun buildWith (datatypes : Ir.dataDef list) (definition : Ir.funDef) : func =
    let
      val b = newBuilder ()
      fun payloadOf (data, constructor) =
        case findData datatypes data of
          SOME d =>
            (case List.find (fn (c, _) => c = constructor) (#constructors d) of
               SOME (_, payload) => payload
             | NONE => NONE)
        | NONE => NONE
      fun holdsLinear ty = not (null (Ir.linearPaths (isLinearIn datatypes) (ty, [])))
      val currentSite = ref (NONE : Diag.span option)
      fun define owns (ty, kind, args) =
        let val dst = freshValue b ty in
          emit b (Def {dst = dst, ty = ty, kind = kind, args = args,
                       owns = owns, site = !currentSite});
          dst
        end
      val def = define true
      val view = define false
      fun moveIf ty = if isCopy ty orelse isBorrow ty then Read else Move

      fun operand env (expression, mode) =
        ( currentSite := siteOf expression
        ; case (place expression, mode) of
          (SOME (name, path as _ :: _), Move) =>
            (case lookup env name of
               SOME root =>
                 def (tyOfExpr expression, "part " ^ Ir.pathToString path,
                      [(root, Part path)])
             | NONE => lower env expression)
        | _ =>
            case expression of

              Ir.Let (bindings, body, drops) =>
                let
                  val env' = bind env bindings
                  val result = operand env' (body, mode)
                in
                  emitDrops env' drops;
                  result
                end
            | _ => lower env expression )

      and retag (value as Ir.Record (fields, Ir.TRecord (shape, _)),
                 ty as Ir.TRecord (declared, _)) =
            if List.map #1 shape = List.map #1 declared
            then Ir.Record (fields, ty)
            else value
        | retag (value, _) = value

      and bind env bindings =
        List.foldl
          (fn ((name, ty, value), env) =>
            let val v = operand env (retag (value, ty), passMode ty) in
              #names b := (v, name) :: !(#names b);
              (name, v) :: env
            end)
          env bindings

      and use env (expression, mode) = (operand env (expression, mode), mode)

      and owned env expression =
        use env (expression, passMode (tyOfExpr expression))

      and lower env expression =
        ( currentSite := siteOf expression
        ; case expression of
          Ir.IntLit (n, ty) => def (ty, "const " ^ Int64.toString n, [])
        | Ir.FloatLit (_, ty) => def (ty, "const float", [])

        | Ir.StringLit (text, ty) => view (ty, "const \"" ^ String.toString text ^ "\"", [])
        | Ir.BoolLit v => def (Ir.Bool, "const " ^ Bool.toString v, [])
        | Ir.UnitLit => def (Ir.Unit, "const ()", [])
        | Ir.Var (name, ty, _) =>
            (case lookup env name of
               SOME v => v
             | NONE => view (ty, "global " ^ name, []))
        | Ir.Bin ("^", _, _, ty, _) =>
            let
              fun flatten (Ir.Bin ("^", l, r, _, _)) = flatten l @ flatten r
                | flatten e = [e]
            in
              def (ty, "concat",
                   List.map (fn e => use env (e, Move)) (flatten expression))
            end
        | Ir.Bin (operator, left, right, ty, _) =>
            def (ty, operator, [use env (left, Read), use env (right, Read)])
        | Ir.Neg (operand, ty) => def (ty, "neg", [use env (operand, Read)])
        | Ir.Call (name, arguments, ty, _) =>
            def (ty, "call " ^ name, List.map (owned env) arguments)
        | Ir.Apply (callee, arguments, ty) =>
            def (ty, "apply",
                 use env (callee, Read) :: List.map (owned env) arguments)
        | Ir.If (condition, thenBranch, elseBranch, ty) =>
            let
              val c = lower env condition
              val thenId = reserveBlock b
              val elseId = reserveBlock b
              val joinId = reserveBlock b
              val result = freshValue b ty
              fun arm (id, branch) =
                ( startBlock b (id, [])
                ; let val v = operand env (branch, passMode ty) in
                    terminate b (Jump (joinId, [(v, passMode ty)]))
                  end )
            in
              terminate b (Branch (c, thenId, elseId));
              arm (thenId, thenBranch);
              arm (elseId, elseBranch);
              startBlock b (joinId, [(result, ty)]);
              result
            end
        | Ir.Let (bindings, body, drops) =>
            let
              val env' = bind env bindings
              val result = lower env' body
            in
              emitDrops env' drops;
              result
            end
        | Ir.Tuple (values, ty) =>
            def (ty, "tuple", List.map (owned env) values)
        | Ir.Record (fields, ty) =>
            def (ty, "record", List.map (fn (_, e) => owned env e) fields)
        | Ir.Field (base, index, ty) =>
            view (ty, "field " ^ Int.toString index, [use env (base, Read)])
        | Ir.RawField (base, offset, ty) =>
            view (ty, "raw-field " ^ Int.toString offset,
                  [use env (base, Read)])
        | Ir.TupleIndex (base, index, ty) =>
            view (ty, "index " ^ Int.toString index, [use env (base, Read)])
        | Ir.Ctor (_, constructor, argument, ty, _) =>
            def (ty, "ctor " ^ constructor,
                 case argument of
                   SOME e => [owned env e]
                 | NONE => [])
        | Ir.Match (scrutinee, branches, ty) => lowerMatch env (scrutinee, branches, ty)
        | Ir.PrintString (argument, consume) =>
            ( emit b (Effect {kind = "print", args = [use env (argument, if consume then Move else Read)]})
            ; def (Ir.Unit, "const ()", []) )
        | Ir.StringBuiltin (name, arguments, ty, _) =>
            def (ty, "string." ^ name, builtinArgs env arguments)
        | Ir.BytesBuiltin (name, arguments, ty, _) =>
            def (ty, "bytes." ^ name, builtinArgs env arguments)
        | Ir.MapBuiltin (name, arguments, ty, _) =>

            (if name = "map_get" then view else def)
              (ty, "map." ^ name,
               if name = "map_set" then
                 List.tabulate (List.length arguments, fn i =>
                   let val (e, _) = List.nth (arguments, i) in
                     if i = 0 then use env (e, Read)
                     else use env (e, moveIf (tyOfExpr e))
                   end)
               else builtinArgs env arguments)
        | Ir.SysBuiltin (name, arguments, ty) =>
            def (ty, "sys." ^ name, List.map (fn e => use env (e, Read)) arguments)
        | Ir.ArrayBuiltin (name, arguments, ty, _) =>

            let
              val stored = LowerBuiltins.arrayStoreIndex name
              val reads = name = "array_get" orelse
                          name = "array_get_unchecked" orelse
                          name = "array_kernel_load"
            in
              (if reads then view else def)
                (ty, "array." ^ name,
                 List.tabulate (List.length arguments, fn i =>
                   let val e = List.nth (arguments, i) in
                     if stored = SOME i then use env (e, moveIf (tyOfExpr e))
                     else use env (e, Read)
                   end))
            end
        | Ir.Copy (operand, ty) => def (ty, "copy", [use env (operand, Read)])

        | Ir.Borrow (_, Ir.StringLit (text, lty), ty) =>
            def (ty, "borrow",
                 [(view (lty, "const \"" ^ String.toString text ^ "\"", []), BorrowOf)])
        | Ir.Borrow (mutable, operand, ty) =>
            def (ty, if mutable then "borrow mut" else "borrow",
                 [use env (operand, BorrowOf)])
        | Ir.Closure (name, ty) => def (ty, "closure " ^ name, [])
        | Ir.ClosureRef (name, ty) => def (ty, "closure_ref " ^ name, []) )

      and builtinArgs env arguments =
        List.map (fn (e, consume) => use env (e, if consume then Move else Read))
          arguments

      and emitDrops env drops =
        List.app
          (fn drop as {name, ty, skip, ...} =>
            case lookup env name of
              SOME v => emit b (Drop {value = v, ty = ty, skip = skip, site = #site drop})
            | NONE => ())
          drops

      and lowerMatch env (scrutinee, branches, ty) =
        let
          val sTy = tyOfExpr scrutinee
          val owning = not (isBorrow sTy) andalso not (isCopy sTy)
          val s = operand env (scrutinee, if owning then Move else Read)
          val joinId = reserveBlock b
          val result = freshValue b ty
          fun name (v, n) = #names b := (v, n) :: !(#names b)
          fun tracked ty = owning andalso not (isCopy ty) andalso not (isBorrow ty)
          fun part (root, path, ty) =
            if null path then root
            else def (ty, "part " ^ Ir.pathToString path, [(root, Part path)])

          fun views env (root, path, pattern) =
            case pattern of
              Ir.PVar (n, pty) =>
                let val v = view (pty, "bind " ^ Ir.pathToString path, [(root, Read)])
                in name (v, n); (n, v) :: env end
            | Ir.PString (_, SOME rest, _) => views env (root, path @ [~1], rest)
            | Ir.PBytes (_, SOME rest, _) => views env (root, path @ [~1], rest)
            | Ir.PTuple (ps, _) =>
                #2 (List.foldl (fn (p, (i, env)) =>
                      (i + 1, views env (root, path @ [i], p))) (0, env) ps)
            | Ir.PRecord (fields, _) =>
                #2 (List.foldl (fn ((_, p), (i, env)) =>
                      (i + 1, views env (root, path @ [i], p))) (0, env) fields)
            | Ir.PCtor (_, _, SOME p, _) => views env (root, path @ [0], p)
            | _ => env

          fun destruct env (root, path, ty, pattern) =
            case pattern of
              Ir.PWildcard =>
                ( if tracked ty then
                    emit b (Drop {value = part (root, path, ty), ty = ty, skip = [],
                          site = !currentSite})
                  else ()
                ; env )
            | Ir.PVar (n, pty) =>
                let
                  val v =
                    if tracked pty then
                      (if null path then root
                       else def (pty, "bind " ^ Ir.pathToString path,
                                 [(root, Part path)]))
                    else view (pty, "bind " ^ Ir.pathToString path, [(root, Read)])
                in
                  name (v, n); (n, v) :: env
                end
            | Ir.PString (_, SOME rest, sty) => suffix env (root, sty, rest)
            | Ir.PBytes (_, SOME rest, sty) => suffix env (root, sty, rest)
            | Ir.PTuple (ps, Ir.TTuple types) =>
                #2 (List.foldl (fn (p, (i, env)) =>
                      (i + 1, destruct env (root, path @ [i], List.nth (types, i), p)))
                      (0, env) ps)
            | Ir.PRecord (fields, Ir.TRecord (types, _)) =>
                #2 (List.foldl (fn ((_, p), (i, env)) =>
                      (i + 1, destruct env (root, path @ [i], #2 (List.nth (types, i)), p)))
                      (0, env) fields)
            | Ir.PCtor (data, constructor, SOME p, _) =>
                (case payloadOf (data, constructor) of
                   SOME pty => destruct env (root, path @ [0], pty, p)
                 | NONE => env)
            | _ => env

          and suffix env (root, sty, rest) =
            if owning then
              let
                val v = def (sty, "suffix", [(root, Read)])
                val env' = destruct env (v, [], sty, rest)
              in
                finish (v, sty, rest); env'
              end
            else views env (root, [~1], rest)
          and finish (root, rty, pattern) =
            case pattern of
              Ir.PVar _ => ()
            | Ir.PWildcard => ()
            | _ => if tracked rty then emit b (Destructure {value = root, ty = rty, site = !currentSite}) else ()

          val remaining =
            ref (case sTy of
                   Ir.TData (name, _, _) =>
                     (case findData datatypes name of
                        SOME d => SOME (#constructors d)
                      | NONE => NONE)
                 | _ => NONE)
          fun irrefutable p =
            case p of
              Ir.PWildcard => true
            | Ir.PVar _ => true
            | Ir.PTuple (ps, _) => List.all irrefutable ps
            | Ir.PRecord (fs, _) => List.all (irrefutable o #2) fs
            | _ => false
          fun caught (pattern, guard) =
            case (pattern, guard, !remaining) of
              (Ir.PCtor (_, c, sub, _), NONE, SOME cs) =>
                if (case sub of NONE => true | SOME p => irrefutable p) then
                  remaining := SOME (List.filter (fn (x, _) => x <> c) cs)
                else ()
            | _ => ()

          fun releasesOnly pattern =
            case (pattern, !remaining) of
              (Ir.PWildcard, SOME cs) =>
                List.all (fn (_, payload) =>
                  case payload of
                    NONE => true
                  | SOME ty => not (holdsLinear ty)) cs
            | _ => false
          fun loop [] = terminate b Unreachable
            | loop ((pattern, guard, body, drops) :: rest) =
                let
                  val bodyId = reserveBlock b
                  val failId = reserveBlock b
                  val test =
                    def (Ir.Bool, "test " ^ Ir.patternToString pattern,
                         [(s, Read)])
                  val _ = terminate b (Branch (test, bodyId, failId))
                  val _ = startBlock b (bodyId, [])
                  val _ =
                    case guard of
                      NONE => ()
                    | SOME condition =>
                        let
                          val passId = reserveBlock b
                          val c = lower (views env (s, [], pattern)) condition
                        in
                          terminate b (Branch (c, passId, failId));
                          startBlock b (passId, [])
                        end
                  val env' =
                    if releasesOnly pattern andalso tracked sTy then
                      (emit b (Destructure {value = s, ty = sTy, site = !currentSite}); env)
                    else destruct env (s, [], sTy, pattern)
                  val _ = finish (s, sTy, pattern)
                  val _ = caught (pattern, guard)
                  val v = operand env' (body, passMode ty)
                  val _ = emitDrops env' drops
                  val _ = terminate b (Jump (joinId, [(v, passMode ty)]))
                in
                  startBlock b (failId, []);
                  loop rest
                end
        in
          loop branches;
          startBlock b (joinId, [(result, ty)]);
          result
        end

      val entry = reserveBlock b
      val params =
        List.map (fn (name, ty) => (name, freshValue b ty)) (#params definition)
      val _ = List.app (fn (name, v) => #names b := (v, name) :: !(#names b)) params
      val _ = startBlock b (entry, List.map (fn (name, v) =>
                (v, #2 (valOf (List.find (fn (n, _) => n = name) (#params definition)))))
                params)
      val env = List.map (fn (name, v) => (name, v)) params
      val result = operand env (#body definition, passMode (#result definition))
      val _ = emitDrops env (#drops definition)
      val _ = terminate b (Return (SOME (result, passMode (#result definition))))
      val count = !(#nextBlock b)
      val table =
        Array.tabulate
          (count, fn id =>
            {id = id, params = [], insts = [], term = Unreachable} : block)
      val _ = List.app (fn (blk : block) => Array.update (table, #id blk, blk))
                (!(#blocks b))
      val blocks = Array.vector table
    in
      { name = #name definition
      , params = List.map (fn (name, v) =>
          (v, #2 (valOf (List.find (fn (n, _) => n = name) (#params definition)))))
          params
      , blocks = blocks
      , names = !(#names b)
      , types = !(#types b)
      , borrowed = #borrowed definition
      }
    end

  val build = buildWith []

  fun buildProgram (program : Ir.program) =
    List.map (buildWith (#datatypes program)) (#functions program)

  fun successors (blk : block) =
    case #term blk of
      Jump (target, _) => [target]
    | Branch (_, t, f) => [t, f]
    | Return _ => []
    | Unreachable => []

  fun instUses inst =
    case inst of
      Def {args, ...} => args
    | Effect {args, ...} => args
    | Drop {value, ...} => [(value, Move)]
    | Destructure {value, ...} => [(value, Move)]

  fun instDef (Def {dst, ...}) = SOME dst
    | instDef _ = NONE

  fun termUses term =
    case term of
      Jump (_, args) => args
    | Branch (c, _, _) => [(c, Read)]
    | Return (SOME arg) => [arg]
    | _ => []

  structure IS = struct

    fun insert (x, []) = [x]
      | insert (x, y :: ys) =
          if x = y then y :: ys
          else if x < y then x :: y :: ys
          else y :: insert (x, ys)
    fun remove (x, ys) = List.filter (fn y => y <> x) ys
    fun union (xs, ys) = List.foldl insert ys xs
    fun member (x, ys) = List.exists (fn y => y = x) ys
  end

  fun liveness (f : func) =
    let
      val n = Vector.length (#blocks f)
      val liveIn = Array.array (n, [] : int list)
      fun liveOut (blk : block) =
        List.foldl (fn (s, acc) => IS.union (Array.sub (liveIn, s), acc)) []
          (successors blk)
      fun transfer (blk : block) =
        let
          val afterTerm =
            List.foldl (fn ((v, _), acc) => IS.insert (v, acc))
              (liveOut blk) (termUses (#term blk))
          val beforeInsts =
            List.foldr
              (fn (inst, acc) =>
                let
                  val acc =
                    case instDef inst of
                      SOME d => IS.remove (d, acc)
                    | NONE => acc
                in
                  List.foldl (fn ((v, _), acc) => IS.insert (v, acc)) acc
                    (instUses inst)
                end)
              afterTerm (#insts blk)
        in
          List.foldl (fn ((p, _), acc) => IS.remove (p, acc)) beforeInsts
            (#params blk)
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
              (Vector.fromList (List.rev (Vector.foldl (op ::) [] (#blocks f))))
        in
          if !changed then iterate () else ()
        end
    in
      iterate ();
      { liveIn = liveIn, liveOut = fn id => liveOut (Vector.sub (#blocks f, id)) }
    end

  fun lastUses (f : func) =
    let
      val {liveOut, ...} = liveness f
    in
      Vector.map
        (fn (blk : block) =>
          let
            val afterTerm = liveOut (#id blk)
            val termLast =
              List.filter (fn (v, _) => not (IS.member (v, afterTerm)))
                (termUses (#term blk))
            val live0 =
              List.foldl (fn ((v, _), acc) => IS.insert (v, acc)) afterTerm
                (termUses (#term blk))
            val (_, marks) =
              List.foldr
                (fn (inst, (live, marks)) =>
                  let
                    val uses = instUses inst
                    val last =
                      List.filter (fn (v, _) => not (IS.member (v, live))) uses
                    val live =
                      case instDef inst of
                        SOME d => IS.remove (d, live)
                      | NONE => live
                    val live =
                      List.foldl (fn ((v, _), acc) => IS.insert (v, acc)) live uses
                  in
                    (live, List.map #1 last :: marks)
                  end)
                (live0, [])
                (#insts blk)
          in
            (marks, List.map #1 termLast)
          end)
        (#blocks f)
    end

  fun valueName (f : func) v =
    case List.find (fn (x, _) => x = v) (#names f) of
      SOME (_, name) => "%" ^ Int.toString v ^ "." ^ name
    | NONE => "%" ^ Int.toString v

  fun tyText ty =
    let
      fun paren (need, level, text) =
        if level < need then "(" ^ text ^ ")" else text
      fun go need ty =
        case ty of
          Ir.Int width => if width = 32 then "int" else "int" ^ Int.toString width
        | Ir.Float width => if width = 64 then "float" else "float" ^ Int.toString width
        | Ir.Bool => "bool"
        | Ir.Unit => "unit"
        | Ir.TToken => "token"
        | Ir.TResource => "resource"
        | Ir.TString => "string"
        | Ir.TCPtr => "c_ptr"
        | Ir.TRecord (fields, discipline) =>
            Ir.disciplinePrefix discipline ^ "{ " ^
            String.concatWith ", "
              (List.map (fn (name, ty) => name ^ " : " ^ go 0 ty) fields) ^
            " }"
        | Ir.TTuple types =>
            if List.length types < 2 then
              "tuple(" ^ String.concatWith ", " (List.map (go 0) types) ^ ")"
            else paren (need, 1, String.concatWith " * " (List.map (go 2) types))
        | Ir.TBorrow (inner, mutable) =>
            paren (need, 2, "&" ^ (if mutable then "mut " else "") ^ go 2 inner)
        | Ir.TArray inner => paren (need, 3, go 3 inner ^ " array")
        | Ir.TMap inner => paren (need, 3, go 3 inner ^ " map")
        | Ir.TData (name, [], _) => name
        | Ir.TData (name, [parameter], _) => paren (need, 3, go 3 parameter ^ " " ^ name)
        | Ir.TData (name, parameters, _) =>
            paren (need, 3,
              "(" ^ String.concatWith ", " (List.map (go 0) parameters) ^ ") " ^ name)
        | Ir.TFun ([param], result) => paren (need, 0, go 1 param ^ " -> " ^ go 0 result)
        | Ir.TFun (params, result) =>
            paren (need, 0,
              "(" ^ String.concatWith ", " (List.map (go 0) params) ^ ") -> " ^ go 0 result)
    in
      go 0 ty
    end

  fun pathText [] = "-"
    | pathText path = Ir.pathToString path

  fun modeToString mode =
    case mode of
      Move => "move "
    | Read => ""
    | BorrowOf => "&"
    | Part path => "move." ^ pathText path ^ " "

  fun toString (f : func) =
    let
      val marks = lastUses f
      fun arg lastSet (v, mode) =
        modeToString mode ^ valueName f v ^
        (if List.exists (fn x => x = v) lastSet then "!" else "")
      fun args lastSet xs = String.concatWith ", " (List.map (arg lastSet) xs)
      fun siteText site =
        case site of
          SOME span => "  # at " ^ Pos.toString (#start span)
        | NONE => ""
      fun instText (inst, lastSet) =
        case inst of
          Def {dst, ty, kind, args = xs, owns, site} =>
            "    " ^ valueName f dst ^ " : " ^ tyText ty ^ " = " ^
            (if owns then "" else "view ") ^ kind ^
            (if null xs then "" else " " ^ args lastSet xs) ^
            siteText site
        | Effect {kind, args = xs} => "    " ^ kind ^ " " ^ args lastSet xs
        | Drop {value, ty, skip, site} =>
            "    drop " ^ valueName f value ^
            (if List.exists (fn x => x = value) lastSet then "!" else "") ^
            " : " ^ tyText ty ^
            (if null skip then ""
             else " skip " ^ String.concatWith ", " (List.map pathText skip)) ^
            siteText site
        | Destructure {value, ty, site} =>
            "    destructure " ^ valueName f value ^
            (if List.exists (fn x => x = value) lastSet then "!" else "") ^
            " : " ^ tyText ty ^ siteText site
      fun termText (term, lastSet) =
        case term of
          Jump (target, xs) =>
            "    jump b" ^ Int.toString target ^ "(" ^ args lastSet xs ^ ")"
        | Branch (c, t, e) =>
            "    branch " ^ valueName f c ^ " b" ^ Int.toString t ^ " b" ^ Int.toString e
        | Return NONE => "    return"
        | Return (SOME x) => "    return " ^ arg lastSet x
        | Unreachable => "    unreachable"
      fun blockText (blk : block) =
        let
          val (instMarks, termMarks) = Vector.sub (marks, #id blk)
          val header =
            "  b" ^ Int.toString (#id blk) ^
            (if null (#params blk) then ""
             else "(" ^ String.concatWith ", "
                    (List.map (fn (v, ty) => valueName f v ^ " : " ^ tyText ty)
                       (#params blk)) ^ ")") ^ ":"
        in
          String.concatWith "\n"
            (header ::
             ListPair.map instText (#insts blk, instMarks) @
             [termText (#term blk, termMarks)])
        end
    in
      "fun " ^ #name f ^
      (if null (#borrowed f) then ""
       else " borrowed(" ^ String.concatWith ", " (#borrowed f) ^ ")") ^
      " {\n" ^
      String.concatWith "\n" (Vector.foldr (fn (blk, acc) => blockText blk :: acc) [] (#blocks f)) ^
      "\n}"
    end

  fun headers isLinearData (funcs : func list) =
    let
      val seen = ref ([] : (string * bool) list)
      fun visit ty =
        case ty of
          Ir.TData (name, parameters, copy) =>
            ( if List.exists (fn (n, _) => n = name) (!seen) then ()
              else seen := (name, copy) :: !seen
            ; List.app visit parameters )
        | Ir.TTuple types => List.app visit types
        | Ir.TRecord (fields, _) => List.app (visit o #2) fields
        | Ir.TBorrow (inner, _) => visit inner
        | Ir.TArray inner => visit inner
        | Ir.TMap inner => visit inner
        | Ir.TFun (params, result) => (List.app visit params; visit result)
        | _ => ()
      val _ = List.app (fn f => List.app (visit o #2) (#types f)) funcs
      fun line (name, copy) =
        (if copy then ["type " ^ name ^ " copy\n"] else []) @
        (if isLinearData name then ["type " ^ name ^ " linear\n"] else [])
      fun insert (x, []) = [x]
        | insert (x, y :: ys) =
            if String.< (#1 x, #1 y) then x :: y :: ys else y :: insert (x, ys)
      val names = List.foldl insert [] (!seen)
    in
      String.concat (List.concat (List.map line names))
    end

  fun programToString isLinearData (funcs : func list) =
    headers isLinearData funcs ^
    String.concat (List.map (fn f => toString f ^ "\n") funcs)
end
