structure SsaOpt =
struct
  fun isConcat (Ir.Bin ("^", _, _, _, _)) = true
    | isConcat _ = false

  fun patternBinds name pattern =
    case pattern of
      Ir.PVar (n, _) => n = name
    | Ir.PString (_, SOME rest, _) => patternBinds name rest
    | Ir.PBytes (_, SOME rest, _) => patternBinds name rest
    | Ir.PTuple (ps, _) => List.exists (patternBinds name) ps
    | Ir.PRecord (fields, _) => List.exists (fn (_, p) => patternBinds name p) fields
    | Ir.PCtor (_, _, SOME p, _) => patternBinds name p
    | _ => false

  fun uses name expression =
    let
      val count = ref 0
      fun walk e =
        case e of
          Ir.Var (n, _, _) => if n = name then count := !count + 1 else ()
        | Ir.Bin (_, l, r, _, _) => (walk l; walk r)
        | Ir.Neg (x, _) => walk x
        | Ir.Call (_, xs, _, _) => List.app walk xs
        | Ir.If (c, t, f, _) => (walk c; walk t; walk f)
        | Ir.Let (bindings, body, drops) =>
            let

              fun go [] =
                    ( walk body
                    ; List.app (fn {name = n, ...} => if n = name then count := !count + 1 else ()) drops )
                | go ((n, _, v) :: rest) = (walk v; if n = name then () else go rest)
            in
              go bindings
            end
        | Ir.Tuple (xs, _) => List.app walk xs
        | Ir.Record (fields, _) => List.app (fn (_, x) => walk x) fields
        | Ir.Field (x, _, _) => walk x
        | Ir.TupleIndex (x, _, _) => walk x
        | Ir.Ctor (_, _, x, _, _) => Option.app walk x
        | Ir.Match (s, branches, _) =>
            ( walk s
            ; List.app
                (fn (p, g, body, drops) =>
                  if patternBinds name p then ()
                  else
                    ( Option.app walk g
                    ; walk body
                    ; List.app (fn {name = n, ...} => if n = name then count := !count + 1 else ()) drops ))
                branches )
        | Ir.PrintString (x, _) => walk x
        | Ir.StringBuiltin (_, xs, _, _) => List.app (fn (x, _) => walk x) xs
        | Ir.BytesBuiltin (_, xs, _, _) => List.app (fn (x, _) => walk x) xs
        | Ir.MapBuiltin (_, xs, _, _) => List.app (fn (x, _) => walk x) xs
        | Ir.SysBuiltin (_, xs, _) => List.app walk xs
        | Ir.ArrayBuiltin (_, xs, _, _) => List.app walk xs
        | Ir.Copy (x, _) => walk x
        | Ir.Borrow (_, x, _) => walk x
        | Ir.Apply (f, xs, _) => (walk f; List.app walk xs)
        | _ => ()
    in
      walk expression;
      !count
    end

  fun replaceHead (name, replacement) expression =
    case expression of
      Ir.Bin ("^", left, right, ty, site) =>
        (case left of
           Ir.Var (n, _, _) =>
             if n = name then SOME (Ir.Bin ("^", replacement, right, ty, site))
             else NONE
         | _ =>
             Option.map (fn left' => Ir.Bin ("^", left', right, ty, site))
               (replaceHead (name, replacement) left))
    | _ => NONE

  fun fuseInto (name, value) body =
    case body of
      Ir.Bin ("^", _, _, _, _) => replaceHead (name, value) body
    | Ir.Let ((n, ty, first) :: rest, inner, drops) =>
        Option.map (fn first' => Ir.Let ((n, ty, first') :: rest, inner, drops))
          (replaceHead (name, value) first)
    | _ => NONE

  val fused = ref 0

  fun rewrite expression =
    case expression of
      Ir.Let ([(name, ty, value)], body, drops) =>
        let
          val value = rewrite value
          val body = rewrite body
          val dropped = List.exists (fn {name = n, ...} => n = name) drops
        in
          if isConcat value andalso not dropped then
            case uses name body of
              1 =>
                (case fuseInto (name, value) body of
                   SOME body' =>
                     ( fused := !fused + 1
                     ; if null drops then body'
                       else Ir.Let ([], body', drops) )
                 | NONE => Ir.Let ([(name, ty, value)], body, drops))
            | _ => Ir.Let ([(name, ty, value)], body, drops)
          else Ir.Let ([(name, ty, value)], body, drops)
        end
    | Ir.Let (bindings, body, drops) =>
        Ir.Let (List.map (fn (n, t, v) => (n, t, rewrite v)) bindings,
                rewrite body, drops)
    | Ir.Bin (operator, l, r, ty, site) => Ir.Bin (operator, rewrite l, rewrite r, ty, site)
    | Ir.Neg (x, ty) => Ir.Neg (rewrite x, ty)
    | Ir.Call (f, xs, ty, escaping) => Ir.Call (f, List.map rewrite xs, ty, escaping)
    | Ir.If (c, t, f, ty) => Ir.If (rewrite c, rewrite t, rewrite f, ty)
    | Ir.Tuple (xs, ty) => Ir.Tuple (List.map rewrite xs, ty)
    | Ir.Record (fields, ty) => Ir.Record (List.map (fn (n, x) => (n, rewrite x)) fields, ty)
    | Ir.Field (x, i, ty) => Ir.Field (rewrite x, i, ty)
    | Ir.TupleIndex (x, i, ty) => Ir.TupleIndex (rewrite x, i, ty)
    | Ir.Ctor (d, c, x, ty, site) => Ir.Ctor (d, c, Option.map rewrite x, ty, site)
    | Ir.Match (s, branches, ty) =>
        Ir.Match (rewrite s,
                  List.map (fn (p, g, body, drops) =>
                    (p, Option.map rewrite g, rewrite body, drops)) branches,
                  ty)
    | Ir.PrintString (x, consume) => Ir.PrintString (rewrite x, consume)
    | Ir.StringBuiltin (n, xs, ty, site) =>
        Ir.StringBuiltin (n, List.map (fn (x, c) => (rewrite x, c)) xs, ty, site)
    | Ir.BytesBuiltin (n, xs, ty, site) =>
        Ir.BytesBuiltin (n, List.map (fn (x, c) => (rewrite x, c)) xs, ty, site)
    | Ir.MapBuiltin (n, xs, ty, site) =>
        Ir.MapBuiltin (n, List.map (fn (x, c) => (rewrite x, c)) xs, ty, site)
    | Ir.SysBuiltin (n, xs, ty) => Ir.SysBuiltin (n, List.map rewrite xs, ty)
    | Ir.ArrayBuiltin (n, xs, ty, site) => Ir.ArrayBuiltin (n, List.map rewrite xs, ty, site)
    | Ir.Copy (x, ty) => Ir.Copy (rewrite x, ty)
    | Ir.Borrow (m, x, ty) => Ir.Borrow (m, rewrite x, ty)
    | Ir.Apply (f, xs, ty) => Ir.Apply (rewrite f, List.map rewrite xs, ty)
    | other => other

  fun optimize (program : Ir.program) : Ir.program =
    { datatypes = #datatypes program
    , functions =
        List.map
          (fn (f : Ir.funDef) =>
            { sourceName = #sourceName f
            , name = #name f
            , params = #params f
            , result = #result f
            , recursive = #recursive f
            , body = rewrite (#body f)
            , drops = #drops f
            , borrowed = #borrowed f
            })
          (#functions program)
    }
end
