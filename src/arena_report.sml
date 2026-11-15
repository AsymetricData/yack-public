structure ArenaReport =
struct
  datatype fate =
      Freed
    | Stored of string
    | Outlives of string

  type entry =
    { line : int
    , column : int
    , what : string
    , fate : fate
    }

  type block =
    { span : Diag.span
    , function : string
    , reached : bool
    , entries : entry list
    , calls : string list
    , nested : int
    , result : Types.ty option
    }

  type input =
    { program : Ast.program
    , fileId : int
    , mode : Memory.mode
    , sites : Lower.traceSite list
    , lowered : string list
    , functions : string list
    , typeAt : Diag.span -> Types.ty option
    }

  fun startOf (span : Diag.span) =
    (Pos.line (#start span), Pos.column (#start span))

  fun stopOf (span : Diag.span) =
    (Pos.line (#stop span), Pos.column (#stop span))

  fun notAfter ((l1, c1), (l2, c2)) = l1 < l2 orelse (l1 = l2 andalso c1 <= c2)

  fun inside (span : Diag.span) point =
    notAfter (startOf span, point) andalso notAfter (point, stopOf span)

  fun sameSpan (a : Diag.span, b : Diag.span) =
    startOf a = startOf b andalso stopOf a = stopOf b

  val storeCalls =
    [ "array_push", "array_push_unchecked", "array_set", "array_set_unchecked"
    , "array_put", "array_put_arena", "array_kernel_write", "map_set" ]

  fun isStoreCall name = List.exists (fn candidate => candidate = name) storeCalls

  fun isStringKind kind =
    kind = "string concat" orelse kind = "sprintf" orelse
    String.isPrefix "string " kind orelse
    (String.isPrefix "bytes " kind andalso
     String.isSubstring "to_string" kind)

  fun describeKind kind =
    let
      fun after prefix =
        String.extract (kind, String.size prefix, NONE)
    in
      if kind = "string concat" then "`^` concatenation"
      else if String.isPrefix "constructor " kind then
        "`" ^ after "constructor " ^ "` constructor"
      else if String.isPrefix "string " kind then "`" ^ after "string " ^ "`"
      else if String.isPrefix "bytes " kind then "`" ^ after "bytes " ^ "`"
      else if String.isPrefix "map " kind then "`" ^ after "map " ^ "`"
      else "`" ^ kind ^ "`"
    end

  fun outlivesWhere mode (site : Lower.traceSite) =
    if String.isPrefix "map map_set" (#kind site) orelse
       String.isPrefix "map map_remove" (#kind site)
    then "grows the map's own storage"
    else
      case mode of
        Memory.Heap => "heap, freed when its owner is dropped"
      | Memory.Stack =>
          (case #siteRef site of
             SOME siteRef =>
               (case !siteRef of
                  Ir.Current => "function region, freed when the function returns"
                | Ir.Destination => "caller's region: the value escapes the function"
                | Ir.NativeStack => "native stack")
           | NONE => "root region, kept until the program ends")

  fun storedValueSpans program =
    let
      val spans = ref []
      fun spanOf expression =
        case expression of
          Ast.EInt (_, s) => s | Ast.EFloat (_, s) => s | Ast.EString (_, s) => s
        | Ast.EBool (_, s) => s | Ast.EUnit s => s | Ast.EVar (_, s) => s
        | Ast.ECall (_, _, s) => s | Ast.EBin (_, _, _, s) => s
        | Ast.ENeg (_, s) => s | Ast.EIf (_, _, _, s) => s
        | Ast.ELet (_, _, s) => s | Ast.EArena (_, _, s) => s
        | Ast.ERecord (_, s) => s | Ast.EField (_, _, s) => s
        | Ast.ETuple (_, s) => s | Ast.ETupleIndex (_, _, s) => s
        | Ast.ECase (_, _, s) => s | Ast.EBorrow (_, _, _, s) => s
        | Ast.EFn (_, _, _, s) => s
        | Ast.EExtern {span, ...} => span
      fun visit expression =
        case expression of
          Ast.ECall (name, arguments, _) =>
            ( if isStoreCall name then
                case arguments of
                  _ :: values =>
                    List.app (fn value => spans := (name, spanOf value) :: !spans) values
                | [] => ()
              else ()
            ; List.app visit arguments )
        | _ => children visit expression
      and children f expression =
        case expression of
          Ast.ECall (_, arguments, _) => List.app f arguments
        | Ast.EBin (_, l, r, _) => (f l; f r)
        | Ast.ENeg (x, _) => f x
        | Ast.EIf (c, t, e, _) => (f c; f t; f e)
        | Ast.ELet (bindings, body, _) => (List.app (bindingChildren f) bindings; f body)
        | Ast.EArena (bindings, body, _) => (List.app (bindingChildren f) bindings; f body)
        | Ast.ERecord (fields, _) => List.app (fn (_, x) => f x) fields
        | Ast.EField (_, x, _) => f x
        | Ast.ETuple (xs, _) => List.app f xs
        | Ast.ETupleIndex (_, x, _) => f x
        | Ast.ECase (s, branches, _) =>
            ( f s
            ; List.app (fn (_, g, b) => (Option.app f g; f b)) branches )
        | Ast.EBorrow (_, _, x, _) => f x
        | Ast.EFn (_, _, b, _) => f b
        | _ => ()
      and bindingChildren f binding =
        case binding of
          Ast.BVal {value, ...} => f value
        | Ast.BPat {value, ...} => f value
        | Ast.BFun {body, ...} => f body
        | Ast.BGuard {value, fallback, ...} => (f value; f fallback)
      val _ =
        List.app
          (fn declaration =>
            case declaration of
              Ast.DFun {body, ...} => visit body
            | Ast.DFunClause {body, ...} => visit body
            | _ => ())
          program
    in
      (!spans, children, bindingChildren)
    end

  fun sortSites sites =
    let
      fun insert (site : Lower.traceSite, []) = [site]
        | insert (site, (first : Lower.traceSite) :: rest) =
            if notAfter (startOf (#span site), startOf (#span first)) then
              site :: first :: rest
            else first :: insert (site, rest)
    in
      List.foldl insert [] sites
    end

  fun scan (input : input) =
    let
      val program = #program input
      val (stores, children, _) = storedValueSpans program
      fun storeAt point =
        Option.map #1
          (List.find (fn (_, span) => inside span point) stores)

      val sites =
        List.foldl
          (fn (site : Lower.traceSite, kept) =>
            if not (#allocates site) then kept
            else if Pos.file (#start (#span site)) <> #fileId input then kept
            else if List.exists
                      (fn (other : Lower.traceSite) =>
                        sameSpan (#span other, #span site) andalso
                        #kind other = #kind site)
                      kept
            then kept
            else site :: kept)
          [] (#sites input)
      val sites = List.rev sites
      val blocks = ref []
      fun arenaBlocks (function, expression) =
        case expression of
          Ast.EArena (_, _, span) =>
            ( blocks := (function, span) :: !blocks
            ; children (fn e => arenaBlocks (function, e)) expression )
        | _ => children (fn e => arenaBlocks (function, e)) expression
      val _ =
        List.app
          (fn declaration =>
            case declaration of
              Ast.DFun {name, body, ...} => arenaBlocks (name, body)
            | Ast.DFunClause {name, body, ...} => arenaBlocks (name, body)
            | _ => ())
          program
      val blocks = List.rev (!blocks)

      fun owner point =
        List.foldl
          (fn ((_, span), best) =>
            if not (inside span point) then best
            else
              case best of
                NONE => SOME span
              | SOME current =>
                  if inside current (startOf span) then SOME span else best)
          NONE blocks

      fun parentOf inner =
        List.foldl
          (fn ((_, span), best) =>
            if sameSpan (span, inner) orelse not (inside span (startOf inner)) then best
            else
              case best of
                NONE => SOME span
              | SOME current =>
                  if inside current (startOf span) then SOME span else best)
          NONE blocks
      fun ownedBy span point =
        case owner point of
          SOME found => sameSpan (found, span)
        | NONE => false
      fun entryOf (site : Lower.traceSite) : entry =
        let
          val point = startOf (#span site)
          val fate =
            if isStringKind (#kind site) then
              case storeAt point of
                SOME store => Stored store
              | NONE => Freed
            else Outlives (outlivesWhere (#mode input) site)
        in
          { line = #1 point, column = #2 point
          , what = describeKind (#kind site), fate = fate }
        end
      fun callsIn span =
        let
          val found = ref []
          fun visit expression =
            ( case expression of
                Ast.ECall (name, _, callSpan) =>
                  if ownedBy span (startOf callSpan) andalso
                     List.exists (fn f => f = name) (#functions input) andalso
                     not (List.exists (fn f => f = name) (!found))
                  then found := name :: !found
                  else ()
              | _ => ()
            ; children visit expression )
          val _ =
            List.app
              (fn declaration =>
                case declaration of
                  Ast.DFun {body, ...} => visit body
                | Ast.DFunClause {body, ...} => visit body
                | _ => ())
              program
        in
          List.rev (!found)
        end
    in
      List.map
        (fn (function, span) =>
          { span = span
          , function = function
          , reached = List.exists (fn f => f = function) (#lowered input)
          , entries =
              List.map entryOf
                (sortSites (List.filter
                   (fn (site : Lower.traceSite) => ownedBy span (startOf (#span site)))
                   sites))
          , calls = callsIn span
          , nested =
              List.length
                (List.filter
                   (fn (_, inner) =>
                     case parentOf inner of
                       SOME parent => sameSpan (parent, span)
                     | NONE => false)
                   blocks)
          , result = #typeAt input span
          })
        blocks
    end

  fun at (blocks : block list, line, column) =
    List.foldl
      (fn (block, best) =>
        if not (inside (#span block) (line, column)) then best
        else
          case best of
            NONE => SOME block
          | SOME (current : block) =>
              if inside (#span current) (startOf (#span block)) then SOME block
              else best)
      NONE blocks

  fun count predicate (block : block) =
    List.length (List.filter predicate (#entries block))

  fun isFreed (entry : entry) = case #fate entry of Freed => true | _ => false
  fun isStored (entry : entry) = case #fate entry of Stored _ => true | _ => false
  fun isOutliving (entry : entry) = case #fate entry of Outlives _ => true | _ => false

  fun plural (n, word) =
    Int.toString n ^ " " ^ word ^ (if n = 1 then "" else "s")

  fun headline (block : block) =
    if not (#reached block) then "not reached from main"
    else
      let
        val freed = count isFreed block
        val stored = count isStored block
        val outliving = count isOutliving block
        val parts =
          (if freed > 0 then [plural (freed, "string") ^ " freed at end"] else [])
          @ (if stored > 0 then [Int.toString stored ^ " stored to root"] else [])
          @ (if outliving > 0 then [Int.toString outliving ^ " outlive the block"] else [])
          @ (if null (#calls block) then []
             else [plural (List.length (#calls block), "call") ^ " allocate here"])
      in
        if null parts then "no allocation" else String.concatWith " · " parts
      end

  fun holdsMemory ty =
    case Types.concretize ty of
      Types.TInt _ => false
    | Types.TIntLit _ => false
    | Types.TFloat _ => false
    | Types.TFloatLit _ => false
    | Types.TBool => false
    | Types.TUnit => false
    | Types.TToken => false
    | Types.TTuple items => List.exists holdsMemory items
    | Types.TRecord fields => List.exists (fn (_, t) => holdsMemory t) fields
    | _ => true

  fun details (link : int * int -> string, strong : string -> string)
      (mode : Memory.mode) (block : block) =
    let
      val (startLine, _) = startOf (#span block)
      val (stopLine, _) = stopOf (#span block)
      val modeText = case mode of Memory.Heap => "heap" | Memory.Stack => "stack"
      fun item (entry : entry) =
        let
          val note =
            case #fate entry of
              Freed => ""
            | Stored store => " — value of `" ^ store ^ "`"
            | Outlives where_ => " — " ^ where_
        in
          "- " ^ link (#line entry, #column entry) ^ " " ^ #what entry ^ note
        end

      fun grouped entries =
        List.foldr
          (fn (entry : entry, (first : entry, n) :: rest) =>
                if #line entry = #line first andalso #column entry = #column first
                   andalso #what entry = #what first andalso #fate entry = #fate first
                then (entry, n + 1) :: rest
                else (entry, 1) :: (first, n) :: rest
            | (entry, []) => [(entry, 1)])
          [] entries
      fun itemCounted (entry, n) =
        item entry ^ (if n > 1 then " ×" ^ Int.toString n else "")
      fun section (title, predicate) =
        case List.filter predicate (#entries block) of
          [] => []
        | entries =>
            ["", strong (title ^ " (" ^ Int.toString (List.length entries) ^ ")")]
            @ List.map itemCounted (grouped entries)
      val header =
        "arena in `" ^ #function block ^ "`, lines " ^ Int.toString startLine ^
        "–" ^ Int.toString stopLine ^ ", " ^ modeText ^ " mode"
      val header = strong header
    in
      if not (#reached block) then
        [ header
        , ""
        , "`" ^ #function block ^ "` is not reached from `main` (or is generic and " ^
          "never instantiated), so no allocation of this block is compiled." ]
      else
        [header]
        @ section ("Freed at `end`", isFreed)
        @ section ("Stored to the root: the container outlives the arena", isStored)
        @ section ("Not arena memory: outlive the block", isOutliving)
        @ (if null (#entries block) then ["", "No allocation in the block itself."] else [])
        @ (if null (#calls block) then []
           else
             [ ""
             , "Strings allocated by " ^
               String.concatWith ", " (List.map (fn f => "`" ^ f ^ "`") (#calls block)) ^
               " while the block runs are freed at `end` too." ])
        @ (case #result block of
             NONE => []
           | SOME ty =>
               [ ""
               , "Value: `" ^ Types.toString ty ^ "`" ^
                 (if holdsMemory ty then
                    " — copied out of the arena before it is released."
                  else " — no memory to keep.") ])
        @ (if #nested block = 0 then []
           else ["", plural (#nested block, "nested arena block") ^ ", reported separately."])
    end

  fun render mode (blocks : block list) =
    if null blocks then
      "No arena block in this file.\n" ^
      "Wrap temporary string work in `arena val ... in ... end` to free it in one step."
    else
      String.concatWith "\n\n"
        (List.map
           (fn block =>
             String.concatWith "\n"
               (details
                  (fn (l, c) => Int.toString l ^ ":" ^ Int.toString c, fn text => text)
                  mode block))
           blocks)
end
