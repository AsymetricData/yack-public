structure SsaParse =
struct
  type program =
    { linear : string list
    , funcs : Ssa.func list
    }

  exception Error of int * int * string

  datatype outcome =
      Parsed of program
    | Failed of string

  type cursor = { text : string, pos : int ref, line : int }

  fun fail (c : cursor) message = raise Error (#line c, !(#pos c) + 1, message)

  fun atEnd (c : cursor) = !(#pos c) >= String.size (#text c)

  fun peekAt (c : cursor) k =
    let val i = !(#pos c) + k
    in if i < String.size (#text c) then SOME (String.sub (#text c, i)) else NONE
    end

  fun peek c = peekAt c 0

  fun advance (c : cursor) n = #pos c := !(#pos c) + n

  fun skipSpace c =
    case peek c of
      SOME #" " => (advance c 1; skipSpace c)
    | SOME #"\t" => (advance c 1; skipSpace c)
    | _ => ()

  fun rest (c : cursor) = String.extract (#text c, !(#pos c), NONE)

  fun looking c word = String.isPrefix word (rest c)

  fun accept c word =
    (skipSpace c;
     if looking c word then (advance c (String.size word); true) else false)

  fun expect c word =
    if accept c word then () else fail c ("expected `" ^ word ^ "`")

  fun isNameChar ch = Char.isAlphaNum ch orelse ch = #"_" orelse ch = #"'"

  fun takeWhile c pred =
    let
      val start = !(#pos c)
      fun loop () =
        case peek c of
          SOME ch => if pred ch then (advance c 1; loop ()) else ()
        | NONE => ()
    in
      loop ();
      String.substring (#text c, start, !(#pos c) - start)
    end

  fun name c =
    (skipSpace c;
     case takeWhile c isNameChar of
       "" => fail c "expected a name"
     | text => text)

  fun int c =
    (skipSpace c;
     case Int.fromString (takeWhile c Char.isDigit) of
       SOME n => n
     | NONE => fail c "expected a number")

  fun endOfLine c =
    (skipSpace c; if atEnd c then () else fail c ("unexpected `" ^ rest c ^ "`"))

  fun site c =
    (skipSpace c;
     if looking c "#" then
       let
         val _ = advance c 1
         val _ = skipSpace c
         val _ = expect c "at "
         val line = int c
         val _ = expect c ":"
         val column = int c
         val position = {file = 0, line = line, column = column, offset = 0}
       in
         SOME {start = position, stop = position}
       end
     else NONE)

  fun value c =
    (skipSpace c;
     expect c "%";
     let
       val n = int c
       val label =
         if looking c "." then
           ( advance c 1
           ; case takeWhile c (fn ch =>
                    not (Char.isSpace ch) andalso ch <> #"," andalso ch <> #")"
                    andalso ch <> #"!" andalso ch <> #":") of
               "" => fail c "expected a value name after `.`"
             | text => SOME text )
         else NONE
       val _ = if looking c "!" then advance c 1 else ()
     in
       (n, label)
     end)

  fun path c =
    if looking c "-" then (advance c 1; [])
    else
      let
        fun more acc =
          if looking c "." andalso
             (case peekAt c 1 of SOME ch => Char.isDigit ch | NONE => false)
          then (advance c 1; more (int c :: acc))
          else List.rev acc
      in
        more [int c]
      end

  datatype group = One of Ir.ty | Group of Ir.ty list

  fun single c (One ty) = ty
    | single c (Group _) = fail c "a type list `(a, b)` must be followed by `->` or a datatype name"

  fun ty (c : cursor, copyOf : string -> bool) : Ir.ty =
    let
      fun list close =
        let
          fun loop acc =
            let val t = ty0 ()
            in
              if accept c "," then loop (t :: acc)
              else (expect c close; List.rev (t :: acc))
            end
        in
          if accept c close then [] else loop []
        end
      and ty0 () =
        let val left = ty1 ()
        in
          if accept c "->" then
            let val result = ty0 ()
            in
              case left of
                One t => Ir.TFun ([t], result)
              | Group ts => Ir.TFun (ts, result)
            end
          else single c left
        end
      and ty1 () =
        let
          val first = ty2 ()
          fun more acc =
            if accept c "*" then more (single c (ty2 ()) :: acc) else List.rev acc
        in
          (skipSpace c;
           if looking c "*" then One (Ir.TTuple (more [single c first])) else first)
        end
      and ty2 () =
        if accept c "&" then
          let
            val mutable = accept c "mut "
          in
            One (Ir.TBorrow (single c (ty2 ()), mutable))
          end
        else ty3 ()
      and ty3 () =
        let
          fun postfix base =
            (skipSpace c;
             case peek c of
               SOME ch =>
                 if Char.isAlpha ch andalso not (looking c "skip") then
                   let
                     val word = name c
                     val next =
                       case (word, base) of
                         ("array", _) => Ir.TArray (single c base)
                       | ("map", _) => Ir.TMap (single c base)
                       | (_, One t) => Ir.TData (word, [t], copyOf word)
                       | (_, Group ts) => Ir.TData (word, ts, copyOf word)
                   in
                     postfix (One next)
                   end
                 else base
             | NONE => base)
        in
          postfix (atom ())
        end
      and record discipline =
           let
             fun field () =
               let
                 val label = name c
                 val _ = expect c ":"
               in
                 (label, ty0 ())
               end
             fun loop acc =
               let val f = field ()
               in
                 if accept c "," then loop (f :: acc)
                 else (expect c "}"; List.rev (f :: acc))
               end
           in
             One (Ir.TRecord (if accept c "}" then [] else loop [], discipline))
           end
      and atom () =
        (skipSpace c;
         if accept c "(" then
           (case list ")" of
              [t] => One t
            | ts => Group ts)
         else if accept c "{" then record Ir.DAffine
         else if looking c "copy {" then (advance c 6; record Ir.DCopy)
         else if looking c "linear {" then (advance c 8; record Ir.DLinear)
         else if looking c "tuple(" then
           (advance c 6; One (Ir.TTuple (list ")")))
         else
           let
             val word = name c
             fun width prefix default =
               if word = prefix then SOME default
               else if String.isPrefix prefix word then
                 Int.fromString (String.extract (word, String.size prefix, NONE))
               else NONE
           in
             One
               (case word of
                  "bool" => Ir.Bool
                | "unit" => Ir.Unit
                | "token" => Ir.TToken
                | "resource" => Ir.TResource
                | "string" => Ir.TString
                | _ =>
                    (case (width "int" 32, width "float" 64) of
                       (SOME w, _) => Ir.Int w
                     | (_, SOME w) => Ir.Float w
                     | _ => Ir.TData (word, [], copyOf word)))
           end)
    in
      ty0 ()
    end

  fun operand c =
    (skipSpace c;
     let
       val mode =
         if looking c "move." then (advance c 5; let val p = path c in Ssa.Part p end)
         else if looking c "move " then (advance c 5; Ssa.Move)
         else if looking c "&" then (advance c 1; Ssa.BorrowOf)
         else Ssa.Read
     in
       (value c, mode)
     end)

  fun operands c =
    let
      fun loop acc =
        let val x = operand c
        in if accept c "," then loop (x :: acc) else List.rev (x :: acc)
        end
    in
      skipSpace c;
      if atEnd c then [] else loop []
    end

  fun operandAt (text, i) =
    let
      val size = String.size text
      fun ch k = if k < size then SOME (String.sub (text, k)) else NONE
      fun digit k = case ch k of SOME d => Char.isDigit d | NONE => false
      fun valueAt k = ch k = SOME #"%" andalso digit (k + 1)
      fun pathThenValue k =

        if ch k = SOME #"-" then ch (k + 1) = SOME #" " andalso valueAt (k + 2)
        else
          let
            fun scan j =
              case ch j of
                SOME d => if Char.isDigit d orelse d = #"." then scan (j + 1) else j
              | NONE => j
            val stop = scan k
          in
            stop > k andalso ch stop = SOME #" " andalso valueAt (stop + 1)
          end
      val tail = String.extract (text, i, NONE)
    in
      valueAt i
      orelse (ch i = SOME #"&" andalso valueAt (i + 1))
      orelse (String.isPrefix "move " tail andalso valueAt (i + 5))
      orelse (String.isPrefix "move." tail andalso pathThenValue (i + 5))
    end

  fun kind (c : cursor) =
    let
      val _ = skipSpace c
      val text = #text c
      val size = String.size text
      val start = !(#pos c)
      fun scan (i, inString) =
        if i >= size then i
        else
          let val ch = String.sub (text, i)
          in
            if inString then
              if ch = #"\\" then scan (i + 2, true)
              else scan (i + 1, ch <> #"\"")
            else if ch = #"\"" then scan (i + 1, true)
            else if (i = start orelse String.sub (text, i - 1) = #" ")
                    andalso operandAt (text, i) then i
            else scan (i + 1, false)
          end
      val stop = scan (start, false)
      val k = String.substring (text, start, stop - start)

      val k =
        if stop < size andalso String.size k > 0
           andalso String.sub (k, String.size k - 1) = #" "
        then String.substring (k, 0, String.size k - 1)
        else k
    in
      #pos c := stop;
      if CharVector.all Char.isSpace k then fail c "expected an instruction kind"
      else k
    end

  type funcState =
    { name : string
    , blocks : Ssa.block list ref
    , current : (int * (Ssa.value * Ir.ty) list * Ssa.inst list) option ref
    , names : (Ssa.value * string) list ref
    , types : (Ssa.value * Ir.ty) list ref
    , uses : (int * Ssa.value) list ref
    , borrowed : string list
    , line : int
    }

  fun note (st : funcState) ((v, label), line) =
    case label of
      NONE => ()
    | SOME text =>
        (case List.find (fn (x, _) => x = v) (!(#names st)) of
           SOME (_, old) =>
             if old = text then ()
             else raise Error (line, 1, "value %" ^ Int.toString v ^
                                  " named both `" ^ old ^ "` and `" ^ text ^ "`")
         | NONE => #names st := (v, text) :: !(#names st))

  fun define (st : funcState) (line, (v, label), t) =
    ( if List.exists (fn (x, _) => x = v) (!(#types st)) then
        raise Error (line, 1, "value %" ^ Int.toString v ^ " is defined twice")
      else #types st := (v, t) :: !(#types st)
    ; note st ((v, label), line)
    ; v )

  fun use (st : funcState) line ((v, label), mode) =
    ( note st ((v, label), line)
    ; #uses st := (line, v) :: !(#uses st)
    ; (v, mode) )

  fun finishFunc (st : funcState) : Ssa.func =
    let
      val _ =
        case !(#current st) of
          SOME (id, _, _) =>
            raise Error (#line st, 1, "block b" ^ Int.toString id ^ " has no terminator")
        | NONE => ()
      val blocks = !(#blocks st)
      val count = List.length blocks
      val table = Array.array (count, NONE : Ssa.block option)
      val _ =
        List.app
          (fn (blk : Ssa.block) =>
            if #id blk >= count then
              raise Error (#line st, 1, "block b" ^ Int.toString (#id blk) ^
                                        " out of range: blocks are numbered from b0")
            else
              case Array.sub (table, #id blk) of
                SOME _ => raise Error (#line st, 1, "block b" ^ Int.toString (#id blk) ^
                                                    " defined twice")
              | NONE => Array.update (table, #id blk, SOME blk))
          blocks
      val vector = Vector.tabulate (count, fn i => valOf (Array.sub (table, i)))
      val _ = if count = 0 then raise Error (#line st, 1, "function `" ^ #name st ^
                                                         "` has no block")
              else ()
      val defined = !(#types st)
      val _ =
        List.app
          (fn (line, v) =>
            if List.exists (fn (x, _) => x = v) defined then ()
            else raise Error (line, 1, "value %" ^ Int.toString v ^ " is never defined"))
          (!(#uses st))
      fun checkTarget id =
        if id < count then ()
        else raise Error (#line st, 1, "jump to missing block b" ^ Int.toString id)
      val _ =
        Vector.app
          (fn (blk : Ssa.block) =>
            case #term blk of
              Ssa.Jump (t, _) => checkTarget t
            | Ssa.Branch (_, t, e) => (checkTarget t; checkTarget e)
            | _ => ())
          vector
    in
      { name = #name st
      , params = #params (Vector.sub (vector, 0))
      , blocks = vector
      , names = !(#names st)
      , types = !(#types st)
      , borrowed = #borrowed st
      }
    end

  fun parse (text : string) : outcome =
    let
      val lines = String.fields (fn ch => ch = #"\n") text
      val linear = ref ([] : string list)
      val copies = ref ([] : string list)
      fun copyOf n = List.exists (fn x => x = n) (!copies)
      val funcs = ref ([] : Ssa.func list)
      val state = ref (NONE : funcState option)

      fun inFunc c =
        case !state of
          SOME st => st
        | NONE => fail c "outside a `fun ... {` block"

      fun push (st : funcState) inst c =
        case !(#current st) of
          SOME (id, params, insts) => #current st := SOME (id, params, inst :: insts)
        | NONE => fail c "instruction outside a block"

      fun terminate (st : funcState) term c =
        case !(#current st) of
          SOME (id, params, insts) =>
            ( #blocks st := { id = id, params = params, insts = List.rev insts
                            , term = term } :: !(#blocks st)
            ; #current st := NONE )
        | NONE => fail c "terminator outside a block"

      fun parseTy c = ty (c, copyOf)

      fun lineAt (number, raw) =
        let
          val c = { text = raw, pos = ref 0, line = number }
          val _ = skipSpace c
        in
          if atEnd c orelse looking c "#" then ()
          else if looking c "type " then
            ( advance c 5
            ; let
                val n = name c
                val _ = skipSpace c
              in
                if accept c "copy" then copies := n :: !copies
                else if accept c "linear" then linear := n :: !linear
                else fail c "expected `copy` or `linear`";
                endOfLine c
              end )
          else if looking c "fun " then
            ( case !state of
                SOME _ => fail c "nested `fun`"
              | NONE => ()
            ; advance c 4
            ; let
                val n = name c

                val borrowed =
                  if accept c "borrowed" then
                    let
                      val _ = expect c "("
                      fun loop acc =
                        let val acc = name c :: acc
                        in if accept c "," then loop acc
                           else (expect c ")"; List.rev acc)
                        end
                    in loop [] end
                  else []
              in
                expect c "{";
                endOfLine c;
                state := SOME { name = n, blocks = ref [], current = ref NONE
                              , names = ref [], types = ref [], uses = ref []
                              , borrowed = borrowed, line = number }
              end )
          else if looking c "}" then
            ( advance c 1
            ; endOfLine c
            ; funcs := finishFunc (inFunc c) :: !funcs
            ; state := NONE )
          else if looking c "b" andalso
                  (case peekAt c 1 of SOME ch => Char.isDigit ch | NONE => false) then
            let
              val st = inFunc c
              val _ = case !(#current st) of
                        SOME (id, _, _) =>
                          fail c ("block b" ^ Int.toString id ^ " has no terminator")
                      | NONE => ()
              val _ = advance c 1
              val id = int c
              val params =
                if accept c "(" then
                  let
                    fun param () =
                      let
                        val v = value c
                        val _ = expect c ":"
                        val t = parseTy c
                      in
                        (define st (number, v, t), t)
                      end
                    fun loop acc =
                      let val p = param ()
                      in
                        if accept c "," then loop (p :: acc)
                        else (expect c ")"; List.rev (p :: acc))
                      end
                  in
                    loop []
                  end
                else []
            in
              expect c ":";
              endOfLine c;
              #current st := SOME (id, params, [])
            end
          else
            let
              val st = inFunc c
              val use = use st number
            in
              if looking c "%" then
                let
                  val dst = value c
                  val _ = expect c ":"
                  val t = parseTy c
                  val _ = expect c "="
                  val _ = skipSpace c
                  val owns = not (accept c "view ")
                  val k = kind c
                  val args = List.map use (operands c)
                  val site = site c
                in
                  endOfLine c;
                  push st (Ssa.Def { dst = define st (number, dst, t), ty = t, kind = k
                                   , args = args, owns = owns, site = site }) c
                end
              else if looking c "drop " then
                let
                  val _ = advance c 5
                  val (v, _) = use (value c, Ssa.Read)
                  val _ = expect c ":"
                  val t = parseTy c
                  val skip =
                    if accept c "skip" then
                      let
                        fun loop acc =
                          (skipSpace c;
                           let val p = path c
                           in if accept c "," then loop (p :: acc) else List.rev (p :: acc)
                           end)
                      in
                        loop []
                      end
                    else []
                  val site = site c
                in
                  endOfLine c;
                  push st (Ssa.Drop { value = v, ty = t, skip = skip, site = site }) c
                end
              else if looking c "destructure " then
                let
                  val _ = advance c 12
                  val (v, _) = use (value c, Ssa.Read)
                  val _ = expect c ":"
                  val t = parseTy c
                  val site = site c
                in
                  endOfLine c;
                  push st (Ssa.Destructure { value = v, ty = t, site = site }) c
                end
              else if looking c "jump " then
                let
                  val _ = advance c 5
                  val _ = expect c "b"
                  val target = int c
                  val _ = expect c "("
                  val args =
                    if accept c ")" then []
                    else
                      let
                        fun loop acc =
                          let val x = use (operand c)
                          in
                            if accept c "," then loop (x :: acc)
                            else (expect c ")"; List.rev (x :: acc))
                          end
                      in
                        loop []
                      end
                in
                  endOfLine c;
                  terminate st (Ssa.Jump (target, args)) c
                end
              else if looking c "branch " then
                let
                  val _ = advance c 7
                  val (cond, _) = use (value c, Ssa.Read)
                  val _ = expect c "b"
                  val t = int c
                  val _ = expect c "b"
                  val e = int c
                in
                  endOfLine c;
                  terminate st (Ssa.Branch (cond, t, e)) c
                end
              else if looking c "return" then
                let
                  val _ = advance c 6
                  val _ = skipSpace c
                  val result = if atEnd c then NONE else SOME (use (operand c))
                in
                  endOfLine c;
                  terminate st (Ssa.Return result) c
                end
              else if looking c "unreachable" then
                ( advance c 11
                ; endOfLine c
                ; terminate st Ssa.Unreachable c )
              else
                let
                  val k = kind c
                  val args = List.map use (operands c)
                in
                  endOfLine c;
                  push st (Ssa.Effect { kind = k, args = args }) c
                end
            end
        end

      fun loop (_, []) = ()
        | loop (number, raw :: more) = (lineAt (number, raw); loop (number + 1, more))
    in
      ( loop (1, lines)
      ; case !state of
          SOME st => raise Error (#line st, 1, "function `" ^ #name st ^ "` is not closed")
        | NONE => ()
      ; Parsed { linear = List.rev (!linear), funcs = List.rev (!funcs) } )
      handle Error (line, col, message) =>
        Failed (Int.toString line ^ ":" ^ Int.toString col ^ ": " ^ message)
    end
end
