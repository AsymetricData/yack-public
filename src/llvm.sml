structure Llvm =
struct

  val debugAlloc = ref false

  fun setDebugAlloc value = debugAlloc := value

  open LlvmTypes

  fun intText value =
    if value < Int64.fromInt 0 then "-" ^ Int64.toString (Int64.~ value)
    else Int64.toString value

  fun generate (mode : Memory.mode, reserve : Int64.int, program : Ir.program) =
    let
      val functions = #functions program
      val datatypes = #datatypes program
      val externs = #externs program
      val lines = ref []
      val counter = ref 0
      val stringLiterals = ref []
      val stringCounter = ref 0

      val stackMode = (mode = Memory.Stack)
      val stackPlan = StackPlan.analyze functions
      val currentRegionArg = ref "%region"
      val currentDestArg = ref "%dest"
      val storeRoot = ref false
      val arenaDirty = ref ([] : (value * Ir.ty) list ref list)
      fun registerDirty (dirtyValue, dirtyTy) =
        case !arenaDirty of
          [] => false
        | dirty :: _ =>
            let
              val text = #text dirtyValue
              val present =
                List.exists
                  (fn (value, _) => #text value = text)
                  (!dirty)
            in
              if present then true
              else
                ( dirty := (dirtyValue, dirtyTy) :: !dirty
                ; true
                )
            end
      fun enterArena () = arenaDirty := ref [] :: !arenaDirty

      val selfLoop = ref (NONE : (value list -> unit) option)
      val currentFunctionName = ref ""
      val currentIsRecursive = ref false
      val currentNeedsRegion = ref true
      val inPromotion = ref false

      val inEntryBlock = ref false
      val hasDynamicAlloca = ref false

      val addressTaken = ref (StringSet.create 16)

      fun hoistAlloca line =
        let
          fun insertAfterEntry lines =
            case lines of
              [] => [line]
            | current :: rest =>
                if current = "entry:" then
                  line :: current :: rest
                else
                  current :: insertAfterEntry rest
        in
          lines := insertAfterEntry (!lines)
        end

      fun emit line =
        ( lines := line :: !lines
        ; if line = "entry:" then inEntryBlock := true
          else if String.isSuffix ":" line then inEntryBlock := false
          else if String.isPrefix "br " line orelse
                  String.isPrefix "ret " line orelse
                  String.isPrefix "switch " line orelse
                  line = "unreachable" then
            inEntryBlock := false
          else if String.isSubstring " = alloca" line andalso
                  not (!inEntryBlock) then
            hasDynamicAlloca := true
          else ()
        )

      fun fresh prefix =
        let val index = !counter
            val _ = counter := index + 1
        in "%" ^ prefix ^ Int.toString index end

      fun freshLabel prefix =
        let val index = !counter
            val _ = counter := index + 1
        in prefix ^ Int.toString index end

      val dropEnv =
        { emit = emit
        , fresh = fresh
        , freshLabel = freshLabel
        }

      val opEnv = { emit = emit, fresh = fresh }

      fun needsDrop ty = LlvmDrop.needsDrop ty

      fun dropValue (value, ty) = LlvmDrop.dropValue (dropEnv, value, ty)

      fun dropValueSkipping (value, ty, skip) =
        LlvmDrop.dropValueSkipping (dropEnv, value, ty, skip)

      fun freeIfHeap value = LlvmDrop.freeIfHeap (dropEnv, value)

      fun coerce (value, target) = LlvmOp.coerce (opEnv, value, target)

      fun lowerBin (operator, left, right, resultTy) =
        LlvmOp.lowerBin (opEnv, operator, left, right, resultTy)

      fun allocStringForSite site size =
        let
          val memory = fresh "string.mem"
          val _ =
            if not stackMode then
              emit (memory ^ " = call ptr @yack_string_alloc(i64 " ^ size ^ ")")
            else
              let
                val region =
                  case site of
                    Ir.Current => !currentRegionArg
                  | Ir.Destination => !currentDestArg
                  | Ir.NativeStack => !currentDestArg
              in
                emit (memory ^ " = call ptr @yack_string_alloc_in(ptr " ^
                      region ^ ", i64 " ^ size ^ ")")
              end
        in
          memory
        end

      fun findData name = LlvmData.findData (datatypes, name)

      fun dataStructTy definition = LlvmData.dataStructTy definition

      fun constructorInfo definition constructor =
        LlvmData.constructorInfo definition constructor

      fun hexByte value =
        let val digits = "0123456789ABCDEF" in
          String.implode
            [ String.sub (digits, value div 16)
            , String.sub (digits, value mod 16)
            ]
        end

      fun llvmEscape text =
        String.concat
          (List.map
             (fn ch =>
               let val code = Char.ord ch in
                 if code >= 32 andalso code <= 126 andalso
                    ch <> #"\"" andalso ch <> #"\\"
                 then Char.toString ch
                 else "\\" ^ hexByte code
               end)
             (String.explode text))

      fun constantString text =
        let
          val escaped = llvmEscape text

          val length = Int.toString (String.size text + 1)
        in
          "[" ^ length ^ " x i8] c\"" ^ escaped ^ "\\00\""
        end

      fun registerString text =
        case List.find (fn (_, candidate) => candidate = text) (!stringLiterals) of
          SOME (name, _) => name
        | NONE =>
            let
              val index = !stringCounter
              val _ = stringCounter := index + 1
              val name = ".str" ^ Int.toString index
              val _ = stringLiterals := (name, text) :: !stringLiterals
            in
              name
            end

      fun findLocal locals name =
        case List.find (fn entry => #name entry = name) locals of
          SOME entry => SOME (#value entry)
        | NONE => NONE

      fun findLocalAddress locals name =
        case List.find (fn entry => #name entry = name) locals of
          SOME entry =>
            (case #address entry of
               SOME slot => SOME (slot, #ty (#value entry))
             | NONE => NONE)
        | NONE => NONE

      fun allocStack ty =
        let
          val slot = fresh "slot"
          val _ = hoistAlloca (slot ^ " = alloca " ^ storageTy ty)
        in
          slot
        end

      val arenaMarks = ref ([] : (string * string) list)
      fun arenaMark () =
        let
          val slot = allocStack (Ir.Int 64)
          val savedSlot = allocStack (Ir.TCPtr)
          val mark = fresh "arena.mark"
        in
          emit (mark ^ " = call i64 @yack_arena_mark(ptr %region, ptr " ^
                savedSlot ^ ")");
          emit ("store i64 " ^ mark ^ ", ptr " ^ slot);
          arenaMarks := (slot, savedSlot) :: !arenaMarks
        end
      fun arenaRelease () =
        case !arenaMarks of
          [] => ()
        | (slot, savedSlot) :: rest =>
            ( arenaMarks := rest
            ; let val mark = fresh "arena.mark"
              in
                emit (mark ^ " = load i64, ptr " ^ slot);
                emit ("call void @yack_arena_release(ptr %region, i64 " ^
                      mark ^ ", ptr " ^ savedSlot ^ ")")
              end
            )

      fun irType expression =
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
        | Ir.Let (_, body, _) => irType body
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

      fun safeLocalScope expression =
        case expression of
          Ir.IntLit _ => true
        | Ir.FloatLit _ => true
        | Ir.StringLit _ => true
        | Ir.BoolLit _ => true
        | Ir.UnitLit => true
        | Ir.Var _ => true
        | Ir.Closure _ => true
        | Ir.ClosureRef _ => true
        | Ir.Bin (_, left, right, _, _) =>
            safeLocalScope left andalso safeLocalScope right
        | Ir.Neg (value, _) => safeLocalScope value
        | Ir.If (condition, thenBranch, elseBranch, _) =>
            safeLocalScope condition andalso
            safeLocalScope thenBranch andalso safeLocalScope elseBranch
        | Ir.Let (bindings, body, _) =>
            List.all (fn (_, _, value) => safeLocalScope value) bindings
            andalso safeLocalScope body
        | Ir.Tuple (values, _) => List.all safeLocalScope values
        | Ir.Record (fields, _) =>
            List.all (fn (_, value) => safeLocalScope value) fields
        | Ir.Field (base, _, _) => safeLocalScope base
        | Ir.TupleIndex (base, _, _) => safeLocalScope base
        | Ir.RawField (base, _, _) => safeLocalScope base
        | Ir.Ctor (_, _, argument, _, _) =>
            (case argument of
               SOME value => safeLocalScope value
             | NONE => true)
        | Ir.Match (scrutinee, branches, _) =>
            safeLocalScope scrutinee andalso
            List.all
              (fn (_, guard, body, _) =>
                 (case guard of
                    SOME condition => safeLocalScope condition
                  | NONE => true) andalso safeLocalScope body)
              branches
        | Ir.StringBuiltin (_, arguments, _, _) =>
            List.all (fn (value, _) => safeLocalScope value) arguments
        | Ir.BytesBuiltin (_, arguments, _, _) =>
            List.all (fn (value, _) => safeLocalScope value) arguments
        | Ir.Copy (value, _) => safeLocalScope value
        | Ir.Borrow (_, value, _) => safeLocalScope value
        | _ => false

      fun resultEscapes ty =
        case ty of
          Ir.Int _ => false
        | Ir.Float _ => false
        | Ir.Bool => false
        | Ir.Unit => false
        | Ir.TToken => false
        | Ir.TResource => false
        | _ => true

      fun bindLocal locals name value =
        let
          val address =
            if not (isUnit (#ty value)) andalso
               StringSet.member (!addressTaken, name)
            then
              let
                val slot = allocStack (#ty value)
                val _ =
                  emit ("store " ^ storageTy (#ty value) ^ " " ^
                        #text value ^ ", ptr " ^ slot)
              in
                SOME slot
              end
            else NONE
        in
          { name = name, value = value, address = address } :: locals
        end

      fun lowerTuple (values : value list, ty : Ir.ty) =
        let
          val structTy = llvmTy ty
          fun insert (index, value, previous) =
            let val reg = fresh "agg" in
              emit (reg ^ " = insertvalue " ^ structTy ^ " " ^ previous ^
                    ", " ^ typed (coerce (value, elementType ty index)) ^
                    ", " ^ Int.toString index);
              reg
            end
        in
          case values of
            [] => { text = "undef", ty = ty, const = false }
          | _ =>
              let
                fun loop (index, remaining, previous) =
                  case remaining of
                    [] => previous
                  | value :: rest =>
                      loop (index + 1, rest, insert (index, value, previous))
              in
                { text = loop (0, values, "undef"), ty = ty, const = false }
              end
        end

      and elementType ty index =
        let
          fun nthOr fallback values =
            List.nth (values, index) handle _ => fallback
        in
          case ty of
            Ir.TTuple types => nthOr Ir.Unit types
          | Ir.TRecord (fields, _) => #2 (nthOr ("?", Ir.Unit) fields)
          | _ => Ir.Unit
        end

      fun lowerRecord (fields : (string * value) list, ty : Ir.ty) =
        let
          val structTy = llvmTy ty
          fun insert (index, (_, value), previous) =
            let val reg = fresh "agg" in
              emit (reg ^ " = insertvalue " ^ structTy ^ " " ^ previous ^
                    ", " ^ typed (coerce (value, elementType ty index)) ^
                    ", " ^ Int.toString index);
              reg
            end
          val values = List.map (fn (name, value) => (name, value)) fields
        in
          case values of
            [] => { text = "undef", ty = ty, const = false }
          | _ =>
              let
                fun loop (index, remaining, previous) =
                  case remaining of
                    [] => previous
                  | pair :: rest =>
                      loop (index + 1, rest, insert (index, pair, previous))
              in
                { text = loop (0, values, "undef"), ty = ty, const = false }
              end
        end

      fun copyValue (value : value, ty : Ir.ty) =
        case ty of
          Ir.TData (dataName, _, _) =>
            let val reg = fresh "copy" in
              emit (reg ^ " = call ptr @yack_copy_" ^ dataName ^
                    "(ptr " ^ #text value ^ ")");
              { text = reg, ty = ty, const = false }
            end
        | Ir.TTuple types =>
            let
              fun extract (index, ty) =
                let val reg = fresh "copyfield" in
                  emit (reg ^ " = extractvalue " ^ llvmTy (#ty value) ^ " " ^
                        #text value ^ ", " ^ Int.toString index);
                  copyValue ({ text = reg, ty = ty, const = false }, ty)
                end
              fun loop (index, remaining, acc) =
                case remaining of
                  [] => List.rev acc
                | fieldTy :: rest =>
                    loop (index + 1, rest, extract (index, fieldTy) :: acc)
            in
              lowerTuple (loop (0, types, []), ty)
            end
        | Ir.TString =>
            let val reg = fresh "copy" in
              emit (reg ^ " = call { ptr, i64, i1 } @yack_copy_string(" ^
                    typed value ^ ")");
              { text = reg, ty = ty, const = false }
            end
        | Ir.TRecord (fields, _) =>
            let
              fun extract (index, fieldTy) =
                let val reg = fresh "copyfield" in
                  emit (reg ^ " = extractvalue " ^ llvmTy (#ty value) ^ " " ^
                        #text value ^ ", " ^ Int.toString index);
                  copyValue ({ text = reg, ty = fieldTy, const = false }, fieldTy)
                end
              fun loop (index, remaining, acc) =
                case remaining of
                  [] => List.rev acc
                | (name, fieldTy) :: rest =>
                    loop (index + 1, rest,
                          (name, extract (index, fieldTy)) :: acc)
            in
              lowerRecord (loop (0, fields, []), ty)
            end
        | _ => value

      fun mayContainString ty =
        case ty of
          Ir.TString => true
        | Ir.TTuple types => List.exists mayContainString types
        | Ir.TRecord (fields, _) =>
            List.exists (fn (_, fieldTy) => mayContainString fieldTy) fields
        | Ir.TArray elementTy => mayContainString elementTy
        | Ir.TMap _ => true
        | Ir.TData _ => true
        | _ => false

      fun rootString value =
        let
          val reg = fresh "str.root"

          val function =
            if stackMode andalso !inPromotion then "yack_string_promote"
            else "yack_string_root"
        in
          emit (reg ^ " = call { ptr, i64, i1 } @" ^ function ^ "(" ^
                typed value ^ ")");
          { text = reg, ty = Ir.TString, const = false }
        end

      fun enterStoreRoot () =
        let
          val old = fresh "store.root.old"
          val next = fresh "store.root.next"
        in
          ( emit (old ^ " = load i64, ptr @yack_store_root, align 8")
          ; emit (next ^ " = add i64 " ^ old ^ ", 1")
          ; emit ("store i64 " ^ next ^ ", ptr @yack_store_root, align 8")
          )
        end

      fun leaveStoreRoot () =
        let
          val old = fresh "store.root.old"
          val next = fresh "store.root.next"
        in
          ( emit (old ^ " = load i64, ptr @yack_store_root, align 8")
          ; emit (next ^ " = sub i64 " ^ old ^ ", 1")
          ; emit ("store i64 " ^ next ^ ", ptr @yack_store_root, align 8")
          )
        end

      fun rootValue (value, ty) =
        let
          val copyContainer =
            stackMode andalso
            (case ty of
               Ir.TArray _ => true
             | Ir.TMap _ => true
             | _ => false)
        in
          if not (mayContainString ty) andalso not copyContainer then value
          else
            case ty of
              Ir.TString => rootString value
            | Ir.TData (dataName, _, _) =>
                let val reg = fresh "root.data" in
                  emit (reg ^ " = call ptr @yack_root_" ^ dataName ^
                        "(ptr " ^ #text value ^ ")");
                  { text = reg, ty = ty, const = false }
                end
            | Ir.TArray elementTy =>
                if stackMode then rootArrayStack (value, elementTy)
                else rootArrayInPlace (value, elementTy)
            | Ir.TMap valueTy =>
                if stackMode then rootMapStack (value, valueTy)
                else value
            | Ir.TTuple types =>
                let
                  val structTy = llvmTy ty
                  fun field (index, fieldTy) =
                    let val reg = fresh "root.field"
                        val _ =
                          emit (reg ^ " = extractvalue " ^ structTy ^ " " ^
                                #text value ^ ", " ^ Int.toString index)
                    in
                      rootValue
                        ({ text = reg, ty = fieldTy, const = false }, fieldTy)
                    end
                in
                  lowerTuple
                    ( ListPair.map field
                        (List.tabulate (List.length types, fn i => i), types)
                    , ty
                    )
                end
            | Ir.TRecord (fields, _) =>
                let
                  val structTy = llvmTy ty
                  fun field (index, (name, fieldTy)) =
                    let val reg = fresh "root.field"
                        val _ =
                          emit (reg ^ " = extractvalue " ^ structTy ^ " " ^
                                #text value ^ ", " ^ Int.toString index)
                    in
                      ( name
                      , rootValue
                          ({ text = reg, ty = fieldTy, const = false }, fieldTy)
                      )
                    end
                in
                  lowerRecord
                    ( ListPair.map field
                        ( List.tabulate (List.length fields, fn i => i)
                        , fields
                        )
                    , ty
                    )
                end
            | _ => value
        end

      and rootArrayInPlace (value, elementTy) =
        let
          val header = #text value
          val idxSlot = allocStack (Ir.Int 64)
          val isNull = fresh "root.arr.null"
          val loopInit = freshLabel "root.arr.init"
          val loopCond = freshLabel "root.arr.cond"
          val loopBody = freshLabel "root.arr.body"
          val loopDone = freshLabel "root.arr.done"
          val dataPtr = fresh "root.arr.datap"
          val lenPtr = fresh "root.arr.lenp"
          val data = fresh "root.arr.data"
          val len = fresh "root.arr.len"
          val idx = fresh "root.arr.idx"
          val more = fresh "root.arr.more"
          val elemPtr = fresh "root.arr.elemp"
          val elem = fresh "root.arr.elem"
          val nextIdx = fresh "root.arr.next"
          val elemStorage = storageTy elementTy
          val _ = emit (isNull ^ " = icmp eq ptr " ^ header ^ ", null")
          val _ =
            emit ("br i1 " ^ isNull ^ ", label %" ^ loopDone ^
                  ", label %" ^ loopInit)
          val _ = emit (loopInit ^ ":")
          val _ =
            emit (dataPtr ^
                  " = getelementptr { ptr, i64, i64, ptr }, ptr " ^
                  header ^ ", i32 0, i32 0")
          val _ =
            emit (lenPtr ^
                  " = getelementptr { ptr, i64, i64, ptr }, ptr " ^
                  header ^ ", i32 0, i32 1")
          val _ = emit (data ^ " = load ptr, ptr " ^ dataPtr)
          val _ = emit (len ^ " = load i64, ptr " ^ lenPtr)
          val _ = emit ("store i64 0, ptr " ^ idxSlot)
          val _ = emit ("br label %" ^ loopCond)
          val _ = emit (loopCond ^ ":")
          val _ = emit (idx ^ " = load i64, ptr " ^ idxSlot)
          val _ = emit (more ^ " = icmp slt i64 " ^ idx ^ ", " ^ len)
          val _ =
            emit ("br i1 " ^ more ^ ", label %" ^ loopBody ^
                  ", label %" ^ loopDone)
          val _ = emit (loopBody ^ ":")
          val _ =
            emit (elemPtr ^ " = getelementptr " ^ elemStorage ^
                  ", ptr " ^ data ^ ", i64 " ^ idx)
          val _ =
            emit (elem ^ " = load " ^ elemStorage ^ ", ptr " ^ elemPtr)
          val rooted =
            rootValue
              ({ text = elem, ty = elementTy, const = false }, elementTy)
          val _ =
            emit ("store " ^ elemStorage ^ " " ^
                  #text (coerce (rooted, elementTy)) ^ ", ptr " ^
                  elemPtr)
          val _ = emit (nextIdx ^ " = add i64 " ^ idx ^ ", 1")
          val _ = emit ("store i64 " ^ nextIdx ^ ", ptr " ^ idxSlot)
          val _ = emit ("br label %" ^ loopCond)
          val _ = emit (loopDone ^ ":")
        in
          value
        end

      and rootArrayStack (value, elementTy) =
        let
          val header = #text value
          val outSlot = allocStack (Ir.TArray elementTy)
          val isNull = fresh "root.arr.null"
          val ownerPtr = fresh "root.arr.ownerp"
          val owner = fresh "root.arr.owner"
          val inScratch = fresh "root.arr.inscratch"
          val notScratch = fresh "root.arr.notscratch"
          val ownerIsRoot = fresh "root.arr.ownerisroot"
          val isRoot = fresh "root.arr.isroot"
          val checkRoot = freshLabel "root.arr.checkroot"
          val rootLabel = freshLabel "root.arr.root"
          val initLabel = freshLabel "root.arr.init"
          val copyLabel = freshLabel "root.arr.copy"
          val loopCond = freshLabel "root.arr.cond"
          val loopBody = freshLabel "root.arr.body"
          val loopDone = freshLabel "root.arr.loopdone"
          val doneLabel = freshLabel "root.arr.done"
          val dataPtr = fresh "root.arr.datap"
          val lenPtr = fresh "root.arr.lenp"
          val data = fresh "root.arr.data"
          val len = fresh "root.arr.len"
          val elemSize = fresh "root.arr.esize"
          val bytes = fresh "root.arr.bytes"
          val newData = fresh "root.arr.newdata"
          val newHeader = fresh "root.arr.newheader"
          val idxSlot = allocStack (Ir.Int 64)
          val idx = fresh "root.arr.idx"
          val more = fresh "root.arr.more"
          val elemPtr = fresh "root.arr.elemp"
          val elem = fresh "root.arr.elem"
          val newElemPtr = fresh "root.arr.newelemp"
          val nextIdx = fresh "root.arr.next"
          val elemStorage = storageTy elementTy
          val headerTy = "{ ptr, i64, i64, ptr }"
          val _ = emit ("store ptr " ^ header ^ ", ptr " ^ outSlot)
          val _ = emit (isNull ^ " = icmp eq ptr " ^ header ^ ", null")
          val _ =
            emit (ownerPtr ^ " = getelementptr " ^ headerTy ^ ", ptr " ^
                  header ^ ", i32 0, i32 3")
          val _ = emit (owner ^ " = load ptr, ptr " ^ ownerPtr)
          val _ =
            emit (inScratch ^ " = call i1 @yack_scratch_owns(ptr " ^
                  header ^ ")")
          val _ = emit (notScratch ^ " = xor i1 " ^ inScratch ^ ", true")
          val _ =
            emit (ownerIsRoot ^ " = icmp eq ptr " ^ owner ^
                  ", @yack_root_region")
          val _ =
            emit (isRoot ^ " = and i1 " ^ ownerIsRoot ^ ", " ^ notScratch)
          val _ =
            emit ("br i1 " ^ isNull ^ ", label %" ^ doneLabel ^
                  ", label %" ^ checkRoot)
          val _ = emit (checkRoot ^ ":")
          val _ =
            emit ("br i1 " ^ isRoot ^ ", label %" ^ rootLabel ^
                  ", label %" ^ initLabel)
          val _ = emit (rootLabel ^ ":")
          val rootedInPlace = rootArrayInPlace (value, elementTy)
          val _ = emit ("store ptr " ^ #text rootedInPlace ^ ", ptr " ^ outSlot)
          val _ = emit ("br label %" ^ doneLabel)
          val _ = emit (initLabel ^ ":")
          val _ =
            emit (dataPtr ^ " = getelementptr " ^ headerTy ^ ", ptr " ^
                  header ^ ", i32 0, i32 0")
          val _ =
            emit (lenPtr ^ " = getelementptr " ^ headerTy ^ ", ptr " ^
                  header ^ ", i32 0, i32 1")
          val _ = emit (data ^ " = load ptr, ptr " ^ dataPtr)
          val _ = emit (len ^ " = load i64, ptr " ^ lenPtr)
          val _ =
            emit (elemSize ^ " = ptrtoint ptr getelementptr (" ^
                  elemStorage ^ ", ptr null, i32 1) to i64")
          val _ = emit (bytes ^ " = mul i64 " ^ len ^ ", " ^ elemSize)
          val _ =
            emit (newData ^ " = call ptr @yack_root_alloc(i64 " ^ bytes ^ ")")
          val _ =
            emit (newHeader ^ " = call ptr @yack_root_alloc(i64 32)")
          val _ = emit ("store i64 0, ptr " ^ idxSlot)
          val _ = emit ("br label %" ^ loopCond)
          val _ = emit (loopCond ^ ":")
          val _ = emit (idx ^ " = load i64, ptr " ^ idxSlot)
          val _ = emit (more ^ " = icmp slt i64 " ^ idx ^ ", " ^ len)
          val _ =
            emit ("br i1 " ^ more ^ ", label %" ^ loopBody ^
                  ", label %" ^ loopDone)
          val _ = emit (loopBody ^ ":")
          val _ =
            emit (elemPtr ^ " = getelementptr " ^ elemStorage ^
                  ", ptr " ^ data ^ ", i64 " ^ idx)
          val _ =
            emit (elem ^ " = load " ^ elemStorage ^ ", ptr " ^ elemPtr)
          val rooted =
            rootValue ({ text = elem, ty = elementTy, const = false },
                       elementTy)
          val _ =
            emit (newElemPtr ^ " = getelementptr " ^ elemStorage ^
                  ", ptr " ^ newData ^ ", i64 " ^ idx)
          val _ =
            emit ("store " ^ elemStorage ^ " " ^
                  #text (coerce (rooted, elementTy)) ^ ", ptr " ^
                  newElemPtr)
          val _ = emit (nextIdx ^ " = add i64 " ^ idx ^ ", 1")
          val _ = emit ("store i64 " ^ nextIdx ^ ", ptr " ^ idxSlot)
          val _ = emit ("br label %" ^ loopCond)
          val _ = emit (loopDone ^ ":")
          val newDataPtr = fresh "root.arr.newdatap"
          val newLenPtr = fresh "root.arr.newlenp"
          val newCapPtr = fresh "root.arr.newcapp"
          val newOwnerPtr = fresh "root.arr.newownerp"
          val _ =
            emit (newDataPtr ^ " = getelementptr " ^ headerTy ^ ", ptr " ^
                  newHeader ^ ", i32 0, i32 0")
          val _ = emit ("store ptr " ^ newData ^ ", ptr " ^ newDataPtr)
          val _ =
            emit (newLenPtr ^ " = getelementptr " ^ headerTy ^ ", ptr " ^
                  newHeader ^ ", i32 0, i32 1")
          val _ = emit ("store i64 " ^ len ^ ", ptr " ^ newLenPtr)
          val _ =
            emit (newCapPtr ^ " = getelementptr " ^ headerTy ^ ", ptr " ^
                  newHeader ^ ", i32 0, i32 2")
          val _ = emit ("store i64 " ^ len ^ ", ptr " ^ newCapPtr)
          val _ =
            emit (newOwnerPtr ^ " = getelementptr " ^ headerTy ^ ", ptr " ^
                  newHeader ^ ", i32 0, i32 3")
          val _ =
            emit ("store ptr @yack_root_region, ptr " ^ newOwnerPtr)
          val _ = emit ("store ptr " ^ newHeader ^ ", ptr " ^ outSlot)
          val _ = emit ("br label %" ^ doneLabel)
          val _ = emit (doneLabel ^ ":")
          val out = fresh "root.arr.out"
          val _ = emit (out ^ " = load ptr, ptr " ^ outSlot)
        in
          { text = out, ty = Ir.TArray elementTy, const = false }
        end

      and rootMapStack (value, valueTy) =
        let
          val header = #text value
          val outSlot = allocStack (Ir.TMap valueTy)
          val headerTy = "{ ptr, i64, i64, i64, ptr }"
          val entryTy = "{ i32, { ptr, i64, i1 }, " ^ storageTy valueTy ^ " }"
          val isNull = fresh "root.map.null"
          val ownerPtr = fresh "root.map.ownerp"
          val owner = fresh "root.map.owner"
          val inScratch = fresh "root.map.inscratch"
          val notScratch = fresh "root.map.notscratch"
          val ownerIsRoot = fresh "root.map.ownerisroot"
          val isRoot = fresh "root.map.isroot"
          val skip = fresh "root.map.skip"
          val initLabel = freshLabel "root.map.init"
          val loopCond = freshLabel "root.map.cond"
          val loopBody = freshLabel "root.map.body"
          val copyLabel = freshLabel "root.map.copy"
          val skipLabel = freshLabel "root.map.skip"
          val loopDone = freshLabel "root.map.loopdone"
          val doneLabel = freshLabel "root.map.done"
          val dataPtr = fresh "root.map.datap"
          val lenPtr = fresh "root.map.lenp"
          val filledPtr = fresh "root.map.filledp"
          val capPtr = fresh "root.map.capp"
          val data = fresh "root.map.data"
          val len = fresh "root.map.len"
          val filled = fresh "root.map.filled"
          val cap = fresh "root.map.cap"
          val entrySize = fresh "root.map.entrysize"
          val bytes = fresh "root.map.bytes"
          val newData = fresh "root.map.newdata"
          val newHeader = fresh "root.map.newheader"
          val idxSlot = allocStack (Ir.Int 64)
          val idx = fresh "root.map.idx"
          val more = fresh "root.map.more"
          val entry = fresh "root.map.entry"
          val state = fresh "root.map.state"
          val used = fresh "root.map.used"
          val newEntry = fresh "root.map.newentry"
          val keyPtr = fresh "root.map.keyp"
          val key = fresh "root.map.key"
          val rootedKey = fresh "root.map.rootedkey"
          val valuePtr = fresh "root.map.valuep"
          val element = fresh "root.map.value"
          val nextIdx = fresh "root.map.next"
          val _ = emit ("store ptr " ^ header ^ ", ptr " ^ outSlot)
          val _ = emit (isNull ^ " = icmp eq ptr " ^ header ^ ", null")
          val _ =
            emit (ownerPtr ^ " = getelementptr " ^ headerTy ^ ", ptr " ^
                  header ^ ", i32 0, i32 4")
          val _ = emit (owner ^ " = load ptr, ptr " ^ ownerPtr)
          val _ =
            emit (inScratch ^ " = call i1 @yack_scratch_owns(ptr " ^
                  header ^ ")")
          val _ = emit (notScratch ^ " = xor i1 " ^ inScratch ^ ", true")
          val _ =
            emit (ownerIsRoot ^ " = icmp eq ptr " ^ owner ^
                  ", @yack_root_region")
          val _ =
            emit (isRoot ^ " = and i1 " ^ ownerIsRoot ^ ", " ^ notScratch)
          val _ = emit (skip ^ " = or i1 " ^ isNull ^ ", " ^ isRoot)
          val _ =
            emit ("br i1 " ^ skip ^ ", label %" ^ doneLabel ^
                  ", label %" ^ initLabel)
          val _ = emit (initLabel ^ ":")
          val _ =
            emit (dataPtr ^ " = getelementptr " ^ headerTy ^ ", ptr " ^
                  header ^ ", i32 0, i32 0")
          val _ =
            emit (lenPtr ^ " = getelementptr " ^ headerTy ^ ", ptr " ^
                  header ^ ", i32 0, i32 1")
          val _ =
            emit (filledPtr ^ " = getelementptr " ^ headerTy ^ ", ptr " ^
                  header ^ ", i32 0, i32 2")
          val _ =
            emit (capPtr ^ " = getelementptr " ^ headerTy ^ ", ptr " ^
                  header ^ ", i32 0, i32 3")
          val _ = emit (data ^ " = load ptr, ptr " ^ dataPtr)
          val _ = emit (len ^ " = load i64, ptr " ^ lenPtr)
          val _ = emit (filled ^ " = load i64, ptr " ^ filledPtr)
          val _ = emit (cap ^ " = load i64, ptr " ^ capPtr)
          val _ =
            emit (entrySize ^ " = ptrtoint ptr getelementptr (" ^ entryTy ^
                  ", ptr null, i32 1) to i64")
          val _ = emit (bytes ^ " = mul i64 " ^ cap ^ ", " ^ entrySize)
          val _ =
            emit (newData ^ " = call ptr @yack_root_alloc(i64 " ^ bytes ^ ")")
          val _ =
            emit (newHeader ^ " = call ptr @yack_root_alloc(i64 40)")
          val _ = emit ("store i64 0, ptr " ^ idxSlot)
          val _ = emit ("br label %" ^ loopCond)
          val _ = emit (loopCond ^ ":")
          val _ = emit (idx ^ " = load i64, ptr " ^ idxSlot)
          val _ = emit (more ^ " = icmp ult i64 " ^ idx ^ ", " ^ cap)
          val _ =
            emit ("br i1 " ^ more ^ ", label %" ^ loopBody ^
                  ", label %" ^ loopDone)
          val _ = emit (loopBody ^ ":")
          val _ =
            emit (entry ^ " = getelementptr " ^ entryTy ^ ", ptr " ^ data ^
                  ", i64 " ^ idx)
          val _ = emit (state ^ " = load i32, ptr " ^ entry)
          val _ = emit (used ^ " = icmp eq i32 " ^ state ^ ", 1")
          val _ =
            emit ("br i1 " ^ used ^ ", label %" ^ copyLabel ^
                  ", label %" ^ skipLabel)
          val _ = emit (copyLabel ^ ":")
          val _ =
            emit (keyPtr ^ " = getelementptr " ^ entryTy ^ ", ptr " ^ entry ^
                  ", i32 0, i32 1")
          val _ = emit (key ^ " = load { ptr, i64, i1 }, ptr " ^ keyPtr)
          val _ =
            emit (rootedKey ^ " = call { ptr, i64, i1 } @yack_string_root" ^
                  "({ ptr, i64, i1 } " ^ key ^ ")")
          val _ =
            emit (valuePtr ^ " = getelementptr " ^ entryTy ^ ", ptr " ^ entry ^
                  ", i32 0, i32 2")
          val _ = emit (element ^ " = load " ^ storageTy valueTy ^
                        ", ptr " ^ valuePtr)
          val rootedValue =
            rootValue ({ text = element, ty = valueTy, const = false },
                       valueTy)
          val _ =
            emit (newEntry ^ " = getelementptr " ^ entryTy ^ ", ptr " ^
                  newData ^ ", i64 " ^ idx)
          val _ = emit ("store i32 1, ptr " ^ newEntry)
          val newKeyPtr = fresh "root.map.newkeyp"
          val _ =
            emit (newKeyPtr ^ " = getelementptr " ^ entryTy ^ ", ptr " ^
                  newEntry ^ ", i32 0, i32 1")
          val _ = emit ("store { ptr, i64, i1 } " ^ rootedKey ^ ", ptr " ^
                        newKeyPtr)
          val newValuePtr = fresh "root.map.newvaluep"
          val _ =
            emit (newValuePtr ^ " = getelementptr " ^ entryTy ^ ", ptr " ^
                  newEntry ^ ", i32 0, i32 2")
          val _ = emit ("store " ^ storageTy valueTy ^ " " ^
                        #text (coerce (rootedValue, valueTy)) ^
                        ", ptr " ^ newValuePtr)
          val _ = emit ("br label %" ^ skipLabel)
          val _ = emit (skipLabel ^ ":")
          val _ = emit (nextIdx ^ " = add i64 " ^ idx ^ ", 1")
          val _ = emit ("store i64 " ^ nextIdx ^ ", ptr " ^ idxSlot)
          val _ = emit ("br label %" ^ loopCond)
          val _ = emit (loopDone ^ ":")
          val newDataPtr = fresh "root.map.newdatap"
          val newLenPtr = fresh "root.map.newlenp"
          val newFilledPtr = fresh "root.map.newfilledp"
          val newCapPtr = fresh "root.map.newcapp"
          val newOwnerPtr = fresh "root.map.newownerp"
          val _ =
            emit (newDataPtr ^ " = getelementptr " ^ headerTy ^ ", ptr " ^
                  newHeader ^ ", i32 0, i32 0")
          val _ = emit ("store ptr " ^ newData ^ ", ptr " ^ newDataPtr)
          val _ =
            emit (newLenPtr ^ " = getelementptr " ^ headerTy ^ ", ptr " ^
                  newHeader ^ ", i32 0, i32 1")
          val _ = emit ("store i64 " ^ len ^ ", ptr " ^ newLenPtr)
          val _ =
            emit (newFilledPtr ^ " = getelementptr " ^ headerTy ^ ", ptr " ^
                  newHeader ^ ", i32 0, i32 2")
          val _ = emit ("store i64 " ^ filled ^ ", ptr " ^ newFilledPtr)
          val _ =
            emit (newCapPtr ^ " = getelementptr " ^ headerTy ^ ", ptr " ^
                  newHeader ^ ", i32 0, i32 3")
          val _ = emit ("store i64 " ^ cap ^ ", ptr " ^ newCapPtr)
          val _ =
            emit (newOwnerPtr ^ " = getelementptr " ^ headerTy ^ ", ptr " ^
                  newHeader ^ ", i32 0, i32 4")
          val _ =
            emit ("store ptr @yack_root_region, ptr " ^ newOwnerPtr)
          val _ = emit ("store ptr " ^ newHeader ^ ", ptr " ^ outSlot)
          val _ = emit ("br label %" ^ doneLabel)
          val _ = emit (doneLabel ^ ":")
          val out = fresh "root.map.out"
          val _ = emit (out ^ " = load ptr, ptr " ^ outSlot)
        in
          { text = out, ty = Ir.TMap valueTy, const = false }
        end

      fun leaveArena () =
        case !arenaDirty of
          [] => ()
        | dirty :: rest =>
            ( arenaDirty := rest
            ; List.app
                (fn (dirtyValue, dirtyTy) =>
                  ignore (rootValue (dirtyValue, dirtyTy)))
                (List.rev (!dirty))
            )

      fun lowerCtor (dataName, constructor, argument, resultTy, siteRef) =
        case findData dataName of
          NONE => { text = "null", ty = resultTy, const = true }
        | SOME definition =>
            (case constructorInfo definition constructor of
               NONE => { text = "null", ty = resultTy, const = true }
             | SOME (tag, payloadField, argumentType) =>
                 (case argument of
                    NONE =>
                      { text =
                          "@yack_unit_" ^ dataName ^ "_" ^
                          Int.toString tag
                      , ty = resultTy
                      , const = true
                      }
                  | SOME _ =>
                 let
                   val structTy = "%" ^ dataName
                   val site = !siteRef
                   val native = (site = Ir.NativeStack)
                   val memory = fresh "mem"
                   val _ =
                     if native then
                       emit (memory ^ " = alloca " ^ structTy)
                     else
                       let
                         val sizePtr = fresh "sizeptr"
                         val size = fresh "size"
                         val _ =
                           emit (sizePtr ^ " = getelementptr " ^ structTy ^
                                 ", ptr null, i32 1")
                         val _ =
                           emit (size ^ " = ptrtoint ptr " ^ sizePtr ^
                                 " to i64")
                         val region =
                           case site of
                             Ir.Current => !currentRegionArg
                           | Ir.Destination => !currentDestArg
                           | Ir.NativeStack => !currentDestArg
                         val _ =
                           if stackMode then
                             emit (memory ^
                                   " = call ptr @yack_region_alloc(ptr " ^
                                   region ^ ", i64 " ^ size ^ ")")
                           else
                             emit (memory ^
                                   " = call ptr @yack_alloc(i64 " ^ size ^ ")")
                       in
                         ()
                       end
                   val tagPtr = fresh "tagp"
                   val stackPtr = fresh "stackp"
                   val _ =
                     emit (tagPtr ^ " = getelementptr inbounds " ^
                           structTy ^ ", ptr " ^ memory ^ ", i32 0, i32 0")
                   val _ = emit ("store i32 " ^ Int.toString tag ^
                                 ", ptr " ^ tagPtr)
                   val _ =
                     emit (stackPtr ^ " = getelementptr inbounds " ^
                           structTy ^ ", ptr " ^ memory ^ ", i32 0, i32 1")
                   val _ = emit ("store i1 " ^
                                 (if native then "true" else "false") ^
                                 ", ptr " ^ stackPtr)
                   val _ =
                     case (argument, argumentType) of
                       (SOME value, SOME elementTy) =>
                         let
                           val payloadPtr = fresh "payload"
                           val _ =
                             emit (payloadPtr ^ " = getelementptr inbounds " ^
                                   structTy ^ ", ptr " ^ memory ^
                                   ", i32 0, i32 " ^ Int.toString payloadField)
                           val _ =
                             emit ("store " ^ typed (coerce (value, elementTy)) ^
                                   ", ptr " ^ payloadPtr)
                         in
                           ()
                         end
                     | _ => ()
                 in
                   { text = memory, ty = resultTy, const = false }
                 end))

      fun lowerCall (locals, name, arguments, resultTy, escaping) =
        let
          val lowered = List.map (lowerExpr locals) arguments
        in
          if name = "os.exit" then
            let
              val argument =
                case lowered of
                  [value] => coerce (value, Ir.Int 32)
                | _ => { text = "0", ty = Ir.Int 32, const = true }
            in
              emit ("call void @exit(" ^ typed argument ^ ")");
              { text = "", ty = Ir.Unit, const = true }
            end
          else if name = "print_string" then
            let
              val argument =
                case lowered of
                  [value] => value
                | _ =>
                    { text = "zeroinitializer"
                    , ty = Ir.TString
                    , const = false
                    }
              val data = fresh "strdata"
              val length = fresh "strlen"
              val owned = fresh "strowned"
              val length32 = fresh "strlen32"
              val freeLabel = freshLabel "print.strfree"
              val doneLabel = freshLabel "print.strdone"
              val _ =
                emit (data ^ " = extractvalue { ptr, i64, i1 } " ^
                      #text argument ^ ", 0")
              val _ =
                emit (length ^ " = extractvalue { ptr, i64, i1 } " ^
                      #text argument ^ ", 1")
              val _ =
                emit (owned ^ " = extractvalue { ptr, i64, i1 } " ^
                      #text argument ^ ", 2")
              val _ =
                emit (length32 ^ " = trunc i64 " ^ length ^ " to i32")
              val _ =
                emit ("call i32 (ptr, ...) @printf(ptr @.fmt_string, i32 " ^
                      length32 ^ ", ptr " ^ data ^ ")")
              val _ =
                emit ("br i1 " ^ owned ^ ", label %" ^ freeLabel ^
                      ", label %" ^ doneLabel)
              val _ = emit (freeLabel ^ ":")
              val _ = emit ("call void @yack_free(ptr " ^ data ^ ")")
              val _ = emit ("br label %" ^ doneLabel)
              val _ = emit (doneLabel ^ ":")
            in
              { text = "", ty = Ir.Unit, const = true }
            end
          else if name = "print_int" then
            let
              val argument =
                case lowered of
                  [value] => coerce (value, Ir.Int 32)
                | _ => { text = "0", ty = Ir.Int 32, const = true }
            in
              emit ("call i32 (ptr, ...) @printf(ptr @.fmt_int, " ^
                    typed argument ^ ")");
              { text = "", ty = Ir.Unit, const = true }
            end
          else if name = "print_int64" then
            let
              val argument =
                case lowered of
                  [value] => coerce (value, Ir.Int 64)
                | _ => { text = "0", ty = Ir.Int 64, const = true }
            in
              emit ("call i32 (ptr, ...) @printf(ptr @.fmt_int64, " ^
                    typed argument ^ ")");
              { text = "", ty = Ir.Unit, const = true }
            end
          else if name = "int64_to_int" then
            let
              val argument =
                case lowered of
                  [value] => coerce (value, Ir.Int 64)
                | _ => { text = "0", ty = Ir.Int 64, const = true }
            in
              { text = LlvmOp.checkedNarrow (opEnv, #text argument)
              , ty = Ir.Int 32, const = false }
            end
          else if name = "int64_to_int_wrapping" then
            let
              val argument =
                case lowered of
                  [value] => coerce (value, Ir.Int 64)
                | _ => { text = "0", ty = Ir.Int 64, const = true }
              val result = fresh "int64.to.int"
            in
              emit (result ^ " = trunc i64 " ^ #text argument ^ " to i32");
              { text = result, ty = Ir.Int 32, const = false }
            end
          else if name = "int64_wrapping_add" orelse
                  name = "int64_wrapping_sub" orelse
                  name = "int64_wrapping_mul" then
            let
              val (left, right) =
                case lowered of
                  [l, r] => (coerce (l, Ir.Int 64), coerce (r, Ir.Int 64))
                | _ => ( { text = "0", ty = Ir.Int 64, const = true }
                       , { text = "0", ty = Ir.Int 64, const = true } )
              val instruction =
                if name = "int64_wrapping_add" then "add"
                else if name = "int64_wrapping_sub" then "sub"
                else "mul"
              val result = fresh "int64.wrap"
            in

              emit (result ^ " = " ^ instruction ^ " i64 " ^ #text left ^
                    ", " ^ #text right);
              { text = result, ty = Ir.Int 64, const = false }
            end
          else if name = "int_to_int64" then
            let
              val argument =
                case lowered of
                  [value] => coerce (value, Ir.Int 32)
                | _ => { text = "0", ty = Ir.Int 32, const = true }
              val result = fresh "int.to.int64"
            in
              emit (result ^ " = sext i32 " ^ #text argument ^ " to i64");
              { text = result, ty = Ir.Int 64, const = false }
            end
          else if name = "print_float" then
            let
              val argument =
                case lowered of
                  [value] => coerce (value, Ir.Float 64)
                | _ => { text = "0.0", ty = Ir.Float 64, const = true }
            in
              emit ("call i32 (ptr, ...) @printf(ptr @.fmt_float, " ^
                    typed argument ^ ")");
              { text = "", ty = Ir.Unit, const = true }
            end
          else if name = "print_bool" then
            let
              val argument =
                case lowered of
                  [value] => coerce (value, Ir.Int 32)
                | _ => { text = "0", ty = Ir.Int 32, const = true }
            in
              emit ("call i32 (ptr, ...) @printf(ptr @.fmt_int, " ^
                    typed argument ^ ")");
              { text = "", ty = Ir.Unit, const = true }
            end
          else if name = "new_token" orelse name = "new_resource" then
            { text = "0", ty = resultTy, const = true }
          else if name = "consume_token" orelse name = "drop_resource" then
            { text = "", ty = Ir.Unit, const = true }
          else if String.isPrefix "yack_arena_keep" name then
            (case lowered of
               [value] =>
                 let
                   val previous = !inPromotion
                   val _ = inPromotion := true
                   val promoted = rootValue (value, resultTy)
                   val _ = inPromotion := previous
                 in
                   promoted
                 end
             | _ => { text = "", ty = resultTy, const = true })
          else
            let
              val argumentsText =
                String.concatWith ", " (List.map typed lowered)
              val callArguments =
                if not stackMode then "(" ^ argumentsText ^ ")"
                else
                  let
                    val regionSlot = fresh "callregion"
                    val _ = emit (regionSlot ^ " = alloca %yack.region")
                    val dest =

                      if stackMode andalso not (List.null (!arenaMarks))
                      then !currentRegionArg
                      else if !escaping then !currentDestArg
                      else !currentRegionArg
                    val separator =
                      if argumentsText = "" then "" else ", "
                  in
                    "(ptr " ^ regionSlot ^ ", ptr " ^ dest ^ separator ^
                    argumentsText ^ ")"
                  end
            in
              if isUnit resultTy then
                ( emit ("call void @" ^ name ^ callArguments)
                ; { text = "", ty = Ir.Unit, const = true }
                )
              else
                let val reg = fresh "call" in
                  emit (reg ^ " = call " ^ llvmTy resultTy ^ " @" ^
                        name ^ callArguments);
                  { text = reg, ty = resultTy, const = false }
                end
            end
        end

      and emitCleanup cleanup =
        List.app
          (fn CleanupDrop value => dropValue (value, #ty value)
            | CleanupFree value => freeIfHeap value)
          cleanup

      and testPattern (owned, borrowed, value : value, pattern, success,
                       failure, locals) =
        case pattern of
          Ir.PWildcard =>
            let
              val _ = emit ("br label %" ^ success)
              val cleanup =
                if not borrowed andalso needsDrop (#ty value) then
                  [CleanupDrop value]
                else []
            in
              (locals, cleanup, [])
            end
        | Ir.PVar (name, _) =>
            let
              val copied =
                not borrowed andalso
                not owned andalso
                (case #ty value of
                   Ir.TData (_, _, true) => true
                 | _ => false)
              val bound =
                if copied then copyValue (value, #ty value) else value
              val locals' = bindLocal locals name bound
              val guardCleanup =
                if copied then [CleanupDrop bound] else []
            in
              emit ("br label %" ^ success);
              (locals', [], guardCleanup)
            end
        | Ir.PBool expected =>
            let val reg = fresh "pat" in
              emit (reg ^ " = icmp eq i1 " ^ #text value ^ ", " ^
                    (if expected then "true" else "false"));
              emit ("br i1 " ^ reg ^ ", label %" ^ success ^
                    ", label %" ^ failure);
              (locals, [], [])
            end
        | Ir.PInt (expected, _) =>
            let val reg = fresh "pat" in
              emit (reg ^ " = icmp eq " ^ llvmTy (#ty value) ^ " " ^
                    #text value ^ ", " ^ intText expected);
              emit ("br i1 " ^ reg ^ ", label %" ^ success ^
                    ", label %" ^ failure);
              (locals, [], [])
            end
        | Ir.PString (literal, rest, _) =>
            let
              val name = registerString literal
              val literalLength = String.size literal
              val data = fresh "str.pat.data"
              val length = fresh "str.pat.len"
              val longEnough = fresh "str.pat.lenok"
              val comparison = fresh "str.pat.cmp"
              val equal = fresh "str.pat.eq"
              val matched = fresh "str.pat.ok"
              val matchedLabel = freshLabel "pat.string"
              val _ =
                emit (data ^ " = extractvalue { ptr, i64, i1 } " ^
                      #text value ^ ", 0")
              val _ =
                emit (length ^ " = extractvalue { ptr, i64, i1 } " ^
                      #text value ^ ", 1")
              val _ =
                emit (longEnough ^ " = " ^
                      (case rest of
                         NONE => "icmp eq i64 "
                       | SOME _ => "icmp uge i64 ") ^
                      length ^ ", " ^ Int.toString literalLength)
              val _ =
                if literalLength = 0 then
                  emit (matched ^ " = and i1 " ^ longEnough ^ ", true")
                else
                  let
                    val _ =
                      emit (comparison ^ " = call i32 @memcmp(ptr " ^ data ^
                            ", ptr @" ^ name ^ ", i64 " ^
                            Int.toString literalLength ^ ")")
                    val _ = emit (equal ^ " = icmp eq i32 " ^ comparison ^ ", 0")
                  in
                    emit (matched ^ " = and i1 " ^ longEnough ^ ", " ^ equal)
                  end
              val _ =
                emit ("br i1 " ^ matched ^ ", label %" ^ matchedLabel ^
                      ", label %" ^ failure)
              val _ = emit (matchedLabel ^ ":")
            in
              case rest of
                NONE =>
                  ( emit ("br label %" ^ success)
                  ; ( locals
                    , if borrowed then [] else [CleanupDrop value]
                    , []
                    )
                  )
              | SOME subpattern =>
                  let
                    val suffix =
                      copyStringSuffix (value, data, length, literalLength)
                    val subFailure = freshLabel "pat.string.subfail"
                    val (locals', cleanup, guardCleanup) =
                      testPattern
                        (owned, borrowed, suffix, subpattern, success,
                         subFailure, locals)
                    val _ = emit (subFailure ^ ":")
                    val _ = emitCleanup [CleanupDrop suffix]
                    val _ = emit ("br label %" ^ failure)
                  in
                    ( locals'
                    , cleanup @
                      (if borrowed then [] else [CleanupDrop value])
                    , CleanupDrop suffix :: guardCleanup
                    )
                  end
            end
        | Ir.PBytes (bytes, rest, _) =>
            let
              val name = registerString bytes
              val literalLength = String.size bytes
              val dataPtr = fresh "bytes.pat.datap"
              val data = fresh "bytes.pat.data"
              val lengthPtr = fresh "bytes.pat.lenp"
              val length = fresh "bytes.pat.len"
              val longEnough = fresh "bytes.pat.lenok"
              val comparison = fresh "bytes.pat.cmp"
              val equal = fresh "bytes.pat.eq"
              val matched = fresh "bytes.pat.ok"
              val matchedLabel = freshLabel "pat.bytes"
              val _ =
                emit (dataPtr ^ " = getelementptr { ptr, i64, i64, ptr }, ptr " ^
                      #text value ^ ", i32 0, i32 0")
              val _ = emit (data ^ " = load ptr, ptr " ^ dataPtr)
              val _ =
                emit (lengthPtr ^ " = getelementptr { ptr, i64, i64, ptr }, ptr " ^
                      #text value ^ ", i32 0, i32 1")
              val _ = emit (length ^ " = load i64, ptr " ^ lengthPtr)
              val _ =
                emit (longEnough ^ " = " ^
                      (case rest of
                         NONE => "icmp eq i64 "
                       | SOME _ => "icmp uge i64 ") ^
                      length ^ ", " ^ Int.toString literalLength)
              val _ =
                if literalLength = 0 then
                  emit (matched ^ " = and i1 " ^ longEnough ^ ", true")
                else
                  let
                    val _ =
                      emit (comparison ^ " = call i32 @memcmp(ptr " ^ data ^
                            ", ptr @" ^ name ^ ", i64 " ^
                            Int.toString literalLength ^ ")")
                    val _ = emit (equal ^ " = icmp eq i32 " ^ comparison ^ ", 0")
                  in
                    emit (matched ^ " = and i1 " ^ longEnough ^ ", " ^ equal)
                  end
              val _ =
                emit ("br i1 " ^ matched ^ ", label %" ^ matchedLabel ^
                      ", label %" ^ failure)
              val _ = emit (matchedLabel ^ ":")
            in
              case rest of
                NONE =>
                  ( emit ("br label %" ^ success)
                  ; ( locals
                    , if borrowed then [] else [CleanupDrop value]
                    , []
                    )
                  )
              | SOME subpattern =>
                  let
                    val suffix =
                      copyBytesSuffix (value, data, length, literalLength)
                    val subFailure = freshLabel "pat.bytes.subfail"
                    val (locals', cleanup, guardCleanup) =
                      testPattern
                        (owned, borrowed, suffix, subpattern, success,
                         subFailure, locals)
                    val _ = emit (subFailure ^ ":")
                    val _ = emitCleanup [CleanupDrop suffix]
                    val _ = emit ("br label %" ^ failure)
                  in
                    ( locals'
                    , cleanup @
                      (if borrowed then [] else [CleanupDrop value])
                    , CleanupDrop suffix :: guardCleanup
                    )
                  end
            end
        | Ir.PTuple (patterns, ty) =>
            let
              val value =
                if borrowed then
                  case #ty value of
                    Ir.TBorrow (inner, _) =>
                      let
                        val reg = fresh "pat.aggregate"
                        val _ =
                          emit (reg ^ " = load " ^ llvmTy inner ^
                                ", ptr " ^ #text value)
                      in
                        { text = reg, ty = inner, const = false }
                      end
                  | _ => value
                else value
              val types =
                case ty of Ir.TTuple types => types | _ => []
              fun extract (index, ty) =
                let val reg = fresh "field" in
                  emit (reg ^ " = extractvalue " ^ llvmTy (#ty value) ^ " " ^
                        #text value ^ ", " ^ Int.toString index);
                  { text = reg, ty = ty, const = false }
                end
              fun loop (index, patterns, types, acc) =
                case (patterns, types) of
                  (pattern :: restPatterns, ty :: restTypes) =>
                    loop (index + 1, restPatterns, restTypes,
                          (extract (index, ty), pattern) :: acc)
                  | _ => List.rev acc
              val pairs = loop (0, patterns, types, [])
            in
              testSequence (owned, borrowed, pairs, success, failure, locals)
            end
        | Ir.PRecord (patterns, ty) =>
            let
              val value =
                if borrowed then
                  case #ty value of
                    Ir.TBorrow (inner, _) =>
                      let
                        val reg = fresh "pat.aggregate"
                        val _ =
                          emit (reg ^ " = load " ^ llvmTy inner ^
                                ", ptr " ^ #text value)
                      in
                        { text = reg, ty = inner, const = false }
                      end
                  | _ => value
                else value
              val fields =
                case ty of Ir.TRecord (fields, _) => fields | _ => []
              fun extract (index, ty) =
                let val reg = fresh "field" in
                  emit (reg ^ " = extractvalue " ^ llvmTy (#ty value) ^ " " ^
                        #text value ^ ", " ^ Int.toString index);
                  { text = reg, ty = ty, const = false }
                end
              fun loop (index, fields, patterns, acc) =
                case (fields, patterns) of
                  ((_, ty) :: restFields, (_, pattern) :: restPatterns) =>
                    loop (index + 1, restFields, restPatterns,
                          (extract (index, ty), pattern) :: acc)
                  | _ => List.rev acc
              val pairs = loop (0, fields, patterns, [])
            in
              testSequence (owned, borrowed, pairs, success, failure, locals)
            end
        | Ir.PCtor (dataName, constructor, argument, _) =>
            (case findData dataName of
               NONE =>
                 (emit ("br label %" ^ failure); (locals, [], []))
             | SOME definition =>
                 (case constructorInfo definition constructor of
                    NONE =>
                      (emit ("br label %" ^ failure); (locals, [], []))
                  | SOME (tag, payloadField, argumentType) =>
                      let
                        val structTy = "%" ^ dataName
                        val tagPtr = fresh "tagp"
                        val tagValue = fresh "tag"
                        val compare = fresh "istag"
                        val tagOk = freshLabel "pat.tag"
                        val _ =
                          emit (tagPtr ^ " = getelementptr inbounds " ^
                                structTy ^ ", ptr " ^ #text value ^
                                ", i32 0, i32 0")
                        val _ =
                          emit (tagValue ^ " = load i32, ptr " ^ tagPtr)
                        val _ =
                          emit (compare ^ " = icmp eq i32 " ^ tagValue ^
                                ", " ^ Int.toString tag)
                        val _ =
                          emit ("br i1 " ^ compare ^ ", label %" ^ tagOk ^
                                ", label %" ^ failure)
                        val _ = emit (tagOk ^ ":")
                      in
                        case (argument, argumentType) of
                          (SOME subpattern, SOME elementTy) =>
                            let
                              val payloadPtr = fresh "payload"
                              val payload = fresh "payloadv"
                              val _ =
                                emit (payloadPtr ^ " = getelementptr inbounds " ^
                                      structTy ^ ", ptr " ^ #text value ^
                                      ", i32 0, i32 " ^ Int.toString payloadField)
                              val _ =
                                emit (payload ^ " = load " ^ storageTy elementTy ^
                                      ", ptr " ^ payloadPtr)
                              val (locals', cleanup, guardCleanup) =
                                testPattern
                                  ( owned
                                  , borrowed
                                  , { text = payload, ty = elementTy
                                    , const = false }
                                  , subpattern, success, failure, locals
                                  )
                            in
                              ( locals'
                              , cleanup @
                                (if borrowed then []
                                 else [CleanupFree value])
                              , guardCleanup
                              )
                            end
                        | _ =>
                            ( emit ("br label %" ^ success)
                            ; ( locals
                              , if borrowed then []
                                else [CleanupFree value]
                              , []
                              )
                            )
                      end))

      and testSequence (owned, borrowed, pairs, success, failure, locals) =
        let
          fun loop (remaining, locals, cleanup, guardCleanup) =
            case remaining of
              [] =>
                ( emit ("br label %" ^ success)
                ; (locals, cleanup, guardCleanup)
                )
            | (value, pattern) :: rest =>
                let
                  val next = freshLabel "pat.next"
                  val pairFailure =
                    if List.null guardCleanup then failure
                    else freshLabel "pat.fail"
                  val (locals', cleanup', guardCleanup') =
                    testPattern
                      (owned, borrowed, value, pattern, next, pairFailure,
                       locals)
                  val _ =
                    if pairFailure = failure then ()
                    else
                      ( emit (pairFailure ^ ":")
                      ; emitCleanup guardCleanup
                      ; emit ("br label %" ^ failure)
                      )
                  val _ = emit (next ^ ":")
                in
                  loop
                    ( rest
                    , locals'
                    , cleanup @ cleanup'
                    , guardCleanup @ guardCleanup'
                    )
                end
        in
          loop (pairs, locals, [], [])
        end

      and copyStringSuffix (value, data, length, start) =
        let
          val owned = fresh "str.pat.owned"
          val start64 = Int64.fromInt start
          val suffixLength = fresh "str.pat.suffixlen"
          val viewPointer = fresh "str.pat.viewptr"
          val copyLabel = freshLabel "pat.string.copy"
          val viewLabel = freshLabel "pat.string.view"
          val doneLabel = freshLabel "pat.string.done"
          val size = fresh "str.pat.size"
          val memory = fresh "str.pat.mem"
          val endPointer = fresh "str.pat.end"
          val copy0 = fresh "str.pat.copy.v0"
          val copy1 = fresh "str.pat.copy.v1"
          val copy2 = fresh "str.pat.copy.v2"
          val view0 = fresh "str.pat.view.v0"
          val view1 = fresh "str.pat.view.v1"
          val view2 = fresh "str.pat.view.v2"
          val result = fresh "str.pat.suffix"
          val _ =
            emit (owned ^ " = extractvalue { ptr, i64, i1 } " ^
                  #text value ^ ", 2")
          val _ =
            emit (suffixLength ^ " = sub i64 " ^ length ^ ", " ^
                  Int64.toString start64)
          val _ =
            emit (viewPointer ^ " = getelementptr i8, ptr " ^ data ^
                  ", i64 " ^ Int64.toString start64)
          val _ =
            emit ("br i1 " ^ owned ^ ", label %" ^ copyLabel ^
                  ", label %" ^ viewLabel)
          val _ = emit (copyLabel ^ ":")
          val _ = emit (size ^ " = add i64 " ^ suffixLength ^ ", 1")
          val _ =
            emit (memory ^ " = call ptr @yack_string_alloc(i64 " ^ size ^ ")")
          val _ =
            emit ("call ptr @memcpy(ptr " ^ memory ^ ", ptr " ^ viewPointer ^
                  ", i64 " ^ suffixLength ^ ")")
          val _ =
            emit (endPointer ^ " = getelementptr i8, ptr " ^ memory ^
                  ", i64 " ^ suffixLength)
          val _ = emit ("store i8 0, ptr " ^ endPointer)
          val _ =
            emit (copy0 ^ " = insertvalue { ptr, i64, i1 } undef, ptr " ^
                  memory ^ ", 0")
          val _ =
            emit (copy1 ^ " = insertvalue { ptr, i64, i1 } " ^ copy0 ^
                  ", i64 " ^ suffixLength ^ ", 1")
          val _ =
            emit (copy2 ^ " = insertvalue { ptr, i64, i1 } " ^ copy1 ^
                  ", i1 true, 2")
          val _ = emit ("br label %" ^ doneLabel)
          val _ = emit (viewLabel ^ ":")
          val _ =
            emit (view0 ^ " = insertvalue { ptr, i64, i1 } undef, ptr " ^
                  viewPointer ^ ", 0")
          val _ =
            emit (view1 ^ " = insertvalue { ptr, i64, i1 } " ^ view0 ^
                  ", i64 " ^ suffixLength ^ ", 1")
          val _ =
            emit (view2 ^ " = insertvalue { ptr, i64, i1 } " ^ view1 ^
                  ", i1 false, 2")
          val _ = emit ("br label %" ^ doneLabel)
          val _ = emit (doneLabel ^ ":")
          val _ =
            emit (result ^ " = phi { ptr, i64, i1 } [ " ^ copy2 ^
                  ", %" ^ copyLabel ^ " ], [ " ^ view2 ^
                  ", %" ^ viewLabel ^ " ]")
        in
          { text = result, ty = Ir.TString, const = false }
        end

      and copyBytesSuffix (value, sourceData, length, start) =
        let
          val start64 = Int64.fromInt start
          val suffixLength = fresh "bytes.pat.suffixlen"
          val capacity = fresh "bytes.pat.capacity"
          val small = fresh "bytes.pat.small"
          val data = fresh "bytes.pat.newdata"
          val source = fresh "bytes.pat.source"
          val empty = fresh "bytes.pat.empty"
          val copyLabel = freshLabel "bytes.pat.copy"
          val copiedLabel = freshLabel "bytes.pat.copied"
          val header = fresh "bytes.pat.header"
          val dataPtr = fresh "bytes.pat.newdatap"
          val lengthPtr = fresh "bytes.pat.newlenp"
          val capacityPtr = fresh "bytes.pat.newcapp"
          val ownerPtr = fresh "bytes.pat.ownerp"
          val _ =
            emit (suffixLength ^ " = sub i64 " ^ length ^ ", " ^
                  Int64.toString start64)
          val _ = emit (small ^ " = icmp slt i64 " ^ suffixLength ^ ", 4")
          val _ =
            emit (capacity ^ " = select i1 " ^ small ^
                  ", i64 4, i64 " ^ suffixLength)
          val _ =
            emit (data ^ " = call ptr @yack_root_alloc(i64 " ^ capacity ^ ")")
          val _ =
            emit (source ^ " = getelementptr i8, ptr " ^ sourceData ^
                  ", i64 " ^ Int64.toString start64)
          val _ = emit (empty ^ " = icmp eq i64 " ^ suffixLength ^ ", 0")
          val _ =
            emit ("br i1 " ^ empty ^ ", label %" ^ copiedLabel ^
                  ", label %" ^ copyLabel)
          val _ = emit (copyLabel ^ ":")
          val _ =
            emit ("call ptr @memcpy(ptr " ^ data ^ ", ptr " ^ source ^
                  ", i64 " ^ suffixLength ^ ")")
          val _ = emit ("br label %" ^ copiedLabel)
          val _ = emit (copiedLabel ^ ":")
          val _ = emit (header ^ " = call ptr @yack_root_alloc(i64 32)")
          val _ =
            emit (dataPtr ^ " = getelementptr { ptr, i64, i64, ptr }, ptr " ^
                  header ^ ", i32 0, i32 0")
          val _ = emit ("store ptr " ^ data ^ ", ptr " ^ dataPtr)
          val _ =
            emit (lengthPtr ^ " = getelementptr { ptr, i64, i64, ptr }, ptr " ^
                  header ^ ", i32 0, i32 1")
          val _ = emit ("store i64 " ^ suffixLength ^ ", ptr " ^ lengthPtr)
          val _ =
            emit (capacityPtr ^ " = getelementptr { ptr, i64, i64, ptr }, ptr " ^
                  header ^ ", i32 0, i32 2")
          val _ = emit ("store i64 " ^ capacity ^ ", ptr " ^ capacityPtr)
          val _ =
            emit (ownerPtr ^ " = getelementptr { ptr, i64, i64, ptr }, ptr " ^
                  header ^ ", i32 0, i32 3")
          val _ =
            emit ("store ptr " ^
                  (if stackMode then "@yack_root_region" else "null") ^
                  ", ptr " ^ ownerPtr)
        in
          { text = header, ty = Ir.TArray (Ir.Int 8), const = false }
        end

      and lowerMatch (locals, scrutinee, branches, resultTy) =
        let
          val scrutineeValue = lowerExpr locals scrutinee
          val scrutineeBorrowed =
            case #ty scrutineeValue of
              Ir.TBorrow _ => true
            | _ => false
          val scrutineeOwned =
            if scrutineeBorrowed then false
            else
              case scrutinee of
                Ir.Copy _ => true
              | Ir.Ctor _ => true
              | _ => false
          val join = freshLabel "match.end"
          val resultSlot =
            if isUnit resultTy then NONE else SOME (allocStack resultTy)
          fun emitDrops (locals, drops) =
            List.app
              (fn {name, ty, skip, ...} =>
                case findLocal locals name of
                  SOME value => dropValueSkipping (value, ty, skip)
                | NONE => ())
              drops
          fun lowerBranch ((pattern, guard, body, drops), remaining) =
            let
              val bodyLabel = freshLabel "match.body"
              val failure = freshLabel "match.fail"
              val (locals', cleanup, guardCleanup) =
                testPattern
                  ( scrutineeOwned, scrutineeBorrowed, scrutineeValue,
                    pattern, bodyLabel, failure, locals )
              val _ = emit (bodyLabel ^ ":")
              val _ =
                case guard of
                  NONE => emitCleanup cleanup
                | SOME condition =>
                    let
                      val guardPass = freshLabel "match.guardpass"
                      val guardFail = freshLabel "match.guardfail"
                      val conditionValue =
                        coerce (lowerExpr locals' condition, Ir.Bool)
                      val _ =
                        emit
                          ("br i1 " ^ #text conditionValue ^ ", label %" ^
                           guardPass ^ ", label %" ^ guardFail)

                      val _ = emit (guardFail ^ ":")
                      val _ = emitCleanup guardCleanup
                      val _ = emit ("br label %" ^ failure)
                      val _ = emit (guardPass ^ ":")
                      val _ = emitCleanup cleanup
                    in
                      ()
                    end
              val bodyValue = lowerExpr locals' body
              val _ = emitDrops (locals', drops)
              val _ =
                case resultSlot of
                  SOME slot =>
                    emit ("store " ^ llvmTy resultTy ^ " " ^
                          #text (coerce (bodyValue, resultTy)) ^
                          ", ptr " ^ slot)
                | NONE => ()
              val _ = emit ("br label %" ^ join)
            in
              case remaining of
                [] =>
                  (emit (failure ^ ":"); emit "unreachable")
              | next :: rest =>
                  (emit (failure ^ ":"); lowerBranch (next, rest))
            end
        in
          case branches of
            [] => { text = "", ty = Ir.Unit, const = true }
          | first :: rest =>
              let
                val _ = lowerBranch (first, rest)
                val _ = emit (join ^ ":")
              in
                case resultSlot of
                  NONE => { text = "", ty = Ir.Unit, const = true }
                | SOME slot =>
                    let val reg = fresh "load" in
                      emit (reg ^ " = load " ^ llvmTy resultTy ^
                            ", ptr " ^ slot);
                      { text = reg, ty = resultTy, const = false }
                    end
              end
        end

      and placeSteps (Ir.Var (name, _, _)) = SOME (name, [])
        | placeSteps (Ir.Field (base, index, _)) =
            (case placeSteps base of
               SOME (name, steps) => SOME (name, steps @ [index])
             | NONE => NONE)
        | placeSteps (Ir.TupleIndex (base, index, _)) =
            (case placeSteps base of
               SOME (name, steps) => SOME (name, steps @ [index])
             | NONE => NONE)
        | placeSteps _ = NONE

      and lowerExpr locals expression =
        let
          val value = lowerExprRaw locals expression
        in
          if stackMode andalso !storeRoot then
            rootValue (value, #ty value)
          else
            value
        end

      and lowerExprRaw locals expression =
        case expression of
          Ir.IntLit (value, ty) =>
            { text = intText value, ty = ty, const = true }
        | Ir.FloatLit (value, ty) =>
            { text = floatText value, ty = ty, const = true }
        | Ir.StringLit (text, ty) =>
            let
              val name = registerString text
              val length = String.size text
              val structTy = llvmTy ty
              val v0 = fresh "str"
              val v1 = fresh "str"
              val v2 = fresh "str"
              val _ =
                emit (v0 ^ " = insertvalue " ^ structTy ^ " undef, ptr @" ^
                      name ^ ", 0")
              val _ =
                emit (v1 ^ " = insertvalue " ^ structTy ^ " " ^ v0 ^
                      ", i64 " ^ Int.toString length ^ ", 1")
              val _ =
                emit (v2 ^ " = insertvalue " ^ structTy ^ " " ^ v1 ^
                      ", i1 false, 2")
            in
              { text = v2, ty = ty, const = false }
            end
        | Ir.StringBuiltin (name, arguments, resultTy, site) =>
            lowerStringBuiltin (locals, name, arguments, resultTy, site)
        | Ir.BytesBuiltin (name, arguments, resultTy, site) =>
            lowerBytesBuiltin (locals, name, arguments, resultTy, site)
        | Ir.SysBuiltin (name, arguments, resultTy) =>
            lowerSysBuiltin (locals, name, arguments, resultTy)
        | Ir.ArrayBuiltin (name, arguments, resultTy, siteRef) =>
            lowerArrayBuiltin (locals, name, arguments, resultTy, siteRef)
        | Ir.MapBuiltin (name, arguments, resultTy, siteRef) =>
            lowerMapBuiltin (locals, name, arguments, resultTy, siteRef)
        | Ir.PrintString (argument, consume) =>
            let
              val value = lowerExpr locals argument
              val data = fresh "strdata"
              val length = fresh "strlen"
              val owned = fresh "strowned"
              val length32 = fresh "strlen32"
              val freeLabel = freshLabel "print.strfree"
              val doneLabel = freshLabel "print.strdone"
              val _ =
                emit (data ^ " = extractvalue { ptr, i64, i1 } " ^
                      #text value ^ ", 0")
              val _ =
                emit (length ^ " = extractvalue { ptr, i64, i1 } " ^
                      #text value ^ ", 1")
              val _ =
                emit (length32 ^ " = trunc i64 " ^ length ^ " to i32")
              val _ =
                emit ("call i32 (ptr, ...) @printf(ptr @.fmt_string, i32 " ^
                      length32 ^ ", ptr " ^ data ^ ")")
              val _ =
                if consume then
                  emit ("call void @yack_drop_string({ ptr, i64, i1 } " ^
                        #text value ^ ")")
                else ()
            in
              { text = "", ty = Ir.Unit, const = true }
            end
        | Ir.BoolLit value =>
            { text = if value then "true" else "false"
            , ty = Ir.Bool
            , const = true
            }
        | Ir.UnitLit => { text = "", ty = Ir.Unit, const = true }
        | Ir.Var (name, ty, _) =>
            (case findLocal locals name of
               SOME value => value
             | NONE =>
                 { text = if isUnit ty then "" else "0"
                 , ty = ty
                 , const = true
                 })
        | Ir.Bin ("^", _, _, resultTy, site) =>
            let
              fun flatten expression =
                case expression of
                  Ir.Bin ("^", left, right, _, _) =>
                    flatten left @ flatten right
                | _ => [expression]
            in
              lowerConcat (locals, flatten expression, resultTy, site)
            end
        | Ir.Bin (operator, left, right, resultTy, _) =>
            lowerBin
              (operator, lowerExpr locals left, lowerExpr locals right, resultTy)
        | Ir.Neg (operand, resultTy) =>
            let val value = lowerExpr locals operand in
              case resultTy of
                Ir.Int _ =>
                  { text = LlvmOp.lowerIntNeg
                      (opEnv, coerce (value, resultTy))
                  , ty = resultTy
                  , const = false
                  }
              | Ir.Float _ =>
                  let val reg = fresh "neg" in
                    emit (reg ^ " = fneg " ^ llvmTy resultTy ^ " " ^
                          #text (coerce (value, resultTy)));
                    { text = reg, ty = resultTy, const = false }
                  end
              | _ => value
            end
        | Ir.Call (name, arguments, resultTy, escaping) =>
            lowerCall (locals, name, arguments, resultTy, escaping)
        | Ir.If (condition, thenBranch, elseBranch, resultTy) =>
            let
              val conditionValue = coerce (lowerExpr locals condition, Ir.Bool)
              val thenLabel = freshLabel "if.then"
              val elseLabel = freshLabel "if.else"
              val joinLabel = freshLabel "if.join"
              val slot =
                if isUnit resultTy then NONE else SOME (allocStack resultTy)
              val _ =
                emit ("br i1 " ^ #text conditionValue ^ ", label %" ^
                      thenLabel ^ ", label %" ^ elseLabel)
              val _ = emit (thenLabel ^ ":")
              val thenValue = lowerExpr locals thenBranch
              val _ =
                case slot of
                  NONE => ()
                | SOME destination =>
                    emit ("store " ^ storageTy resultTy ^ " " ^
                          #text (coerce (thenValue, resultTy)) ^
                          ", ptr " ^ destination)
              val _ = emit ("br label %" ^ joinLabel)
              val _ = emit (elseLabel ^ ":")
              val elseValue = lowerExpr locals elseBranch
              val _ =
                case slot of
                  NONE => ()
                | SOME destination =>
                    emit ("store " ^ storageTy resultTy ^ " " ^
                          #text (coerce (elseValue, resultTy)) ^
                          ", ptr " ^ destination)
              val _ = emit ("br label %" ^ joinLabel)
              val _ = emit (joinLabel ^ ":")
            in
              case slot of
                NONE => { text = "", ty = Ir.Unit, const = true }
              | SOME source =>
                  let val reg = fresh "if" in
                    emit (reg ^ " = load " ^ llvmTy resultTy ^
                          ", ptr " ^ source);
                    { text = reg, ty = resultTy, const = false }
                  end
            end
        | Ir.Let (bindings, body, drops) =>
            let
              val useMark =
                stackMode andalso
                not (LowerEscape.hasPointers (irType body)) andalso
                List.exists
                  (fn (_, ty, value) =>
                     LowerEscape.hasPointers ty andalso safeLocalScope value)
                  bindings andalso
                safeLocalScope body
              val markSlot = if useMark then SOME (allocStack (Ir.Int 64)) else NONE
              val _ =
                case markSlot of
                  SOME slot =>
                    let val mark = fresh "let.mark"
                    in
                      emit (mark ^ " = call i64 @yack_region_mark(ptr %region)");
                      emit ("store i64 " ^ mark ^ ", ptr " ^ slot)
                    end
                | NONE => ()
              fun bindOne ((name, _, valueExpr), locals) =
                bindLocal locals name (lowerExpr locals valueExpr)
              val extended = List.foldl bindOne locals bindings
              val value = lowerExpr extended body
              val _ =
                List.app
                  (fn {name, ty, skip, ...} =>
                    case findLocal extended name of
                      SOME bound => dropValueSkipping (bound, ty, skip)
                    | NONE => ())
                  drops
              val _ =
                case markSlot of
                  SOME slot =>
                    let val mark = fresh "let.mark"
                    in
                      emit (mark ^ " = load i64, ptr " ^ slot);
                      emit ("call void @yack_region_release(ptr %region, i64 " ^
                            mark ^ ")")
                    end
                | NONE => ()
            in
              value
            end
        | Ir.Tuple (values, ty) =>
            lowerTuple (List.map (lowerExpr locals) values, ty)
        | Ir.Record (fields, ty) =>
            lowerRecord
              (List.map (fn (name, value) => (name, lowerExpr locals value)) fields,
               ty)
        | Ir.Field (base, index, ty) =>
            let val value = lowerExpr locals base in
              case #ty value of
                Ir.TRecord (fields, _) =>
                  let
                    val fieldTy =
                      #2 ((List.nth (fields, index) handle _ => ("?", ty)))
                    val reg = fresh "field" in
                    emit (reg ^ " = extractvalue " ^ llvmTy (#ty value) ^ " " ^
                          #text value ^ ", " ^ Int.toString index);
                    { text = reg, ty = fieldTy, const = false }
                  end
              | Ir.TBorrow (Ir.TRecord (fields, _), _) =>
                  let
                    val fieldTy =
                      #2 ((List.nth (fields, index) handle _ => ("?", ty)))
                    val pointer = fresh "borrow.fieldp"
                    val reg = fresh "borrow.field"
                    val _ =
                      emit (pointer ^ " = getelementptr " ^
                            llvmTy (Ir.TRecord (fields, Ir.DAffine)) ^ ", ptr " ^
                            #text value ^ ", i32 0, i32 " ^
                            Int.toString index)
                    val _ =
                      emit (reg ^ " = load " ^ llvmTy fieldTy ^
                            ", ptr " ^ pointer)
                  in
                    { text = reg, ty = fieldTy, const = false }
                  end
              | _ => value
            end
        | Ir.RawField (base, offset, ty) =>
            let
              val value = lowerExpr locals base
              val pointer = fresh "raw.fieldp"
              val reg = fresh "raw.field"
              val _ =
                emit (pointer ^ " = getelementptr i8, ptr " ^
                      #text value ^ ", i64 " ^ Int.toString offset)
              val _ =
                emit (reg ^ " = load " ^ llvmTy ty ^ ", ptr " ^ pointer ^
                      ", align 1")
            in
              { text = reg, ty = ty, const = false }
            end
        | Ir.TupleIndex (base, index, ty) =>
            let val value = lowerExpr locals base in
              let val reg = fresh "field" in
                emit (reg ^ " = extractvalue " ^ llvmTy (#ty value) ^ " " ^
                      #text value ^ ", " ^ Int.toString index);
                { text = reg, ty = ty, const = false }
              end
            end
        | Ir.Ctor (dataName, constructor, argument, resultTy, siteRef) =>
            lowerCtor
              ( dataName
              , constructor
              , Option.map (lowerExpr locals) argument
              , resultTy
              , siteRef
              )
        | Ir.Match (scrutinee, branches, resultTy) =>
            lowerMatch (locals, scrutinee, branches, resultTy)
        | Ir.Copy (operand, ty) =>
            copyValue (lowerExpr locals operand, ty)
        | Ir.Borrow (_, operand, resultTy) =>
            let
              fun fallback () =
                let val value = lowerExpr locals operand in
                  case resultTy of
                    Ir.TString => value
                  | Ir.TBorrow (Ir.TString, _) => value
                  | Ir.TBorrow (Ir.TData _, _) => value
                  | Ir.TBorrow (Ir.TArray _, _) => value
                  | Ir.TBorrow (Ir.TMap _, _) => value
                  | _ =>
                      let
                        val slot = allocStack (#ty value)
                        val _ =
                          emit ("store " ^ storageTy (#ty value) ^ " " ^
                                (if isUnit (#ty value) then "0"
                                 else #text value) ^
                                ", ptr " ^ slot)
                      in
                        { text = slot, ty = resultTy, const = false }
                      end
                end
              fun descend (pointer, currentTy, []) = (pointer, currentTy)
                | descend (pointer, currentTy, index :: rest) =
                    let
                      val fieldPtr = fresh "place.fieldp"
                      val _ =
                        emit (fieldPtr ^ " = getelementptr " ^
                              storageTy currentTy ^ ", ptr " ^
                              pointer ^ ", i32 0, i32 " ^
                              Int.toString index)
                      val nextTy = elementType currentTy index
                    in
                      descend (fieldPtr, nextTy, rest)
                    end
              fun fromSlot (slot, rootTy, steps) =
                let
                  val (address, finalTy) = descend (slot, rootTy, steps)
                  fun loadReference () =
                    let val reg = fresh "place.load" in
                      emit (reg ^ " = load " ^ storageTy finalTy ^
                            ", ptr " ^ address);
                      { text = reg, ty = resultTy, const = false }
                    end
                in
                  case resultTy of
                    Ir.TString => loadReference ()
                  | Ir.TBorrow (Ir.TString, _) => loadReference ()
                  | Ir.TBorrow (Ir.TData _, _) => loadReference ()
                  | Ir.TBorrow (Ir.TArray _, _) => loadReference ()
                  | Ir.TBorrow (Ir.TMap _, _) => loadReference ()
                  | _ => { text = address, ty = resultTy, const = false }
                end
            in
              case placeSteps operand of
                SOME (rootName, steps) =>
                  (case findLocalAddress locals rootName of
                     SOME (slot, rootTy) => fromSlot (slot, rootTy, steps)
                   | NONE => fallback ())
              | NONE => fallback ()
            end
        | Ir.Closure (name, functionTy) =>
            { text = "@" ^ name, ty = functionTy, const = true }
        | Ir.ClosureRef (target, functionTy) =>
            { text = "@" ^ target, ty = functionTy, const = true }
        | Ir.Apply (callee, arguments, resultTy) =>
            let
              val calleeValue = lowerExpr locals callee
              val loweredArguments =
                List.map (lowerExpr locals) arguments
              val argumentText =
                String.concatWith ", "
                  (List.map (fn value => typed value) loweredArguments)
              val callArguments =
                if not stackMode then "(" ^ argumentText ^ ")"
                else
                  let
                    val regionSlot = fresh "applyregion"
                    val _ = emit (regionSlot ^ " = alloca %yack.region")
                    val dest =
                      if stackMode andalso not (List.null (!arenaMarks))
                      then !currentRegionArg
                      else if resultEscapes resultTy then !currentDestArg
                      else !currentRegionArg
                    val separator =
                      if argumentText = "" then "" else ", "
                  in
                    "(ptr " ^ regionSlot ^ ", ptr " ^ dest ^
                    separator ^ argumentText ^ ")"
                  end
              val callText = "call " ^ llvmTy resultTy ^ " " ^
                             #text calleeValue ^ callArguments
            in
              if isUnit resultTy then
                ( emit callText
                ; { text = "", ty = Ir.Unit, const = true }
                )
              else
                let val reg = fresh "closure.result" in
                  emit (reg ^ " = " ^ callText);
                  { text = reg, ty = resultTy, const = false }
                end
            end

      and lowerStringBuiltin (locals, name, arguments, resultTy, site) =
        LlvmBuiltinString.lower
          { emit = emit
          , fresh = fresh
          , freshLabel = freshLabel
          , lowerExpr = lowerExpr
          , allocStack = allocStack
          , allocString = allocStringForSite (!site)
          , coerce = coerce
          , lowerCtor = lowerCtor
          }
          (locals, name, arguments, resultTy)
      and lowerSysBuiltin (locals, name, arguments, resultTy) =
        LlvmBuiltinSys.lower
          { emit = emit
          , fresh = fresh
          , freshLabel = freshLabel
          , lowerExpr = lowerExpr
          , allocString = allocStringForSite Ir.Current
          , stackMode = stackMode
          , enterArena = enterArena
          , leaveArena = leaveArena
          , arenaMark = arenaMark
          , arenaRelease = arenaRelease
          }
          (locals, name, arguments, resultTy)
      and lowerBytesBuiltin (locals, name, arguments, resultTy, site) =
        LlvmBuiltinBytes.lower
          { emit = emit
          , fresh = fresh
          , freshLabel = freshLabel
          , lowerExpr = lowerExpr
          , allocStack = allocStack
          , allocString = allocStringForSite (!site)
          , coerce = coerce
          , dropValue = dropValue
          , needsDrop = needsDrop
          , currentDestArg = !currentDestArg
          , stackMode = stackMode
          , lowerCtor = lowerCtor
          }
          (locals, name, arguments, resultTy)
      and lowerArrayBuiltin (locals, name, arguments, resultTy, siteRef) =
        LlvmBuiltinArray.lower
          { emit = emit
          , fresh = fresh
          , freshLabel = freshLabel
          , lowerExpr = lowerExpr
          , allocStack = allocStack
          , currentRegionArg = currentRegionArg
          , currentDestArg = currentDestArg
          , storeRoot = storeRoot
          , rootValue = rootValue
          , registerDirty = registerDirty
          , mayContainString = mayContainString
          , enterStoreRoot = enterStoreRoot
          , leaveStoreRoot = leaveStoreRoot
          , stackMode = stackMode
          , inArena = fn () => not (List.null (!arenaMarks))
          , coerce = coerce
          , needsDrop = needsDrop
          , dropValue = dropValue
          }
          (locals, name, arguments, resultTy, siteRef)
      and lowerMapBuiltin (locals, name, arguments, resultTy, siteRef) =
        LlvmBuiltinMap.lower
          { emit = emit
          , fresh = fresh
          , freshLabel = freshLabel
          , lowerExpr = lowerExpr
          , allocStack = allocStack
          , bindLocal = bindLocal
          , findLocal = findLocal
          , coerce = coerce
          , typed = typed
          , dropValue = dropValue
          , freeIfHeap = freeIfHeap
          , currentRegionArg = currentRegionArg
          , currentDestArg = currentDestArg
          , storeRoot = storeRoot
          , rootValue = rootValue
          , enterStoreRoot = enterStoreRoot
          , leaveStoreRoot = leaveStoreRoot
          , currentFunctionName = !currentFunctionName
          , stackMode = stackMode
          , inArena = fn () => not (List.null (!arenaMarks))
          }
          (locals, name, arguments, resultTy, siteRef)
      and lowerConcat (locals, operands, resultTy, site) =
        case operands of
          [] => { text = "zeroinitializer", ty = resultTy, const = false }
        | [single] => lowerExpr locals single
        | _ =>
            let
              val parts =
                List.map
                  (fn operand =>
                    let
                      val value = lowerExpr locals operand
                      val data = fresh "concat.data"
                      val length = fresh "concat.len"
                      val owned = fresh "concat.owned"
                      val _ =
                        emit (data ^ " = extractvalue { ptr, i64, i1 } " ^
                              #text value ^ ", 0")
                      val _ =
                        emit (length ^ " = extractvalue { ptr, i64, i1 } " ^
                              #text value ^ ", 1")
                      val _ =
                        emit (owned ^ " = extractvalue { ptr, i64, i1 } " ^
                              #text value ^ ", 2")
                    in
                      (data, length, owned)
                    end)
                  operands
              fun totalLength ([], accumulated) = accumulated
                | totalLength ((_, length, _) :: rest, accumulated) =
                    let val next = fresh "concat.total" in
                      emit (next ^ " = add i64 " ^ accumulated ^ ", " ^ length);
                      totalLength (rest, next)
                    end
              val total = totalLength (parts, "0")
              val size = fresh "concat.size"
              val endPtr = fresh "concat.end"
              val _ =
                emit (size ^ " = add i64 " ^ total ^ ", 1")

              val (memory, copyFrom, firstFree) =
                case parts of
                  (data0, length0, owned0) :: _ =>
                    if stackMode then
                      (allocStringForSite (!site) size, parts, owned0)
                    else
                      let
                        val depth = fresh "concat.depth"
                        val idle = fresh "concat.idle"
                        val grow = fresh "concat.grow"
                        val free0 = fresh "concat.free0"
                        val grown = fresh "concat.grown"
                        val fresh0 = fresh "concat.fresh"
                        val memory = fresh "concat.mem"
                        val growLabel = freshLabel "concat.grow"
                        val copyLabel = freshLabel "concat.copy"
                        val joinLabel = freshLabel "concat.join"
                      in
                        emit (depth ^ " = load i64, ptr @yack_arena_depth, align 8");
                        emit (idle ^ " = icmp eq i64 " ^ depth ^ ", 0");
                        emit (grow ^ " = and i1 " ^ owned0 ^ ", " ^ idle);
                        emit (free0 ^ " = xor i1 " ^ owned0 ^ ", " ^ grow);
                        emit ("br i1 " ^ grow ^ ", label %" ^ growLabel ^
                              ", label %" ^ copyLabel);
                        emit (growLabel ^ ":");
                        emit (grown ^ " = call ptr @yack_realloc(ptr " ^ data0 ^
                              ", i64 " ^ length0 ^ ", i64 " ^ size ^ ")");
                        emit ("br label %" ^ joinLabel);
                        emit (copyLabel ^ ":");
                        emit (fresh0 ^ " = call ptr @yack_string_alloc(i64 " ^ size ^ ")");
                        emit ("call ptr @memcpy(ptr " ^ fresh0 ^ ", ptr " ^ data0 ^
                              ", i64 " ^ length0 ^ ")");
                        emit ("br label %" ^ joinLabel);
                        emit (joinLabel ^ ":");
                        emit (memory ^ " = phi ptr [ " ^ grown ^ ", %" ^ growLabel ^
                              " ], [ " ^ fresh0 ^ ", %" ^ copyLabel ^ " ]");
                        (memory, (data0, length0, "false") :: tl parts, free0)
                      end
                | [] => (allocStringForSite (!site) size, parts, "false")
              fun copyParts ([], offset) = offset
                | copyParts ((data, length, _) :: rest, offset) =
                    let
                      val destination = fresh "concat.dst"
                      val next = fresh "concat.offset"
                      val _ =
                        emit (destination ^ " = getelementptr i8, ptr " ^
                              memory ^ ", i64 " ^ offset)
                      val _ =
                        emit ("call ptr @memcpy(ptr " ^ destination ^
                              ", ptr " ^ data ^ ", i64 " ^ length ^ ")")
                      val _ =
                        emit (next ^ " = add i64 " ^ offset ^ ", " ^ length)
                    in
                      copyParts (rest, next)
                    end

              val _ =
                if stackMode then copyParts (parts, "0")
                else
                  case copyFrom of
                    (_, length0, _) :: rest => copyParts (rest, length0)
                  | [] => "0"
              val _ =
                emit (endPtr ^ " = getelementptr i8, ptr " ^ memory ^
                      ", i64 " ^ total)
              val _ = emit ("store i8 0, ptr " ^ endPtr)
              fun freeParts [] = ()
                | freeParts ((data, _, owned) :: rest) =
                    let
                      val freeLabel = freshLabel "concat.free"
                      val doneLabel = freshLabel "concat.freed"
                    in
                      emit ("br i1 " ^ owned ^ ", label %" ^ freeLabel ^
                            ", label %" ^ doneLabel);
                      emit (freeLabel ^ ":");
                      emit ("call void @yack_free(ptr " ^ data ^ ")");
                      emit ("br label %" ^ doneLabel);
                      emit (doneLabel ^ ":");
                      freeParts rest
                    end
              val _ =
                case parts of
                  (data0, length0, _) :: rest =>
                    freeParts ((data0, length0, firstFree) :: rest)
                | [] => ()
              val v0 = fresh "concat.v0"
              val v1 = fresh "concat.v1"
              val v2 = fresh "concat.v2"
              val _ =
                emit (v0 ^ " = insertvalue { ptr, i64, i1 } undef, ptr " ^
                      memory ^ ", 0")
              val _ =
                emit (v1 ^ " = insertvalue { ptr, i64, i1 } " ^ v0 ^
                      ", i64 " ^ total ^ ", 1")
              val _ =
                emit (v2 ^ " = insertvalue { ptr, i64, i1 } " ^ v1 ^
                      ", i1 true, 2")
            in
              { text = v2, ty = resultTy, const = false }
            end

      fun lowerTailExpr functionDrops safeNames locals expression =
        LlvmTail.lowerExprWith
          { emit = emit
          , fresh = fresh
          , freshLabel = freshLabel
          , lowerExpr = lowerExpr
          , allocStack = allocStack
          , bindLocal = bindLocal
          , findLocal = findLocal
          , coerce = coerce
          , typed = typed
          , dropValue = dropValue
          , freeIfHeap = freeIfHeap
          , currentFunctionName = !currentFunctionName
          , currentIsRecursive = currentIsRecursive
          , currentNeedsRegion = currentNeedsRegion
          , selfLoop = selfLoop
          , stackMode = stackMode
          , dropValueSkipping = dropValueSkipping
          , testPattern = testPattern
          , canTailCall = fn () => not (!hasDynamicAlloca)
          , irType = irType
          , safeLocalScope = safeLocalScope
          }
          functionDrops safeNames locals expression
      fun borrowRootName (Ir.Var (name, _, _)) = SOME name
        | borrowRootName (Ir.Field (base, _, _)) = borrowRootName base
        | borrowRootName (Ir.TupleIndex (base, _, _)) = borrowRootName base
        | borrowRootName (Ir.RawField (base, _, _)) = borrowRootName base
        | borrowRootName _ = NONE

      fun collectBorrowRoots expression =
        let
          val names = StringSet.create 64
          fun add name = ignore (StringSet.insert (names, name))
          fun walkOption value =
            case value of
              SOME inner => walk inner
            | NONE => ()
          and walk expression =
            case expression of
              Ir.IntLit _ => ()
            | Ir.FloatLit _ => ()
            | Ir.StringLit _ => ()
            | Ir.BoolLit _ => ()
            | Ir.UnitLit => ()
            | Ir.Var _ => ()
            | Ir.Bin (_, left, right, _, _) => (walk left; walk right)
            | Ir.Neg (operand, _) => walk operand
            | Ir.Call (_, arguments, _, _) => List.app walk arguments
            | Ir.If (condition, thenBranch, elseBranch, _) =>
                (walk condition; walk thenBranch; walk elseBranch)
            | Ir.Let (bindings, body, _) =>
                ( List.app (fn (_, _, value) => walk value) bindings
                ; walk body
                )
            | Ir.Tuple (values, _) => List.app walk values
            | Ir.Record (fields, _) =>
                List.app (fn (_, value) => walk value) fields
            | Ir.Field (base, _, _) => walk base
            | Ir.TupleIndex (base, _, _) => walk base
            | Ir.RawField (base, _, _) => walk base
            | Ir.Ctor (_, _, argument, _, _) => walkOption argument
            | Ir.Match (scrutinee, branches, _) =>
                ( walk scrutinee
                ; List.app
                    (fn (_, guard, body, _) =>
                       (walkOption guard; walk body))
                    branches
                )
            | Ir.PrintString (value, _) => walk value
            | Ir.StringBuiltin (_, arguments, _, _) =>
                List.app (fn (value, _) => walk value) arguments
            | Ir.BytesBuiltin (_, arguments, _, _) =>
                List.app (fn (value, _) => walk value) arguments
            | Ir.SysBuiltin (_, arguments, _) => List.app walk arguments
            | Ir.ArrayBuiltin (_, arguments, _, _) => List.app walk arguments
            | Ir.MapBuiltin (_, arguments, _, _) =>
                List.app (fn (value, _) => walk value) arguments
            | Ir.Copy (value, _) => walk value
            | Ir.Borrow (_, operand, _) =>
                ( (case borrowRootName operand of
                     SOME name => add name
                   | NONE => ())
                ; walk operand
                )
            | Ir.Closure _ => ()
            | Ir.ClosureRef _ => ()
            | Ir.Apply (callee, arguments, _) =>
                ( walk callee ; List.app walk arguments )
        in
          walk expression;
          names
        end

      val preludeOptionNames =
        let
          val names = StringSet.create 8
          fun add definition =
            if #prelude definition andalso #sourceName definition = "option"
               andalso
               (case #constructors definition of
                  [("None", NONE), ("Some", SOME _)] => true
                | _ => false)
            then ignore (StringSet.insert (names, #name definition))
            else ()
          val _ = List.app add datatypes
        in
          names
        end
      fun isPreludeOption name = StringSet.member (preludeOptionNames, name)

      val (lowerFunction, generateDropFunction, generateOwnerDropFunction,
           generateCopyFunction, generateRootFunction) =
        LlvmFunction.lowerFunctionWith
          { emit = emit
          , fresh = fresh
          , freshLabel = freshLabel
          , lowerTailExpr = lowerTailExpr
          , collectBorrowRoots = collectBorrowRoots
          , coerce = coerce
          , typed = typed
          , dropValue = dropValue
          , dropValueSkipping = dropValueSkipping
          , findLocal = findLocal
          , bindLocal = bindLocal
          , allocStack = allocStack
          , copyValue = copyValue
          , rootValue = rootValue
          , currentRegionArg = currentRegionArg
          , currentDestArg = currentDestArg
          , currentFunctionName = currentFunctionName
          , currentIsRecursive = currentIsRecursive
          , currentNeedsRegion = currentNeedsRegion
          , addressTaken = addressTaken
          , isPreludeOption = isPreludeOption
          , selfLoop = selfLoop
          , stackPlan = stackPlan
          , stackMode = stackMode
          , inEntryBlock = inEntryBlock
          , hasDynamicAlloca = hasDynamicAlloca
          , resetArenaMarks = fn () => arenaMarks := []
          }
      val _ = List.app lowerFunction functions
      val _ = List.app generateDropFunction datatypes
      val _ = if stackMode then List.app generateOwnerDropFunction datatypes
              else ()
      val _ = List.app generateCopyFunction datatypes
      val _ = List.app generateRootFunction datatypes

      fun findMain () =
        List.find
          (fn ({sourceName, ...} : Ir.funDef) => sourceName = "main")
          functions

      val dataDeclarations =
        List.map
          (fn definition =>
            "%" ^ #name definition ^ " = type " ^
            dataStructTy definition)
          datatypes

      val nullaryDeclarations =
        List.concat
          (List.map
             (fn definition =>
               let
                 val payloadTypes =
                   LlvmData.constructorPayloadTypes definition
                 fun loop (tag, constructors, acc) =
                   case constructors of
                     [] => List.rev acc
                   | (_, SOME _) :: rest =>
                       loop (tag + 1, rest, acc)
                   | (_, NONE) :: rest =>
                       let
                         val initializer =
                           "{ i32 " ^ Int.toString tag ^ ", i1 true" ^
                           String.concat
                             (List.map
                                (fn ty => ", " ^ ty ^ " zeroinitializer")
                                payloadTypes) ^
                           " }"
                         val name =
                           "@yack_unit_" ^ #name definition ^ "_" ^
                           Int.toString tag
                       in
                         loop
                           ( tag + 1
                           , rest
                           , (name ^ " = global " ^
                              dataStructTy definition ^ " " ^ initializer) :: acc
                           )
                       end
               in
                 loop (0, #constructors definition, [])
               end)
             datatypes)

      val stringDeclarations =
        List.map
          (fn (name, text) =>
            "@" ^ name ^ " = private unnamed_addr constant [" ^
            Int.toString (String.size text + 1) ^ " x i8] c\"" ^
            llvmEscape text ^ "\\00\"")
          (!stringLiterals)

      val stringRuntime =
        [ "define { ptr, i64, i1 } @yack_copy_string({ ptr, i64, i1 } %value) {"
        , "entry:"
        , "  %data = extractvalue { ptr, i64, i1 } %value, 0"
        , "  %len = extractvalue { ptr, i64, i1 } %value, 1"
        , "  %owned = extractvalue { ptr, i64, i1 } %value, 2"
        , "  br i1 %owned, label %copy, label %done"
        , "copy:"
        , "  %size = add i64 %len, 1"
        , "  %mem = call ptr @yack_string_alloc(i64 %size)"
        , "  call ptr @memcpy(ptr %mem, ptr %data, i64 %len)"
        , "  %end = getelementptr i8, ptr %mem, i64 %len"
        , "  store i8 0, ptr %end"
        , "  %v0 = insertvalue { ptr, i64, i1 } undef, ptr %mem, 0"
        , "  %v1 = insertvalue { ptr, i64, i1 } %v0, i64 %len, 1"
        , "  %v2 = insertvalue { ptr, i64, i1 } %v1, i1 true, 2"
        , "  ret { ptr, i64, i1 } %v2"
        , "done:"
        , "  ret { ptr, i64, i1 } %value"
        , "}"
        , ""
        , "define void @yack_drop_string({ ptr, i64, i1 } %value) {"
        , "entry:"
        , "  %data = extractvalue { ptr, i64, i1 } %value, 0"
        , "  %owned = extractvalue { ptr, i64, i1 } %value, 2"
        , "  br i1 %owned, label %free, label %done"
        , "free:"
        , "  call void @yack_free(ptr %data)"
        , "  br label %done"
        , "done:"
        , "  ret void"
        , "}"
        , ""
        , "define i64 @yack_hash_string({ ptr, i64, i1 } %value) {"
        , "entry:"
        , "  %data = extractvalue { ptr, i64, i1 } %value, 0"
        , "  %length = extractvalue { ptr, i64, i1 } %value, 1"
        , "  br label %loop"
        , "loop:"
        , "  %index = phi i64 [ 0, %entry ], [ %next, %body ]"
        , "  %hash = phi i64 [ 14695981039346656037, %entry ], [ %mixed, %body ]"
        , "  %more = icmp ult i64 %index, %length"
        , "  br i1 %more, label %body, label %done"
        , "body:"
        , "  %address = getelementptr i8, ptr %data, i64 %index"
        , "  %byte = load i8, ptr %address"
        , "  %wide = zext i8 %byte to i64"
        , "  %xored = xor i64 %hash, %wide"
        , "  %mixed = mul i64 %xored, 1099511628211"
        , "  %next = add i64 %index, 1"
        , "  br label %loop"
        , "done:"
        , "  ret i64 %hash"
        , "}"
        , ""
        , "define { ptr, i64, i1 } @yack_string_concat({ ptr, i64, i1 } %left, { ptr, i64, i1 } %right) {"
        , "entry:"
        , "  %ldata = extractvalue { ptr, i64, i1 } %left, 0"
        , "  %llen = extractvalue { ptr, i64, i1 } %left, 1"
        , "  %lowned = extractvalue { ptr, i64, i1 } %left, 2"
        , "  %rdata = extractvalue { ptr, i64, i1 } %right, 0"
        , "  %rlen = extractvalue { ptr, i64, i1 } %right, 1"
        , "  %rowned = extractvalue { ptr, i64, i1 } %right, 2"
        , "  %total = add i64 %llen, %rlen"
        , "  %size = add i64 %total, 1"
        , "  %mem = call ptr @yack_string_alloc(i64 %size)"
        , "  call ptr @memcpy(ptr %mem, ptr %ldata, i64 %llen)"
        , "  %offset = getelementptr i8, ptr %mem, i64 %llen"
        , "  call ptr @memcpy(ptr %offset, ptr %rdata, i64 %rlen)"
        , "  %end = getelementptr i8, ptr %mem, i64 %total"
        , "  store i8 0, ptr %end"
        , "  br i1 %lowned, label %freeleft, label %afterleft"
        , "freeleft:"
        , "  call void @yack_free(ptr %ldata)"
        , "  br label %afterleft"
        , "afterleft:"
        , "  br i1 %rowned, label %freeright, label %afterright"
        , "freeright:"
        , "  call void @yack_free(ptr %rdata)"
        , "  br label %afterright"
        , "afterright:"
        , "  %v0 = insertvalue { ptr, i64, i1 } undef, ptr %mem, 0"
        , "  %v1 = insertvalue { ptr, i64, i1 } %v0, i64 %total, 1"
        , "  %v2 = insertvalue { ptr, i64, i1 } %v1, i1 true, 2"
        , "  ret { ptr, i64, i1 } %v2"
        , "}"
        , ""
        ]

      val generated = List.rev (!lines)

      val usesScratch =
        List.exists
          (fn line => String.isSubstring "yack_scratch_enter" line)
          generated
      val usesThreads =
        List.exists
          (fn line => String.isSubstring "yack_thread_spawn" line)
          generated

      val allocationRuntime =
        AllocRuntime.runtime (mode, reserve, !debugAlloc, usesScratch,
                              usesThreads, Memory.traceEnabled ())

      val fixedDeclarations =
        [ ""
        , "@.fmt_int = private unnamed_addr constant [4 x i8] c\"%d\\0A\\00\""
        , "@.fmt_int_plain = private unnamed_addr constant [3 x i8] c\"%d\\00\""
        , "@.fmt_float = private unnamed_addr constant [4 x i8] c\"%f\\0A\\00\""
        , "@.fmt_float_g15 = private unnamed_addr constant [6 x i8] c\"%.15g\\00\""
        , "@.fmt_float_g16 = private unnamed_addr constant [6 x i8] c\"%.16g\\00\""
        , "@.fmt_float_g17 = private unnamed_addr constant [6 x i8] c\"%.17g\\00\""
        , "@.fmt_string = private unnamed_addr constant [6 x i8] c\"%.*s\\0A\\00\""
        , "@.fmt_span = private unnamed_addr constant [5 x i8] c\"%.*s\\00\""
        , "@.fmt_int64 = private unnamed_addr constant [6 x i8] c\"%lld\\0A\\00\""
        , "@.fmt_int64_plain = private unnamed_addr constant [5 x i8] c\"%lld\\00\""
        , "@.mode_rbe = private unnamed_addr constant [4 x i8] c\"rbe\\00\""
        , "@.mode_wbe = private unnamed_addr constant [4 x i8] c\"wbe\\00\""
        , "@stdin = external global ptr"
        , "@yack_fmt_buffer = thread_local global [64 x i8] zeroinitializer"
        , "declare i32 @printf(ptr, ...) #1"
        , "declare void @abort() noreturn"
        , "declare ptr @memcpy(ptr, ptr, i64) #1"
        , "declare i32 @madvise(ptr, i64, i32) #1"
        , "declare i32 @memcmp(ptr, ptr, i64) #0"
        , "declare ptr @memchr(ptr, i32, i64) #0"
        , "declare i32 @snprintf(ptr, i64, ptr, ...) #1"
        , "declare i64 @strlen(ptr) #0"
        , "declare i64 @strtoll(ptr, ptr, i32) #1"
        , "declare double @strtod(ptr, ptr) #1"
        , "declare ptr @fopen(ptr, ptr) #1"
        , "declare i32 @fclose(ptr) #1"
        , "declare i32 @fseek(ptr, i64, i32) #1"
        , "declare i64 @ftell(ptr) #1"
        , "declare i64 @fread(ptr, i64, i64, ptr) #1"
        , "declare i64 @fwrite(ptr, i64, i64, ptr) #1"
        , "declare i64 @recv(i32, ptr, i64, i32) #1"
        , "declare i64 @send(i32, ptr, i64, i32) #1"
        , "declare i32 @socket(i32, i32, i32) #1"
        , "declare i32 @bind(i32, ptr, i32) #1"
        , "declare i32 @setsockopt(i32, i32, i32, ptr, i32) #1"
        , "declare i32 @listen(i32, i32) #1"
        , "declare i32 @accept(i32, ptr, ptr) #1"
        , "declare i32 @accept4(i32, ptr, ptr, i32) #1"
        , "declare i32 @connect(i32, ptr, i32) #1"
        , "declare i32 @getsockname(i32, ptr, ptr) #1"
        , "declare i32 @close(i32) #1"
        , "declare void @exit(i32) noreturn"
        , "declare i32 @fcntl(i32, i32, i32) #1"
        , "declare i32 @epoll_create1(i32) #1"
        , "declare i32 @epoll_ctl(i32, i32, i32, ptr) #1"
        , "declare i32 @epoll_wait(i32, ptr, i32, i32) #1"
        , "declare i16 @htons(i16) #1"
        , "declare i32 @htonl(i32) #1"
        , "declare i16 @ntohs(i16) #1"
        , "declare ptr @__errno_location() #1"
        , "@yack_empty = private unnamed_addr constant [1 x i8] zeroinitializer"
        , "declare i32 @sqlite3_open(ptr, ptr) #1"
        , "declare i32 @sqlite3_close(ptr) #1"
        , "declare i32 @sqlite3_exec(ptr, ptr, ptr, ptr, ptr) #1"
        , "declare i32 @sqlite3_prepare_v2(ptr, ptr, i32, ptr, ptr) #1"
        , "declare i32 @sqlite3_finalize(ptr) #1"
        , "declare i32 @sqlite3_step(ptr) #1"
        , "declare i32 @sqlite3_column_count(ptr) #1"
        , "declare ptr @sqlite3_column_name(ptr, i32) #1"
        , "declare i32 @sqlite3_column_type(ptr, i32) #1"
        , "declare i32 @sqlite3_column_int(ptr, i32) #1"
        , "declare ptr @sqlite3_column_text(ptr, i32) #1"
        , "declare i32 @sqlite3_column_bytes(ptr, i32) #1"
        , "declare i32 @sqlite3_changes(ptr) #1"
        , "declare ptr @sqlite3_errmsg(ptr) #1"
        , ""
        ]

      fun symbolOfDeclaration line =
        if String.isPrefix "declare " line then
          let
            val body = String.substring (line, 8, String.size line - 8)
            fun beforeAt index =
              if index >= String.size body then NONE
              else if String.sub (body, index) = #"@" then
                let
                  fun take position acc =
                    if position >= String.size body then String.implode (List.rev acc)
                    else if String.sub (body, position) = #"(" then
                      String.implode (List.rev acc)
                    else take (position + 1) (String.sub (body, position) :: acc)
                in
                  SOME (take (index + 1) [])
                end
              else beforeAt (index + 1)
          in
            beforeAt 0
          end
        else NONE
      val declaredSymbols =
        List.mapPartial symbolOfDeclaration
          (dataDeclarations @ nullaryDeclarations @ stringDeclarations @
           fixedDeclarations)
      fun isDeclared symbol =
        List.exists (fn candidate => candidate = symbol) declaredSymbols
      val externDeclarations =
        List.filter
          (fn (declaration : Ir.externDecl) =>
            not (isDeclared (#symbol declaration)))
          externs
      val externLines =
        List.map
          (fn {symbol, parameters, result} =>
            let
              fun cParameter ty =
                case ty of
                  Ir.TString => "ptr"
                | Ir.TBorrow (Ir.TString, _) => "ptr"
                | _ => llvmTy ty
              val resultText =
                if isUnit result then "void" else llvmTy result
            in
              "declare " ^ resultText ^ " @" ^ symbol ^ "(" ^
              String.concatWith ", " (List.map cParameter parameters) ^ ")"
            end)
          externDeclarations

      val preamble =
        dataDeclarations @ nullaryDeclarations @ stringDeclarations @
        fixedDeclarations @ externLines @
        [ ""
        , "attributes #0 = { nounwind readonly willreturn }"
        , "attributes #1 = { nounwind willreturn }"
        , "attributes #2 = { nounwind allocsize(0) willreturn }"
        , ""
        ] @ allocationRuntime @ stringRuntime

      val mainWrapper =
        case findMain () of
          NONE => []
        | SOME {name, ...} =>
            if stackMode then
              [ "define i32 @main() {"
              , "entry:"
              , "  call void @yack_root_init()"
              , "  call void @yack_region_init(ptr @yack_main_region, ptr @yack_root_region)"
              , "  call void @" ^ name ^
                "(ptr @yack_main_region, ptr @yack_root_region)"
              , "  ret i32 0"
              , "}"
              ]
            else
              [ "define i32 @main() {"
              , "entry:"
              , "  call void @" ^ name ^ "()"
              ]
              @ (if !debugAlloc then
                   [ "  call void @yack_alloc_report()" ]
                 else [])
              @ [ "  ret i32 0"
                , "}"
                ]
    in
      let

        fun addSanitize line =
          if String.isPrefix "define " line then
            let val size = String.size line in
              if size >= 2 andalso
                 String.substring (line, size - 2, 2) = " {"
              then String.substring (line, 0, size - 2) ^ " #3 {"
              else line
            end
          else line
        val body =
          preamble @ generated @ [""] @ mainWrapper @ [""]
        val body =
          if Memory.poisonEnabled () then
            List.map addSanitize body @
            [ "attributes #3 = { sanitize_address }"
            , ""
            ]
          else body
      in
        String.concatWith "\n" body
      end
    end
end
