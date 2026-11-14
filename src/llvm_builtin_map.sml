structure LlvmBuiltinMap =
struct
  open LlvmTypes

  type env =
    { emit : string -> unit
    , fresh : string -> string
    , freshLabel : string -> string
    , lowerExpr : localEntry list -> Ir.expr -> value
    , allocStack : Ir.ty -> string
    , currentRegionArg : string ref
    , bindLocal : localEntry list -> string -> value -> localEntry list
    , findLocal : localEntry list -> string -> value option
    , coerce : value * Ir.ty -> value
    , typed : value -> string
    , dropValue : value * Ir.ty -> unit
    , freeIfHeap : value -> unit
    , currentDestArg : string ref
    , storeRoot : bool ref
    , rootValue : value * Ir.ty -> value
    , enterStoreRoot : unit -> unit
    , leaveStoreRoot : unit -> unit
    , currentFunctionName : string
    , stackMode : bool
    }

  fun lower (env : env) (locals, name, arguments, resultTy, siteRef) =
    let
      val emit = #emit env
      val fresh = #fresh env
      val freshLabel = #freshLabel env
      val lowerExpr = #lowerExpr env
      val allocStack = #allocStack env
      val currentRegionArg = #currentRegionArg env
      val bindLocal = #bindLocal env
      val findLocal = #findLocal env
      val coerce = #coerce env
      val typed = #typed env
      val dropValue = #dropValue env
      val freeIfHeap = #freeIfHeap env
      val currentDestArg = #currentDestArg env
      val storeRoot = #storeRoot env
      val rootValue = #rootValue env
      val enterStoreRoot = #enterStoreRoot env
      val leaveStoreRoot = #leaveStoreRoot env
      val currentFunctionName = #currentFunctionName env
      val stackMode = #stackMode env
      val unitValue = { text = "", ty = Ir.Unit, const = true }
      val headerTy = "{ ptr, i64, i64, i64, ptr }"
      val rootRegion = "@yack_root_region"

      fun allocRegion () =
        case !siteRef of
          Ir.Current => !currentRegionArg
        | Ir.Destination => !currentDestArg
        | Ir.NativeStack => !currentDestArg

      fun allocInRegion region size =
        let val reg = fresh "map.alloc" in
          if stackMode then
            emit (reg ^ " = call ptr @yack_region_alloc(ptr " ^ region ^
                  ", i64 " ^ size ^ ")")
          else
            emit (reg ^ " = call ptr @yack_root_alloc(i64 " ^ size ^ ")");
          reg
        end

      fun allocDirect region size =
        let val reg = fresh "map.alloc.direct" in
          if stackMode then
            emit (reg ^ " = call ptr @yack_region_alloc_direct(ptr " ^
                  region ^ ", i64 " ^ size ^ ")")
          else
            emit (reg ^ " = call ptr @yack_root_alloc(i64 " ^ size ^ ")");
          reg
        end

      fun allocBytes size =
        case !siteRef of
          Ir.Current => allocInRegion (allocRegion ()) size
        | _ => allocDirect (allocRegion ()) size

      fun ownerValue () =
        if stackMode then allocRegion () else "null"

      fun ownerOf mapValue =
        let
          val ownerPointer = fresh "map.ownerp"
          val _ =
            emit (ownerPointer ^ " = getelementptr " ^ headerTy ^ ", ptr " ^
                  #text mapValue ^ ", i32 0, i32 4")
          val owner = fresh "map.owner"
          val _ = emit (owner ^ " = load ptr, ptr " ^ ownerPointer)
        in
          owner
        end

      val lowered =
        case (name, arguments) of
          ("map_set",
           [(mapExpression, mapConsume),
            (keyExpression, keyConsume),
            (valueExpression, valueConsume)]) =>
            let
              val mapValue = lowerExpr locals mapExpression
              val valueTy =
                case #ty mapValue of
                  Ir.TMap ty => ty
                | Ir.TBorrow (Ir.TMap ty, _) => ty
                | _ => Ir.Unit
              val previousRoot = !storeRoot
              val _ = storeRoot := true
              val _ = enterStoreRoot ()
              val keyValue = lowerExpr locals keyExpression
              val previous = !currentDestArg
              val _ =
                if stackMode then
                  currentDestArg := ownerOf mapValue
                else ()
              val valueValue = lowerExpr locals valueExpression
              val _ = leaveStoreRoot ()
              val _ = currentDestArg := previous
              val _ = storeRoot := previousRoot
              val keyValue = rootValue (keyValue, Ir.TString)
              val valueValue = rootValue (valueValue, valueTy)
            in
              [ (mapValue, mapConsume)
              , (keyValue, keyConsume)
              , (valueValue, valueConsume)
              ]
            end
        | _ =>
            List.map
              (fn (expression, consume) =>
                (lowerExpr locals expression, consume))
              arguments

      fun valueTypeOf value =
        case #ty value of
          Ir.TMap valueTy => valueTy
        | Ir.TBorrow (Ir.TMap valueTy, _) => valueTy
        | _ => Ir.Unit

      fun entryTy valueTy =
        "{ i32, { ptr, i64, i1 }, " ^ storageTy valueTy ^ " }"

      fun fieldPointer value index =
        let val pointer = fresh "map.fieldp" in
          emit (pointer ^ " = getelementptr " ^ headerTy ^ ", ptr " ^
                #text value ^ ", i32 0, i32 " ^ Int.toString index);
          pointer
        end

      fun loadField value index storage =
        let
          val pointer = fieldPointer value index
          val reg = fresh "map.field"
        in
          emit (reg ^ " = load " ^ storage ^ ", ptr " ^ pointer);
          reg
        end

      fun storeField value index storage contents =
        let val pointer = fieldPointer value index in
          emit ("store " ^ storage ^ " " ^ contents ^ ", ptr " ^ pointer)
        end

      fun entrySize valueTy =
        let val reg = fresh "map.esize" in
          emit (reg ^ " = ptrtoint ptr getelementptr (" ^
                entryTy valueTy ^ ", ptr null, i32 1) to i64");
          reg
        end

      fun entryPointer (data, index, valueTy) =
        let val reg = fresh "map.entryp" in
          emit (reg ^ " = getelementptr " ^ entryTy valueTy ^
                ", ptr " ^ data ^ ", i64 " ^ index);
          reg
        end

      fun entryField (entry, valueTy, index) =
        let val reg = fresh "map.entryfield" in
          emit (reg ^ " = getelementptr " ^ entryTy valueTy ^
                ", ptr " ^ entry ^ ", i32 0, i32 " ^
                Int.toString index);
          reg
        end

      fun loadEntryKey (entry, valueTy) =
        let
          val pointer = entryField (entry, valueTy, 1)
          val reg = fresh "map.key"
        in
          emit (reg ^ " = load { ptr, i64, i1 }, ptr " ^ pointer);
          reg
        end

      fun loadEntryValue (entry, valueTy) =
        if isUnit valueTy then unitValue
        else
          let
            val pointer = entryField (entry, valueTy, 2)
            val reg = fresh "map.value"
          in
            emit (reg ^ " = load " ^ storageTy valueTy ^
                  ", ptr " ^ pointer);
            { text = reg, ty = valueTy, const = false }
          end

      fun loadEntryState (entry) =
        let val reg = fresh "map.state" in
          emit (reg ^ " = load i32, ptr " ^ entry);
          reg
        end

      fun storeEntryKey (entry, valueTy, key) =
        let val pointer = entryField (entry, valueTy, 1) in
          emit ("store { ptr, i64, i1 } " ^ key ^ ", ptr " ^ pointer)
        end

      fun storeEntryValue (entry, valueTy, value) =
        if isUnit valueTy then ()
        else
          let val pointer = entryField (entry, valueTy, 2) in
            emit ("store " ^ storageTy valueTy ^ " " ^ #text value ^
                  ", ptr " ^ pointer)
          end

      fun storeEntryState (entry, valueTy, state) =
        let val pointer = entryField (entry, valueTy, 0) in
          emit ("store i32 " ^ Int.toString state ^ ", ptr " ^ pointer)
        end

      fun dropKey key =
        emit ("call void @yack_drop_string({ ptr, i64, i1 } " ^ key ^ ")")

      fun hashKey key =
        let val reg = fresh "map.hash" in
          emit (reg ^ " = call i64 @yack_hash_string({ ptr, i64, i1 } " ^
                key ^ ")");
          reg
        end

      fun keyEquals (left, right, doneLabel) =
        let
          val leftData = fresh "map.ldata"
          val leftLen = fresh "map.llen"
          val rightData = fresh "map.rdata"
          val rightLen = fresh "map.rlen"
          val sameLength = fresh "map.samelen"
          val compareLabel = freshLabel "map.keycmp"
          val falseLabel = freshLabel "map.keyfalse"
          val cmp = fresh "map.memcmp"
          val eq = fresh "map.keyeq"
          val result = fresh "map.keyres"
          val _ = emit (leftData ^ " = extractvalue { ptr, i64, i1 } " ^
                        left ^ ", 0")
          val _ = emit (leftLen ^ " = extractvalue { ptr, i64, i1 } " ^
                        left ^ ", 1")
          val _ = emit (rightData ^ " = extractvalue { ptr, i64, i1 } " ^
                        right ^ ", 0")
          val _ = emit (rightLen ^ " = extractvalue { ptr, i64, i1 } " ^
                        right ^ ", 1")
          val _ = emit (sameLength ^ " = icmp eq i64 " ^ leftLen ^ ", " ^
                        rightLen)
          val _ = emit ("br i1 " ^ sameLength ^ ", label %" ^ compareLabel ^
                        ", label %" ^ falseLabel)
          val _ = emit (compareLabel ^ ":")
          val _ = emit (cmp ^ " = call i32 @memcmp(ptr " ^ leftData ^
                        ", ptr " ^ rightData ^ ", i64 " ^ leftLen ^ ")")
          val _ = emit (eq ^ " = icmp eq i32 " ^ cmp ^ ", 0")
          val _ = emit ("br label %" ^ doneLabel)
          val _ = emit (falseLabel ^ ":")
          val _ = emit ("br label %" ^ doneLabel)
          val _ = emit (doneLabel ^ ":")
          val _ = emit (result ^ " = phi i1 [ " ^ eq ^ ", %" ^
                        compareLabel ^ " ], [ false, %" ^ falseLabel ^ " ]")
        in
          result
        end

      fun emitGrow (map, valueTy) =
        let
          val dataPointer = fieldPointer map 0
          val lengthPointer = fieldPointer map 1
          val filledPointer = fieldPointer map 2
          val capacityPointer = fieldPointer map 3
          val oldData = fresh "map.grow.olddata"
          val oldCapacity = fresh "map.grow.oldcap"
          val length = fresh "map.grow.len"
          val isZero = fresh "map.grow.zero"
          val doubled = fresh "map.grow.doubled"
          val newCapacity = fresh "map.grow.newcap"
          val mask = fresh "map.grow.mask"
          val size = entrySize valueTy
          val bytes = fresh "map.grow.bytes"
          val newData = fresh "map.grow.newdata"
          val zeroSlot = fresh "map.grow.zeroidx"
          val zeroIndex = fresh "map.grow.zeroi"
          val zeroCond = fresh "map.grow.zerocond"
          val zeroEntry = fresh "map.grow.zeroentry"
          val zeroNext = fresh "map.grow.zeronext"
          val zeroLoop = freshLabel "map.grow.zeroloop"
          val zeroBody = freshLabel "map.grow.zerobody"
          val zeroDone = freshLabel "map.grow.zerodone"
          val moveSlot = fresh "map.grow.moveidx"
          val moveIndex = fresh "map.grow.movei"
          val moveCond = fresh "map.grow.movecond"
          val moveEntry = fresh "map.grow.moveentry"
          val moveState = fresh "map.grow.movestate"
          val moveUsed = fresh "map.grow.moveused"
          val moveNext = fresh "map.grow.movenext"
          val moveBody = freshLabel "map.grow.movebody"
          val moveSkip = freshLabel "map.grow.moveskip"
          val movePlace = freshLabel "map.grow.moveplace"
          val moveLoop = freshLabel "map.grow.moveloop"
          val moveDone = freshLabel "map.grow.movedone"
          val probeLoop = freshLabel "map.grow.probe"
          val probeBody = freshLabel "map.grow.probebody"
          val probeNext = freshLabel "map.grow.probenext"
          val probeIndex = fresh "map.grow.probei"
          val probeEntry = fresh "map.grow.probeentry"
          val probeState = fresh "map.grow.probestate"
          val probeEmpty = fresh "map.grow.probeempty"
          val probeNextIndex = fresh "map.grow.probenexti"
          val _ = emit (oldData ^ " = load ptr, ptr " ^ dataPointer)
          val _ = emit (oldCapacity ^ " = load i64, ptr " ^ capacityPointer)
          val _ = emit (length ^ " = load i64, ptr " ^ lengthPointer)
          val _ = emit (isZero ^ " = icmp eq i64 " ^ oldCapacity ^ ", 0")
          val _ = emit (doubled ^ " = shl i64 " ^ oldCapacity ^ ", 1")
          val _ = emit (newCapacity ^ " = select i1 " ^ isZero ^
                        ", i64 8, i64 " ^ doubled)
          val _ = emit (mask ^ " = sub i64 " ^ newCapacity ^ ", 1")
          val _ = emit (bytes ^ " = mul i64 " ^ newCapacity ^ ", " ^ size)
          val _ =
            if stackMode then
              emit (newData ^ " = call ptr @yack_region_alloc_direct(ptr " ^
                    ownerOf map ^ ", i64 " ^ bytes ^ ")")
            else
              emit (newData ^ " = call ptr @yack_root_alloc(i64 " ^
                    bytes ^ ")")
          val _ = emit (zeroSlot ^ " = alloca i64")
          val _ = emit ("store i64 0, ptr " ^ zeroSlot)
          val _ = emit ("br label %" ^ zeroLoop)
          val _ = emit (zeroLoop ^ ":")
          val _ = emit (zeroIndex ^ " = load i64, ptr " ^ zeroSlot)
          val _ = emit (zeroCond ^ " = icmp ult i64 " ^ zeroIndex ^ ", " ^
                        newCapacity)
          val _ = emit ("br i1 " ^ zeroCond ^ ", label %" ^ zeroBody ^
                        ", label %" ^ zeroDone)
          val _ = emit (zeroBody ^ ":")
          val _ = emit (zeroEntry ^ " = getelementptr " ^ entryTy valueTy ^
                        ", ptr " ^ newData ^ ", i64 " ^ zeroIndex)
          val _ = emit ("store i32 0, ptr " ^ zeroEntry)
          val _ = emit (zeroNext ^ " = add i64 " ^ zeroIndex ^ ", 1")
          val _ = emit ("store i64 " ^ zeroNext ^ ", ptr " ^ zeroSlot)
          val _ = emit ("br label %" ^ zeroLoop)
          val _ = emit (zeroDone ^ ":")
          val _ = emit (moveSlot ^ " = alloca i64")
          val _ = emit ("store i64 0, ptr " ^ moveSlot)
          val _ = emit ("br label %" ^ moveLoop)
          val _ = emit (moveLoop ^ ":")
          val _ = emit (moveIndex ^ " = load i64, ptr " ^ moveSlot)
          val _ = emit (moveCond ^ " = icmp ult i64 " ^ moveIndex ^ ", " ^
                        oldCapacity)
          val _ = emit ("br i1 " ^ moveCond ^ ", label %" ^ moveBody ^
                        ", label %" ^ moveDone)
          val _ = emit (moveBody ^ ":")
          val _ = emit (moveEntry ^ " = getelementptr " ^ entryTy valueTy ^
                        ", ptr " ^ oldData ^ ", i64 " ^ moveIndex)
          val _ = emit (moveState ^ " = load i32, ptr " ^ moveEntry)
          val _ = emit (moveUsed ^ " = icmp eq i32 " ^ moveState ^ ", 1")
          val _ = emit ("br i1 " ^ moveUsed ^ ", label %" ^ movePlace ^
                        ", label %" ^ moveSkip)
          val _ = emit (moveSkip ^ ":")
          val _ = emit (moveNext ^ " = add i64 " ^ moveIndex ^ ", 1")
          val _ = emit ("store i64 " ^ moveNext ^ ", ptr " ^ moveSlot)
          val _ = emit ("br label %" ^ moveLoop)
          val _ = emit (movePlace ^ ":")
          val entryKey = loadEntryKey (moveEntry, valueTy)
          val entryValue = loadEntryValue (moveEntry, valueTy)
          val hash = hashKey entryKey
          val placedSlot = fresh "map.grow.placed"
          val _ = emit (placedSlot ^ " = and i64 " ^ hash ^ ", " ^ mask)
          val _ = emit ("br label %" ^ probeLoop)
          val _ = emit (probeLoop ^ ":")
          val _ = emit (probeIndex ^ " = phi i64 [ " ^ placedSlot ^
                        ", %" ^ movePlace ^ " ], [ " ^ probeNextIndex ^
                        ", %" ^ probeNext ^ " ]")
          val _ = emit (probeEntry ^ " = getelementptr " ^ entryTy valueTy ^
                        ", ptr " ^ newData ^ ", i64 " ^ probeIndex)
          val _ = emit (probeState ^ " = load i32, ptr " ^ probeEntry)
          val _ = emit (probeEmpty ^ " = icmp eq i32 " ^ probeState ^ ", 0")
          val _ = emit ("br i1 " ^ probeEmpty ^ ", label %" ^ probeBody ^
                        ", label %" ^ probeNext)
          val _ = emit (probeNext ^ ":")
          val probeSum = fresh "map.grow.probesum"
          val _ = emit (probeSum ^ " = add i64 " ^ probeIndex ^ ", 1")
          val _ = emit (probeNextIndex ^ " = and i64 " ^ probeSum ^
                        ", " ^ mask)
          val _ = emit ("br label %" ^ probeLoop)
          val _ = emit (probeBody ^ ":")
          val _ = storeEntryState (probeEntry, valueTy, 1)
          val _ = storeEntryKey (probeEntry, valueTy, entryKey)
          val _ = storeEntryValue (probeEntry, valueTy, entryValue)
          val _ = emit ("br label %" ^ moveSkip)
          val _ = emit (moveDone ^ ":")
          val _ = emit ("call void @yack_free(ptr " ^ oldData ^ ")")
          val _ = emit ("store ptr " ^ newData ^ ", ptr " ^ dataPointer)
          val _ = emit ("store i64 " ^ newCapacity ^ ", ptr " ^
                        capacityPointer)
          val _ = emit ("store i64 " ^ length ^ ", ptr " ^ filledPointer)
        in
          ()
        end

      fun emitProbe (map, key, nonEmpty, onFound, onMiss) =
        let
          val valueTy = valueTypeOf map
          val data = loadField map 0 "ptr"
          val capacity = loadField map 3 "i64"
          val lookupLabel = freshLabel "map.probe.lookup"
          val loopLabel = freshLabel "map.probe.loop"
          val checkLabel = freshLabel "map.probe.check"
          val matchLabel = freshLabel "map.probe.match"
          val foundLabel = freshLabel "map.probe.found"
          val missLabel = freshLabel "map.probe.miss"
          val advanceLabel = freshLabel "map.probe.advance"
          val slotSlot = fresh "map.probe.slotslot"
          val mask = fresh "map.probe.mask"
          val hash = fresh "map.probe.hash"
          val slot = fresh "map.probe.slot"
          val index = fresh "map.probe.index"
          val entry = fresh "map.probe.entry"
          val state = fresh "map.probe.state"
          val isEmpty = fresh "map.probe.isempty"
          val isUsed = fresh "map.probe.isused"
          val stored = fresh "map.probe.stored"
          val nextIndex = fresh "map.probe.next"
          val keyDone = freshLabel "map.keydone"
          val _ =
            if nonEmpty then ()
            else
              let val isZero = fresh "map.probe.zero" in
                emit (isZero ^ " = icmp eq i64 " ^ capacity ^ ", 0");
                emit ("br i1 " ^ isZero ^ ", label %" ^ missLabel ^
                      ", label %" ^ lookupLabel)
              end
          val _ =
            if nonEmpty then emit ("br label %" ^ lookupLabel)
            else ()
          val _ = emit (lookupLabel ^ ":")
          val _ = emit (mask ^ " = sub i64 " ^ capacity ^ ", 1")
          val _ = emit (hash ^ " = call i64 @yack_hash_string" ^
                        "({ ptr, i64, i1 } " ^ #text key ^ ")")
          val _ = emit (slot ^ " = and i64 " ^ hash ^ ", " ^ mask)
          val _ = emit (slotSlot ^ " = alloca i64")
          val _ = emit ("store i64 " ^ slot ^ ", ptr " ^ slotSlot)
          val _ = emit ("br label %" ^ loopLabel)
          val _ = emit (loopLabel ^ ":")
          val _ = emit (index ^ " = load i64, ptr " ^ slotSlot)
          val _ = emit (entry ^ " = getelementptr " ^ entryTy valueTy ^
                        ", ptr " ^ data ^ ", i64 " ^ index)
          val _ = emit (state ^ " = load i32, ptr " ^ entry)
          val _ = emit (isEmpty ^ " = icmp eq i32 " ^ state ^ ", 0")
          val _ = emit ("br i1 " ^ isEmpty ^ ", label %" ^ missLabel ^
                        ", label %" ^ checkLabel)
          val _ = emit (checkLabel ^ ":")
          val _ = emit (isUsed ^ " = icmp eq i32 " ^ state ^ ", 1")
          val _ = emit ("br i1 " ^ isUsed ^ ", label %" ^ matchLabel ^
                        ", label %" ^ advanceLabel)
          val _ = emit (matchLabel ^ ":")
          val keyPointer = entryField (entry, valueTy, 1)
          val _ = emit (stored ^ " = load { ptr, i64, i1 }, ptr " ^
                        keyPointer)
          val compared = keyEquals (stored, #text key, keyDone)
          val _ = emit ("br i1 " ^ compared ^ ", label %" ^ foundLabel ^
                        ", label %" ^ advanceLabel)
          val _ = emit (advanceLabel ^ ":")
          val nextSum = fresh "map.probe.nextsum"
          val _ = emit (nextSum ^ " = add i64 " ^ index ^ ", 1")
          val _ = emit (nextIndex ^ " = and i64 " ^ nextSum ^ ", " ^ mask)
          val _ = emit ("store i64 " ^ nextIndex ^ ", ptr " ^ slotSlot)
          val _ = emit ("br label %" ^ loopLabel)
          val _ = emit (foundLabel ^ ":")
          val _ = onFound entry
          val _ = emit (missLabel ^ ":")
          val _ = onMiss entry
        in
          ()
        end

      fun dropConsumedKey (key, consume) =
        if consume then dropKey (#text key) else ()
    in
      case (name, lowered) of
        ("map_new", _) =>
          let
            val header = allocBytes "40"
            val _ = storeField { text = header, ty = resultTy
                               , const = false } 0 "ptr" "null"
            val _ = storeField { text = header, ty = resultTy
                               , const = false } 1 "i64" "0"
            val _ = storeField { text = header, ty = resultTy
                               , const = false } 2 "i64" "0"
            val _ = storeField { text = header, ty = resultTy
                               , const = false } 3 "i64" "0"
            val _ = storeField { text = header, ty = resultTy
                               , const = false } 4 "ptr" (ownerValue ())
          in
            { text = header, ty = resultTy, const = false }
          end
      | ("map_length", [(map, _)]) =>
          let
            val length = loadField map 1 "i64"
            val result = fresh "map.length"
            val _ = emit (result ^ " = trunc i64 " ^ length ^ " to i32")
          in
            { text = result, ty = Ir.Int 32, const = false }
          end
      | ("map_length_borrow", [(map, _)]) =>
          let
            val length = loadField map 1 "i64"
            val result = fresh "map.length"
            val _ = emit (result ^ " = trunc i64 " ^ length ^ " to i32")
          in
            { text = result, ty = Ir.Int 32, const = false }
          end
      | ("map_keys", [(map, _)]) =>
          let
            val valueTy = valueTypeOf map
            val mapData = loadField map 0 "ptr"
            val keyCount = loadField map 1 "i64"
            val mapCapacity = loadField map 3 "i64"
            val arrayHeaderTy = "{ ptr, i64, i64 }"
            val stringSize = fresh "map.keys.ssize"
            val _ =
              emit (stringSize ^ " = ptrtoint ptr getelementptr (" ^
                    storageTy Ir.TString ^ ", ptr null, i32 1) to i64")
            val small = fresh "map.keys.small"
            val capacity = fresh "map.keys.capacity"
            val _ = emit (small ^ " = icmp slt i64 " ^ keyCount ^ ", 4")
            val _ =
              emit (capacity ^ " = select i1 " ^ small ^
                    ", i64 4, i64 " ^ keyCount)
            val bytes = fresh "map.keys.bytes"
            val _ =
              emit (bytes ^ " = mul i64 " ^ capacity ^ ", " ^ stringSize)
            val entries = allocBytes bytes
            val header = allocBytes "32"
            val dataPtr = fresh "map.keys.datap"
            val lengthPtr = fresh "map.keys.lenp"
            val capacityPtr = fresh "map.keys.capp"
            val _ =
              emit (dataPtr ^ " = getelementptr " ^ arrayHeaderTy ^
                    ", ptr " ^ header ^ ", i32 0, i32 0")
            val _ = emit ("store ptr " ^ entries ^ ", ptr " ^ dataPtr)
            val _ =
              emit (lengthPtr ^ " = getelementptr " ^ arrayHeaderTy ^
                    ", ptr " ^ header ^ ", i32 0, i32 1")
            val _ = emit ("store i64 " ^ keyCount ^ ", ptr " ^ lengthPtr)
            val _ =
              emit (capacityPtr ^ " = getelementptr " ^ arrayHeaderTy ^
                    ", ptr " ^ header ^ ", i32 0, i32 2")
            val _ =
              emit ("store i64 " ^ capacity ^ ", ptr " ^ capacityPtr)
            val ownerPtr = fresh "map.keys.ownerp"
            val _ =
              emit (ownerPtr ^ " = getelementptr { ptr, i64, i64, ptr }" ^
                    ", ptr " ^ header ^ ", i32 0, i32 3")
            val _ =
              emit ("store ptr " ^ ownerValue () ^ ", ptr " ^ ownerPtr)
            val indexSlot = fresh "map.keys.indexslot"
            val outSlot = fresh "map.keys.outslot"
            val _ = emit (indexSlot ^ " = alloca i64")
            val _ = emit (outSlot ^ " = alloca i64")
            val _ = emit ("store i64 0, ptr " ^ indexSlot)
            val _ = emit ("store i64 0, ptr " ^ outSlot)
            val loopLabel = freshLabel "map.keys.loop"
            val bodyLabel = freshLabel "map.keys.body"
            val copyLabel = freshLabel "map.keys.copy"
            val skipLabel = freshLabel "map.keys.skip"
            val doneLabel = freshLabel "map.keys.done"
            val index = fresh "map.keys.index"
            val condition = fresh "map.keys.condition"
            val entry = fresh "map.keys.entry"
            val state = fresh "map.keys.state"
            val used = fresh "map.keys.used"
            val outIndex = fresh "map.keys.outindex"
            val destination = fresh "map.keys.destination"
            val nextIndex = fresh "map.keys.nextindex"
            val nextOut = fresh "map.keys.nextout"
            val _ = emit ("br label %" ^ loopLabel)
            val _ = emit (loopLabel ^ ":")
            val _ = emit (index ^ " = load i64, ptr " ^ indexSlot)
            val _ =
              emit (condition ^ " = icmp ult i64 " ^ index ^ ", " ^
                    mapCapacity)
            val _ =
              emit ("br i1 " ^ condition ^ ", label %" ^ bodyLabel ^
                    ", label %" ^ doneLabel)
            val _ = emit (bodyLabel ^ ":")
            val _ =
              emit (entry ^ " = getelementptr " ^ entryTy valueTy ^
                    ", ptr " ^ mapData ^ ", i64 " ^ index)
            val _ = emit (state ^ " = load i32, ptr " ^ entry)
            val _ = emit (used ^ " = icmp eq i32 " ^ state ^ ", 1")
            val _ =
              emit ("br i1 " ^ used ^ ", label %" ^ copyLabel ^
                    ", label %" ^ skipLabel)
            val _ = emit (copyLabel ^ ":")
            val storedKey = loadEntryKey (entry, valueTy)
            val copied =
              if not stackMode then
                let
                  val reg = fresh "map.keys.copied"
                  val _ =
                    emit (reg ^ " = call { ptr, i64, i1 } @yack_copy_string" ^
                          "({ ptr, i64, i1 } " ^ storedKey ^ ")")
                in
                  reg
                end
              else
                let
                  val keyData = fresh "map.keys.keydata"
                  val keyLen = fresh "map.keys.keylen"
                  val keySize = fresh "map.keys.keysize"
                  val keyMem = fresh "map.keys.keymem"
                  val keyEnd = fresh "map.keys.keyend"
                  val v0 = fresh "map.keys.keyv0"
                  val v1 = fresh "map.keys.keyv1"
                  val v2 = fresh "map.keys.keyv2"
                  val _ =
                    emit (keyData ^ " = extractvalue { ptr, i64, i1 } " ^
                          storedKey ^ ", 0")
                  val _ =
                    emit (keyLen ^ " = extractvalue { ptr, i64, i1 } " ^
                          storedKey ^ ", 1")
                  val _ = emit (keySize ^ " = add i64 " ^ keyLen ^ ", 1")
                  val _ =
                    emit (keyMem ^ " = call ptr @yack_string_alloc_in(ptr " ^
                          allocRegion () ^ ", i64 " ^ keySize ^ ")")
                  val _ =
                    emit ("call ptr @memcpy(ptr " ^ keyMem ^ ", ptr " ^
                          keyData ^ ", i64 " ^ keyLen ^ ")")
                  val _ =
                    emit (keyEnd ^ " = getelementptr i8, ptr " ^ keyMem ^
                          ", i64 " ^ keyLen)
                  val _ = emit ("store i8 0, ptr " ^ keyEnd)
                  val _ =
                    emit (v0 ^ " = insertvalue { ptr, i64, i1 } undef, ptr " ^
                          keyMem ^ ", 0")
                  val _ =
                    emit (v1 ^ " = insertvalue { ptr, i64, i1 } " ^ v0 ^
                          ", i64 " ^ keyLen ^ ", 1")
                  val _ =
                    emit (v2 ^ " = insertvalue { ptr, i64, i1 } " ^ v1 ^
                          ", i1 true, 2")
                in
                  v2
                end
            val _ = emit (outIndex ^ " = load i64, ptr " ^ outSlot)
            val _ =
              emit (destination ^ " = getelementptr " ^
                    storageTy Ir.TString ^ ", ptr " ^ entries ^
                    ", i64 " ^ outIndex)
            val _ =
              emit ("store { ptr, i64, i1 } " ^ copied ^ ", ptr " ^
                    destination)
            val _ = emit (nextOut ^ " = add i64 " ^ outIndex ^ ", 1")
            val _ = emit ("store i64 " ^ nextOut ^ ", ptr " ^ outSlot)
            val _ = emit ("br label %" ^ skipLabel)
            val _ = emit (skipLabel ^ ":")
            val _ = emit (nextIndex ^ " = add i64 " ^ index ^ ", 1")
            val _ = emit ("store i64 " ^ nextIndex ^ ", ptr " ^ indexSlot)
            val _ = emit ("br label %" ^ loopLabel)
            val _ = emit (doneLabel ^ ":")
          in
            { text = header, ty = resultTy, const = false }
          end
      | ("map_has", [(map, _), (key, consume)]) =>
          let
            val slot = allocStack Ir.Bool
            val doneLabel = freshLabel "map.has.done"
            val _ =
              emitProbe
                ( map, key, false
                , fn _ =>
                    ( emit ("store i1 true, ptr " ^ slot)
                    ; emit ("br label %" ^ doneLabel) )
                , fn _ =>
                    ( emit ("store i1 false, ptr " ^ slot)
                    ; emit ("br label %" ^ doneLabel) )
                )
            val _ = emit (doneLabel ^ ":")
            val result = fresh "map.has"
            val _ = emit (result ^ " = load i1, ptr " ^ slot)
            val _ = dropConsumedKey (key, consume)
          in
            { text = result, ty = Ir.Bool, const = false }
          end
      | ("map_get", [(map, _), (key, consume)]) =>
          let
            val valueTy = valueTypeOf map
            val slot = allocStack valueTy
            val doneLabel = freshLabel "map.get.done"
            val _ =
              emitProbe
                ( map, key, false
                , fn entry =>
                    let val value = loadEntryValue (entry, valueTy) in
                      ( if isUnit valueTy then ()
                        else
                          emit ("store " ^ storageTy valueTy ^ " " ^
                                #text value ^ ", ptr " ^ slot)
                      ; emit ("br label %" ^ doneLabel) )
                    end
                , fn _ =>
                    ( if isUnit valueTy then ()
                      else
                        emit ("store " ^ storageTy valueTy ^
                              " zeroinitializer, ptr " ^ slot)
                    ; emit ("br label %" ^ doneLabel) )
                )
            val _ = emit (doneLabel ^ ":")
            val result =
              if isUnit valueTy then unitValue
              else
                let val reg = fresh "map.get" in
                  emit (reg ^ " = load " ^ storageTy valueTy ^
                        ", ptr " ^ slot);
                  { text = reg, ty = valueTy, const = false }
                end
            val _ = dropConsumedKey (key, consume)
          in
            result
          end
      | ("map_get_copy_borrow", [(map, _), (key, consume)]) =>
          let
            val valueTy = valueTypeOf map
            val slot = allocStack valueTy
            val doneLabel = freshLabel "map.get.done"
            val _ =
              emitProbe
                ( map, key, false
                , fn entry =>
                    let val value = loadEntryValue (entry, valueTy) in
                      ( if isUnit valueTy then ()
                        else
                          emit ("store " ^ storageTy valueTy ^ " " ^
                                #text value ^ ", ptr " ^ slot)
                      ; emit ("br label %" ^ doneLabel) )
                    end
                , fn _ =>
                    ( if isUnit valueTy then ()
                      else
                        emit ("store " ^ storageTy valueTy ^
                              " zeroinitializer, ptr " ^ slot)
                    ; emit ("br label %" ^ doneLabel) )
                )
            val _ = emit (doneLabel ^ ":")
            val result =
              if isUnit valueTy then unitValue
              else
                let val reg = fresh "map.get" in
                  emit (reg ^ " = load " ^ storageTy valueTy ^
                        ", ptr " ^ slot);
                  { text = reg, ty = valueTy, const = false }
                end
            val _ = dropConsumedKey (key, consume)
          in
            result
          end
      | ("map_has_borrow", [(map, _), (key, consume)]) =>
          let
            val slot = allocStack Ir.Bool
            val doneLabel = freshLabel "map.has.done"
            val _ =
              emitProbe
                ( map, key, false
                , fn _ =>
                    ( emit ("store i1 true, ptr " ^ slot)
                    ; emit ("br label %" ^ doneLabel) )
                , fn _ =>
                    ( emit ("store i1 false, ptr " ^ slot)
                    ; emit ("br label %" ^ doneLabel) )
                )
            val _ = emit (doneLabel ^ ":")
            val result = fresh "map.has"
            val _ = emit (result ^ " = load i1, ptr " ^ slot)
            val _ = dropConsumedKey (key, consume)
          in
            { text = result, ty = Ir.Bool, const = false }
          end
      | ("map_get_borrow", [(map, _), (key, consume)]) =>
          let
            val valueTy = valueTypeOf map
            val slot = allocStack valueTy
            val doneLabel = freshLabel "map.borrow.done"
            val _ =
              emitProbe
                ( map, key, false
                , fn entry =>
                    let val value = loadEntryValue (entry, valueTy) in
                      ( if isUnit valueTy then ()
                        else
                          emit ("store " ^ storageTy valueTy ^ " " ^
                                #text value ^ ", ptr " ^ slot)
                      ; emit ("br label %" ^ doneLabel) )
                    end
                , fn _ =>
                    ( if isUnit valueTy then ()
                      else
                        emit ("store " ^ storageTy valueTy ^
                              " zeroinitializer, ptr " ^ slot)
                    ; emit ("br label %" ^ doneLabel) )
                )
            val _ = emit (doneLabel ^ ":")
            val result =
              if isUnit valueTy then unitValue
              else if resultTy = Ir.TString orelse
                      (case resultTy of
                         Ir.TBorrow (Ir.TString, _) => true
                       | _ => false)
              then
                let val reg = fresh "map.borrow" in
                  emit (reg ^ " = load " ^ storageTy valueTy ^
                        ", ptr " ^ slot);
                  { text = reg, ty = Ir.TString, const = false }
                end
              else if boxedBorrowType valueTy then
                let val reg = fresh "map.borrow" in
                  emit (reg ^ " = load " ^ storageTy valueTy ^
                        ", ptr " ^ slot);
                  { text = reg, ty = resultTy, const = false }
                end
              else
                { text = slot, ty = resultTy, const = false }
            val _ = dropConsumedKey (key, consume)
          in
            result
          end
      | ("map_get_borrow_view", [(map, _), (key, consume)]) =>
          let
            val valueTy = valueTypeOf map
            val slot = allocStack valueTy
            val doneLabel = freshLabel "map.borrow.done"
            val _ =
              emitProbe
                ( map, key, false
                , fn entry =>
                    let val value = loadEntryValue (entry, valueTy) in
                      ( if isUnit valueTy then ()
                        else
                          emit ("store " ^ storageTy valueTy ^ " " ^
                                #text value ^ ", ptr " ^ slot)
                      ; emit ("br label %" ^ doneLabel) )
                    end
                , fn _ =>
                    ( if isUnit valueTy then ()
                      else
                        emit ("store " ^ storageTy valueTy ^
                              " zeroinitializer, ptr " ^ slot)
                    ; emit ("br label %" ^ doneLabel) )
                )
            val _ = emit (doneLabel ^ ":")
            val result =
              if isUnit valueTy then unitValue
              else if resultTy = Ir.TString orelse
                      (case resultTy of
                         Ir.TBorrow (Ir.TString, _) => true
                       | _ => false)
              then
                let val reg = fresh "map.borrow" in
                  emit (reg ^ " = load " ^ storageTy valueTy ^
                        ", ptr " ^ slot);
                  { text = reg, ty = Ir.TString, const = false }
                end
              else if boxedBorrowType valueTy then
                let val reg = fresh "map.borrow" in
                  emit (reg ^ " = load " ^ storageTy valueTy ^
                        ", ptr " ^ slot);
                  { text = reg, ty = resultTy, const = false }
                end
              else
                { text = slot, ty = resultTy, const = false }
            val _ = dropConsumedKey (key, consume)
          in
            result
          end
      | ("map_remove", [(map, _), (key, consume)]) =>
          let
            val valueTy = valueTypeOf map
            val lengthPointer = fieldPointer map 1
            val doneLabel = freshLabel "map.remove.done"
            val _ =
              emitProbe
                ( map, key, false
                , fn entry =>
                    let
                      val storedKey = loadEntryKey (entry, valueTy)
                      val storedValue = loadEntryValue (entry, valueTy)
                      val _ = dropKey storedKey
                      val _ = dropValue (storedValue, valueTy)
                      val _ = storeEntryState (entry, valueTy, 2)
                      val length = fresh "map.remove.len"
                      val nextLength = fresh "map.remove.next"
                      val _ =
                        emit (length ^ " = load i64, ptr " ^
                              lengthPointer)
                      val _ =
                        emit (nextLength ^ " = sub i64 " ^ length ^ ", 1")
                      val _ = emit ("store i64 " ^ nextLength ^ ", ptr " ^
                                    lengthPointer)
                    in
                      emit ("br label %" ^ doneLabel)
                    end
                , fn _ => emit ("br label %" ^ doneLabel) )
            val _ = emit (doneLabel ^ ":")
            val _ = dropConsumedKey (key, consume)
          in
            unitValue
          end
      | ("map_set", [(map, _), (key, _), (value, _)]) =>
          let
            val valueTy = valueTypeOf map
            val doneLabel = freshLabel "map.set.done"
            val afterGrow = freshLabel "map.set.aftergrow"
            val growLabel = freshLabel "map.set.grow"
            val capacity = loadField map 3 "i64"
            val filled = loadField map 2 "i64"
            val isZero = fresh "map.set.zero"
            val increased = fresh "map.set.increased"
            val doubled = fresh "map.set.doubled"
            val full = fresh "map.set.full"
            val needGrow = fresh "map.set.needgrow"
            val _ = emit (isZero ^ " = icmp eq i64 " ^ capacity ^ ", 0")
            val _ = emit (increased ^ " = add i64 " ^ filled ^ ", 1")
            val _ = emit (doubled ^ " = mul i64 " ^ increased ^ ", 2")
            val _ = emit (full ^ " = icmp sge i64 " ^ doubled ^ ", " ^
                          capacity)
            val _ = emit (needGrow ^ " = or i1 " ^ isZero ^ ", " ^ full)
            val _ = emit ("br i1 " ^ needGrow ^ ", label %" ^ growLabel ^
                          ", label %" ^ afterGrow)
            val _ = emit (growLabel ^ ":")
            val _ = emitGrow (map, valueTy)
            val _ = emit ("br label %" ^ afterGrow)
            val _ = emit (afterGrow ^ ":")
            val lengthPointer = fieldPointer map 1
            val filledPointer = fieldPointer map 2
            val _ =
              emitProbe
                ( map, key, true
                , fn entry =>
                    let
                      val previous = loadEntryValue (entry, valueTy)
                      val _ = dropKey (#text key)
                      val _ = dropValue (previous, valueTy)
                      val _ = storeEntryValue (entry, valueTy, value)
                    in
                      emit ("br label %" ^ doneLabel)
                    end
                , fn entry =>
                    let
                      val length = fresh "map.set.len"
                      val nextLength = fresh "map.set.nextlen"
                      val filledNow = fresh "map.set.filled"
                      val nextFilled = fresh "map.set.nextfilled"
                      val _ = storeEntryState (entry, valueTy, 1)
                      val _ = storeEntryKey (entry, valueTy, #text key)
                      val _ = storeEntryValue (entry, valueTy, value)
                      val _ =
                        emit (length ^ " = load i64, ptr " ^ lengthPointer)
                      val _ =
                        emit (nextLength ^ " = add i64 " ^ length ^ ", 1")
                      val _ = emit ("store i64 " ^ nextLength ^ ", ptr " ^
                                    lengthPointer)
                      val _ =
                        emit (filledNow ^ " = load i64, ptr " ^
                              filledPointer)
                      val _ =
                        emit (nextFilled ^ " = add i64 " ^ filledNow ^
                              ", 1")
                      val _ = emit ("store i64 " ^ nextFilled ^ ", ptr " ^
                                    filledPointer)
                    in
                      emit ("br label %" ^ doneLabel)
                    end
                )
            val _ = emit (doneLabel ^ ":")
          in
            unitValue
          end
      | _ => unitValue
    end

end
