structure LlvmBuiltinArray =
struct
  open LlvmTypes

  type env =
    { emit : string -> unit
    , fresh : string -> string
    , freshLabel : string -> string
    , lowerExpr : localEntry list -> Ir.expr -> value
    , allocStack : Ir.ty -> string
    , currentRegionArg : string ref
    , currentDestArg : string ref
    , storeRoot : bool ref
    , rootValue : value * Ir.ty -> value
    , registerDirty : value * Ir.ty -> bool
    , mayContainString : Ir.ty -> bool
    , enterStoreRoot : unit -> unit
    , leaveStoreRoot : unit -> unit
    , stackMode : bool
    , inArena : unit -> bool
    , coerce : value * Ir.ty -> value
    , needsDrop : Ir.ty -> bool
    , dropValue : value * Ir.ty -> unit
    }

  fun lower (env : env) (locals, name, arguments, resultTy, siteRef) =
    let
      val emit = #emit env
      val fresh = #fresh env
      val freshLabel = #freshLabel env
      val lowerExpr = #lowerExpr env
      val allocStack = #allocStack env
      val currentRegionArg = #currentRegionArg env
      val currentDestArg = #currentDestArg env
      val storeRoot = #storeRoot env
      val rootValue = #rootValue env
      val registerDirty = #registerDirty env
      val mayContainString = #mayContainString env
      val enterStoreRoot = #enterStoreRoot env
      val leaveStoreRoot = #leaveStoreRoot env
      val stackMode = #stackMode env
      val inArena = #inArena env
      val coerce = #coerce env
      val needsDrop = #needsDrop env
      val dropValue = #dropValue env
      val unitValue = { text = "", ty = Ir.Unit, const = true }
      val headerTy = "{ ptr, i64, i64, ptr }"
      val rootRegion = "@yack_root_region"

      fun ownerOf arrayValue =
        let
          val ownerPointer = fresh "arr.ownerp"
          val _ =
            emit (ownerPointer ^
                  " = getelementptr { ptr, i64, i64, ptr }, ptr " ^
                  #text arrayValue ^ ", i32 0, i32 3")
          val owner = fresh "arr.owner"
          val _ = emit (owner ^ " = load ptr, ptr " ^ ownerPointer)
        in
          owner
        end

      fun isDataType ty =
        case ty of
          Ir.TData (_, _, _) => true
        | _ => false

      fun dataNameOf ty =
        case ty of
          Ir.TData (name, _, _) => name
        | _ => ""

      fun allocRegion () =
        case !siteRef of
          Ir.Current => !currentRegionArg
        | Ir.Destination => !currentDestArg
        | Ir.NativeStack => !currentDestArg

      fun allocInRegion region size =
        let val reg = fresh "arr.alloc" in
          if stackMode then
            emit (reg ^ " = call ptr @yack_region_alloc(ptr " ^
                  region ^ ", i64 " ^ size ^ ")")
          else
            emit (reg ^ " = call ptr @yack_root_alloc(i64 " ^ size ^ ")");
          reg
        end

      fun allocDirect region size =
        let val reg = fresh "arr.alloc.direct" in
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

      fun allocationOwner arrayValue =
        if stackMode andalso inArena () then "@yack_root_region"
        else ownerOf arrayValue

      fun lowerInOwner (arrayValue, argument) =
        let
          val owner = allocationOwner arrayValue
          val elementTy =
            case #ty arrayValue of
              Ir.TArray ty => ty
            | Ir.TBorrow (Ir.TArray ty, _) => ty
            | _ => Ir.Unit
          val needsRoot = mayContainString elementTy
          val previous = !currentDestArg
          val previousRoot = !storeRoot
          val _ = currentDestArg := owner
          val _ = storeRoot := needsRoot
          val _ = if needsRoot then enterStoreRoot () else ()
          val value = lowerExpr locals argument
          val _ = if needsRoot then leaveStoreRoot () else ()
          val _ = storeRoot := previousRoot
          val _ = currentDestArg := previous
        in
          value
        end

      fun isPush name =
        name = "array_push" orelse name = "array_push_unchecked"

      fun isSet name =
        name = "array_set" orelse name = "array_set_unchecked" orelse
        name = "array_kernel_write"

      fun lowerArguments () =
        case arguments of
          [valueArgument, countArgument] =>
            if name = "array_make" then
              let
                val elementTy =
                  case resultTy of
                    Ir.TArray ty => ty
                  | _ => Ir.Unit
                val previous = !currentDestArg
                val previousRoot = !storeRoot
                val needsRoot = mayContainString elementTy
                val _ =
                  if stackMode then
                    currentDestArg :=
                      (if inArena () then "@yack_root_region"
                       else allocRegion ())
                  else ()
                val _ = storeRoot := needsRoot
                val _ = if needsRoot then enterStoreRoot () else ()
                val value = lowerExpr locals valueArgument
                val _ = if needsRoot then leaveStoreRoot () else ()
                val _ = storeRoot := previousRoot
                val value = rootValue (value, elementTy)
                val _ = currentDestArg := previous
                val count = lowerExpr locals countArgument
              in
                [value, count]
              end
            else if isPush name then
              let val arrayValue = lowerExpr locals valueArgument in
                [arrayValue, lowerInOwner (arrayValue, countArgument)]
              end
            else
              List.map (lowerExpr locals) arguments
        | [arrayArgument, indexArgument, valueArgument] =>
            if isSet name then
              let
                val arrayValue = lowerExpr locals arrayArgument
                val indexValue = lowerExpr locals indexArgument
              in
                [ arrayValue
                , indexValue
                , lowerInOwner (arrayValue, valueArgument)
                ]
              end
            else
              List.map (lowerExpr locals) arguments
        | _ => List.map (lowerExpr locals) arguments

      val lowered = lowerArguments ()

      fun elementTypeOf value =
        case #ty value of
          Ir.TArray elementTy => elementTy
        | Ir.TBorrow (Ir.TArray elementTy, _) => elementTy
        | _ => Ir.Unit

      fun fieldPointer value index =
        let val pointer = fresh "arr.fieldp" in
          emit (pointer ^ " = getelementptr " ^ headerTy ^ ", ptr " ^
                #text value ^ ", i32 0, i32 " ^ Int.toString index);
          pointer
        end

      fun loadField value index storage =
        let
          val pointer = fieldPointer value index
          val reg = fresh "arr.field"
        in
          emit (reg ^ " = load " ^ storage ^ ", ptr " ^ pointer);
          reg
        end

      fun elementSize elementTy =
        let val reg = fresh "arr.esize" in
          emit (reg ^ " = ptrtoint ptr getelementptr (" ^
                storageTy elementTy ^ ", ptr null, i32 1) to i64");
          reg
        end

      fun index64 index =
        let val reg = fresh "arr.index" in
          emit (reg ^ " = sext i32 " ^ #text index ^ " to i64");
          reg
        end

      fun storeElement (data, index, value, elementTy) =
        if isUnit elementTy then ()
        else
          let
            val pointer = fresh "arr.elementp"
            val _ = emit (pointer ^ " = getelementptr " ^
                          storageTy elementTy ^ ", ptr " ^ data ^
                          ", i64 " ^ index)
            val _ = emit ("store " ^ storageTy elementTy ^ " " ^
                          #text (coerce (value, elementTy)) ^
                          ", ptr " ^ pointer)
          in
            ()
          end

      fun loadElement (data, index, elementTy) =
        if isUnit elementTy then unitValue
        else
          let
            val pointer = fresh "arr.elementp"
            val reg = fresh "arr.element"
            val _ = emit (pointer ^ " = getelementptr " ^
                          storageTy elementTy ^ ", ptr " ^ data ^
                          ", i64 " ^ index)
            val _ = emit (reg ^ " = load " ^ storageTy elementTy ^
                          ", ptr " ^ pointer)
          in
            { text = reg, ty = elementTy, const = false }
          end

      fun abortUnlessBelowAfter (indexValue, limit, beforeAbort) =
        let
          val outside = fresh "arr.outside"
          val abortLabel = freshLabel "arr.oob"
          val okLabel = freshLabel "arr.inbounds"
        in
          emit (outside ^ " = icmp uge i64 " ^ indexValue ^ ", " ^ limit);
          emit ("br i1 " ^ outside ^ ", label %" ^ abortLabel ^
                ", label %" ^ okLabel ^ ", !prof !{!\"branch_weights\", i32 1, i32 1048575}");
          emit (abortLabel ^ ":");
          beforeAbort ();
          emit ("call void @yack_abort_bounds(i64 " ^ indexValue ^ ", i64 " ^
                limit ^ ")");
          emit "unreachable";
          emit (okLabel ^ ":")
        end

      fun abortUnlessBelow (indexValue, limit) =
        abortUnlessBelowAfter (indexValue, limit, fn () => ())

      fun holePath (Ir.TData _) = SOME []
        | holePath (Ir.TArray _) = SOME []
        | holePath (Ir.TMap _) = SOME []
        | holePath (Ir.TFun _) = SOME []
        | holePath (Ir.TTuple types) = firstHole (types, 0)
        | holePath (Ir.TRecord (fields, _)) =
            firstHole (List.map #2 fields, 0)
        | holePath _ = NONE
      and firstHole ([], _) = NONE
        | firstHole (ty :: rest, index) =
            (case holePath ty of
               SOME path => SOME (index :: path)
             | NONE => firstHole (rest, index + 1))

      fun loadLive (data, indexValue, elementTy) =
        let
          val element = loadElement (data, indexValue, elementTy)
        in
          case holePath elementTy of
            NONE => element
          | SOME path =>
              let
                val pointer =
                  if List.null path then #text element
                  else
                    let val reg = fresh "arr.holeptr" in
                      emit (reg ^ " = extractvalue " ^
                            storageTy elementTy ^ " " ^ #text element ^
                            String.concat
                              (List.map (fn i => ", " ^ Int.toString i)
                                 path));
                      reg
                    end
                val isHole = fresh "arr.hole"
                val holeLabel = freshLabel "arr.taken"
                val liveLabel = freshLabel "arr.live"
              in
                emit (isHole ^ " = icmp eq ptr " ^ pointer ^ ", null");
                emit ("br i1 " ^ isHole ^ ", label %" ^ holeLabel ^
                      ", label %" ^ liveLabel ^
                      ", !prof !{!\"branch_weights\", i32 1, i32 1048575}");
                emit (holeLabel ^ ":");
                emit ("call void @yack_abort_taken(i64 " ^ indexValue ^ ")");
                emit "unreachable";
                emit (liveLabel ^ ":");
                element
              end
        end

      fun guardedIndex (array, index) =
        let
          val data = loadField array 0 "ptr"
          val length = loadField array 1 "i64"
          val indexValue = index64 index
        in
          abortUnlessBelow (indexValue, length);
          (data, indexValue)
        end

      fun lowerLength array =
        let
          val length = loadField array 1 "i64"
          val length32 = fresh "arr.length"
          val _ = emit (length32 ^ " = trunc i64 " ^ length ^ " to i32")
        in
          { text = length32, ty = Ir.Int 32, const = false }
        end

      fun lowerGet array index _ =
        let
          val elementTy = elementTypeOf array
          val (data, indexValue) = guardedIndex (array, index)
        in
          loadLive (data, indexValue, elementTy)
        end

      fun lowerBorrowGet (array, index, resultTy) =
        let
          val elementTy = elementTypeOf array
        in
          if isUnit elementTy then
            ( ignore (guardedIndex (array, index)); unitValue )
          else
            case resultTy of
              Ir.TString => lowerGet array index true
            | Ir.TBorrow (Ir.TString, _) => lowerGet array index true
            | _ =>
                let
                  val (data, indexValue) = guardedIndex (array, index)
                  val element = loadLive (data, indexValue, elementTy)
                in
                  if boxedBorrowType elementTy then
                    { text = #text element, ty = resultTy, const = false }
                  else

                    let
                      val slot = fresh "arr.borrow.elementp"
                      val _ =
                        emit (slot ^ " = getelementptr " ^
                              storageTy elementTy ^ ", ptr " ^ data ^
                              ", i64 " ^ indexValue)
                    in
                      { text = slot, ty = resultTy, const = false }
                    end
                end
        end

      fun lowerTake array index =
        let
          val elementTy = elementTypeOf array
          val (data, indexValue) = guardedIndex (array, index)
        in
          if isUnit elementTy then unitValue
          else
            let
              val element = loadLive (data, indexValue, elementTy)
              val pointer = fresh "arr.take.elementp"
              val _ = emit (pointer ^ " = getelementptr " ^
                            storageTy elementTy ^ ", ptr " ^ data ^
                            ", i64 " ^ indexValue)
              val _ = emit ("store " ^ storageTy elementTy ^
                            " zeroinitializer, ptr " ^ pointer)
            in
              element
            end
        end

      fun lowerKernelDrop array index =
        let
          val elementTy = elementTypeOf array
          val data = loadField array 0 "ptr"
          val indexValue = index64 index
          val element = loadElement (data, indexValue, elementTy)
          val _ =
            if stackMode andalso elementTy = Ir.TString then
              emit ("call void @yack_owner_recycle_string(ptr " ^
                    ownerOf array ^ ", { ptr, i64, i1 } " ^
                    #text element ^ ")")
            else if stackMode andalso isDataType elementTy then
              emit ("call void @yack_owner_drop_" ^ dataNameOf elementTy ^
                    "(ptr " ^ ownerOf array ^ ", ptr " ^ #text element ^ ")")
            else if needsDrop elementTy then
              dropValue (element, elementTy)
            else ()
          val pointer = fresh "arr.drop.elementp"
          val _ = emit (pointer ^ " = getelementptr " ^
                        storageTy elementTy ^ ", ptr " ^ data ^
                        ", i64 " ^ indexValue)
          val _ = emit ("store " ^ storageTy elementTy ^
                        " zeroinitializer, ptr " ^ pointer)
        in
          unitValue
        end

      fun dropRejected (value, elementTy) () =
        if needsDrop elementTy then dropValue (value, elementTy) else ()

      fun lowerSet array index value _ =
        let
          val elementTy = elementTypeOf array
          val value = rootValue (value, elementTy)
          val data = loadField array 0 "ptr"
          val length = loadField array 1 "i64"
          val indexValue = index64 index
          val _ = abortUnlessBelowAfter
                    (indexValue, length, dropRejected (value, elementTy))
          val _ =
            if not (needsDrop elementTy) then ()
            else
              let val old = loadElement (data, indexValue, elementTy) in
                if stackMode andalso elementTy = Ir.TString then

                  emit ("call void @yack_owner_recycle_string(ptr " ^
                        ownerOf array ^
                        ", { ptr, i64, i1 } " ^ #text old ^ ")")
                else if stackMode andalso isDataType elementTy then

                  emit ("call void @yack_owner_drop_" ^
                        dataNameOf elementTy ^ "(ptr " ^ ownerOf array ^
                        ", ptr " ^ #text old ^ ")")
                else
                  dropValue (old, elementTy)
              end
        in
          storeElement (data, indexValue, value, elementTy);
          unitValue
        end

      fun lowerPut array index value rooted =
        let
          val elementTy = elementTypeOf array
          val value =
            if rooted then rootValue (value, elementTy)
            else if registerDirty (array, #ty array) then value
            else rootValue (value, elementTy)
          val data = loadField array 0 "ptr"
          val length = loadField array 1 "i64"
          val indexValue = index64 index
          val _ = abortUnlessBelowAfter
                    (indexValue, length, dropRejected (value, elementTy))
        in
          storeElement (data, indexValue, value, elementTy);
          unitValue
        end

      fun lowerPush array value checked =
        let
          val elementTy = elementTypeOf array
          val value = rootValue (value, elementTy)
          val dataPointer = fieldPointer array 0
          val lengthPointer = fieldPointer array 1
          val capacityPointer = fieldPointer array 2
          val data0 = fresh "arr.push.data"
          val length = fresh "arr.push.len"
          val capacity = fresh "arr.push.cap"
          val dataSlot = fresh "arr.push.dataslot"
          val grow = freshLabel "arr.push.grow"
          val storeLabel = freshLabel "arr.push.store"
          val doneLabel = freshLabel "arr.push.done"
          val _ = emit (data0 ^ " = load ptr, ptr " ^ dataPointer)
          val _ = emit (length ^ " = load i64, ptr " ^ lengthPointer)
          val _ = emit (capacity ^ " = load i64, ptr " ^ capacityPointer)
          val _ = emit (dataSlot ^ " = alloca ptr")
          val _ = emit ("store ptr " ^ data0 ^ ", ptr " ^ dataSlot)
          val _ =
            if checked then
              let
                val full = fresh "arr.push.full"
                val _ = emit (full ^ " = icmp sge i64 " ^ length ^
                              ", " ^ capacity)
                val _ = emit ("br i1 " ^ full ^ ", label %" ^ grow ^
                              ", label %" ^ storeLabel)
              in
                ()
              end
            else
              ( abortUnlessBelow (length, capacity)
              ; emit ("br label %" ^ storeLabel)
              )
          val _ = emit (grow ^ ":")
          val isZero = fresh "arr.push.zero"
          val doubled = fresh "arr.push.doubled"
          val nextCapacity = fresh "arr.push.nextcap"
          val size = elementSize elementTy
          val bytes = fresh "arr.push.bytes"
          val oldBytes = fresh "arr.push.oldbytes"
          val grown = fresh "arr.push.grown"
          val _ = emit (isZero ^ " = icmp eq i64 " ^ capacity ^ ", 0")
          val _ = emit (doubled ^ " = shl i64 " ^ capacity ^ ", 1")
          val _ = emit (nextCapacity ^ " = select i1 " ^ isZero ^
                        ", i64 4, i64 " ^ doubled)
          val _ = emit (bytes ^ " = mul i64 " ^ nextCapacity ^ ", " ^ size)
          val _ = emit (oldBytes ^ " = mul i64 " ^ capacity ^ ", " ^ size)
          val _ =
            if stackMode then
              ( emit (grown ^ " = call ptr @yack_region_alloc_direct(ptr " ^
                      allocationOwner array ^ ", i64 " ^ bytes ^ ")")
              ; emit ("call void @memcpy(ptr " ^ grown ^ ", ptr " ^ data0 ^
                      ", i64 " ^ oldBytes ^ ")")
              ; emit ("call void @yack_owner_recycle_block(ptr " ^
                      allocationOwner array ^ ", ptr " ^ data0 ^ ", i64 " ^
                      oldBytes ^ ")")
              )
            else
              emit (grown ^ " = call ptr @yack_root_realloc(ptr " ^ data0 ^
                    ", i64 " ^ oldBytes ^ ", i64 " ^ bytes ^ ")")
          val _ = emit ("store ptr " ^ grown ^ ", ptr " ^ dataSlot)
          val _ = emit ("store ptr " ^ grown ^ ", ptr " ^ dataPointer)
          val _ = emit ("store i64 " ^ nextCapacity ^ ", ptr " ^
                        capacityPointer)
          val _ = emit ("br label %" ^ storeLabel)
          val _ = emit (storeLabel ^ ":")
          val data = fresh "arr.push.finaldata"
          val _ = emit (data ^ " = load ptr, ptr " ^ dataSlot)
          val _ = storeElement (data, length, value, elementTy)
          val nextLength = fresh "arr.push.nextlen"
          val _ = emit (nextLength ^ " = add i64 " ^ length ^ ", 1")
          val _ = emit ("store i64 " ^ nextLength ^ ", ptr " ^ lengthPointer)
          val _ = emit ("br label %" ^ doneLabel)
          val _ = emit (doneLabel ^ ":")
        in
          unitValue
        end

      fun kernelField array fieldIndex =
        let
          val raw = loadField array fieldIndex "i64"
          val truncated = fresh "arr.kernel.field"
          val _ = emit (truncated ^ " = trunc i64 " ^ raw ^ " to i32")
        in
          { text = truncated, ty = Ir.Int 32, const = false }
        end

      fun kernelElementType array =
        case #ty array of
          Ir.TArray elementTy => elementTy
        | Ir.TBorrow (Ir.TArray elementTy, _) => elementTy
        | _ => Ir.Unit

      fun lowerKernelNew capacity =
        let
          val elementTy =
            case resultTy of
              Ir.TArray ty => ty
              | _ => Ir.Unit
          val capacity64 = index64 capacity
          val negative = fresh "arr.kernel.negative"
          val safeCapacity = fresh "arr.kernel.capacity"
          val size = elementSize elementTy
          val bytes = fresh "arr.kernel.bytes"
          val dataSlot = fresh "arr.kernel.dataslot"
          val data = fresh "arr.kernel.data"
          val header = allocBytes "32"
          val emptyLabel = freshLabel "arr.kernel.nodata"
          val initLabel = freshLabel "arr.kernel.data"
          val doneLabel = freshLabel "arr.kernel.donedata"
          fun storeHeaderField (index, storage, value) =
            let val pointer = fieldPointer { text = header, ty = resultTy
                                           , const = false } index in
              emit ("store " ^ storage ^ " " ^ value ^ ", ptr " ^ pointer)
            end
          val _ = emit (dataSlot ^ " = alloca ptr")
          val _ = emit (negative ^ " = icmp sle i64 " ^ capacity64 ^ ", 0")
          val _ = emit (safeCapacity ^ " = select i1 " ^ negative ^
                        ", i64 0, i64 " ^ capacity64)
          val _ = emit (bytes ^ " = mul i64 " ^ safeCapacity ^ ", " ^ size)
          val _ = emit ("br i1 " ^ negative ^ ", label %" ^ emptyLabel ^
                        ", label %" ^ initLabel)
          val _ = emit (emptyLabel ^ ":")
          val _ = emit ("store ptr null, ptr " ^ dataSlot)
          val _ = emit ("br label %" ^ doneLabel)
          val _ = emit (initLabel ^ ":")
          val allocated = allocBytes bytes
          val _ = emit ("store ptr " ^ allocated ^ ", ptr " ^ dataSlot)
          val _ = emit ("br label %" ^ doneLabel)
          val _ = emit (doneLabel ^ ":")
          val _ = emit (data ^ " = load ptr, ptr " ^ dataSlot)
          val _ = storeHeaderField (0, "ptr", data)
          val _ = storeHeaderField (1, "i64", "0")
          val _ = storeHeaderField (2, "i64", safeCapacity)
          val _ = storeHeaderField (3, "ptr", ownerValue ())
        in
          { text = header, ty = resultTy, const = false }
        end

      fun lowerKernelReserve (array, capacity) =
        let
          val elementTy = kernelElementType array
          val size = elementSize elementTy
          val wanted = index64 capacity
          val dataPointer = fieldPointer array 0
          val capacityPointer = fieldPointer array 2
          val data0 = fresh "arr.reserve.data"
          val oldCapacity = fresh "arr.reserve.cap"
          val enough = fresh "arr.reserve.enough"
          val bytes = fresh "arr.reserve.bytes"
          val oldBytes = fresh "arr.reserve.oldbytes"
          val copiedBytes = fresh "arr.reserve.copied"
          val shrinks = fresh "arr.reserve.shrinks"
          val grown = fresh "arr.reserve.grown"
          val growLabel = freshLabel "arr.reserve.grow"
          val doneLabel = freshLabel "arr.reserve.done"
          val _ = emit (data0 ^ " = load ptr, ptr " ^ dataPointer)
          val _ = emit (oldCapacity ^ " = load i64, ptr " ^ capacityPointer)
          val _ = emit (enough ^ " = icmp sle i64 " ^ wanted ^ ", " ^ oldCapacity)
          val _ = emit ("br i1 " ^ enough ^ ", label %" ^ doneLabel ^
                        ", label %" ^ growLabel)
          val _ = emit (growLabel ^ ":")
          val _ = emit (bytes ^ " = mul i64 " ^ wanted ^ ", " ^ size)
          val _ = emit (oldBytes ^ " = mul i64 " ^ oldCapacity ^ ", " ^ size)
          val _ = emit (shrinks ^ " = icmp slt i64 " ^ oldBytes ^ ", " ^ bytes)
          val _ = emit (copiedBytes ^ " = select i1 " ^ shrinks ^ ", i64 " ^
                        oldBytes ^ ", i64 " ^ bytes)
          val _ =
            if stackMode then
              ( emit (grown ^ " = call ptr @yack_region_alloc_direct(ptr " ^
                      allocationOwner array ^ ", i64 " ^ bytes ^ ")")
              ; emit ("call void @memcpy(ptr " ^ grown ^ ", ptr " ^ data0 ^
                      ", i64 " ^ copiedBytes ^ ")")
              ; emit ("call void @yack_owner_recycle_block(ptr " ^
                      allocationOwner array ^ ", ptr " ^ data0 ^ ", i64 " ^
                      oldBytes ^ ")")
              )
            else
              emit (grown ^ " = call ptr @yack_root_realloc(ptr " ^ data0 ^
                    ", i64 " ^ oldBytes ^ ", i64 " ^ bytes ^ ")")
          val _ = emit ("store ptr " ^ grown ^ ", ptr " ^ dataPointer)
          val _ = emit ("store i64 " ^ wanted ^ ", ptr " ^ capacityPointer)
          val _ = emit ("br label %" ^ doneLabel)
          val _ = emit (doneLabel ^ ":")
        in
          unitValue
        end

      fun lowerKernelSetLength (array, length) =
        let
          val pointer = fieldPointer array 1
          val value = index64 length
        in
          emit ("store i64 " ^ value ^ ", ptr " ^ pointer);
          unitValue
        end

      fun lowerKernelLoad (array, index) =
        let
          val elementTy = kernelElementType array
          val data = loadField array 0 "ptr"
          val indexValue = index64 index
        in
          loadLive (data, indexValue, elementTy)
        end

      fun lowerKernelBorrow (array, index, resultTy) =
        let
          val elementTy = kernelElementType array
        in
          case resultTy of
            Ir.TString => lowerKernelLoad (array, index)
          | Ir.TBorrow (Ir.TString, _) => lowerKernelLoad (array, index)
          | _ =>
              let
                val data = loadField array 0 "ptr"
                val indexValue = index64 index
                val element = loadLive (data, indexValue, elementTy)
              in
                if boxedBorrowType elementTy then
                  { text = #text element, ty = resultTy, const = false }
                else

                  let
                    val slot = fresh "arr.kernel.borrowp"
                    val _ =
                      emit (slot ^ " = getelementptr " ^
                            storageTy elementTy ^ ", ptr " ^ data ^
                            ", i64 " ^ indexValue)
                  in
                    { text = slot, ty = resultTy, const = false }
                  end
              end
        end

      fun lowerKernelTake (array, index) =
        let
          val elementTy = kernelElementType array
          val data = loadField array 0 "ptr"
          val indexValue = index64 index
        in
          if isUnit elementTy then unitValue
          else
            let
              val element = loadLive (data, indexValue, elementTy)
              val pointer = fresh "arr.kernel.takep"
              val _ = emit (pointer ^ " = getelementptr " ^
                            storageTy elementTy ^ ", ptr " ^ data ^
                            ", i64 " ^ indexValue)
              val _ = emit ("store " ^ storageTy elementTy ^
                            " zeroinitializer, ptr " ^ pointer)
            in
              element
            end
        end

      fun lowerKernelWrite (array, index, value) =
        let
          val elementTy = kernelElementType array
          val value = rootValue (value, elementTy)
          val data = loadField array 0 "ptr"
          val indexValue = index64 index
        in
          storeElement (data, indexValue, value, elementTy);
          unitValue
        end

      fun lowerKernelAbort (index, length) =
        let
          val indexValue = index64 index
          val lengthValue = index64 length
        in
          emit ("call void @yack_abort_bounds(i64 " ^ indexValue ^ ", i64 " ^
                lengthValue ^ ")");
          unitValue
        end
    in
      case (name, lowered) of
        ("array_new", _) =>
          let
            val header = allocBytes "32"
            val dataPointer = fieldPointer { text = header, ty = resultTy
                                           , const = false } 0
            val _ = emit ("store ptr null, ptr " ^ dataPointer)
            val lengthPointer = fieldPointer { text = header, ty = resultTy
                                             , const = false } 1
            val _ = emit ("store i64 0, ptr " ^ lengthPointer)
            val capacityPointer = fieldPointer { text = header, ty = resultTy
                                               , const = false } 2
            val _ = emit ("store i64 0, ptr " ^ capacityPointer)
            val ownerPointer = fieldPointer { text = header, ty = resultTy
                                            , const = false } 3
            val _ = emit ("store ptr " ^ ownerValue () ^
                          ", ptr " ^ ownerPointer)
          in
            { text = header, ty = resultTy, const = false }
          end
        | ("array_make", [value, count]) =>
          let
            val elementTy =
              case resultTy of
                Ir.TArray elementTy => elementTy
                | _ => Ir.Unit
            val count64 = index64 count
            val negative = fresh "arr.make.negative"
            val safeCount = fresh "arr.make.count"
            val small = fresh "arr.make.small"
            val capacity = fresh "arr.make.capacity"
            val size = elementSize elementTy
            val bytes = fresh "arr.make.bytes"
            val header = fresh "arr.make.header"
            val indexSlot = fresh "arr.make.indexslot"
            val index = fresh "arr.make.index"
            val condition = fresh "arr.make.condition"
            val next = fresh "arr.make.next"
            val loopLabel = freshLabel "arr.make.loop"
            val bodyLabel = freshLabel "arr.make.body"
            val doneLabel = freshLabel "arr.make.done"
            val _ = emit (negative ^ " = icmp slt i64 " ^ count64 ^ ", 0")
            val _ = emit (safeCount ^ " = select i1 " ^ negative ^
                          ", i64 0, i64 " ^ count64)
            val _ = emit (small ^ " = icmp slt i64 " ^ safeCount ^ ", 4")
            val _ = emit (capacity ^ " = select i1 " ^ small ^
                          ", i64 4, i64 " ^ safeCount)
            val _ = emit (bytes ^ " = mul i64 " ^ capacity ^ ", " ^ size)
            val data = allocBytes bytes
            val header = allocBytes "32"
            val dataPointer = fieldPointer { text = header, ty = resultTy
                                           , const = false } 0
            val _ = emit ("store ptr " ^ data ^ ", ptr " ^ dataPointer)
            val lengthPointer = fieldPointer { text = header, ty = resultTy
                                             , const = false } 1
            val _ = emit ("store i64 " ^ safeCount ^ ", ptr " ^
                          lengthPointer)
            val capacityPointer = fieldPointer { text = header, ty = resultTy
                                               , const = false } 2
            val _ = emit ("store i64 " ^ capacity ^ ", ptr " ^
                          capacityPointer)
            val ownerPointer = fieldPointer { text = header, ty = resultTy
                                            , const = false } 3
            val _ = emit ("store ptr " ^ ownerValue () ^
                          ", ptr " ^ ownerPointer)
            val _ = emit (indexSlot ^ " = alloca i64")
            val _ = emit ("store i64 0, ptr " ^ indexSlot)
            val _ = emit ("br label %" ^ loopLabel)
            val _ = emit (loopLabel ^ ":")
            val _ = emit (index ^ " = load i64, ptr " ^ indexSlot)
            val _ = emit (condition ^ " = icmp slt i64 " ^ index ^ ", " ^
                          safeCount)
            val _ = emit ("br i1 " ^ condition ^ ", label %" ^ bodyLabel ^
                          ", label %" ^ doneLabel)
            val _ = emit (bodyLabel ^ ":")
            val _ = storeElement (data, index, value, elementTy)
            val _ = emit (next ^ " = add i64 " ^ index ^ ", 1")
            val _ = emit ("store i64 " ^ next ^ ", ptr " ^ indexSlot)
            val _ = emit ("br label %" ^ loopLabel)
            val _ = emit (doneLabel ^ ":")
          in
            { text = header, ty = resultTy, const = false }
          end
      | ("array_make_uninit", [count]) =>
          let
            val elementTy =
              case resultTy of
                Ir.TArray elementTy => elementTy
                | _ => Ir.Unit
            val count64 = index64 count
            val negative = fresh "arr.uninit.negative"
            val safeCount = fresh "arr.uninit.count"
            val small = fresh "arr.uninit.small"
            val capacity = fresh "arr.uninit.capacity"
            val size = elementSize elementTy
            val bytes = fresh "arr.uninit.bytes"
            val header = fresh "arr.uninit.header"
            val _ = emit (negative ^ " = icmp slt i64 " ^ count64 ^ ", 0")
            val _ = emit (safeCount ^ " = select i1 " ^ negative ^
                          ", i64 0, i64 " ^ count64)
            val _ = emit (small ^ " = icmp slt i64 " ^ safeCount ^ ", 4")
            val _ = emit (capacity ^ " = select i1 " ^ small ^
                          ", i64 4, i64 " ^ safeCount)
            val _ = emit (bytes ^ " = mul i64 " ^ capacity ^ ", " ^ size)
            val data = allocBytes bytes

            val noHuge = fresh "arr.uninit.nohuge"
            val noHugeAddr = fresh "arr.uninit.nohuge.addr"
            val noHugeEnd = fresh "arr.uninit.nohuge.end"
            val noHugePage = fresh "arr.uninit.nohuge.page"
            val noHugeStart = fresh "arr.uninit.nohuge.start"
            val noHugeStop = fresh "arr.uninit.nohuge.stop"
            val noHugeSize = fresh "arr.uninit.nohuge.size"
            val noHugeEmpty = fresh "arr.uninit.nohuge.empty"
            val noHugePtr = fresh "arr.uninit.nohuge.ptr"
            val noHugeDo = freshLabel "arr.uninit.nohuge.do"
            val noHugeSkip = freshLabel "arr.uninit.nohuge.skip"
            val _ = emit (noHugeAddr ^ " = ptrtoint ptr " ^ data ^
                          " to i64")
            val _ = emit (noHugeEnd ^ " = add i64 " ^ noHugeAddr ^
                          ", " ^ bytes)
            val _ = emit (noHugePage ^ " = add i64 " ^ noHugeAddr ^
                          ", 4095")
            val _ = emit (noHugeStart ^ " = and i64 " ^ noHugePage ^
                          ", -4096")
            val _ = emit (noHugeStop ^ " = and i64 " ^ noHugeEnd ^
                          ", -4096")
            val _ = emit (noHugeSize ^ " = sub i64 " ^ noHugeStop ^
                          ", " ^ noHugeStart)
            val _ = emit (noHugeEmpty ^ " = icmp uge i64 " ^ noHugeStart ^
                          ", " ^ noHugeStop)
            val _ = emit ("br i1 " ^ noHugeEmpty ^ ", label %" ^
                          noHugeSkip ^ ", label %" ^ noHugeDo)
            val _ = emit (noHugeDo ^ ":")
            val _ = emit (noHugePtr ^ " = inttoptr i64 " ^ noHugeStart ^
                          " to ptr")
            val _ = emit (noHuge ^ " = call i32 @madvise(ptr " ^ noHugePtr ^
                          ", i64 " ^ noHugeSize ^ ", i32 15)")
            val _ = emit ("br label %" ^ noHugeSkip)
            val _ = emit (noHugeSkip ^ ":")
            val header = allocBytes "32"
            val dataPointer = fieldPointer { text = header, ty = resultTy
                                           , const = false } 0
            val _ = emit ("store ptr " ^ data ^ ", ptr " ^ dataPointer)
            val lengthPointer = fieldPointer { text = header, ty = resultTy
                                             , const = false } 1
            val _ = emit ("store i64 " ^ safeCount ^ ", ptr " ^
                          lengthPointer)
            val capacityPointer = fieldPointer { text = header, ty = resultTy
                                               , const = false } 2
            val _ = emit ("store i64 " ^ capacity ^ ", ptr " ^
                          capacityPointer)
            val ownerPointer = fieldPointer { text = header, ty = resultTy
                                            , const = false } 3
            val _ = emit ("store ptr " ^ ownerValue () ^
                          ", ptr " ^ ownerPointer)
          in
            { text = header, ty = resultTy, const = false }
          end
      | ("array_length", [array]) =>
          lowerLength array
      | ("array_length_borrow", [array]) =>
          lowerLength array
      | ("array_get", [array, index]) =>
          lowerGet array index true
      | ("array_get_unchecked", [array, index]) =>
          lowerGet array index false
      | ("array_get_borrow", [array, index]) =>
          lowerBorrowGet (array, index, resultTy)
      | ("array_get_borrow_view", [array, index]) =>
          lowerBorrowGet (array, index, resultTy)
      | ("array_get_copy_borrow", [array, index]) =>
          lowerGet array index true
      | ("array_take", [array, index]) =>
          lowerTake array index
      | ("array_put", [array, index, value]) =>
          lowerPut array index value true
      | ("array_put_arena", [array, index, value]) =>
          lowerPut array index value false
      | ("array_set", [array, index, value]) =>
          lowerSet array index value true
      | ("array_set_unchecked", [array, index, value]) =>
          lowerSet array index value false
      | ("array_push", [array, value]) =>
          lowerPush array value true
      | ("array_push_unchecked", [array, value]) =>
          lowerPush array value false
      | ("array_kernel_new", [capacity]) => lowerKernelNew capacity
      | ("array_kernel_length", [array]) => kernelField array 1
      | ("array_kernel_capacity", [array]) => kernelField array 2
      | ("array_kernel_load", [array, index]) => lowerKernelLoad (array, index)
      | ("array_kernel_borrow", [array, index]) =>
          lowerKernelBorrow (array, index, resultTy)
      | ("array_kernel_take", [array, index]) => lowerKernelTake (array, index)
      | ("array_kernel_drop", [array, index]) => lowerKernelDrop array index
      | ("array_kernel_write", [array, index, value]) =>
          lowerKernelWrite (array, index, value)
      | ("array_kernel_reserve", [array, capacity]) =>
          lowerKernelReserve (array, capacity)
      | ("array_kernel_set_length", [array, length]) =>
          lowerKernelSetLength (array, length)
      | ("array_kernel_bounds_abort", [index, length]) =>
          lowerKernelAbort (index, length)
      | _ => unitValue
    end

end
