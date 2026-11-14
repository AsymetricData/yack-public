structure LlvmDrop =
struct
  open LlvmTypes

  type env =
    { emit : string -> unit
    , fresh : string -> string
    , freshLabel : string -> string
    }

      fun needsDrop ty =
        case ty of
          Ir.Int _ => false
        | Ir.Float _ => false
        | Ir.Bool => false
        | Ir.Unit => false
        | Ir.TBorrow _ => false
        | Ir.TToken => false
        | Ir.TResource => false
        | Ir.TTuple _ => true
        | Ir.TRecord (_, _) => true
        | Ir.TData (_, _, _) => true
        | Ir.TString => true
        | Ir.TArray _ => true
        | Ir.TMap _ => true
        | Ir.TFun _ => false

      fun dropValue (env : env, value : value, ty : Ir.ty) =
        dropValueSkipping (env, value, ty, [])

      and dropValueSkipping (env : env, value : value, ty : Ir.ty,
                             skip : int list list) =
        let
          val emit = #emit env
          val fresh = #fresh env
          val freshLabel = #freshLabel env
          fun dropValueHere (value, ty) = dropValue (env, value, ty)
          fun dropValueSkippingHere (value, ty, skip) =
            dropValueSkipping (env, value, ty, skip)

          fun skippedWhole index =
            List.exists (fn path => path = [index]) skip

          fun subPaths index =
            List.mapPartial
              (fn path =>
                case path of
                  head :: rest =>
                    if head = index andalso not (List.null rest) then
                      SOME rest
                    else NONE
                | [] => NONE)
              skip
        in
          case ty of
            Ir.TData (name, _, _) =>
              if List.null skip then
                emit ("call void @yack_drop_" ^ name ^
                      "(ptr " ^ #text value ^ ")")
              else ()
          | Ir.TTuple types =>
              let
                fun loop (index, remaining) =
                  case remaining of
                    [] => ()
                  | fieldTy :: rest =>
                      ( if skippedWhole index then ()
                        else
                          let val reg = fresh "dropfield" in
                            emit (reg ^ " = extractvalue " ^ llvmTy ty ^ " " ^
                                  #text value ^ ", " ^ Int.toString index);
                            dropValueSkippingHere
                              ( { text = reg, ty = fieldTy, const = false }
                              , fieldTy
                              , subPaths index )
                          end
                      ; loop (index + 1, rest) )
              in
                loop (0, types)
              end
          | Ir.TRecord (fields, _) =>
              let
                fun loop (index, remaining) =
                  case remaining of
                    [] => ()
                  | (_, fieldTy) :: rest =>
                      ( if skippedWhole index then ()
                        else
                          let val reg = fresh "dropfield" in
                            emit (reg ^ " = extractvalue " ^ llvmTy ty ^ " " ^
                                  #text value ^ ", " ^ Int.toString index);
                            dropValueSkippingHere
                              ( { text = reg, ty = fieldTy, const = false }
                              , fieldTy
                              , subPaths index )
                          end
                      ; loop (index + 1, rest) )
              in
                loop (0, fields)
              end
          | Ir.TString =>
              emit ("call void @yack_drop_string({ ptr, i64, i1 } " ^
                    #text value ^ ")")
          | Ir.TArray elementTy =>
              let
                val header = #text value
                val isNull = fresh "arr.dropl.null"
                val freeLabel = freshLabel "arr.dropl.free"
                val doneLabel = freshLabel "arr.dropl.done"
                val dataPtr = fresh "arr.dropl.datap"
                val data = fresh "arr.dropl.data"
                val dropLabel = freshLabel "arr.dropl.drop"
                val loopLabel = freshLabel "arr.dropl.loop"
                val bodyLabel = freshLabel "arr.dropl.body"
                val indexSlot = fresh "arr.dropl.indexslot"
                val index = fresh "arr.dropl.index"
                val lengthPtr = fresh "arr.dropl.lenp"
                val length = fresh "arr.dropl.len"
                val condition = fresh "arr.dropl.condition"
                val elementPtr = fresh "arr.dropl.elementp"
                val element = fresh "arr.dropl.element"
                val next = fresh "arr.dropl.next"
                val dropElements = needsDrop elementTy
                val _ = emit (isNull ^ " = icmp eq ptr " ^ header ^ ", null")
                val _ = emit ("br i1 " ^ isNull ^ ", label %" ^ doneLabel ^
                              ", label %" ^
                              (if dropElements then dropLabel else freeLabel))
                val _ =
                  if dropElements then
                    let
                      val _ = emit (dropLabel ^ ":")
                      val _ = emit (dataPtr ^
                                    " = getelementptr { ptr, i64, i64, ptr }, ptr " ^
                                    header ^ ", i32 0, i32 0")
                      val _ = emit (data ^ " = load ptr, ptr " ^ dataPtr)
                      val _ = emit (lengthPtr ^
                                    " = getelementptr { ptr, i64, i64, ptr }, ptr " ^
                                    header ^ ", i32 0, i32 1")
                      val _ = emit (length ^ " = load i64, ptr " ^ lengthPtr)
                      val _ = emit (indexSlot ^ " = alloca i64")
                      val _ = emit ("store i64 0, ptr " ^ indexSlot)
                      val _ = emit ("br label %" ^ loopLabel)
                      val _ = emit (loopLabel ^ ":")
                      val _ = emit (index ^ " = load i64, ptr " ^ indexSlot)
                      val _ = emit (condition ^ " = icmp slt i64 " ^ index ^
                                    ", " ^ length)
                      val _ = emit ("br i1 " ^ condition ^ ", label %" ^
                                    bodyLabel ^ ", label %" ^ freeLabel)
                      val _ = emit (bodyLabel ^ ":")
                      val _ =
                        emit (elementPtr ^ " = getelementptr " ^
                              storageTy elementTy ^ ", ptr " ^ data ^
                              ", i64 " ^ index)
                      val _ =
                        emit (element ^ " = load " ^ storageTy elementTy ^
                              ", ptr " ^ elementPtr)
                      val _ =
                        dropValueHere ( { text = element, ty = elementTy
                                    , const = false }
                                  , elementTy )
                      val _ = emit (next ^ " = add i64 " ^ index ^ ", 1")
                      val _ = emit ("store i64 " ^ next ^ ", ptr " ^ indexSlot)
                      val _ = emit ("br label %" ^ loopLabel)
                    in
                      ()
                    end
                  else ()
                val _ = emit (freeLabel ^ ":")
                val _ =
                  if dropElements then ()
                  else
                    ( emit (dataPtr ^
                            " = getelementptr { ptr, i64, i64, ptr }, ptr " ^
                            header ^ ", i32 0, i32 0")
                    ; emit (data ^ " = load ptr, ptr " ^ dataPtr) )
                val _ = emit ("call void @yack_free(ptr " ^ data ^ ")")
                val _ = emit ("call void @yack_free(ptr " ^ header ^ ")")
                val _ = emit ("br label %" ^ doneLabel)
                val _ = emit (doneLabel ^ ":")
              in
                ()
              end
          | Ir.TMap valueTy =>
              let
                val header = #text value
                val entryTy =
                  "{ i32, { ptr, i64, i1 }, " ^ storageTy valueTy ^ " }"
                val isNull = fresh "map.dropl.null"
                val bodyLabel = freshLabel "map.dropl.body"
                val dropLabel = freshLabel "map.dropl.drop"
                val loopLabel = freshLabel "map.dropl.loop"
                val dropEntry = freshLabel "map.dropl.entry"
                val freeLabel = freshLabel "map.dropl.free"
                val doneLabel = freshLabel "map.dropl.done"
                val dataPtr = fresh "map.dropl.datap"
                val data = fresh "map.dropl.data"
                val capacityPtr = fresh "map.dropl.capp"
                val capacity = fresh "map.dropl.cap"
                val indexSlot = fresh "map.dropl.idslot"
                val index = fresh "map.dropl.index"
                val condition = fresh "map.dropl.cond"
                val entryPtr = fresh "map.dropl.entryp"
                val state = fresh "map.dropl.state"
                val used = fresh "map.dropl.used"
                val keyPtr = fresh "map.dropl.keyp"
                val key = fresh "map.dropl.key"
                val valuePtr = fresh "map.dropl.valuep"
                val entryValue = fresh "map.dropl.value"
                val next = fresh "map.dropl.next"
                val nextSkip = fresh "map.dropl.nextskip"
                val _ = emit (isNull ^ " = icmp eq ptr " ^ header ^ ", null")
                val _ = emit ("br i1 " ^ isNull ^ ", label %" ^ doneLabel ^
                              ", label %" ^ bodyLabel)
                val _ = emit (bodyLabel ^ ":")
                val _ = emit (dataPtr ^
                              " = getelementptr { ptr, i64, i64, i64 }, ptr " ^
                              header ^ ", i32 0, i32 0")
                val _ = emit (data ^ " = load ptr, ptr " ^ dataPtr)
                val _ = emit (capacityPtr ^
                              " = getelementptr { ptr, i64, i64, i64 }, ptr " ^
                              header ^ ", i32 0, i32 3")
                val _ = emit (capacity ^ " = load i64, ptr " ^ capacityPtr)
                val _ = emit (indexSlot ^ " = alloca i64")
                val _ = emit ("store i64 0, ptr " ^ indexSlot)
                val _ = emit ("br label %" ^ loopLabel)
                val _ = emit (loopLabel ^ ":")
                val _ = emit (index ^ " = load i64, ptr " ^ indexSlot)
                val _ = emit (condition ^ " = icmp ult i64 " ^ index ^
                              ", " ^ capacity)
                val _ = emit ("br i1 " ^ condition ^ ", label %" ^ dropLabel ^
                              ", label %" ^ freeLabel)
                val _ = emit (dropLabel ^ ":")
                val _ = emit (entryPtr ^ " = getelementptr " ^ entryTy ^
                              ", ptr " ^ data ^ ", i64 " ^ index)
                val _ = emit (state ^ " = load i32, ptr " ^ entryPtr)
                val _ = emit (used ^ " = icmp eq i32 " ^ state ^ ", 1")
                val _ = emit ("br i1 " ^ used ^ ", label %" ^ dropEntry ^
                              ", label %" ^ freeLabel ^ ".next")
                val _ = emit (freeLabel ^ ".next:")
                val _ = emit (nextSkip ^ " = add i64 " ^ index ^ ", 1")
                val _ = emit ("store i64 " ^ nextSkip ^ ", ptr " ^ indexSlot)
                val _ = emit ("br label %" ^ loopLabel)
                val _ = emit (dropEntry ^ ":")
                val _ = emit (keyPtr ^ " = getelementptr " ^ entryTy ^
                              ", ptr " ^ entryPtr ^ ", i32 0, i32 1")
                val _ = emit (key ^ " = load { ptr, i64, i1 }, ptr " ^ keyPtr)
                val _ =
                  emit ("call void @yack_drop_string({ ptr, i64, i1 } " ^
                        key ^ ")")
                val _ = emit (valuePtr ^ " = getelementptr " ^ entryTy ^
                              ", ptr " ^ entryPtr ^ ", i32 0, i32 2")
                val _ = emit (entryValue ^ " = load " ^ storageTy valueTy ^
                              ", ptr " ^ valuePtr)
                val _ =
                  dropValueHere ( { text = entryValue, ty = valueTy
                              , const = false }
                            , valueTy )
                val _ = emit (next ^ " = add i64 " ^ index ^ ", 1")
                val _ = emit ("store i64 " ^ next ^ ", ptr " ^ indexSlot)
                val _ = emit ("br label %" ^ loopLabel)
                val _ = emit (freeLabel ^ ":")
                val _ = emit ("call void @yack_free(ptr " ^ data ^ ")")
                val _ = emit ("call void @yack_free(ptr " ^ header ^ ")")
                val _ = emit ("br label %" ^ doneLabel)
                val _ = emit (doneLabel ^ ":")
              in
                ()
              end
          | Ir.TFun _ => ()
          | _ => ()
        end

      fun freeIfHeap (env : env, value : value) =
        case #ty value of
          Ir.TData (dataName, _, _) =>
            let
              val emit = #emit env
              val fresh = #fresh env
              val freshLabel = #freshLabel env
              val flagPtr = fresh "free.flagp"
              val flag = fresh "free.stack"
              val freeLabel = freshLabel "free.do"
              val doneLabel = freshLabel "free.done"
              val _ =
                emit (flagPtr ^ " = getelementptr inbounds %" ^ dataName ^
                      ", ptr " ^ #text value ^ ", i32 0, i32 1")
              val _ = emit (flag ^ " = load i1, ptr " ^ flagPtr)
              val _ =
                emit ("br i1 " ^ flag ^ ", label %" ^ doneLabel ^
                      ", label %" ^ freeLabel)
              val _ = emit (freeLabel ^ ":")
              val _ = emit ("call void @yack_free(ptr " ^ #text value ^ ")")
              val _ = emit ("br label %" ^ doneLabel)
              val _ = emit (doneLabel ^ ":")
            in
              ()
            end
        | _ => ()

end
