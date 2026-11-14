structure LlvmBuiltinBytes =
struct
  open LlvmTypes

  type env =
    { emit : string -> unit
    , fresh : string -> string
    , freshLabel : string -> string
    , lowerExpr : localEntry list -> Ir.expr -> value
    , allocStack : Ir.ty -> string
    , allocString : string -> string
    , coerce : value * Ir.ty -> value
    , dropValue : value * Ir.ty -> unit
    , needsDrop : Ir.ty -> bool
    , currentDestArg : string
    , stackMode : bool
    , lowerCtor :
        string * string * value option * Ir.ty * Ir.allocSite ref -> value
    }

  fun lower (env : env) (locals, name, arguments, resultTy) =
    let
      val emit = #emit env
      val fresh = #fresh env
      val freshLabel = #freshLabel env
      val lowerExpr = #lowerExpr env
      val allocStack = #allocStack env
      val allocString = #allocString env
      val coerce = #coerce env
      val dropValue = #dropValue env
      val needsDrop = #needsDrop env
      val currentDestArg = #currentDestArg env
      val stackMode = #stackMode env
      val lowerCtor = #lowerCtor env
      val lowered =
        List.map
          (fn (argument, consume) =>
             (lowerExpr locals argument, consume))
          arguments
      val headerTy = "{ ptr, i64, i64, ptr }"
      val unitValue = { text = "", ty = Ir.Unit, const = true }

      fun fieldPointer header index =
        let val pointer = fresh "bytes.fieldp" in
          emit (pointer ^ " = getelementptr " ^ headerTy ^ ", ptr " ^
                header ^ ", i32 0, i32 " ^ Int.toString index);
          pointer
        end

      fun loadField value index storage =
        let
          val pointer = fieldPointer (#text value) index
          val result = fresh "bytes.field"
        in
          emit (result ^ " = load " ^ storage ^ ", ptr " ^ pointer);
          result
        end

      fun storeField header index storage text =
        let val pointer = fieldPointer header index in
          emit ("store " ^ storage ^ " " ^ text ^ ", ptr " ^ pointer)
        end

      fun makeArray (data, length) =
        let
          val header = fresh "bytes.header"
          val _ = emit (header ^ " = call ptr @yack_root_alloc(i64 32)")
          val _ = storeField header 0 "ptr" data
          val _ = storeField header 1 "i64" length
          val _ = storeField header 2 "i64" length
          val owner =
            if stackMode then "@yack_root_region" else "null"
          val _ = storeField header 3 "ptr" owner
        in
          { text = header, ty = resultTy, const = false }
        end

      fun makeString (memory, length) =
        let
          val v0 = fresh "bytes.string.v0"
          val v1 = fresh "bytes.string.v1"
          val v2 = fresh "bytes.string.v2"
          val _ = emit (v0 ^ " = insertvalue { ptr, i64, i1 } undef, ptr " ^
                        memory ^ ", 0")
          val _ = emit (v1 ^ " = insertvalue { ptr, i64, i1 } " ^ v0 ^
                        ", i64 " ^ length ^ ", 1")
          val _ = emit (v2 ^ " = insertvalue { ptr, i64, i1 } " ^ v1 ^
                        ", i1 true, 2")
        in
          { text = v2, ty = Ir.TString, const = false }
        end

      fun dropString value =
        emit ("call void @yack_drop_string({ ptr, i64, i1 } " ^
              #text value ^ ")")

      fun ioErrorValue text =
        lowerExpr locals (Ir.StringLit (text, Ir.TString))
      fun ioResult constructor argument =
        case resultTy of
          Ir.TData (dataName, _, _) =>
            lowerCtor
              (dataName, constructor, SOME argument, resultTy,
               ref Ir.Destination)
        | _ => argument

      fun transferData (callee, flags, errorText, fd, data, length, offset, count) =
        let
          val slot = allocStack resultTy
          val offset64 = fresh "bytes.offset64"
          val count64 = fresh "bytes.count64"
          val offsetOk = fresh "bytes.offset.ok"
          val countOk = fresh "bytes.count.ok"
          val endOffset = fresh "bytes.end"
          val endOk = fresh "bytes.end.ok"
          val valid1 = fresh "bytes.valid1"
          val valid = fresh "bytes.valid"
          val validLabel = freshLabel "bytes.transfer.valid"
          val invalidLabel = freshLabel "bytes.transfer.invalid"
          val errorLabel = freshLabel "bytes.transfer.error"
          val againLabel = freshLabel "bytes.transfer.again"
          val successLabel = freshLabel "bytes.transfer.success"
          val doneLabel = freshLabel "bytes.transfer.done"
          val destination = fresh "bytes.destination"
          val transferred = fresh "bytes.transferred"
          val negative = fresh "bytes.negative"
          val transferred32 = fresh "bytes.transferred32"
          val result = fresh "bytes.result"
          val _ = emit (offset64 ^ " = sext i32 " ^ #text offset ^
                        " to i64")
          val _ = emit (count64 ^ " = sext i32 " ^ #text count ^
                        " to i64")
          val _ = emit (offsetOk ^ " = icmp sge i64 " ^ offset64 ^ ", 0")
          val _ = emit (countOk ^ " = icmp sge i64 " ^ count64 ^ ", 0")
          val _ = emit (endOffset ^ " = add i64 " ^ offset64 ^ ", " ^
                        count64)
          val _ = emit (endOk ^ " = icmp sle i64 " ^ endOffset ^ ", " ^
                        length)
          val _ = emit (valid1 ^ " = and i1 " ^ offsetOk ^ ", " ^ countOk)
          val _ = emit (valid ^ " = and i1 " ^ valid1 ^ ", " ^ endOk)
          val _ = emit ("br i1 " ^ valid ^ ", label %" ^ validLabel ^
                        ", label %" ^ invalidLabel)
          val _ = emit (invalidLabel ^ ":")
          val invalidValue =
            ioResult "Error" (ioErrorValue "buffer out of bounds")
          val _ = emit ("store " ^ llvmTy resultTy ^ " " ^
                        #text invalidValue ^ ", ptr " ^ slot)
          val _ = emit ("br label %" ^ doneLabel)
          val _ = emit (validLabel ^ ":")
          val _ = emit (destination ^ " = getelementptr i8, ptr " ^
                        data ^ ", i64 " ^ offset64)
          val _ = emit (transferred ^ " = call i64 @" ^ callee ^
                        "(i32 " ^ #text fd ^ ", ptr " ^ destination ^
                        ", i64 " ^ count64 ^ ", i32 " ^ flags ^ ")")
          val _ = emit (negative ^ " = icmp slt i64 " ^ transferred ^
                        ", 0")
          val _ = emit ("br i1 " ^ negative ^ ", label %" ^ errorLabel ^
                        ", label %" ^ successLabel)
          val _ = emit (errorLabel ^ ":")
          val errnoPtr = fresh "bytes.errnop"
          val errnoValue = fresh "bytes.errno"
          val wouldBlock = fresh "bytes.wouldblock"
          val _ = emit (errnoPtr ^ " = call ptr @__errno_location()")
          val _ = emit (errnoValue ^ " = load i32, ptr " ^ errnoPtr)
          val _ = emit (wouldBlock ^ " = icmp eq i32 " ^ errnoValue ^
                        ", 11")
          val _ = emit ("br i1 " ^ wouldBlock ^ ", label %" ^
                        againLabel ^ ", label %" ^ errorLabel ^ ".real")
          val _ = emit (errorLabel ^ ".real:")
          val errorValue = ioResult "Error" (ioErrorValue errorText)
          val _ = emit ("store " ^ llvmTy resultTy ^ " " ^
                        #text errorValue ^ ", ptr " ^ slot)
          val _ = emit ("br label %" ^ doneLabel)
          val _ = emit (againLabel ^ ":")
          val againValue =
            ioResult "Ok" { text = "-1", ty = Ir.Int 32, const = true }
          val _ = emit ("store " ^ llvmTy resultTy ^ " " ^
                        #text againValue ^ ", ptr " ^ slot)
          val _ = emit ("br label %" ^ doneLabel)
          val _ = emit (successLabel ^ ":")
          val _ = emit (transferred32 ^ " = trunc i64 " ^ transferred ^
                        " to i32")
          val successValue =
            ioResult "Ok"
              { text = transferred32, ty = Ir.Int 32, const = false }
          val _ = emit ("store " ^ llvmTy resultTy ^ " " ^
                        #text successValue ^ ", ptr " ^ slot)
          val _ = emit ("br label %" ^ doneLabel)
          val _ = emit (doneLabel ^ ":")
          val _ = emit (result ^ " = load " ^ llvmTy resultTy ^
                        ", ptr " ^ slot)
        in
          { text = result, ty = resultTy, const = false }
        end

      fun transfer (callee, flags, errorText, fd, buffer, offset, count) =
        let
          val data = loadField buffer 0 "ptr"
          val length = loadField buffer 1 "i64"
        in
          transferData
            (callee, flags, errorText, fd, data, length, offset, count)
        end
    in
      case (name, lowered) of
        ("bytes_from_string", [(value, consume)]) =>
          let
            val data = fresh "bytes.str.data"
            val length = fresh "bytes.str.len"
            val memory = fresh "bytes.str.mem"
            val _ = emit (data ^ " = extractvalue { ptr, i64, i1 } " ^
                          #text value ^ ", 0")
            val _ = emit (length ^ " = extractvalue { ptr, i64, i1 } " ^
                          #text value ^ ", 1")
            val _ = emit (memory ^ " = call ptr @yack_root_alloc(i64 " ^
                          length ^ ")")
            val _ = emit ("call ptr @memcpy(ptr " ^ memory ^ ", ptr " ^
                          data ^ ", i64 " ^ length ^ ")")
            val array = makeArray (memory, length)
            val _ = if consume then dropString value else ()
          in
            array
          end
      | ("bytes_to_string", [(value, _)]) =>
          let
            val data = loadField value 0 "ptr"
            val length = loadField value 1 "i64"
            val size = fresh "bytes.string.size"
            val endPtr = fresh "bytes.string.end"
            val v0 = fresh "bytes.string.v0"
            val v1 = fresh "bytes.string.v1"
            val v2 = fresh "bytes.string.v2"
            val _ = emit (size ^ " = add i64 " ^ length ^ ", 1")
            val memory = allocString size
            val _ = emit ("call ptr @memcpy(ptr " ^ memory ^ ", ptr " ^
                          data ^ ", i64 " ^ length ^ ")")
            val _ = emit (endPtr ^ " = getelementptr i8, ptr " ^ memory ^
                          ", i64 " ^ length)
            val _ = emit ("store i8 0, ptr " ^ endPtr)
            val _ = emit (v0 ^ " = insertvalue { ptr, i64, i1 } undef, ptr " ^
                          memory ^ ", 0")
            val _ = emit (v1 ^ " = insertvalue { ptr, i64, i1 } " ^ v0 ^
                          ", i64 " ^ length ^ ", 1")
            val _ = emit (v2 ^ " = insertvalue { ptr, i64, i1 } " ^ v1 ^
                          ", i1 true, 2")
          in
            { text = v2, ty = resultTy, const = false }
          end
      | ("bytes_to_string_range",
         [(value, _), (offset, _), (count, _)]) =>
          let
            val slot = allocStack resultTy
            val data = loadField value 0 "ptr"
            val length = loadField value 1 "i64"
            val offset64 = fresh "bytes.range.offset64"
            val count64 = fresh "bytes.range.count64"
            val offsetOk = fresh "bytes.range.offset.ok"
            val countOk = fresh "bytes.range.count.ok"
            val endOffset = fresh "bytes.range.end"
            val endOk = fresh "bytes.range.end.ok"
            val valid1 = fresh "bytes.range.valid1"
            val valid = fresh "bytes.range.valid"
            val validLabel = freshLabel "bytes.range.valid"
            val invalidLabel = freshLabel "bytes.range.invalid"
            val doneLabel = freshLabel "bytes.range.done"
            val source = fresh "bytes.range.source"
            val size = fresh "bytes.range.size"
            val endPtr = fresh "bytes.range.endptr"
            val stringValue = fresh "bytes.range.string"
            val result = fresh "bytes.range.result"
            val _ = emit (offset64 ^ " = sext i32 " ^ #text offset ^
                          " to i64")
            val _ = emit (count64 ^ " = sext i32 " ^ #text count ^
                          " to i64")
            val _ = emit (offsetOk ^ " = icmp sge i64 " ^ offset64 ^ ", 0")
            val _ = emit (countOk ^ " = icmp sge i64 " ^ count64 ^ ", 0")
            val _ = emit (endOffset ^ " = add i64 " ^ offset64 ^ ", " ^
                          count64)
            val _ = emit (endOk ^ " = icmp sle i64 " ^ endOffset ^ ", " ^
                          length)
            val _ = emit (valid1 ^ " = and i1 " ^ offsetOk ^ ", " ^ countOk)
            val _ = emit (valid ^ " = and i1 " ^ valid1 ^ ", " ^ endOk)
            val _ = emit ("br i1 " ^ valid ^ ", label %" ^ validLabel ^
                          ", label %" ^ invalidLabel)
            val _ = emit (invalidLabel ^ ":")
            val emptyMemory = allocString "1"
            val _ = emit ("store i8 0, ptr " ^ emptyMemory)
            val emptyString = makeString (emptyMemory, "0")
            val _ = emit ("store " ^ llvmTy resultTy ^ " " ^
                          #text emptyString ^ ", ptr " ^ slot)
            val _ = emit ("br label %" ^ doneLabel)
            val _ = emit (validLabel ^ ":")
            val _ = emit (source ^ " = getelementptr i8, ptr " ^ data ^
                          ", i64 " ^ offset64)
            val _ = emit (size ^ " = add i64 " ^ count64 ^ ", 1")
            val memory = allocString size
            val _ = emit ("call ptr @memcpy(ptr " ^ memory ^ ", ptr " ^
                          source ^ ", i64 " ^ count64 ^ ")")
            val _ = emit (endPtr ^ " = getelementptr i8, ptr " ^ memory ^
                          ", i64 " ^ count64)
            val _ = emit ("store i8 0, ptr " ^ endPtr)
            val stringValue = makeString (memory, count64)
            val _ = emit ("store " ^ llvmTy resultTy ^ " " ^
                          #text stringValue ^ ", ptr " ^ slot)
            val _ = emit ("br label %" ^ doneLabel)
            val _ = emit (doneLabel ^ ":")
            val _ = emit (result ^ " = load " ^ llvmTy resultTy ^
                          ", ptr " ^ slot)
          in
            { text = result, ty = resultTy, const = false }
          end
      | ("send_string",
         [(fd, _), (value, _), (offset, _), (count, _)]) =>
          let
            val data = fresh "bytes.send.data"
            val length = fresh "bytes.send.len"
            val _ = emit (data ^ " = extractvalue { ptr, i64, i1 } " ^
                          #text value ^ ", 0")
            val _ = emit (length ^ " = extractvalue { ptr, i64, i1 } " ^
                          #text value ^ ", 1")
          in
            transferData
              ("send", "16448", "send failed", fd, data, length,
               offset, count)
          end
      | ("bytes_header_end", [(value, _), (start, _), (limit, _)]) =>
          let
            val data = loadField value 0 "ptr"
            val resultSlot = fresh "bytes.header.result"
            val indexSlot = fresh "bytes.header.indexslot"
            val index = fresh "bytes.header.index"
            val limit64 = fresh "bytes.header.limit64"
            val start64 = fresh "bytes.header.start64"
            val stop = fresh "bytes.header.stop"
            val condition = fresh "bytes.header.condition"
            val loopLabel = freshLabel "bytes.header.loop"
            val bodyLabel = freshLabel "bytes.header.body"
            val nextLabel = freshLabel "bytes.header.next"
            val matchLabel = freshLabel "bytes.header.match"
            val doneLabel = freshLabel "bytes.header.done"
            val result = fresh "bytes.header.value"
            val pointer = fresh "bytes.header.pointer"
            val b0 = fresh "bytes.header.b0"
            val b1 = fresh "bytes.header.b1"
            val b2 = fresh "bytes.header.b2"
            val b3 = fresh "bytes.header.b3"
            val isCr = fresh "bytes.header.cr"
            val isLf = fresh "bytes.header.lf"
            val isCr2 = fresh "bytes.header.cr2"
            val isLf2 = fresh "bytes.header.lf2"
            val all0 = fresh "bytes.header.all0"
            val all = fresh "bytes.header.all"
            val next = fresh "bytes.header.next"
            val matched = fresh "bytes.header.matched"
            val _ = emit (resultSlot ^ " = alloca i32")
            val _ = emit ("store i32 -1, ptr " ^ resultSlot)
            val _ = emit (indexSlot ^ " = alloca i64")
            val _ = emit (start64 ^ " = sext i32 " ^ #text start ^
                          " to i64")
            val _ = emit (limit64 ^ " = sext i32 " ^ #text limit ^
                          " to i64")
            val _ = emit ("store i64 " ^ start64 ^ ", ptr " ^ indexSlot)
            val _ = emit (stop ^ " = sub i64 " ^ limit64 ^ ", 3")
            val _ = emit ("br label %" ^ loopLabel)
            val _ = emit (loopLabel ^ ":")
            val _ = emit (index ^ " = load i64, ptr " ^ indexSlot)
            val _ = emit (condition ^ " = icmp slt i64 " ^ index ^ ", " ^
                          stop)
            val _ = emit ("br i1 " ^ condition ^ ", label %" ^ bodyLabel ^
                          ", label %" ^ doneLabel)
            val _ = emit (bodyLabel ^ ":")
            val _ = emit (pointer ^ " = getelementptr i8, ptr " ^ data ^
                          ", i64 " ^ index)
            val _ = emit (b0 ^ " = load i8, ptr " ^ pointer)
            val _ = emit (isCr ^ " = icmp eq i8 " ^ b0 ^ ", 13")
            val _ = emit ("br i1 " ^ isCr ^ ", label %" ^ nextLabel ^
                          ", label %" ^ nextLabel)
            val _ = emit (nextLabel ^ ":")
            val _ = emit (pointer ^ ".1 = getelementptr i8, ptr " ^
                          pointer ^ ", i64 1")
            val _ = emit (b1 ^ " = load i8, ptr " ^ pointer ^ ".1")
            val _ = emit (isLf ^ " = icmp eq i8 " ^ b1 ^ ", 10")
            val _ = emit (all0 ^ " = and i1 " ^ isCr ^ ", " ^ isLf)
            val _ = emit (pointer ^ ".2 = getelementptr i8, ptr " ^
                          pointer ^ ", i64 2")
            val _ = emit (b2 ^ " = load i8, ptr " ^ pointer ^ ".2")
            val _ = emit (isCr2 ^ " = icmp eq i8 " ^ b2 ^ ", 13")
            val _ = emit (pointer ^ ".3 = getelementptr i8, ptr " ^
                          pointer ^ ", i64 3")
            val _ = emit (b3 ^ " = load i8, ptr " ^ pointer ^ ".3")
            val _ = emit (isLf2 ^ " = icmp eq i8 " ^ b3 ^ ", 10")
            val _ = emit (all ^ " = and i1 " ^ all0 ^ ", " ^ isCr2)
            val _ = emit (all ^ ".2 = and i1 " ^ all ^ ", " ^ isLf2)
            val _ = emit ("br i1 " ^ all ^ ".2, label %" ^ matchLabel ^
                          ", label %" ^ nextLabel ^ ".2")
            val _ = emit (matchLabel ^ ":")
            val _ = emit (matched ^ " = add i64 " ^ index ^ ", 4")
            val _ = emit (matched ^ ".32 = trunc i64 " ^ matched ^
                          " to i32")
            val _ = emit ("store i32 " ^ matched ^ ".32, ptr " ^
                          resultSlot)
            val _ = emit ("br label %" ^ doneLabel)
            val _ = emit (nextLabel ^ ".2:")
            val _ = emit (next ^ " = add i64 " ^ index ^ ", 1")
            val _ = emit ("store i64 " ^ next ^ ", ptr " ^ indexSlot)
            val _ = emit ("br label %" ^ loopLabel)
            val _ = emit (doneLabel ^ ":")
            val _ = emit (result ^ " = load i32, ptr " ^ resultSlot)
          in
            { text = result, ty = resultTy, const = false }
          end
      | ("bytes_write_string", [(value, _), (offset, _), (limit, _), (text, _)]) =>
          let
            val data = loadField value 0 "ptr"
            val length = loadField value 1 "i64"
            val textData = fresh "bytes.write.data"
            val textLength = fresh "bytes.write.len"
            val offset64 = fresh "bytes.write.offset64"
            val limit64 = fresh "bytes.write.limit64"
            val endOffset = fresh "bytes.write.end"
            val offsetOk = fresh "bytes.write.offset.ok"
            val limitOk = fresh "bytes.write.limit.ok"
            val boundsOk = fresh "bytes.write.bounds.ok"
            val endOk = fresh "bytes.write.end.ok"
            val valid1 = fresh "bytes.write.valid1"
            val valid = fresh "bytes.write.valid"
            val slot = fresh "bytes.write.slot"
            val validLabel = freshLabel "bytes.write.valid"
            val doneLabel = freshLabel "bytes.write.done"
            val destination = fresh "bytes.write.destination"
            val newOffset = fresh "bytes.write.newoffset"
            val final = fresh "bytes.write.final"
            val _ = emit (textData ^ " = extractvalue { ptr, i64, i1 } " ^
                          #text text ^ ", 0")
            val _ = emit (textLength ^ " = extractvalue { ptr, i64, i1 } " ^
                          #text text ^ ", 1")
            val _ = emit (offset64 ^ " = sext i32 " ^ #text offset ^
                          " to i64")
            val _ = emit (limit64 ^ " = sext i32 " ^ #text limit ^
                          " to i64")
            val _ = emit (endOffset ^ " = add i64 " ^ offset64 ^ ", " ^
                          textLength)
            val _ = emit (offsetOk ^ " = icmp sge i64 " ^ offset64 ^ ", 0")
            val _ = emit (limitOk ^ " = icmp sle i64 " ^ offset64 ^ ", " ^
                          limit64)
            val _ = emit (boundsOk ^ " = and i1 " ^ offsetOk ^ ", " ^ limitOk)
            val _ = emit (endOk ^ " = icmp sle i64 " ^ endOffset ^ ", " ^
                          length)
            val _ = emit (valid1 ^ " = and i1 " ^ boundsOk ^ ", " ^ endOk)
            val _ = emit (valid ^ " = icmp sle i64 " ^ endOffset ^ ", " ^
                          limit64)
            val _ = emit (valid1 ^ ".limit = and i1 " ^ valid1 ^ ", " ^ valid)
            val _ = emit (slot ^ " = alloca i32")
            val _ = emit ("store i32 -2, ptr " ^ slot)
            val _ = emit ("br i1 " ^ valid1 ^ ".limit, label %" ^ validLabel ^
                          ", label %" ^ doneLabel)
            val _ = emit (validLabel ^ ":")
            val _ = emit (destination ^ " = getelementptr i8, ptr " ^ data ^
                          ", i64 " ^ offset64)
            val _ = emit ("call ptr @memcpy(ptr " ^ destination ^ ", ptr " ^
                          textData ^ ", i64 " ^ textLength ^ ")")
            val _ = emit (newOffset ^ " = trunc i64 " ^ endOffset ^
                          " to i32")
            val _ = emit ("store i32 " ^ newOffset ^ ", ptr " ^ slot)
            val _ = emit ("br label %" ^ doneLabel)
            val _ = emit (doneLabel ^ ":")
            val _ = emit (final ^ " = load i32, ptr " ^ slot)
          in
            { text = final, ty = resultTy, const = false }
          end
      | ("bytes_write_int", [(value, _), (offset, _), (limit, _), (number, _)]) =>
          let
            val data = loadField value 0 "ptr"
            val length = loadField value 1 "i64"
            val offset64 = fresh "bytes.write.int.offset64"
            val limit64 = fresh "bytes.write.int.limit64"
            val remainingLimit = fresh "bytes.write.int.remaining.limit"
            val remainingArray = fresh "bytes.write.int.remaining.array"
            val remaining = fresh "bytes.write.int.remaining"
            val offsetOk = fresh "bytes.write.int.offset.ok"
            val remainingOk = fresh "bytes.write.int.remaining.ok"
            val valid = fresh "bytes.write.int.valid"
            val slot = fresh "bytes.write.int.slot"
            val validLabel = freshLabel "bytes.write.int.valid"
            val doneLabel = freshLabel "bytes.write.int.done"
            val destination = fresh "bytes.write.int.destination"
            val written = fresh "bytes.write.int.written"
            val written64 = fresh "bytes.write.int.written64"
            val writtenOk = fresh "bytes.write.int.written.ok"
            val fits = fresh "bytes.write.int.fits"
            val newOffset64 = fresh "bytes.write.int.newoffset64"
            val newOffset = fresh "bytes.write.int.newoffset"
            val final = fresh "bytes.write.int.final"
            val _ = emit (offset64 ^ " = sext i32 " ^ #text offset ^
                          " to i64")
            val _ = emit (limit64 ^ " = sext i32 " ^ #text limit ^
                          " to i64")
            val _ = emit (remainingLimit ^ " = sub i64 " ^ limit64 ^ ", " ^
                          offset64)
            val _ = emit (remainingArray ^ " = sub i64 " ^ length ^ ", " ^
                          offset64)
            val _ = emit (remainingOk ^ " = icmp sgt i64 " ^ remainingLimit ^
                          ", 0")
            val _ = emit (remaining ^ " = select i1 " ^ remainingOk ^
                          ", i64 " ^ remainingLimit ^ ", i64 0")
            val _ = emit (remaining ^ ".array = icmp sgt i64 " ^
                          remainingArray ^ ", 0")
            val _ = emit (remaining ^ ".min = select i1 " ^
                          remaining ^ ".array, i64 " ^ remaining ^
                          ", i64 0")
            val _ = emit (offsetOk ^ " = icmp sge i64 " ^ offset64 ^ ", 0")
            val _ = emit (valid ^ " = and i1 " ^ offsetOk ^ ", " ^
                          remainingOk)
            val _ = emit (valid ^ ".array = and i1 " ^ valid ^ ", " ^
                          remaining ^ ".array")
            val _ = emit (slot ^ " = alloca i32")
            val _ = emit ("store i32 -2, ptr " ^ slot)
            val _ = emit ("br i1 " ^ valid ^ ".array, label %" ^ validLabel ^
                          ", label %" ^ doneLabel)
            val _ = emit (validLabel ^ ":")
            val _ = emit (destination ^ " = getelementptr i8, ptr " ^ data ^
                          ", i64 " ^ offset64)
            val _ = emit (written ^ " = call i32 @snprintf(ptr " ^
                          destination ^ ", i64 " ^ remaining ^ ".min" ^
                          ", ptr @.fmt_int_plain, i32 " ^ #text number ^ ")")
            val _ = emit (written64 ^ " = sext i32 " ^ written ^ " to i64")
            val _ = emit (writtenOk ^ " = icmp sge i32 " ^ written ^ ", 0")
            val _ = emit (fits ^ " = icmp slt i64 " ^ written64 ^ ", " ^
                          remaining ^ ".min")
            val _ = emit (newOffset64 ^ " = add i64 " ^ offset64 ^ ", " ^
                          written64)
            val _ = emit (newOffset ^ " = trunc i64 " ^ newOffset64 ^
                          " to i32")
            val _ = emit (newOffset ^ ".value = select i1 " ^ writtenOk ^
                          ", i32 " ^ newOffset ^ ", i32 -2")
            val _ = emit (newOffset ^ ".fit = select i1 " ^ fits ^
                          ", i32 " ^ newOffset ^ ".value, i32 -2")
            val _ = emit ("store i32 " ^ newOffset ^ ".fit, ptr " ^ slot)
            val _ = emit ("br label %" ^ doneLabel)
            val _ = emit (doneLabel ^ ":")
            val _ = emit (final ^ " = load i32, ptr " ^ slot)
          in
            { text = final, ty = resultTy, const = false }
          end
      | ("recv_into", [(fd, _), (buffer, _), (offset, _), (count, _)]) =>
          transfer ("recv", "64", "recv failed", fd, buffer, offset, count)
      | ("send_from", [(fd, _), (buffer, _), (offset, _), (count, _)]) =>
          transfer ("send", "16448", "send failed", fd, buffer, offset, count)
      | _ => unitValue
    end

end
