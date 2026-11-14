structure LlvmBuiltinString =
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
      val lowerCtor = #lowerCtor env
      val lowered =
        List.map
          (fn (argument, consume) =>
            (lowerExpr locals argument, consume))
          arguments
      fun freeAfter (value, consume) =
        if consume then
          emit ("call void @yack_drop_string({ ptr, i64, i1 } " ^
                #text value ^ ")")
        else ()
      fun ioErrorValue text =
        lowerExpr locals (Ir.StringLit (text, Ir.TString))
      fun ioResult constructor argument =
        case resultTy of
          Ir.TData (dataName, _, _) =>
            lowerCtor
              (dataName, constructor, SOME argument, resultTy,
               ref Ir.Destination)
        | _ => argument
      fun optionValue (constructor, payload) =
        case resultTy of
          Ir.TData (dataName, _, _) =>
            lowerCtor (dataName, constructor, payload, resultTy, ref Ir.Destination)
        | _ => { text = "zeroinitializer", ty = resultTy, const = true }
      fun cString (value : value) =
        let
          val data = fresh "io.pathdata"
          val length = fresh "io.pathlen"
          val size = fresh "io.pathsize"
          val endPtr = fresh "io.pathend"
          val _ =
            emit (data ^ " = extractvalue { ptr, i64, i1 } " ^
                  #text value ^ ", 0")
          val _ =
            emit (length ^ " = extractvalue { ptr, i64, i1 } " ^
                  #text value ^ ", 1")
          val _ =
            emit (size ^ " = add i64 " ^ length ^ ", 1")
          val memory = allocString size
          val _ =
            emit ("call ptr @memcpy(ptr " ^ memory ^ ", ptr " ^ data ^
                  ", i64 " ^ length ^ ")")
          val _ =
            emit (endPtr ^ " = getelementptr i8, ptr " ^ memory ^
                  ", i64 " ^ length)
          val _ = emit ("store i8 0, ptr " ^ endPtr)
        in
          memory
        end
      fun integerBounds width =
        case width of
          1 => ("-1", "0")
        | 2 => ("-2", "1")
        | 4 => ("-8", "7")
        | 8 => ("-128", "127")
        | 16 => ("-32768", "32767")
        | 32 => ("-2147483648", "2147483647")
        | 64 => ("-9223372036854775808", "9223372036854775807")
        | _ => ("0", "0")
      fun lowerIntToString (width, value) =
        let
          val length = fresh "int.str.len"
          val v0 = fresh "int.str.v0"
          val v1 = fresh "int.str.v1"
          val v2 = fresh "int.str.v2"
          val memory = allocString "32"
          val _ =
            if width = 64 then
              emit ("call i32 @snprintf(ptr " ^ memory ^
                    ", i64 32, ptr @.fmt_int64_plain, i64 " ^
                    #text (coerce (value, Ir.Int 64)) ^ ")")
            else
              emit ("call i32 @snprintf(ptr " ^ memory ^
                    ", i64 32, ptr @.fmt_int_plain, i32 " ^
                    #text (coerce (value, Ir.Int 32)) ^ ")")
          val _ =
            emit (length ^ " = call i64 @strlen(ptr " ^ memory ^ ")")
          val _ =
            emit (v0 ^ " = insertvalue { ptr, i64, i1 } undef, ptr " ^
                  memory ^ ", 0")
          val _ =
            emit (v1 ^ " = insertvalue { ptr, i64, i1 } " ^ v0 ^
                  ", i64 " ^ length ^ ", 1")
          val _ =
            emit (v2 ^ " = insertvalue { ptr, i64, i1 } " ^ v1 ^
                  ", i1 true, 2")
        in
          { text = v2, ty = Ir.TString, const = false }
        end
      fun lowerStringToInt (width, value) =
        let
          val length = fresh "str2int.len"
          val path = cString value
          val endSlot = fresh "str2int.endslot"
          val endPtr = fresh "str2int.end"
          val expected = fresh "str2int.expected"
          val parsed = fresh "str2int.parsed"
          val nonEmpty = fresh "str2int.nonempty"
          val atEnd = fresh "str2int.atend"
          val tooLow = fresh "str2int.low"
          val tooHigh = fresh "str2int.high"
          val outOfRange = fresh "str2int.range"
          val inRange = fresh "str2int.inrange"
          val valid = fresh "str2int.valid"
          val validAll = fresh "str2int.validall"
          val slot = allocStack resultTy
          val someLabel = freshLabel "str2int.some"
          val noneLabel = freshLabel "str2int.none"
          val doneLabel = freshLabel "str2int.done"
          val result = fresh "str2int.result"
          val (minimum, maximum) = integerBounds width
          val _ =
            emit (length ^ " = extractvalue { ptr, i64, i1 } " ^
                  #text value ^ ", 1")
          val _ = emit (endSlot ^ " = alloca ptr")
          val _ =
            emit (parsed ^ " = call i64 @strtoll(ptr " ^ path ^
                  ", ptr " ^ endSlot ^ ", i32 10)")
          val _ = emit (endPtr ^ " = load ptr, ptr " ^ endSlot)
          val _ =
            emit (expected ^ " = getelementptr i8, ptr " ^ path ^
                  ", i64 " ^ length)
          val _ =
            emit (nonEmpty ^ " = icmp ne ptr " ^ endPtr ^ ", " ^ path)
          val _ =
            emit (atEnd ^ " = icmp eq ptr " ^ endPtr ^ ", " ^ expected)
          val _ =
            emit (tooLow ^ " = icmp slt i64 " ^ parsed ^ ", " ^ minimum)
          val _ =
            emit (tooHigh ^ " = icmp sgt i64 " ^ parsed ^ ", " ^ maximum)
          val _ =
            emit (outOfRange ^ " = or i1 " ^ tooLow ^ ", " ^ tooHigh)
          val _ = emit (inRange ^ " = xor i1 " ^ outOfRange ^ ", true")
          val _ =
            emit (valid ^ " = and i1 " ^ nonEmpty ^ ", " ^ atEnd)
          val _ =
            emit (validAll ^ " = and i1 " ^ valid ^ ", " ^ inRange)
          val converted =
            if width = 64 then parsed
            else
              let val reg = fresh "str2int.conv" in
                emit (reg ^ " = trunc i64 " ^ parsed ^ " to i" ^
                      Int.toString width);
                reg
              end
          val _ =
            emit ("br i1 " ^ validAll ^ ", label %" ^ someLabel ^
                  ", label %" ^ noneLabel)
          val _ = emit (someLabel ^ ":")
          val someValue =
            optionValue
              ( "Some"
              , SOME { text = converted, ty = Ir.Int width, const = false }
              )
          val _ =
            emit ("store " ^ storageTy resultTy ^ " " ^
                  #text someValue ^ ", ptr " ^ slot)
          val _ = emit ("br label %" ^ doneLabel)
          val _ = emit (noneLabel ^ ":")
          val noneValue = optionValue ("None", NONE)
          val _ =
            emit ("store " ^ storageTy resultTy ^ " " ^
                  #text noneValue ^ ", ptr " ^ slot)
          val _ = emit ("br label %" ^ doneLabel)
          val _ = emit (doneLabel ^ ":")
          val _ =
            emit (result ^ " = load " ^ storageTy resultTy ^
                  ", ptr " ^ slot)
          val _ = emit ("call void @yack_free(ptr " ^ path ^ ")")
        in
          { text = result, ty = resultTy, const = false }
        end
      fun conversionTarget name =
        if name = "int_to_string" then SOME (true, 32)
        else if name = "string_to_int" then SOME (false, 32)
        else
          let
            fun loop [] = NONE
              | loop (width :: rest) =
                  if name = "int" ^ Int.toString width ^ "_to_string" then
                    SOME (true, width)
                  else if name = "string_to_int" ^ Int.toString width then
                    SOME (false, width)
                  else loop rest
          in
            loop [1, 2, 4, 8, 16, 32, 64]
          end
      val unitValue = { text = "", ty = Ir.Unit, const = true }
    in
      case (name, lowered) of
        ("io.read_file", [(value, consume)]) =>
          let
            val slot = allocStack resultTy
            val path = cString value
            val fp = fresh "io.fp"
            val isNull = fresh "io.fpnull"
            val nullLabel = freshLabel "io.read.null"
            val okLabel = freshLabel "io.read.ok"
            val doneLabel = freshLabel "io.read.done"
            val bufferSlot = fresh "io.read.bufslot"
            val lengthSlot = fresh "io.read.lenslot"
            val capacitySlot = fresh "io.read.capslot"
            val loopLabel = freshLabel "io.read.loop"
            val growLabel = freshLabel "io.read.grow"
            val checkLabel = freshLabel "io.read.check"
            val finishLabel = freshLabel "io.read.finish"
            val memory = fresh "io.read.mem"
            val length = fresh "io.read.len"
            val capacity = fresh "io.read.cap"
            val remaining = fresh "io.read.rem"
            val destination = fresh "io.read.dst"
            val count = fresh "io.read.count"
            val nextLength = fresh "io.read.nextlen"
            val isZero = fresh "io.read.zero"
            val isFull = fresh "io.read.full"
            val nextCapacity = fresh "io.read.nextcap"
            val endPtr = fresh "io.read.end"
            val v0 = fresh "io.read.v0"
            val v1 = fresh "io.read.v1"
            val v2 = fresh "io.read.v2"
            val result = fresh "io.read.result"
            val _ =
              emit (fp ^ " = call ptr @fopen(ptr " ^ path ^
                    ", ptr @.mode_rb)")
            val _ =
              emit (isNull ^ " = icmp eq ptr " ^ fp ^ ", null")
            val _ =
              emit ("br i1 " ^ isNull ^ ", label %" ^ nullLabel ^
                    ", label %" ^ okLabel)
            val _ = emit (nullLabel ^ ":")
            val errorValue =
              ioResult "Error" (ioErrorValue "cannot read file")
            val _ =
              emit ("store " ^ llvmTy resultTy ^ " " ^ #text errorValue ^
                    ", ptr " ^ slot)
            val _ = emit ("br label %" ^ doneLabel)
            val _ = emit (okLabel ^ ":")

            val _ = emit (bufferSlot ^ " = alloca ptr")
            val _ = emit (lengthSlot ^ " = alloca i64")
            val _ = emit (capacitySlot ^ " = alloca i64")
            val initialMemory = allocString "4096"
            val _ = emit ("store ptr " ^ initialMemory ^ ", ptr " ^ bufferSlot)
            val _ = emit ("store i64 0, ptr " ^ lengthSlot)
            val _ = emit ("store i64 4096, ptr " ^ capacitySlot)
            val _ = emit ("br label %" ^ loopLabel)
            val _ = emit (loopLabel ^ ":")
            val _ = emit (memory ^ " = load ptr, ptr " ^ bufferSlot)
            val _ = emit (length ^ " = load i64, ptr " ^ lengthSlot)
            val _ = emit (capacity ^ " = load i64, ptr " ^ capacitySlot)
            val _ =
              emit (remaining ^ " = sub i64 " ^ capacity ^ ", " ^ length)
            val _ =
              emit (destination ^ " = getelementptr i8, ptr " ^ memory ^
                    ", i64 " ^ length)
            val _ =
              emit (count ^ " = call i64 @fread(ptr " ^ destination ^
                    ", i64 1, i64 " ^ remaining ^ ", ptr " ^ fp ^ ")")
            val _ =
              emit (nextLength ^ " = add i64 " ^ length ^ ", " ^ count)
            val _ = emit ("store i64 " ^ nextLength ^ ", ptr " ^ lengthSlot)
            val _ = emit (isZero ^ " = icmp eq i64 " ^ count ^ ", 0")
            val _ =
              emit ("br i1 " ^ isZero ^ ", label %" ^ finishLabel ^
                    ", label %" ^ checkLabel)
            val _ = emit (checkLabel ^ ":")
            val _ =
              emit (isFull ^ " = icmp eq i64 " ^ nextLength ^ ", " ^ capacity)
            val _ =
              emit ("br i1 " ^ isFull ^ ", label %" ^ growLabel ^
                    ", label %" ^ loopLabel)
            val _ = emit (growLabel ^ ":")
            val _ =
              emit (nextCapacity ^ " = shl i64 " ^ capacity ^ ", 1")
            val grown = allocString nextCapacity
            val _ =
              emit ("call ptr @memcpy(ptr " ^ grown ^ ", ptr " ^ memory ^
                    ", i64 " ^ nextLength ^ ")")
            val _ = emit ("call void @yack_free(ptr " ^ memory ^ ")")
            val _ = emit ("store ptr " ^ grown ^ ", ptr " ^ bufferSlot)
            val _ =
              emit ("store i64 " ^ nextCapacity ^ ", ptr " ^ capacitySlot)
            val _ = emit ("br label %" ^ loopLabel)
            val _ = emit (finishLabel ^ ":")
            val _ = emit ("call i32 @fclose(ptr " ^ fp ^ ")")
            val finalMemory = fresh "io.read.finalmem"
            val finalLength = fresh "io.read.finallen"
            val _ = emit (finalMemory ^ " = load ptr, ptr " ^ bufferSlot)
            val _ = emit (finalLength ^ " = load i64, ptr " ^ lengthSlot)
            val _ =
              emit (endPtr ^ " = getelementptr i8, ptr " ^ finalMemory ^
                    ", i64 " ^ finalLength)
            val _ = emit ("store i8 0, ptr " ^ endPtr)
            val _ =
              emit (v0 ^ " = insertvalue { ptr, i64, i1 } undef, ptr " ^
                    finalMemory ^ ", 0")
            val _ =
              emit (v1 ^ " = insertvalue { ptr, i64, i1 } " ^ v0 ^
                    ", i64 " ^ finalLength ^ ", 1")
            val _ =
              emit (v2 ^ " = insertvalue { ptr, i64, i1 } " ^ v1 ^
                    ", i1 true, 2")
            val successValue =
              ioResult "Ok" { text = v2, ty = Ir.TString, const = false }
            val _ =
              emit ("store " ^ llvmTy resultTy ^ " " ^
                    #text successValue ^ ", ptr " ^ slot)
            val _ = emit ("br label %" ^ doneLabel)
            val _ = emit (doneLabel ^ ":")
            val _ =
              emit (result ^ " = load " ^ llvmTy resultTy ^ ", ptr " ^ slot)
            val _ = emit ("call void @yack_free(ptr " ^ path ^ ")")
            val _ = freeAfter (value, consume)
          in
            { text = result, ty = resultTy, const = false }
          end
      | ("io.read_stdin", []) =>
          let
            val bufferSlot = fresh "io.stdin.bufslot"
            val lengthSlot = fresh "io.stdin.lenslot"
            val capacitySlot = fresh "io.stdin.capslot"
            val loopLabel = freshLabel "io.stdin.loop"
            val growLabel = freshLabel "io.stdin.grow"
            val doneLabel = freshLabel "io.stdin.done"
            val checkLabel = freshLabel "io.stdin.check"
            val memory = fresh "io.stdin.mem"
            val length = fresh "io.stdin.len"
            val capacity = fresh "io.stdin.cap"
            val remaining = fresh "io.stdin.rem"
            val destination = fresh "io.stdin.dst"
            val stdin = fresh "io.stdin.file"
            val count = fresh "io.stdin.count"
            val nextLength = fresh "io.stdin.nextlen"
            val isZero = fresh "io.stdin.zero"
            val isFull = fresh "io.stdin.full"
            val nextCapacity = fresh "io.stdin.nextcap"
            val endPtr = fresh "io.stdin.end"
            val v0 = fresh "io.stdin.v0"
            val v1 = fresh "io.stdin.v1"
            val v2 = fresh "io.stdin.v2"
            val _ = emit (bufferSlot ^ " = alloca ptr")
            val _ = emit (lengthSlot ^ " = alloca i64")
            val _ = emit (capacitySlot ^ " = alloca i64")
            val initialMemory = allocString "4096"
            val _ = emit ("store ptr " ^ initialMemory ^ ", ptr " ^ bufferSlot)
            val _ = emit ("store i64 0, ptr " ^ lengthSlot)
            val _ = emit ("store i64 4096, ptr " ^ capacitySlot)
            val _ = emit ("br label %" ^ loopLabel)
            val _ = emit (loopLabel ^ ":")
            val _ = emit (memory ^ " = load ptr, ptr " ^ bufferSlot)
            val _ = emit (length ^ " = load i64, ptr " ^ lengthSlot)
            val _ = emit (capacity ^ " = load i64, ptr " ^ capacitySlot)
            val _ =
              emit (remaining ^ " = sub i64 " ^ capacity ^ ", " ^ length)
            val _ =
              emit (destination ^ " = getelementptr i8, ptr " ^ memory ^
                    ", i64 " ^ length)
            val _ = emit (stdin ^ " = load ptr, ptr @stdin")
            val _ =
              emit (count ^ " = call i64 @fread(ptr " ^ destination ^
                    ", i64 1, i64 " ^ remaining ^ ", ptr " ^ stdin ^ ")")
            val _ =
              emit (nextLength ^ " = add i64 " ^ length ^ ", " ^ count)
            val _ = emit ("store i64 " ^ nextLength ^ ", ptr " ^ lengthSlot)
            val _ = emit (isZero ^ " = icmp eq i64 " ^ count ^ ", 0")
            val _ =
              emit ("br i1 " ^ isZero ^ ", label %" ^ doneLabel ^
                    ", label %" ^ checkLabel)
            val _ = emit (checkLabel ^ ":")
            val _ =
              emit (isFull ^ " = icmp eq i64 " ^ nextLength ^ ", " ^ capacity)
            val _ =
              emit ("br i1 " ^ isFull ^ ", label %" ^ growLabel ^
                    ", label %" ^ loopLabel)
            val _ = emit (growLabel ^ ":")
            val _ =
              emit (nextCapacity ^ " = shl i64 " ^ capacity ^ ", 1")
            val grown = allocString nextCapacity
            val _ =
              emit ("call ptr @memcpy(ptr " ^ grown ^ ", ptr " ^ memory ^
                    ", i64 " ^ nextLength ^ ")")
            val _ = emit ("call void @yack_free(ptr " ^ memory ^ ")")
            val _ = emit ("store ptr " ^ grown ^ ", ptr " ^ bufferSlot)
            val _ =
              emit ("store i64 " ^ nextCapacity ^ ", ptr " ^ capacitySlot)
            val _ = emit ("br label %" ^ loopLabel)
            val _ = emit (doneLabel ^ ":")
            val finalMemory = fresh "io.stdin.finalmem"
            val finalLength = fresh "io.stdin.finallen"
            val _ = emit (finalMemory ^ " = load ptr, ptr " ^ bufferSlot)
            val _ = emit (finalLength ^ " = load i64, ptr " ^ lengthSlot)
            val _ =
              emit (endPtr ^ " = getelementptr i8, ptr " ^ finalMemory ^
                    ", i64 " ^ finalLength)
            val _ = emit ("store i8 0, ptr " ^ endPtr)
            val _ =
              emit (v0 ^ " = insertvalue { ptr, i64, i1 } undef, ptr " ^
                    finalMemory ^ ", 0")
            val _ =
              emit (v1 ^ " = insertvalue { ptr, i64, i1 } " ^ v0 ^
                    ", i64 " ^ finalLength ^ ", 1")
            val _ =
              emit (v2 ^ " = insertvalue { ptr, i64, i1 } " ^ v1 ^
                    ", i1 true, 2")
            val successValue = ioResult "Ok" { text = v2, ty = Ir.TString, const = false }
          in
            successValue
          end
      | ("io.write_file", [(pathValue, pathConsume), (content, contentConsume)]) =>
          let
            val slot = allocStack resultTy
            val path = cString pathValue
            val data = fresh "io.write.data"
            val length = fresh "io.write.len"
            val fp = fresh "io.write.fp"
            val isNull = fresh "io.write.null"
            val nullLabel = freshLabel "io.write.null"
            val okLabel = freshLabel "io.write.ok"
            val doneLabel = freshLabel "io.write.done"
            val _ =
              emit (data ^ " = extractvalue { ptr, i64, i1 } " ^
                    #text content ^ ", 0")
            val _ =
              emit (length ^ " = extractvalue { ptr, i64, i1 } " ^
                    #text content ^ ", 1")
            val _ =
              emit (fp ^ " = call ptr @fopen(ptr " ^ path ^
                    ", ptr @.mode_wb)")
            val _ =
              emit (isNull ^ " = icmp eq ptr " ^ fp ^ ", null")
            val _ =
              emit ("br i1 " ^ isNull ^ ", label %" ^ nullLabel ^
                    ", label %" ^ okLabel)
            val _ = emit (nullLabel ^ ":")
            val errorValue =
              ioResult "Error" (ioErrorValue "cannot write file")
            val _ =
              emit ("store " ^ llvmTy resultTy ^ " " ^ #text errorValue ^
                    ", ptr " ^ slot)
            val _ = emit ("br label %" ^ doneLabel)
            val _ = emit (okLabel ^ ":")
            val _ =
              emit ("call i64 @fwrite(ptr " ^ data ^ ", i64 1, i64 " ^
                    length ^ ", ptr " ^ fp ^ ")")
            val _ = emit ("call i32 @fclose(ptr " ^ fp ^ ")")
            val successValue = ioResult "Ok" unitValue
            val _ =
              emit ("store " ^ llvmTy resultTy ^ " " ^
                    #text successValue ^ ", ptr " ^ slot)
            val _ = emit ("br label %" ^ doneLabel)
            val _ = emit (doneLabel ^ ":")
            val result = fresh "io.write.result"
            val _ =
              emit (result ^ " = load " ^ llvmTy resultTy ^ ", ptr " ^ slot)
            val _ = emit ("call void @yack_free(ptr " ^ path ^ ")")
            val _ = freeAfter (pathValue, pathConsume)
            val _ = freeAfter (content, contentConsume)
          in
            { text = result, ty = resultTy, const = false }
          end
      | ("string_of_char", [(value, _)]) =>
          let
            val byte = fresh "str.char.byte"
            val endPtr = fresh "str.char.end"
            val v0 = fresh "str.char.v0"
            val v1 = fresh "str.char.v1"
            val v2 = fresh "str.char.v2"
            val memory = allocString "2"
            val _ = emit (byte ^ " = trunc i32 " ^ #text value ^ " to i8")
            val _ = emit ("store i8 " ^ byte ^ ", ptr " ^ memory)
            val _ = emit (endPtr ^ " = getelementptr i8, ptr " ^ memory ^
                          ", i64 1")
            val _ = emit ("store i8 0, ptr " ^ endPtr)
            val _ = emit (v0 ^ " = insertvalue { ptr, i64, i1 } undef, ptr " ^
                          memory ^ ", 0")
            val _ = emit (v1 ^ " = insertvalue { ptr, i64, i1 } " ^ v0 ^
                          ", i64 1, 1")
            val _ = emit (v2 ^ " = insertvalue { ptr, i64, i1 } " ^ v1 ^
                          ", i1 true, 2")
          in
            { text = v2, ty = resultTy, const = false }
          end
      | ("string_of_int", [(value, _)]) =>
          let
            val length = fresh "str.int.len"
            val length64 = fresh "str.int.len64"
            val v0 = fresh "str.int.v0"
            val v1 = fresh "str.int.v1"
            val v2 = fresh "str.int.v2"
            val memory = allocString "32"
            val _ = emit (length ^ " = call i32 @snprintf(ptr " ^ memory ^
                          ", i64 32, ptr @.fmt_int_plain, i32 " ^
                          #text value ^ ")")
            val _ = emit (length64 ^ " = sext i32 " ^ length ^ " to i64")
            val _ = emit (v0 ^ " = insertvalue { ptr, i64, i1 } undef, ptr " ^
                          memory ^ ", 0")
            val _ = emit (v1 ^ " = insertvalue { ptr, i64, i1 } " ^ v0 ^
                          ", i64 " ^ length64 ^ ", 1")
            val _ = emit (v2 ^ " = insertvalue { ptr, i64, i1 } " ^ v1 ^
                          ", i1 true, 2")
          in
            { text = v2, ty = resultTy, const = false }
          end
      | ("float_to_string", [(value, _)]) =>
          let
            val length = fresh "str.float.len"
            val length64 = fresh "str.float.len64"
            val v0 = fresh "str.float.v0"
            val v1 = fresh "str.float.v1"
            val v2 = fresh "str.float.v2"
            val memory = allocString "64"
            val _ = emit (length ^ " = call i32 @snprintf(ptr " ^ memory ^
                          ", i64 64, ptr @.fmt_float_plain, double " ^
                          #text value ^ ")")
            val _ = emit (length64 ^ " = sext i32 " ^ length ^ " to i64")
            val _ = emit (v0 ^ " = insertvalue { ptr, i64, i1 } undef, ptr " ^
                          memory ^ ", 0")
            val _ = emit (v1 ^ " = insertvalue { ptr, i64, i1 } " ^ v0 ^
                          ", i64 " ^ length64 ^ ", 1")
            val _ = emit (v2 ^ " = insertvalue { ptr, i64, i1 } " ^ v1 ^
                          ", i1 true, 2")
          in
            { text = v2, ty = resultTy, const = false }
          end
      | ("string_copy", [(value, _)]) =>
          let
            val data = fresh "str.copy.data"
            val length = fresh "str.copy.len"
            val size = fresh "str.copy.size"
            val endPtr = fresh "str.copy.end"
            val v0 = fresh "str.copy.v0"
            val v1 = fresh "str.copy.v1"
            val v2 = fresh "str.copy.v2"
            val _ =
              emit (data ^ " = extractvalue { ptr, i64, i1 } " ^
                    #text value ^ ", 0")
            val _ =
              emit (length ^ " = extractvalue { ptr, i64, i1 } " ^
                    #text value ^ ", 1")
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
      | ("string_length", [(value, consume)]) =>
          let
            val length = fresh "str.len"
            val length32 = fresh "str.len32"
            val _ =
              emit (length ^ " = extractvalue { ptr, i64, i1 } " ^
                    #text value ^ ", 1")
            val _ =
              emit (length32 ^ " = trunc i64 " ^ length ^ " to i32")
            val _ = freeAfter (value, consume)
          in
            { text = length32, ty = Ir.Int 32, const = false }
          end
      | (conversionName, [(value, _)]) =>
          (case conversionTarget conversionName of
             SOME (true, width) => lowerIntToString (width, value)
           | SOME (false, width) => lowerStringToInt (width, value)
           | NONE => unitValue)
      | ("string_char_at_unchecked", [(value, consume), (index, _)]) =>
          let
            val data = fresh "str.udata"
            val index64 = fresh "str.uindex"
            val pointer = fresh "str.uptr"
            val byte = fresh "str.ubyte"
            val char = fresh "str.uchar"
            val _ =
              emit (data ^ " = extractvalue { ptr, i64, i1 } " ^
                    #text value ^ ", 0")
            val _ =
              emit (index64 ^ " = sext i32 " ^ #text index ^ " to i64")
            val length = fresh "str.ulen"
            val outside = fresh "str.uoutside"
            val abortLabel = freshLabel "str.uoob"
            val okLabel = freshLabel "str.uinbounds"
            val _ =
              emit (length ^ " = extractvalue { ptr, i64, i1 } " ^
                    #text value ^ ", 1")
            val _ =
              emit (outside ^ " = icmp uge i64 " ^ index64 ^ ", " ^ length)
            val _ =
              emit ("br i1 " ^ outside ^ ", label %" ^ abortLabel ^
                    ", label %" ^ okLabel ^
                    ", !prof !{!\"branch_weights\", i32 1, i32 1048575}")
            val _ = emit (abortLabel ^ ":")
            val _ =
              emit ("call void @yack_abort_bounds(i64 " ^ index64 ^
                    ", i64 " ^ length ^ ")")
            val _ = emit "unreachable"
            val _ = emit (okLabel ^ ":")
            val _ =
              emit (pointer ^ " = getelementptr i8, ptr " ^ data ^
                    ", i64 " ^ index64)
            val _ = emit (byte ^ " = load i8, ptr " ^ pointer)
            val _ = emit (char ^ " = zext i8 " ^ byte ^ " to i32")
            val _ = freeAfter (value, consume)
          in
            { text = char, ty = Ir.Int 32, const = false }
          end
      | ("string_char_at", [(value, consume), (index, _)]) =>
          let
            val data = fresh "str.data"
            val length = fresh "str.len"
            val index64 = fresh "str.index"
            val negative = fresh "str.neg"
            val high = fresh "str.high"
            val bad = fresh "str.bad"
            val loadLabel = freshLabel "str.char.load"
            val outLabel = freshLabel "str.char.out"
            val doneLabel = freshLabel "str.char.done"
            val bytePtr = fresh "str.byteptr"
            val byte = fresh "str.byte"
            val char = fresh "str.char"
            val result = fresh "str.charcode"
            val _ =
              emit (data ^ " = extractvalue { ptr, i64, i1 } " ^
                    #text value ^ ", 0")
            val _ =
              emit (length ^ " = extractvalue { ptr, i64, i1 } " ^
                    #text value ^ ", 1")
            val _ =
              emit (index64 ^ " = sext i32 " ^ #text index ^ " to i64")
            val _ =
              emit (negative ^ " = icmp slt i64 " ^ index64 ^ ", 0")
            val _ =
              emit (high ^ " = icmp sge i64 " ^ index64 ^ ", " ^ length)
            val _ =
              emit (bad ^ " = or i1 " ^ negative ^ ", " ^ high)
            val _ =
              emit ("br i1 " ^ bad ^ ", label %" ^ outLabel ^
                    ", label %" ^ loadLabel)
            val _ = emit (loadLabel ^ ":")
            val _ =
              emit (bytePtr ^ " = getelementptr i8, ptr " ^ data ^
                    ", i64 " ^ index64)
            val _ = emit (byte ^ " = load i8, ptr " ^ bytePtr)
            val _ = emit (char ^ " = zext i8 " ^ byte ^ " to i32")
            val _ = emit ("br label %" ^ doneLabel)
            val _ = emit (outLabel ^ ":")
            val _ = emit ("br label %" ^ doneLabel)
            val _ = emit (doneLabel ^ ":")
            val _ =
              emit (result ^ " = phi i32 [ " ^ char ^ ", %" ^ loadLabel ^
                    " ], [ -1, %" ^ outLabel ^ " ]")
            val _ = freeAfter (value, consume)
          in
            { text = result, ty = Ir.Int 32, const = false }
          end
      | ("string_slice",
         [(value, consume), (startIndex, _), (stopIndex, _)]) =>
          let
            val data = fresh "str.data"
            val length = fresh "str.len"
            val owned = fresh "str.owned"
            val start64 = fresh "str.start"
            val stop64 = fresh "str.stop"
            val startNeg = fresh "str.startneg"
            val startLow = fresh "str.startlow"
            val startHigh = fresh "str.starthigh"
            val startFinal = fresh "str.startfinal"
            val stopNeg = fresh "str.stopneg"
            val stopLow = fresh "str.stoplow"
            val stopHigh = fresh "str.stophigh"
            val stopFinal = fresh "str.stopfinal"
            val diff = fresh "str.diff"
            val negative = fresh "str.sliceneg"
            val sliceLen = fresh "str.slicelen"
            val size = fresh "str.slicesize"
            val sourcePtr = fresh "str.slicesrc"
            val endPtr = fresh "str.sliceend"
            val copyLabel = freshLabel "str.slice.copy"
            val viewLabel = freshLabel "str.slice.view"
            val doneLabel = freshLabel "str.slice.done"
            val copy0 = fresh "str.copy.v0"
            val copy1 = fresh "str.copy.v1"
            val copy2 = fresh "str.copy.v2"
            val view0 = fresh "str.view.v0"
            val view1 = fresh "str.view.v1"
            val view2 = fresh "str.view.v2"
            val result = fresh "str.slice"
            val _ =
              emit (data ^ " = extractvalue { ptr, i64, i1 } " ^
                    #text value ^ ", 0")
            val _ =
              emit (length ^ " = extractvalue { ptr, i64, i1 } " ^
                    #text value ^ ", 1")
            val _ =
              emit (owned ^ " = extractvalue { ptr, i64, i1 } " ^
                    #text value ^ ", 2")
            val _ =
              emit (start64 ^ " = sext i32 " ^ #text startIndex ^
                    " to i64")
            val _ =
              emit (stop64 ^ " = sext i32 " ^ #text stopIndex ^
                    " to i64")
            val _ =
              emit (startNeg ^ " = icmp slt i64 " ^ start64 ^ ", 0")
            val _ =
              emit (startLow ^ " = select i1 " ^ startNeg ^
                    ", i64 0, i64 " ^ start64)
            val _ =
              emit (startHigh ^ " = icmp sgt i64 " ^ startLow ^
                    ", " ^ length)
            val _ =
              emit (startFinal ^ " = select i1 " ^ startHigh ^
                    ", i64 " ^ length ^ ", i64 " ^ startLow)
            val _ =
              emit (stopNeg ^ " = icmp slt i64 " ^ stop64 ^ ", 0")
            val _ =
              emit (stopLow ^ " = select i1 " ^ stopNeg ^
                    ", i64 0, i64 " ^ stop64)
            val _ =
              emit (stopHigh ^ " = icmp sgt i64 " ^ stopLow ^
                    ", " ^ length)
            val _ =
              emit (stopFinal ^ " = select i1 " ^ stopHigh ^
                    ", i64 " ^ length ^ ", i64 " ^ stopLow)
            val _ =
              emit (diff ^ " = sub i64 " ^ stopFinal ^ ", " ^ startFinal)
            val _ =
              emit (negative ^ " = icmp slt i64 " ^ diff ^ ", 0")
            val _ =
              emit (sliceLen ^ " = select i1 " ^ negative ^
                    ", i64 0, i64 " ^ diff)
            val _ =
              emit (sourcePtr ^ " = getelementptr i8, ptr " ^ data ^
                    ", i64 " ^ startFinal)
            val _ =
              emit ("br i1 " ^ owned ^ ", label %" ^ copyLabel ^
                    ", label %" ^ viewLabel)
            val _ = emit (copyLabel ^ ":")
            val _ =
              emit (size ^ " = add i64 " ^ sliceLen ^ ", 1")
            val memory = allocString size
            val _ =
              emit ("call ptr @memcpy(ptr " ^ memory ^ ", ptr " ^
                    sourcePtr ^ ", i64 " ^ sliceLen ^ ")")
            val _ =
              emit (endPtr ^ " = getelementptr i8, ptr " ^ memory ^
                    ", i64 " ^ sliceLen)
            val _ = emit ("store i8 0, ptr " ^ endPtr)
            val _ =
              emit (copy0 ^ " = insertvalue { ptr, i64, i1 } undef, ptr " ^
                    memory ^ ", 0")
            val _ =
              emit (copy1 ^ " = insertvalue { ptr, i64, i1 } " ^ copy0 ^
                    ", i64 " ^ sliceLen ^ ", 1")
            val _ =
              emit (copy2 ^ " = insertvalue { ptr, i64, i1 } " ^ copy1 ^
                    ", i1 true, 2")
            val _ = emit ("br label %" ^ doneLabel)
            val _ = emit (viewLabel ^ ":")
            val _ =
              emit (view0 ^ " = insertvalue { ptr, i64, i1 } undef, ptr " ^
                    sourcePtr ^ ", 0")
            val _ =
              emit (view1 ^ " = insertvalue { ptr, i64, i1 } " ^ view0 ^
                    ", i64 " ^ sliceLen ^ ", 1")
            val _ =
              emit (view2 ^ " = insertvalue { ptr, i64, i1 } " ^ view1 ^
                    ", i1 false, 2")
            val _ = emit ("br label %" ^ doneLabel)
            val _ = emit (doneLabel ^ ":")
            val _ =
              emit (result ^ " = phi { ptr, i64, i1 } [ " ^ copy2 ^
                    ", %" ^ copyLabel ^ " ], [ " ^ view2 ^
                    ", %" ^ viewLabel ^ " ]")
            val _ = freeAfter (value, consume)
          in
            { text = result, ty = resultTy, const = false }
          end
      | ("string_find_char_from",
         [(value, consume), (startIndex, _), (needle, _)]) =>
          let
            val data = fresh "str.findfdata"
            val length = fresh "str.findflen"
            val start64 = fresh "str.findfstart"
            val startNeg = fresh "str.findfneg"
            val startLow = fresh "str.findflow"
            val startHigh = fresh "str.findfhigh"
            val startFinal = fresh "str.findffinal"
            val remaining = fresh "str.findfremaining"
            val search = fresh "str.findfsearch"
            val found = fresh "str.findffound"
            val isNull = fresh "str.findfnull"
            val dataInt = fresh "str.findfbase"
            val foundInt = fresh "str.findfptr"
            val offset = fresh "str.findfoff"
            val offset32 = fresh "str.findfoff32"
            val result = fresh "str.findfresult"
            val _ =
              emit (data ^ " = extractvalue { ptr, i64, i1 } " ^
                    #text value ^ ", 0")
            val _ =
              emit (length ^ " = extractvalue { ptr, i64, i1 } " ^
                    #text value ^ ", 1")
            val _ =
              emit (start64 ^ " = sext i32 " ^ #text startIndex ^
                    " to i64")
            val _ =
              emit (startNeg ^ " = icmp slt i64 " ^ start64 ^ ", 0")
            val _ =
              emit (startLow ^ " = select i1 " ^ startNeg ^
                    ", i64 0, i64 " ^ start64)
            val _ =
              emit (startHigh ^ " = icmp sgt i64 " ^ startLow ^
                    ", " ^ length)
            val _ =
              emit (startFinal ^ " = select i1 " ^ startHigh ^
                    ", i64 " ^ length ^ ", i64 " ^ startLow)
            val _ =
              emit (remaining ^ " = sub i64 " ^ length ^ ", " ^ startFinal)
            val _ =
              emit (search ^ " = getelementptr i8, ptr " ^ data ^
                    ", i64 " ^ startFinal)
            val _ =
              emit (found ^ " = call ptr @memchr(ptr " ^ search ^
                    ", i32 " ^ #text needle ^ ", i64 " ^ remaining ^ ")")
            val _ =
              emit (isNull ^ " = icmp eq ptr " ^ found ^ ", null")
            val _ = emit (dataInt ^ " = ptrtoint ptr " ^ data ^ " to i64")
            val _ =
              emit (foundInt ^ " = ptrtoint ptr " ^ found ^ " to i64")
            val _ =
              emit (offset ^ " = sub i64 " ^ foundInt ^ ", " ^ dataInt)
            val _ =
              emit (offset32 ^ " = trunc i64 " ^ offset ^ " to i32")
            val _ =
              emit (result ^ " = select i1 " ^ isNull ^ ", i32 -1, i32 " ^
                    offset32)
            val _ = freeAfter (value, consume)
          in
            { text = result, ty = Ir.Int 32, const = false }
          end
      | ("string_find_char", [(value, consume), (needle, _)]) =>
          let
            val data = fresh "str.finddata"
            val length = fresh "str.findlen"
            val found = fresh "str.found"
            val isNull = fresh "str.findnull"
            val dataInt = fresh "str.findbase"
            val foundInt = fresh "str.findptr"
            val offset = fresh "str.findoff"
            val offset32 = fresh "str.findoff32"
            val result = fresh "str.findresult"
            val _ =
              emit (data ^ " = extractvalue { ptr, i64, i1 } " ^
                    #text value ^ ", 0")
            val _ =
              emit (length ^ " = extractvalue { ptr, i64, i1 } " ^
                    #text value ^ ", 1")
            val _ =
              emit (found ^ " = call ptr @memchr(ptr " ^ data ^
                    ", i32 " ^ #text needle ^ ", i64 " ^ length ^ ")")
            val _ =
              emit (isNull ^ " = icmp eq ptr " ^ found ^ ", null")
            val _ = emit (dataInt ^ " = ptrtoint ptr " ^ data ^ " to i64")
            val _ =
              emit (foundInt ^ " = ptrtoint ptr " ^ found ^ " to i64")
            val _ =
              emit (offset ^ " = sub i64 " ^ foundInt ^ ", " ^ dataInt)
            val _ =
              emit (offset32 ^ " = trunc i64 " ^ offset ^ " to i32")
            val _ =
              emit (result ^ " = select i1 " ^ isNull ^ ", i32 -1, i32 " ^
                    offset32)
            val _ = freeAfter (value, consume)
          in
            { text = result, ty = Ir.Int 32, const = false }
          end
      | ("print_span",
         [(value, consume), (startIndex, _), (lengthIndex, _)]) =>
          let
            val data = fresh "str.spandata"
            val length = fresh "str.spanlen"
            val start64 = fresh "str.spanstart"
            val len64 = fresh "str.spanlength"
            val startNeg = fresh "str.spanstartneg"
            val startLow = fresh "str.spanstartlow"
            val startHigh = fresh "str.spanstarthigh"
            val startFinal = fresh "str.spanstartfinal"
            val lenNeg = fresh "str.spanlenneg"
            val lenLow = fresh "str.spanlenlow"
            val remaining = fresh "str.spanremaining"
            val lenHigh = fresh "str.spanlenhigh"
            val lenFinal = fresh "str.spanlenfinal"
            val available = fresh "str.spanavail"
            val lenAfterStart = fresh "str.spanavail2"
            val negative = fresh "str.spanneg"
            val sliceLen = fresh "str.spanslicelen"
            val ptr = fresh "str.spanptr"
            val len32 = fresh "str.spanlen32"
            val _ =
              emit (data ^ " = extractvalue { ptr, i64, i1 } " ^
                    #text value ^ ", 0")
            val _ =
              emit (length ^ " = extractvalue { ptr, i64, i1 } " ^
                    #text value ^ ", 1")
            val _ =
              emit (start64 ^ " = sext i32 " ^ #text startIndex ^
                    " to i64")
            val _ =
              emit (len64 ^ " = sext i32 " ^ #text lengthIndex ^
                    " to i64")
            val _ =
              emit (startNeg ^ " = icmp slt i64 " ^ start64 ^ ", 0")
            val _ =
              emit (startLow ^ " = select i1 " ^ startNeg ^
                    ", i64 0, i64 " ^ start64)
            val _ =
              emit (startHigh ^ " = icmp sgt i64 " ^ startLow ^
                    ", " ^ length)
            val _ =
              emit (startFinal ^ " = select i1 " ^ startHigh ^
                    ", i64 " ^ length ^ ", i64 " ^ startLow)
            val _ =
              emit (lenNeg ^ " = icmp slt i64 " ^ len64 ^ ", 0")
            val _ =
              emit (lenLow ^ " = select i1 " ^ lenNeg ^
                    ", i64 0, i64 " ^ len64)
            val _ =
              emit (remaining ^ " = sub i64 " ^ length ^ ", " ^ startFinal)
            val _ =
              emit (lenHigh ^ " = icmp sgt i64 " ^ lenLow ^
                    ", " ^ remaining)
            val _ =
              emit (lenFinal ^ " = select i1 " ^ lenHigh ^
                    ", i64 " ^ remaining ^ ", i64 " ^ lenLow)
            val _ =
              emit (ptr ^ " = getelementptr i8, ptr " ^ data ^
                    ", i64 " ^ startFinal)
            val _ =
              emit (len32 ^ " = trunc i64 " ^ lenFinal ^ " to i32")
            val _ =
              emit ("call i32 (ptr, ...) @printf(ptr @.fmt_span, i32 " ^
                    len32 ^ ", ptr " ^ ptr ^ ")")
            val _ = freeAfter (value, consume)
          in
            unitValue
          end
      | ("string_eq", [(left, leftConsume), (right, rightConsume)]) =>
          let
            val leftData = fresh "str.eqldata"
            val leftLen = fresh "str.eqllen"
            val rightData = fresh "str.eqrdata"
            val rightLen = fresh "str.eqrlen"
            val sameLength = fresh "str.eqlen"
            val compareLabel = freshLabel "str.eq.compare"
            val falseLabel = freshLabel "str.eq.false"
            val doneLabel = freshLabel "str.eq.done"
            val cmp = fresh "str.eqcmp"
            val eq = fresh "str.eq"
            val result = fresh "str.eqresult"
            val _ =
              emit (leftData ^ " = extractvalue { ptr, i64, i1 } " ^
                    #text left ^ ", 0")
            val _ =
              emit (leftLen ^ " = extractvalue { ptr, i64, i1 } " ^
                    #text left ^ ", 1")
            val _ =
              emit (rightData ^ " = extractvalue { ptr, i64, i1 } " ^
                    #text right ^ ", 0")
            val _ =
              emit (rightLen ^ " = extractvalue { ptr, i64, i1 } " ^
                    #text right ^ ", 1")
            val _ =
              emit (sameLength ^ " = icmp eq i64 " ^ leftLen ^ ", " ^
                    rightLen)
            val _ =
              emit ("br i1 " ^ sameLength ^ ", label %" ^ compareLabel ^
                    ", label %" ^ falseLabel)
            val _ = emit (compareLabel ^ ":")
            val _ =
              emit (cmp ^ " = call i32 @memcmp(ptr " ^ leftData ^
                    ", ptr " ^ rightData ^ ", i64 " ^ leftLen ^ ")")
            val _ = emit (eq ^ " = icmp eq i32 " ^ cmp ^ ", 0")
            val _ = emit ("br label %" ^ doneLabel)
            val _ = emit (falseLabel ^ ":")
            val _ = emit ("br label %" ^ doneLabel)
            val _ = emit (doneLabel ^ ":")
            val _ =
              emit (result ^ " = phi i1 [ " ^ eq ^ ", %" ^ compareLabel ^
                    " ], [ false, %" ^ falseLabel ^ " ]")
            val _ = freeAfter (left, leftConsume)
            val _ = freeAfter (right, rightConsume)
          in
            { text = result, ty = Ir.Bool, const = false }
          end
      | _ => unitValue
    end

end
