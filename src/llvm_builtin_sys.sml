structure LlvmBuiltinSys =
struct
  open LlvmTypes

  type env =
    { emit : string -> unit
    , fresh : string -> string
    , freshLabel : string -> string
    , lowerExpr : localEntry list -> Ir.expr -> value
    , allocString : string -> string
    , stackMode : bool
    , enterArena : unit -> unit
    , leaveArena : unit -> unit
    , arenaMark : unit -> unit
    , arenaRelease : unit -> unit
    }

  fun lower (env : env) (locals, name, arguments, resultTy) =
let
  val emit = #emit env
  val fresh = #fresh env
  val freshLabel = #freshLabel env
  val lowerExpr = #lowerExpr env
  val allocString = #allocString env
  val stackMode = #stackMode env
  val enterArena = #enterArena env
  val leaveArena = #leaveArena env
  val arenaMark = #arenaMark env
  val arenaRelease = #arenaRelease env
  val lowered = List.map (lowerExpr locals) arguments
      val result = fn text => { text = text, ty = resultTy, const = false }
      val headerTy = "{ ptr, i64, i64, ptr }"

      fun stringData value =
        let val reg = fresh "sqlite.str" in
          emit (reg ^ " = extractvalue { ptr, i64, i1 } " ^
                #text value ^ ", 0");
          reg
        end

      fun makeString pointer length =
        let
          val v0 = fresh "sqlite.s0"
          val v1 = fresh "sqlite.s1"
          val v2 = fresh "sqlite.s2"
          val _ = emit (v0 ^ " = insertvalue { ptr, i64, i1 } undef, ptr " ^
                        pointer ^ ", 0")
          val _ = emit (v1 ^ " = insertvalue { ptr, i64, i1 } " ^ v0 ^
                        ", i64 " ^ length ^ ", 1")
          val _ = emit (v2 ^ " = insertvalue { ptr, i64, i1 } " ^ v1 ^
                        ", i1 true, 2")
        in
          { text = v2, ty = resultTy, const = false }
        end

      fun handlePointer value =
        let val reg = fresh "sqlite.ptr" in
          emit (reg ^ " = inttoptr i64 " ^ #text value ^ " to ptr");
          reg
        end

      fun handleInteger pointer =
        let val reg = fresh "sqlite.handle" in
          emit (reg ^ " = ptrtoint ptr " ^ pointer ^ " to i64");
          reg
        end

      fun outPointer () =
        let val slot = fresh "sqlite.out" in
          emit (slot ^ " = alloca ptr");
          emit ("store ptr null, ptr " ^ slot);
          slot
        end

      fun loadPointer slot =
        let val reg = fresh "sqlite.load" in
          emit (reg ^ " = load ptr, ptr " ^ slot);
          reg
        end

      fun copyCString (pointer, length) =
        let
          val size = fresh "sqlite.size"
          val memory = fresh "sqlite.mem"
          val endPointer = fresh "sqlite.end"
          val _ = emit (size ^ " = add i64 " ^ length ^ ", 1")
          val _ = emit (memory ^ " = call ptr @yack_string_alloc(i64 " ^ size ^ ")")
          val _ = emit ("call ptr @memcpy(ptr " ^ memory ^ ", ptr " ^ pointer ^
                        ", i64 " ^ length ^ ")")
          val _ = emit (endPointer ^ " = getelementptr i8, ptr " ^ memory ^
                        ", i64 " ^ length)
          val _ = emit ("store i8 0, ptr " ^ endPointer)
        in makeString (memory) (length)
        end

      fun copiedCString pointer =
        let val length = fresh "sqlite.len" in
          emit (length ^ " = call i64 @strlen(ptr " ^ pointer ^ ")");
          copyCString (pointer, length)
        end

      fun loadArrayField value index storage =
        let
          val pointer = fresh "sys.fieldp"
          val loaded = fresh "sys.field"
          val _ = emit (pointer ^ " = getelementptr " ^ headerTy ^
                        ", ptr " ^ #text value ^ ", i32 0, i32 " ^
                        Int.toString index)
          val _ = emit (loaded ^ " = load " ^ storage ^ ", ptr " ^ pointer)
        in
          loaded
        end

      fun rawTransfer (callee, flags, fd, data, length, offset, count) =
        let
          val slot = fresh "sys.raw.slot"
          val offset64 = fresh "sys.raw.offset64"
          val count64 = fresh "sys.raw.count64"
          val offsetOk = fresh "sys.raw.offset.ok"
          val countOk = fresh "sys.raw.count.ok"
          val endOffset = fresh "sys.raw.end"
          val endOk = fresh "sys.raw.end.ok"
          val valid1 = fresh "sys.raw.valid1"
          val valid = fresh "sys.raw.valid"
          val validLabel = freshLabel "sys.raw.valid"
          val doneLabel = freshLabel "sys.raw.done"
          val destination = fresh "sys.raw.destination"
          val transferred = fresh "sys.raw.transferred"
          val transferred32 = fresh "sys.raw.transferred32"
          val final = fresh "sys.raw.final"
          val _ = emit (slot ^ " = alloca i32")
          val _ = emit ("store i32 -2, ptr " ^ slot)
          val _ = emit (offset64 ^ " = sext i32 " ^ #text offset ^ " to i64")
          val _ = emit (count64 ^ " = sext i32 " ^ #text count ^ " to i64")
          val _ = emit (offsetOk ^ " = icmp sge i64 " ^ offset64 ^ ", 0")
          val _ = emit (countOk ^ " = icmp sge i64 " ^ count64 ^ ", 0")
          val _ = emit (endOffset ^ " = add i64 " ^ offset64 ^ ", " ^ count64)
          val _ = emit (endOk ^ " = icmp sle i64 " ^ endOffset ^ ", " ^ length)
          val _ = emit (valid1 ^ " = and i1 " ^ offsetOk ^ ", " ^ countOk)
          val _ = emit (valid ^ " = and i1 " ^ valid1 ^ ", " ^ endOk)
          val _ = emit ("br i1 " ^ valid ^ ", label %" ^ validLabel ^
                        ", label %" ^ doneLabel)
          val _ = emit (validLabel ^ ":")
          val _ = emit (destination ^ " = getelementptr i8, ptr " ^ data ^
                        ", i64 " ^ offset64)
          val _ = emit (transferred ^ " = call i64 @" ^ callee ^
                        "(i32 " ^ #text fd ^ ", ptr " ^ destination ^
                        ", i64 " ^ count64 ^ ", i32 " ^ flags ^ ")")
          val _ = emit (transferred32 ^ " = trunc i64 " ^ transferred ^
                        " to i32")
          val _ = emit ("store i32 " ^ transferred32 ^ ", ptr " ^ slot)
          val _ = emit ("br label %" ^ doneLabel)
          val _ = emit (doneLabel ^ ":")
          val _ = emit (final ^ " = load i32, ptr " ^ slot)
        in
          result final
        end

      fun sockaddr (port, address) =
        let
          val slot = fresh "sys.sockaddr"
          val familyPtr = fresh "sys.familyp"
          val port16 = fresh "sys.port16"
          val portNet = fresh "sys.portnet"
          val portPtr = fresh "sys.portp"
          val addressNet = fresh "sys.addressnet"
          val addressPtr = fresh "sys.addressp"
          val zeroPtr = fresh "sys.zerop"
          val _ = emit (slot ^ " = alloca i8, i64 16")
          val _ = emit (familyPtr ^ " = getelementptr i8, ptr " ^ slot ^
                        ", i64 0")
          val _ = emit ("store i16 2, ptr " ^ familyPtr)
          val _ = emit (port16 ^ " = trunc i32 " ^ #text port ^ " to i16")
          val _ = emit (portNet ^ " = call i16 @htons(i16 " ^ port16 ^ ")")
          val _ = emit (portPtr ^ " = getelementptr i8, ptr " ^ slot ^
                        ", i64 2")
          val _ = emit ("store i16 " ^ portNet ^ ", ptr " ^ portPtr)
          val _ = emit (addressNet ^ " = call i32 @htonl(i32 " ^
                        Int.toString address ^ ")")
          val _ = emit (addressPtr ^ " = getelementptr i8, ptr " ^ slot ^
                        ", i64 4")
          val _ = emit ("store i32 " ^ addressNet ^ ", ptr " ^ addressPtr)
          val _ = emit (zeroPtr ^ " = getelementptr i8, ptr " ^ slot ^
                        ", i64 8")
          val _ = emit ("store i64 0, ptr " ^ zeroPtr)
        in
          slot
        end

      fun bindTo (address, fd, port) =
        let
          val sa = sockaddr (port, address)
          val reg = fresh "sys.bind"
          val _ = emit (reg ^ " = call i32 @bind(i32 " ^ #text fd ^
                        ", ptr " ^ sa ^ ", i32 16)")
        in
          result reg
        end

      fun epollControl (operation, epfd, fd, events, tokenValue) =
        let
          val event = fresh "sys.epoll.event"
          val eventsPtr = fresh "sys.epoll.eventsp"
          val dataPtr = fresh "sys.epoll.datap"
          val token = fresh "sys.epoll.token"
          val reg = fresh "sys.epoll.ctl"
          val _ = emit (event ^ " = alloca i8, i64 12")
          val _ = emit (eventsPtr ^ " = getelementptr i8, ptr " ^ event ^
                        ", i64 0")
          val _ = emit ("store i32 " ^ #text events ^ ", ptr " ^ eventsPtr)
          val _ = emit (token ^ " = sext i32 " ^ #text tokenValue ^
                        " to i64")
          val _ = emit (dataPtr ^ " = getelementptr i8, ptr " ^ event ^
                        ", i64 4")
          val _ = emit ("store i64 " ^ token ^ ", ptr " ^ dataPtr ^
                        ", align 1")
          val _ = emit (reg ^ " = call i32 @epoll_ctl(i32 " ^ #text epfd ^
                        ", i32 " ^ Int.toString operation ^ ", i32 " ^
                        #text fd ^ ", ptr " ^ event ^ ")")
        in
          result reg
        end
    in
      if String.isPrefix "extern:" name then
        lowerExtern env (name, lowered, resultTy)
      else if name = "extern_view" then
        lowerExternView env (lowered, resultTy)
      else if name = "extern_view_ptr" then
        lowerExternViewPtr env (lowered, resultTy)
      else if name = "c_ptr.of_int" then
        let
          val value =
            case lowered of
              argument :: _ => argument
            | [] => { text = "0", ty = Ir.Int 64, const = true }
          val reg = fresh "cptr.of_int"
          val _ = emit (reg ^ " = inttoptr i64 " ^ #text value ^ " to ptr")
        in
          { text = reg, ty = resultTy, const = false }
        end
      else if name = "c_ptr.is_null" then
        let
          val value =
            case lowered of
              argument :: _ => argument
            | [] => { text = "null", ty = Ir.TCPtr, const = true }
          val reg = fresh "cptr.is_null"
          val _ = emit (reg ^ " = icmp eq ptr " ^ #text value ^ ", null")
        in
          { text = reg, ty = resultTy, const = false }
        end
      else
      case (name, lowered) of
        ("sys.socket_tcp", []) =>
          let val reg = fresh "sys.socket" in
            emit (reg ^ " = call i32 @socket(i32 2, i32 524289, i32 0)");
            result reg
          end
      | ("sys.reuseaddr", [fd]) =>
          let
            val oneSlot = fresh "sys.reuseaddr.slot"
            val reg = fresh "sys.reuseaddr"
            val _ = emit (oneSlot ^ " = alloca i32")
            val _ = emit ("store i32 1, ptr " ^ oneSlot)
            val _ = emit (reg ^ " = call i32 @setsockopt(i32 " ^
                          #text fd ^ ", i32 1, i32 2, ptr " ^ oneSlot ^
                          ", i32 4)")
          in
            result reg
          end
      | ("sys.reuseport", [fd]) =>
          let
            val oneSlot = fresh "sys.reuseport.slot"
            val reg = fresh "sys.reuseport"
            val _ = emit (oneSlot ^ " = alloca i32")
            val _ = emit ("store i32 1, ptr " ^ oneSlot)
            val _ = emit (reg ^ " = call i32 @setsockopt(i32 " ^
                          #text fd ^ ", i32 1, i32 15, ptr " ^ oneSlot ^
                          ", i32 4)")
          in
            result reg
          end
      | ("sys.nodelay", [fd]) =>
          let
            val oneSlot = fresh "sys.nodelay.slot"
            val reg = fresh "sys.nodelay"
            val _ = emit (oneSlot ^ " = alloca i32")
            val _ = emit ("store i32 1, ptr " ^ oneSlot)
            val _ = emit (reg ^ " = call i32 @setsockopt(i32 " ^
                          #text fd ^ ", i32 6, i32 1, ptr " ^ oneSlot ^
                          ", i32 4)")
          in
            result reg
          end
      | ("sys.bind_loopback", [fd, port]) => bindTo (2130706433, fd, port)
      | ("sys.bind_any", [fd, port]) => bindTo (0, fd, port)
      | ("sys.listen", [fd, backlog]) =>
          let val reg = fresh "sys.listen" in
            emit (reg ^ " = call i32 @listen(i32 " ^ #text fd ^
                  ", i32 " ^ #text backlog ^ ")");
            result reg
          end
      | ("sys.accept", [fd]) =>
          let val reg = fresh "sys.accept" in
            emit (reg ^ " = call i32 @accept4(i32 " ^ #text fd ^
                  ", ptr null, ptr null, i32 524288)");
            result reg
          end
      | ("sys.connect_loopback", [fd, port]) =>
          let
            val sa = sockaddr (port, 2130706433)
            val reg = fresh "sys.connect"
            val _ = emit (reg ^ " = call i32 @connect(i32 " ^ #text fd ^
                          ", ptr " ^ sa ^ ", i32 16)")
          in
            result reg
          end
      | ("sys.local_port", [fd]) =>
          let
            val slot = fresh "sys.sockaddr"
            val lengthPtr = fresh "sys.socklenp"
            val lengthValue = fresh "sys.socklen"
            val reg = fresh "sys.getsockname"
            val portSlot = fresh "sys.portslot"
            val ok = fresh "sys.port.ok"
            val okLabel = freshLabel "sys.port.ok"
            val doneLabel = freshLabel "sys.port.done"
            val portPtr = fresh "sys.portp"
            val port16 = fresh "sys.port16"
            val portHost = fresh "sys.porthost"
            val port32 = fresh "sys.port32"
            val finalPort = fresh "sys.port"
            val _ = emit (slot ^ " = alloca i8, i64 16")
            val _ = emit (lengthPtr ^ " = alloca i32")
            val _ = emit ("store i32 16, ptr " ^ lengthPtr)
            val _ = emit (reg ^ " = call i32 @getsockname(i32 " ^
                          #text fd ^ ", ptr " ^ slot ^ ", ptr " ^
                          lengthPtr ^ ")")
            val _ = emit (portSlot ^ " = alloca i32")
            val _ = emit ("store i32 -1, ptr " ^ portSlot)
            val _ = emit (ok ^ " = icmp sge i32 " ^ reg ^ ", 0")
            val _ = emit ("br i1 " ^ ok ^ ", label %" ^ okLabel ^
                          ", label %" ^ doneLabel)
            val _ = emit (okLabel ^ ":")
            val _ = emit (portPtr ^ " = getelementptr i8, ptr " ^ slot ^
                          ", i64 2")
            val _ = emit (port16 ^ " = load i16, ptr " ^ portPtr)
            val _ = emit (portHost ^ " = call i16 @ntohs(i16 " ^ port16 ^ ")")
            val _ = emit (port32 ^ " = zext i16 " ^ portHost ^ " to i32")
            val _ = emit ("store i32 " ^ port32 ^ ", ptr " ^ portSlot)
            val _ = emit ("br label %" ^ doneLabel)
            val _ = emit (doneLabel ^ ":")
            val _ = emit (finalPort ^ " = load i32, ptr " ^ portSlot)
          in
            result finalPort
          end
      | ("sys.close", [fd]) =>
          let val reg = fresh "sys.close" in
            emit (reg ^ " = call i32 @close(i32 " ^ #text fd ^ ")");
            result reg
          end
      | ("sys.set_nonblocking", [fd]) =>
          let
            val flags = fresh "sys.fcntl.flags"
            val newFlags = fresh "sys.fcntl.newflags"
            val reg = fresh "sys.fcntl"
            val _ = emit (flags ^ " = call i32 @fcntl(i32 " ^ #text fd ^
                          ", i32 3, i32 0)")
            val _ = emit (newFlags ^ " = or i32 " ^ flags ^ ", 2048")
            val _ = emit (reg ^ " = call i32 @fcntl(i32 " ^ #text fd ^
                          ", i32 4, i32 " ^ newFlags ^ ")")
          in
            result reg
          end
      | ("sys.epoll_create", []) =>
          let val reg = fresh "sys.epoll" in
            emit (reg ^ " = call i32 @epoll_create1(i32 524288)");
            result reg
          end
      | ("sys.epoll_add", [epfd, fd, events]) =>
          epollControl (1, epfd, fd, events, fd)
      | ("sys.epoll_add_token", [epfd, fd, events, token]) =>
          epollControl (1, epfd, fd, events, token)
      | ("sys.epoll_mod_token", [epfd, fd, events, token]) =>
          epollControl (3, epfd, fd, events, token)
      | ("sys.epoll_del", [epfd, fd]) =>
          let val zero = { text = "0", ty = Ir.Int 32, const = true } in
            epollControl (2, epfd, fd, zero, fd)
          end
      | ("sys.epoll_wait",
         [epfd, fds, events, maxEvents, timeout]) =>
          let
            val fdsData = fresh "sys.epoll.fds"
            val fdsLength = fresh "sys.epoll.fdslen"
            val eventData = fresh "sys.epoll.events"
            val eventLength = fresh "sys.epoll.eventslen"
            val max64 = fresh "sys.epoll.max64"
            val limit1 = fresh "sys.epoll.limit1"
            val limit2 = fresh "sys.epoll.limit2"
            val limit3 = fresh "sys.epoll.limit3"
            val le1 = fresh "sys.epoll.le1"
            val le2 = fresh "sys.epoll.le2"
            val le3 = fresh "sys.epoll.le3"
            val limit32 = fresh "sys.epoll.limit32"
            val nonPositive = fresh "sys.epoll.nonpositive"
            val zeroLabel = freshLabel "sys.epoll.zero"
            val waitLabel = freshLabel "sys.epoll.wait"
            val waitDoneLabel = freshLabel "sys.epoll.waitdone"
            val emptyLabel = freshLabel "sys.epoll.empty"
            val loopLabel = freshLabel "sys.epoll.loop"
            val bodyLabel = freshLabel "sys.epoll.body"
            val doneLabel = freshLabel "sys.epoll.done"
            val resultSlot = fresh "sys.epoll.resultslot"
            val buffer = fresh "sys.epoll.buffer"
            val count = fresh "sys.epoll.count"
            val count64 = fresh "sys.epoll.count64"
            val indexSlot = fresh "sys.epoll.indexslot"
            val index = fresh "sys.epoll.index"
            val condition = fresh "sys.epoll.condition"
            val offset = fresh "sys.epoll.offset"
            val base = fresh "sys.epoll.base"
            val eventPtr = fresh "sys.epoll.eventptr"
            val eventValue = fresh "sys.epoll.eventvalue"
            val tokenPtr = fresh "sys.epoll.tokenptr"
            val tokenValue = fresh "sys.epoll.tokenvalue"
            val fd32 = fresh "sys.epoll.fd32"
            val fdSlot = fresh "sys.epoll.fdslot"
            val eventSlot = fresh "sys.epoll.eventslot"
            val next = fresh "sys.epoll.next"
            val finalCount = fresh "sys.epoll.final"
            val pointerAt = fn (value, index, storage) =>
              let val pointer = fresh "sys.epoll.elementp" in
                emit (pointer ^ " = getelementptr " ^ storage ^ ", ptr " ^
                      value ^ ", i64 " ^ index);
                pointer
              end
            val _ = emit (fdsData ^ " = getelementptr { ptr, i64, i64, ptr }, ptr " ^
                          #text fds ^ ", i32 0, i32 0")
            val _ = emit (fdsData ^ ".load = load ptr, ptr " ^ fdsData)
            val fdsDataPtr = fdsData ^ ".load"
            val _ = emit (fdsLength ^ " = getelementptr { ptr, i64, i64, ptr }, ptr " ^
                          #text fds ^ ", i32 0, i32 1")
            val _ = emit (fdsLength ^ ".load = load i64, ptr " ^ fdsLength)
            val fdsLen = fdsLength ^ ".load"
            val _ = emit (eventData ^ " = getelementptr { ptr, i64, i64, ptr }, ptr " ^
                          #text events ^ ", i32 0, i32 0")
            val _ = emit (eventData ^ ".load = load ptr, ptr " ^ eventData)
            val eventDataPtr = eventData ^ ".load"
            val _ = emit (eventLength ^ " = getelementptr { ptr, i64, i64, ptr }, ptr " ^
                          #text events ^ ", i32 0, i32 1")
            val _ = emit (eventLength ^ ".load = load i64, ptr " ^ eventLength)
            val eventLen = eventLength ^ ".load"
            val _ = emit (max64 ^ " = sext i32 " ^ #text maxEvents ^
                          " to i64")
            val _ = emit (le1 ^ " = icmp sle i64 " ^ max64 ^ ", 64")
            val _ = emit (limit1 ^ " = select i1 " ^ le1 ^ ", i64 " ^
                          max64 ^ ", i64 64")
            val _ = emit (le2 ^ " = icmp sle i64 " ^ limit1 ^ ", " ^
                          fdsLen)
            val _ = emit (limit2 ^ " = select i1 " ^ le2 ^ ", i64 " ^
                          limit1 ^ ", i64 " ^ fdsLen)
            val _ = emit (le3 ^ " = icmp sle i64 " ^ limit2 ^ ", " ^
                          eventLen)
            val _ = emit (limit3 ^ " = select i1 " ^ le3 ^ ", i64 " ^
                          limit2 ^ ", i64 " ^ eventLen)
            val _ = emit (limit32 ^ " = trunc i64 " ^ limit3 ^ " to i32")
            val _ = emit (nonPositive ^ " = icmp sle i32 " ^ limit32 ^
                          ", 0")
            val _ = emit (resultSlot ^ " = alloca i32")
            val _ = emit (buffer ^ " = alloca i8, i64 768")
            val _ = emit ("br i1 " ^ nonPositive ^ ", label %" ^ zeroLabel ^
                          ", label %" ^ waitLabel)
            val _ = emit (zeroLabel ^ ":")
            val _ = emit ("store i32 0, ptr " ^ resultSlot)
            val _ = emit ("br label %" ^ doneLabel)
            val _ = emit (waitLabel ^ ":")
            val _ = emit (count ^ " = call i32 @epoll_wait(i32 " ^
                          #text epfd ^ ", ptr " ^ buffer ^ ", i32 " ^
                          limit32 ^ ", i32 " ^ #text timeout ^ ")")
            val _ = emit ("store i32 " ^ count ^ ", ptr " ^ resultSlot)
            val _ = emit ("br label %" ^ waitDoneLabel)
            val _ = emit (waitDoneLabel ^ ":")
            val _ = emit (indexSlot ^ " = alloca i64")
            val _ = emit ("store i64 0, ptr " ^ indexSlot)
            val _ = emit ("br label %" ^ loopLabel)
            val _ = emit (loopLabel ^ ":")
            val _ = emit (index ^ " = load i64, ptr " ^ indexSlot)
            val _ = emit (count64 ^ " = sext i32 " ^ count ^ " to i64")
            val _ = emit (condition ^ " = icmp slt i64 " ^ index ^ ", " ^
                          count64)
            val _ = emit ("br i1 " ^ condition ^ ", label %" ^ bodyLabel ^
                          ", label %" ^ emptyLabel)
            val _ = emit (bodyLabel ^ ":")
            val _ = emit (offset ^ " = mul i64 " ^ index ^ ", 12")
            val _ = emit (base ^ " = getelementptr i8, ptr " ^ buffer ^
                          ", i64 " ^ offset)
            val _ = emit (eventPtr ^ " = getelementptr i8, ptr " ^ base ^
                          ", i64 0")
            val _ = emit (eventValue ^ " = load i32, ptr " ^ eventPtr)
            val _ = emit (tokenPtr ^ " = getelementptr i8, ptr " ^ base ^
                          ", i64 4")
            val _ = emit (tokenValue ^ " = load i64, ptr " ^ tokenPtr ^
                          ", align 1")
            val _ = emit (fd32 ^ " = trunc i64 " ^ tokenValue ^ " to i32")
            val _ = emit (fdSlot ^ " = getelementptr i32, ptr " ^
                          fdsDataPtr ^ ", i64 " ^ index)
            val _ = emit ("store i32 " ^ fd32 ^ ", ptr " ^ fdSlot)
            val _ = emit (eventSlot ^ " = getelementptr i32, ptr " ^
                          eventDataPtr ^ ", i64 " ^ index)
            val _ = emit ("store i32 " ^ eventValue ^ ", ptr " ^ eventSlot)
            val _ = emit (next ^ " = add i64 " ^ index ^ ", 1")
            val _ = emit ("store i64 " ^ next ^ ", ptr " ^ indexSlot)
            val _ = emit ("br label %" ^ loopLabel)
            val _ = emit (emptyLabel ^ ":")
            val _ = emit ("br label %" ^ doneLabel)
            val _ = emit (doneLabel ^ ":")
            val _ = emit (finalCount ^ " = load i32, ptr " ^ resultSlot)
          in
            result finalCount
          end
      | ("sys.recv_into_raw", [fd, buffer, offset, count]) =>
          let
            val data = loadArrayField buffer 0 "ptr"
            val length = loadArrayField buffer 1 "i64"
          in
            rawTransfer ("recv", "64", fd, data, length, offset, count)
          end
      | ("sys.send_string_raw", [fd, value, offset, count]) =>
          let
            val data = fresh "sys.send.data"
            val length = fresh "sys.send.len"
            val _ = emit (data ^ " = extractvalue { ptr, i64, i1 } " ^
                          #text value ^ ", 0")
            val _ = emit (length ^ " = extractvalue { ptr, i64, i1 } " ^
                          #text value ^ ", 1")
          in
            rawTransfer ("send", "16448", fd, data, length, offset, count)
          end
      | ("sys.send_from_raw", [fd, buffer, offset, count]) =>
          let
            val data = loadArrayField buffer 0 "ptr"
            val length = loadArrayField buffer 1 "i64"
          in
            rawTransfer ("send", "16448", fd, data, length, offset, count)
          end
      | ("sys.scratch_enter", []) =>
          ( if Memory.arenaEnabled () then
              if stackMode then arenaMark ()
              else emit "call void @yack_scratch_enter()"
            else ()
          ; { text = "", ty = resultTy, const = true }
          )
      | ("sys.scratch_enter_all", []) =>
          ( if Memory.arenaEnabled () then
              if stackMode then arenaMark ()
              else emit "call void @yack_scratch_enter_all()"
            else ()
          ; { text = "", ty = resultTy, const = true }
          )
      | ("sys.scratch_leave", []) =>
          ( if Memory.arenaEnabled () then
              if stackMode then arenaRelease ()
              else emit "call void @yack_scratch_leave()"
            else ()
          ; { text = "", ty = resultTy, const = true }
          )
      | ("sys.thread_spawn", [worker, argument]) =>
          let val reg = fresh "thread.id"
              val _ =
                emit (reg ^ " = call i64 @yack_thread_spawn(ptr " ^
                      #text worker ^ ", i64 " ^ #text argument ^ ")")
          in
              { text = reg, ty = resultTy, const = false }
          end
      | ("sys.thread_join", [thread]) =>
          ( emit ("call void @yack_thread_join(i64 " ^ #text thread ^ ")")
          ; { text = "", ty = resultTy, const = true }
          )
      | ("sys.trace_enter", [site, _]) =>
          ( emit ("; yack.alloc site=" ^ #text site)
          ; emit ("call void @yack_trace_enter(i64 " ^ #text site ^ ")")
          ; { text = "", ty = resultTy, const = true }
          )
      | ("sys.trace_leave", []) =>
          ( emit "call void @yack_trace_leave()"
          ; { text = "", ty = resultTy, const = true }
          )
      | ("sys.errno", []) =>
          let
            val errnoPtr = fresh "sys.errnop"
            val reg = fresh "sys.errno"
            val _ = emit (errnoPtr ^ " = call ptr @__errno_location()")
            val _ = emit (reg ^ " = load i32, ptr " ^ errnoPtr)
          in
            result reg
          end
      | ("sys.install_stop_signals", []) =>
          ( emit "call void @yack_install_stop_signals()"
          ; { text = "", ty = resultTy, const = true }
          )
      | ("sys.stop_requested", []) =>
          let
            val raw = fresh "sys.stop"
            val flag = fresh "sys.stop.bool"
            val _ = emit (raw ^ " = call i32 @yack_stop_requested()")
            val _ = emit (flag ^ " = icmp ne i32 " ^ raw ^ ", 0")
          in
            result flag
          end
      | ("sys.sleep_ms", [milliseconds]) =>
          let
            val width = fresh "sys.sleep.ms"
            val _ = emit (width ^ " = sext i32 " ^ #text milliseconds ^
                          " to i64")
            val _ = emit ("call void @yack_sleep_ms(i64 " ^ width ^ ")")
          in
            { text = "", ty = resultTy, const = true }
          end
      | ("sys.monotonic_ms", []) =>
          let val reg = fresh "sys.monotonic"
          in
            emit (reg ^ " = call i64 @yack_monotonic_ms()");
            result reg
          end
      | ("sys.realtime_ms", []) =>
          let val reg = fresh "sys.realtime"
          in
            emit (reg ^ " = call i64 @yack_realtime_ms()");
            result reg
          end
      | ("sqlite_open", [path]) =>
          let val data = stringData path
              val slot = outPointer ()
              val status = fresh "sqlite.status"
              val _ = emit (status ^ " = call i32 @sqlite3_open(ptr " ^
                            data ^ ", ptr " ^ slot ^ ")")
              val db = loadPointer slot
              val integer = handleInteger db
              val pair0 = fresh "sqlite.pair0"
              val pair1 = fresh "sqlite.pair1"
              val _ = emit (pair0 ^ " = insertvalue { i32, i64 } undef, i32 " ^
                            status ^ ", 0")
              val _ = emit (pair1 ^ " = insertvalue { i32, i64 } " ^ pair0 ^
                            ", i64 " ^ integer ^ ", 1")
          in result pair1
          end
      | ("sqlite_close", [db]) =>
          let val reg = fresh "sqlite.close" in
            emit (reg ^ " = call i32 @sqlite3_close(ptr " ^
                  handlePointer db ^ ")");
            result reg
          end
      | ("sqlite_exec", [db, sql]) =>
          let val reg = fresh "sqlite.exec" in
            emit (reg ^ " = call i32 @sqlite3_exec(ptr " ^
                  handlePointer db ^ ", ptr " ^ stringData sql ^
                  ", ptr null, ptr null, ptr null)");
            result reg
          end
      | ("sqlite_prepare", [db, sql]) =>
          let val slot = outPointer ()
              val _ = emit ("%sqlite.prepare = call i32 @sqlite3_prepare_v2(ptr " ^
                            handlePointer db ^ ", ptr " ^ stringData sql ^
                            ", i32 -1, ptr " ^ slot ^ ", ptr null)")
              val stmt = loadPointer slot
          in result (handleInteger stmt)
          end
      | ("sqlite_finalize", [stmt]) =>
          let val reg = fresh "sqlite.finalize" in
            emit (reg ^ " = call i32 @sqlite3_finalize(ptr " ^
                  handlePointer stmt ^ ")");
            result reg
          end
      | ("sqlite_step", [stmt]) =>
          let val reg = fresh "sqlite.step" in
            emit (reg ^ " = call i32 @sqlite3_step(ptr " ^
                  handlePointer stmt ^ ")");
            result reg
          end
      | ("sqlite_column_count", [stmt]) =>
          let val reg = fresh "sqlite.columns" in
            emit (reg ^ " = call i32 @sqlite3_column_count(ptr " ^
                  handlePointer stmt ^ ")");
            result reg
          end
      | ("sqlite_column_name", [stmt, index]) =>
          let val pointer = fresh "sqlite.name" in
            emit (pointer ^ " = call ptr @sqlite3_column_name(ptr " ^
                  handlePointer stmt ^ ", i32 " ^ #text index ^ ")");
            copiedCString pointer
          end
      | ("sqlite_column_type", [stmt, index]) =>
          let val reg = fresh "sqlite.type" in
            emit (reg ^ " = call i32 @sqlite3_column_type(ptr " ^
                  handlePointer stmt ^ ", i32 " ^ #text index ^ ")");
            result reg
          end
      | ("sqlite_column_int", [stmt, index]) =>
          let val reg = fresh "sqlite.int" in
            emit (reg ^ " = call i32 @sqlite3_column_int(ptr " ^
                  handlePointer stmt ^ ", i32 " ^ #text index ^ ")");
            result reg
          end
      | ("sqlite_column_text", [stmt, index]) =>
          let val pointer = fresh "sqlite.text"
              val _ = emit (pointer ^ " = call ptr @sqlite3_column_text(ptr " ^
                            handlePointer stmt ^ ", i32 " ^ #text index ^ ")")
              val nullValue = fresh "sqlite.null"
              val safe = fresh "sqlite.safe"
              val _ = emit (nullValue ^ " = icmp eq ptr " ^ pointer ^ ", null")
              val _ = emit (safe ^ " = select i1 " ^ nullValue ^
                            ", ptr @yack_empty, ptr " ^ pointer)
              val count = fresh "sqlite.textlen"
              val _ = emit (count ^ " = call i32 @sqlite3_column_bytes(ptr " ^
                            handlePointer stmt ^ ", i32 " ^ #text index ^ ")")
              val length = fresh "sqlite.textlen64"
              val _ = emit (length ^ " = sext i32 " ^ count ^ " to i64")
          in copyCString (safe, length)
          end
      | ("sqlite_changes", [db]) =>
          let val reg = fresh "sqlite.changes" in
            emit (reg ^ " = call i32 @sqlite3_changes(ptr " ^
                  handlePointer db ^ ")");
            result reg
          end
      | ("sqlite_errmsg", [db]) =>
          let val pointer = fresh "sqlite.errmsg" in
            emit (pointer ^ " = call ptr @sqlite3_errmsg(ptr " ^
                  handlePointer db ^ ")");
            copiedCString pointer
          end
      | _ =>
          { text = "0", ty = resultTy, const = true }
    end

  and lowerExtern (env : env) (name, lowered, resultTy) =
    let
      val emit = #emit env
      val fresh = #fresh env
      val allocString = #allocString env
      fun stringData value =
        let val data = fresh "extern.data" in
          emit (data ^ " = extractvalue { ptr, i64, i1 } " ^
                #text value ^ ", 0");
          data
        end
      fun cString value =
        let
          val data = stringData value
          val length = fresh "extern.len"
          val size = fresh "extern.size"
          val endPointer = fresh "extern.end"
          val _ = emit (length ^ " = extractvalue { ptr, i64, i1 } " ^
                        #text value ^ ", 1")
          val _ = emit (size ^ " = add i64 " ^ length ^ ", 1")
          val memory = allocString size
          val _ = emit ("call void @memcpy(ptr " ^ memory ^ ", ptr " ^
                        data ^ ", i64 " ^ length ^ ")")
          val _ = emit (endPointer ^ " = getelementptr i8, ptr " ^ memory ^
                        ", i64 " ^ length)
          val _ = emit ("store i8 0, ptr " ^ endPointer)
        in
          memory
        end
      fun arrayData value =
        let
          val field = fresh "extern.field"
          val data = fresh "extern.array"
          val _ = emit (field ^ " = getelementptr { ptr, i64, i64, ptr }, ptr " ^
                        #text value ^ ", i32 0, i32 0")
          val _ = emit (data ^ " = load ptr, ptr " ^ field)
        in
          data
        end
      val temporaries = ref []
      fun argumentText value =
        case #ty value of
          Ir.Int _ => llvmTy (#ty value) ^ " " ^ #text value
        | Ir.TCPtr => "ptr " ^ #text value
        | Ir.TString =>
            let val memory = cString value in
              temporaries := memory :: !temporaries;
              "ptr " ^ memory
            end
        | Ir.TBorrow (Ir.TString, _) =>
            let val memory = cString value in
              temporaries := memory :: !temporaries;
              "ptr " ^ memory
            end
        | Ir.TArray _ => "ptr " ^ arrayData value
        | Ir.TBorrow (Ir.TArray _, _) => "ptr " ^ arrayData value
        | _ => "i64 0"
      val symbol = String.extract (name, String.size "extern:", NONE)
      val argumentTexts = List.map argumentText lowered
      val resultText = if isUnit resultTy then "void" else llvmTy resultTy
      val callText =
        "call " ^ resultText ^ " @" ^ symbol ^ "(" ^
        String.concatWith ", " argumentTexts ^ ")"
      fun freeTemporaries () =
        List.app
          (fn memory => emit ("call void @yack_free(ptr " ^ memory ^ ")"))
          (!temporaries)
    in
      if isUnit resultTy then
        ( emit callText
        ; freeTemporaries ()
        ; { text = "", ty = resultTy, const = true }
        )
      else
        let val reg = fresh "extern.result" in
          emit (reg ^ " = " ^ callText);
          freeTemporaries ();
          { text = reg, ty = resultTy, const = false }
        end
    end

  and lowerExternView (env : env) (lowered, resultTy) =
    let
      val emit = #emit env
      val fresh = #fresh env
      val freshLabel = #freshLabel env
      val size =
        case resultTy of
          Ir.TBorrow (Ir.TRecord (_, Ir.DExtern (_, size)), _) => size
        | _ => 0
      val value =
        case lowered of
          argument :: _ => argument
        | [] => { text = "null", ty = Ir.TArray (Ir.Int 8), const = true }
      val field = fresh "extern.view.field"
      val data = fresh "extern.view.data"
      val length = fresh "extern.view.length"
      val ok = fresh "extern.view.ok"
      val badLabel = freshLabel "extern.view.bad"
      val goodLabel = freshLabel "extern.view.good"
      val _ =
        emit (field ^ " = getelementptr { ptr, i64, i64, ptr }, ptr " ^
              #text value ^ ", i32 0, i32 0")
      val _ = emit (data ^ " = load ptr, ptr " ^ field)
      val lengthPointer = fresh "extern.view.lenp"
      val _ =
        emit (lengthPointer ^ " = getelementptr { ptr, i64, i64, ptr }, ptr " ^
              #text value ^ ", i32 0, i32 1")
      val _ = emit (length ^ " = load i64, ptr " ^ lengthPointer)
      val _ = emit (ok ^ " = icmp uge i64 " ^ length ^ ", " ^
                    Int.toString size)
      val _ = emit ("br i1 " ^ ok ^ ", label %" ^ goodLabel ^
                    ", label %" ^ badLabel)
      val _ = emit (badLabel ^ ":")
      val _ =
        emit ("call void @yack_abort_bounds(i64 " ^ Int.toString size ^
              ", i64 " ^ length ^ ")")
      val _ = emit "unreachable"
      val _ = emit (goodLabel ^ ":")
    in
      { text = data, ty = resultTy, const = false }
    end

  and lowerExternViewPtr (env : env) (lowered, resultTy) =
    let
      val emit = #emit env
      val fresh = #fresh env
      val freshLabel = #freshLabel env
      val size =
        case resultTy of
          Ir.TBorrow (Ir.TRecord (_, Ir.DExtern (_, size)), _) => size
        | _ => 0
      val address =
        case lowered of
          argument :: _ => argument
        | [] => { text = "null", ty = Ir.TCPtr, const = true }
      val requested =
        case lowered of
          _ :: argument :: _ => argument
        | _ => { text = "0", ty = Ir.Int 32, const = true }
      val requested64 = fresh "extern.view.ptr.size64"
      val matches = fresh "extern.view.ptr.matches"
      val nonNull = fresh "extern.view.ptr.nonnull"
      val valid = fresh "extern.view.ptr.valid"
      val badLabel = freshLabel "extern.view.ptr.bad"
      val goodLabel = freshLabel "extern.view.ptr.good"
      val _ = emit (requested64 ^ " = sext i32 " ^ #text requested ^ " to i64")
      val _ = emit (matches ^ " = icmp eq i64 " ^ requested64 ^ ", " ^
                    Int.toString size)
      val _ = emit (nonNull ^ " = icmp ne ptr " ^ #text address ^ ", null")
      val _ = emit (valid ^ " = and i1 " ^ matches ^ ", " ^ nonNull)
      val _ = emit ("br i1 " ^ valid ^ ", label %" ^ goodLabel ^
                    ", label %" ^ badLabel)
      val _ = emit (badLabel ^ ":")
      val _ = emit "call void @abort()"
      val _ = emit "unreachable"
      val _ = emit (goodLabel ^ ":")
    in
      { text = #text address, ty = resultTy, const = false }
    end

end
