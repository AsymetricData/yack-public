structure LlvmOp =
struct
  open LlvmTypes

  type env =
    { emit : string -> unit
    , fresh : string -> string
    }

  fun coerce (env : env, value : value, target : Ir.ty) =
    let val emit = #emit env
    in
      if #ty value = target then value
      else
        case (#ty value, target) of
          (Ir.Int sourceWidth, Ir.Int targetWidth) =>
            let val reg = #fresh env "cast" in
              if targetWidth < sourceWidth then
                emit (reg ^ " = trunc i" ^ Int.toString sourceWidth ^
                      " " ^ #text value ^ " to i" ^ Int.toString targetWidth)
              else
                emit (reg ^ " = sext i" ^ Int.toString sourceWidth ^
                      " " ^ #text value ^ " to i" ^ Int.toString targetWidth);
              { text = reg, ty = target, const = false }
            end
        | (Ir.Float sourceWidth, Ir.Float targetWidth) =>
            let val reg = #fresh env "cast" in
              if targetWidth < sourceWidth then
                emit (reg ^ " = fptrunc " ^ llvmTy (#ty value) ^
                      " " ^ #text value ^ " to " ^ llvmTy target)
              else
                emit (reg ^ " = fpext " ^ llvmTy (#ty value) ^
                      " " ^ #text value ^ " to " ^ llvmTy target);
              { text = reg, ty = target, const = false }
            end
        | (Ir.Int _, Ir.Float _) =>
            let val reg = #fresh env "cast" in
              emit (reg ^ " = sitofp " ^ typed value ^ " to " ^ llvmTy target);
              { text = reg, ty = target, const = false }
            end
        | (Ir.Bool, Ir.Int 32) =>
            let val reg = #fresh env "cast" in
              emit (reg ^ " = zext i1 " ^ #text value ^ " to i32");
              { text = reg, ty = target, const = false }
            end
        | _ => value
    end

  fun commonNumeric (left : Ir.ty, right : Ir.ty) =
    case (left, right) of
      (Ir.Float width1, Ir.Float width2) => Ir.Float (Int.max (width1, width2))
    | (Ir.Float width, Ir.Int _) => Ir.Float width
    | (Ir.Int _, Ir.Float width) => Ir.Float width
    | (Ir.Int width1, Ir.Int width2) => Ir.Int (Int.max (width1, width2))
    | _ => Ir.Int 32

  fun intComparison operator =
    case operator of
      "<" => "slt" | "<=" => "sle" | ">" => "sgt" | ">=" => "sge"
    | "=" => "eq" | "<>" => "ne" | _ => "eq"

  fun floatComparison operator =
    case operator of
      "<" => "olt" | "<=" => "ole" | ">" => "ogt" | ">=" => "oge"
    | "=" => "oeq" | "<>" => "one" | _ => "oeq"

  fun intArithmetic operator =
    case operator of
      "+" => "add" | "-" => "sub" | "*" => "mul"
    | "/" => "sdiv" | "%" => "srem" | _ => "add"

  fun floatArithmetic operator =
    case operator of
      "+" => "fadd" | "-" => "fsub" | "*" => "fmul" | "/" => "fdiv"
    | _ => "fadd"

  fun intMinLiteral width =
    case width of
      8 => "-128"
    | 16 => "-32768"
    | 32 => "-2147483648"
    | _ => "-9223372036854775808"

  fun lowerIntDivision (env : env, operator, width, dividend : value,
                        divisor : value, reg) =
    let
      val emit = #emit env
      val fresh = #fresh env
      fun label prefix =
        let val name = fresh prefix in String.extract (name, 1, NONE) end
      val ty = llvmTy (Ir.Int width)
      val dividendText = #text dividend
      val divisorText = #text divisor
      fun abortBlock (message, prefix) =
        let
          val bad = label prefix
          val dead = label "div.dead"
        in
          emit ("br label %" ^ bad);
          emit (bad ^ ":");
          emit ("call void @yack_abort_msg(ptr " ^ message ^ ")");
          emit "unreachable";
          emit (dead ^ ":")
        end
      val () =
        if not (#const divisor) then
          let
            val isZero = fresh "div.zero"
            val bad = label "div.zero.abort"
            val ok = label "div.zero.ok"
          in
            emit (isZero ^ " = icmp eq " ^ ty ^ " " ^ divisorText ^ ", 0");
            emit ("br i1 " ^ isZero ^ ", label %" ^ bad ^ ", label %" ^ ok ^
                  ", !prof !{!\"branch_weights\", i32 1, i32 1048575}");
            emit (bad ^ ":");
            emit ("call void @yack_abort_msg(ptr @.yack_msg_div_zero)");
            emit "unreachable";
            emit (ok ^ ":")
          end
        else if divisorText = "0" then
          abortBlock ("@.yack_msg_div_zero", "div.zero.abort")
        else ()
      val minText = intMinLiteral width
      val mayOverflow =
        (not (#const dividend) orelse dividendText = minText)
        andalso (not (#const divisor) orelse divisorText = "-1")
      val overflowFlag =
        if not mayOverflow then NONE
        else if #const dividend andalso #const divisor then NONE
        else
          let
            val leftFlag =
              if #const dividend then NONE
              else
                let val r = fresh "div.min" in
                  emit (r ^ " = icmp eq " ^ ty ^ " " ^ dividendText ^ ", " ^
                        minText);
                  SOME r
                end
            val rightFlag =
              if #const divisor then NONE
              else
                let val r = fresh "div.negone" in
                  emit (r ^ " = icmp eq " ^ ty ^ " " ^ divisorText ^ ", -1");
                  SOME r
                end
          in
            case (leftFlag, rightFlag) of
              (SOME l, SOME r) =>
                let val both = fresh "div.overflow" in
                  emit (both ^ " = and i1 " ^ l ^ ", " ^ r);
                  SOME both
                end
            | (SOME flag, NONE) => SOME flag
            | (NONE, SOME flag) => SOME flag
            | (NONE, NONE) => NONE
          end
      fun emitSdiv () =
        emit (reg ^ " = sdiv " ^ ty ^ " " ^ dividendText ^ ", " ^ divisorText)
      fun emitSrem () =
        emit (reg ^ " = srem " ^ ty ^ " " ^ dividendText ^ ", " ^ divisorText)
    in
      if operator = "/" then
        let
          val () =
            if not mayOverflow then ()
            else
              case overflowFlag of
                NONE =>
                  abortBlock ("@.yack_msg_div_overflow", "div.overflow.abort")
              | SOME flag =>
                  let
                    val bad = label "div.overflow.abort"
                    val ok = label "div.overflow.ok"
                  in
                    emit ("br i1 " ^ flag ^ ", label %" ^ bad ^
                          ", label %" ^ ok ^
                          ", !prof !{!\"branch_weights\", i32 1, i32 1048575}");
                    emit (bad ^ ":");
                    emit ("call void @yack_abort_msg(ptr @.yack_msg_div_overflow)");
                    emit "unreachable";
                    emit (ok ^ ":")
                  end
        in
          emitSdiv ();
          reg
        end
      else if not mayOverflow then
        ( emitSrem (); reg )
      else
        case overflowFlag of
          NONE => "0"
        | SOME flag =>
            let
              val zeroLabel = label "div.mod.zero"
              val computeLabel = label "div.mod.compute"
              val doneLabel = label "div.mod.done"
              val computed = fresh "div.mod"
            in
              emit ("br i1 " ^ flag ^ ", label %" ^ zeroLabel ^
                    ", label %" ^ computeLabel ^
                    ", !prof !{!\"branch_weights\", i32 1, i32 1048575}");
              emit (zeroLabel ^ ":");
              emit ("br label %" ^ doneLabel);
              emit (computeLabel ^ ":");
              emit (computed ^ " = srem " ^ ty ^ " " ^ dividendText ^ ", " ^
                    divisorText);
              emit ("br label %" ^ doneLabel);
              emit (doneLabel ^ ":");
              emit (reg ^ " = phi " ^ ty ^ " [ 0, %" ^ zeroLabel ^ " ], [ " ^
                    computed ^ ", %" ^ computeLabel ^ " ]");
              reg
            end
    end

  fun freshLabel (env : env, prefix) =
    let val name = #fresh env prefix in String.extract (name, 1, NONE) end

  fun checkedIntOp (env : env, intrinsic, ty, leftText, rightText) =
    let
      val emit = #emit env
      val fresh = #fresh env
      val pair = fresh "arith.ovf"
      val value = fresh "arith.value"
      val flag = fresh "arith.flag"
      val bad = freshLabel (env, "arith.overflow")
      val ok = freshLabel (env, "arith.ok")
    in
      emit (pair ^ " = call { " ^ ty ^ ", i1 } @llvm." ^ intrinsic ^
            ".with.overflow." ^ ty ^ "(" ^ ty ^ " " ^ leftText ^ ", " ^ ty ^
            " " ^ rightText ^ ")");
      emit (value ^ " = extractvalue { " ^ ty ^ ", i1 } " ^ pair ^ ", 0");
      emit (flag ^ " = extractvalue { " ^ ty ^ ", i1 } " ^ pair ^ ", 1");
      emit ("br i1 " ^ flag ^ ", label %" ^ bad ^ ", label %" ^ ok ^
            ", !prof !{!\"branch_weights\", i32 1, i32 1048575}");
      emit (bad ^ ":");
      emit "call void @yack_abort_msg(ptr @.yack_msg_overflow)";
      emit "unreachable";
      emit (ok ^ ":");
      value
    end

  fun checkedNarrow (env : env, text) =
    let
      val emit = #emit env
      val fresh = #fresh env
      val narrow = fresh "int64.to.int"
      val back = fresh "int64.to.int.back"
      val same = fresh "int64.to.int.fits"
      val bad = freshLabel (env, "narrow.overflow")
      val ok = freshLabel (env, "narrow.ok")
    in
      emit (narrow ^ " = trunc i64 " ^ text ^ " to i32");
      emit (back ^ " = sext i32 " ^ narrow ^ " to i64");
      emit (same ^ " = icmp eq i64 " ^ back ^ ", " ^ text);
      emit ("br i1 " ^ same ^ ", label %" ^ ok ^ ", label %" ^ bad ^
            ", !prof !{!\"branch_weights\", i32 1048575, i32 1}");
      emit (bad ^ ":");
      emit "call void @yack_abort_msg(ptr @.yack_msg_overflow)";
      emit "unreachable";
      emit (ok ^ ":");
      narrow
    end

  fun integerIntrinsic operator =
    case operator of
      "+" => SOME "sadd"
    | "-" => SOME "ssub"
    | "*" => SOME "smul"
    | _ => NONE

  fun lowerIntNeg (env : env, value : value) =
    checkedIntOp (env, "ssub", llvmTy (#ty value), "0", #text value)

  fun lowerBin (env : env, operator, left : value, right : value, resultTy : Ir.ty) =
    let
      val emit = #emit env
      val fresh = #fresh env
    in
      if operator = "^" then
        let val reg = fresh "concat" in
          emit (reg ^ " = call { ptr, i64, i1 } @yack_string_concat(" ^
                typed left ^ ", " ^ typed right ^ ")");
          { text = reg, ty = resultTy, const = false }
        end
      else
      let
        val comparison =
          operator = "=" orelse operator = "<>" orelse
          operator = "<" orelse operator = "<=" orelse
          operator = ">" orelse operator = ">="
        val target =
          if comparison then commonNumeric (#ty left, #ty right)
          else resultTy
        val left' = coerce (env, left, target)
        val right' = coerce (env, right, target)
        val resultType = if comparison then Ir.Bool else resultTy
        val reg = fresh "bin"
        val isDivision = operator = "/" orelse operator = "%"
        val resultText =
          case target of
            Ir.Int width =>
              if comparison then
                ( emit (reg ^ " = icmp " ^ intComparison operator ^ " " ^
                        llvmTy target ^ " " ^ #text left' ^ ", " ^ #text right')
                ; reg )
              else if isDivision then
                lowerIntDivision (env, operator, width, left', right', reg)
              else
                (case integerIntrinsic operator of
                   SOME intrinsic =>
                     checkedIntOp
                       (env, intrinsic, llvmTy target, #text left', #text right')
                 | NONE =>
                     ( emit (reg ^ " = " ^ intArithmetic operator ^ " " ^
                             llvmTy target ^ " " ^ #text left' ^ ", " ^
                             #text right')
                     ; reg ) )
          | Ir.Float _ =>
              ( if comparison then
                  emit (reg ^ " = fcmp " ^ floatComparison operator ^ " " ^
                        llvmTy target ^ " " ^ #text left' ^ ", " ^ #text right')
                else
                  emit (reg ^ " = " ^ floatArithmetic operator ^ " " ^
                        llvmTy target ^ " " ^ #text left' ^ ", " ^ #text right')
              ; reg )
          | Ir.Bool =>
              ( emit (reg ^ " = icmp " ^
                      (if operator = "<>" then "ne" else "eq") ^
                      " i1 " ^ #text left' ^ ", " ^ #text right')
              ; reg )
          | _ => reg
        in
          { text = resultText, ty = resultType, const = false }
        end
    end
end
