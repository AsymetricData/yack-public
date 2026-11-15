structure LlvmFunction =
struct
  open LlvmTypes

  type env =
    { emit : string -> unit
    , fresh : string -> string
    , freshLabel : string -> string
    , lowerTailExpr :
        Ir.drop list -> string list -> localEntry list -> Ir.expr
        -> value * bool * bool
    , collectBorrowRoots : Ir.expr -> StringSet.t
    , coerce : value * Ir.ty -> value
    , typed : value -> string
    , dropValue : value * Ir.ty -> unit
    , dropValueSkipping : value * Ir.ty * int list list -> unit
    , findLocal : localEntry list -> string -> value option
    , bindLocal : localEntry list -> string -> value -> localEntry list
    , allocStack : Ir.ty -> string
    , copyValue : value * Ir.ty -> value
    , rootValue : value * Ir.ty -> value
    , currentRegionArg : string ref
    , currentDestArg : string ref
    , currentFunctionName : string ref
    , currentIsRecursive : bool ref
    , currentNeedsRegion : bool ref
    , addressTaken : StringSet.t ref
    , isPreludeOption : string -> bool
    , selfLoop : (value list -> unit) option ref
    , stackPlan : StackPlan.plan
    , stackMode : bool
    , inEntryBlock : bool ref
    , hasDynamicAlloca : bool ref
    , resetArenaMarks : unit -> unit
    }

  fun lowerFunctionWith (env : env) = let
  val emit = #emit env
  val fresh = #fresh env
  val freshLabel = #freshLabel env
  val lowerTailExpr = #lowerTailExpr env
  val collectBorrowRoots = #collectBorrowRoots env
  val coerce = #coerce env
  val typed = #typed env
  val dropValue = #dropValue env
  val dropValueSkipping = #dropValueSkipping env
  val findLocal = #findLocal env
  val bindLocal = #bindLocal env
  val allocStack = #allocStack env
  val copyValue = #copyValue env
  val rootValue = #rootValue env
  val currentRegionArg = #currentRegionArg env
  val currentDestArg = #currentDestArg env
  val currentFunctionName = #currentFunctionName env
  val currentIsRecursive = #currentIsRecursive env
  val currentNeedsRegion = #currentNeedsRegion env
  val addressTaken = #addressTaken env
  val isPreludeOption = #isPreludeOption env
  val selfLoop = #selfLoop env
  val stackPlan = #stackPlan env
  val stackMode = #stackMode env
  val inEntryBlock = #inEntryBlock env
  val hasDynamicAlloca = #hasDynamicAlloca env
  val resetArenaMarks = #resetArenaMarks env

fun lowerFunction
      ({ sourceName, name, params, result, recursive, body, drops,
         borrowed = _ } : Ir.funDef) =
  let
    fun parameterType (Ir.Unit) = "i8"
      | parameterType ty = llvmTy ty
    val indexedParams =
      ListPair.zip
        (List.tabulate (List.length params, fn i => i), params)
    val parametersText =
      String.concatWith ", "
        (List.map
           (fn (index, (_, ty)) =>
             parameterType ty ^ " %arg" ^ Int.toString index)
           indexedParams)
    val allParameters =
      if not stackMode then parametersText
      else if parametersText = "" then "ptr %region, ptr %dest"
      else "ptr %region, ptr %dest, " ^ parametersText
    val functionNeedsRegion =
      (not stackMode) orelse StackPlan.needsRegion stackPlan name

    val loopLabel =
      if recursive andalso LlvmTail.tailCallsSelf (name, body) then
        SOME (freshLabel "loop")
      else NONE
    val loopBackedge = if Option.isSome loopLabel then freshLabel "loop.backedge" else ""

    val _ = resetArenaMarks ()
    val _ = inEntryBlock := false
    val _ = hasDynamicAlloca := false
    val _ = emit ("define " ^ llvmTy result ^ " @" ^ name ^
                  "(" ^ allParameters ^ ") {")
    val _ = emit "entry:"
    val _ =
      case loopLabel of
        NONE =>
          if stackMode andalso functionNeedsRegion then
            emit "call void @yack_region_init(ptr %region, ptr %dest)"
          else ()
      | SOME _ => ()
    val loopInfo =
      case loopLabel of
        NONE => NONE
      | SOME label =>
          let
            val slots =
              List.map
                (fn (_, ty) => if isUnit ty then NONE else SOME (allocStack ty))
                params
            val phis = List.tabulate (List.length params, fn _ => fresh "loop.arg")
            val nexts = List.tabulate (List.length params, fn _ => fresh "loop.next")
            val stackReg = fresh "loop.stack"
            fun phiLine ((index, (_, ty)), (phi, next)) =
              if isUnit ty then NONE
              else
                SOME (phi ^ " = phi " ^ llvmTy ty ^
                      " [ %arg" ^ Int.toString index ^ ", %entry ], [ " ^
                      next ^ ", %" ^ loopBackedge ^ " ]")
            val lines =
              List.map phiLine
                (ListPair.zip (indexedParams, ListPair.zip (phis, nexts)))
          in
            emit ("br label %" ^ label);
            emit (label ^ ":");
            List.app (fn SOME line => emit line | NONE => ()) lines;

            emit (stackReg ^ " = call ptr @llvm.stacksave()");
            if stackMode andalso functionNeedsRegion then
              emit "call void @yack_region_init(ptr %region, ptr %dest)"
            else ();
            SOME (label, slots, phis, nexts, stackReg)
          end
    val _ = currentRegionArg := "%region"
    val _ = currentDestArg := "%dest"
    val _ = currentFunctionName := name
    val _ = currentIsRecursive := recursive
    val _ = currentNeedsRegion := functionNeedsRegion
    val _ = addressTaken := collectBorrowRoots body
    fun parameterValue (index, ty) =
      if isUnit ty then ""
      else
        case loopInfo of
          SOME (_, _, phis, _, _) => List.nth (phis, index)
        | NONE => "%arg" ^ Int.toString index
    fun addParameter ((index, (paramName, ty)), locals) =
      bindLocal locals paramName
        { text = parameterValue (index, ty)
        , ty = ty
        , const = false
        }
    val locals =
      List.foldl
        (fn (entry, locals) => addParameter (entry, locals))
        []
        indexedParams
    val safeNames = List.map (fn (name, _) => name) params
    val _ =
      selfLoop :=
        (case loopInfo of
          NONE => NONE
        | SOME (_, slots, _, _, stackReg) =>
            SOME
              (fn arguments =>
                 let
                   fun storeArgument (argument, ((_, ty), slot)) =
                     case slot of
                       NONE => ()
                     | SOME destination =>
                         let val value = coerce (argument, ty) in
                           emit ("store " ^ llvmTy ty ^ " " ^ #text value ^
                                 ", ptr " ^ destination)
                         end
                 in
                   List.app storeArgument
                     (ListPair.zip
                        (arguments, ListPair.zip (params, slots)));
                   emit ("call void @llvm.stackrestore(ptr " ^ stackReg ^ ")");
                   emit ("br label %" ^ loopBackedge)
                 end))
    val (bodyValue, tailReturned, dropsHandled) =
      lowerTailExpr drops safeNames locals body
    val _ =
      if tailReturned orelse dropsHandled then ()
      else
        List.app
          (fn {name, ty, skip, ...} =>
            case findLocal locals name of
              SOME bound => dropValueSkipping (bound, ty, skip)
            | NONE => ())
          drops
  in
    if tailReturned then ()
    else
      ( if stackMode andalso functionNeedsRegion then
          emit "call void @yack_region_destroy(ptr %region)"
        else ()
      ; if isUnit result then
          emit "ret void"
        else
          emit ("ret " ^ llvmTy result ^ " " ^
                #text (coerce (bodyValue, result)))
      );
    (case loopInfo of
       NONE => ()
     | SOME (label, slots, _, nexts, _) =>
         let
           fun loadNext ((_, ty), (slot, next)) =
             case slot of
               NONE => ()
             | SOME source =>
                 emit (next ^ " = load " ^ llvmTy ty ^ ", ptr " ^ source)
         in
           emit (loopBackedge ^ ":");
           List.app loadNext
             (ListPair.zip (params, ListPair.zip (slots, nexts)));
           emit ("br label %" ^ label)
         end);
    emit "}"
  end

fun generateDropFunctionWith (owner : bool) (definition : Ir.dataDef) =
  let
    val dataName = #name definition
    val first = "%first"
    fun defineHeader () =
      if owner then
        emit ("define void @yack_owner_drop_" ^ dataName ^
              "(ptr %region, ptr " ^ first ^ ") {")
      else
        emit ("define void @yack_drop_" ^ dataName ^ "(ptr " ^ first ^ ") {")
    fun freeNode (nodeReg, typeName) =
      if owner then
        emit ("call void @yack_owner_recycle_block(ptr %region, ptr " ^
              nodeReg ^ ", i64 ptrtoint (ptr getelementptr (%" ^ typeName ^
              ", ptr null, i32 1) to i64))")
      else
        emit ("call void @yack_free(ptr " ^ nodeReg ^ ")")

    fun ownerDropValue (value, ty) = ownerDropSkipping (value, ty, [])
    and ownerDropSkipping (value, ty, skip) =
      let
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
        fun fields types =
          let
            fun loop (index, remaining) =
              case remaining of
                [] => ()
              | fieldTy :: rest =>
                  ( if skippedWhole index then ()
                    else
                      let val reg = fresh "odrop.field" in
                        emit (reg ^ " = extractvalue " ^ llvmTy ty ^ " " ^
                              #text value ^ ", " ^ Int.toString index);
                        ownerDropSkipping
                          ({ text = reg, ty = fieldTy, const = false },
                           fieldTy, subPaths index)
                      end
                  ; loop (index + 1, rest) )
          in
            loop (0, types)
          end
      in
        case ty of
          Ir.TString =>
            emit ("call void @yack_owner_recycle_string(ptr %region, " ^
                  "{ ptr, i64, i1 } " ^ #text value ^ ")")
        | Ir.TData (name, _, _) =>
            if List.null skip then
              emit ("call void @yack_owner_drop_" ^ name ^
                    "(ptr %region, ptr " ^ #text value ^ ")")
            else ()
        | Ir.TTuple types => fields types
        | Ir.TRecord (recordFields, _) => fields (List.map #2 recordFields)
        | _ => ()
      end
    fun dropField (value, ty) =
      if owner then ownerDropValue (value, ty) else dropValue (value, ty)
    fun dropFieldSkipping (value, ty, skip) =
      if owner then ownerDropSkipping (value, ty, skip)
      else dropValueSkipping (value, ty, skip)
    val node = "%node"
    val loopLabel = freshLabel "drop.loop"
    val bodyLabel = freshLabel "drop.body"
    val doneLabel = freshLabel "drop.done"
    val freeLabel = freshLabel "drop.free"
    val nextLabel = freshLabel "drop.next"
    val nullCheck = fresh "drop.null"
    val tagPtr = fresh "drop.tagp"
    val tag = fresh "drop.tag"
    val nextSlot = fresh "drop.nextslot"
    val next = fresh "drop.nextnode"
    val stackSave = fresh "drop.stacksave"

    datatype keepPath =
        DirectPath of int list
      | OptionPath of int list * string list

    fun selfPaths ty =
      case ty of
        Ir.TData (name, _, _) =>
          if name = dataName then [DirectPath []] else []
      | Ir.TTuple types => indexedPaths selfPaths types
      | Ir.TRecord (fields, _) =>
          indexedPaths selfPaths (List.map #2 fields)
      | _ => []
    and indexedPaths pathsOf types =
      List.concat
        (List.tabulate
           (List.length types,
            fn index =>
              List.map (fn path => prepend index path)
                (pathsOf (List.nth (types, index)))))
    and prepend index path =
      case path of
        DirectPath rest => DirectPath (index :: rest)
      | OptionPath (rest, optionNames) =>
          OptionPath (index :: rest, optionNames)

    fun isOptionData name = isPreludeOption name

    fun optionChainToSelf ty =
      case ty of
        Ir.TData (name, [inner], _) =>
          if isOptionData name then
            (case inner of
               Ir.TData (innerName, _, _) =>
                 if innerName = dataName then
                   SOME [name]
                 else
                   (case optionChainToSelf inner of
                      SOME rest => SOME (name :: rest)
                    | NONE => NONE)
             | _ => NONE)
          else NONE
      | _ => NONE

    fun optionPaths ty =
      case ty of
        Ir.TData (_, _, _) =>
          (case optionChainToSelf ty of
             SOME optionNames => [OptionPath ([], optionNames)]
           | NONE => [])
      | Ir.TTuple types => indexedPaths optionPaths types
      | Ir.TRecord (fields, _) =>
          indexedPaths optionPaths (List.map #2 fields)
      | _ => []
    val payloadConstructors =
      let
        fun loop (tagIndex, payloadField, constructors, acc) =
          case constructors of
            [] => List.rev acc
          | (_, argument) :: rest =>
              (case argument of
                 NONE => loop (tagIndex + 1, payloadField, rest, acc)
               | SOME ty =>
                   loop
                     ( tagIndex + 1
                     , payloadField + 1
                     , rest
                     , (tagIndex, payloadField, ty,
                        List.foldl (fn (path, _) => SOME path) NONE
                          (selfPaths ty @ optionPaths ty)) :: acc
                     ))
      in
        loop (0, 2, #constructors definition, [])
      end
    val loops =
      List.exists (fn (_, _, _, kept) => Option.isSome kept)
        payloadConstructors
    val _ = defineHeader ()
    val _ = emit "entry:"
    val _ =
      if loops then emit (nextSlot ^ " = alloca ptr") else ()
    val _ = emit ("br label %" ^ loopLabel)
    val _ = emit (loopLabel ^ ":")
    val _ =
      emit (node ^ " = phi ptr [ " ^ first ^ ", %entry ]" ^
            (if loops then ", [ " ^ next ^ ", %" ^ nextLabel ^ " ]" else ""))
    val _ = emit (nullCheck ^ " = icmp eq ptr " ^ node ^ ", null")
    val _ = emit ("br i1 " ^ nullCheck ^ ", label %" ^ doneLabel ^
                  ", label %" ^ bodyLabel)
    val _ = emit (bodyLabel ^ ":")

    val _ =
      if loops then
        ( emit (stackSave ^ " = call ptr @llvm.stacksave()")
        ; emit ("store ptr null, ptr " ^ nextSlot) )
      else ()
    val stackPtr = fresh "drop.stackp"
    val stackFlag = fresh "drop.stack"
    val freeBlock = freshLabel "drop.freeblock"
    val _ = emit (tagPtr ^ " = getelementptr inbounds %" ^ dataName ^
                  ", ptr " ^ node ^ ", i32 0, i32 0")
    val _ = emit (tag ^ " = load i32, ptr " ^ tagPtr)
    val _ = emit (stackPtr ^ " = getelementptr inbounds %" ^ dataName ^
                  ", ptr " ^ node ^ ", i32 0, i32 1")
    val _ = emit (stackFlag ^ " = load i1, ptr " ^ stackPtr)
    val _ =
      if List.null payloadConstructors then
        emit ("br label %" ^ freeLabel)
      else
        let
          val cases =
            List.map
              (fn (tagIndex, _, _, _) =>
                "i32 " ^ Int.toString tagIndex ^
                ", label %drop.case" ^ Int.toString tagIndex)
              payloadConstructors
          val _ =
            emit ("switch i32 " ^ tag ^ ", label %" ^ freeLabel ^
                  " [ " ^ String.concatWith " " cases ^ " ]")
          val _ =
            List.app
              (fn (tagIndex, payloadField, payloadTy, kept) =>
                let
                  val payloadPtr = fresh "drop.payloadp"
                  val payloadValue = fresh "drop.payload"
                  val payload =
                    { text = payloadValue, ty = payloadTy, const = false }
                  val _ = emit ("drop.case" ^ Int.toString tagIndex ^ ":")
                  val _ =
                    emit (payloadPtr ^ " = getelementptr inbounds %" ^
                          dataName ^ ", ptr " ^ node ^
                          ", i32 0, i32 " ^ Int.toString payloadField)
                  val _ =
                    emit (payloadValue ^ " = load " ^ storageTy payloadTy ^
                          ", ptr " ^ payloadPtr)
                  val _ =
                    case kept of
                      NONE => dropField (payload, payloadTy)
                    | SOME (DirectPath []) =>

                        emit ("store ptr " ^ payloadValue ^ ", ptr " ^
                              nextSlot)
                    | SOME (DirectPath path) =>
                        let val kept = fresh "drop.kept" in

                          dropFieldSkipping (payload, payloadTy, [path]);
                          emit (kept ^ " = extractvalue " ^
                                llvmTy payloadTy ^ " " ^ payloadValue ^ ", " ^
                                String.concatWith ", "
                                  (List.map Int.toString path));
                          emit ("store ptr " ^ kept ^ ", ptr " ^ nextSlot)
                        end
                    | SOME (OptionPath (path, optionNames)) =>
                        let
                          val optionValue =
                            if List.null path then payloadValue
                            else
                              let val reg = fresh "drop.option" in
                                emit (reg ^ " = extractvalue " ^
                                      llvmTy payloadTy ^ " " ^ payloadValue ^
                                      ", " ^
                                      String.concatWith ", "
                                        (List.map Int.toString path));
                                reg
                              end
                          val doneLabel = freshLabel "drop.opt.done"
                          fun emitLayers names value =
                            case names of
                              [] =>
                                ( emit ("store ptr " ^ value ^ ", ptr " ^
                                        nextSlot)
                                ; emit ("br label %" ^ doneLabel) )
                            | optionName :: rest =>
                                let
                                  val nullLabel = freshLabel "drop.opt.none"
                                  val bodyLabel = freshLabel "drop.opt.body"
                                  val someLabel = freshLabel "drop.opt.some"
                                  val freeLabel = freshLabel "drop.opt.free"
                                  val keepLabel = freshLabel "drop.opt.keep"
                                  val isNull = fresh "drop.opt.isnull"
                                  val tagPtr = fresh "drop.opt.tagp"
                                  val tag = fresh "drop.opt.tag"
                                  val isSome = fresh "drop.opt.issome"
                                  val stackPtr = fresh "drop.opt.stackp"
                                  val stack = fresh "drop.opt.stack"
                                  val payloadPtr = fresh "drop.opt.payloadp"
                                  val nextPayload = fresh "drop.opt.payload"
                                in
                                  emit (isNull ^ " = icmp eq ptr " ^ value ^
                                        ", null");
                                  emit ("br i1 " ^ isNull ^ ", label %" ^
                                        nullLabel ^ ", label %" ^ bodyLabel);
                                  emit (bodyLabel ^ ":");
                                  emit (tagPtr ^
                                        " = getelementptr inbounds %" ^
                                        optionName ^ ", ptr " ^ value ^
                                        ", i32 0, i32 0");
                                  emit (tag ^ " = load i32, ptr " ^ tagPtr);
                                  emit (isSome ^ " = icmp eq i32 " ^ tag ^
                                        ", 1");
                                  emit ("br i1 " ^ isSome ^ ", label %" ^
                                        someLabel ^ ", label %" ^ nullLabel);
                                  emit (someLabel ^ ":");
                                  emit (payloadPtr ^
                                        " = getelementptr inbounds %" ^
                                        optionName ^ ", ptr " ^ value ^
                                        ", i32 0, i32 2");
                                  emit (nextPayload ^ " = load ptr, ptr " ^
                                        payloadPtr);
                                  emit (stackPtr ^
                                        " = getelementptr inbounds %" ^
                                        optionName ^ ", ptr " ^ value ^
                                        ", i32 0, i32 1");
                                  emit (stack ^ " = load i1, ptr " ^
                                        stackPtr);
                                  emit ("br i1 " ^ stack ^ ", label %" ^
                                        keepLabel ^ ", label %" ^ freeLabel);
                                  emit (freeLabel ^ ":");
                                  freeNode (value, optionName);
                                  emit ("br label %" ^ keepLabel);
                                  emit (keepLabel ^ ":");
                                  emitLayers rest nextPayload;
                                  emit (nullLabel ^ ":");
                                  emit ("store ptr null, ptr " ^ nextSlot);
                                  emit ("br label %" ^ doneLabel)
                                end
                        in

                          dropValueSkipping (payload, payloadTy, [path]);
                          emitLayers optionNames optionValue;
                          emit (doneLabel ^ ":")
                        end
                  val _ = emit ("br label %" ^ freeLabel)
                in
                  ()
                end)
              payloadConstructors
        in
          ()
        end
    val _ = emit (freeLabel ^ ":")
    val afterFree = if loops then nextLabel else doneLabel
    val _ =
      emit ("br i1 " ^ stackFlag ^ ", label %" ^ afterFree ^
            ", label %" ^ freeBlock)
    val _ = emit (freeBlock ^ ":")
    val _ = freeNode (node, dataName)
    val _ = emit ("br label %" ^ afterFree)
    val _ =
      if loops then
        ( emit (nextLabel ^ ":")
        ; emit (next ^ " = load ptr, ptr " ^ nextSlot)
        ; emit ("call void @llvm.stackrestore(ptr " ^ stackSave ^ ")")
        ; emit ("br label %" ^ loopLabel) )
      else ()
    val _ = emit (doneLabel ^ ":")
    val _ = emit "ret void"
    val _ = emit "}"
  in
    ()
  end

fun generateCopyFunction (definition : Ir.dataDef) =
  let
    val dataName = #name definition
    val node = "%node"
    val nullLabel = freshLabel "copy.null"
    val bodyLabel = freshLabel "copy.body"
    val doneLabel = freshLabel "copy.done"
    val isNull = fresh "copy.isnull"
    val sourceTagPtr = fresh "copy.tagp"
    val sourceTag = fresh "copy.tag"
    val sizePtr = fresh "copy.sizeptr"
    val size = fresh "copy.size"
    val memory = fresh "copy.mem"
    val destTagPtr = fresh "copy.desttagp"
    val destFlagPtr = fresh "copy.destflagp"
    val payloadConstructors =
      let
        fun loop (tagIndex, payloadField, constructors, acc) =
          case constructors of
            [] => List.rev acc
          | (_, argument) :: rest =>
              (case argument of
                 NONE => loop (tagIndex + 1, payloadField, rest, acc)
               | SOME ty =>
                   loop
                     ( tagIndex + 1
                     , payloadField + 1
                     , rest
                     , (tagIndex, payloadField, ty) :: acc
                     ))
      in
        loop (0, 2, #constructors definition, [])
      end
    val _ =
      emit ("define ptr @yack_copy_" ^ dataName ^ "(ptr " ^ node ^ ") {")
    val _ = emit "entry:"
    val _ = emit (isNull ^ " = icmp eq ptr " ^ node ^ ", null")
    val _ =
      emit ("br i1 " ^ isNull ^ ", label %" ^ nullLabel ^
            ", label %" ^ bodyLabel)
    val _ = emit (nullLabel ^ ":")
    val _ = emit "ret ptr null"
    val _ = emit (bodyLabel ^ ":")
    val _ =
      emit (sizePtr ^ " = getelementptr %" ^ dataName ^
            ", ptr null, i32 1")
    val _ = emit (size ^ " = ptrtoint ptr " ^ sizePtr ^ " to i64")
    val _ =
      emit (memory ^ " = call ptr @yack_alloc(i64 " ^ size ^ ")")
    val _ =
      emit (sourceTagPtr ^ " = getelementptr inbounds %" ^ dataName ^
            ", ptr " ^ node ^ ", i32 0, i32 0")
    val _ = emit (sourceTag ^ " = load i32, ptr " ^ sourceTagPtr)
    val _ =
      emit (destTagPtr ^ " = getelementptr inbounds %" ^ dataName ^
            ", ptr " ^ memory ^ ", i32 0, i32 0")
    val _ = emit ("store i32 " ^ sourceTag ^ ", ptr " ^ destTagPtr)
    val _ =
      emit (destFlagPtr ^ " = getelementptr inbounds %" ^ dataName ^
            ", ptr " ^ memory ^ ", i32 0, i32 1")
    val _ = emit ("store i1 false, ptr " ^ destFlagPtr)
    val _ =
      if List.null payloadConstructors then
        emit ("br label %" ^ doneLabel)
      else
        let
          val cases =
            List.map
              (fn (tagIndex, _, _) =>
                "i32 " ^ Int.toString tagIndex ^
                ", label %copy.case" ^ Int.toString tagIndex)
              payloadConstructors
          val _ =
            emit ("switch i32 " ^ sourceTag ^ ", label %" ^ doneLabel ^
                  " [ " ^ String.concatWith " " cases ^ " ]")
          val _ =
            List.app
              (fn (tagIndex, payloadField, payloadTy) =>
                let
                  val sourcePtr = fresh "copy.payloadsrc"
                  val sourceValue = fresh "copy.payload"
                  val copied = fresh "copy.payloadcopy"
                  val destPtr = fresh "copy.payloaddst"
                  val _ = emit ("copy.case" ^ Int.toString tagIndex ^ ":")
                  val _ =
                    emit (sourcePtr ^ " = getelementptr inbounds %" ^
                          dataName ^ ", ptr " ^ node ^
                          ", i32 0, i32 " ^ Int.toString payloadField)
                  val _ =
                    emit (sourceValue ^ " = load " ^ storageTy payloadTy ^
                          ", ptr " ^ sourcePtr)
                  val copiedValue =
                    copyValue
                      ({ text = sourceValue
                       , ty = payloadTy
                       , const = false
                       }, payloadTy)
                  val _ =
                    emit (destPtr ^ " = getelementptr inbounds %" ^
                          dataName ^ ", ptr " ^ memory ^
                          ", i32 0, i32 " ^ Int.toString payloadField)
                  val _ =
                    emit ("store " ^ typed copiedValue ^
                          ", ptr " ^ destPtr)
                  val _ = emit ("br label %" ^ doneLabel)
                in
                  ()
                end)
              payloadConstructors
        in
          ()
        end
    val _ = emit (doneLabel ^ ":")
    val _ = emit ("ret ptr " ^ memory)
    val _ = emit "}"
  in
    ()
  end

fun generateRootFunction (definition : Ir.dataDef) =
  let
    val dataName = #name definition
    val node = "%node"
    val keepLabel = freshLabel "root.keep"
    val forceLabel = freshLabel "root.force"
    val checkLabel = freshLabel "root.check"
    val inArena = fresh "root.inarena"
    val sourceTagPtr = fresh "root.tagp"
    val sourceTag = fresh "root.tag"

    fun orReg (left, right) =
      if left = "false" then right
      else if right = "false" then left
      else
        let
          val reg = fresh "root.changed.or"
          val _ = emit (reg ^ " = or i1 " ^ left ^ ", " ^ right)
        in
          reg
        end

    fun changedValue (original : value, rooted : value, ty) =
      case ty of
        Ir.TString =>
          let
            val oldData = fresh "root.olddata"
            val newData = fresh "root.newdata"
            val changed = fresh "root.strchanged"
            val _ =
              emit (oldData ^ " = extractvalue { ptr, i64, i1 } " ^
                    #text original ^ ", 0")
            val _ =
              emit (newData ^ " = extractvalue { ptr, i64, i1 } " ^
                    #text rooted ^ ", 0")
            val _ = emit (changed ^ " = icmp ne ptr " ^ oldData ^
                          ", " ^ newData)
          in
            changed
          end
      | Ir.TData _ =>
          let val changed = fresh "root.datachanged" in
            emit (changed ^ " = icmp ne ptr " ^ #text original ^ ", " ^
                  #text rooted);
            changed
          end
      | Ir.TTuple types =>
          let
            fun loop (index, remaining, acc) =
              case remaining of
                [] => acc
              | fieldTy :: rest =>
                  let
                    val oldField = fresh "root.oldfield"
                    val newField = fresh "root.newfield"
                    val _ =
                      emit (oldField ^ " = extractvalue " ^ llvmTy ty ^
                            " " ^ #text original ^ ", " ^
                            Int.toString index)
                    val _ =
                      emit (newField ^ " = extractvalue " ^ llvmTy ty ^
                            " " ^ #text rooted ^ ", " ^
                            Int.toString index)
                    val fieldChanged =
                      changedValue
                        ( { text = oldField, ty = fieldTy, const = false }
                        , { text = newField, ty = fieldTy, const = false }
                        , fieldTy
                        )
                  in
                    loop (index + 1, rest, orReg (acc, fieldChanged))
                  end
          in
            loop (0, types, "false")
          end
      | Ir.TRecord (fields, _) =>
          let
            fun loop (index, remaining, acc) =
              case remaining of
                [] => acc
              | (_, fieldTy) :: rest =>
                  let
                    val oldField = fresh "root.oldfield"
                    val newField = fresh "root.newfield"
                    val _ =
                      emit (oldField ^ " = extractvalue " ^ llvmTy ty ^
                            " " ^ #text original ^ ", " ^
                            Int.toString index)
                    val _ =
                      emit (newField ^ " = extractvalue " ^ llvmTy ty ^
                            " " ^ #text rooted ^ ", " ^
                            Int.toString index)
                    val fieldChanged =
                      changedValue
                        ( { text = oldField, ty = fieldTy, const = false }
                        , { text = newField, ty = fieldTy, const = false }
                        , fieldTy
                        )
                  in
                    loop (index + 1, rest, orReg (acc, fieldChanged))
                  end
          in
            loop (0, fields, "false")
          end
      | _ => "false"

    val payloadConstructors =
      let
        fun loop (tagIndex, payloadField, constructors, acc) =
          case constructors of
            [] => List.rev acc
          | (_, argument) :: rest =>
              (case argument of
                 NONE => loop (tagIndex + 1, payloadField, rest, acc)
               | SOME ty =>
                   loop
                     ( tagIndex + 1
                     , payloadField + 1
                     , rest
                     , (tagIndex, payloadField, ty) :: acc
                     ))
      in
        loop (0, 2, #constructors definition, [])
      end

    fun payloadAddress (payloadField) =
      let
        val pointer = fresh "root.payloadsrc"
        val _ =
          emit (pointer ^ " = getelementptr inbounds %" ^ dataName ^
                ", ptr " ^ node ^ ", i32 0, i32 " ^
                Int.toString payloadField)
      in
        pointer
      end

    fun loadPayload (payloadField, payloadTy) =
      let
        val pointer = payloadAddress payloadField
        val value = fresh "root.payload"
        val _ = emit (value ^ " = load " ^ storageTy payloadTy ^
                      ", ptr " ^ pointer)
      in
        { text = value, ty = payloadTy, const = false }
      end

    fun emitCopyHeader (memory) =
      let
        val destTagPtr = fresh "root.desttagp"
        val destFlagPtr = fresh "root.destflagp"
        val _ =
          emit (destTagPtr ^ " = getelementptr inbounds %" ^ dataName ^
                ", ptr " ^ memory ^ ", i32 0, i32 0")
        val _ = emit ("store i32 " ^ sourceTag ^ ", ptr " ^ destTagPtr)
        val _ =
          emit (destFlagPtr ^ " = getelementptr inbounds %" ^ dataName ^
                ", ptr " ^ memory ^ ", i32 0, i32 1")
        val _ = emit ("store i1 false, ptr " ^ destFlagPtr)
      in
        ()
      end

    fun emitAllocate () =
      let
        val sizePtr = fresh "root.sizeptr"
        val size = fresh "root.size"
        val memory = fresh "root.mem"
        val _ =
          emit (sizePtr ^ " = getelementptr %" ^ dataName ^
                ", ptr null, i32 1")
        val _ = emit (size ^ " = ptrtoint ptr " ^ sizePtr ^ " to i64")
        val _ =
          emit (memory ^ " = call ptr @yack_root_alloc(i64 " ^ size ^ ")")
        val _ = emitCopyHeader memory
      in
        memory
      end

    fun emitFreeOldNode () =
      let
        val stackPtr = fresh "root.stackp"
        val stackFlag = fresh "root.stack"
        val freeLabel = freshLabel "root.freeold"
        val doneLabel = freshLabel "root.freedone"
        val _ =
          emit (stackPtr ^ " = getelementptr inbounds %" ^ dataName ^
                ", ptr " ^ node ^ ", i32 0, i32 1")
        val _ = emit (stackFlag ^ " = load i1, ptr " ^ stackPtr)
        val _ =
          emit ("br i1 " ^ stackFlag ^ ", label %" ^ doneLabel ^
                ", label %" ^ freeLabel)
        val _ = emit (freeLabel ^ ":")
        val _ = emit ("call void @yack_free(ptr " ^ node ^ ")")
        val _ = emit ("br label %" ^ doneLabel)
        val _ = emit (doneLabel ^ ":")
      in
        ()
      end

    fun emitCopyPayload (payloadField, payloadTy, rooted) =
      let
        val memory = emitAllocate ()
        val destPtr = fresh "root.payloaddst"
        val _ =
          emit (destPtr ^ " = getelementptr inbounds %" ^ dataName ^
                ", ptr " ^ memory ^ ", i32 0, i32 " ^
                Int.toString payloadField)
        val _ =
          emit ("store " ^ storageTy payloadTy ^ " " ^ #text rooted ^
                ", ptr " ^ destPtr)
        val _ = emitFreeOldNode ()
      in
        emit ("ret ptr " ^ memory)
      end

    fun emitEmptyCopy () =
      let
        val memory = emitAllocate ()
        val _ = emitFreeOldNode ()
      in
        emit ("ret ptr " ^ memory)
      end
    val nullLabel = freshLabel "root.null"
    val bodyLabel = freshLabel "root.body"
    val isNull = fresh "root.isnull"
  in
    emit ("define ptr @yack_root_" ^ dataName ^ "(ptr " ^ node ^ ") {");
    emit "entry:";
    emit (isNull ^ " = icmp eq ptr " ^ node ^ ", null");
    emit ("br i1 " ^ isNull ^ ", label %" ^ nullLabel ^
          ", label %" ^ bodyLabel);
    emit (nullLabel ^ ":");
    emit "ret ptr null";
    emit (bodyLabel ^ ":");
    emit (sourceTagPtr ^ " = getelementptr inbounds %" ^ dataName ^
          ", ptr " ^ node ^ ", i32 0, i32 0");
    emit (sourceTag ^ " = load i32, ptr " ^ sourceTagPtr);
    emit (inArena ^ " = call i1 @yack_ptr_in_arena(ptr " ^ node ^ ")");
    emit ("br i1 " ^ inArena ^ ", label %" ^ forceLabel ^
          ", label %" ^ checkLabel);
    emit (keepLabel ^ ":");
    emit ("ret ptr " ^ node);

    if List.null payloadConstructors then
      ( emit (forceLabel ^ ":")
      ; emitEmptyCopy ()
      ; emit (checkLabel ^ ":")
      ; emit ("ret ptr " ^ node)
      )
    else
      let
        fun cases prefix =
          String.concatWith " "
            (List.map
               (fn (tagIndex, _, _) =>
                 "i32 " ^ Int.toString tagIndex ^ ", label %root.case." ^
                 prefix ^ Int.toString tagIndex)
               payloadConstructors)
        fun emitCases (prefix, force) =
          List.app
            (fn (tagIndex, payloadField, payloadTy) =>
              let
                val caseLabel =
                  "root.case." ^ prefix ^ Int.toString tagIndex
                val copyLabel = "root.copy." ^ prefix ^
                                Int.toString tagIndex
                val _ = emit (caseLabel ^ ":")
                val sourceValue = loadPayload (payloadField, payloadTy)
                val rooted = rootValue (sourceValue, payloadTy)
                val _ =
                  if force then
                    emit ("br label %" ^ copyLabel)
                  else
                    let
                      val changed =
                        changedValue (sourceValue, rooted, payloadTy)
                      val _ =
                        emit ("br i1 " ^ changed ^ ", label %" ^ copyLabel ^
                              ", label %" ^ keepLabel)
                    in
                      ()
                    end
                val _ = emit (copyLabel ^ ":")
                val _ = emitCopyPayload (payloadField, payloadTy, rooted)
              in
                ()
              end)
            payloadConstructors
      in
        emit (forceLabel ^ ":");
        emit ("switch i32 " ^ sourceTag ^ ", label %root.copy.empty.force [ " ^
              cases "force" ^ " ]");
        emitCases ("force", true);
        emit ("root.copy.empty.force:");
        emitEmptyCopy ();
        emit (checkLabel ^ ":");
        emit ("switch i32 " ^ sourceTag ^ ", label %" ^ keepLabel ^ " [ " ^
              cases "check" ^ " ]");
        emitCases ("check", false)
      end;
    emit "}";
    ()
  end

    fun generateDropFunction definition =
      generateDropFunctionWith false definition
    fun generateOwnerDropFunction definition =
      generateDropFunctionWith true definition
  in (lowerFunction, generateDropFunction, generateOwnerDropFunction,
      generateCopyFunction, generateRootFunction) end
end
