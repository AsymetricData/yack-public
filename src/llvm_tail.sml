structure LlvmTail =
struct
  open LlvmTypes

  type env =
    { emit : string -> unit
    , fresh : string -> string
    , freshLabel : string -> string
    , lowerExpr : localEntry list -> Ir.expr -> value
    , allocStack : Ir.ty -> string
    , bindLocal : localEntry list -> string -> value -> localEntry list
    , findLocal : localEntry list -> string -> value option
    , coerce : value * Ir.ty -> value
    , typed : value -> string
    , dropValue : value * Ir.ty -> unit
    , freeIfHeap : value -> unit
    , currentFunctionName : string
    , currentIsRecursive : bool ref
    , currentNeedsRegion : bool ref
    , selfLoop : (value list -> unit) option ref
    , stackMode : bool
    , dropValueSkipping : value * Ir.ty * int list list -> unit
    , testPattern :
        bool * bool * value * Ir.pattern * string * string * localEntry list
        -> localEntry list * cleanup list * cleanup list
    , canTailCall : unit -> bool
    }

  fun tailPositions expression =
    case expression of
      Ir.Let (_, body, _) => [body]
    | Ir.If (_, thenBranch, elseBranch, _) => [thenBranch, elseBranch]
    | Ir.Match (_, branches, _) =>
        List.map (fn (_, _, body, _) => body) branches
    | _ => []

  fun tailCallsSelf (self, expression) =
    case expression of
      Ir.Call (name, _, _, _) => name = self
    | _ =>
        List.exists
          (fn inner => tailCallsSelf (self, inner))
          (tailPositions expression)

  fun lowerExprWith (env : env) = let
  val emit = #emit env
  val fresh = #fresh env
  val freshLabel = #freshLabel env
  val lowerExpr = #lowerExpr env
  val allocStack = #allocStack env
  val bindLocal = #bindLocal env
  val findLocal = #findLocal env
  val coerce = #coerce env
  val typed = #typed env
  val dropValue = #dropValue env
  val freeIfHeap = #freeIfHeap env
  val currentFunctionName = #currentFunctionName env
  val currentIsRecursive = #currentIsRecursive env
  val currentNeedsRegion = #currentNeedsRegion env
  val selfLoop = #selfLoop env
  val stackMode = #stackMode env
  val dropValueSkipping = #dropValueSkipping env
  val testPattern = #testPattern env
  val canTailCall = #canTailCall env

fun scalarType ty =
  case ty of
    Ir.Int _ => true
  | Ir.Float _ => true
  | Ir.Bool => true
  | Ir.Unit => true
  | Ir.TToken => true
  | Ir.TResource => true
  | _ => false

fun safeTailType ty =
  case ty of
    Ir.TBorrow _ => false
  | Ir.Int _ => true
  | Ir.Float _ => true
  | Ir.Bool => true
  | Ir.Unit => true
  | Ir.TToken => true
  | Ir.TResource => true
  | Ir.TString => true
  | Ir.TArray _ => true

  | Ir.TMap element => safeTailType element

  | Ir.TFun _ => true
  | Ir.TData (_, _, copy) => not copy
  | Ir.TTuple values => List.all safeTailType values
  | Ir.TRecord (fields, _) =>
      List.all (fn (_, fieldTy) => safeTailType fieldTy) fields

fun isSafeName (safeNames, name) =
  List.exists (fn candidate => candidate = name) safeNames

fun patternNames pattern =
  case pattern of
    Ir.PVar (name, _) => [name]
  | Ir.PWildcard => []
  | Ir.PBool _ => []
  | Ir.PInt _ => []
  | Ir.PString (_, rest, _) =>
      (case rest of
         SOME subpattern => patternNames subpattern
       | NONE => [])
  | Ir.PBytes (_, rest, _) =>
      (case rest of
         SOME subpattern => patternNames subpattern
       | NONE => [])
  | Ir.PTuple (patterns, _) =>
      List.concat (List.map patternNames patterns)
  | Ir.PRecord (fields, _) =>
      List.concat
        (List.map (fn (_, subpattern) => patternNames subpattern) fields)
  | Ir.PCtor (_, _, argument, _) =>
      (case argument of
         SOME subpattern => patternNames subpattern
       | NONE => [])

fun boxedViewType ty =
  case ty of
    Ir.TBorrow (inner, false) => boxedViewType inner
  | Ir.TBorrow (_, true) => false
  | Ir.TData _ => true
  | Ir.TString => true
  | Ir.TArray _ => true
  | Ir.TMap _ => true
  | _ => false

fun boxedPatternNames pattern =
  case pattern of
    Ir.PVar (name, ty) => if boxedViewType ty then [name] else []
  | Ir.PTuple (patterns, _) =>
      List.concat (List.map boxedPatternNames patterns)
  | Ir.PRecord (fields, _) =>
      List.concat
        (List.map (fn (_, subpattern) => boxedPatternNames subpattern) fields)
  | Ir.PCtor (_, _, SOME subpattern, _) => boxedPatternNames subpattern
  | _ => []

fun withoutNames (names, roots) =
  List.filter
    (fn root => not (List.exists (fn name => name = root) names))
    roots

fun rootArgument roots expression =
  case expression of
    Ir.Var (_, Ir.TBorrow (_, true), _) => false
  | Ir.Var (name, _, _) => isSafeName (roots, name)
  | _ => false

fun rootScrutinee roots expression =
  case expression of
    Ir.Var (name, Ir.TBorrow (_, false), _) => isSafeName (roots, name)
  | _ => false

fun safeTailArgument roots safeNames expression =
  let
    val safe = safeTailArgument roots safeNames
    fun safeOrRoot value = rootArgument roots value orelse safe value
  in
  case expression of
    Ir.IntLit _ => true
  | Ir.FloatLit _ => true
  | Ir.StringLit _ => true
  | Ir.BoolLit _ => true
  | Ir.UnitLit => true
  | Ir.Var (name, ty, _) =>
      scalarType ty orelse
      (safeTailType ty andalso isSafeName (safeNames, name))
  | Ir.Bin ("^", _, _, ty, _) =>
      safeTailType ty
  | Ir.Bin (_, _, _, ty, _) => scalarType ty
  | Ir.Neg (_, ty) => scalarType ty
  | Ir.If (condition, thenBranch, elseBranch, _) =>
      safe condition andalso safe thenBranch andalso safe elseBranch
  | Ir.Let (bindings, body, _) =>
      let

        fun extend ((name, _, value), (names, currentRoots)) =
          let
            val safeValue = safeTailArgument currentRoots names value
            val names = if safeValue then name :: names else names
            val currentRoots = withoutNames ([name], currentRoots)
          in
            (names, currentRoots)
          end
        val (extended, extendedRoots) =
          List.foldl extend (safeNames, roots) bindings
      in
        safeTailArgument extendedRoots extended body
      end
  | Ir.Match (scrutinee, branches, _) =>
      safe scrutinee andalso
      List.all
        (fn (pattern, guard, body, _) =>
          (case guard of
             NONE => true
           | SOME condition => safe condition) andalso

          safeTailArgument
            (withoutNames (patternNames pattern, roots))
            (if safe scrutinee then patternNames pattern @ safeNames
             else safeNames)
            body)
        branches
  | Ir.Call (_, arguments, ty, escapes) =>
      if scalarType ty then true
      else
        !escapes andalso safeTailType ty andalso
        List.all safeOrRoot arguments
  | Ir.Ctor (_, _, argument, _, siteRef) =>
      !siteRef = Ir.Destination andalso
      (case argument of
         NONE => true
       | SOME value => safe value)
  | Ir.Tuple (values, _) => List.all safe values
  | Ir.Record (fields, _) =>
      List.all (fn (_, value) => safe value) fields
  | Ir.Field (base, _, _) => safe base
  | Ir.TupleIndex (base, _, _) => safe base
  | Ir.Copy (value, _) => safe value
  | Ir.StringBuiltin (name, arguments, ty, _) =>
      if LowerBuiltins.stringProduces name then safeTailType ty
      else
        safeTailType ty andalso
        List.all (fn (value, _) => safeOrRoot value) arguments
  | Ir.ClosureRef _ => true
  | _ => false
  end

fun safeTailBinding roots safeNames (_, _, value) =
  case value of
    Ir.ArrayBuiltin (name, _, _, _) =>
      name = "array_new" orelse
      name = "array_make" orelse
      name = "array_make_uninit" orelse
      name = "array_kernel_new"
  | Ir.StringBuiltin _ => true
  | _ => safeTailArgument roots safeNames value

fun emitLocalDrops (locals, drops) =
  List.app
    (fn {name, ty, skip, ...} =>
      case findLocal locals name of
        SOME value => dropValueSkipping (value, ty, skip)
      | NONE => ())
    drops

fun shadowsDrops (names, drops) =
  List.exists
    (fn name =>
      List.exists (fn {name = dropped, ...} => name = dropped) drops)
    names

fun expressionMentions names expression =
  let
    fun inList expressions = List.exists loop expressions
    and loop expr =
      case expr of
        Ir.IntLit _ => false
      | Ir.FloatLit _ => false
      | Ir.StringLit _ => false
      | Ir.BoolLit _ => false
      | Ir.UnitLit => false
      | Ir.Closure _ => false
      | Ir.ClosureRef _ => false
      | Ir.Var (name, _, _) =>
          List.exists (fn candidate => candidate = name) names
      | Ir.Bin (_, left, right, _, _) => loop left orelse loop right
      | Ir.Neg (value, _) => loop value
      | Ir.Call (_, arguments, _, _) => inList arguments
      | Ir.If (condition, thenBranch, elseBranch, _) =>
          loop condition orelse loop thenBranch orelse loop elseBranch
      | Ir.Let (bindings, body, _) =>
          List.exists (fn (_, _, value) => loop value) bindings orelse
          loop body
      | Ir.Tuple (values, _) => inList values
      | Ir.Record (fields, _) =>
          List.exists (fn (_, value) => loop value) fields
      | Ir.Field (base, _, _) => loop base
      | Ir.TupleIndex (base, _, _) => loop base
      | Ir.Ctor (_, _, NONE, _, _) => false
      | Ir.Ctor (_, _, SOME value, _, _) => loop value
      | Ir.Match (scrutinee, branches, _) =>
          loop scrutinee orelse
          List.exists
            (fn (_, guard, body, _) =>
              (case guard of
                 SOME condition => loop condition
               | NONE => false) orelse
              loop body)
            branches
      | Ir.PrintString (value, _) => loop value
      | Ir.StringBuiltin (_, arguments, _, _) =>
          List.exists (fn (value, _) => loop value) arguments
      | Ir.BytesBuiltin (_, arguments, _, _) =>
          List.exists (fn (value, _) => loop value) arguments
      | Ir.SysBuiltin (_, arguments, _) => inList arguments
      | Ir.ArrayBuiltin (_, arguments, _, _) => inList arguments
      | Ir.MapBuiltin (_, arguments, _, _) =>
          List.exists (fn (value, _) => loop value) arguments
      | Ir.Copy (value, _) => loop value
      | Ir.Borrow (_, value, _) => loop value
      | Ir.Apply (callee, arguments, _) => loop callee orelse inList arguments
  in
    loop expression
  end

fun mayOwnMemory ty =
  case ty of
    Ir.TString => true
  | Ir.TArray _ => true
  | Ir.TMap _ => true
  | Ir.TData _ => true
  | Ir.TTuple types => List.exists mayOwnMemory types
  | Ir.TRecord (fields, _) =>
      List.exists (fn (_, fieldTy) => mayOwnMemory fieldTy) fields
  | _ => false

fun dropOwnsMemory ({ ty, skip, ... } : Ir.drop) =
  let
    fun owns (ty, skips) =
      if List.exists List.null skips then false
      else if List.null skips then mayOwnMemory ty
      else
        let
          fun under index =
            List.mapPartial
              (fn (head :: rest) => if head = index then SOME rest else NONE
                | [] => NONE)
              skips
          fun anyField (_, []) = false
            | anyField (index, fieldTy :: rest) =
                owns (fieldTy, under index) orelse anyField (index + 1, rest)
        in
          case ty of
            Ir.TRecord (fields, _) => anyField (0, List.map #2 fields)
          | Ir.TTuple types => anyField (0, types)
          | _ => mayOwnMemory ty
        end
  in
    owns (ty, skip)
  end

fun plainValue ty =
  case ty of
    Ir.Int _ => true
  | Ir.Float _ => true
  | Ir.Bool => true
  | Ir.Unit => true
  | Ir.TTuple types => List.all plainValue types
  | Ir.TRecord (fields, _) => List.all (fn (_, fieldTy) => plainValue fieldTy) fields
  | _ => false

fun knownType expression =
  case expression of
    Ir.IntLit (_, ty) => SOME ty
  | Ir.FloatLit (_, ty) => SOME ty
  | Ir.BoolLit _ => SOME Ir.Bool
  | Ir.UnitLit => SOME Ir.Unit
  | Ir.Var (_, ty, _) => SOME ty
  | Ir.Bin (_, _, _, ty, _) => SOME ty
  | Ir.Neg (_, ty) => SOME ty
  | Ir.Call (_, _, ty, _) => SOME ty
  | Ir.Field (_, _, ty) => SOME ty
  | Ir.TupleIndex (_, _, ty) => SOME ty
  | Ir.StringBuiltin (_, _, ty, _) => SOME ty
  | Ir.ArrayBuiltin (_, _, ty, _) => SOME ty
  | Ir.MapBuiltin (_, _, ty, _) => SOME ty
  | Ir.SysBuiltin (_, _, ty) => SOME ty
  | _ => NONE

fun safeAfterDrops names expression =
  not (expressionMentions names expression) orelse
  (case knownType expression of
     SOME ty => plainValue ty
   | NONE => false) orelse
  (case expression of
     Ir.StringBuiltin (name, _, _, _) => LowerBuiltins.stringProduces name
   | Ir.Record (fields, _) =>
       List.all (fn (_, field) => safeAfterDrops names field) fields
   | Ir.Tuple (items, _) => List.all (safeAfterDrops names) items
   | Ir.Ctor (_, _, SOME argument, _, _) => safeAfterDrops names argument
   | _ => false)

fun expressionConsumes name expression =
  let
    fun inList expressions = List.exists loop expressions
    and loop expr =
      case expr of
        Ir.Var (candidate, _, _) => candidate = name
      | Ir.Ctor (_, _, NONE, _, _) => false
      | Ir.Ctor (_, _, SOME value, _, _) => loop value
      | Ir.Tuple (values, _) => inList values
      | Ir.Record (fields, _) =>
          List.exists (fn (_, value) => loop value) fields
      | Ir.Bin ("^", left, right, _, _) => loop left orelse loop right
      | _ => false
  in
    loop expression
  end

fun lowerTailExprWith pendingDrops safeNames roots locals expression =
  case expression of
    Ir.Call (name, arguments, resultTy, _) =>
      let
        fun consumedByArguments drop =
          let val dropped = #name drop in
            List.exists (fn argument => expressionConsumes dropped argument)
              arguments
            andalso
            not
              (List.exists
                 (fn argument =>
                   expressionMentions [dropped] argument andalso
                   not (expressionConsumes dropped argument))
                 arguments)
          end
        val remainingDrops =
          List.filter (fn drop => not (consumedByArguments drop)) pendingDrops
        val pendingNames =
          List.map #name
            (List.filter dropOwnsMemory remainingDrops)
        val usesDroppedArgument =
          List.exists (fn argument => not (safeAfterDrops pendingNames argument))
            arguments
      in
      if !currentIsRecursive andalso
         name = currentFunctionName andalso
         not usesDroppedArgument andalso
         List.all
           (fn argument =>
             rootArgument roots argument orelse
             safeTailArgument roots safeNames argument)
           arguments then
        let
          val lowered = List.map (lowerExpr locals) arguments
          val _ = emitLocalDrops (locals, remainingDrops)
          val _ =
            if stackMode andalso !currentNeedsRegion then
              emit "call void @yack_region_destroy(ptr %region)"
            else ()
        in

          let
            val loop = !selfLoop
          in
          (case loop of
             SOME emitLoop => emitLoop lowered
           | NONE =>

               raise Fail ("internal: self tail call in `" ^ name ^
                           "` without a loop header; " ^
                           "LlvmTail.tailPositions misses this form"));
          ( { text = "", ty = Ir.Unit, const = true }
          , true
          , true
          )
          end
        end
      else (lowerExpr locals expression, false, false)
      end
  | Ir.Let (bindings, body, drops) =>
      let
        val shadowed =
          List.exists
            (fn (name, _, _) => shadowsDrops ([name], pendingDrops))
            bindings
      in
        if shadowed then
          (lowerExpr locals expression, false, false)
        else
          let
            fun bindOne ((name, _, valueExpr), environment) =
              bindLocal environment name (lowerExpr environment valueExpr)
            val extended = List.foldl bindOne locals bindings

            val extendedRoots = withoutNames (List.map #1 bindings, roots)
            fun addSafe (binding as (name, _, _), names) =
              if safeTailBinding extendedRoots safeNames binding then
                name :: names
              else names
            val extendedSafeNames = List.foldl addSafe safeNames bindings
            val (value, returned, handled) =
              lowerTailExprWith
                (drops @ pendingDrops) extendedSafeNames extendedRoots
                extended body
            val _ =
              if returned orelse handled then ()
              else emitLocalDrops (extended, drops @ pendingDrops)
          in
            (value, returned, true)
          end
      end
  | Ir.If (condition, thenBranch, elseBranch, resultTy) =>
      let
        val conditionValue =
          coerce (lowerExpr locals condition, Ir.Bool)
        val thenLabel = freshLabel "if.then"
        val elseLabel = freshLabel "if.else"
        val joinLabel = freshLabel "if.join"
        val slot =
          if isUnit resultTy then NONE else SOME (allocStack resultTy)
        val _ =
          emit ("br i1 " ^ #text conditionValue ^ ", label %" ^
                thenLabel ^ ", label %" ^ elseLabel)
        val _ = emit (thenLabel ^ ":")
        val (thenValue, thenReturned, thenHandled) =
          lowerTailExprWith pendingDrops safeNames roots locals thenBranch
        val _ =
          if thenReturned orelse thenHandled then ()
          else emitLocalDrops (locals, pendingDrops)
        val _ =
          if thenReturned then ()
          else
            (case slot of
               NONE => ()
             | SOME destination =>
                 emit ("store " ^ storageTy resultTy ^ " " ^
                       #text (coerce (thenValue, resultTy)) ^
                       ", ptr " ^ destination);
             emit ("br label %" ^ joinLabel))
        val _ = emit (elseLabel ^ ":")
        val (elseValue, elseReturned, elseHandled) =
          lowerTailExprWith pendingDrops safeNames roots locals elseBranch
        val _ =
          if elseReturned orelse elseHandled then ()
          else emitLocalDrops (locals, pendingDrops)
        val _ =
          if elseReturned then ()
          else
            (case slot of
               NONE => ()
             | SOME destination =>
                 emit ("store " ^ storageTy resultTy ^ " " ^
                       #text (coerce (elseValue, resultTy)) ^
                       ", ptr " ^ destination);
             emit ("br label %" ^ joinLabel))
        val _ = emit (joinLabel ^ ":")
      in
        if thenReturned andalso elseReturned then
          ( emit "unreachable"
          ; ({ text = "", ty = Ir.Unit, const = true }, true, true)
          )
        else
          case slot of
            NONE => ({ text = "", ty = Ir.Unit, const = true }, false, true)
          | SOME source =>
              let val reg = fresh "if" in
                emit (reg ^ " = load " ^ llvmTy resultTy ^
                      ", ptr " ^ source);
                ({ text = reg, ty = resultTy, const = false }, false, true)
              end
      end
  | Ir.Match (scrutinee, branches, resultTy) =>
      let
        val branchPatternNames =
          List.concat
            (List.map
               (fn (pattern, _, _, _) => patternNames pattern)
               branches)
      in
        if shadowsDrops (branchPatternNames, pendingDrops) then
          (lowerExpr locals expression, false, false)
        else
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
            val scrutineeSafe = safeTailArgument roots safeNames scrutinee
            val scrutineeRoot =
              scrutineeBorrowed andalso rootScrutinee roots scrutinee
            val joinLabel = freshLabel "match.tail.end"
            val slot =
              if isUnit resultTy then NONE else SOME (allocStack resultTy)
            fun emitCleanup cleanup =
              List.app
                (fn CleanupDrop value => dropValue (value, #ty value)
                  | CleanupFree value => freeIfHeap value)
                cleanup
            fun lowerBranches remaining =
              case remaining of
                [] => (emit "unreachable"; true)
              | (pattern, guard, body, drops) :: rest =>
                  let
                    val bodyLabel = freshLabel "match.tail.body"
                    val failure = freshLabel "match.tail.fail"
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
                            val guardPass = freshLabel "match.tail.guardpass"
                            val guardFail = freshLabel "match.tail.guardfail"
                            val conditionValue =
                              coerce (lowerExpr locals' condition, Ir.Bool)
                            val _ =
                              emit
                                ("br i1 " ^ #text conditionValue ^
                                 ", label %" ^ guardPass ^ ", label %" ^
                                 guardFail)
                            val _ = emit (guardFail ^ ":")
                            val _ = emitCleanup guardCleanup
                            val _ = emit ("br label %" ^ failure)
                            val _ = emit (guardPass ^ ":")
                            val _ = emitCleanup cleanup
                          in
                            ()
                          end
                    val branchNames =
                      if scrutineeSafe then patternNames pattern @ safeNames
                      else safeNames
                    val unshadowed =
                      withoutNames (patternNames pattern, roots)
                    val branchRoots =
                      if scrutineeRoot then
                        boxedPatternNames pattern @ unshadowed
                      else unshadowed
                    val pending = drops @ pendingDrops
                    val (bodyValue, returned, handled) =
                      lowerTailExprWith
                        pending branchNames branchRoots locals' body
                    val _ =
                      if returned orelse handled then ()
                      else emitLocalDrops (locals', pending)
                    val _ =
                      if returned then ()
                      else
                        (case slot of
                           NONE => ()
                         | SOME destination =>
                             emit ("store " ^ storageTy resultTy ^ " " ^
                                   #text (coerce (bodyValue, resultTy)) ^
                                   ", ptr " ^ destination);
                         emit ("br label %" ^ joinLabel))
                    val restReturned =
                      case rest of
                        [] =>
                          ( emit (failure ^ ":")
                          ; emit "unreachable"
                          ; true
                          )
                      | _ =>
                          ( emit (failure ^ ":")
                          ; lowerBranches rest
                          )
                  in
                    returned andalso restReturned
                  end
            val allReturned = lowerBranches branches
            val _ =
              if allReturned then ()
              else emit (joinLabel ^ ":")
          in
            if allReturned then
              ({ text = "", ty = Ir.Unit, const = true }, true, true)
            else
              case slot of
                NONE =>
                  ({ text = "", ty = Ir.Unit, const = true }, false, true)
              | SOME source =>
                  let val reg = fresh "match.tail" in
                    emit (reg ^ " = load " ^ llvmTy resultTy ^
                          ", ptr " ^ source);
                    ({ text = reg, ty = resultTy, const = false }, false, true)
                  end
          end
      end
  | _ => (lowerExpr locals expression, false, false)

fun lowerTailExpr functionDrops safeNames locals expression =
  let
    val roots =
      List.filter
        (fn name =>
          case findLocal locals name of
            SOME { ty = Ir.TBorrow (_, false), ... } => true
          | _ => false)
        safeNames
  in
    lowerTailExprWith functionDrops safeNames roots locals expression
  end

  in lowerTailExpr end
end
