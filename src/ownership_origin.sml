structure OwnershipOrigin =
struct
  open OwnershipTypes

  type env =
    { placeOf : Ast.expr -> place option
    , useBindingAt : Diag.span -> int option
    , bindingIdBySpan : Diag.span -> int option
    , functionByName : string -> (string * Check.funSig) option
    , expressionCarriesBorrow : Ast.expr -> bool
    }

  fun make (env : env) =
    let
      val placeOf = #placeOf env
      val useBindingAt = #useBindingAt env
      val bindingIdBySpan = #bindingIdBySpan env
      val functionByName = #functionByName env
      val expressionCarriesBorrow = #expressionCarriesBorrow env

      fun borrowArgumentIndex name =
  if name = "array_get_borrow" orelse name = "array_kernel_borrow" orelse
     name = "map_get_borrow" then
    SOME 1
  else
    case functionByName name of
      SOME (_, {result, ...}) =>
        (case Types.prune result of
           Types.TBorrow (_, _, SOME index) => SOME index
         | _ => NONE)
    | NONE => NONE

      fun originIdsOf (environment, expression) =
  case expression of
    Ast.EVar (_, span) =>
      (case useBindingAt span of
         SOME id =>
           (case List.find
                   (fn (candidate, _) => candidate = id)
                   environment of
              SOME (_, origin) => [origin]
            | NONE => [])
       | NONE => [])
  | Ast.EField (_, base, _) => originIdsOf (environment, base)
  | Ast.ETupleIndex (_, base, _) => originIdsOf (environment, base)
  | Ast.ETuple (values, _) =>
      List.concat (List.map (fn value => originIdsOf (environment, value)) values)
  | Ast.ERecord (fields, _) =>
      List.concat
        (List.map (fn (_, value) => originIdsOf (environment, value)) fields)
  | Ast.ECall (name, arguments, _) =>
      (case borrowArgumentIndex name of
         SOME index =>
           if index >= 1 andalso index <= List.length arguments then
             let val argument = List.nth (arguments, index - 1) in

               case placeOf argument of
                 SOME place => [#root place]
               | NONE => originIdsOf (environment, argument)
             end
           else []
       | NONE =>

           if expressionCarriesBorrow expression then
             List.concat
               (List.map
                  (fn argument =>
                    if expressionCarriesBorrow argument then
                      originIdsOf (environment, argument)
                    else [])
                  arguments)
           else [])
  | Ast.EBorrow (_, _, operand, _) =>
      (case placeOf operand of
         SOME place => [#root place]
       | NONE => [])
  | Ast.ELet (bindings, body, _) =>
      let
        fun addBinding (binding, environment) =
          case binding of
            Ast.BVal {span, value, ...} =>
              (case bindingIdBySpan span of
                 SOME id =>
                   List.foldl
                     (fn (origin, environment) =>
                       (id, origin) :: environment)
                     environment
                     (originIdsOf (environment, value))
               | NONE => environment)
          | Ast.BPat _ => environment
          | Ast.BFun _ => raise Fail "ownership: desugared local function"
          | Ast.BGuard _ => raise Fail "ownership: desugared guard binding"
        val extended = List.foldl addBinding environment bindings
      in
        originIdsOf (extended, body)
      end
  | Ast.EIf (_, thenBranch, elseBranch, _) =>
      originIdsOf (environment, thenBranch) @
      originIdsOf (environment, elseBranch)
  | Ast.ECase (_, branches, _) =>
      List.concat
        (List.map
           (fn (_, _, body) => originIdsOf (environment, body))
           branches)
  | _ => []

      fun isArenaKeep name =
        name = "arena_keep" orelse
        String.isPrefix "yack_arena_keep" name orelse
        String.isSuffix "arena_keep" name

      fun isScratchEnter name =
        name = "sys.scratch_enter_all" orelse
        name = "sys.scratch_enter" orelse
        String.isPrefix "yack_sys_scratch_enter" name orelse
        String.isSuffix "scratch_enter_all" name

      fun isScratchLeave name =
        name = "sys.scratch_leave" orelse
        String.isPrefix "yack_sys_scratch_leave" name orelse
        String.isSuffix "scratch_leave" name

      fun escapingBorrowOwner expression =
        let
          fun addArena flag value =
            case value of
              NONE => NONE
            | SOME (id, _) => SOME (id, flag)
          fun loopExpressions expressions =
            case expressions of
              [] => NONE
            | expression :: rest =>
                (case escapingBorrowOwner expression of
                   SOME found => SOME found
                 | NONE => loopExpressions rest)
        in
          case expression of
            Ast.ELet (bindings, body, _) =>
              let
                val localIds =
                  List.mapPartial
                    (fn Ast.BVal {span, ...} => bindingIdBySpan span
                      | Ast.BPat _ => NONE
                      | Ast.BFun _ =>
                          raise Fail "ownership: desugared local function"
                      | Ast.BGuard _ =>
                          raise Fail "ownership: desugared guard binding")
                    bindings
                val origins = originIdsOf ([], expression)
                val direct =
                  List.find
                    (fn origin =>
                      List.exists (fn id => id = origin) localIds)
                    origins
                val closesArena =
                  (case body of
                     Ast.ECall (name, _, _) => isArenaKeep name
                   | _ => false) orelse
                  List.exists
                    (fn Ast.BVal {value = Ast.ECall (name, _, _), ...} =>
                       isScratchLeave name
                      | _ => false)
                    bindings
                fun nested () =
                  let
                    fun loop [] = NONE
                      | loop (binding :: rest) =
                          (case binding of
                             Ast.BVal {value, ...} =>
                               (case escapingBorrowOwner value of
                                  SOME found => SOME found
                                | NONE => loop rest)
                           | Ast.BPat _ => loop rest
                           | Ast.BFun _ =>
                               raise Fail "ownership: desugared local function"
                           | Ast.BGuard _ =>
                               raise Fail "ownership: desugared guard binding")
                  in
                    case loop bindings of
                      SOME found => SOME found
                    | NONE => escapingBorrowOwner body
                  end
              in
                case direct of
                  SOME id => SOME (id, closesArena)
                | NONE => addArena closesArena (nested ())
              end
          | Ast.EIf (_, thenBranch, elseBranch, _) =>
              let
                val closesArena =
                  (case thenBranch of
                     Ast.ECall (name, _, _) => isArenaKeep name
                   | _ => false) orelse
                  (case elseBranch of
                     Ast.ECall (name, _, _) => isArenaKeep name
                   | _ => false)
              in
                addArena closesArena
                  (case escapingBorrowOwner thenBranch of
                     SOME found => SOME found
                   | NONE => escapingBorrowOwner elseBranch)
              end
          | Ast.ECase (_, branches, _) =>
              let
                val closesArena =
                  List.exists
                    (fn (_, _, body) =>
                      case body of
                        Ast.ECall (name, _, _) => isArenaKeep name
                      | _ => false)
                    branches
              in
                addArena closesArena
                  (loopExpressions
                     (List.map (fn (_, _, body) => body) branches))
              end
          | Ast.ECall (_, arguments, _) =>
              loopExpressions arguments
          | Ast.ETuple (values, _) => loopExpressions values
          | Ast.ERecord (fields, _) =>
              loopExpressions (List.map (fn (_, value) => value) fields)
          | Ast.EField (_, base, _) => escapingBorrowOwner base
          | Ast.ETupleIndex (_, base, _) => escapingBorrowOwner base
          | Ast.EBin (_, left, right, _) =>
              (case escapingBorrowOwner left of
                 SOME found => SOME found
               | NONE => escapingBorrowOwner right)
          | Ast.ENeg (operand, _) => escapingBorrowOwner operand
          | Ast.EBorrow (_, _, operand, _) => escapingBorrowOwner operand
          | _ => NONE
        end

      fun containsArenaEnter expression =
        let
          fun any expressions = List.exists containsArenaEnter expressions
          fun bindings values =
            List.exists
              (fn Ast.BVal {value, ...} => containsArenaEnter value
                | _ => false)
              values
        in
          case expression of
            Ast.ECall (name, arguments, _) =>
              isScratchEnter name orelse any arguments
          | Ast.EBin (_, left, right, _) =>
              containsArenaEnter left orelse containsArenaEnter right
          | Ast.ENeg (operand, _) => containsArenaEnter operand
          | Ast.EIf (condition, thenBranch, elseBranch, _) =>
              containsArenaEnter condition orelse
              containsArenaEnter thenBranch orelse
              containsArenaEnter elseBranch
          | Ast.ECase (scrutinee, branches, _) =>
              containsArenaEnter scrutinee orelse
              List.exists
                (fn (_, _, body) => containsArenaEnter body)
                branches
          | Ast.ELet (values, body, _) =>
              bindings values orelse containsArenaEnter body
          | Ast.ETuple (values, _) => any values
          | Ast.ERecord (fields, _) =>
              List.exists (fn (_, value) => containsArenaEnter value) fields
          | Ast.EField (_, base, _) => containsArenaEnter base
          | Ast.ETupleIndex (_, base, _) => containsArenaEnter base
          | Ast.EBorrow (_, _, operand, _) => containsArenaEnter operand
          | _ => false
        end
    in
      (originIdsOf, escapingBorrowOwner, containsArenaEnter)
    end
end
