structure OwnershipBranches =
struct
  open OwnershipTypes

  type env =
    { bindings : Resolve.binding list
    , bindingById : int -> Resolve.binding option
    , bindingTypeById : int -> Types.ty option
    , statusOf : state -> int -> status
    , isCopy : Types.ty -> bool
    , reportBranchMismatch : int -> unit
    , addEarlyDrop : Diag.span * string * Types.ty * int list list -> unit
    , placeOf : Ast.expr -> place option
    , hasAnyLoan : place -> bool
    , report : string -> string -> Diag.span -> string list -> string list -> unit
    , placeDescription : Ast.expr -> string
    }

  fun make (env : env) =
    let
      val bindingById = #bindingById env
      val bindingTypeById = #bindingTypeById env
      val statusOf = #statusOf env
      val isCopy = #isCopy env
      val reportBranchMismatch = #reportBranchMismatch env
      val addEarlyDrop = #addEarlyDrop env
      val placeOf = #placeOf env
      val hasAnyLoan = #hasAnyLoan env
      val report = #report env
      val placeDescription = #placeDescription env

      fun candidateIds states =
        OwnershipPaths.sortUnique
          (List.concat (List.map (List.map #1) states))

      fun allSameStatus statuses =
  case statuses of
    [] => false
  | first :: rest => List.all (fn status => status = first) rest

      fun joinStatuses statuses =
        if allSameStatus statuses then
          (case statuses of
             first :: _ => first
           | [] => Available)
        else if List.exists (fn status => status = Moved orelse status = Borrowed)
                  statuses
        then Moved
        else
          Partial
            (List.foldl
               (fn (Partial paths, acc) =>
                     List.foldl (fn (p, acc) => OwnershipPaths.addMovedPath (acc, p))
                       acc paths
                 | (_, acc) => acc)
               [] statuses)

      fun childTypes ty =
        case Types.prune ty of
          Types.TCon {fields, ...} => SOME (List.map #2 fields)
        | Types.TRecord fields => SOME (List.map #2 fields)
        | Types.TTuple fields => SOME fields
        | _ => NONE

      fun skipFor (ty, targets, moved) =
        let
          val prefix = OwnershipPaths.pathIsPrefix
          fun walk (ty, path) =
            if List.exists (fn m => prefix (m, path)) moved then [path]
            else if List.exists (fn t => prefix (t, path)) targets andalso
                    not (List.exists (fn m => prefix (path, m)) moved)
            then []
            else if not (List.exists (fn t => prefix (path, t) orelse prefix (t, path))
                           targets)
            then [path]
            else
              case childTypes ty of
                SOME types =>
                  List.concat
                    (List.tabulate (List.length types, fn i =>
                       walk (List.nth (types, i), path @ [i])))
              | NONE => [path]
        in
          walk (ty, [])
        end

      fun mergeBranchStates (baseState, branchStates, patternIds) =
  let
    fun isPatternId id =
      List.exists (fn candidate => candidate = id) patternIds
    fun foldIds (remaining, merged) =
      case remaining of
        [] => merged
      | id :: rest =>
          if isPatternId id then foldIds (rest, merged)
          else
            (case bindingTypeById id of
               NONE => foldIds (rest, merged)
             | SOME ty =>
                 if isCopy ty then foldIds (rest, merged)
                 else
                   let
                     val statuses =
                       List.map
                         (fn state => statusOf state id)
                         branchStates
                     val _ =
                       if not (allSameStatus statuses) andalso
                          Types.propertyOf ty = Types.Linear
                       then reportBranchMismatch id
                       else ()
                     val mergedState =
                       case joinStatuses statuses of
                         Available => merged
                       | status => (id, status) :: merged
                   in
                     foldIds (rest, mergedState)
                   end)
  in
    foldIds (candidateIds branchStates, baseState)
  end

      fun scheduleBranchDrops (branches, patternIds) =
  let
    fun isPatternId id =
      List.exists (fn candidate => candidate = id) patternIds
    val branchStates = List.map (fn (_, state) => state) branches
    fun foldIds remaining =
      case remaining of
        [] => ()
      | id :: rest =>
          if isPatternId id then foldIds rest
          else
            (case (bindingById id, bindingTypeById id) of
               (SOME binding, SOME ty) =>
                 if isCopy ty then foldIds rest
                 else
                   let
                     val statuses =
                       List.map
                         (fn (_, state) => statusOf state id)
                         branches
                   in
                     if allSameStatus statuses then foldIds rest
                     else
                       let
                         val joined = joinStatuses statuses
                         fun release (span, state) =
                           case (joined, statusOf state id) of
                             (_, Moved) => ()
                           | (_, Borrowed) => ()
                           | (Partial union, Available) =>
                               addEarlyDrop
                                 (span, #name binding, ty, skipFor (ty, union, []))
                           | (Partial union, Partial paths) =>
                               if List.all
                                    (fn u => List.exists (fn p => OwnershipPaths.pathIsPrefix (p, u)) paths)
                                    union
                               then ()
                               else
                                 addEarlyDrop
                                   (span, #name binding, ty, skipFor (ty, union, paths))
                           | (_, Available) =>
                               addEarlyDrop (span, #name binding, ty, [])
                           | (_, Partial paths) =>
                               addEarlyDrop (span, #name binding, ty, paths)
                       in
                         List.app release branches
                       ; foldIds rest
                       end
                   end
             | _ => foldIds rest)
  in
    foldIds (candidateIds branchStates)
  end

      fun checkBorrowedMutation expression =
  case placeOf expression of
    NONE => ()
  | SOME place =>
      if hasAnyLoan place then
        report
          "E0502"
          ("cannot mutate `" ^ placeDescription expression ^
           "` while a borrow of it is active")
          (Ast.exprSpan expression)
          [ "a borrow requires the owner to stay unchanged while the borrow is active" ]
          [ "mutate before taking the borrow"
          , "or copy out the value that needs to change"
          ]
      else ()

    in
      (mergeBranchStates, scheduleBranchDrops, checkBorrowedMutation, joinStatuses)
    end
end
