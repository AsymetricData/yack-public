structure OwnershipPaths =
struct
  open OwnershipTypes

  fun pathIsPrefix (prefix, whole) =
    case prefix of
      [] => true
    | index :: rest =>
        (case whole of
           [] => false
         | other :: otherRest =>
             index = other andalso pathIsPrefix (rest, otherRest))

  fun pathUnavailable (paths, path) =
    List.exists
      (fn moved =>
        pathIsPrefix (moved, path) orelse pathIsPrefix (path, moved))
      paths

  fun sortUnique ids =
    let
      fun insert (id, []) = [id]
        | insert (id, values as (value :: rest)) =
            (case Int.compare (id, value) of
               LESS => id :: values
             | EQUAL => values
             | GREATER => value :: insert (id, rest))
    in
      List.foldl (fn (id, sorted) => insert (id, sorted)) [] ids
    end

  fun pathAlreadyMoved (paths, path) =
    List.exists
      (fn moved =>
        pathIsPrefix (moved, path) orelse pathIsPrefix (path, moved))
      paths

  fun comparePath ([], []) = EQUAL
    | comparePath ([], _) = LESS
    | comparePath (_, []) = GREATER
    | comparePath (left :: leftRest, right :: rightRest) =
        (case Int.compare (left, right) of
           EQUAL => comparePath (leftRest, rightRest)
         | order => order)

  fun addMovedPath (paths, path) =
    if List.exists (fn moved => moved = path) paths then paths
    else
      let
        fun insert ([]) = [path]
          | insert (moved :: rest) =
              (case comparePath (path, moved) of
                 LESS => path :: moved :: rest
               | EQUAL => moved :: rest
               | GREATER => moved :: insert rest)
      in
        insert paths
      end

  fun placesOverlap (left : place, right : place) =
    #root left = #root right andalso
    (pathIsPrefix (#path left, #path right) orelse
     pathIsPrefix (#path right, #path left))

  fun sameLoan (left : loan, right : loan) =
    #id left = #id right

  fun hasAnyLoan (loans : loan list ref, place : place) =
    List.exists (fn loan => placesOverlap (place, #place loan)) (!loans)

  fun hasMutableLoan (loans : loan list ref, place : place) =
    List.exists
      (fn loan => #mutable loan andalso placesOverlap (place, #place loan))
      (!loans)

  fun addLoan (loans : loan list ref, id, place : place, mutable) =
    loans := { id = id, place = place, mutable = mutable } :: !loans

  fun unionLoanLists lists =
    List.foldl
      (fn (loan, acc) =>
        if List.exists (fn candidate => sameLoan (candidate, loan)) acc
        then acc
        else loan :: acc)
      []
      (List.concat lists)
end
