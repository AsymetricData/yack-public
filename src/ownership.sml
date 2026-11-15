structure Ownership =
struct

  open OwnershipTypes

  fun check
      ({ program
       , resolution
       , functions
       , bindingTypes
       , expressionTypes
       } : input) =
    let
      val diagnostics = ref []
      val drops = ref []
      val partialDrops = ref []
      val earlyDrops = ref []
      val readonlyParams = ref []
      val loans = ref []

      val borrowedParams = ref []

      val expectedCallback = ref NONE

      val lambdaParams = ref ([] : (int * (bool * Types.ty) list) list)

      val borrowedLambdaParams = ref ([] : (Diag.span * string) list)
      val reportedCallbackConflicts = ref []
      val reportedArenaBorrows = ref []
      val bindings = #bindings resolution
      val uses = #uses resolution
      val liveness =
        OwnershipLiveness.make { bindings = bindings, uses = uses }
      fun noteUse id = OwnershipLiveness.noteUse (liveness, id)
      fun carriedLoans id = OwnershipLiveness.carriedBy (liveness, id)
      fun carryLoans (id, loanIds) =
        OwnershipLiveness.carry (liveness, id, loanIds)
      fun releaseCarrier id = OwnershipLiveness.release (liveness, id)
      fun activeLoanIds () = List.map #id (!loans)
      fun flushLoans () = loans := OwnershipLiveness.flush (liveness, !loans)
      fun pinLoans () = OwnershipLiveness.pin (liveness, activeLoanIds ())

      fun analyzeArms arms = OwnershipLiveness.branches (liveness, arms)
      fun report code message span notes hints =
        let val cell = diagnostics in
          cell := Diag.error (code, message, span, notes, hints) :: !cell
        end

      val pathIsPrefix = OwnershipPaths.pathIsPrefix
      val pathUnavailable = OwnershipPaths.pathUnavailable
      val pathAlreadyMoved = OwnershipPaths.pathAlreadyMoved
      val addMovedPath = OwnershipPaths.addMovedPath
      val sortUniqueIds = OwnershipPaths.sortUnique
      val placesOverlap = OwnershipPaths.placesOverlap
      val sameLoan = OwnershipPaths.sameLoan
      fun hasAnyLoan place = OwnershipPaths.hasAnyLoan (loans, place)
      fun hasMutableLoan place = OwnershipPaths.hasMutableLoan (loans, place)
      val nextLoanId = ref 0
      fun addLoan (place, mutable) =
        let
          val id = !nextLoanId
          val _ = nextLoanId := id + 1
        in
          OwnershipPaths.addLoan (loans, id, place, mutable)
        end
      val unionLoanLists = OwnershipPaths.unionLoanLists

      val index =
        OwnershipIndex.make
          { bindings = bindings
          , bindingTypes = bindingTypes
          , expressionTypes = expressionTypes
          , uses = uses
          , functions = functions
          }

      val bindingById = #bindingById index
      val bindingIdBySpan = #bindingIdBySpan index
      val expressionTypeAt = #expressionTypeAt index
      val bindingTypeById = #bindingTypeById index
      val useBindingAt = #useBindingAt index
      val functionByName = #functionByName index
      fun isCopy ty = Types.propertyOf ty = Types.Copy

      fun isConsume ty =
        case Types.prune ty of
          Types.TConsume _ => true
        | _ => false

      fun needsDrop ty =
        not (isCopy ty) andalso
        (case Types.prune ty of
           Types.TBorrow _ => false
         | Types.TToken => false
         | Types.TResource => false
         | _ => true)

      fun carriesBorrow ty = Types.containsBorrow ty

      fun expressionCarriesBorrow expression =
        case expressionTypeAt (Ast.exprSpan expression) of
          SOME ty => carriesBorrow ty
          | NONE => false

      fun valueBindingIds expression =
        let
          fun merge (left, right) =
            case (left, right) of
              (SOME leftIds, SOME rightIds) => SOME (leftIds @ rightIds)
            | _ => NONE
          fun ids expressions =
            List.foldr
              (fn (expression, acc) => merge (valueBindingIds expression, acc))
              (SOME [])
              expressions
          fun patternIds pattern =
            case pattern of
              Ast.PVar (_, span) =>
                (case bindingIdBySpan span of
                   SOME id => SOME [id]
                 | NONE => SOME [])
            | Ast.PWildcard _ => SOME []
            | Ast.PInt _ => SOME []
            | Ast.PBool _ => SOME []
            | Ast.PString (_, rest, _) =>
                (case rest of SOME p => patternIds p | NONE => SOME [])
            | Ast.PBytes (_, rest, _) =>
                (case rest of SOME p => patternIds p | NONE => SOME [])
            | Ast.PConstructor (_, argument, _) =>
                (case argument of SOME p => patternIds p | NONE => SOME [])
            | Ast.PTuple (patterns, _) =>
                List.foldr
                  (fn (pattern, acc) => merge (patternIds pattern, acc))
                  (SOME [])
                  patterns
            | Ast.PRecord (fields, _) =>
                List.foldr
                  (fn ((_, pattern), acc) => merge (patternIds pattern, acc))
                  (SOME [])
                  fields
        in
          case expression of
            Ast.EVar (_, span) =>
              (case useBindingAt span of
                 SOME id => SOME [id]
               | NONE => SOME [])
          | Ast.EField (_, base, _) => valueBindingIds base
          | Ast.ETupleIndex (_, base, _) => valueBindingIds base
          | Ast.ETuple (values, _) => ids values
          | Ast.ERecord (fields, _) =>
              ids (List.map (fn (_, value) => value) fields)
          | Ast.ECall (_, arguments, _) => ids arguments
          | Ast.EBin (_, left, right, _) =>
              merge (valueBindingIds left, valueBindingIds right)
          | Ast.ENeg (operand, _) => valueBindingIds operand
          | Ast.EBorrow (_, _, operand, _) => valueBindingIds operand
          | Ast.EIf (condition, thenBranch, elseBranch, _) =>
              merge
                ( merge (valueBindingIds condition, valueBindingIds thenBranch)
                , valueBindingIds elseBranch )
          | Ast.ELet (bindings, body, _) =>
              let
                val values =
                  List.foldr
                    (fn (binding, acc) =>
                      case binding of
                        Ast.BVal {value, ...} =>
                          merge (valueBindingIds value, acc)
                      | Ast.BPat {value, ...} =>
                          merge (valueBindingIds value, acc)
                      | Ast.BFun _ =>
                          raise Fail "ownership: desugared local function"
                      | Ast.BGuard _ =>
                          raise Fail "ownership: desugared guard binding")
                    (valueBindingIds body)
                    bindings
              in
                values
              end
          | Ast.ECase (scrutinee, branches, _) =>
              List.foldr
                (fn ((pattern, guard, body), acc) =>
                  let
                    val guardIds =
                      case guard of
                        NONE => SOME []
                      | SOME condition => valueBindingIds condition
                  in
                    merge
                      ( merge (patternIds pattern, guardIds)
                      , merge (valueBindingIds body, acc) )
                  end)
                (valueBindingIds scrutinee)
                branches
          | Ast.EFn _ => SOME []
          | Ast.EInt _ => SOME []
          | Ast.EFloat _ => SOME []
          | Ast.EString _ => SOME []
          | Ast.EBool _ => SOME []
          | Ast.EUnit _ => SOME []
          | _ => NONE
        end

      fun inheritedLoanIds expression =
        case valueBindingIds expression of
          SOME ids => List.concat (List.map carriedLoans ids)
        | NONE => activeLoanIds ()

      fun rootBindingId expression =
        case expression of
          Ast.EVar (_, span) => useBindingAt span
        | Ast.EField (_, base, _) => rootBindingId base
        | Ast.ETupleIndex (_, base, _) => rootBindingId base
        | _ => NONE

      fun notePlaceUse expression =
        case rootBindingId expression of
          SOME id => noteUse id
        | NONE => ()

      fun newLoanIds (marker, bindingId, value) =
        case bindingTypeById bindingId of
          SOME ty =>
            if Types.containsBorrow ty then
              let
                fun isNew (loan : loan) = #id loan >= marker
                val created = List.map #id (List.filter isNew (!loans))
              in
                created @ inheritedLoanIds value
              end
            else []
        | NONE => []

      fun hasDropLeaf ty =
        case Types.prune ty of
          Types.TString => true
        | Types.TArray _ => true

        | Types.TMap _ => true
        | Types.TData _ => not (isCopy ty)
        | Types.TDataApp _ => not (isCopy ty)
        | Types.TCon _ => not (isCopy ty)
        | Types.TRecord fields =>
            List.exists (fn (_, fieldTy) => hasDropLeaf fieldTy) fields
        | Types.TTuple fields => List.exists hasDropLeaf fields
        | Types.TConsume inner => hasDropLeaf inner
        | _ => false

      fun isContainerType ty =
        case Types.prune ty of
          Types.TArray _ => true
        | Types.TMap _ => true
        | Types.TDataApp (Types.Data {name, property, ...}, _) =>
            !property = Types.Affine andalso
            name <> "option" andalso name <> "result"
        | Types.TData (Types.Data {name, property, ...}) =>
            !property = Types.Affine andalso
            name <> "option" andalso name <> "result"
        | _ => false

      fun readAsCopy ty =
        isCopy ty orelse
        (Types.propertyOf ty = Types.Affine andalso not (hasDropLeaf ty))

      fun genericField ty =
        case Types.prune ty of
          Types.TVar _ => true
        | _ => false

      fun fieldReadsAsCopy ty = readAsCopy ty andalso not (genericField ty)

      fun addDrop span =
        if List.exists (fn candidate => candidate = span) (!drops) then ()
        else drops := span :: !drops

      fun addPartialDrop (span, indices) =
        if List.exists
             (fn (candidate, _) => candidate = span)
             (!partialDrops) then ()
        else partialDrops := (span, indices) :: !partialDrops

      fun addEarlyDrop (span, name, ty, skip) =
        let
          fun update [] =
                [(span, [{name = name, ty = ty, skip = skip}])]
            | update ((candidate, drops) :: rest) =
                if candidate = span then
                  ( candidate
                  , if List.exists (fn drop => #name drop = name) drops
                    then drops
                    else {name = name, ty = ty, skip = skip} :: drops
                  ) :: rest
                else (candidate, drops) :: update rest
        in
          earlyDrops := update (!earlyDrops)
        end

      fun addDropsForStatus (span, ty, status) =
        if needsDrop ty then
          case status of
            Available => addDrop span
          | Partial paths => addPartialDrop (span, paths)
          | Moved => ()
          | Borrowed => ()
        else ()

      fun expressionType expression =
        case expression of
          Ast.EVar (_, span) =>
            (case useBindingAt span of
               SOME id => bindingTypeById id
             | NONE => NONE)
        | Ast.EField (field, base, _) =>
            (case expressionType base of
               SOME ty =>
                 (case Types.prune ty of
                    Types.TCon {fields, ...} => lookupField field fields
                  | Types.TRecord fields => lookupField field fields
                  | Types.TData data => Types.dataCommonField field data []
                  | Types.TDataApp (data, arguments) =>
                      Types.dataCommonField field data arguments
                  | _ => NONE)
             | NONE => NONE)
        | Ast.ETupleIndex (index, base, _) =>
            (case expressionType base of
               SOME ty =>
                 (case Types.prune ty of
                    Types.TTuple fields =>
                      if index >= 0 andalso index < List.length fields then
                        SOME (List.nth (fields, index))
                      else NONE
                  | _ => NONE)
             | NONE => NONE)
        | _ => NONE

      and lookupField field fields =
        case List.find (fn (candidate, _) => candidate = field) fields of
          SOME (_, ty) => SOME ty
        | NONE => NONE

      and findFieldIndex field fields =
        let
          fun loop (index, []) = NONE
            | loop (index, (name, ty) :: rest) =
                if name = field then SOME (index, ty)
                else loop (index + 1, rest)
        in
          loop (0, fields)
        end

      fun fieldIndexOf field ty =
        case Types.prune ty of
          Types.TCon {fields, ...} =>
            (case findFieldIndex field fields of
               SOME (index, _) => SOME index
             | NONE => NONE)
        | Types.TRecord fields =>
            (case findFieldIndex field fields of
               SOME (index, _) => SOME index
             | NONE => NONE)
        | _ => NONE

      fun typeAtPlace (root, path) =
        case bindingTypeById root of
          NONE => NONE
        | SOME ty =>
            let
              fun walk ([], current) = SOME current
                | walk (index :: rest, current) =
                    (case Types.prune current of
                       Types.TCon {fields, ...} =>
                         (case nthField
                                 (index, List.map (fn (_, ty) => ty) fields) of
                            SOME fieldTy => walk (rest, fieldTy)
                          | NONE => NONE)
                     | Types.TRecord fields =>
                         (case nthField
                                 (index, List.map (fn (_, ty) => ty) fields) of
                            SOME fieldTy => walk (rest, fieldTy)
                          | NONE => NONE)
                     | Types.TTuple fields =>
                         (case nthField (index, fields) of
                            SOME fieldTy => walk (rest, fieldTy)
                          | NONE => NONE)
                     | _ => NONE)
            in
              walk (path, ty)
            end

      and nthField (index, fields) =
        if index < 0 then NONE
        else
          let
            fun loop (remaining, current) =
              case remaining of
                [] => NONE
              | field :: rest =>
                  if current = index then SOME field
                  else loop (rest, current + 1)
          in
            loop (fields, 0)
          end

      fun placeOf expression =
        case expression of
          Ast.EVar (_, span) =>
            (case useBindingAt span of
               SOME id => SOME { root = id, path = [] }
             | NONE => NONE)
        | Ast.EField (field, base, _) =>
            (case (placeOf base, expressionType base) of
               (SOME place, SOME ty) =>
                 (case fieldIndexOf field ty of
                    SOME index =>
                      SOME { root = #root place, path = #path place @ [index] }
                  | NONE => NONE)
             | _ => NONE)
        | Ast.ETupleIndex (index, base, _) =>
            (case placeOf base of
               SOME place =>
                 SOME { root = #root place, path = #path place @ [index] }
             | NONE => NONE)
        | _ => NONE

      val (bindingName, placeDescription, isConstructor, reportUseAfterMove,
           linearConsumeHint, reportPlaceUseAfterMove, reportNotConsumed,
           reportBranchMismatch) =
        OwnershipReports.make
          { report = report
          , bindingById = bindingById
          , bindingTypeById = bindingTypeById
          , bindings = bindings
          }
      fun statusOf state id =
        case List.find (fn (candidate, _) => candidate = id) state of
          SOME (_, status) => status
        | NONE => Available

      fun forgetBindings state ids =
        List.filter
          (fn (id, _) =>
            not (List.exists (fn localId => localId = id) ids))
          state

      fun releasesLoanAtCall name =
        OwnershipSummaries.releasesArgumentLoans name

      fun markMoved state id = (id, Moved) :: state

      fun markPartial state id path =
        let
          val paths =
            case statusOf state id of
              Partial existing => existing
            | _ => []
        in
          (id, Partial (addMovedPath (paths, path))) :: state
        end

      fun placeAvailable state place =
        case statusOf state (#root place) of
          Available => true
        | Moved => false
        | Borrowed => true
        | Partial paths => not (pathUnavailable (paths, #path place))

      fun readPlace (state, expression, place) =
        let val _ = notePlaceUse expression in
          if placeAvailable state place then state
          else
            (reportUseAfterMove (#root place) (Ast.exprSpan expression); state)
        end

      val (isBorrowedParam, reportCallbackMove, callbackConflict,
           reportCallbackConflict) =
        OwnershipCallbacks.make
          { borrowedParams = borrowedParams
          , expectedCallback = expectedCallback
          , reportedCallbackConflicts = reportedCallbackConflicts
          , bindingTypeById = bindingTypeById
          , report = report
          , bindingName = bindingName
          , placeDescription = placeDescription
          , expressionTypeAt = expressionTypeAt
          , isConsume = isConsume
          , hasDropLeaf = hasDropLeaf
          }

          fun consumeCallbackConflict (paramType, argument) =
          case argument of
          Ast.EVar (_, span) =>
           (case useBindingAt span of
              SOME id =>
                (case List.find
                        (fn (candidate, _) => candidate = id)
                        (!lambdaParams) of
                   SOME (_, summary) =>
                     let
                       val expected =
                         case Types.prune paramType of
                           Types.TConsume inner => Types.prune inner
                         | other => other
                     in
                       case expected of
                         Types.TFun (expectedParams, _) =>
                           let
                             fun loop (expected, remaining) =
                               case (expected, remaining) of
                                 (e :: es, (owned, ty) :: rest) =>
                                   if isConsume e andalso not owned andalso
                                      hasDropLeaf ty
                                   then SOME ty
                                   else loop (es, rest)
                               | _ => NONE
                           in
                             loop (expectedParams, summary)
                           end
                       | _ => NONE
                     end
                 | NONE => NONE)
            | NONE => NONE)
          | _ => NONE
      fun moveBinding (state, id, span) =
        case bindingTypeById id of
          NONE => state
        | SOME ty =>
            if isCopy ty then state
            else if isBorrowedParam id then
              if not (readAsCopy ty) then
                ( reportCallbackMove
                    ("callback parameter `" ^ bindingName id ^ "`", id, span)
                ; state
                )
              else

                state
            else
              (case statusOf state id of
                 Available =>
                   if hasAnyLoan { root = id, path = [] } then
                     ( report
                         "E0501"
                         ("cannot move `" ^ bindingName id ^
                          "` while it is borrowed")
                         span
                         [ "a borrow of `" ^ bindingName id ^
                           "` is still needed after this point"
                         , "moving the owner would leave the borrow dangling"
                         ]
                         [ "end the borrow first, then move `" ^ bindingName id ^ "`"
                         , "or move `" ^ bindingName id ^ "` before taking the borrow"
                         ]
                     ; state
                     )
                   else markMoved state id
               | Moved => (reportUseAfterMove id span; state)
               | Partial _ =>
                   ( report
                       "E0402"
                       ("value `" ^ bindingName id ^
                        "` is used after part of it was moved")
                       span
                       [ "one or more fields of `" ^ bindingName id ^
                         "` were moved out earlier"
                       , "the remaining fields are still owned, but the whole value is not"
                       ]
                       [ "move the whole value before extracting fields from it"
                       , "or keep using only the fields that are still available"
                       ]
                   ; state
                   )
               | Borrowed => state)

      fun reportBorrowedMove expression place span =
        report
          "E0501"
          ("cannot move `" ^ placeDescription expression ^
           "` while a borrow of it is active")
          span
          [ "a borrow of this place is still needed after this point"
          , "moving this place would leave the borrow dangling"
          ]
          [ "end the borrow first, then move this value"
          , "or move the value before taking the borrow"
          ]

      fun reportReadUnderMutableBorrow expression place span =
        report
          "E0502"
          ("cannot read `" ^ placeDescription expression ^
           "` while a mutable borrow is active")
          span
          [ "a mutable borrow of this place is still used after this point"
          , "reading it now would observe a value the borrow may change"
          ]
          [ "end the mutable borrow first"
          , "or read the value before taking the borrow"
          ]

      fun rootIsCopy id =
        case typeAtPlace (id, []) of
          SOME ty => isCopy ty
        | NONE => false

      fun datatypeFieldBase expression =
        case expression of
          Ast.EField (field, base, _) =>
            let
              val baseType =
                case expressionType base of
                  SOME ty => SOME ty
                | NONE => expressionTypeAt (Ast.exprSpan base)
            in
              case baseType of
                SOME ty =>
                  (case Types.prune ty of
                     Types.TData data =>
                       (case Types.dataCommonField field data [] of
                          SOME _ => SOME base
                        | NONE => NONE)
                   | Types.TDataApp (data, arguments) =>
                       (case Types.dataCommonField field data arguments of
                          SOME _ => SOME base
                        | NONE => NONE)
                   | _ => NONE)
              | NONE => NONE
            end
        | _ => NONE

      fun accessField (state, expression) =
        case placeOf expression of
          NONE => state
        | SOME place =>
            let
              val id = #root place
              val path = #path place
              val span = Ast.exprSpan expression
              val _ = notePlaceUse expression
            in
              case typeAtPlace (id, path) of
                NONE => state
              | SOME fieldTy =>
                  (case statusOf state id of
                     Moved =>
                       (reportPlaceUseAfterMove (expression, id, span); state)
                   | Borrowed =>
                       if not (readAsCopy fieldTy) then
                         ( reportCallbackMove
                             ( "field `" ^ placeDescription expression ^
                               "` of callback parameter `" ^ bindingName id ^ "`"
                             , id, span )
                         ; state
                         )
                       else state
                   | Available =>
                       if fieldReadsAsCopy fieldTy then

                         if hasMutableLoan place andalso
                            not (rootIsCopy id) then
                           (reportReadUnderMutableBorrow expression place span
                           ; state)
                         else state
                       else if hasAnyLoan place then
                         (reportBorrowedMove expression place span; state)
                       else markPartial state id path
                   | Partial paths =>
                       if pathAlreadyMoved (paths, path) then
                         (reportPlaceUseAfterMove (expression, id, span); state)
                       else if fieldReadsAsCopy fieldTy then
                         if hasMutableLoan place andalso
                            not (rootIsCopy id) then
                           (reportReadUnderMutableBorrow expression place span
                           ; state)
                         else state
                       else if hasAnyLoan place then
                         (reportBorrowedMove expression place span; state)
                       else markPartial state id path)
            end

      fun patternVariables pattern =
        case pattern of
          Ast.PVar (name, span) => [(name, span)]
        | Ast.PWildcard _ => []
        | Ast.PInt _ => []
        | Ast.PBool _ => []
        | Ast.PString (_, rest, _) =>
            (case rest of
               SOME pattern => patternVariables pattern
             | NONE => [])
        | Ast.PBytes (_, rest, _) =>
            (case rest of
               SOME pattern => patternVariables pattern
             | NONE => [])
        | Ast.PConstructor (_, argument, _) =>
            (case argument of
               SOME pattern => patternVariables pattern
             | NONE => [])
        | Ast.PTuple (patterns, _) =>
            List.concat (List.map patternVariables patterns)
        | Ast.PRecord (fields, _) =>
            List.concat
              (List.map (fn (_, pattern) => patternVariables pattern) fields)

      fun reportLinearDiscard (span, ty) =
        report
          "E0403"
          ("linear value of type `" ^ Types.toString ty ^
           "` is discarded by this pattern")
          span
          [ "values with the Linear property must be consumed exactly once on every path"
          , "`_` or an omitted field would drop it silently"
          ]
          [ "bind it to a name and consume it in the branch" ]

      fun checkLinearDiscards (pattern, ty) =
        let
          val ty = Types.prune ty
          val ty = case ty of Types.TConsume inner => Types.prune inner | _ => ty
          fun linear ty = Types.propertyOf ty = Types.Linear
          fun recordFields () =
            case ty of
              Types.TRecord fields => SOME fields
            | Types.TCon {fields, ...} => SOME fields
            | _ => NONE
        in
          case (pattern, ty) of
            (_, Types.TBorrow _) => ()
          | (Ast.PWildcard span, _) =>
              if linear ty then reportLinearDiscard (span, ty) else ()
          | (Ast.PTuple (patterns, _), Types.TTuple types) =>
              if List.length patterns = List.length types then
                ListPair.app checkLinearDiscards (patterns, types)
              else ()
          | (Ast.PRecord (patterns, span), _) =>
              (case recordFields () of
                 NONE => ()
               | SOME fields =>
                   List.app
                     (fn (field, fieldTy) =>
                       case List.find (fn (name, _) => name = field) patterns of
                         SOME (_, subpattern) =>
                           checkLinearDiscards (subpattern, fieldTy)
                       | NONE =>
                           if linear fieldTy then
                             reportLinearDiscard (span, fieldTy)
                           else ())
                     fields)
          | (Ast.PConstructor (name, SOME subpattern, _), _) =>
              let
                val (data, arguments) =
                  case ty of
                    Types.TData data => (SOME data, [])
                  | Types.TDataApp (data, arguments) => (SOME data, arguments)
                  | _ => (NONE, [])
              in
                case data of
                  SOME (data as Types.Data {constructors, ...}) =>
                    (case List.find (fn (constructor, _) => constructor = name)
                            (!constructors) of
                       SOME (_, SOME payload) =>
                         checkLinearDiscards
                           ( subpattern
                           , Types.instantiateData data arguments payload )
                     | _ => ())
                | NONE => ()
              end
          | _ => ()
        end

      and checkTopWildcard (span, earlier, ty) =
        let
          val ty = Types.prune ty
          val ty = case ty of Types.TConsume inner => Types.prune inner | _ => ty
          fun linear ty = Types.propertyOf ty = Types.Linear
          fun irrefutable pattern =
            case pattern of
              Ast.PWildcard _ => true
            | Ast.PVar _ => true
            | Ast.PTuple (patterns, _) => List.all irrefutable patterns
            | Ast.PRecord (fields, _) => List.all (fn (_, p) => irrefutable p) fields
            | _ => false
          fun caught name =
            List.exists
              (fn (pattern, guard, _) =>
                not (isSome guard) andalso
                (case pattern of
                   Ast.PConstructor (n, NONE, _) => n = name
                 | Ast.PConstructor (n, SOME sub, _) => n = name andalso irrefutable sub
                 | _ => false))
              earlier
          val (data, arguments) =
            case ty of
              Types.TData data => (SOME data, [])
            | Types.TDataApp (data, arguments) => (SOME data, arguments)
            | _ => (NONE, [])
        in
          case (ty, data) of
            (Types.TBorrow _, _) => ()
          | (_, SOME (data as Types.Data {property, constructors, ...})) =>
              if !property = Types.Linear then reportLinearDiscard (span, ty)
              else if
                List.exists
                  (fn (name, SOME payload) =>
                      not (caught name) andalso
                      linear (Types.instantiateData data arguments payload)
                    | (_, NONE) => false)
                  (!constructors)
              then reportLinearDiscard (span, ty)
              else ()
          | _ => checkLinearDiscards (Ast.PWildcard span, ty)
        end

      fun checkPatternScope (state, pattern) =
        List.app
          (fn (_, span) =>
            case bindingIdBySpan span of
              NONE => ()
            | SOME id =>
                (case bindingTypeById id of
                   SOME ty =>
                     if Types.propertyOf ty = Types.Linear andalso
                        statusOf state id = Available
                     then reportNotConsumed id
                     else ()
                 | NONE => ()))
          (patternVariables pattern)

      val (originIdsOf, escapingBorrowOwner, containsArenaEnter) =
        OwnershipOrigin.make
          { placeOf = placeOf
          , useBindingAt = useBindingAt
          , bindingIdBySpan = bindingIdBySpan
          , functionByName = functionByName
          , expressionCarriesBorrow = expressionCarriesBorrow
          }
      val (mergeBranchStates, scheduleBranchDrops, checkBorrowedMutation, joinStatuses) =
        OwnershipBranches.make
          { bindings = bindings
          , bindingById = bindingById
          , bindingTypeById = bindingTypeById
          , statusOf = statusOf
          , isCopy = isCopy
          , reportBranchMismatch = reportBranchMismatch
          , addEarlyDrop = addEarlyDrop
          , placeOf = placeOf
          , hasAnyLoan = hasAnyLoan
          , report = report
          , placeDescription = placeDescription
          }
      fun lambdaCaptures (params : Ast.param list, body) =
        let
          fun patternIds pattern =
            List.mapPartial
              (fn (_, span) => bindingIdBySpan span)
              (patternVariables pattern)
          fun adds id ids = if List.exists (fn x => x = id) ids then ids else id :: ids
          fun addBound span ids =
            case bindingIdBySpan span of
              SOME id => adds id ids
            | NONE => ids
          val initialBound =
            List.foldl (fn (param, ids) => addBound (#span param) ids) [] params
          val captures = ref []
          fun addCapture (bound, id, span) =
            if List.exists (fn candidate => candidate = id) bound then ()
            else if List.exists (fn (candidate, _) => candidate = id) (!captures)
            then ()
            else captures := (id, span) :: !captures
          fun walk bound expression =
            case expression of
              Ast.EInt _ => ()
            | Ast.EFloat _ => ()
            | Ast.EString _ => ()
            | Ast.EBool _ => ()
            | Ast.EUnit _ => ()
            | Ast.EVar (_, span) =>
                (case useBindingAt span of
                   SOME id => addCapture (bound, id, span)
                 | NONE => ())
            | Ast.ECall (_, arguments, span) =>
                ( (case useBindingAt span of
                     SOME id => addCapture (bound, id, span)
                   | NONE => ())
                ; List.app (walk bound) arguments
                )
            | Ast.EBin (_, left, right, _) =>
                ( walk bound left ; walk bound right )
            | Ast.ENeg (operand, _) => walk bound operand
            | Ast.EIf (condition, thenBranch, elseBranch, _) =>
                ( walk bound condition
                ; walk bound thenBranch
                ; walk bound elseBranch
                )
            | Ast.ELet (bindings, body, _) =>
                let
                  fun walkBinding (binding, bound) =
                    case binding of
                      Ast.BVal {name, value, span, ...} =>
                        let
                          val _ = walk bound value
                        in
                          case bindingIdBySpan span of
                            SOME id => adds id bound
                          | NONE => bound
                        end
                    | Ast.BPat {pattern, value, span, ...} =>
                        let
                          val _ = walk bound value
                          val _ = span
                          val ids = patternIds pattern
                        in
                          List.foldl (fn (id, bound) => adds id bound) bound ids
                        end
                    | Ast.BFun _ =>
                        raise Fail "ownership: desugared local function"
                    | Ast.BGuard _ =>
                        raise Fail "ownership: desugared guard binding"
                  val extended = List.foldl walkBinding bound bindings
                in
                  walk extended body
                end
            | Ast.EArena _ =>
                raise Fail "ownership: desugared arena expression"
            | Ast.ERecord (fields, _) =>
                List.app (fn (_, value) => walk bound value) fields
            | Ast.EField (_, base, _) => walk bound base
            | Ast.ETuple (values, _) => List.app (walk bound) values
            | Ast.ETupleIndex (_, base, _) => walk bound base
            | Ast.ECase (scrutinee, branches, _) =>
                let
                  val _ = walk bound scrutinee
                  val _ =
                    List.app
                      (fn (pattern, guard, body) =>
                        let
                          val ids = patternIds pattern
                          val branchBound =
                            List.foldl (fn (id, bound) => adds id bound)
                              bound ids
                          val _ =
                            case guard of
                              NONE => ()
                            | SOME condition => walk branchBound condition
                        in
                          walk branchBound body
                        end)
                      branches
                in
                  ()
                end
            | Ast.EBorrow (_, _, operand, _) => walk bound operand
            | Ast.EFn (innerParams, _, innerBody, _) =>
                let
                  val innerBound =
                    List.foldl
                      (fn (param, ids) => addBound (#span param) ids)
                      bound
                      innerParams
                in
                  walk innerBound innerBody
                end
            | Ast.EExtern _ => ()
        in
          walk initialBound body
          ; List.rev (!captures)
        end

      fun borrowSourceOf (result, params) =
        case Types.prune result of
          Types.TBorrow (_, _, SOME index) => SOME index
        | _ =>
            let
              fun numbered (index, remaining) =
                case remaining of
                  [] => []
                | ty :: rest =>
                    (case Types.prune ty of
                       Types.TBorrow _ => index :: numbered (index + 1, rest)
                     | _ => numbered (index + 1, rest))
              val indexes = numbered (1, params)
            in
              case indexes of
                [index] => SOME index
              | _ => NONE
            end

      fun callBorrowSource (name, span) =
        case useBindingAt span of
          SOME id =>
            (case bindingTypeById id of
               SOME ty =>
                 (case Types.prune ty of
                    Types.TFun (params, result) =>
                      borrowSourceOf (result, params)
                  | _ => NONE)
             | NONE => NONE)
        | NONE =>
            (case functionByName name of
               SOME (_, {params, result, ...}) =>
                 borrowSourceOf (result, params)
             | NONE => OwnershipSummaries.resultBorrowSource name)

      fun callKeepsLoan (marker, arguments, source, loan) =
        if #id loan < marker then true
        else
          case source of
            NONE => true
          | SOME index =>
              if index < 1 orelse index > List.length arguments then true
              else
                case valueBindingIds (List.nth (arguments, index - 1)) of
                  NONE => true
                | SOME roots =>
                    List.exists
                      (fn root => root = #root (#place loan))
                      roots

      fun analyzeExpression (state, expression) =
        case expression of
          Ast.EInt _ => state
        | Ast.EFloat _ => state
        | Ast.EString _ => state
        | Ast.EBool _ => state
        | Ast.EUnit _ => state

        | Ast.EExtern _ => state

        | Ast.EArena _ =>
            raise Fail "ownership: desugared arena expression"

        | Ast.EVar (_, span) =>
            (case useBindingAt span of
               SOME id =>
                 let val _ = noteUse id in
                   (case bindingTypeById id of
                      SOME ty =>
                        if isCopy ty then state
                        else if statusOf state id = Moved then
                          (reportUseAfterMove id span; state)
                        else state
                    | NONE => state)
                 end
             | NONE => state)

        | Ast.ERecord (fields, _) =>
            List.foldl
              (fn ((_, value), state') => moveExpression (state', value))
              state
              fields

        | Ast.EField _ =>
            accessFieldOrMove (state, expression)

        | Ast.ETuple (values, _) =>
            List.foldl
              (fn (value, state') => moveExpression (state', value))
              state
              values

        | Ast.ETupleIndex _ =>
            accessField (state, expression)

        | Ast.ECase (scrutinee, branches, span) =>
            analyzeCase (state, scrutinee, branches, false, span)

        | Ast.EBorrow (mutable, ampersand, operand, span) =>
            (case placeOf operand of
               NONE => state
             | SOME place =>
                 if not (placeAvailable state place) then
                   (reportUseAfterMove (#root place) (Ast.exprSpan operand); state)
                 else
                   let
                     val conflict =
                       if mutable then hasAnyLoan place
                       else hasMutableLoan place
                   in
                     if conflict then
                       ( report
                           "E0502"
                           (if mutable then
                              "cannot borrow mutably while another borrow is active"
                            else
                              "cannot borrow while a mutable borrow is active")
                           span
                           [ "shared borrows are read-only and may overlap; a mutable borrow is exclusive" ]
                           [ "end the existing borrow first"
                           , if mutable then
                               "use `borrow` instead of `borrow mut` if you only need to read"
                             else
                               "wait until the mutable borrow is released"
                           ]
                       ; state
                       )
                     else
                       (addLoan (place, mutable); state)
                   end)

        | Ast.EFn (params, _, body, span) =>
            let
              val captures = lambdaCaptures (params, body)
              val _ =
                List.app
                  (fn (id, _) =>
                    report
                      "E0511"
                      ("closure cannot capture `" ^ bindingName id ^
                       "`: closures only receive their arguments")
                      span
                      [ "function values carry no environment in Yack" ]
                      [ "pass the value as an explicit argument of the closure"
                      , "or declare a module-level function with `fun`"
                      ])
                  captures
              val savedLoans = !loans
              val savedBorrowed = !borrowedParams
              val _ = loans := []
              val expectedParams =
                case !expectedCallback of
                  SOME ty =>
                    (case Types.prune ty of
                       Types.TFun (expected, _) => expected
                     | Types.TConsume inner =>
                         (case Types.prune inner of
                            Types.TFun (expected, _) => expected
                          | _ => [])
                     | _ => [])
                | NONE => []
              fun expectedAt index =
                let
                  fun loop (remaining, current) =
                    case remaining of
                      [] => NONE
                    | ty :: rest =>
                        if current = index then SOME ty
                        else loop (rest, current + 1)
                in
                  loop (expectedParams, 0)
                end
              val paramInfo =
                List.map
                  (fn (param : Ast.param) =>
                    let
                      val id = bindingIdBySpan (#span param)
                      val ty =
                        case id of
                          SOME bindingId => bindingTypeById bindingId
                        | NONE => NONE
                    in
                      (#span param, id, ty)
                    end)
                  params
              fun paramOwned (index, actual) =
                case expectedAt index of
                  SOME expected => isConsume expected
                | NONE => isConsume actual

              val borrowed =
                List.mapPartial
                  (fn (index, (_, idOpt, tyOpt)) =>
                    case (idOpt, tyOpt) of
                      (SOME id, SOME ty) =>
                        if not (paramOwned (index, ty)) andalso needsDrop ty
                        then SOME id
                        else NONE
                    | _ => NONE)
                  (ListPair.zip
                     (List.tabulate (List.length paramInfo, fn i => i),
                      paramInfo))
              val _ = borrowedParams := borrowed
              val borrowedParamEntries =
                List.mapPartial
                  (fn (index, (paramSpan, idOpt, tyOpt)) =>
                    case (idOpt, tyOpt) of
                      (SOME id, SOME ty) =>
                        if not (paramOwned (index, ty)) andalso needsDrop ty
                        then SOME (paramSpan, bindingName id)
                        else NONE
                    | _ => NONE)
                  (ListPair.zip
                     (List.tabulate (List.length paramInfo, fn i => i),
                      paramInfo))
              val _ =
                borrowedLambdaParams :=
                  borrowedParamEntries @ !borrowedLambdaParams
              val resultType =
                case expressionTypeAt span of
                  SOME ty =>
                    (case Types.prune ty of
                       Types.TFun (_, result) => SOME result
                     | _ => NONE)
                | NONE => NONE
              val bodyState =
                case resultType of
                  SOME ty =>
                    if isCopy ty then analyzeExpression ([], body)
                    else moveExpression ([], body)
                | NONE => analyzeExpression ([], body)
              val _ = borrowedParams := savedBorrowed
              val _ = loans := savedLoans
              val _ =
                List.app
                  (fn (index, (paramSpan, idOpt, tyOpt)) =>
                    case (idOpt, tyOpt) of
                      (SOME id, SOME ty) =>
                        if paramOwned (index, ty) then
                          if Types.propertyOf ty = Types.Linear andalso
                             statusOf bodyState id = Available
                          then reportNotConsumed id
                          else
                            addDropsForStatus
                              (paramSpan, ty, statusOf bodyState id)
                        else ()
                    | _ => ())
                  (ListPair.zip
                     (List.tabulate (List.length paramInfo, fn i => i),
                      paramInfo))
            in
              state
            end

        | Ast.ECall (name, arguments, span) =>
            if releasesLoanAtCall name then
              let
                val savedLoans = !loans
                val state' = analyzeCall (state, name, arguments, span)
                val _ = loans := savedLoans
              in
                state'
              end
            else
              analyzeCall (state, name, arguments, span)

        | Ast.EBin ("^", left, right, _) =>
            let val state' = moveExpression (state, left)
            in moveExpression (state', right) end
        | Ast.EBin (_, left, right, _) =>
            let val state' = analyzeExpression (state, left)
            in analyzeExpression (state', right) end

        | Ast.ENeg (operand, _) =>
            analyzeExpression (state, operand)

        | Ast.EIf (condition, thenBranch, elseBranch, span) =>
            analyzeIf (state, condition, thenBranch, elseBranch, false, span)

        | Ast.ELet (bindings, body, span) =>
            analyzeLet (state, bindings, body, false, span)

      and moveExpression (state, expression) =
        case expression of
          Ast.EVar (_, span) =>
            (case useBindingAt span of
               SOME id => (noteUse id; moveBinding (state, id, span))
             | NONE => state)

        | Ast.EIf (condition, thenBranch, elseBranch, span) =>
            analyzeIf (state, condition, thenBranch, elseBranch, true, span)

        | Ast.ELet (bindings, body, span) =>
            analyzeLet (state, bindings, body, true, span)

        | Ast.ERecord (fields, _) =>
            List.foldl
              (fn ((_, value), state') => moveExpression (state', value))
              state
              fields

        | Ast.EField _ =>
            accessFieldOrMove (state, expression)

        | Ast.ETuple (values, _) =>
            List.foldl
              (fn (value, state') => moveExpression (state', value))
              state
              values

        | Ast.ETupleIndex _ =>
            accessField (state, expression)

        | Ast.ECase (scrutinee, branches, span) =>
            analyzeCase (state, scrutinee, branches, true, span)

        | Ast.EBorrow (mutable, ampersand, operand, span) =>
            analyzeExpression (state, Ast.EBorrow (mutable, ampersand, operand, span))

        | Ast.ECall _ => analyzeExpression (state, expression)

        | _ => analyzeExpression (state, expression)

      and accessFieldOrMove (state, expression) =
        case datatypeFieldBase expression of
          SOME base => moveExpression (state, base)
        | NONE => accessField (state, expression)

      and analyzeCase (state, scrutinee, branches, moveBodies, span) =
        let
          val afterScrutinee0 = analyzeExpression (state, scrutinee)
          val afterScrutinee =
            case scrutinee of
              Ast.EVar (_, span) =>
                (case useBindingAt span of
                   SOME id =>
                     (case bindingTypeById id of
                        SOME ty =>
                          if needsDrop ty then
                            moveBinding (afterScrutinee0, id, span)
                          else afterScrutinee0
                      | NONE => afterScrutinee0)
                 | NONE => afterScrutinee0)
            | _ => afterScrutinee0

          val _ =
            case expressionTypeAt (Ast.exprSpan scrutinee) of
              SOME scrutineeTy =>
                let
                  fun go (_, []) = ()
                    | go (earlier, (branch as (pattern, _, _)) :: rest) =
                        ( case pattern of
                            Ast.PWildcard span =>
                              checkTopWildcard (span, List.rev earlier, scrutineeTy)
                          | _ => checkLinearDiscards (pattern, scrutineeTy)
                        ; go (branch :: earlier, rest) )
                in
                  go ([], branches)
                end
            | NONE => ()
          val scrutineeLoanIds = activeLoanIds ()
          val branchBase = !loans
          val branchResults =
            analyzeArms
            (List.map
              (fn (pattern, guard, body) =>
                (Ast.exprSpan body ::
                   (case guard of
                      SOME condition => [Ast.exprSpan condition]
                    | NONE => []),
                fn () =>
                let
                  val _ = loans := branchBase
                  val branchIds =
                    List.mapPartial
                      (fn (_, span) => bindingIdBySpan span)
                      (patternVariables pattern)
                  val _ = List.app (fn id => carryLoans (id, scrutineeLoanIds))
                             branchIds

                  val _ = flushLoans ()
                  val guardState =
                    case guard of
                      NONE => afterScrutinee
                    | SOME condition =>
                        analyzeExpression (afterScrutinee, condition)
                  val _ = flushLoans ()
                  val branchState =
                    if moveBodies then moveExpression (guardState, body)
                    else analyzeExpression (guardState, body)
                  val branchLoans = !loans
                  val _ = checkPatternScope (branchState, pattern)
                  val _ =
                    List.app
                      (fn (_, span) =>
                        case bindingIdBySpan span of
                          NONE => ()
                        | SOME id =>
                            (case bindingTypeById id of
                               NONE => ()
                             | SOME ty =>
                                 addDropsForStatus
                                   (span, ty, statusOf branchState id)))
                      (patternVariables pattern)
                  val _ = List.app releaseCarrier branchIds
                in
                  (forgetBindings branchState branchIds, branchLoans)
                end))
              branches)
          val branchStates = List.map (fn (state, _) => state) branchResults

          val allLoans =
            unionLoanLists (List.map (fn (_, loans) => loans) branchResults)
          val resultCarriesBorrow =
            case expressionTypeAt span of
              SOME ty => Types.containsBorrow ty
            | NONE => false
          val _ = loans := allLoans
          val _ = if resultCarriesBorrow then pinLoans () else flushLoans ()
          val patternIds =
            List.foldl
              (fn ((pattern, _, _), ids) =>
                List.foldl
                  (fn ((_, span), ids) =>
                    case bindingIdBySpan span of
                      SOME id => id :: ids
                    | NONE => ids)
                  ids
                  (patternVariables pattern))
              []
              branches
          val _ =
            scheduleBranchDrops
              ( ListPair.zip
                  (List.map
                     (fn (_, _, body) => Ast.exprSpan body)
                     branches,
                   branchStates)
              , patternIds )
        in
          mergeBranchStates (afterScrutinee, branchStates, patternIds)
        end

      and analyzeCall (state, name, arguments, span) =
        let
          val savedLoans = !loans
          val marker = !nextLoanId
          val keepLoans =
            case expressionTypeAt span of
              SOME ty => carriesBorrow ty
            | NONE => OwnershipSummaries.resultBorrowSource name <> NONE
          val state' = analyzeCallInner (state, name, arguments, span)
          val _ =
            if keepLoans then
              let
                val source = callBorrowSource (name, span)
                val kept =
                  List.filter
                    (fn loan => callKeepsLoan (marker, arguments, source, loan))
                    (!loans)
              in
                loans := kept
                ; pinLoans ()
              end
            else (loans := savedLoans; flushLoans ())
        in
          state'
        end

      and analyzeCallInner (state, name, arguments, span) =
        case useBindingAt span of
          SOME id =>
            let
              val _ = noteUse id
              val state =
                if statusOf state id = Moved then
                  (reportUseAfterMove id span; state)
                else state
              val paramTypes =
                case bindingTypeById id of
                  SOME ty =>
                    (case Types.prune ty of
                       Types.TFun (params, _) => params
                     | _ => [])
                | NONE => []
              val summary =
                case List.find
                       (fn (candidate, _) => candidate = id)
                       (!lambdaParams) of
                  SOME (_, flags) => flags
                | NONE => []
              fun loop (state, paramTypes, arguments, summary) =
                case (paramTypes, arguments) of
                  (paramType :: restParams, argument :: restArguments) =>
                    let
                      val borrowed =
                        case summary of
                          (owned, _) :: _ => not owned
                        | [] => false
                      val affineArgument =
                        case expressionTypeAt (Ast.exprSpan argument) of
                          SOME ty => not (isCopy ty)
                        | NONE => true
                      val state' =
                        if borrowed andalso affineArgument then
                          ( report
                              "E0517"
                              ("cannot call `" ^ bindingName id ^
                               "` with an owned argument: its parameter is borrowed")
                              (Ast.exprSpan argument)
                              [ "`" ^ bindingName id ^
                                "` was bound to a lambda whose parameter is only read"
                              , "the call would transfer the argument without freeing it"
                              ]
                              [ "inline the lambda at a call site whose callback type decides ownership"
                              , "or pass the value to a named function, whose parameters own their arguments"
                              ]
                          ; analyzeExpression (state, argument) )
                        else
                          analyzeCallArgument
                            (state, paramType, argument, isConsume paramType)
                      val restSummary =
                        case summary of
                          _ :: rest => rest
                        | [] => []
                    in
                      loop (state', restParams, restArguments, restSummary)
                    end
                | (_, remaining) =>
                    List.foldl
                      (fn (argument, state') =>
                        analyzeExpression (state', argument))
                      state
                      remaining
            in
              loop (state, paramTypes, arguments, summary)
            end
        | NONE =>
        if LowerBuiltins.arrayMovesStoredValue name orelse
           LowerBuiltins.arrayMutatesHeader name orelse
           name = "array_take" orelse
           name = "map_set" orelse name = "map_remove" then
          let

            val movedIndices =
              if LowerBuiltins.arrayMovesStoredValue name then
                case LowerBuiltins.arrayStoreIndex name of
                  SOME index => [index]
                | NONE => []
              else if name = "map_set" then [1, 2]
              else []
            fun isMoved index = List.exists (fn i => i = index) movedIndices
            fun step (index, argument, state') =
              if isMoved index then
                ( if expressionCarriesBorrow argument then
                    report
                      "E0515"
                      "cannot store a borrow in a container"
                      (Ast.exprSpan argument)
                      [ "the container can outlive the borrow's loan" ]
                      [ "copy the borrowed data before storing it"
                      , "keep the borrow local and use it directly"
                      ]
                  else ()
                ; moveExpression (state', argument) )
              else
                ( if index = 0 then checkBorrowedMutation argument else ()
                ; analyzeArgumentRead (state', argument) )
            fun loop (index, remaining, state') =
              case remaining of
                [] => state'
                | argument :: rest =>
                    loop (index + 1, rest, step (index, argument, state'))
          in
            loop (0, arguments, state)
          end
        else if OwnershipSummaries.isContainerGetBorrow name then
          let
            val containerKind =
              if OwnershipSummaries.isMapGetBorrow name then "map" else "array"
            val afterArguments =
              List.foldl
                (fn (argument, state') => analyzeArgumentRead (state', argument))
                state
                arguments
          in
            case arguments of
              container :: _ =>
                (case placeOf container of
                   SOME place =>
                     if not (placeAvailable afterArguments place) then
                       (reportUseAfterMove (#root place)
                          (Ast.exprSpan container); afterArguments)
                     else if hasMutableLoan place then
                       ( report
                           "E0502"
                           ("cannot borrow the " ^ containerKind ^
                            " while a mutable borrow is active")
                           (Ast.exprSpan container)
                           [ "reading through `" ^ name ^ "` needs a shared borrow, but a mutable borrow is active" ]
                           [ "end the existing mutable borrow first"
                           , "or finish the mutation before reading the container"
                           ]
                       ; afterArguments
                       )
                     else
                       (addLoan (place, false); afterArguments)
                 | NONE => afterArguments)
            | [] => afterArguments
          end
        else if name = "bytes_from_string" then
          (case arguments of
             [Ast.EVar _] =>
               List.foldl
                 (fn (argument, state') => analyzeExpression (state', argument))
                 state
                 arguments
           | _ =>
               List.foldl
                 (fn (argument, state') => moveExpression (state', argument))
                 state
                 arguments)
        else if name = "bytes_to_string" orelse
                name = "bytes_to_string_range" orelse
                name = "bytes_header_end" orelse
                name = "bytes_write_string" orelse
                name = "bytes_write_int" orelse
                name = "sys.recv_into_raw" orelse
                name = "sys.send_string_raw" orelse
                name = "sys.send_from_raw" orelse
                name = "recv_into" orelse name = "send_from" orelse
                name = "sys.epoll_wait" orelse

                name = "sqlite_open" orelse name = "sqlite_exec" orelse
                name = "sqlite_prepare" then
          List.foldl
            (fn (argument, state') =>
              case argument of
                Ast.EVar _ => analyzeExpression (state', argument)
              | _ => moveExpression (state', argument))
            state
            arguments
        else if name = "os.exit" orelse
           name = "io.read_file" orelse name = "io.read_stdin" orelse
           name = "io.write_file" orelse
           name = "string_length" orelse name = "string_of_char" orelse
           name = "string_of_int" orelse name = "string_copy" orelse
           name = "sprintf" orelse name = "float_to_string" orelse
           name = "string_char_at" orelse
           name = "string_char_at_unchecked" orelse
           name = "string_slice" orelse name = "string_eq" orelse
           name = "string_find_char" orelse
           name = "string_find_char_from" orelse name = "print_span" orelse
           name = "array_new" orelse name = "array_make" orelse
           name = "array_make_uninit" orelse
           name = "array_length" orelse
           name = "array_get" orelse name = "array_get_unchecked" orelse
           name = "array_kernel_new" orelse
           name = "array_kernel_length" orelse
           name = "array_kernel_capacity" orelse
           name = "array_kernel_load" orelse
           name = "array_kernel_bounds_abort" orelse
           name = "map_new" orelse name = "map_length" orelse
           name = "map_has" orelse name = "map_get" orelse
           name = "map_keys" then
          List.foldl
            (fn (argument, state') => analyzeArgumentRead (state', argument))
            state
            arguments
        else if name = "print_string" then
          (case arguments of
             [argument] =>
               (case argument of
                  Ast.EVar _ => analyzeExpression (state, argument)
                | _ => moveExpression (state, argument))
           | _ => List.foldl
                    (fn (argument, state') =>
                      analyzeExpression (state', argument))
                    state
                    arguments)
        else
        case functionByName name of
          NONE =>
            List.foldl
              (fn (argument, state') =>
                if isConstructor name then
                  moveExpression (state', argument)
                else
                  analyzeExpression (state', argument))
              state
              arguments
        | SOME (_, {params, ...}) =>
            let
              fun loop (state, paramTypes, arguments) =
                case (paramTypes, arguments) of
                  (paramType :: restParams, argument :: restArguments) =>
                    let
                      val state' =
                        analyzeCallArgument
                          (state, paramType, argument, not (isCopy paramType))
                    in
                      loop (state', restParams, restArguments)
                    end
                | (_, remaining) =>
                    List.foldl
                      (fn (argument, state') =>
                        analyzeExpression (state', argument))
                      state
                      remaining
            in
              loop (state, params, arguments)
            end

      and analyzeCallArgument (state, paramType, argument, moveAffine) =
        case argument of
          Ast.EFn _ =>
            let
              val saved = !expectedCallback
              val _ = expectedCallback := SOME paramType
              val result = analyzeExpression (state, argument)
              val _ = expectedCallback := saved
            in
              result
            end
        | _ =>
            let
              val _ =
                case callbackConflict (paramType, argument) of
                  SOME actualParamTy =>
                    reportCallbackConflict
                      ( argument, actualParamTy, paramType
                      , Ast.exprSpan argument )
                | NONE => ()
              val _ =
                case consumeCallbackConflict (paramType, argument) of
                  SOME actualTy =>
                    report
                      "E0514"
                      ("`" ^ placeDescription argument ^
                       "` borrows its parameter, but this callback consumes it")
                      (Ast.exprSpan argument)
                      [ "the callback type is `" ^ Types.toString paramType ^
                        "`: the caller moves the argument into the callback"
                      , "`" ^ placeDescription argument ^
                        "` was bound to a lambda whose parameter of type `" ^
                        Types.toString actualTy ^
                        "` is only borrowed, so it would not free the argument"
                      ]
                      [ "inline the lambda at the call site, where the callback type decides ownership"
                      , "or pass a named function: its parameters own their arguments"
                      ]
                | NONE => ()
            in
              if moveAffine then moveExpression (state, argument)
              else analyzeExpression (state, argument)
            end

      and analyzeArgumentRead (state, argument) =
        case argument of
          Ast.EVar _ => analyzeExpression (state, argument)
        | Ast.EField _ =>
            (case placeOf argument of
               SOME place => readPlace (state, argument, place)
             | NONE => moveExpression (state, argument))
        | Ast.ETupleIndex _ =>
            (case placeOf argument of
               SOME place => readPlace (state, argument, place)
             | NONE => moveExpression (state, argument))
        | _ => moveExpression (state, argument)

      and analyzeIf (state, condition, thenBranch, elseBranch, moveBodies, span) =
        let
          val state' = analyzeExpression (state, condition)

          val _ = flushLoans ()
          val savedLoans = !loans
          fun arm branch () =
            let
              val _ = loans := savedLoans

              val _ = flushLoans ()
              val armState =
                if moveBodies then moveExpression (state', branch)
                else analyzeExpression (state', branch)
            in
              (armState, !loans)
            end
          val (thenState, loansThen, elseState, loansElse) =
            case analyzeArms
                   [ ([Ast.exprSpan thenBranch], arm thenBranch)
                   , ([Ast.exprSpan elseBranch], arm elseBranch) ] of
              [(thenState, loansThen), (elseState, loansElse)] =>
                (thenState, loansThen, elseState, loansElse)
            | _ => raise Fail "analyzeIf: two arms"
          val resultCarriesBorrow =
            case expressionTypeAt span of
              SOME ty => Types.containsBorrow ty
            | NONE => false
          val _ = loans := unionLoanLists [loansThen, loansElse]
          val _ = if resultCarriesBorrow then pinLoans () else flushLoans ()
          val _ =
            scheduleBranchDrops
              ( [ (Ast.exprSpan thenBranch, thenState)
                , (Ast.exprSpan elseBranch, elseState)
                ]
              , [] )
        in
          mergeStates (state', thenState, elseState)
        end

      and mergeStates (baseState, thenState, elseState) =
        let
          val candidateIds =
            sortUniqueIds
              (List.map #1 thenState @ List.map #1 elseState)
          fun loop (remaining, merged) =
            case remaining of
              [] => merged
            | id :: rest =>
                (case bindingTypeById id of
                   NONE => loop (rest, merged)
                 | SOME ty =>
                     if isCopy ty then loop (rest, merged)
                     else
                       let
                         val thenStatus = statusOf thenState id
                         val elseStatus = statusOf elseState id
                         val mergedState =
                           if thenStatus = elseStatus then
                             (case thenStatus of
                                Available => merged
                              | status => (id, status) :: merged)
                           else
                             let
                               val _ =
                                 if Types.propertyOf ty = Types.Linear then
                                   reportBranchMismatch id
                                 else ()
                             in
                               (case joinStatuses [thenStatus, elseStatus] of
                                  Available => merged
                                | status => (id, status) :: merged)
                             end
                       in
                         loop (rest, mergedState)
                       end)
          in
            loop (candidateIds, baseState)
          end

      and analyzeLet (state, bindings, body, moveBody, span) =
        let
          fun bindOne (binding, state) =
            case binding of
              Ast.BVal {span, value, ...} =>
                let
                  val _ =
                    case escapingBorrowOwner value of
                      SOME (owner, _) =>
                        if containsArenaEnter value then
                          ( reportedArenaBorrows :=
                              owner :: !reportedArenaBorrows
                          ; report
                            "E0703"
                            ("arena closes while `" ^ bindingName owner ^
                             "` is still borrowed")
                            (Ast.exprSpan value)
                            [ "the borrowed value lives in the arena region closed at the end of this block"
                            , "using it after the arena closes would read freed memory"
                            ]
                            [ "use or copy the borrowed data inside the arena"
                            , "or keep the owned value outside the arena and borrow that"
                            ]
                          )
                        else if
                          List.exists
                            (fn id => id = owner)
                            (!reportedArenaBorrows)
                        then ()
                        else
                          report
                            "E0507"
                            ("borrow of `" ^ bindingName owner ^
                             "` escapes the scope that owns it")
                            (Ast.exprSpan value)
                            [ "`" ^ bindingName owner ^
                              "` is declared and released inside this `let`"
                            , "the borrow value is used after its owner is freed, so it would read freed memory"
                            ]
                            [ "declare `" ^ bindingName owner ^
                              "` in an enclosing `let` so it outlives the borrow"
                            , "or copy the data before leaving the scope: `string_copy (borrow " ^
                              bindingName owner ^ ")`, or `array_get` for Copy elements"
                            , "or keep the borrow local and use it before the `let` ends"
                            ]
                    | NONE => ()
                  val bindingId = bindingIdBySpan span
                  val _ =
                    case (bindingId, value) of
                      (SOME id, Ast.EFn (params, _, _, _)) =>
                        let
                          fun summary (param : Ast.param) =
                            case bindingIdBySpan (#span param) of
                              SOME pid =>
                                (case bindingTypeById pid of
                                   SOME ty => SOME (isConsume ty, ty)
                                 | NONE => NONE)
                            | NONE => NONE
                        in
                          lambdaParams :=
                            (id, List.mapPartial summary params) :: !lambdaParams
                        end
                    | _ => ()
                  val transfers =
                    case bindingId of
                      NONE => false
                    | SOME id =>
                        (case bindingTypeById id of
                           SOME ty => not (isCopy ty)
                         | NONE => false)
                  val loanMarker = !nextLoanId
                  val state' =
                    if transfers then moveExpression (state, value)
                    else analyzeExpression (state, value)

                  val _ =
                    case bindingId of
                      NONE => ()
                    | SOME id =>
                        carryLoans (id, newLoanIds (loanMarker, id, value))
                  val _ = flushLoans ()
                in
                  state'
                end
            | Ast.BPat _ =>
                raise Fail "ownership: desugared pattern binding"
            | Ast.BFun _ =>
                raise Fail "ownership: desugared local function"
            | Ast.BGuard _ =>
                raise Fail "ownership: desugared guard binding"
          val afterBindings = List.foldl bindOne state bindings
          val _ = flushLoans ()
          val afterBody =
            if moveBody then moveExpression (afterBindings, body)
            else analyzeExpression (afterBindings, body)

          val resultCarriesBorrow =
            case expressionTypeAt span of
              SOME ty => Types.containsBorrow ty
            | NONE => false
          val _ = if resultCarriesBorrow then pinLoans () else ()
          val localIds =
            List.mapPartial
              (fn Ast.BVal {span, ...} => bindingIdBySpan span
                | Ast.BPat _ => NONE
                | Ast.BFun _ =>
                    raise Fail "ownership: desugared local function"
                | Ast.BGuard _ =>
                    raise Fail "ownership: desugared guard binding")
              bindings
          val _ = List.app releaseCarrier localIds
          val _ = if resultCarriesBorrow then () else flushLoans ()
          val _ = checkLetScope (afterBody, bindings)
          val _ =
            List.app
              (fn binding =>
                case binding of
                  Ast.BVal {span, ...} =>
                    (case bindingIdBySpan span of
                       SOME id =>
                         (case bindingTypeById id of
                            SOME ty =>
                              addDropsForStatus
                                (span, ty, statusOf afterBody id)
                          | NONE => ())
                     | NONE => ())
                | Ast.BPat _ => ()
                | Ast.BFun _ =>
                    raise Fail "ownership: desugared local function"
                | Ast.BGuard _ =>
                    raise Fail "ownership: desugared guard binding")
              bindings
        in
          forgetBindings afterBody localIds
        end

      and checkLetScope (state, bindings) =
        List.app
          (fn binding =>
            case binding of
              Ast.BVal {span, ...} =>
                (case bindingIdBySpan span of
                   SOME id =>
                     (case bindingTypeById id of
                        SOME ty =>
                          if Types.propertyOf ty = Types.Linear andalso
                             statusOf state id = Available
                          then reportNotConsumed id
                          else ()
                      | NONE => ())
                 | NONE => ())
            | Ast.BPat _ => ()
            | Ast.BFun _ =>
                raise Fail "ownership: desugared local function"
            | Ast.BGuard _ =>
                raise Fail "ownership: desugared guard binding")
          bindings

      fun checkParameters (state, params, paramTypes) =
        case (params, paramTypes) of
          (param :: restParams, paramType :: restTypes) =>
            let
              val _ =
                case bindingIdBySpan (#span param) of
                  SOME id =>
                    if Types.propertyOf paramType = Types.Linear then
                      if statusOf state id = Available then
                        reportNotConsumed id
                      else ()
                    else
                      addDropsForStatus
                        (#span param, paramType, statusOf state id)
                | NONE => ()
            in
              checkParameters (state, restParams, restTypes)
            end
        | _ => ()

      fun checkFunction declaration =
        case declaration of
          Ast.DFunClause _ =>
            raise Fail "ownership: desugared function clause"
        | Ast.DFun {name, params, body, ...} =>
            (case functionByName name of
               NONE => ()
             | SOME (_, {params = paramTypes, result, ...}) =>
                 let
                   val savedLoans = !loans
                   val _ = loans := []
                   val initialState = []
                   val bodyState =
                     if isCopy result then
                       analyzeExpression (initialState, body)
                     else
                       moveExpression (initialState, body)
                   val _ = loans := savedLoans
                   val _ = checkParameters (bodyState, params, paramTypes)

                   val _ =
                     ListPair.app
                       (fn (param, ty) =>
                         case bindingIdBySpan (#span param) of
                           SOME id =>
                             ( addDropsForStatus
                                 (#span param, ty, statusOf bodyState id)
                             ; if statusOf bodyState id = Available andalso
                                  isContainerType ty andalso
                                  List.exists
                                    (fn use => #binding use = id)
                                    uses
                               then
                                 readonlyParams :=
                                   (#span param, #name param, ty) ::
                                   !readonlyParams
                               else ()
                             )
                         | NONE => ())
                       (params, paramTypes)
                       handle _ => ()
                 in
                   ()
                 end)
        | Ast.DType _ => ()
        | Ast.DAlias _ => ()
        | Ast.DDatatype _ => ()
        | Ast.DImport _ => ()
        | Ast.DExternRecord _ => ()

      val _ = List.app checkFunction program
    in
      { diagnostics = List.map Diag.demangle (List.rev (!diagnostics))
      , drops = List.rev (!drops)
      , partialDrops = List.rev (!partialDrops)
      , earlyDrops = List.rev (!earlyDrops)
      , readonlyParams = List.rev (!readonlyParams)
      , borrowedLambdaParams = List.rev (!borrowedLambdaParams)
      }
    end
end
