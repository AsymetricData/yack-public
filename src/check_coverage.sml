structure CheckCoverage =
struct
fun check (report, span, scrutineeType, branches) =
  let
    fun isIrrefutable pattern =
      case pattern of
        Ast.PWildcard _ => true
      | Ast.PVar _ => true
      | Ast.PRecord (fields, _) =>
          List.all (fn (_, subpattern) => isIrrefutable subpattern) fields
      | Ast.PTuple (patterns, _) =>
          List.all isIrrefutable patterns
      | _ => false

    fun constructorName pattern =
      case pattern of
        Ast.PConstructor (name, _, _) => SOME name
      | _ => NONE
    fun boolValue pattern =
      case pattern of
        Ast.PBool (value, _) => SOME value
      | _ => NONE

    fun checkUnreachable
          (branches, coveredAll, coveredConstructors, seenTrue, seenFalse) =
      case branches of
        [] => ()
      | (pattern, guard, _) :: rest =>
          let
            val patternSpan = Ast.patternSpan pattern
            val _ =
              if coveredAll then
                report
                  "E3022"
                  "unreachable case branch"
                  patternSpan
                  []
                  []
              else ()
            val (candidateCovered, candidateConstructors,
                 candidateTrue, candidateFalse) =
              if coveredAll then
                (true, coveredConstructors, seenTrue, seenFalse)
              else
                case pattern of
                  Ast.PWildcard _ =>
                    (true, coveredConstructors, seenTrue, seenFalse)
                | Ast.PVar _ =>
                    (true, coveredConstructors, seenTrue, seenFalse)
                | Ast.PConstructor (name, NONE, _) =>
                    if List.exists
                         (fn candidate => candidate = name)
                         coveredConstructors then
                      ( report
                          "E3022"
                          ("unreachable case branch: constructor `" ^
                           name ^ "` is already covered")
                          patternSpan
                          []
                          []
                      ; (false, coveredConstructors, seenTrue, seenFalse)
                      )
                    else
                      (false, name :: coveredConstructors, seenTrue, seenFalse)
                | Ast.PConstructor (_, SOME _, _) =>
                    (false, coveredConstructors, seenTrue, seenFalse)
                | Ast.PBool (value, _) =>
                    if (value andalso seenTrue) orelse
                       ((not value) andalso seenFalse) then
                      ( report
                          "E3022"
                          "unreachable case branch: boolean already covered"
                          patternSpan
                          []
                          []
                      ; (false, coveredConstructors, seenTrue, seenFalse)
                      )
                    else
                      ( false
                      , coveredConstructors
                      , value orelse seenTrue
                      , (not value) orelse seenFalse
                      )
                | other =>
                    if isIrrefutable other then
                      (true, coveredConstructors, seenTrue, seenFalse)
                    else
                      (false, coveredConstructors, seenTrue, seenFalse)

            val (nextCovered, nextConstructors, nextTrue, nextFalse) =
              case guard of
                NONE => (candidateCovered, candidateConstructors,
                         candidateTrue, candidateFalse)
              | SOME _ => (coveredAll, coveredConstructors,
                           seenTrue, seenFalse)
          in
            checkUnreachable
              (rest, nextCovered, nextConstructors, nextTrue, nextFalse)
          end

    val _ =
      checkUnreachable (branches, false, [], false, false)

    datatype coverage =
        CompleteCoverage of (string * Types.ty list) list
      | IncompleteCoverage

    fun signatureOf ty =
      case Types.prune ty of
        Types.TBool =>
          CompleteCoverage [("true", []), ("false", [])]
      | Types.TUnit =>
          CompleteCoverage [("unit", [])]
      | Types.TTuple types =>
          CompleteCoverage [("tuple", types)]
      | Types.TData (Types.Data {constructors, ...}) =>
          CompleteCoverage
            (List.map
               (fn (name, argument) =>
                 ( name
                 , case argument of
                     SOME argumentTy => [argumentTy]
                   | NONE => []
                 ))
               (!constructors))
      | Types.TDataApp (data as Types.Data {constructors, ...}, arguments) =>
          CompleteCoverage
            (List.map
               (fn (name, argument) =>
                 ( name
                 , case argument of
                     SOME argumentTy =>
                       [Types.instantiateData data arguments argumentTy]
                   | NONE => []
                 ))
               (!constructors))
      | _ => IncompleteCoverage

    fun wildcards types =
      List.map (fn _ => Ast.PWildcard span) types

    fun specialize (constructor, argumentTypes, matrix) =
      List.mapPartial
        (fn row =>
          case row of
            [] => NONE
          | first :: rest =>
              (case first of
                 Ast.PWildcard _ =>
                   SOME (wildcards argumentTypes @ rest)
               | Ast.PVar _ =>
                   SOME (wildcards argumentTypes @ rest)
               | Ast.PConstructor (name, argument, _) =>
                   if name = constructor then
                     let
                       val arguments =
                         case (argument, argumentTypes) of
                           (NONE, []) => []
                         | (SOME subpattern, [_]) => [subpattern]
                         | _ => wildcards argumentTypes
                     in
                       SOME (arguments @ rest)
                     end
                   else NONE
               | Ast.PBool (value, _) =>
                   let
                     val name = if value then "true" else "false"
                   in
                     if name = constructor then
                       SOME (wildcards argumentTypes @ rest)
                     else NONE
                   end
               | Ast.PTuple (patterns, _) =>
                   if constructor = "tuple" then
                     SOME (patterns @ rest)
                   else NONE
               | _ => NONE))
        matrix

    fun covered (matrix, types) =
      if List.exists (fn row => List.all isIrrefutable row) matrix then true
      else
      case (matrix, types) of
        ([], _) => false
      | (_, []) => true
      | (_, ty :: restTypes) =>
          (case signatureOf ty of
             IncompleteCoverage =>
               let
                 val defaultRows =
                   List.mapPartial
                     (fn row =>
                       case row of
                         [] => NONE
                       | first :: rest =>
                           (case first of
                              Ast.PWildcard _ => SOME rest
                            | Ast.PVar _ => SOME rest
                            | Ast.PRecord _ =>
                                if isIrrefutable first then SOME rest
                                else NONE
                            | _ => NONE))
                     matrix
               in
                 not (List.null defaultRows) andalso
                 covered (defaultRows, restTypes)
               end
           | CompleteCoverage constructors =>
               List.all
                 (fn (constructor, argumentTypes) =>
                   covered
                     (specialize (constructor, argumentTypes, matrix),
                      argumentTypes @ restTypes))
                 constructors)

    val unguardedBranches =
      List.mapPartial
        (fn (pattern, guard, body) =>
          case guard of
            NONE => SOME (pattern, body)
          | SOME _ => NONE)
        branches
    val matrix =
      List.map (fn (pattern, _) => [pattern]) unguardedBranches
    val exhaustive = covered (matrix, [scrutineeType])

    val coveredConstructors =
      List.foldl
        (fn ((pattern, _), covered) =>
          case constructorName pattern of
            SOME name => name :: covered
          | NONE => covered)
        []
        unguardedBranches
    val topConstructors =
      case signatureOf scrutineeType of
        CompleteCoverage constructors => List.map (fn (name, _) => name) constructors
      | IncompleteCoverage => []
    val missingTop =
      List.filter
        (fn name =>
          not (List.exists (fn covered => covered = name)
                 coveredConstructors))
        topConstructors
    val coveredTrue =
      List.exists
        (fn (pattern, _) =>
          case boolValue pattern of
            SOME true => true
          | _ => false)
        unguardedBranches
    val coveredFalse =
      List.exists
        (fn (pattern, _) =>
          case boolValue pattern of
            SOME false => true
          | _ => false)
        unguardedBranches
  in
    if exhaustive then ()
    else
      case Types.prune scrutineeType of
        Types.TData _ =>
          report
            "E3021"
            "non-exhaustive case expression"
            span
            [ if List.null missingTop then
                "some constructor arguments are not covered"
              else
                "missing constructors: " ^
                String.concatWith ", " missingTop ]
            ["add a wildcard branch or complete the nested patterns"]
      | Types.TDataApp _ =>
          report
            "E3021"
            "non-exhaustive case expression"
            span
            [ if List.null missingTop then
                "some constructor arguments are not covered"
              else
                "missing constructors: " ^
                String.concatWith ", " missingTop ]
            ["add a wildcard branch or complete the nested patterns"]
      | Types.TBool =>
          report
            "E3021"
            "non-exhaustive case expression on bool"
            span
            [ "missing: " ^
              (if coveredTrue then "false" else "true") ]
            []
      | Types.TInt _ =>
          report
            "E3021"
            "non-exhaustive case expression on int"
            span
            ["add a wildcard branch `_ => ...`"]
            []
      | Types.TIntLit _ =>
          report
            "E3021"
            "non-exhaustive case expression on int"
            span
            ["add a wildcard branch `_ => ...`"]
            []
      | Types.TTuple _ =>
          report
            "E3021"
            "non-exhaustive case expression on a tuple"
            span
            ["some tuple combinations are not covered; add a wildcard branch"]
            []
      | Types.TRecord _ =>
          report
            "E3021"
            "non-exhaustive case expression on a record"
            span
            ["add a wildcard branch `_ => ...`"]
            []
      | Types.TCon _ =>
          report
            "E3021"
            "non-exhaustive case expression on a record type"
            span
            ["add a wildcard branch `_ => ...`"]
            []
      | Types.TToken =>
          report
            "E3021"
            "non-exhaustive case expression on token"
            span
            ["add a wildcard branch `_ => ...`"]
            []
      | Types.TResource =>
          report
            "E3021"
            "non-exhaustive case expression on resource"
            span
            ["add a wildcard branch `_ => ...`"]
            []
      | _ => ()
  end

end
