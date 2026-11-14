structure LowerTypes =
struct
  type env =
    { report : string -> string -> Diag.span -> string list -> string list -> unit
    , reportOnce : string -> string -> string -> Diag.span -> string list -> string list -> unit
    , lookupDataInfo : string -> Check.declaredData option
    , dataDefinitions : Ir.dataDef list ref
    , dataSeen : StringSet.t
    , lookupExpressionType : Diag.span -> Types.ty option
    , lookupBindingType : Diag.span -> Types.ty option
    , earlyDrops : (Diag.span * Ownership.earlyDrop list) list
    }

  fun make (env : env) =
    let
      val report = #report env
      val reportOnce = #reportOnce env
      val lookupDataInfo = #lookupDataInfo env
      val dataDefinitions = #dataDefinitions env
      val dataSeen = #dataSeen env
      val lookupExpressionType = #lookupExpressionType env
      val lookupBindingType = #lookupBindingType env
      val earlyDrops = #earlyDrops env
      val earlyDropTable = SpanTable.make earlyDrops
      val builtinSpan = { start = Pos.start, stop = Pos.start }

      fun instantiate substitution ty =
        Types.concretize (Types.instantiateWith substitution ty)

      fun sanitize text =
  String.implode
    (List.map
       (fn ch =>
         if Char.isAlphaNum ch orelse ch = #"_" then ch else #"_")
       (String.explode text))

fun findFieldIndex name fields =
  let
    fun loop (index, []) = NONE
      | loop (index, (candidate, _) :: rest) =
          if candidate = name then SOME index
          else loop (index + 1, rest)
  in
    loop (0, fields)
  end

      fun sortFields fields =
  let
    fun insert ((name, ty), []) = [(name, ty)]
      | insert ((name, ty), (other as (otherName, _)) :: rest) =
          if name <= otherName then (name, ty) :: other :: rest
          else other :: insert ((name, ty), rest)
  in
    List.foldl (fn (field, sorted) => insert (field, sorted)) [] fields
  end

      fun typeSuffix ty =
  case Types.prune (Types.concretize ty) of
    Types.TInt width => "int" ^ Int.toString width
  | Types.TFloat width => "float" ^ Int.toString width
  | Types.TBool => "bool"
  | Types.TUnit => "unit"
  | Types.TTuple fields =>
      "tuple" ^ String.concatWith "_" (List.map typeSuffix fields)
  | Types.TRecord fields =>
      "record" ^
      String.concatWith "_"
        (List.map (fn (name, ty) => name ^ typeSuffix ty) (sortFields fields))
  | Types.TCon {name, fields, property} =>
      "record_" ^ sanitize name ^
      String.concatWith "_"
        (List.map (fn (field, ty) => field ^ typeSuffix ty)
           (sortFields fields))
  | Types.TData (Types.Data {name, ...}) =>
      "data_" ^ sanitize name
  | Types.TDataApp (Types.Data {name, ...}, arguments) =>
      "data_" ^ sanitize name ^
      String.concatWith "_" (List.map typeSuffix arguments)
  | Types.TBorrow (inner, mutable, _) =>
      "borrow" ^ (if mutable then "mut_" else "") ^ typeSuffix inner
  | Types.TToken => "token"
  | Types.TResource => "resource"
  | Types.TString => "string"
  | Types.TArray inner => "array" ^ typeSuffix inner
  | Types.TMap inner => "map" ^ typeSuffix inner
  | _ => "any"

      fun discipline Types.Copy = Ir.DCopy
        | discipline Types.Affine = Ir.DAffine
        | discipline Types.Linear = Ir.DLinear

      and anonymousDiscipline lowered =
        Ir.structuralDiscipline
          (List.map (fn (name, SOME value) => (name, value)
                      | (name, NONE) => (name, Ir.Unit)) lowered)

      fun lowerType span ty =
  case Types.prune (Types.concretize ty) of
    Types.TInt width => SOME (Ir.Int width)
  | Types.TFloat width => SOME (Ir.Float width)
  | Types.TBool => SOME Ir.Bool
  | Types.TUnit => SOME Ir.Unit
  | Types.TTuple types =>
      let
        val lowered = List.map (lowerType span) types
      in
        if List.all (fn SOME _ => true | NONE => false) lowered then
          SOME (Ir.TTuple (List.map (fn SOME value => value | NONE => Ir.Unit) lowered))
        else NONE
      end
  | Types.TRecord fields =>
      let
        val sorted = sortFields fields
        val lowered =
          List.map (fn (name, ty) => (name, lowerType span ty)) sorted
      in
        if List.all (fn (_, SOME _) => true | (_, NONE) => false) lowered then
          SOME
            (Ir.TRecord
               (List.map
                  (fn (name, SOME value) => (name, value)
                    | (name, NONE) => (name, Ir.Unit))
                  lowered,
                anonymousDiscipline lowered))
        else NONE
      end
  | Types.TCon {fields, property, ...} =>
      let
        val sorted = sortFields fields
        val lowered =
          List.map (fn (field, ty) => (field, lowerType span ty)) sorted
      in
        if List.all (fn (_, SOME _) => true | (_, NONE) => false) lowered then
          SOME
            (Ir.TRecord
               (List.map
                  (fn (field, SOME value) => (field, value)
                    | (field, NONE) => (field, Ir.Unit))
                  lowered,
                discipline property))
        else NONE
      end
  | Types.TData (Types.Data {name, property, ...}) =>
      let val specialised = ensureData (name, [])
          val copy = !property = Types.Copy
      in
        SOME (Ir.TData (specialised, [], copy))
      end
  | Types.TDataApp (data as Types.Data {name, property, ...}, arguments) =>
      let
        val copy = !property = Types.Copy
        val specialised = ensureData (name, arguments)
        val loweredArguments =
          List.map
            (fn argument =>
              case lowerType span argument of
                SOME lowered => lowered
              | NONE => Ir.Unit)
            arguments
      in
        SOME (Ir.TData (specialised, loweredArguments, copy))
      end
  | Types.TBorrow (inner, mutable, _) =>
      (case lowerType span inner of
         SOME lowered => SOME (Ir.TBorrow (lowered, mutable))
       | NONE => NONE)
  | Types.TMap inner =>
      (case lowerType span inner of
         SOME lowered => SOME (Ir.TMap lowered)
       | NONE => NONE)
  | Types.TConsume inner => lowerType span inner
  | Types.TFun (params, result) =>
      let
        val loweredParams = List.map (lowerType span) params
        val loweredResult = lowerType span result
      in
        if List.all (fn SOME _ => true | NONE => false) loweredParams andalso
           (case loweredResult of SOME _ => true | NONE => false)
        then
          SOME
            (Ir.TFun
               ( List.map
                   (fn SOME value => value | NONE => Ir.Unit)
                   loweredParams
               , case loweredResult of
                   SOME value => value
                 | NONE => Ir.Unit
               ))
        else NONE
      end
  | Types.TToken => SOME Ir.TToken
  | Types.TResource => SOME Ir.TResource
  | Types.TString => SOME Ir.TString
  | Types.TArray inner =>
      (case lowerType span inner of
         SOME lowered => SOME (Ir.TArray lowered)
       | NONE => NONE)
  | other =>
      ( reportOnce
          ("type:" ^ Types.toString other)
          "E0600"
          ("IR: unsupported type `" ^ Types.toString other ^ "`")
          span
          ["the backend cannot represent this type yet"]
          [];
        NONE )

and ensureData (sourceName, arguments) =
  let
    val concreteArguments = List.map Types.concretize arguments
    val suffix =
      if List.null concreteArguments then ""
      else "__" ^
           String.concatWith "_"
             (List.map typeSuffix concreteArguments)
    val specialisedName =
      "yack_data_" ^ sanitize sourceName ^ suffix
  in
    if StringSet.insert (dataSeen, specialisedName) then
      (case lookupDataInfo sourceName of
         NONE =>
           ( report
               "E0605"
               ("IR: unknown datatype `" ^ sourceName ^ "`")
               builtinSpan
               []
               []
           ; specialisedName
           )
       | SOME datatypeInfo =>
           let
             fun lowerConstructor (constructor, argument) =
               ( constructor
               , case argument of
                   NONE => NONE
                 | SOME argumentTy =>
                     let
                       val concrete =
                         Types.instantiateParameters
                           (#paramTypes datatypeInfo)
                           concreteArguments
                           argumentTy
                     in
                       lowerType builtinSpan concrete
                     end
               )
             val constructors =
               List.map lowerConstructor (#constructors datatypeInfo)
             val parameters =
               List.map
                 (fn argument =>
                   case lowerType builtinSpan argument of
                     SOME lowered => lowered
                   | NONE => Ir.Unit)
                 concreteArguments
             val definition =
               { name = specialisedName
               , sourceName = sourceName
               , prelude = #prelude datatypeInfo
               , parameters = parameters
               , constructors = constructors
               , linear =
                   Types.joinProperties
                     (#effective datatypeInfo ::
                      List.map Types.propertyOf concreteArguments)
                   = Types.Linear
               }
           in
             dataDefinitions := definition :: !dataDefinitions;
             specialisedName
           end)
    else specialisedName
  end

      fun typeAt substitution span =
  case lookupExpressionType span of
    SOME ty =>
      (case lowerType span (instantiate substitution ty) of
         SOME lowered => lowered
       | NONE => Ir.Unit)
  | NONE =>
      ( reportOnce
          ("missing-expr-type:" ^ Pos.toString (#start span))
          "E0603"
          "IR: missing expression type"
          span
          ["the type checker did not record this expression"]
          [];
        Ir.Unit )

      fun bindingTypeAt substitution span =
  case lookupBindingType span of
    SOME ty =>
      (case lowerType span (instantiate substitution ty) of
         SOME lowered => lowered
       | NONE => Ir.Unit)
  | NONE =>
      ( reportOnce
          ("missing-binding-type:" ^ Pos.toString (#start span))
          "E0603"
          "IR: missing binding type"
          span
          []
          [];
        Ir.Unit )

      fun earlyDropsAt substitution span =
  case SpanTable.lookup (earlyDropTable, span) of
    NONE => []
  | SOME drops =>
      List.mapPartial
        (fn {name, ty, skip, ...} =>
          case lowerType span (instantiate substitution ty) of
            SOME lowered => SOME {name = name, ty = lowered, skip = skip, site = SOME span}
          | NONE => NONE)
        drops

    in
      { typeSuffix = typeSuffix
      , lowerType = lowerType
      , typeAt = typeAt
      , bindingTypeAt = bindingTypeAt
      , earlyDropsAt = earlyDropsAt
      , sortFields = sortFields
      , findFieldIndex = findFieldIndex
      , sanitize = sanitize
      }
    end
end
