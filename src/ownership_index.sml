structure OwnershipIndex =
struct
  type t =
    { bindingById : int -> Resolve.binding option
    , bindingIdBySpan : Diag.span -> int option
    , expressionTypeAt : Diag.span -> Types.ty option
    , bindingTypeById : int -> Types.ty option
    , useBindingAt : Diag.span -> int option
    , functionByName : string -> (string * Check.funSig) option
    }

  fun spanKey ({start = {file, offset, ...}, stop, ...} : Diag.span) =
    let
      val file = Word.fromInt file
      val start = Word.fromInt offset
      val stop = Word.fromInt (Pos.offset stop)
    in
      (file * 0w1000003 + start) * 0w1000003 + stop
    end

  fun hashKey size key =
    Word.toInt (key mod Word.fromInt (Int.max (1, size)))

  fun buildSpanTable keyOf entries =
    let
      val size = Int.max (64, 2 * List.length entries)
      val buckets = Array.array (size, [])
      fun insert entry =
        let
          val key = keyOf entry
          val index = hashKey size key
          val bucket = Array.sub (buckets, index)
        in
          Array.update (buckets, index, (key, entry) :: bucket)
        end
      val _ = List.app insert entries
    in
      (buckets, size)
    end

  fun findSpanEntry (buckets, size) key =
    let
      val bucket = Array.sub (buckets, hashKey size key)
      fun loop remaining =
        case remaining of
          [] => NONE
        | (candidate, entry) :: rest =>
            if candidate = key then SOME entry else loop rest
    in
      loop bucket
    end

  fun make
        { bindings : Resolve.binding list
        , bindingTypes : (Diag.span * Types.ty) list
        , expressionTypes : (Diag.span * Types.ty) list
        , uses : Resolve.use list
        , functions : (string * Check.funSig) list
        } =
    let
      val bindingIdTable =
        let
          val size = Int.max (64, 2 * List.length bindings)
          val buckets = Array.array (size, [])
          fun insert binding =
            let
              val index = #id binding mod size
              val bucket = Array.sub (buckets, index)
            in
              Array.update (buckets, index, binding :: bucket)
            end
          val _ = List.app insert bindings
        in
          (buckets, size)
        end

      fun bindingById id =
        let
          val (buckets, size) = bindingIdTable
          val bucket = Array.sub (buckets, id mod size)
        in
          List.find (fn binding => #id binding = id) bucket
        end

      val bindingSpanTable =
        buildSpanTable (fn binding => spanKey (#span binding)) bindings

      fun bindingIdBySpan span =
        case findSpanEntry bindingSpanTable (spanKey span) of
          SOME binding => SOME (#id binding)
        | NONE => NONE

      val typeSpanTable =
        buildSpanTable (fn (span, _) => spanKey span) bindingTypes

      val expressionSpanTable =
        buildSpanTable (fn (span, _) => spanKey span) expressionTypes

      fun expressionTypeAt span =
        case findSpanEntry expressionSpanTable (spanKey span) of
          SOME (_, ty) => SOME ty
        | NONE => NONE

      fun bindingTypeById id =
        case bindingById id of
          NONE => NONE
        | SOME binding =>
            (case findSpanEntry typeSpanTable (spanKey (#span binding)) of
               SOME (_, ty) => SOME ty
             | NONE => NONE)

      val readUseSpanTable =
        buildSpanTable
          (fn use => spanKey (#span use))
          (List.filter (fn use => #kind use = Resolve.Read) uses)

      fun useBindingAt span =
        case findSpanEntry readUseSpanTable (spanKey span) of
          SOME use => SOME (#binding use)
        | NONE => NONE

      fun functionByName name =
        List.find (fn (candidate, _) => candidate = name) functions
    in
      { bindingById = bindingById
      , bindingIdBySpan = bindingIdBySpan
      , expressionTypeAt = expressionTypeAt
      , bindingTypeById = bindingTypeById
      , useBindingAt = useBindingAt
      , functionByName = functionByName
      }
    end
end
