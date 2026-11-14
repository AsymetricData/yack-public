structure Lower =
struct
  type input =
    { program : Ast.program
    , functions : (string * Check.funSig) list
    , datatypes : Check.declaredData list
    , expressionTypes : (Diag.span * Types.ty) list
    , bindingTypes : (Diag.span * Types.ty) list
    , calls : Check.callInfo list
    , functionValues : Check.functionValue list
    , applications : (Diag.span * Types.ty) list
    , drops : Diag.span list
    , partialDrops : (Diag.span * int list list) list
    , earlyDrops : (Diag.span * Ownership.earlyDrop list) list
    , borrowedLambdaParams : (Diag.span * string) list
    , mode : Memory.mode
    }

  type traceSite =
    { id : int
    , span : Diag.span
    , function : string
    , line : int
    , column : int
    , kind : string
    , scope : string
    , allocates : bool
    , siteRef : Ir.allocSite ref option
    , inArena : bool
    , destination : string
    , reason : string
    }

  type result =
    { program : Ir.program
    , diagnostics : Diag.t list
    , sites : traceSite list
    }

  val builtinSpan = { start = Pos.start, stop = Pos.start }

  fun lowerFrom roots
      ({ program
       , functions
       , datatypes
       , expressionTypes
       , bindingTypes
       , calls
       , functionValues
       , applications
       , drops
       , partialDrops
       , earlyDrops
       , borrowedLambdaParams
       , mode
       } : input) =
    let
      val stackMode = (mode = Memory.Stack)
      val context = Types.newContext ()
      val diagnostics = ref []
      val emitted = ref []
      val dataDefinitions = ref []
      val dataSeen = StringSet.create 64
      val queue = ref []
      val seen = StringSet.create 128
      val reported = StringSet.create 128
      val currentFunEnv : (string * string) list ref = ref []
      val currentFunctionName = ref ""
      val traceCounter = ref 0
      val traceSites = ref []
      val arenaDepth = ref 0
      val expressionTypeTable = SpanTable.make expressionTypes
      val bindingTypeTable = SpanTable.make bindingTypes
      val callTable =
        SpanTable.make (List.map (fn call => (#span call, call)) calls)
      val functionValueTable =
        SpanTable.make
          (List.map (fn value => (#span value, value)) functionValues)
      val applicationTable = SpanTable.make applications
      val dropTable =
        SpanTable.make (List.map (fn span => (span, ())) drops)
      val partialDropTable = SpanTable.make partialDrops

      fun report code message span notes hints =
        diagnostics :=
          Diag.error (code, message, span, notes, hints) :: !diagnostics

      fun reportOnce key code message span notes hints =
        if StringSet.insert (reported, key) then
          report code message span notes hints
        else ()

      fun regionFreeType ty =
        case ty of
          Ir.Int _ => true
        | Ir.Float _ => true
        | Ir.Bool => true
        | Ir.Unit => true
        | Ir.TToken => true
        | Ir.TResource => true
        | Ir.TString => true
        | Ir.TArray element => regionFreeType element
        | Ir.TMap _ => false
        | Ir.TTuple values => List.all regionFreeType values
        | Ir.TRecord (fields, _) =>
            List.all (fn (_, fieldTy) => regionFreeType fieldTy) fields
        | Ir.TData _ => false
        | Ir.TBorrow _ => false
        | Ir.TFun _ => true

      fun exprType expression =
        case expression of
          Ir.IntLit (_, ty) => ty
        | Ir.FloatLit (_, ty) => ty
        | Ir.StringLit (_, ty) => ty
        | Ir.BoolLit _ => Ir.Bool
        | Ir.UnitLit => Ir.Unit
        | Ir.Var (_, ty, _) => ty
        | Ir.Bin (_, _, _, ty, _) => ty
        | Ir.Neg (_, ty) => ty
        | Ir.Call (_, _, ty, _) => ty
        | Ir.If (_, _, _, ty) => ty
        | Ir.Let (_, body, _) => exprType body
        | Ir.Tuple (_, ty) => ty
        | Ir.Record (_, ty) => ty
        | Ir.Field (_, _, ty) => ty
        | Ir.TupleIndex (_, _, ty) => ty
        | Ir.Ctor (_, _, _, ty, _) => ty
        | Ir.Match (_, _, ty) => ty
        | Ir.PrintString _ => Ir.Unit
        | Ir.StringBuiltin (_, _, ty, _) => ty
        | Ir.BytesBuiltin (_, _, ty, _) => ty
        | Ir.SysBuiltin (_, _, ty) => ty
        | Ir.ArrayBuiltin (_, _, ty, _) => ty
        | Ir.MapBuiltin (_, _, ty, _) => ty
        | Ir.Copy (_, ty) => ty
        | Ir.Borrow (_, _, ty) => ty
        | Ir.Closure (_, ty) => ty
        | Ir.ClosureRef (_, ty) => ty
        | Ir.Apply (_, _, ty) => ty

      fun producerInfo expression =
        case expression of
          Ir.StringBuiltin (name, _, _, site) =>
            if LowerBuiltins.stringProduces name then (true, SOME site)
            else (false, NONE)
        | Ir.BytesBuiltin (name, _, _, site) =>
            if LowerBuiltins.bytesProducesString name then (true, SOME site)
            else if name = "bytes_from_string" then (true, NONE)
            else (false, NONE)
        | Ir.Bin ("^", _, _, _, site) => (true, SOME site)
        | Ir.Ctor (_, _, SOME _, _, site) => (true, SOME site)
        | Ir.ArrayBuiltin (name, _, _, siteRef) =>
            (case LowerBuiltins.arrayColor name of
               SOME LowerBuiltins.ArrayAlloc => (true, SOME siteRef)
             | SOME LowerBuiltins.ArrayAllocRooted => (true, SOME siteRef)
             | SOME LowerBuiltins.ArrayAllocUninit => (true, SOME siteRef)
             | _ => (false, NONE))
        | Ir.MapBuiltin (name, _, _, siteRef) =>
            if name = "map_new" orelse name = "map_keys" then
              (true, SOME siteRef)
            else
              (name = "map_set" orelse name = "map_remove", NONE)
        | Ir.SysBuiltin ("sqlite_column_text", _, _) => (true, NONE)
        | _ => (false, NONE)

      fun traced kind span expression =
          let
            val id = !traceCounter
            val _ = traceCounter := id + 1
            val (allocates, siteRef) = producerInfo expression
            val inArena = !arenaDepth > 0
            val _ =
              traceSites :=
                { id = id
                , span = span
                , function = !currentFunctionName
                , line = Pos.line (#start span)
                , column = Pos.column (#start span)
                , kind = kind
                , scope =
                    if inArena then "arena"
                    else "function/root"
                , allocates = allocates
                , siteRef = siteRef
                , inArena = inArena
                , destination = ""
                , reason = ""
                } :: !traceSites
          in
            if not (Memory.traceEnabled ()) then expression
            else
          let
            val ty = exprType expression
            val label = kind ^ " @ " ^ Pos.toString (#start span)
            val enter =
              Ir.SysBuiltin
                ( "sys.trace_enter"
                , [ Ir.IntLit (Int64.fromInt id, Ir.Int 32)
                  , Ir.StringLit (label, Ir.TString)
                  ]
                , Ir.Unit
                )
            val leave =
              Ir.SysBuiltin ("sys.trace_leave", [], Ir.Unit)
            val valueName = "__yack_trace_value_" ^ Int.toString id
          in
            Ir.Let
              ( [ ("__yack_trace_enter_" ^ Int.toString id, Ir.Unit, enter)
                , (valueName, ty, expression)
                ]
              , Ir.Let
                  ( [("__yack_trace_leave_" ^ Int.toString id, Ir.Unit, leave)]
                  , Ir.Var (valueName, ty, NONE)
                  , []
                  )
              , []
              )
          end
          end

      val tempCounter = ref 0

      fun typeNeedsDrop ty =
        case ty of
          Ir.TString => true
        | Ir.TArray _ => true
        | Ir.TMap _ => true
        | Ir.TFun _ => true
        | Ir.TTuple _ => true
        | Ir.TRecord (_, _) => true
        | Ir.TData (_, _, copy) => true
        | _ => false

      fun isPlaceExpression expression =
        case expression of
          Ir.Var _ => true
        | Ir.Field _ => true
        | Ir.TupleIndex _ => true
        | Ir.Borrow _ => true
        | _ => false

      fun protectsContainer name =
        name = "array_push" orelse name = "array_push_unchecked" orelse
        name = "array_set" orelse name = "array_set_unchecked" orelse
        name = "array_put" orelse name = "array_put_arena" orelse
        name = "array_length" orelse
        name = "array_length_borrow" orelse
        name = "array_get" orelse name = "array_get_unchecked" orelse
        name = "array_get_borrow_view" orelse
        name = "array_get_copy_borrow" orelse
        name = "array_kernel_length" orelse
        name = "array_kernel_capacity" orelse
        name = "array_kernel_load" orelse
        name = "array_kernel_borrow" orelse
        name = "array_kernel_take" orelse
        name = "array_kernel_write" orelse
        name = "array_kernel_reserve" orelse
        name = "array_kernel_set_length" orelse
        name = "map_set" orelse name = "map_remove" orelse
        name = "map_length" orelse name = "map_has" orelse
        name = "map_get" orelse
        name = "map_keys"

      fun protectsBytes name =
        name = "bytes_to_string" orelse
        name = "bytes_to_string_range" orelse
        name = "bytes_header_end"

      fun protectContainer (arguments, build) =
        case arguments of
          first :: rest =>
            if typeNeedsDrop (exprType first) andalso
               not (isPlaceExpression first)
            then
              let
                val index = !tempCounter
                val _ = tempCounter := index + 1
                val name = "__yack_borrow_temp_" ^ Int.toString index
                val ty = exprType first
                val call = build (Ir.Var (name, ty, NONE) :: rest)
              in
                Ir.Let
                  ( [(name, ty, first)]
                  , call
                  , [{name = name, ty = ty, skip = [], site = NONE}]
                  )
              end
            else build arguments
        | [] => build arguments

      fun protectArguments (arguments, build) =
        let
          fun loop ([], done) = build (List.rev done)
            | loop (argument :: rest, done) =
                if typeNeedsDrop (exprType argument) andalso
                   not (isPlaceExpression argument)
                then
                  let
                    val index = !tempCounter
                    val _ = tempCounter := index + 1
                    val name = "__yack_sys_temp_" ^ Int.toString index
                    val ty = exprType argument
                  in
                    Ir.Let
                      ( [(name, ty, argument)]
                      , loop (rest, Ir.Var (name, ty, NONE) :: done)
                      , [{name = name, ty = ty, skip = [], site = NONE}]
                      )
                  end
                else loop (rest, argument :: done)
        in
          loop (arguments, [])
        end

      fun copyIrType ty =
        case ty of
          Ir.Int _ => true
        | Ir.Float _ => true
        | Ir.Bool => true
        | Ir.Unit => true
        | Ir.TFun _ => true
        | Ir.TBorrow (_, false) => true
        | Ir.TData (_, _, copy) => copy
        | Ir.TTuple values => List.all copyIrType values
        | Ir.TRecord (fields, _) =>
            List.all (fn (_, fieldTy) => copyIrType fieldTy) fields
        | _ => false

      fun safeStoreValue expression =
        let val ty = exprType expression in
          if regionFreeType ty then true
          else
            case ty of

              Ir.TBorrow _ => false
            | _ =>
                if not (copyIrType ty) then

                  true
                else
                  case expression of
                    Ir.Ctor (_, _, argument, _, _) =>
                      (case argument of
                         NONE => true
                       | SOME value => safeStoreValue value)
                  | Ir.Tuple (values, _) =>
                      List.all safeStoreValue values
                  | Ir.Record (fields, _) =>
                      List.all
                        (fn (_, value) => safeStoreValue value)
                        fields
                  | Ir.Copy (value, _) => safeStoreValue value
                  | _ => false
        end

      fun validateUninitArray (name, resultTy, span) =
        if name <> "array_make_uninit" then ()
        else
          case resultTy of
            Ir.TArray (Ir.Int _) => ()
          | Ir.TArray (Ir.Float _) => ()
          | Ir.TArray Ir.Bool => ()
          | Ir.TArray element =>
              report
                "E3040"
                "`array_make_uninit` requires a primitive element type"
                span
                ["element type: " ^ Ir.tyToString element]
                ["use `array_make (value, count)` for owned elements"]
          | _ => ()

      fun validateArrayStore (name, arguments, span) =
        let
          val stored =
            case LowerBuiltins.arrayStoreIndex name of
              SOME index =>
                (case List.drop (arguments, index) of
                   value :: _ => SOME value
                 | [] => NONE)
            | NONE => NONE
          fun reject message =
            report "E0702" message span
              [ "Copy values may alias; an array element must be owned by the container" ]
              [ "use an Affine datatype for owned array elements"
              , "or store a Copy primitive/record without heap allocation"
              ]
        in
          case stored of
            NONE => ()
          | SOME value =>
              (case exprType value of
                 Ir.TData (_, _, true) =>
                   if stackMode then
                     (if safeStoreValue value then ()
                      else reject
                        ("stack mode cannot prove that the value stored by `" ^
                         name ^ "` outlives the container"))
                   else reject
                     "cannot store a Copy datatype in an array: the container owns heap storage"
               | _ =>
                   if not stackMode then ()
                   else if safeStoreValue value then ()
                   else reject
                     ("stack mode cannot prove that the value stored by `" ^
                      name ^ "` outlives the container"))
        end

      fun validateMapStore (name, arguments, span) =
        let
          val stored =
            case (name, arguments) of
              ("map_set", [_, _, value]) => SOME value
            | _ => NONE
          fun reject message =
            report "E0702" message span
              [ "a map value must live at least as long as the map" ]
              [ "store a freshly constructed value, a scalar, or a root-allocated value"
              , "bind the value in an outer scope if it is older than the map"
              ]
        in
          case stored of
            NONE => ()
          | SOME value =>
              if stackMode andalso not (safeStoreValue value) then
                reject
                  ("stack mode cannot prove that the value stored by `" ^
                   name ^ "` outlives the map")
              else ()
        end

      fun lookupFunction name =
        List.find (fn (candidate, _) => candidate = name) functions

      fun lookupDataInfo name =
        List.find (fn datatypeInfo => #name datatypeInfo = name) datatypes

      fun lookupDataDefinition name =
        List.find (fn definition => #name definition = name) (!dataDefinitions)

      fun lookupConstructor name =
        let
          fun scan [] = NONE
            | scan (datatypeInfo :: rest) =
                (case List.find
                        (fn (constructor, _) => constructor = name)
                        (#constructors datatypeInfo) of
                   SOME (_, argument) =>
                     SOME (datatypeInfo, name, argument)
                 | NONE => scan rest)
        in
          scan datatypes
        end

      fun lookupDeclaration name =
        List.find
          (fn declaration =>
            case declaration of
              Ast.DFun {name = candidate, ...} => candidate = name
            | _ => false)
          program

      fun lookupExpressionType span =
        SpanTable.lookup (expressionTypeTable, span)

      fun lookupBindingType span =
        SpanTable.lookup (bindingTypeTable, span)

      fun lookupCall span =
        SpanTable.lookup (callTable, span)

      fun lookupFunctionValue span =
        SpanTable.lookup (functionValueTable, span)

      fun lookupApplication span =
        SpanTable.lookup (applicationTable, span)

      fun instantiate substitution ty =
        Types.concretize (Types.instantiateWith substitution ty)

      fun deriveCallSubstitution callerSubstitution sigInfo callInfo =
        let
          val variables = !(#generalized sigInfo)
          val freshSubstitution = Types.makeSubstitution context variables
          val freshParams =
            List.map
              (Types.instantiateWith freshSubstitution)
              (#params sigInfo)
          val freshResult =
            Types.instantiateWith freshSubstitution (#result sigInfo)
          val callParams =
            List.map
              (fn ty =>
                Types.concretize
                  (Types.instantiateWith callerSubstitution ty))
              (#params callInfo)
          val callResult =
            Types.concretize
              (Types.instantiateWith callerSubstitution (#result callInfo))
          val paramsOk =
            List.length freshParams = List.length callParams andalso
            List.all
              (fn (left, right) => Types.unify left right)
              (ListPair.zip (freshParams, callParams))
          val resultOk = Types.unify freshResult callResult
        in
          if paramsOk andalso resultOk then
            List.map
              (fn (id, ty) => (id, Types.concretize ty))
              freshSubstitution
          else []
        end

      fun needsDrop ty =
        case ty of
          Ir.Int _ => false
        | Ir.Float _ => false
        | Ir.Bool => false
        | Ir.Unit => false
        | Ir.TBorrow _ => false
        | Ir.TToken => false
        | Ir.TResource => false
        | Ir.TString => true
        | Ir.TArray _ => true
        | Ir.TMap _ => true
        | Ir.TFun _ => true
        | Ir.TTuple _ => true
        | Ir.TRecord (_, _) => true
        | Ir.TData (_, _, copy) => true

      val isArrayBuiltin = LowerBuiltins.isArrayBuiltin
      val isMapBuiltin = LowerBuiltins.isMapBuiltin
      val isConversionBuiltin = LowerBuiltins.isConversionBuiltin
      val isStringBuiltin = LowerBuiltins.isStringBuiltin
      val isBytesBuiltin = LowerBuiltins.isBytesBuiltin
      val isSysBuiltin = LowerBuiltins.isSysBuiltin
      val isBuiltinCall = LowerBuiltins.isBuiltinCall
      fun shouldDrop span =
        SpanTable.contains (dropTable, span)

      fun partialSkip span =
        SpanTable.lookup (partialDropTable, span)

      fun isLinearData name =
        case List.find (fn definition => #name definition = name)
               (!dataDefinitions) of
          SOME definition => #linear definition
        | NONE => false

      fun rejectLinearDrop (name, span, ty, skip) =
        case Ir.linearPaths isLinearData (ty, skip) of
          [] => ()
        | path :: _ =>
            reportOnce
              ("linear-drop:" ^ name ^ ":" ^
               Int.toString (#file (#start span)) ^ ":" ^
               Int.toString (#offset (#start span)) ^ ":" ^
               Ir.tyToString ty)
              "E0403"
              ("linear value `" ^ name ^ "` would be dropped implicitly")
              span
              [ "this instance has type `" ^ Ir.tyToString ty ^
                "`, which holds a Linear value" ^
                (if null path then ""
                 else " at field path " ^ Ir.pathToString path)
              , "generic code may drop a value of a type variable; a Linear \
                \instantiation must be consumed exactly once instead"
              ]
              [ "consume the value explicitly in the generic function, or do \
                \not instantiate it with a Linear type" ]

      fun makeDrop (name, span, ty) =
        ( case partialSkip span of
            SOME skip => rejectLinearDrop (name, span, ty, skip)
          | NONE =>
              if shouldDrop span then rejectLinearDrop (name, span, ty, [])
              else ()
        ; makeDrop' (name, span, ty) )

      and makeDrop' (name, span, ty) =
        if not (needsDrop ty) then NONE
        else
          case ty of
            Ir.TData (_, _, true) =>
              SOME { name = name, ty = ty, skip = [], site = SOME span }
          | _ =>
              (case partialSkip span of
                 SOME skip => SOME { name = name, ty = ty, skip = skip, site = SOME span }
               | NONE =>
                   if shouldDrop span then
                     SOME { name = name, ty = ty, skip = [], site = SOME span }
                   else NONE)

      fun makeParamDrop (name, span, ty) =
        case ty of
          Ir.TData (_, _, true) =>
            if needsDrop ty then
              SOME { name = name, ty = ty, skip = [], site = SOME span }
            else NONE
        | _ => makeDrop (name, span, ty)

      fun patternVariables pattern =
        case pattern of
          Ast.PVar (name, span) => [(name, span)]
        | Ast.PWildcard _ => []
        | Ast.PInt _ => []
        | Ast.PBool _ => []
        | Ast.PString (_, rest, _) =>
            (case rest of
               SOME subpattern => patternVariables subpattern
             | NONE => [])
        | Ast.PBytes (_, rest, _) =>
            (case rest of
               SOME subpattern => patternVariables subpattern
             | NONE => [])
        | Ast.PConstructor (_, argument, _) =>
            (case argument of
               SOME subpattern => patternVariables subpattern
             | NONE => [])
        | Ast.PTuple (patterns, _) =>
            List.concat (List.map patternVariables patterns)
        | Ast.PRecord (fields, _) =>
            List.concat
              (List.map (fn (_, subpattern) =>
                patternVariables subpattern) fields)

      val typeLowering =
        LowerTypes.make
          { report = report
          , reportOnce = reportOnce
          , lookupDataInfo = lookupDataInfo
          , dataDefinitions = dataDefinitions
          , dataSeen = dataSeen
          , lookupExpressionType = lookupExpressionType
          , lookupBindingType = lookupBindingType
          , earlyDrops = earlyDrops
          }

      val typeSuffix = #typeSuffix typeLowering
      val lowerType = #lowerType typeLowering
      val typeAt = #typeAt typeLowering
      val bindingTypeAt = #bindingTypeAt typeLowering
      val earlyDropsAt = #earlyDropsAt typeLowering
      val sortFields = #sortFields typeLowering
      val findFieldIndex = #findFieldIndex typeLowering
      val sanitize = #sanitize typeLowering
      val lambdaCounter = ref 0

      fun freshLambdaName span =
        let
          val index = !lambdaCounter
          val _ = lambdaCounter := index + 1
          val {start = {file, offset, ...}, ...} = span
        in
          "yack_lambda_" ^ Int.toString file ^ "_" ^
          Int.toString offset ^ "_" ^ Int.toString index
        end

      fun findSubstitution id substitution =
        case List.find (fn (candidate, _) => candidate = id) substitution of
          SOME (_, ty) => SOME ty
        | NONE => NONE

      fun completeSubstitution sigInfo substitution =
        let
          val variables = !(#generalized sigInfo)
          val complete =
            List.all
              (fn (id, _) =>
                case findSubstitution id substitution of
                  SOME _ => true
                | NONE => false)
              variables
          val concrete =
            List.all
              (fn (_, ty) => Types.isConcrete (Types.concretize ty))
              substitution
        in
          complete andalso concrete
        end

      fun specializationSuffix sigInfo substitution =
        String.concatWith "_"
          (List.map
             (fn (id, _) =>
               case findSubstitution id substitution of
                 SOME ty => typeSuffix ty
               | NONE => "any")
             (!(#generalized sigInfo)))

      fun specializationName name sigInfo substitution =
        let
          val suffix = specializationSuffix sigInfo substitution
        in
          if suffix = "" then "yack_" ^ sanitize name
          else "yack_" ^ sanitize name ^ "__" ^ suffix
        end

      fun valueSubstitutionSuffix values =
        if List.null values then ""
        else
          "__hof" ^
          String.concatWith "_"
            (List.map
               (fn (index, target) =>
                 Int.toString index ^ "_" ^ sanitize target)
               values)

      fun specializationNameWithValues name sigInfo substitution values =
        specializationName name sigInfo substitution ^
        valueSubstitutionSuffix values

      fun schedule (name, substitution, values) =
        case lookupFunction name of
          NONE => ()
        | SOME (_, sigInfo) =>
            let
              val key =
                specializationNameWithValues name sigInfo substitution values
            in
              if StringSet.insert (seen, key) then
                queue := (name, substitution, values) :: !queue
              else ()
            end

      fun lookupFunEnv name =
        case List.find (fn (candidate, _) => candidate = name) (!currentFunEnv) of
          SOME (_, target) => SOME target
        | NONE => NONE

      fun peelIrBorrow ty =
        case ty of
          Ir.TBorrow (inner, _) => (inner, true)
        | _ => (ty, false)

      fun borrowifyIrType ty =
        case ty of
          Ir.TBorrow _ => ty
        | Ir.TTuple types =>
            Ir.TTuple (List.map borrowifyIrType types)
        | Ir.TRecord (fields, _) =>
            let
              val viewFields =
                List.map (fn (name, fieldTy) =>
                  (name, borrowifyIrType fieldTy)) fields
            in

              Ir.TRecord (viewFields, Ir.structuralDiscipline viewFields)
            end
        | Ir.TString => Ir.TString
        | Ir.TData _ => Ir.TBorrow (ty, false)
        | Ir.TArray _ => Ir.TBorrow (ty, false)
        | Ir.TMap _ => Ir.TBorrow (ty, false)
        | _ => ty

      fun viewBinders (pattern, expected, borrowed) =
        let
          val (inner, here) = peelIrBorrow expected
          val borrowed = borrowed orelse here
          fun nth (types, i) = List.nth (types, i) handle Subscript => Ir.Unit
        in
          case pattern of
            Ast.PVar (_, span) => if borrowed then [span] else []
          | Ast.PTuple (patterns, _) =>
              let
                val types = case inner of Ir.TTuple types => types | _ => []
              in
                List.concat
                  (List.tabulate (List.length patterns, fn i =>
                     viewBinders (List.nth (patterns, i), nth (types, i), borrowed)))
              end
          | Ast.PRecord (fields, _) =>
              let
                val types = case inner of Ir.TRecord (types, _) => types | _ => []
                fun fieldTy name =
                  case List.find (fn (candidate, _) => candidate = name) types of
                    SOME (_, ty) => ty
                  | NONE => Ir.Unit
              in
                List.concat
                  (List.map (fn (name, sub) =>
                     viewBinders (sub, fieldTy name, borrowed)) fields)
              end
          | Ast.PConstructor (name, SOME sub, _) =>
              let
                val argumentTy =
                  case inner of
                    Ir.TData (specialised, _, _) =>
                      (case lookupDataDefinition specialised of
                         SOME definition =>
                           (case List.find (fn (constructor, _) => constructor = name)
                                   (#constructors definition) of
                              SOME (_, SOME ty) => ty
                            | _ => Ir.Unit)
                       | NONE => Ir.Unit)
                  | _ => Ir.Unit
              in
                viewBinders (sub, argumentTy, borrowed)
              end
          | Ast.PString (_, SOME sub, _) =>
              viewBinders (sub, Ir.TString, borrowed)
          | Ast.PBytes (_, SOME sub, _) =>
              viewBinders (sub, Ir.TArray (Ir.Int 8), borrowed)
          | _ => []
        end

      fun lowerPattern substitution expected pattern =
        case pattern of
          Ast.PWildcard _ => Ir.PWildcard
        | Ast.PVar (name, span) =>
            Ir.PVar (name, bindingTypeAt substitution span)
        | Ast.PInt (value, _) =>
            Ir.PInt (value, expected)
        | Ast.PBool (value, _) => Ir.PBool value
        | Ast.PString (literal, rest, _) =>
            Ir.PString
              ( literal
              , Option.map
                  (fn subpattern =>
                    lowerPattern substitution Ir.TString subpattern)
                  rest
              , expected
              )
        | Ast.PBytes (bytes, rest, _) =>
            Ir.PBytes
              ( bytes
              , Option.map
                  (fn subpattern =>
                    lowerPattern substitution
                      (Ir.TArray (Ir.Int 8)) subpattern)
                  rest
              , expected
              )
        | Ast.PTuple (patterns, _) =>
            let
              val (innerExpected, borrowed) = peelIrBorrow expected
            in
              case innerExpected of
                Ir.TTuple types =>
                  let
                    val types =
                      if borrowed then List.map borrowifyIrType types
                      else types
                  in
                    if List.length patterns = List.length types then
                      Ir.PTuple
                        (ListPair.map
                           (fn (pattern, ty) =>
                             lowerPattern substitution ty pattern)
                           (patterns, types), innerExpected)
                    else Ir.PWildcard
                  end
              | _ => Ir.PWildcard
            end
        | Ast.PRecord (patterns, _) =>
            let
              val (innerExpected, borrowed) = peelIrBorrow expected
            in
              case innerExpected of
                Ir.TRecord (fields, _) =>
                  let
                    val fields =
                      if borrowed then
                        List.map
                          (fn (name, ty) =>
                             (name, borrowifyIrType ty))
                          fields
                      else fields
                  in
                    Ir.PRecord
                      (List.map
                         (fn (name, ty) =>
                           ( name
                           , case List.find
                                   (fn (candidate, _) => candidate = name)
                                   patterns of
                               SOME (_, pattern) =>
                                 lowerPattern substitution ty pattern
                             | NONE => Ir.PWildcard
                           ))
                         fields,
                       innerExpected)
                  end
              | _ => Ir.PWildcard
            end
        | Ast.PConstructor (name, argument, _) =>
            let
              val (innerExpected, borrowed) = peelIrBorrow expected
            in
              case innerExpected of
                Ir.TData (specialised, _, _) =>
                  (case lookupDataDefinition specialised of
                     SOME definition =>
                       (case List.find
                               (fn (constructor, _) => constructor = name)
                               (#constructors definition) of
                          SOME (_, argumentType) =>
                            let
                              val argumentType =
                                case argumentType of
                                  SOME ty =>
                                    SOME
                                      (if borrowed then borrowifyIrType ty
                                       else ty)
                                | NONE => NONE
                            in
                              Ir.PCtor
                                ( specialised
                                , name
                                , Option.map
                                    (lowerPattern substitution
                                       (case argumentType of
                                          SOME ty => ty
                                        | NONE => Ir.Unit))
                                    argument
                                , innerExpected
                                )
                            end
                        | NONE =>
                            ( reportOnce
                                ("pattern-constructor:" ^ name)
                                "E0607"
                                ("IR: unknown constructor `" ^ name ^ "`")
                                (Ast.patternSpan pattern)
                                []
                                [];
                              Ir.PWildcard ))
                   | NONE => Ir.PWildcard)
              | _ => Ir.PWildcard
            end

      val lowerSprintf = LowerSprintf.make { report = report, exprType = exprType }
      fun ctorSite ty =
        case ty of
          Ir.TData (_, _, true) => ref Ir.Current
        | _ => ref Ir.NativeStack

      fun copyIfNeeded ty value =
        case ty of
          Ir.TData (_, _, true) => Ir.Copy (value, ty)
        | _ => value

      fun argumentIsBorrow substitution argument =
        case typeAt substitution (Ast.exprSpan argument) of
          Ir.TBorrow _ => true
        | _ => false

      fun argumentIsConsumed substitution argument =
        not (argumentIsBorrow substitution argument) andalso
        (case argument of
           Ast.EVar _ => false
         | Ast.EBorrow _ => false
         | _ => true)

      fun lowerExpr substitution expression =
        case expression of
          Ast.EInt (value, span) =>
            (case typeAt substitution span of
               Ir.Int width => Ir.IntLit (value, Ir.Int width)
             | Ir.Float width =>
                 Ir.FloatLit
                   (Real.fromLargeInt (Int64.toLarge value), Ir.Float width)
             | _ =>
                 ( reportOnce
                     ("int-literal:" ^ Pos.toString (#start span))
                     "E0606"
                     "IR: integer literal does not have a numeric type"
                     span
                     []
                     [];
                   Ir.IntLit (value, Ir.Int 32) ))

        | Ast.EString (value, span) =>
            Ir.StringLit (value, typeAt substitution span)

        | Ast.EFloat (value, span) =>
            (case typeAt substitution span of
               Ir.Float width => Ir.FloatLit (value, Ir.Float width)
             | _ =>
                 ( reportOnce
                     ("float-literal:" ^ Pos.toString (#start span))
                     "E0606"
                     "IR: float literal does not have a float type"
                     span
                     []
                     [];
                   Ir.FloatLit (value, Ir.Float 64) ))

        | Ast.EBool (value, _) => Ir.BoolLit value

        | Ast.EUnit _ => Ir.UnitLit

        | Ast.EVar (name, span) =>
            (case lookupConstructor name of
               SOME (_, _, NONE) =>
                 let val resultType = typeAt substitution span in
                   Ir.Ctor
                     ( (case resultType of
                          Ir.TData (specialised, _, _) => specialised
                        | _ => "yack_data_unknown")
                     , name
                     , NONE
                     , resultType
                     , ctorSite resultType
                     )
                 end
             | SOME (_, _, SOME _) =>
                 ( reportOnce
                     ("ctor-value:" ^ name)
                     "E0608"
                     ("IR: constructor `" ^ name ^ "` expects an argument")
                     span
                     []
                     [];
                   Ir.UnitLit )
             | NONE =>
                 (case lookupFunEnv name of
                    SOME target =>
                      Ir.ClosureRef (target, typeAt substitution span)
                  | NONE =>
                 (case lookupFunctionValue span of
                    SOME {name = functionName, substitution = valueSubstitution, ...} =>
                      (case lookupFunction functionName of
                         NONE =>
                           ( reportOnce
                               ("function-value-target:" ^ functionName)
                               "E0604"
                               ("IR: unknown function value `" ^ functionName ^ "`")
                               span
                               []
                               []
                           ; Ir.UnitLit )
                       | SOME (_, sigInfo) =>
                           let
                             val target =
                               specializationName
                                 functionName sigInfo valueSubstitution
                             val _ = schedule (functionName, valueSubstitution, [])
                           in
                             Ir.ClosureRef (target, typeAt substitution span)
                           end)
                  | NONE =>
                      let
                        val ty = typeAt substitution span
                      in
                        copyIfNeeded ty (Ir.Var (name, ty, SOME span))
                      end)))

        | Ast.EBin (operator, left, right, span) =>
            let
              val expression =
                Ir.Bin
                  ( operator
                  , lowerExpr substitution left
                  , lowerExpr substitution right
                  , typeAt substitution span
                  , ref Ir.Current
                  )
            in
              if operator = "^" then traced "string concat" span expression
              else expression
            end

        | Ast.ENeg (operand, span) =>
            Ir.Neg (lowerExpr substitution operand, typeAt substitution span)

        | Ast.EIf (condition, thenBranch, elseBranch, span) =>
            let
              fun wrap branch expression =
                let
                  val drops =
                    earlyDropsAt substitution (Ast.exprSpan branch)
                  val lowered = lowerExpr substitution expression
                in
                  if List.null drops then lowered
                  else Ir.Let ([], lowered, drops)
                end
            in
              Ir.If
                ( lowerExpr substitution condition
                , wrap thenBranch thenBranch
                , wrap elseBranch elseBranch
                , typeAt substitution span
                )
            end

        | Ast.ELet (bindings, body, _) =>
            let
              val loweredBindings =
                List.map
                  (fn binding =>
                    case binding of
                      Ast.BVal {name, value, span, ...} =>
                        ( name
                        , bindingTypeAt substitution span
                        , lowerExpr substitution value
                        )
                    | Ast.BPat {span, ...} =>
                        ( reportOnce
                            "ir-destructuring-binding"
                            "E0601"
                            "IR: internal error: destructuring binding survived desugaring"
                            span
                            []
                            []
                        ; ("", Ir.Unit, Ir.UnitLit)
                        )
                    | Ast.BFun _ =>
                        raise Fail "lower: desugared local function"
                    | Ast.BGuard _ =>
                        raise Fail "lower: desugared guard binding")
                  bindings
              val loweredDrops =
                List.mapPartial
                  (fn binding =>
                    case binding of
                      Ast.BVal {name, span, ...} =>
                        makeDrop (name, span, bindingTypeAt substitution span)
                    | Ast.BPat _ => NONE
                    | Ast.BFun _ =>
                        raise Fail "lower: desugared local function"
                    | Ast.BGuard _ =>
                        raise Fail "lower: desugared guard binding")
                  bindings
            in
              Ir.Let
                (loweredBindings, lowerExpr substitution body, loweredDrops)
            end

        | Ast.ETuple (values, span) =>
            Ir.Tuple
              (List.map (lowerExpr substitution) values,
               typeAt substitution span)

        | Ast.ERecord (fields, span) =>
            let
              val recordType = typeAt substitution span
            in
              case recordType of
                Ir.TRecord (declaredFields, _) =>
                  Ir.Record
                    (List.map
                       (fn (name, ty) =>
                         ( name
                         , case List.find
                                 (fn (candidate, _) => candidate = name)
                                 fields of
                             SOME (_, value) => lowerExpr substitution value
                           | NONE => Ir.UnitLit
                         ))
                       declaredFields,
                     recordType)
              | _ => Ir.Record ([], Ir.TRecord ([], Ir.DAffine))
            end

        | Ast.EField (field, base, span) =>
            let
              val resultType = typeAt substitution span
              val baseType = typeAt substitution (Ast.exprSpan base)
            in
              case baseType of
                Ir.TRecord (fields, _) =>
                  (case findFieldIndex field fields of
                     SOME index =>
                       Ir.Field
                         (lowerExpr substitution base, index, resultType)
                   | NONE =>
                       ( reportOnce
                           ("field:" ^ field)
                           "E0609"
                           ("IR: unknown record field `" ^ field ^ "`")
                           span
                           []
                           [];
                         Ir.UnitLit ))
              | Ir.TBorrow (Ir.TRecord (fields, _), _) =>
                  (case findFieldIndex field fields of
                     SOME index =>
                       Ir.Field
                         (lowerExpr substitution base, index, resultType)
                   | NONE =>
                       ( reportOnce
                           ("borrow-field:" ^ field)
                           "E0609"
                           ("IR: unknown record field `" ^ field ^ "`")
                           span
                           []
                           [];
                         Ir.UnitLit ))
              | Ir.TData (dataName, _, _) =>
                  (case lookupDataDefinition dataName of
                     NONE =>
                       ( reportOnce
                           ("data-field:" ^ dataName ^ "." ^ field)
                           "E0609"
                           ("IR: unknown datatype `" ^ dataName ^ "`")
                           span
                           []
                           [];
                         Ir.UnitLit )
                   | SOME definition =>
                       let
                         val payloadName = "__projected_payload"
                         fun branch (constructor, argument) =
                           case argument of
                             SOME (payloadTy as Ir.TRecord (fields, _)) =>
                               (case findFieldIndex field fields of
                                  NONE => NONE
                                | SOME index =>
                                    SOME
                                      ( Ir.PCtor
                                          ( dataName
                                          , constructor
                                          , SOME (Ir.PVar (payloadName, payloadTy))
                                          , baseType
                                          )
                                      , NONE
                                      , Ir.Field
                                          ( Ir.Var (payloadName, payloadTy, NONE)
                                          , index
                                          , resultType
                                          )
                                      , [ { name = payloadName
                                          , ty = payloadTy
                                          , skip = [[index]]
                                          , site = NONE
                                          }
                                        ]
                                      ))
                           | _ => NONE
                         val branches =
                           List.mapPartial branch (#constructors definition)
                       in
                         if List.length branches =
                            List.length (#constructors definition)
                         then
                           Ir.Match
                             ( lowerExpr substitution base
                             , branches
                             , resultType
                             )
                         else
                           ( reportOnce
                               ("data-field-constructor:" ^ dataName ^ "." ^ field)
                               "E0609"
                               ("IR: datatype `" ^ dataName ^
                                "` has no common field `" ^ field ^ "`")
                               span
                               []
                               [];
                             Ir.UnitLit )
                       end)
              | _ => Ir.UnitLit
            end

        | Ast.ETupleIndex (index, base, span) =>
            Ir.TupleIndex
              (lowerExpr substitution base, index,
               typeAt substitution span)

        | Ast.ECall (name, arguments, span) =>
            let
              val resultType = typeAt substitution span
              val loweredArguments =
                List.map (lowerExpr substitution) arguments
              fun functionTarget value =
                case value of
                  Ir.ClosureRef (target, _) => SOME target
                | Ir.Closure (target, _) => SOME target
                | _ => NONE
              fun valueBindings index values =
                case values of
                  [] => []
                | value :: rest =>
                    (case functionTarget value of
                       SOME target =>
                         (index, target) :: valueBindings (index + 1) rest
                     | NONE => valueBindings (index + 1) rest)
              val valueSubstitution = valueBindings 0 loweredArguments
              val application = lookupApplication span
            in
              case lookupFunEnv name of
                SOME target =>
                  Ir.Call (target, loweredArguments, resultType, ref false)
              | NONE =>
              case application of
               SOME functionType =>
                 let
                   val loweredFunctionType =
                     case lowerType span
                            (instantiate substitution functionType) of
                       SOME lowered => lowered
                     | NONE => Ir.Unit
                 in
                   Ir.Apply
                     ( Ir.Var (name, loweredFunctionType, NONE)
                     , loweredArguments
                     , resultType
                     )
                 end
             | NONE =>
            let
              val printable =
                name = "print_int" orelse
                name = "print_int64" orelse
                name = "print_float" orelse
                name = "print_bool"
              val numericConversion =
                name = "int64_to_int" orelse name = "int_to_int64" orelse
                name = "int64_to_int_wrapping" orelse
                name = "int64_wrapping_add" orelse
                name = "int64_wrapping_sub" orelse
                name = "int64_wrapping_mul"
              val tokenBuiltin =
                name = "os.exit" orelse
                name = "new_token" orelse
                name = "consume_token" orelse
                name = "new_resource" orelse
                name = "drop_resource"
            in
              if name = "print_string" then
                let
                  val consume =
                    case arguments of
                      [argument] => argumentIsConsumed substitution argument
                    | _ => false
                  val argument =
                    case loweredArguments of
                      [value] => value
                    | _ => Ir.UnitLit
                in
                  Ir.PrintString (argument, consume)
                end
              else if name = "sprintf" then
                traced "sprintf" span
                  (lowerSprintf (span, arguments, loweredArguments))
              else if isArrayBuiltin name then
                ( validateArrayStore (name, loweredArguments, span)
                ; validateUninitArray (name, resultType, span)
                ; if name = "array_get_borrow" then
                    (case loweredArguments of
                       first :: _ =>
                         if not (isPlaceExpression first) then
                           report
                             "E0500"
                             "cannot borrow an array element from a temporary array"
                             span
                             ["the temporary array would be freed while the borrow is still alive"]
                             ["bind the array to a `val` first, then borrow one of its elements"]
                         else ()
                     | [] => ())
                  else ()
                ; let
                    val build =
                      if not (protectsContainer name) then
                        fn args =>
                          Ir.ArrayBuiltin (name, args, resultType,
                                           ref Ir.Current)
                      else fn args =>
                        protectContainer
                          (args, fn protected =>
                            Ir.ArrayBuiltin (name, protected, resultType,
                                             ref Ir.Current))
                    val expression = build loweredArguments
                  in
                    if name = "array_new" orelse
                       name = "array_make" orelse
                       name = "array_make_uninit" orelse
                       name = "array_kernel_new"
                    then traced name span expression
                    else if name = "array_get" orelse
                            name = "array_get_unchecked" orelse
                            name = "array_kernel_load"
                    then copyIfNeeded resultType expression
                    else expression
                  end
                )
              else if isMapBuiltin name then
                let
                  val _ = validateMapStore (name, loweredArguments, span)
                  val _ =
                    if name = "map_get_borrow" then
                      (case loweredArguments of
                         first :: _ =>
                           if not (isPlaceExpression first) then
                             report
                               "E0500"
                               "cannot borrow a map value from a temporary map"
                               span
                               ["the temporary map would be freed while the borrow is still alive"]
                               ["bind the map to a `val` first, then borrow one of its values"]
                           else ()
                       | [] => ())
                    else ()

                  val consumesKey =
                    name = "map_has" orelse name = "map_has_borrow" orelse
                    name = "map_get" orelse name = "map_get_copy_borrow" orelse
                    name = "map_get_borrow" orelse
                    name = "map_get_borrow_view" orelse name = "map_remove"
                  fun isPlaceExpression argument =
                    case argument of
                      Ast.EVar _ => true
                    | Ast.EField (_, base, _) => isPlaceExpression base
                    | Ast.ETupleIndex (_, base, _) => isPlaceExpression base
                    | _ => false
                  fun mark (index, remaining) =
                    case remaining of
                      [] => []
                    | argument :: rest =>
                        ( consumesKey andalso index = 1 andalso
                          not (isPlaceExpression argument)
                        ) :: mark (index + 1, rest)
                  val consumed = mark (0, arguments)
                in
                  let
                    val expression =
                      if not (protectsContainer name) then
                        Ir.MapBuiltin
                          (name, ListPair.zip (loweredArguments, consumed),
                           resultType, ref Ir.Current)
                      else
                        protectContainer
                          ( loweredArguments
                          , fn protected =>
                              Ir.MapBuiltin
                                ( name
                                , ListPair.zip (protected, consumed)
                                , resultType
                                , ref Ir.Current
                                )
                          )
                  in
                    if name = "map_new" orelse name = "map_keys" orelse
                       name = "map_set" orelse name = "map_remove"
                    then traced ("map " ^ name) span expression
                    else if name = "map_get" then
                      copyIfNeeded resultType expression
                    else expression
                  end
                end
              else if isSysBuiltin name then
                let
                  val _ =
                    if name = "sys.scratch_enter_all" then
                      arenaDepth := !arenaDepth + 1
                    else if name = "sys.scratch_leave" then
                      arenaDepth := Int.max (0, !arenaDepth - 1)
                    else ()
                in
                  if name = "sqlite_open" orelse name = "sqlite_exec" orelse
                     name = "sqlite_prepare"
                  then
                    protectArguments
                      (loweredArguments, fn arguments =>
                        Ir.SysBuiltin (name, arguments, resultType))
                  else Ir.SysBuiltin (name, loweredArguments, resultType)
                end
              else if isBytesBuiltin name then
                let

                  val consumed =
                    case List.map (argumentIsConsumed substitution) arguments of
                      _ :: rest => if protectsBytes name then false :: rest
                                   else argumentIsConsumed substitution (hd arguments) :: rest
                    | [] => []
                  val expression =
                    if protectsBytes name then
                      protectContainer
                        ( loweredArguments
                        , fn protected =>
                            Ir.BytesBuiltin
                              ( name
                              , ListPair.zip (protected, consumed)
                              , resultType
                              , ref Ir.Current
                              )
                        )
                    else
                      Ir.BytesBuiltin
                        ( name
                        , ListPair.zip (loweredArguments, consumed)
                        , resultType
                        , ref Ir.Current
                        )
                in
                  traced ("bytes " ^ name) span expression
                end
              else if isStringBuiltin name then
                let
                  val expression =
                    Ir.StringBuiltin
                      ( name
                      , ListPair.zip
                          ( loweredArguments
                          , List.map (argumentIsConsumed substitution) arguments
                          )
                      , resultType
                      , ref Ir.Current
                      )
                in
                  traced ("string " ^ name) span expression
                end
              else if printable orelse numericConversion orelse tokenBuiltin then
                Ir.Call (name, loweredArguments, resultType, ref false)
              else
                (case lookupConstructor name of
                   SOME (_, _, _) =>
                     (case resultType of
                        Ir.TData (specialised, _, _) =>
                          traced ("constructor " ^ name) span
                            (Ir.Ctor
                               ( specialised
                               , name
                               , case loweredArguments of
                                   [argument] => SOME argument
                                 | _ => NONE
                               , resultType
                               , ctorSite resultType
                               )
                            )
                      | _ => Ir.UnitLit)
                 | NONE =>
                     (case lookupCall span of
                        NONE =>
                          ( reportOnce
                              ("call:" ^ Pos.toString (#start span))
                              "E0604"
                              ("IR: no call information for `" ^ name ^ "`")
                              span
                              ["function values and local functions are not lowered yet"]
                              [];
                            Ir.UnitLit )
                      | SOME callInfo =>
                          (case lookupFunction (#name callInfo) of
                             NONE =>
                               ( reportOnce
                                   ("call-target:" ^ #name callInfo)
                                   "E0604"
                                   ("IR: unknown call target `" ^ #name callInfo ^ "`")
                                   span
                                   []
                                   [];
                                 Ir.UnitLit )
                           | SOME (_, sigInfo) =>
                               let
                                 val concreteSubstitution =
                                   deriveCallSubstitution
                                     substitution sigInfo callInfo
                                 val target =
                                   specializationNameWithValues
                                     (#name callInfo) sigInfo
                                     concreteSubstitution valueSubstitution
                                 val _ =
                                   schedule
                                     (#name callInfo, concreteSubstitution,
                                      valueSubstitution)
                               in
                                 if completeSubstitution sigInfo concreteSubstitution then
                                   Ir.Call
                                     (target, loweredArguments, resultType,
                                      ref false)
                                 else
                                   ( reportOnce
                                       ("generic-call:" ^ #name callInfo)
                                       "E0601"
                                       ("IR: cannot monomorphise `" ^ #name callInfo ^ "`")
                                       span
                                       ["the call has unresolved type variables: " ^
                                        String.concatWith ", "
                                          (List.map
                                             (fn (id, ty) =>
                                               Int.toString id ^ "=" ^
                                               Types.toString ty)
                                             concreteSubstitution)]
                                       [];
                                     Ir.Call
                                       (target, loweredArguments, resultType,
                                        ref false) )
                               end)))
            end
            end

        | Ast.ECase (scrutinee, branches, span) =>
            let
              val scrutineeType =
                typeAt substitution (Ast.exprSpan scrutinee)
              val loweredBranches =
                List.map
                  (fn (pattern, guard, body) =>
                    let
                      val views = viewBinders (pattern, scrutineeType, false)
                      fun isView span = List.exists (fn v => v = span) views
                    in
                    ( lowerPattern substitution scrutineeType pattern
                    , Option.map (lowerExpr substitution) guard
                    , lowerExpr substitution body
                    , List.mapPartial
                        (fn (name, variableSpan) =>
                          if isView variableSpan then NONE
                          else
                          makeDrop
                            (name, variableSpan,
                             bindingTypeAt substitution variableSpan))
                        (patternVariables pattern)
                      @ earlyDropsAt substitution (Ast.exprSpan body)
                    )
                    end)
                  branches
            in
              Ir.Match
                (lowerExpr substitution scrutinee, loweredBranches,
                 typeAt substitution span)
            end

        | Ast.EFn (params, _, body, span) =>
            let
              val functionType = typeAt substitution span
              val result =
                case functionType of
                  Ir.TFun (_, resultType) => resultType
                | _ => Ir.Unit
              val parameters =
                List.map
                  (fn (param : Ast.param) =>
                    (#name param, bindingTypeAt substitution (#span param)))
                  params
              val name = freshLambdaName span
              val savedFunEnv = !currentFunEnv
              val _ = currentFunEnv := []
              val body' = lowerExpr substitution body
              val _ = currentFunEnv := savedFunEnv

              val functionDrops =
                List.mapPartial
                  (fn (param : Ast.param) =>
                    makeParamDrop
                      ( #name param
                      , #span param
                      , bindingTypeAt substitution (#span param) ))
                  params
              val borrowedNames =
                List.mapPartial
                  (fn (param : Ast.param) =>
                    if List.exists
                         (fn (span, name) =>
                           name = #name param andalso span = #span param)
                         borrowedLambdaParams
                    then SOME (#name param)
                    else NONE)
                  params
              val _ =
                emitted :=
                  { sourceName = name
                  , name = name
                  , params = parameters
                  , result = result
                  , recursive = false
                  , body = body'
                  , drops = functionDrops
                  , borrowed = borrowedNames
                  } :: !emitted
            in
              Ir.Closure (name, functionType)
            end

        | Ast.EBorrow (mutable, _, operand, span) =>
            Ir.Borrow
              (mutable, lowerExpr substitution operand, typeAt substitution span)

        | Ast.EArena _ =>
            raise Fail "lower: desugared arena expression"

      fun lowerFunction (name, substitution, valueSubstitution) =
        case (lookupFunction name, lookupDeclaration name) of
          (SOME (_, sigInfo),
           SOME (Ast.DFun {params, body, span, recursive = declaredRec, ...})) =>
            if not (completeSubstitution sigInfo substitution) then
              ( reportOnce
                  ("function-specialization:" ^ name)
                  "E0601"
                  ("IR: cannot monomorphise function `" ^ name ^ "`")
                  span
                  ["substitution: " ^
                   String.concatWith ", "
                     (List.map
                        (fn (id, ty) =>
                          Int.toString id ^ "=" ^ Types.toString ty)
                        substitution)]
                  [];
                NONE )
            else
              let
                val parameterTypes =
                  List.map
                    (fn ty => instantiate substitution ty)
                    (#params sigInfo)
                val loweredParameterTypes =
                  List.map
                    (fn ty =>
                      case lowerType span ty of
                        SOME lowered => lowered
                      | NONE => Ir.Unit)
                    parameterTypes
                val parameters =
                  ListPair.zip
                    (List.map (fn param => #name param) params,
                     loweredParameterTypes)
                val functionDrops =
                  List.mapPartial
                    (fn (param, ty) => makeParamDrop (#name param, #span param, ty))
                    (ListPair.zip (params, loweredParameterTypes) handle _ => [])
                val result =
                  case lowerType span (instantiate substitution (#result sigInfo)) of
                    SOME lowered => lowered
                  | NONE => Ir.Unit
                val funEnv =
                  List.mapPartial
                    (fn (index, target) =>
                      if index < List.length params then
                        SOME (#name (List.nth (params, index)), target)
                      else NONE)
                    valueSubstitution
                val savedFunEnv = !currentFunEnv
                val _ = currentFunEnv := funEnv
                val savedName = !currentFunctionName
                val _ = currentFunctionName := name
                val body' = lowerExpr substitution body
                val _ = currentFunctionName := savedName
                val _ = currentFunEnv := savedFunEnv
                val loweredName =
                  specializationNameWithValues name sigInfo substitution
                    valueSubstitution
                val recursive =
                  declaredRec orelse Ir.callsName loweredName body'
              in
                SOME
                  { sourceName = name
                  , name = loweredName
                  , params = parameters
                  , result = result
                  , recursive = recursive
                  , body = body'
                  , drops = functionDrops
                  , borrowed = []
                  }
              end
          | _ => NONE

      fun processQueue () =
        case !queue of
          [] => ()
        | (name, substitution, valueSubstitution) :: rest =>
            ( queue := rest
            ; case lowerFunction (name, substitution, valueSubstitution) of
                SOME definition =>
                  (emitted := definition :: !emitted; processQueue ())
              | NONE => processQueue ()
            )

      val _ =
        List.app
          (fn name =>
            case lookupFunction name of
              SOME (_, sigInfo) =>
                if name = "main" orelse completeSubstitution sigInfo [] then
                  schedule (name, [], [])
                else ()
            | NONE => ())
          roots
      val _ = processQueue ()
      val _ = LowerEscape.annotate (!emitted)
      fun destinationOf (site : traceSite) =
        if not (#allocates site) then
          ("read", "lecture seule")
        else
          case mode of
            Memory.Heap => ("heap", "mode heap")
          | Memory.Stack =>
              if #inArena site then
                ("arena", "bloc arena")
              else
                case #siteRef site of
                  SOME siteRef =>
                    (case !siteRef of
                       Ir.Current =>
                         ("activation", "local non échappant : région d'activation")
                     | Ir.Destination =>
                         ("destination", "résultat échappant : région destination")
                     | Ir.NativeStack =>
                         ("stack", "pile native"))
                | NONE =>
                    if String.isPrefix "map" (#kind site) then
                      ("owner", "écriture dans la région du conteneur")
                    else if String.isPrefix "array" (#kind site) orelse
                            String.isSubstring "bytes_from_string" (#kind site)
                    then ("root", "conteneur : racine (M3)")
                    else ("root", "allocation racine non suivie")
      fun finishSite (site : traceSite) =
        let val (destination, reason) = destinationOf site
        in
          { id = #id site
          , span = #span site
          , function = #function site
          , line = #line site
          , column = #column site
          , kind = #kind site
          , scope = #scope site
          , allocates = #allocates site
          , siteRef = #siteRef site
          , inArena = #inArena site
          , destination = destination
          , reason = reason
          }
        end
    in
      { program =
          { datatypes = List.rev (!dataDefinitions)
          , functions = List.rev (!emitted)
          }
      , diagnostics = List.map Diag.demangle (List.rev (!diagnostics))
      , sites = List.map finishSite (List.rev (!traceSites))
      }
    end

  fun lower input = lowerFrom ["main"] input
end
