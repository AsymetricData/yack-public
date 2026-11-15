structure Check =
struct
  type funSig = CheckTypes.funSig
  type declaredType = CheckTypes.declaredType
  type declaredData = CheckTypes.declaredData
  type callInfo = CheckTypes.callInfo
  type functionValue = CheckTypes.functionValue
  type aliasInfo = CheckTypes.aliasInfo
  type declaredAlias = CheckTypes.declaredAlias
  type result = CheckTypes.result

  val conversionWidths = CheckTypes.conversionWidths
  val intToStringAliases = CheckTypes.intToStringAliases
  val stringToIntAliases = CheckTypes.stringToIntAliases
  val conversionBuiltinNames = CheckTypes.conversionBuiltinNames
  val builtins = CheckTypes.builtins
  val lookup = CheckTypes.lookup
  val builtinSpan = { start = Pos.start, stop = Pos.start }

  fun checkWith (requireMain : bool, isPrelude : Diag.span -> bool)
                (program : Ast.program) : result =
    let
      val context = Types.newContext ()
      val diagnostics = ref []
      val skipBodies = ref []
      val bindingTypes = ref []
      val expressionTypes = ref []
      val calls = ref []
      val functionValues = ref []
      val applications = ref []
      val typeEnvironment = ref []
      val aliasEnvironment : (string * aliasInfo) list ref = ref []
      val declaredTypes = ref []
      val declaredAliases = ref []
      val externRecordSizes = ref []
      val declaredDatatypes = ref []
      val constructorEnvironment = ref []
      val currentTypeVars = ref []
      val currentTypeName = ref ""
      val currentParameters = ref []
      val allowTypeVars = ref false

      fun recordBindingType span ty =
        let val cell = bindingTypes in cell := (span, ty) :: !cell end

      fun recordBindingTypes (spans, types) =
        case (spans, types) of
          (span :: restSpans, ty :: restTypes) =>
            let val _ = recordBindingType span ty
            in recordBindingTypes (restSpans, restTypes) end
        | _ => ()

      fun recordExpressionType span ty =
        expressionTypes := (span, ty) :: !expressionTypes

      fun recordFunctionValue span name substitution =
        functionValues :=
          { span = span
          , name = name
          , substitution =
              List.map
                (fn (id, ty) => (id, Types.concretize ty))
                substitution
          } :: !functionValues

      fun recordCall span name substitution params result =
        calls :=
          { span = span
          , name = name
          , substitution =
              List.map
                (fn (id, ty) => (id, Types.concretize ty))
                substitution
          , params = List.map Types.concretize params
          , result = Types.concretize result
          } :: !calls

      fun report code message span notes hints =
        diagnostics :=
          Diag.error (code, message, span, notes, hints) :: !diagnostics

      fun widthName width = Types.widthToString width

      fun isBytes ty =
        case Types.prune ty of
          Types.TArray (Types.TInt 8) => true
        | _ => false

      fun owningParameterType ty =
        case Types.prune ty of
          Types.TConsume _ => ty
        | _ =>
            if Types.propertyOf ty = Types.Copy then ty
            else Types.TConsume ty

      fun namedFunctionValueType params result =
        Types.TFun (List.map owningParameterType params, result)

      fun typeHints expected actual =
        let
          val expected = Types.prune expected
          val actual = Types.prune actual
        in
          case (expected, actual) of
            (Types.TInt expectedWidth, Types.TInt actualWidth) =>
              if expectedWidth = actualWidth then []
              else
                [ "fixed-width integers do not convert implicitly: `" ^
                  widthName expectedWidth ^ "` is " ^
                  Int.toString expectedWidth ^ "-bit, `" ^
                  widthName actualWidth ^ "` is " ^
                  Int.toString actualWidth ^ "-bit"
                , if expectedWidth = 32 andalso actualWidth = 64 then
                    "use `int64_to_int (value)` to narrow (it stops if the value does not fit), or keep `int64` and print it with `print_int64`"
                  else if expectedWidth = 64 andalso actualWidth = 32 then
                    "use `int_to_int64 (value)` to widen explicitly"
                  else
                    "print the value as text with `print_string (" ^
                    widthName actualWidth ^ "_to_string (value))`, or use `" ^
                    widthName expectedWidth ^ "` if that range is enough"
                ]
          | (Types.TInt 32, Types.TIntLit _) =>
              ["integer literals default to `int` (32-bit); add an annotation such as `(value : int64)` if you need a wider type"]
          | (Types.TIntLit _, Types.TInt _) => []
          | (Types.TUnit, _) =>
              ["this context expects `unit`; use `let _ = value in () end` if the value is only needed for its effect"]
          | (_, Types.TUnit) =>
              ["this expression produces `unit`; the surrounding context expects a value"]
          | (Types.TBool, Types.TInt _) =>
              ["a condition must be a `bool`; compare explicitly, for example `x = 0` or `x <> 0`"]
          | (Types.TInt _, Types.TBool) =>
              ["a `bool` is not an integer; convert it explicitly, for example `if b then 1 else 0`"]

          | (Types.TBorrow (inner, false, _), _) =>
              if Types.toString (Types.prune inner) = Types.toString actual then
                case actual of
                  Types.TString =>
                    ["pass a shared borrow: `borrow value`; a literal works too: `borrow \"text\"`"]
                | _ => ["pass a shared borrow: `borrow value`"]
              else []
          | (Types.TBorrow (inner, true, _), _) =>
              if Types.toString (Types.prune inner) = Types.toString actual then
                ["pass a mutable borrow: `borrow mut value`"]
              else []
          | (Types.TString, _) =>
              if isBytes actual then
                ["convert the bytes with `bytes_to_string (value)`"]
              else []
          | (_, Types.TString) =>
              if isBytes expected then
                ["convert the string with `bytes_from_string (borrow value)`"]
              else []
          | (_, Types.TBorrow (inner, _, _)) =>
              if Types.toString expected = Types.toString (Types.prune inner) then
                ["the value is a borrow; use the owned value, or take a `borrow` parameter in the callee"]
              else []
          | _ => []
        end

      fun mismatchWith message span expected actual extraHints =
        report
          "E3004"
          message
          span
          [ "expected: " ^ Types.toString expected
          , "found: " ^ Types.toString actual
          ]
          (typeHints expected actual @ extraHints)

      fun mismatch message span expected actual =
        mismatchWith message span expected actual []

      fun conventionMismatch message span expected actual =
        report
          "E3004"
          message
          span
          [ "expected: " ^ Types.toString expected
          , "found: " ^ Types.toString actual
          ]
          [ "make the ownership convention explicit and identical on both sides"
          , "a named function owns its parameters; a callback borrows them unless its type says `consume`"
          ]

      fun argumentHints name expected actual =
        case (name, Types.prune expected, Types.prune actual) of
          ("print_int", Types.TInt 32, Types.TInt 64) =>
              [ "`print_int` only prints 32-bit `int`; use `print_int64 (value)` for an `int64`" ]
        | ("print_int", Types.TInt 32, Types.TInt width) =>
            if width = 32 then []
            else
              [ "`print_int` only prints 32-bit `int`; this argument has type `" ^
                widthName width ^ "`" ]
        | ("print_int64", Types.TInt 64, Types.TInt 32) =>
              [ "`print_int64` expects an `int64`; use `print_int` or convert with `int_to_int64 (value)`" ]
        | ("print_bool", _, Types.TBool) => []
        | ("print_bool", _, _) =>
            ["`print_bool` expects a `bool`; print the numeric result with `print_int`"]
        | ("print_string", _, Types.TString) => []
        | ("print_string", Types.TString, Types.TInt width) =>
            if width = 32 then ["convert the number with `string_of_int (value)`"]
            else
              ["convert the number with `" ^ widthName width ^
               "_to_string (value)`"]
        | _ => []

      fun ioResultOf ty =
        case List.find (fn (name, _) => name = "result") (!typeEnvironment) of
          SOME (_, Types.TData (data as Types.Data _)) =>
            Types.TDataApp (data, [ty])
        | _ => ty

      fun optionOf ty =
        case List.find (fn (name, _) => name = "option") (!typeEnvironment) of
          SOME (_, Types.TData (data as Types.Data _)) =>
            Types.TDataApp (data, [ty])
        | _ => ty

      fun sortFields fields =
        let
          fun insert ((name, ty), []) = [(name, ty)]
            | insert ((name, ty), (other as (otherName, _)) :: rest) =
                if name <= otherName then (name, ty) :: other :: rest
                else other :: insert ((name, ty), rest)
        in
          List.foldl (fn (field, sorted) => insert (field, sorted)) [] fields
        end

      fun resolveSyntaxType span syntax =
        case syntax of
          Ast.TyInt width => Types.TInt width
        | Ast.TyFloat width => Types.TFloat width
        | Ast.TyBool => Types.TBool
        | Ast.TyUnit => Types.TUnit
        | Ast.TyToken => Types.TToken
        | Ast.TyResource => Types.TResource
        | Ast.TyString => Types.TString
        | Ast.TyTuple types =>
            Types.TTuple (List.map (resolveSyntaxType span) types)
        | Ast.TyFun (params, result) =>
            Types.TFun
              ( List.map (resolveSyntaxType span) params
              , resolveSyntaxType span result
              )
        | Ast.TyConsume (_, inner) =>
            Types.TConsume (resolveSyntaxType span inner)
        | Ast.TyRecord fields =>
            Types.TRecord
              (sortFields
                 (List.map
                    (fn (field, syntax) =>
                      (field, resolveSyntaxType span syntax))
                    fields))
        | Ast.TyBorrow (mutable, _, inner, provenance) =>
            let
              val resolvedProvenance =
                case provenance of
                  NONE => NONE
                | SOME (Ast.ProvIndex index) => SOME index
                | SOME (Ast.ProvName name) =>
                    (case List.find
                            (fn (candidate, _) => candidate = name)
                            (!currentParameters) of
                       SOME (_, index) => SOME index
                     | NONE =>
                         ( report
                             "E0505"
                             ("unknown borrow source parameter `" ^ name ^ "`")
                             span
                             ["name a parameter of this function"]
                             [];
                           NONE ))
            in
              Types.TBorrow
                (resolveSyntaxType span inner, mutable, resolvedProvenance)
            end
        | Ast.TyVar (name, equality) =>
            if not (!allowTypeVars) then
              (report
                 "E3028"
                 "type variables in type declarations are not supported yet"
                 span
                 []
                 [];
               Types.fresh context)
            else
              (case List.find
                      (fn (candidate, _) => candidate = name)
                      (!currentTypeVars) of
                 SOME (_, ty) => ty
               | NONE =>
                   let
                     val ty =
                       if equality then Types.freshEquality context
                       else Types.fresh context
                     val _ = currentTypeVars := (name, ty) :: !currentTypeVars
                   in
                     ty
                   end)
        | Ast.TyName "io_result" => ioResultOf (Types.TString)
        | Ast.TyName "c_ptr" => Types.TCPtr
        | Ast.TyName name =>
            (case List.find
                    (fn (candidate, _) => candidate = name)
                    (!aliasEnvironment) of
               SOME (_, info) =>
                 if List.null (#params info) then #body info
                 else
                   (report
                      "E3029"
                      ("type alias `" ^ name ^
                       "` expects " ^ Int.toString (List.length (#params info)) ^
                       " type argument(s)")
                      span
                      [ "write `" ^
                        (if List.length (#params info) = 1 then "'a " ^ name
                         else "('a, 'b) " ^ name) ^ "`" ]
                      [];
                    Types.TInt 32)
             | NONE =>
            (case List.find
                    (fn (candidate, _) => candidate = name)
                    (!typeEnvironment) of
               SOME (_, (ty as Types.TData (Types.Data {params, ...}))) =>
                 if List.null params then ty
                 else
                   (report
                      "E3029"
                      ("type constructor `" ^ name ^
                       "` expects " ^ Int.toString (List.length params) ^
                       " type argument(s)")
                      span
                      [ "write `" ^
                        (if List.length params = 1 then "'a " ^ name
                         else "('a, 'b) " ^ name) ^ "`" ]
                      [];
                    Types.TInt 32)
             | SOME (_, ty) => ty
             | NONE =>
                 if name = !currentTypeName then
                   (report
                      "E3012"
                      ("recursive record type `" ^ name ^
                       "` is not supported")
                      span
                      []
                      ["declare a recursive datatype instead of a `type` record"];
                    Types.TInt 32)
                 else
                   (report
                      "E3012"
                      ("unknown type `" ^ name ^ "`")
                      span
                      []
                      [];
                    Types.TInt 32)))
        | Ast.TyApp ("array", argumentSyntax) =>
            Types.TArray (resolveSyntaxType span argumentSyntax)
        | Ast.TyApp ("map", argumentSyntax) =>
            Types.TMap (resolveSyntaxType span argumentSyntax)
        | Ast.TyApp (name, argumentSyntax) =>
            (case List.find
                    (fn (candidate, _) => candidate = name)
                    (!aliasEnvironment) of
               SOME (_, info) =>
                 let val params = #params info in
                   if List.null params then
                     (report
                        "E3029"
                        ("type alias `" ^ name ^ "` is not parameterized")
                        span
                        []
                        [];
                      Types.TInt 32)
                   else
                     let
                       val argument = resolveSyntaxType span argumentSyntax
                       val arguments =
                         case params of
                           [_] => [argument]
                         | _ =>
                             (case Types.prune argument of
                                Types.TTuple items =>
                                  if List.length items = List.length params then
                                    items
                                  else []
                              | _ => [])
                     in
                       if List.length arguments = List.length params then
                         Types.instantiateParameters params arguments (#body info)
                       else
                         (report
                            "E3029"
                            ("type alias `" ^ name ^ "` expects " ^
                             Int.toString (List.length params) ^
                             " type arguments")
                            span
                            [ "write `('a, 'b) " ^ name ^ "`" ]
                            [];
                          Types.TInt 32)
                     end
                 end
             | NONE =>
            (case List.find
                    (fn (candidate, _) => candidate = name)
                    (!typeEnvironment) of
               SOME (_, Types.TData (data as Types.Data {params, ...})) =>
                 if List.length params = 1 then
                   Types.TDataApp (data, [resolveSyntaxType span argumentSyntax])
                 else if List.length params = 0 then
                   (report
                      "E3029"
                      ("type `" ^ name ^ "` is not parameterized")
                      span
                      []
                      [];
                    Types.TInt 32)
                 else
                   let
                     val argument = resolveSyntaxType span argumentSyntax
                     val arguments =
                       case Types.prune argument of
                         Types.TTuple items =>
                           if List.length items = List.length params then items
                           else []
                       | _ => []
                   in
                     if List.length arguments = List.length params then
                       Types.TDataApp (data, arguments)
                     else
                       (report
                          "E3029"
                          ("type `" ^ name ^ "` expects " ^
                           Int.toString (List.length params) ^
                           " type arguments")
                          span
                          [ "write `('a, 'b) " ^ name ^ "`" ]
                          [];
                        Types.TInt 32)
                   end
             | SOME (_, _) =>
                 (report
                    "E3029"
                    ("type `" ^ name ^ "` is not a datatype type constructor")
                    span
                    []
                    [];
                  Types.TInt 32)
             | NONE =>
                 (report
                    "E3012"
                    ("unknown type `" ^ name ^ "`")
                    span
                    []
                    [];
                  Types.TInt 32)))

      fun lookupConstructor name =
        List.find
          (fn (constructor, _, _) => constructor = name)
          (!constructorEnvironment)

      fun instantiateConstructor (resultType, argumentType) =
        case resultType of
          Types.TData (data as Types.Data {params, ...}) =>
            if List.null params then (resultType, argumentType)
            else
              let
                val variables = Types.freeVarsOfTypes params
                val substitution = Types.makeSubstitution context variables
                val arguments = List.map (Types.instantiateWith substitution) params
              in
                ( Types.TDataApp (data, arguments)
                , Option.map (Types.instantiateWith substitution) argumentType
                )
              end
        | _ => (resultType, argumentType)

      fun propertyJoin properties =
        List.foldl
          (fn (property, best) =>
            case (property, best) of
              (Types.Linear, _) => Types.Linear
            | (_, Types.Linear) => Types.Linear
            | (Types.Affine, _) => Types.Affine
            | (_, Types.Affine) => Types.Affine
            | _ => Types.Copy)
          Types.Copy
          properties

      fun typeNameTaken name =
        List.exists (fn (existing, _) => existing = name) (!typeEnvironment)
        orelse List.exists (fn (existing, _) => existing = name)
                 (!aliasEnvironment)

      fun buildTypeDeclarations program =
        List.app
          (fn declaration =>
            case declaration of
              Ast.DFun _ => ()
            | Ast.DImport _ => ()
            | Ast.DFunClause _ =>
                raise Fail "check: desugared function clause"
            | Ast.DAlias {name, params, body, span} =>
                if typeNameTaken name then
                  report
                    "E3013"
                    ("duplicate type `" ^ name ^ "`")
                    span
                    []
                    []
                else
                  let
                    val parameterTypes =
                      List.map
                        (fn (_, equality) =>
                          if equality then Types.freshEquality context
                          else Types.fresh context)
                        params
                    val parameterEnvironment =
                      ListPair.zip
                        (List.map (fn (name, _) => name) params,
                         parameterTypes)
                    val savedVars = !currentTypeVars
                    val savedAllow = !allowTypeVars
                    val _ = currentTypeVars := parameterEnvironment
                    val _ = allowTypeVars := true
                    val bodyType = resolveSyntaxType span body
                    val _ = currentTypeVars := savedVars
                    val _ = allowTypeVars := savedAllow
                  in
                    aliasEnvironment :=
                      (name,
                       { params = parameterTypes
                       , body = bodyType
                       , span = span
                       }) :: !aliasEnvironment;
                    declaredAliases :=
                      { name = name
                      , property = Types.propertyOf bodyType
                      , body = Ast.tyToString body
                      , span = span
                      } :: !declaredAliases
                  end
            | Ast.DType {name, declared, fields, span} =>
                if typeNameTaken name then
                  report
                    "E3013"
                    ("duplicate type `" ^ name ^ "`")
                    span
                    []
                    []
                else
                  let
                    val savedTypeName = !currentTypeName
                    val _ = currentTypeName := name
                    val fieldTypes =
                      sortFields
                        (List.map
                           (fn (field, syntax) =>
                             (field, resolveSyntaxType span syntax))
                           fields)
                    val _ = currentTypeName := savedTypeName
                    val fieldProperty =
                      propertyJoin
                        (List.map (fn (_, ty) => Types.propertyOf ty) fieldTypes)
                    val declaredProperty =
                      case declared of
                        SOME Ast.PCopy => Types.Copy
                      | SOME Ast.PAffine => Types.Affine
                      | SOME Ast.PLinear => Types.Linear
                      | NONE => Types.Affine
                    val effective =
                      case declared of
                        NONE => propertyJoin [Types.Affine, fieldProperty]
                      | SOME Ast.PCopy =>
                          if fieldProperty = Types.Copy then Types.Copy
                          else
                            (report
                               "E3015"
                               ("type `" ^ name ^
                                "` is declared Copy but contains non-Copy fields")
                               span
                               ["effective field property: " ^
                                Types.propertyToString fieldProperty]
                               [];
                             fieldProperty)
                      | SOME Ast.PAffine =>
                          if fieldProperty = Types.Linear then
                            (report
                               "E3016"
                               ("type `" ^ name ^
                                "` is declared Affine but contains a Linear field")
                               span
                               ["declare it Linear, or remove the Linear field"]
                               [];
                             Types.Linear)
                          else Types.Affine
                      | SOME Ast.PLinear => Types.Linear
                    val typeValue =
                      Types.TCon
                        { name = name
                        , property = effective
                        , fields = fieldTypes
                        }
                    val declaredInfo =
                      { name = name
                      , declared = declaredProperty
                      , effective = effective
                      , fields = fieldTypes
                      , span = span
                      }
                  in
                    typeEnvironment := (name, typeValue) :: !typeEnvironment;
                    declaredTypes := declaredInfo :: !declaredTypes
                  end

            | Ast.DExternRecord {name, nameSpan, size, fields, span} =>
                if typeNameTaken name then
                  report
                    "E3013"
                    ("duplicate type `" ^ name ^ "`")
                    nameSpan
                    []
                    []
                else
                  let
                    val savedTypeName = !currentTypeName
                    val _ = currentTypeName := name
                    val resolvedFields =
                      List.map
                        (fn (field, syntax, offset) =>
                          (field, resolveSyntaxType span syntax, offset))
                        fields
                    val _ = currentTypeName := savedTypeName
                    fun fieldSize ty =
                      case Types.prune ty of
                        Types.TInt 8 => SOME 1
                      | Types.TInt 16 => SOME 2
                      | Types.TInt 32 => SOME 4
                      | Types.TInt 64 => SOME 8
                      | Types.TCPtr => SOME 8
                      | Types.TFloat 32 => SOME 4
                      | Types.TFloat 64 => SOME 8
                      | Types.TCon {name = other, ...} =>
                          (case List.find
                                  (fn (candidate, _) => candidate = other)
                                  (!externRecordSizes) of
                             SOME (_, otherSize) => SOME otherSize
                           | NONE => NONE)
                      | _ => NONE
                    val _ =
                      List.app
                        (fn (field, ty, offset) =>
                          case fieldSize ty of
                            NONE =>
                              report
                                "E3047"
                                ("extern record field `" ^ field ^
                                 "` has an unsupported type")
                                span
                                [ "type: " ^ Types.toString ty ]
                                [ "use int8/int16/int32/int64, float32/float64 or another extern record" ]
                          | SOME bytes =>
                              if offset + bytes <= size then ()
                              else
                                report
                                  "E3047"
                                  ("extern record field `" ^ field ^
                                   "` does not fit in " ^ Int.toString size ^
                                   " bytes")
                                  span
                                  [ "offset: " ^ Int.toString offset ^
                                    ", size: " ^ Int.toString bytes ]
                                  [ "increase `size` or move the field" ])
                        resolvedFields
                    val typeFields =
                      List.map (fn (field, ty, _) => (field, ty)) resolvedFields
                    val property =
                      propertyJoin
                        (List.map (fn (_, ty, _) => Types.propertyOf ty)
                           resolvedFields)
                    val effective =
                      if property = Types.Copy then Types.Copy
                      else
                        (report
                           "E3047"
                           ("extern record `" ^ name ^
                            "` may only contain Copy fields")
                           span
                           []
                           [];
                         Types.Copy)
                    val typeValue =
                      Types.TCon
                        { name = name
                        , property = effective
                        , fields = typeFields
                        }
                    val declaredInfo =
                      { name = name
                      , declared = Types.Copy
                      , effective = effective
                      , fields = typeFields
                      , span = span
                      }
                  in
                    typeEnvironment := (name, typeValue) :: !typeEnvironment;
                    declaredTypes := declaredInfo :: !declaredTypes;
                    externRecordSizes := (name, size) :: !externRecordSizes
                  end

            | Ast.DDatatype {name, params, declared, constructors, span} =>
                if typeNameTaken name then
                  report
                    "E3013"
                    ("duplicate type `" ^ name ^ "`")
                    span
                    []
                    []
                else
                  let
                    val parameterTypes =
                      List.map
                        (fn (_, equality) =>
                          if equality then Types.freshEquality context
                          else Types.fresh context)
                        params
                    val parameterEnvironment =
                      ListPair.zip
                        (List.map (fn (name, _) => name) params, parameterTypes)
                    val dataRecord =
                      { name = name
                      , params = parameterTypes
                      , property = ref Types.Affine
                      , constructors = ref []
                      }
                    val typeValue = Types.TData (Types.Data dataRecord)
                    val _ =
                      typeEnvironment := (name, typeValue) :: !typeEnvironment
                    val seen = StringSet.create 64
                    val savedVars = !currentTypeVars
                    val savedAllow = !allowTypeVars
                    val _ = currentTypeVars := parameterEnvironment
                    val _ = allowTypeVars := true
                    val constructorTypes =
                      List.map
                        (fn (constructor, argumentSyntax) =>
                          let
                            val _ =
                              if StringSet.insert (seen, constructor) then ()
                                                    else
                              report
                                "E3024"
                                ("duplicate constructor `" ^ constructor ^ "`")
                                span
                                []
                                []
                            val _ =
                              if List.exists
                                   (fn (existing, _, _) =>
                                     existing = constructor)
                                   (!constructorEnvironment) then
                                report
                                  "E3024"
                                  ("constructor `" ^ constructor ^
                                   "` is already declared")
                                  span
                                  []
                                  []
                              else ()
                            val argumentType =
                              case argumentSyntax of
                                SOME syntax => SOME (resolveSyntaxType span syntax)
                              | NONE => NONE
                            val _ =
                              constructorEnvironment :=
                                (constructor, typeValue, argumentType) ::
                                !constructorEnvironment
                          in
                            (constructor, argumentType)
                          end)
                        constructors
                    val _ = currentTypeVars := savedVars
                    val _ = allowTypeVars := savedAllow
                    val argumentProperty =
                      propertyJoin
                        (List.map
                           (fn (_, argument) =>
                             case argument of
                               SOME ty => Types.propertyOf ty
                             | NONE => Types.Copy)
                           constructorTypes)
                    val declaredProperty =
                      case declared of
                        SOME Ast.PCopy => Types.Copy
                      | SOME Ast.PAffine => Types.Affine
                      | SOME Ast.PLinear => Types.Linear
                      | NONE => Types.Affine
                    val effective =
                      case declared of
                        NONE =>
                          propertyJoin [Types.Affine, argumentProperty]
                      | SOME Ast.PCopy =>
                          if argumentProperty = Types.Copy then Types.Copy
                          else
                            (report
                               "E3015"
                               ("type `" ^ name ^
                                "` is declared Copy but contains non-Copy constructor arguments")
                               span
                               ["effective argument property: " ^
                                Types.propertyToString argumentProperty]
                               [];
                             argumentProperty)
                      | SOME Ast.PAffine =>
                          if argumentProperty = Types.Linear then
                            (report
                               "E3016"
                               ("type `" ^ name ^
                                "` is declared Affine but contains a Linear constructor argument")
                               span
                               ["declare it Linear, or remove the Linear argument"]
                               [];
                             Types.Linear)
                          else Types.Affine
                      | SOME Ast.PLinear => Types.Linear
                    val _ = #property dataRecord := effective
                    val _ = #constructors dataRecord := constructorTypes
                    val declaredInfo =
                      { name = name
                      , params = params
                      , paramTypes = parameterTypes
                      , declared = declaredProperty
                      , effective = effective
                      , constructors = constructorTypes
                      , span = span
                      , prelude = isPrelude span
                      }
                  in
                    declaredDatatypes := declaredInfo :: !declaredDatatypes
                  end)
          program

      fun buildEnvironment () =
        let
          fun isExternBody body =
            case body of
              Ast.EExtern _ => true
            | _ => false
          fun externParameterSupported ty =
            case Types.prune ty of
              Types.TInt 32 => true
            | Types.TInt 64 => true
            | Types.TCPtr => true
            | Types.TBorrow (Types.TString, false, _) => true
            | Types.TBorrow (Types.TArray (Types.TInt 8), _, _) => true
            | _ => false
          fun externResultSupported ty =
            case Types.prune ty of
              Types.TInt 32 => true
            | Types.TInt 64 => true
            | Types.TUnit => true
            | Types.TCPtr => true
            | _ => false
          fun checkExternSignature
                (name, params, paramTypes, result, resultType, resultSpan) =
            let
              val pairs =
                ListPair.zip (params, paramTypes)
                handle UnequalLengths => []
              val _ =
                List.app
                  (fn (param, ty) =>
                    if externParameterSupported ty then ()
                    else
                      report
                        "E3045"
                        ("extern parameter `" ^ #name param ^
                         "` has an unsupported type")
                        (#span param)
                        [ "type: " ^ Types.toString ty ]
                        [ "extern accepts `int`, `int64`, `borrow string` and " ^
                          "`borrow int8 array` parameters" ])
                  pairs
            in
              if not (Option.isSome result) then
                report
                  "E3046"
                  "extern needs an explicit result type"
                  resultSpan
                  []
                  ["write `: int`, `: int64` or `: unit`"]
              else if not (externResultSupported resultType) then
                report
                  "E3046"
                  ("extern `" ^ name ^
                   "` has an unsupported result type")
                  resultSpan
                  [ "type: " ^ Types.toString resultType ]
                  ["extern returns `int`, `int64` or `unit`; fill a buffer for anything else"]
              else ()
            end
          fun step (declaration, functions) =
            case declaration of
              Ast.DType _ => functions
            | Ast.DAlias _ => functions
            | Ast.DDatatype _ => functions
            | Ast.DImport _ => functions
            | Ast.DExternRecord _ => functions
            | Ast.DFunClause _ =>
                raise Fail "check: desugared function clause"
            | Ast.DFun {name, params, result, span, body, ...} =>
                if List.exists (fn builtin => builtin = name) builtins then
                  ( report
                      "E3011"
                      ("builtin `" ^ name ^ "` cannot be redefined")
                      span
                      []
                      []
                  ; skipBodies := name :: !skipBodies
                  ; functions
                  )
                else if List.exists (fn (existing, _) => existing = name) functions then
                  ( report
                      "E3009"
                      ("duplicate function `" ^ name ^ "`")
                      span
                      []
                      []
                  ; skipBodies := name :: !skipBodies
                  ; functions
                  )
                else if List.exists
                          (fn (constructor, _, _) => constructor = name)
                          (!constructorEnvironment) then
                  ( report
                      "E3025"
                      ("name `" ^ name ^
                       "` is already used by a constructor")
                      span
                      []
                      []
                  ; skipBodies := name :: !skipBodies
                  ; functions
                  )
                else
                  let
                    val _ = currentTypeVars := []
                    val _ = allowTypeVars := true
                    val savedParameters = !currentParameters
                    val _ = currentParameters :=
                      let
                        fun loop (index, remaining, acc) =
                          case remaining of
                            [] => List.rev acc
                          | param :: rest =>
                              loop
                                ( index + 1
                                , rest
                                , (#name param, index) :: acc
                                )
                      in
                        loop (1, params, [])
                      end
                    val paramSpans =
                      List.map (fn param => #span param) params
                    val paramTypes =
                      List.map
                        (fn param =>
                          case #annot param of
                            SOME syntaxType => resolveSyntaxType (#span param) syntaxType
                          | NONE => Types.fresh context)
                        params
                    val resultType =
                      case result of
                        SOME syntaxType => resolveSyntaxType span syntaxType
                      | NONE => Types.fresh context
                    fun borrowParameterIndices types =
                      let
                        fun loop (index, remaining, acc) =
                          case remaining of
                            [] => List.rev acc
                          | ty :: rest =>
                              (case Types.prune ty of
                                 Types.TBorrow _ =>
                                   loop (index + 1, rest, index :: acc)
                               | _ => loop (index + 1, rest, acc))
                      in
                        loop (1, types, [])
                      end
                    val resultType =
                      case Types.prune resultType of
                        Types.TBorrow (inner, mutable, NONE) =>
                          (case borrowParameterIndices paramTypes of
                             [index] =>
                               Types.TBorrow (inner, mutable, SOME index)
                           | _ => resultType)
                      | _ => resultType
                    val _ =
                      if isExternBody body then
                        checkExternSignature
                          ( name
                          , params
                          , paramTypes
                          , result
                          , resultType
                          , span
                          )
                      else ()
                    val _ = currentParameters := savedParameters
                    val _ = recordBindingTypes (paramSpans, paramTypes)
                    val typeVars = !currentTypeVars
                    val signatureInfo =
                      { name = name
                      , params = paramTypes
                      , result = resultType
                      , span = span
                      , arity = List.length params
                      , paramSpans = paramSpans
                      , typeVars = typeVars
                      , generalized = ref []
                      }
                  in
                    (name, signatureInfo) :: functions
                  end
        in
          List.foldl step (CheckBuiltins.functions optionOf ioResultOf) program
        end

      val _ = allowTypeVars := false
      val _ = currentTypeVars := []
      val _ = buildTypeDeclarations program
      val functions = buildEnvironment ()
      val _ = allowTypeVars := false
      val _ = currentTypeVars := []

      fun unifyArguments name params arguments span =
        let
          fun loop (index, paramTypes, argumentTypes) =
            case (paramTypes, argumentTypes) of
              ([], []) => ()
            | (paramType :: restParams, argumentType :: restArguments) =>
                ( if Types.unify paramType argumentType then ()
                  else
                    mismatchWith
                      ("type mismatch in argument " ^ Int.toString index ^
                       " of `" ^ name ^ "`")
                      span
                      paramType
                      argumentType
                      (argumentHints name paramType argumentType)
                ; loop (index + 1, restParams, restArguments)
                )
            | _ => ()
        in
          loop (1, params, arguments)
        end

      fun checkMapValue name params span =
        case (name, params) of
          ("map_get", Types.TMap value :: _) =>
            if Types.propertyOf value = Types.Copy then ()
            else
              report
                "E3044"
                "`map_get` cannot copy an Affine value"
                span
                [ "value type: " ^ Types.toString value ]
                ["use `map_get_borrow` to read the value without taking ownership"]
        | ("map_get_copy_borrow",
           Types.TBorrow (Types.TMap value, _, _) :: _) =>
            if Types.propertyOf value = Types.Copy then ()
            else
              report
                "E3044"
                "`map_get_copy_borrow` cannot copy an Affine value"
                span
                [ "value type: " ^ Types.toString value ]
                ["use `map_get_borrow_view` to read the value without taking ownership"]
        | _ => ()

      fun arrayElementType name params =
        case (name, params) of
          ("array_make", element :: _) => SOME element
        | ("array_get", Types.TArray element :: _) => SOME element
        | ("array_get_copy_borrow",
           Types.TBorrow (Types.TArray element, _, _) :: _) => SOME element
        | ("array_kernel_load",
           Types.TBorrow (Types.TArray element, _, _) :: _) => SOME element
        | ("array_get_unchecked", Types.TArray element :: _) => SOME element
        | ("array_set", Types.TArray element :: _) => SOME element
        | ("array_set_unchecked", Types.TArray element :: _) => SOME element
        | ("array_push", Types.TArray element :: _) => SOME element
        | ("array_push_unchecked", Types.TArray element :: _) => SOME element
        | ("array_put_arena", Types.TArray element :: _) => SOME element
        | _ => NONE

      fun checkArrayElement name params span =
        case arrayElementType name params of
          NONE => ()
        | SOME element =>
            if Types.propertyOf element = Types.Copy then ()
            else if name = "array_make" then
              report
                "E3040"
                "array_make requires Copy elements"
                span
                [ "element type: " ^ Types.toString element ]
                ["build an Affine array with `array_new ()` and `array_push`"]
            else if name = "array_get" orelse
                    name = "array_get_unchecked" orelse
                    name = "array_kernel_load" orelse
                    name = "array_get_copy_borrow" then
              report
                "E3041"
                ("`" ^ name ^ "` cannot copy an Affine element")
                span
                [ "element type: " ^ Types.toString element ]
                ["use `array_get_borrow` to read the element without taking ownership"]
            else ()

      fun integerOperand span ty label =
        case Types.prune ty of
          Types.TInt width => (SOME width, false)
        | Types.TIntLit _ => (SOME 32, true)
        | Types.TVar _ => (NONE, false)
        | _ =>
            (mismatch ("type mismatch in " ^ label) span
               (Types.TInt 32) ty;
             (SOME 32, false))

      fun promoteIntegers span leftType rightType =
        let
          val (leftWidth, leftLiteral) =
            integerOperand span leftType "the left operand"
          val (rightWidth, rightLiteral) =
            integerOperand span rightType "the right operand"
        in
          case (leftWidth, rightWidth) of
            (SOME width1, SOME width2) =>
              let
                val width = Int.max (width1, width2)
                val _ =
                  if leftLiteral then
                    Types.bindIntLiteralWidth leftType width
                  else ()
                val _ =
                  if rightLiteral then
                    Types.bindIntLiteralWidth rightType width
                  else ()
              in
                Types.TInt width
              end
          | (SOME width, NONE) =>
              let val _ = Types.unify rightType (Types.TInt width)
              in Types.TInt width end
          | (NONE, SOME width) =>
              let val _ = Types.unify leftType (Types.TInt width)
              in Types.TInt width end
          | (NONE, NONE) =>
              let
                val _ = Types.unify leftType (Types.TInt 32)
                val _ = Types.unify rightType (Types.TInt 32)
              in
                Types.TInt 32
              end
        end

      fun promoteNumbers span leftType rightType =
        let
          datatype numberKind =
              NInt of int
            | NIntLit
            | NFloat of int
            | NFloatLit
            | NNum
            | NNot
          fun kindOf ty =
            case Types.prune ty of
              Types.TInt width => NInt width
            | Types.TIntLit _ => NIntLit
            | Types.TFloat width => NFloat width
            | Types.TFloatLit _ => NFloatLit
            | Types.TNumVar _ => NNum
            | Types.TVar _ => NNum
            | _ => NNot
          val leftOk = Types.constrainNumeric context leftType
          val rightOk = Types.constrainNumeric context rightType
          val _ =
            if leftOk then ()
            else mismatch "type mismatch in the left operand" span (Types.TInt 32) leftType
          val _ =
            if rightOk then ()
            else mismatch "type mismatch in the right operand" span (Types.TInt 32) rightType
          val leftKind = kindOf leftType
          val rightKind = kindOf rightType
          fun bindLiteral ty result =
            case Types.prune ty of
              Types.TIntLit _ => ignore (Types.unify ty result)
            | Types.TFloatLit _ => ignore (Types.unify ty result)
            | _ => ()
          fun concreteResult () =
            let
              val hasFloat =
                (case leftKind of NFloat _ => true | NFloatLit => true | _ => false) orelse
                (case rightKind of NFloat _ => true | NFloatLit => true | _ => false)
              val leftFloatWidth =
                case leftKind of NFloat width => width | NFloatLit => 64 | _ => 0
              val rightFloatWidth =
                case rightKind of NFloat width => width | NFloatLit => 64 | _ => 0
              val leftIntWidth =
                case leftKind of NInt width => width | NIntLit => 32 | _ => 0
              val rightIntWidth =
                case rightKind of NInt width => width | NIntLit => 32 | _ => 0
            in
              if hasFloat then
                let
                  val width = Int.max (leftFloatWidth, rightFloatWidth)
                  val result = Types.TFloat width
                  val _ = bindLiteral leftType result
                  val _ = bindLiteral rightType result
                in
                  result
                end
              else
                let
                  val width = Int.max (leftIntWidth, rightIntWidth)
                  val result = Types.TInt width
                  val _ = bindLiteral leftType result
                  val _ = bindLiteral rightType result
                in
                  result
                end
            end
        in
          case (leftKind, rightKind) of
            (NNum, NNum) =>
              (ignore (Types.unify leftType rightType); leftType)
          | (NNum, NInt width) =>
              (ignore (Types.unify leftType (Types.TInt width)); Types.TInt width)
          | (NNum, NFloat width) =>
              (ignore (Types.unify leftType (Types.TFloat width)); Types.TFloat width)
          | (NInt width, NNum) =>
              (ignore (Types.unify rightType (Types.TInt width)); Types.TInt width)
          | (NFloat width, NNum) =>
              (ignore (Types.unify rightType (Types.TFloat width)); Types.TFloat width)
          | (NNum, NIntLit) => leftType
          | (NNum, NFloatLit) => leftType
          | (NIntLit, NNum) => rightType
          | (NFloatLit, NNum) => rightType
          | (NNot, _) => Types.TInt 32
          | (_, NNot) => Types.TInt 32
          | _ => concreteResult ()
        end

      fun peelBorrow ty =
        case Types.prune ty of
          Types.TBorrow (inner, mutable, provenance) =>
            (inner, SOME (mutable, provenance))
        | _ => (ty, NONE)

      fun borrowifyPatternType (mutable, provenance) ty =
        case Types.prune ty of
          Types.TBorrow _ => ty
        | Types.TConsume inner =>
            borrowifyPatternType (mutable, provenance) inner
        | Types.TTuple types =>
            Types.TTuple
              (List.map
                 (borrowifyPatternType (mutable, provenance))
                 types)
        | Types.TRecord fields =>
            Types.TRecord
              (List.map
                 (fn (name, fieldTy) =>
                    (name, borrowifyPatternType (mutable, provenance) fieldTy))
                 fields)
        | Types.TCon { name, property, fields } =>
            Types.TCon
              { name = name
              , property = property
              , fields =
                  List.map
                    (fn (fieldName, fieldTy) =>
                       ( fieldName
                       , borrowifyPatternType (mutable, provenance) fieldTy
                       ))
                    fields
              }
        | Types.TString =>
            Types.TBorrow (Types.TString, mutable, provenance)
        | Types.TArray _ =>
            Types.TBorrow (ty, mutable, provenance)
        | Types.TMap _ =>
            Types.TBorrow (ty, mutable, provenance)
        | Types.TData _ =>
            Types.TBorrow (ty, mutable, provenance)
        | Types.TDataApp _ =>
            Types.TBorrow (ty, mutable, provenance)
        | _ =>
            if Types.propertyOf ty = Types.Copy then ty
            else Types.TBorrow (ty, mutable, provenance)

      fun checkPattern (seen, locals, pattern, expected) =
        case pattern of
          Ast.PVar (name, span) =>
            let
              val _ =
                if StringSet.insert (seen, name) then ()
                else
                  report
                    "E3020"
                    ("duplicate pattern variable `" ^ name ^ "`")
                    span
                    []
                    []
              val _ = recordBindingType span expected
            in
              (name, expected) :: locals
            end
        | Ast.PWildcard _ => locals
        | Ast.PInt (value, span) =>
            let
              val (innerExpected, _) = peelBorrow expected
              val literal = Types.intLiteral context value
            in
              if Types.unify innerExpected literal then ()
              else
                mismatch
                  "integer pattern does not match the scrutinee type"
                  span
                  innerExpected
                  literal;
              locals
            end
        | Ast.PBool (value, span) =>
            let
              val (innerExpected, _) = peelBorrow expected
              val _ =
                if Types.unify innerExpected Types.TBool then ()
                else
                  mismatch
                    "boolean pattern does not match the scrutinee type"
                    span
                    Types.TBool
                    innerExpected
            in
              locals
            end
        | Ast.PString (_, rest, span) =>
            let
              val (innerExpected, _) = peelBorrow expected
              val _ =
                if Types.unify innerExpected Types.TString then ()
                else
                  mismatch
                    "string pattern does not match the scrutinee type"
                    span
                    Types.TString
                    innerExpected
            in
              case rest of
                NONE => locals
              | SOME subpattern =>

                  checkPattern (seen, locals, subpattern, Types.TString)
            end
        | Ast.PBytes (_, rest, span) =>
            let
              val (innerExpected, _) = peelBorrow expected
              val bytesType = Types.TArray (Types.TInt 8)
              val _ =
                if Types.unify innerExpected bytesType then ()
                else
                  mismatch
                    "bit array pattern does not match the scrutinee type"
                    span
                    bytesType
                    innerExpected
            in
              case rest of
                NONE => locals
              | SOME subpattern =>
                  checkPattern (seen, locals, subpattern, bytesType)
            end
        | Ast.PConstructor (name, argument, span) =>
            (case lookupConstructor name of
               NONE =>
                 ( report
                     "E3027"
                     ("unknown constructor `" ^ name ^ "`")
                     span
                     []
                     []
                 ; locals
                 )
             | SOME (_, resultType0, argumentType0) =>
                 let
                   val (innerExpected, borrowInfo) = peelBorrow expected
                   val (resultType, argumentType) =
                     instantiateConstructor (resultType0, argumentType0)
                   val _ =
                     if Types.unify innerExpected resultType then ()
                     else
                       mismatch
                         ("constructor `" ^ name ^
                          "` does not match the scrutinee type")
                         span
                         resultType
                         innerExpected
                   val argumentType =
                     case (borrowInfo, argumentType) of
                       (SOME (mutable, provenance), SOME argType) =>
                         SOME (borrowifyPatternType (mutable, provenance) argType)
                     | _ => argumentType
                 in
                   case (argumentType, argument) of
                     (NONE, NONE) => locals
                   | (SOME argType, SOME subpattern) =>
                       checkPattern (seen, locals, subpattern, argType)
                   | (NONE, SOME _) =>
                       ( report
                           "E3026"
                           ("constructor `" ^ name ^ "` expects no argument")
                           span
                           []
                           []
                       ; locals
                       )
                   | (SOME _, NONE) =>
                       ( report
                           "E3026"
                           ("constructor `" ^ name ^ "` expects one argument")
                           span
                           []
                           []
                       ; locals
                       )
                 end)
        | Ast.PRecord (fields, span) =>
            let
              val (innerExpected, borrowInfo) = peelBorrow expected
              val expected' = Types.prune innerExpected
              fun recordFields () =
                case expected' of
                  Types.TCon {fields = available, ...} => SOME available
                | Types.TRecord available => SOME available
                | Types.TVar _ =>
                    let
                      val freshFields =
                        sortFields
                          (List.map
                             (fn (name, _) => (name, Types.fresh context))
                             fields)
                      val _ =
                        Types.unify expected' (Types.TRecord freshFields)
                    in
                      SOME freshFields
                    end
                | _ => NONE
              val seenFields = StringSet.create 32
              fun checkField ((name, subpattern), locals) =
                let
                  val _ =
                    if StringSet.insert (seenFields, name) then ()
                    else
                      report
                        "E3018"
                        ("duplicate record pattern field `" ^ name ^ "`")
                        span
                        []
                        []
                in
                  case recordFields () of
                    NONE =>
                      ( report
                          "E3014"
                          "record pattern on a non-record type"
                          span
                          [ "type: " ^ Types.toString expected' ]
                          []
                      ; locals
                      )
                  | SOME available =>
                      (case List.find
                              (fn (candidate, _) => candidate = name)
                              available of
                         SOME (_, ty) =>
                           let
                             val ty =
                               case borrowInfo of
                                 SOME (mutable, provenance) =>
                                   borrowifyPatternType
                                     (mutable, provenance) ty
                               | NONE => ty
                           in
                             checkPattern (seen, locals, subpattern, ty)
                           end
                       | NONE =>
                           ( report
                               "E3014"
                               ("record has no field `" ^ name ^ "`")
                               span
                               [ "available fields: " ^
                                 String.concatWith ", "
                                   (List.map (fn (candidate, _) => candidate) available)
                               ]
                               []
                           ; locals
                           ))
                end
            in
              List.foldl checkField locals fields
            end

        | Ast.PTuple (patterns, span) =>
            let
              val (innerExpected, borrowInfo) = peelBorrow expected
              val expected' = Types.prune innerExpected
            in
              case expected' of
                Types.TTuple types =>
                  let
                    val types =
                      case borrowInfo of
                        SOME (mutable, provenance) =>
                          List.map
                            (borrowifyPatternType (mutable, provenance))
                            types
                      | NONE => types
                  in
                    if List.length types = List.length patterns then
                      ListPair.foldl
                        (fn (pattern, ty, locals) =>
                          checkPattern (seen, locals, pattern, ty))
                        locals
                        (patterns, types)
                    else
                      ( report
                          "E3019"
                          "tuple pattern size does not match the scrutinee type"
                          span
                          [ "expected: " ^ Int.toString (List.length types)
                          , "found: " ^ Int.toString (List.length patterns)
                          ]
                          []
                      ; locals
                      )
                  end
              | Types.TVar _ =>
                  let
                    val freshTypes =
                      List.map (fn _ => Types.fresh context) patterns
                    val _ = Types.unify expected' (Types.TTuple freshTypes)
                  in
                    ListPair.foldl
                      (fn (pattern, ty, locals) =>
                        checkPattern (seen, locals, pattern, ty))
                      locals
                      (patterns, freshTypes)
                  end
              | _ =>
                  ( report
                      "E3019"
                      "tuple pattern on a non-tuple type"
                      span
                      [ "type: " ^ Types.toString expected' ]
                      []
                  ; locals
                  )
            end

      fun checkCaseCoverage (span, scrutineeType, branches) =
        CheckCoverage.check (report, span, scrutineeType, branches)

      val checkLambdaBorrow =
        ref (fn (_ : string * Ast.param list * Types.ty list *
                   Types.ty * Ast.expr) => ())

      fun infer locals expression =
        let
          val ty = inferExpression locals expression
          val _ = recordExpressionType (Ast.exprSpan expression) ty
        in
          ty
        end

      and inferExpression locals expression =
        case expression of
          Ast.EInt (value, _) => Types.intLiteral context value
        | Ast.EFloat (value, _) => Types.floatLiteral context value

        | Ast.EString _ => Types.TString

        | Ast.EBool _ => Types.TBool

        | Ast.EUnit _ => Types.TUnit

        | Ast.EArena _ =>
            raise Fail "check: desugared arena expression"

        | Ast.EVar (name, span) =>
            (case List.find (fn (candidate, _) => candidate = name) locals of
               SOME (_, ty) => ty
             | NONE =>
                 (case lookupConstructor name of
                    SOME (_, resultType0, argumentType0) =>
                      let
                        val (resultType, argumentType) =
                          instantiateConstructor (resultType0, argumentType0)
                      in
                        case argumentType of
                          NONE => resultType
                        | SOME _ =>
                            ( report
                                "E3026"
                                ("constructor `" ^ name ^
                                 "` expects an argument")
                                span
                                []
                                ["write `" ^ name ^ " (value)`"]
                            ; resultType
                            )
                      end
                  | NONE =>
                      (case lookup name functions of
                         SOME (_, {params = signatureParams, result = signatureResult, generalized, ...}) =>
                           let
                             val substitution =
                               Types.makeSubstitution context (!generalized)
                             val params =
                               List.map
                                 (Types.instantiateWith substitution)
                                 signatureParams
                             val result =
                               Types.instantiateWith substitution
                                 signatureResult
                             val _ = recordFunctionValue span name substitution
                           in
                             namedFunctionValueType params result
                           end
                       | NONE =>
                           ( report
                               "E3001"
                               ("unknown variable `" ^ name ^ "`")
                               span
                               []
                               []
                           ; Types.fresh context
                           ))))

        | Ast.ECase (scrutinee, branches, span) =>
            let
              val scrutineeType = infer locals scrutinee
              val resultType = Types.fresh context
              val _ =
                List.app
                  (fn (pattern, guard, body) =>
                    let
                      val seen = StringSet.create 64
                      val branchLocals =
                        checkPattern (seen, locals, pattern, scrutineeType)
                      val _ =
                        case guard of
                          NONE => ()
                        | SOME condition =>
                            let val guardType = infer branchLocals condition in
                              if Types.unify guardType Types.TBool then ()
                              else
                                mismatch
                                  "a case guard must have type bool"
                                  (Ast.exprSpan condition)
                                  Types.TBool
                                  guardType
                            end
                      val bodyType = infer branchLocals body
                      val sameConvention =
                        Types.sameConsumeConventions resultType bodyType
                      val _ =
                        if Types.unify resultType bodyType then
                          ( if sameConvention then ()
                            else
                              conventionMismatch
                                "all case branches must use the same ownership convention"
                                span
                                resultType
                                bodyType
                          )
                        else
                          report
                            "E3004"
                            "all case branches must have the same type"
                            span
                            [ "branch: " ^ Types.toString bodyType
                            , "expected: " ^ Types.toString resultType
                            ]
                            []
                    in
                      ()
                    end)
                  branches
              val _ = checkCaseCoverage (span, scrutineeType, branches)
            in
              resultType
            end

        | Ast.ERecord (fields, _) =>
            let
              val seen = StringSet.create 64
              val fieldTypes =
                List.map
                  (fn (name, value) =>
                    let
                      val _ =
                        if StringSet.insert (seen, name) then ()
                        else
                          report
                            "E3018"
                            ("duplicate record field `" ^ name ^ "`")
                            (Ast.exprSpan value)
                            []
                            []
                      val ty = infer locals value
                    in
                      (name, ty)
                    end)
                  fields
            in
              Types.TRecord (sortFields fieldTypes)
            end

        | Ast.ETuple (values, _) =>
            Types.TTuple (List.map (infer locals) values)

        | Ast.ETupleIndex (index, base, span) =>
            let
              val baseType = infer locals base
            in
              case Types.prune baseType of
                Types.TTuple fields =>
                  if index >= 0 andalso index < List.length fields then
                    List.nth (fields, index)
                  else
                    ( report
                        "E3019"
                        ("tuple index " ^ Int.toString index ^ " is out of bounds")
                        span
                        [ "tuple size: " ^ Int.toString (List.length fields) ]
                        []
                    ; Types.fresh context
                    )
              | Types.TVar _ =>
                  ( report
                      "E3017"
                      "cannot infer the tuple type of this index access"
                      span
                      []
                      ["add a type annotation to the tuple value"]
                  ; Types.fresh context
                  )
              | _ =>
                  ( report
                      "E3019"
                      "tuple index on a non-tuple type"
                      span
                      [ "type: " ^ Types.toString baseType ]
                      []
                  ; Types.fresh context
                  )
            end

        | Ast.EField (field, base, span) =>
            let
              val baseType = infer locals base
              fun lookupField fields =
                case List.find
                       (fn (candidate, _) => candidate = field)
                       fields of
                  SOME (_, ty) => ty
                | NONE =>
                    ( report
                        "E3014"
                        ("record has no field `" ^ field ^ "`")
                        span
                        [ "available fields: " ^
                          String.concatWith ", "
                            (List.map (fn (name, _) => name) fields)
                        ]
                        []
                    ; Types.fresh context
                    )
              fun lookupBorrowedField fields =
                let val fieldType = lookupField fields in
                  if Types.propertyOf fieldType = Types.Copy then fieldType
                  else
                    ( report
                        "E0508"
                        ("cannot read non-Copy field `" ^ field ^
                         "` through a borrow")
                        span
                        [ "field type: " ^ Types.toString fieldType ]
                        [ "copy the value out through an explicit accessor"
                        , "or access the field through an owned value"
                        ]
                    ; Types.fresh context
                    )
                end
              fun lookupDatatypeField name common =
                case common of
                  SOME ty => ty
                | NONE =>
                    ( report
                        "E3014"
                        ("datatype `" ^ name ^
                         "` has no field `" ^ field ^
                         "` common to every constructor")
                        span
                        [ "type: " ^ Types.toString baseType ]
                        [ "a field is projectable only when every constructor carries a record with that field"
                        , "match the value with `case` before accessing the field"
                        ]
                    ; Types.fresh context
                    )
            in
              case Types.prune baseType of
                Types.TCon {fields, ...} => lookupField fields
              | Types.TRecord fields => lookupField fields
              | Types.TData (data as Types.Data {name, ...}) =>
                  lookupDatatypeField name
                    (Types.dataCommonField field data [])
              | Types.TDataApp (data as Types.Data {name, ...}, arguments) =>
                  lookupDatatypeField name
                    (Types.dataCommonField field data arguments)
              | Types.TBorrow (inner, _, _) =>
                  (case Types.prune inner of
                     Types.TCon {fields, ...} => lookupBorrowedField fields
                   | Types.TRecord fields => lookupBorrowedField fields
                   | _ =>
                       ( report
                           "E3014"
                           "field access on a non-record borrow"
                           span
                           [ "type: " ^ Types.toString baseType ]
                           []
                       ; Types.fresh context
                       ))
              | Types.TVar _ =>
                  ( report
                      "E3017"
                      "cannot infer the record type of this field access"
                      span
                      []
                      ["add a type annotation to the record value"]
                  ; Types.fresh context
                  )
              | _ =>
                  ( report
                      "E3014"
                      "field access on a non-record type"
                      span
                      [ "type: " ^ Types.toString baseType ]
                      []
                  ; Types.fresh context
                  )
            end

        | Ast.EBorrow (mutable, _, operand, span) =>
            let
              fun isStringLiteral (Ast.EString _) = true
                | isStringLiteral _ = false
              fun isPlace (Ast.EVar (name, _)) =
                    List.exists
                      (fn (candidate, _) => candidate = name)
                      locals
                | isPlace (Ast.EField (_, base, _)) = isPlace base
                | isPlace (Ast.ETupleIndex (_, base, _)) = isPlace base

                | isPlace (Ast.EString _) = not mutable
                | isPlace _ = false
              fun placeText (Ast.EVar (name, _)) = name
                | placeText (Ast.EField (field, base, _)) =
                    placeText base ^ "." ^ field
                | placeText (Ast.ETupleIndex (index, base, _)) =
                    placeText base ^ "." ^ Int.toString index
                | placeText _ = ""
              fun describe operand =
                case placeText operand of
                  "" => "this expression"
                | text => "`" ^ text ^ "`"
            in
              if isPlace operand then
                let
                  val operandType = infer locals operand
                  val _ =
                    if mutable then
                      case Types.prune operandType of
                        Types.TData _ =>
                          report
                            "E0516"
                            "mutable borrow of a container is not supported yet"
                            span
                            [ "shared `borrow` is supported for reads" ]
                            [ "use `borrow` for a read-only traversal" ]
                      | Types.TDataApp _ =>
                          report
                            "E0516"
                            "mutable borrow of a container is not supported yet"
                            span
                            [ "shared `borrow` is supported for reads" ]
                            [ "use `borrow` for a read-only traversal" ]
                      | Types.TArray _ =>
                          report
                            "E0516"
                            "mutable borrow of a container is not supported yet"
                            span
                            [ "shared `borrow` is supported for reads" ]
                            [ "use `borrow` for a read-only traversal" ]
                      | Types.TMap _ =>
                          report
                            "E0516"
                            "mutable borrow of a container is not supported yet"
                            span
                            [ "shared `borrow` is supported for reads" ]
                            [ "use `borrow` for a read-only traversal" ]
                      | _ => ()
                    else ()
                in
                  Types.TBorrow (operandType, mutable, NONE)
                end
              else if isStringLiteral operand then
                ( report
                    "E0500"
                    "cannot mutably borrow a string literal: string literals are immutable"
                    span
                    []
                    ["use a shared borrow: `borrow \"literal\"` or `&\"literal\"`"]
                ; Types.TBorrow (Types.TString, false, NONE)
                )
              else
                ( report
                    "E0500"
                    ("cannot borrow " ^ describe operand ^
                     ": only a local variable or a field of one can be borrowed")
                    span
                    []
                    ["borrow a local place, for example `borrow user` or `borrow user.name`"]
                ; Types.fresh context
                )
            end

        | Ast.EFn (params, resultAnnotation, body, span) =>
            let
              val seen = StringSet.create 16
              val paramTypes =
                List.map
                  (fn param =>
                    let
                      val _ =
                        if StringSet.insert (seen, #name param) then ()
                        else
                          report
                            "E3008"
                            ("duplicate parameter `" ^ #name param ^ "`")
                            (#span param)
                            []
                            []
                    in
                      case #annot param of
                        SOME syntaxType =>
                          resolveSyntaxType (#span param) syntaxType
                      | NONE => Types.fresh context
                    end)
                  params
              val _ =
                recordBindingTypes
                  (List.map (fn param => #span param) params, paramTypes)
              val extended =
                List.foldl
                  (fn ((param, ty), locals) =>
                    (#name param, ty) :: locals)
                  locals
                  (ListPair.zip (params, paramTypes))
              val bodyType = infer extended body
              val resultType =
                case resultAnnotation of
                  NONE => bodyType
                | SOME syntaxType =>
                    let
                      val annotated = resolveSyntaxType span syntaxType
                      val sameConvention =
                        Types.sameConsumeConventions annotated bodyType
                    in
                      if Types.unify annotated bodyType then
                        ( if sameConvention then ()
                          else
                            conventionMismatch
                              "ownership convention mismatch in the function result"
                              span
                              annotated
                              bodyType
                        ; annotated
                        )
                      else
                        ( mismatch
                            "function result annotation does not match its body"
                            span
                            annotated
                            bodyType
                        ; annotated
                        )
                    end
              val _ =
                (!checkLambdaBorrow)
                  ("<lambda>", params, paramTypes, resultType, body)
            in
              Types.TFun (paramTypes, resultType)
            end

        | Ast.EExtern {result, span, ...} =>
            resolveSyntaxType span result

        | Ast.ECall (name, arguments, span) =>
            (case List.find (fn (candidate, _) => candidate = name) locals of
               SOME (_, localType) =>
                 let
                   val argumentTypes =
                     List.map (infer locals) arguments
                   val resultType = Types.fresh context
                   val expected =
                     Types.TFun
                       (List.map (fn _ => Types.fresh context) argumentTypes,
                        resultType)
                 in
                   if Types.unify localType expected then ()
                   else
                     mismatch
                       ("local value `" ^ name ^
                        "` is not a function of " ^
                        Int.toString (List.length argumentTypes) ^
                        " argument(s)")
                       span
                       expected
                       localType;
                   unifyArguments name
                     (case Types.prune expected of
                        Types.TFun (params, _) => params
                      | _ => [])
                     argumentTypes
                     span;
                   applications := (span, Types.prune localType) :: !applications;
                   resultType
                 end
             | NONE =>
                 (case lookupConstructor name of
                    SOME (_, resultType0, argumentType0) =>
                      let
                        val (resultType, argumentType) =
                          instantiateConstructor (resultType0, argumentType0)
                        val argumentTypes =
                          List.map (infer locals) arguments
                        val _ =
                          case (argumentType, argumentTypes) of
                            (NONE, []) => ()
                          | (SOME expected, [actual]) =>
                              if Types.unify expected actual then ()
                              else
                                mismatch
                                  ("type mismatch in constructor `" ^ name ^ "`")
                                  span
                                  expected
                                  actual
                          | (NONE, _) =>
                              report
                                "E3026"
                                ("constructor `" ^ name ^
                                 "` expects no argument")
                                span
                                []
                                []
                          | (SOME _, _) =>
                              report
                                "E3026"
                                ("constructor `" ^ name ^
                                 "` expects one argument")
                                span
                                []
                                []
                      in
                        resultType
                      end
                  | NONE =>
                      (case lookup name functions of
                         NONE =>
                           ( List.app
                               (fn argument => ignore (infer locals argument))
                               arguments
                           ; report
                               "E3002"
                               ("unknown function `" ^ name ^ "`")
                               span
                               []
                               []
                           ; Types.fresh context
                           )
                       | SOME (_, {params = signatureParams, result = signatureResult, arity, generalized, ...}) =>
                           let
                             val substitution =
                               Types.makeSubstitution context (!generalized)
                             val params =
                               List.map
                                 (Types.instantiateWith substitution)
                                 signatureParams
                             val result =
                               Types.instantiateWith substitution
                                 signatureResult
                             val argumentTypes =
                               List.map (infer locals) arguments
                             val _ =
                               if List.length argumentTypes = arity then ()
                               else
                                 report
                                   "E3003"
                                   ("function `" ^ name ^ "` expects " ^
                                    Int.toString arity ^
                                    " argument(s), but got " ^
                                    Int.toString (List.length argumentTypes))
                                   span
                                   []
                                   []
                             val _ =
                               unifyArguments name params argumentTypes span
                             val _ =
                               checkArrayElement name params span
                             val _ =
                               checkMapValue name params span
                             val _ =
                               recordCall span name substitution params result
                           in
                             result
                           end)))

        | Ast.EBin (operator, left, right, span) =>
            let
              val leftType = infer locals left
              val rightType = infer locals right

              fun checkEquality () =
                let
                  fun isInteger ty =
                    case Types.prune ty of
                      Types.TInt _ => true
                    | Types.TIntLit _ => true
                    | _ => false
                  val _ =
                    if isInteger leftType orelse isInteger rightType then
                      let val _ = promoteIntegers span leftType rightType
                      in () end
                    else if Types.unify leftType rightType then
                      if Types.constrainEquality context leftType then ()
                      else
                        report
                          "E3004"
                          "equality is only supported on equality types"
                          span
                          [ "type: " ^ Types.toString leftType ]
                          []
                    else
                      report
                        "E3004"
                        "both sides of an equality must have the same type"
                        span
                        [ "left: " ^ Types.toString leftType
                        , "right: " ^ Types.toString rightType
                        ]
                        []
                in
                  ()
                end
            in
              case operator of
                "^" =>
                  let
                    val _ =
                      if Types.unify leftType Types.TString then ()
                      else
                        mismatch
                          "the left operand of `^` must be a string"
                          span
                          Types.TString
                          leftType
                    val _ =
                      if Types.unify rightType Types.TString then ()
                      else
                        mismatch
                          "the right operand of `^` must be a string"
                          span
                          Types.TString
                          rightType
                  in
                    Types.TString
                  end
              | "+" => promoteNumbers span leftType rightType
              | "-" => promoteNumbers span leftType rightType
              | "*" => promoteNumbers span leftType rightType
              | "/" => promoteNumbers span leftType rightType
              | "%" => promoteIntegers span leftType rightType
              | "<" =>
                  (promoteNumbers span leftType rightType; Types.TBool)
              | "<=" =>
                  (promoteNumbers span leftType rightType; Types.TBool)
              | ">" =>
                  (promoteNumbers span leftType rightType; Types.TBool)
              | ">=" =>
                  (promoteNumbers span leftType rightType; Types.TBool)
              | "=" =>
                  let val _ = checkEquality () in Types.TBool end
              | "<>" =>
                  let val _ = checkEquality () in Types.TBool end
              | _ =>
                  ( report
                      "E2002"
                      ("unknown operator `" ^ operator ^ "`")
                      span
                      []
                      []
                  ; Types.fresh context
                  )
            end

        | Ast.ENeg (operand, span) =>
            let
              val operandType = infer locals operand
              val (width, isLiteral) =
                integerOperand span operandType "the operand of unary `-`"
            in
              case width of
                SOME width =>
                  if isLiteral then operandType else Types.TInt width
              | NONE =>
                  let val _ = Types.unify operandType (Types.TInt 32)
                  in Types.TInt 32 end
            end

        | Ast.EIf (condition, thenBranch, elseBranch, span) =>
            let
              val conditionType = infer locals condition
              val _ =
                if Types.unify conditionType Types.TBool then ()
                else mismatch
                  "the condition of `if` must be bool"
                  (Ast.exprSpan condition)
                  Types.TBool
                  conditionType
              val thenType = infer locals thenBranch
              val elseType = infer locals elseBranch
              val sameConvention =
                Types.sameConsumeConventions thenType elseType
              val _ =
                if Types.unify thenType elseType then
                  ( if sameConvention then ()
                    else
                      conventionMismatch
                        "both branches of `if` must use the same ownership convention"
                        span
                        thenType
                        elseType
                  )
                else
                  report
                    "E3004"
                    "both branches of `if` must have the same type"
                    span
                    [ "then: " ^ Types.toString thenType
                    , "else: " ^ Types.toString elseType
                    ]
                    []
            in
              thenType
            end

        | Ast.ELet (bindings, body, _) =>
            let
              fun addBinding (binding, locals) =
                case binding of
                  Ast.BVal {name, annot, value, span} =>
                    let
                      val valueType = infer locals value
                      val bindingType =
                        case annot of
                          NONE => valueType
                        | SOME syntaxType =>
                            let
                              val expected = resolveSyntaxType span syntaxType
                              val sameConvention =
                                Types.sameConsumeConventions expected valueType
                            in
                              if Types.unify expected valueType then
                                ( if sameConvention then ()
                                  else
                                    conventionMismatch
                                      ("ownership convention mismatch in the binding of `" ^
                                       name ^ "`")
                                      span
                                      expected
                                      valueType
                                ; expected
                                )
                              else
                                (mismatch
                                   ("type mismatch in the binding of `" ^ name ^ "`")
                                   span
                                   expected
                                   valueType;
                                 expected)
                            end
                      val _ = recordBindingType span bindingType
                    in
                      (name, bindingType) :: locals
                    end
                | Ast.BPat {pattern, value, span} =>
                    let
                      val valueType = infer locals value
                      val _ = recordBindingType span valueType
                      val seen = StringSet.create 64
                    in
                      checkPattern (seen, locals, pattern, valueType)
                    end
                | Ast.BFun _ =>
                    raise Fail "check: desugared local function"
                | Ast.BGuard _ =>
                    raise Fail "check: desugared guard binding"
              val extended = List.foldl addBinding locals bindings
            in
              infer extended body
            end

      fun checkReturnedBorrow
            (functionName, parameters, parameterTypes, result, body) =
        let
          datatype provenance =
              PParam of int
            | PLocal
            | PMixed
            | PNeutral

          fun combine (left, right) =
            case (left, right) of
              (PNeutral, value) => value
            | (value, PNeutral) => value
            | (PLocal, _) => PLocal
            | (_, PLocal) => PLocal
            | (PMixed, _) => PMixed
            | (_, PMixed) => PMixed
            | (PParam index1, PParam index2) =>
                if index1 = index2 then PParam index1 else PMixed

          fun patternNames pattern =
            case pattern of
              Ast.PVar (name, _) => [name]
            | Ast.PWildcard _ => []
            | Ast.PInt _ => []
            | Ast.PBool _ => []
            | Ast.PString (_, rest, _) =>
                (case rest of
                   SOME subpattern => patternNames subpattern
                 | NONE => [])
            | Ast.PBytes (_, rest, _) =>
                (case rest of
                   SOME subpattern => patternNames subpattern
                 | NONE => [])
            | Ast.PConstructor (_, argument, _) =>
                (case argument of
                   SOME subpattern => patternNames subpattern
                 | NONE => [])
            | Ast.PTuple (patterns, _) =>
                List.concat (List.map patternNames patterns)
            | Ast.PRecord (fields, _) =>
                List.concat
                  (List.map (fn (_, subpattern) =>
                    patternNames subpattern) fields)

          fun lookupProvenance (environment, name) =
            case List.find
                   (fn (candidate, _) => candidate = name)
                   environment of
              SOME (_, provenance) => provenance
            | NONE => PLocal

          fun sameSpan (left, right) =
            Pos.offset (#start left) = Pos.offset (#start right) andalso
            Pos.offset (#stop left) = Pos.offset (#stop right)

          fun expressionTypeAt span =
            case List.find
                   (fn (candidate, _) => sameSpan (candidate, span))
                   (!expressionTypes) of
              SOME (_, ty) => SOME ty
            | NONE => NONE

          fun expressionHasBorrow expression =
            case expressionTypeAt (Ast.exprSpan expression) of
              SOME ty => Types.containsBorrow ty
            | NONE => true

          fun provenanceOf (environment, expression) =
            if not (expressionHasBorrow expression) then PNeutral
            else
              case expression of
                Ast.EVar (name, _) =>
                  (case lookupConstructor name of
                     SOME (_, _, NONE) => PNeutral
                   | _ => lookupProvenance (environment, name))
              | Ast.EBorrow (_, _, operand, _) =>
                  (case operand of
                     Ast.EVar (name, span) =>
                       (case expressionTypeAt (Ast.exprSpan operand) of
                          SOME ty =>
                            (case Types.prune ty of
                               Types.TBorrow _ =>
                                 lookupProvenance (environment, name)
                             | _ => PLocal)
                        | NONE => PLocal)
                   | Ast.EField (_, base, _) =>
                       provenanceOf (environment, base)
                   | Ast.ETupleIndex (_, base, _) =>
                       provenanceOf (environment, base)
                   | _ => PLocal)
              | Ast.EField (_, base, _) =>
                  provenanceOf (environment, base)
              | Ast.ETupleIndex (_, base, _) =>
                  provenanceOf (environment, base)
              | Ast.ETuple (values, _) =>
                  combineAll (environment, values)
              | Ast.ERecord (fields, _) =>
                  combineAll (environment, List.map (fn (_, value) => value) fields)
              | Ast.EIf (_, thenBranch, elseBranch, _) =>
                  combineAll (environment, [thenBranch, elseBranch])
              | Ast.ELet (bindings, body, _) =>
                  provenanceOf
                    (extendEnvironment (environment, bindings), body)
              | Ast.ECase (scrutinee, branches, _) =>
                  let
                    val scrutineeProvenance =
                      provenanceOf (environment, scrutinee)
                    fun branchEnvironment pattern =
                      List.foldl
                        (fn (name, environment) =>
                          (name, scrutineeProvenance) :: environment)
                        environment
                        (patternNames pattern)
                    fun branchProvenance ((pattern, _, branchBody), acc) =
                      combine
                        ( acc
                        , provenanceOf (branchEnvironment pattern, branchBody)
                        )
                  in
                    case branches of
                      [] => PLocal
                    | (firstPattern, _, firstBody) :: rest =>
                        let
                          val initial =
                            provenanceOf
                              (branchEnvironment firstPattern, firstBody)
                        in
                          List.foldl branchProvenance initial rest
                        end
                  end
              | Ast.ECall (name, arguments, _) =>
                  (case lookupConstructor name of
                     SOME _ =>
                       (case arguments of
                          [] => PNeutral
                        | _ => combineAll (environment, arguments))
                   | NONE =>
                       (case lookup name functions of
                          SOME (_, {result = calleeResult, ...}) =>
                            (case Types.prune calleeResult of
                               Types.TBorrow (_, _, SOME index) =>
                                 if index >= 1 andalso
                                    index <= List.length arguments then
                                   provenanceOf
                                     (environment,
                                      List.nth (arguments, index - 1))
                                 else PLocal
                             | _ => PLocal)
                        | NONE => PLocal))
              | _ => PLocal

          and combineAll (environment, expressions) =
            let
              val relevant =
                List.filter expressionHasBorrow expressions
            in
              case relevant of
                [] => PNeutral
              | first :: rest =>
                  List.foldl
                    (fn (expression, acc) =>
                      combine
                        (acc, provenanceOf (environment, expression)))
                    (provenanceOf (environment, first))
                    rest
            end

          and extendEnvironment (environment, bindings) =
            List.foldl
              (fn (binding, environment) =>
                case binding of
                  Ast.BVal {name, value, ...} =>
                    (name, provenanceOf (environment, value)) :: environment
                | Ast.BPat {pattern, value, ...} =>
                    let
                      val provenance =
                        provenanceOf (environment, value)
                    in
                      List.foldl
                        (fn (name, environment) =>
                          (name, provenance) :: environment)
                        environment
                        (patternNames pattern)
                    end
                | Ast.BFun _ =>
                    raise Fail "check: desugared local function"
                | Ast.BGuard _ =>
                    raise Fail "check: desugared guard binding")
              environment
              bindings

          fun initialEnvironment (index, params, types, environment) =
            case (params, types) of
              (param :: restParams, ty :: restTypes) =>
                let
                  val provenance =
                    case Types.prune ty of
                      Types.TBorrow _ => PParam index
                    | _ => PLocal
                in
                  initialEnvironment
                    ( index + 1
                    , restParams
                    , restTypes
                    , (#name param, provenance) :: environment
                    )
                end
            | _ => environment

          fun borrowParameterIndices types =
            let
              fun loop (index, remaining, acc) =
                case remaining of
                  [] => List.rev acc
                | ty :: rest =>
                    (case Types.prune ty of
                       Types.TBorrow _ =>
                         loop (index + 1, rest, index :: acc)
                     | _ => loop (index + 1, rest, acc))
            in
              loop (1, types, [])
            end

          fun checkOneBorrow (environment, returnType, expression) =
            case Types.prune returnType of
              Types.TBorrow (_, returnMutable, provenance) =>
                let
                  val bodySpan = Ast.exprSpan expression
                  val borrowParameters = borrowParameterIndices parameterTypes
                  val bodyProvenance = provenanceOf (environment, expression)
                  fun reportWrongSource actual =
                    report
                      "E0506"
                      ("returned borrow comes from parameter " ^
                       Int.toString actual ^
                       ", but the signature says from " ^
                       Int.toString (case provenance of
                                        SOME index => index
                                      | NONE => 0))
                      bodySpan
                      []
                      ["change the `from` annotation to match the returned parameter"]
                  fun reportLocal () =
                    report
                      "E0506"
                      "returned borrow escapes a local value"
                      bodySpan
                      []
                      ["return a borrow of a borrow parameter, or keep the borrow local"]
                in
                  case provenance of
                    NONE =>
                      (case borrowParameters of
                         [index] =>
                           (case bodyProvenance of
                              PParam actual =>
                                if actual = index then ()
                                else reportWrongSource actual
                            | PLocal =>
                                report
                                  "E0506"
                                  ("returned borrow in `" ^ functionName ^
                                   "` escapes a local value")
                                  bodySpan
                                  ["no parameter of `" ^ functionName ^
                                   "` is a borrow"]
                                  ["return a borrow of a borrow parameter, or keep the borrow local"]
                            | PMixed =>
                                report
                                  "E0506"
                                  "returned borrow may come from several places"
                                  bodySpan
                                  []
                                  ["make every branch borrow the same parameter"]
                            | PNeutral =>
                                reportLocal ())
                       | _ =>
                           report
                             "E0503"
                             ("function `" ^ functionName ^
                              "` returns a borrow without provenance")
                             bodySpan
                             [ case borrowParameters of
                                 [] => "no parameter has a borrow type"
                               | indices =>
                                   "candidate parameter(s): " ^
                                   String.concatWith ", "
                                     (List.map Int.toString indices) ]
                             [ if List.null borrowParameters then
                                 "return a borrow of a borrow parameter, or keep the borrow local"
                               else
                                 "annotate the result as `borrow ... from N`" ])
                  | SOME index =>
                      if index < 1 orelse index > List.length parameterTypes then
                        report
                          "E0504"
                          ("borrow provenance index " ^ Int.toString index ^
                           " is out of range")
                          bodySpan
                          [ "function has " ^
                            Int.toString (List.length parameterTypes) ^
                            " parameter(s)" ]
                          []
                      else
                        case List.nth (parameterTypes, index - 1) of
                          Types.TBorrow (_, sourceMutable, _) =>
                            if returnMutable andalso not sourceMutable then
                              report
                                "E0505"
                                ("cannot return a mutable borrow from shared parameter " ^
                                 Int.toString index)
                                bodySpan
                                []
                                ["use a `borrow mut` parameter, or return a shared borrow"]
                            else
                              (case bodyProvenance of
                                 PParam actual =>
                                   if actual = index then ()
                                   else reportWrongSource actual
                               | PLocal => reportLocal ()
                               | PNeutral => reportLocal ()
                               | PMixed =>
                                   report
                                     "E0506"
                                     "returned borrow may come from several parameters"
                                     bodySpan
                                     []
                                     ["make every branch return the same parameter"])
                          | _ =>
                              report
                                "E0505"
                                ("parameter " ^ Int.toString index ^
                                 " is not a borrow; `from` must name a borrow parameter")
                                bodySpan
                                []
                                []
                end
            | _ => ()

          fun checkAggregate (environment, ty, expression) =
            let
              val bodySpan = Ast.exprSpan expression
              val borrowParameters = borrowParameterIndices parameterTypes
              fun reportLocal () =
                report
                  "E0506"
                  ("returned value of type `" ^ Types.toString ty ^
                   "` contains a borrow that escapes a local value")
                  bodySpan
                  [ "composite results may hold borrows only when they come from a borrow parameter" ]
                  [ "return the borrow parameter itself, or keep the composite value local" ]
            in
              case provenanceOf (environment, expression) of
                PParam index =>
                  if index >= 1 andalso index <= List.length parameterTypes then
                    (case List.nth (parameterTypes, index - 1) of
                       Types.TBorrow _ => ()
                     | _ =>
                         report
                           "E0505"
                           ("parameter " ^ Int.toString index ^
                            " is not a borrow")
                           bodySpan
                           []
                           [])
                  else
                    report
                      "E0504"
                      ("borrow provenance index " ^ Int.toString index ^
                       " is out of range")
                      bodySpan
                      []
                      []
              | PNeutral => ()
              | PLocal => reportLocal ()
              | PMixed =>
                  report
                    "E0506"
                    ("returned value of type `" ^ Types.toString ty ^
                     "` may contain borrows from several places")
                    bodySpan
                    []
                    ["make every borrowed component come from the same borrow parameter"]
            end

          fun checkValue (environment, ty, expression) =
            case Types.prune ty of
              Types.TBorrow _ => checkOneBorrow (environment, ty, expression)
            | Types.TConsume inner =>
                checkValue (environment, inner, expression)
            | Types.TTuple fields =>
                if not (Types.containsBorrow ty) then ()
                else
                (case expression of
                   Ast.ETuple (values, _) =>
                     let
                       fun loop ([], _) = ()
                         | loop (field :: restFields, value :: restValues) =
                             ( checkValue (environment, field, value)
                             ; loop (restFields, restValues)
                             )
                         | loop _ =
                             checkAggregate (environment, ty, expression)
                     in
                       loop (fields, values)
                     end
                 | Ast.ELet (bindings, letBody, _) =>
                     checkValue
                       (extendEnvironment (environment, bindings), ty, letBody)
                 | Ast.EIf (_, thenBranch, elseBranch, _) =>
                     ( checkValue (environment, ty, thenBranch)
                     ; checkValue (environment, ty, elseBranch)
                     )
                 | Ast.ECase (scrutinee, branches, _) =>
                     let
                       val scrutineeProvenance =
                         provenanceOf (environment, scrutinee)
                       fun branchEnvironment pattern =
                         List.foldl
                           (fn (name, environment) =>
                             (name, scrutineeProvenance) :: environment)
                           environment
                           (patternNames pattern)
                     in
                       List.app
                         (fn (pattern, _, branchBody) =>
                           checkValue
                             (branchEnvironment pattern, ty, branchBody))
                         branches
                     end
                 | _ => checkAggregate (environment, ty, expression))
            | Types.TRecord fields =>
                if not (Types.containsBorrow ty) then ()
                else checkRecordValue (environment, ty, fields, expression)
            | Types.TCon {fields, ...} =>
                if not (Types.containsBorrow ty) then ()
                else checkRecordValue (environment, ty, fields, expression)
            | _ =>
                if Types.containsBorrow ty then
                  checkAggregate (environment, ty, expression)
                else ()

          and checkRecordValue (environment, ty, fields, expression) =
            case expression of
              Ast.ERecord (values, _) =>
                List.app
                  (fn (name, fieldTy) =>
                    case List.find
                           (fn (candidate, _) => candidate = name)
                           values of
                      SOME (_, value) =>
                        checkValue (environment, fieldTy, value)
                    | NONE =>
                        checkAggregate (environment, ty, expression))
                  fields
            | Ast.ELet (bindings, letBody, _) =>
                checkValue
                  (extendEnvironment (environment, bindings), ty, letBody)
            | Ast.EIf (_, thenBranch, elseBranch, _) =>
                ( checkValue (environment, ty, thenBranch)
                ; checkValue (environment, ty, elseBranch)
                )
            | Ast.ECase (_, branches, _) =>
                List.app
                  (fn (pattern, _, branchBody) =>
                    let
                      val branchEnvironment =
                        List.foldl
                          (fn (name, environment) =>
                            (name, PLocal) :: environment)
                          environment
                          (patternNames pattern)
                    in
                      checkValue (branchEnvironment, ty, branchBody)
                    end)
                  branches
            | _ => checkAggregate (environment, ty, expression)

          val bodyEnvironment =
            initialEnvironment (1, parameters, parameterTypes, [])
        in
          checkValue (bodyEnvironment, result, body)
        end

      val _ = checkLambdaBorrow := checkReturnedBorrow

      fun checkFunctionDeclaration declaration =
        case declaration of
          Ast.DFun {name, params, body, ...} =>
            if List.exists (fn skipped => skipped = name) (!skipBodies) then ()
            else
              (case lookup name functions of
                 NONE => ()
               | SOME (_, {params = paramTypes, result, typeVars, ...}) =>
                   let
                     val savedVars = !currentTypeVars
                     val savedAllow = !allowTypeVars
                     val _ = currentTypeVars := typeVars
                     val _ = allowTypeVars := true
                     val locals =
                       ListPair.zip
                         (List.map (fn param => #name param) params, paramTypes)
                     val bodyType = infer locals body
                     val sameConvention =
                       Types.sameConsumeConventions result bodyType
                     val _ =
                       if Types.unify bodyType result then
                         ( if sameConvention then ()
                           else
                             conventionMismatch
                               ("ownership convention mismatch in the body of `" ^
                                name ^ "`")
                               (Ast.exprSpan body)
                               result
                               bodyType
                         )
                       else mismatch
                         ("type mismatch in the body of `" ^ name ^ "`")
                         (Ast.exprSpan body)
                         result
                         bodyType
                     val _ =
                       checkReturnedBorrow
                         (name, params, paramTypes, result, body)
                     val _ = currentTypeVars := savedVars
                     val _ = allowTypeVars := savedAllow
                   in
                     ()
                   end)
        | _ => ()

      fun generalizeName name =
        case lookup name functions of
          NONE => ()
        | SOME (_, {params, result, generalized, ...}) =>
            let
              val variables = Types.freeVarsOfTypes (params @ [result])
              val _ = generalized := variables
            in
              ()
            end

      val declarations =
        List.filter
          (fn declaration =>
            case declaration of
              Ast.DFun _ => true
            | _ => false)
          program
      val functionNames =
        List.map
          (fn declaration =>
            case declaration of
              Ast.DFun {name, ...} => name
            | _ => "")
          declarations

      fun hashName size name =
        let
          val length = String.size name
          fun loop (index, accumulator) =
            if index >= length then accumulator mod size
            else
              loop
                ( index + 1
                , (accumulator * 31 + Char.ord (String.sub (name, index))) mod size
                )
        in
          loop (0, 0)
        end

      val functionCount = List.length declarations
      val functionNameArray = Array.fromList functionNames
      val declarationArray = Array.fromList declarations

      val nameIndexTable =
        let
          val size = Int.max (64, 2 * functionCount)
          val buckets = Array.array (size, [])
          fun insert (index, name) =
            let
              val bucketIndex = hashName size name
              val bucket = Array.sub (buckets, bucketIndex)
            in
              Array.update (buckets, bucketIndex, (name, index) :: bucket)
            end
          val _ =
            List.app insert
              (List.tabulate
                 ( functionCount
                 , fn index =>
                     (index, Array.sub (functionNameArray, index))
                 ))
        in
          (buckets, size)
        end

      fun indexOf name =
        let
          val (buckets, size) = nameIndexTable
          val bucket = Array.sub (buckets, hashName size name)
        in
          case List.find (fn (candidate, _) => candidate = name) bucket of
            SOME (_, index) => SOME index
          | NONE => NONE
        end

      fun declarationOf name =
        case indexOf name of
          SOME index => SOME (Array.sub (declarationArray, index))
        | NONE => NONE

      fun calledNames expression =
        let
          fun walk expr =
            case expr of
              Ast.ECall (name, arguments, _) =>
                name :: List.concat (List.map walk arguments)
            | Ast.EVar _ => []
            | Ast.EInt _ => []
            | Ast.EFloat _ => []
            | Ast.EString _ => []
            | Ast.EBool _ => []
            | Ast.EUnit _ => []
            | Ast.EBin (_, left, right, _) => walk left @ walk right
            | Ast.ENeg (operand, _) => walk operand
            | Ast.EIf (condition, thenBranch, elseBranch, _) =>
                walk condition @ walk thenBranch @ walk elseBranch
            | Ast.ELet (bindings, body, _) =>
                List.concat
                  (List.map
                     (fn Ast.BVal {value, ...} => walk value
                       | Ast.BPat {value, ...} => walk value
                       | Ast.BFun _ =>
                           raise Fail "check: desugared local function"
                       | Ast.BGuard _ =>
                           raise Fail "check: desugared guard binding")
                     bindings) @
                walk body
            | Ast.EArena _ =>
                raise Fail "check: desugared arena expression"
            | Ast.ERecord (fields, _) =>
                List.concat (List.map (fn (_, value) => walk value) fields)
            | Ast.EField (_, base, _) => walk base
            | Ast.ETuple (values, _) => List.concat (List.map walk values)
            | Ast.ETupleIndex (_, base, _) => walk base
            | Ast.ECase (scrutinee, branches, _) =>
                walk scrutinee @
                List.concat
                  (List.map
                     (fn (_, guard, body) =>
                       (case guard of NONE => [] | SOME condition => walk condition) @
                       walk body)
                     branches)
            | Ast.EBorrow (_, _, operand, _) => walk operand
            | Ast.EFn (_, _, body, _) => walk body
            | Ast.EExtern _ => []
        in
          walk expression
        end

      fun callsOf name =
        case declarationOf name of
          SOME (Ast.DFun {body, ...}) => calledNames body
        | _ => []

      val callGraph =
        Array.fromList
          (List.map
             (fn name => List.mapPartial indexOf (callsOf name))
             functionNames)

      val visitIndex = Array.array (functionCount, ~1)
      val lowLink = Array.array (functionCount, 0)
      val onStack = Array.array (functionCount, false)
      val stack = ref ([] : int list)
      val nextIndex = ref 0
      val finished = ref ([] : int list list)

      fun strongConnect node =
        let
          val current = !nextIndex
          val _ = nextIndex := current + 1
          val _ = Array.update (visitIndex, node, current)
          val _ = Array.update (lowLink, node, current)
          val _ = stack := node :: !stack
          val _ = Array.update (onStack, node, true)
          fun visitEdge next =
            if Array.sub (visitIndex, next) = ~1 then
              ( strongConnect next
              ; Array.update
                  ( lowLink
                  , node
                  , Int.min
                      (Array.sub (lowLink, node), Array.sub (lowLink, next))
                  )
              )
            else if Array.sub (onStack, next) then
              Array.update
                ( lowLink
                , node
                , Int.min
                    (Array.sub (lowLink, node), Array.sub (visitIndex, next))
                )
            else
              ()
          val _ = List.app visitEdge (Array.sub (callGraph, node))
          val _ =
            if Array.sub (lowLink, node) = Array.sub (visitIndex, node) then
              let
                fun pop component =
                  case !stack of
                    [] => component
                  | top :: rest =>
                      ( stack := rest
                      ; Array.update (onStack, top, false)
                      ; if top = node then top :: component
                        else pop (top :: component)
                      )
                val component = pop []
              in
                finished := component :: !finished
              end
            else
              ()
        in
          ()
        end

      val _ =
        List.app
          (fn node =>
            if Array.sub (visitIndex, node) = ~1 then strongConnect node
            else ())
          (List.tabulate (functionCount, fn index => index))

      val orderedSccs =
        List.rev
          (List.map
             (fn component =>
               List.map
                 (fn node => Array.sub (functionNameArray, node))
                 component)
             (!finished))

      val _ =
        List.app
          (fn scc =>
            let
              val _ =
                List.app
                  (fn name =>
                    case declarationOf name of
                      SOME declaration => checkFunctionDeclaration declaration
                    | NONE => ())
                  scc
              val _ = List.app generalizeName scc
            in
              ()
            end)
          orderedSccs

      val _ =
        if not requireMain then ()
        else
        case lookup "main" functions of
          NONE =>
            report
              "E3010"
              "missing `main` function"
              builtinSpan
              []
              ["define `fun main () = ...` at module level"]
        | SOME (_, {arity, result, span, ...}) =>
            ( if arity <> 0 then
                report
                  "E3010"
                  "`main` must take no parameters"
                  span
                  []
                  []
              else ()
            ; if Types.unify result Types.TUnit then ()
              else
                report
                  "E3010"
                  "`main` must return unit"
                  span
                  [ "expected: unit"
                  , "found: " ^ Types.toString result
                  ]
                  []
            )
    in
      { diagnostics = List.map Diag.demangle (List.rev (!diagnostics))
      , functions = functions
      , bindingTypes = List.rev (!bindingTypes)
      , expressionTypes = List.rev (!expressionTypes)
      , calls = List.rev (!calls)
      , functionValues = List.rev (!functionValues)
      , applications = List.rev (!applications)
      , types = List.rev (!declaredTypes)
      , aliases = List.rev (!declaredAliases)
      , datatypes = List.rev (!declaredDatatypes)
      }
    end

  fun check program = checkWith (true, fn _ => false) program

  fun checkLibrary program = checkWith (false, fn _ => false) program

  fun checkWithPrelude (preludeFileId, requireMain, program) =
    checkWith
      (requireMain, fn span => Pos.file (#start span) = preludeFileId)
      program
end
