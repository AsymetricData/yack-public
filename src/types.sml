structure Types =
struct
  datatype property = Copy | Affine | Linear
  datatype varKind = Ordinary | Equality | Numeric

  datatype ty =
      TVar of int * tv ref
    | TEqVar of int * tv ref
    | TNumVar of int * tv ref
    | TInt of int
    | TIntLit of int * Int64.int * tv ref
    | TFloat of int
    | TFloatLit of int * real * tv ref
    | TBool
    | TUnit
    | TToken
    | TResource
    | TString
    | TCPtr
    | TArray of ty
    | TMap of ty
    | TCon of
        { name : string
        , property : property
        , fields : (string * ty) list
        }
    | TRecord of (string * ty) list
    | TTuple of ty list
    | TData of data
    | TDataApp of data * ty list
    | TBorrow of ty * bool * int option
    | TConsume of ty
    | TFun of ty list * ty

  and tv =
      Unbound
    | Link of ty

  and data =
    Data of
      { name : string
      , params : ty list
      , property : property ref
      , constructors : (string * ty option) list ref
      }

  type context =
    { next : int ref
    , nextLiteral : int ref
    }

  fun newContext () : context =
    { next = ref 0
    , nextLiteral = ref 0
    }

  fun fresh ({next, ...} : context) =
    let
      val id = !next
      val _ = next := id + 1
    in
      TVar (id, ref Unbound)
    end

  fun freshEquality ({next, ...} : context) =
    let
      val id = !next
      val _ = next := id + 1
    in
      TEqVar (id, ref Unbound)
    end

  fun freshNumeric ({next, ...} : context) =
    let
      val id = !next
      val _ = next := id + 1
    in
      TNumVar (id, ref Unbound)
    end

  fun intLiteral ({nextLiteral, ...} : context) value =
    let
      val id = !nextLiteral
      val _ = nextLiteral := id + 1
    in
      TIntLit (id, value, ref Unbound)
    end

  fun floatLiteral ({nextLiteral, ...} : context) value =
    let
      val id = !nextLiteral
      val _ = nextLiteral := id + 1
    in
      TFloatLit (id, value, ref Unbound)
    end

  fun prune (TVar (id, cell)) =
        (case !cell of
           Link ty =>
             let val ty' = prune ty
             in cell := Link ty'; ty' end
         | Unbound => TVar (id, cell))
    | prune (TEqVar (id, cell)) =
        (case !cell of
           Link ty =>
             let val ty' = prune ty
             in cell := Link ty'; ty' end
         | Unbound => TEqVar (id, cell))
    | prune (TNumVar (id, cell)) =
        (case !cell of
           Link ty =>
             let val ty' = prune ty
             in cell := Link ty'; ty' end
         | Unbound => TNumVar (id, cell))
    | prune (TFloatLit (id, value, cell)) =
        (case !cell of
           Link ty =>
             let val ty' = prune ty
             in cell := Link ty'; ty' end
         | Unbound => TFloatLit (id, value, cell))
    | prune (TIntLit (id, value, cell)) =
        (case !cell of
           Link ty =>
             let val ty' = prune ty
             in cell := Link ty'; ty' end
         | Unbound => TIntLit (id, value, cell))
    | prune ty = ty

  fun occurs id ty =
    case prune ty of
      TVar (other, _) => id = other
    | TEqVar (other, _) => id = other
    | TNumVar (other, _) => id = other
    | TFun (params, result) =>
        List.exists (fn param => occurs id param) params orelse occurs id result
    | TCon {fields, ...} =>
        List.exists (fn (_, fieldTy) => occurs id fieldTy) fields
    | TRecord fields =>
        List.exists (fn (_, fieldTy) => occurs id fieldTy) fields
    | TTuple fields =>
        List.exists (fn fieldTy => occurs id fieldTy) fields

    | TData _ => false
    | TDataApp (_, arguments) => List.exists (occurs id) arguments
    | TBorrow (inner, _, _) => occurs id inner
    | TConsume inner => occurs id inner
    | TArray inner => occurs id inner
    | TMap inner => occurs id inner
    | _ => false

  fun isEqualityType ty =
    case prune ty of
      TInt _ => true
    | TIntLit _ => true
    | TFloat _ => false
    | TFloatLit _ => false
    | TNumVar _ => false
    | TBool => true
    | TUnit => true
    | TToken => false
    | TResource => false
    | TString => false
    | TCPtr => false
    | TCon {property, fields, ...} =>
        property = Copy andalso
        List.all (fn (_, fieldTy) => isEqualityType fieldTy) fields
    | TRecord _ => false
    | TTuple _ => false
    | TData (Data {property, ...}) => !property = Copy
    | TDataApp (Data {property, ...}, arguments) =>
        !property = Copy andalso List.all isEqualityType arguments
    | TBorrow _ => false
    | TConsume _ => false
    | TArray _ => false
    | TMap _ => false
    | TVar _ => true
    | TEqVar _ => true
    | TFun _ => false

  fun constrainEquality context ty =
    case prune ty of
      TVar (_, cell) =>
        let val equality = freshEquality context in
          cell := Link equality;
          true
        end
    | TEqVar _ => true
    | other => isEqualityType other

  fun constrainNumeric context ty =
    case prune ty of
      TInt _ => true
    | TIntLit _ => true
    | TFloat _ => true
    | TFloatLit _ => true
    | TNumVar _ => true
    | TVar (_, cell) =>
        let val numeric = freshNumeric context in
          cell := Link numeric;
          true
        end
    | _ => false

  fun unify left right =
    case (prune left, prune right) of
      (TInt width1, TInt width2) => width1 = width2
    | (TIntLit (_, _, cell), TInt width) =>
        (cell := Link (TInt width); true)
    | (TInt width, TIntLit (_, _, cell)) =>
        (cell := Link (TInt width); true)
    | (TIntLit (id1, _, cell1), TIntLit (id2, _, cell2)) =>
        if id1 = id2 then true
        else (cell1 := Link (TIntLit (id2, Int64.fromInt 0, cell2)); true)
    | (TIntLit (_, _, cell), TFloat width) =>
        (cell := Link (TFloat width); true)
    | (TFloat width, TIntLit (_, _, cell)) =>
        (cell := Link (TFloat width); true)
    | (TFloat width1, TFloat width2) => width1 = width2
    | (TFloatLit (_, _, cell), TFloat width) =>
        (cell := Link (TFloat width); true)
    | (TFloat width, TFloatLit (_, _, cell)) =>
        (cell := Link (TFloat width); true)
    | (TFloatLit (id1, _, cell1), TFloatLit (id2, _, cell2)) =>
        if id1 = id2 then true
        else (cell1 := Link (TFloatLit (id2, 0.0, cell2)); true)
    | (TBool, TBool) => true
    | (TUnit, TUnit) => true
    | (TToken, TToken) => true
    | (TResource, TResource) => true
    | (TString, TString) => true
    | (TCPtr, TCPtr) => true
    | (TArray inner1, TArray inner2) => unify inner1 inner2
    | (TMap inner1, TMap inner2) => unify inner1 inner2
    | (TCon {name = name1, ...}, TCon {name = name2, ...}) =>
        name1 = name2
    | (TData (Data {name = name1, ...}), TData (Data {name = name2, ...})) =>
        name1 = name2
    | (TDataApp (Data {name = name1, ...}, arguments1),
       TDataApp (Data {name = name2, ...}, arguments2)) =>
        name1 = name2 andalso
        List.length arguments1 = List.length arguments2 andalso
        List.all (fn (left, right) => unify left right)
          (ListPair.zip (arguments1, arguments2))

    | (TConsume inner1, TConsume inner2) => unify inner1 inner2

    | (TConsume inner, TVar (id, cell)) =>

        if occurs id (TConsume inner) then unify inner (TVar (id, cell))
        else (cell := Link (TConsume inner); true)
    | (TVar (id, cell), TConsume inner) =>
        if occurs id (TConsume inner) then unify (TVar (id, cell)) inner
        else (cell := Link (TConsume inner); true)
    | (TConsume inner, other) => unify inner other
    | (other, TConsume inner) => unify other inner
    | (TBorrow (inner1, mutable1, _), TBorrow (inner2, mutable2, _)) =>
        mutable1 = mutable2 andalso unify inner1 inner2
    | (TRecord fields1, TRecord fields2) => sameFields (fields1, fields2)
    | (TRecord fields1, TCon {fields = fields2, ...}) =>
        sameFields (fields1, fields2)
    | (TCon {fields = fields1, ...}, TRecord fields2) =>
        sameFields (fields1, fields2)
    | (TTuple fields1, TTuple fields2) =>
        List.length fields1 = List.length fields2 andalso
        List.all (fn (a, b) => unify a b) (ListPair.zip (fields1, fields2))
    | (TNumVar (id1, cell1), TNumVar (id2, cell2)) =>
        if id1 = id2 then true
        else (cell1 := Link (TNumVar (id2, cell2)); true)
    | (TNumVar (_, cell), (other as TInt _)) =>
        (cell := Link other; true)
    | (TNumVar (_, cell), (other as TFloat _)) =>
        (cell := Link other; true)
    | (TNumVar (_, cell), (other as TIntLit _)) =>
        (cell := Link other; true)
    | (TNumVar (_, cell), (other as TFloatLit _)) =>
        (cell := Link other; true)
    | ((other as TInt _), TNumVar (_, cell)) =>
        (cell := Link other; true)
    | ((other as TFloat _), TNumVar (_, cell)) =>
        (cell := Link other; true)
    | ((other as TIntLit _), TNumVar (_, cell)) =>
        (cell := Link other; true)
    | ((other as TFloatLit _), TNumVar (_, cell)) =>
        (cell := Link other; true)
    | (TVar (_, cell), TNumVar (id, cellEq)) =>
        (cell := Link (TNumVar (id, cellEq)); true)
    | (TNumVar (id, cellEq), TVar (_, cell)) =>
        (cell := Link (TNumVar (id, cellEq)); true)
    | (TEqVar (id1, cell1), TEqVar (id2, cell2)) =>
        if id1 = id2 then true
        else (cell1 := Link (TEqVar (id2, cell2)); true)
    | (TVar (_, cell), TEqVar (id, cellEq)) =>
        (cell := Link (TEqVar (id, cellEq)); true)
    | (TEqVar (id, cellEq), TVar (_, cell)) =>
        (cell := Link (TEqVar (id, cellEq)); true)
    | (TEqVar (id, cell), other) =>
        if isEqualityType other then
          if occurs id other then false
          else (cell := Link other; true)
        else false
    | (other, TEqVar (id, cell)) =>
        if isEqualityType other then
          if occurs id other then false
          else (cell := Link other; true)
        else false
    | (TVar (id1, cell1), TVar (id2, cell2)) =>
        if id1 = id2 then true
        else (cell1 := Link (TVar (id2, cell2)); true)
    | (TVar (id, cell), other) =>
        if occurs id other then false
        else (cell := Link other; true)
    | (other, TVar (id, cell)) =>
        if occurs id other then false
        else (cell := Link other; true)
    | (TFun (params1, result1), TFun (params2, result2)) =>
        List.length params1 = List.length params2
        andalso List.all (fn (a, b) => unify a b) (ListPair.zip (params1, params2))
        andalso unify result1 result2
    | _ => false

  and sameFields (left, right) =
    List.length left = List.length right andalso
    List.all
      (fn (name, ty) =>
        case List.find (fn (candidate, _) => candidate = name) right of
          SOME (_, other) => unify ty other
        | NONE => false)
      left

  fun concretize ty =
    case prune ty of
      TIntLit _ => TInt 32
    | TFloatLit _ => TFloat 64
    | TTuple fields => TTuple (List.map concretize fields)
    | TRecord fields =>
        TRecord (List.map (fn (name, fieldTy) => (name, concretize fieldTy)) fields)
    | TCon {name, property, fields} =>
        TCon
          { name = name
          , property = property
          , fields = List.map (fn (field, fieldTy) => (field, concretize fieldTy)) fields
          }
    | TDataApp (data, arguments) =>
        TDataApp (data, List.map concretize arguments)
    | TBorrow (inner, mutable, provenance) =>
        TBorrow (concretize inner, mutable, provenance)
    | TConsume inner => TConsume (concretize inner)
    | TArray inner => TArray (concretize inner)
    | TMap inner => TMap (concretize inner)
    | TFun (params, result) =>
        TFun (List.map concretize params, concretize result)
    | other => other

  fun isConcrete ty =
    case prune ty of
      TVar _ => false
    | TEqVar _ => false
    | TNumVar _ => false
    | TInt _ => true
    | TIntLit _ => true
    | TFloat _ => true
    | TFloatLit _ => true
    | TBool => true
    | TUnit => true
    | TToken => true
    | TResource => true
    | TString => true
    | TCPtr => true
    | TCon {fields, ...} =>
        List.all (fn (_, fieldTy) => isConcrete fieldTy) fields
    | TRecord fields =>
        List.all (fn (_, fieldTy) => isConcrete fieldTy) fields
    | TTuple fields =>
        List.all isConcrete fields

    | TData _ => true
    | TDataApp (_, arguments) => List.all isConcrete arguments
    | TBorrow (inner, _, _) => isConcrete inner
    | TConsume inner => isConcrete inner
    | TArray inner => isConcrete inner
    | TMap inner => isConcrete inner
    | TFun (params, result) =>
        List.all isConcrete params andalso isConcrete result

  fun joinProperties properties =
    List.foldl
      (fn (property, best) =>
        case (property, best) of
          (Linear, _) => Linear
        | (_, Linear) => Linear
        | (Affine, _) => Affine
        | (_, Affine) => Affine
        | _ => Copy)
      Copy
      properties

  fun propertyOf ty =
    case prune ty of
      TInt _ => Copy
    | TIntLit _ => Copy
    | TFloat _ => Copy
    | TFloatLit _ => Copy
    | TNumVar _ => Copy
    | TEqVar _ => Copy
    | TBool => Copy
    | TUnit => Copy
    | TToken => Linear
    | TResource => Affine
    | TString => Affine
    | TCPtr => Copy
    | TCon {property, ...} => property
    | TRecord fields =>
        joinProperties
          (Affine :: List.map (fn (_, fieldTy) => propertyOf fieldTy) fields)
    | TTuple fields =>
        joinProperties (Affine :: List.map propertyOf fields)
    | TData (Data {property, ...}) => !property
    | TDataApp (Data {property, ...}, arguments) =>
        joinProperties (!property :: List.map propertyOf arguments)
    | TBorrow (_, false, _) => Copy
    | TBorrow (_, true, _) => Affine
    | TConsume inner => propertyOf inner
    | TArray _ => Affine
    | TMap _ => Affine
    | TFun _ => Copy
    | TVar _ => Affine

  fun intWidth ty =
    case prune ty of
      TInt width => SOME width
    | TIntLit _ => SOME 32
    | _ => NONE

  fun bindIntLiteralWidth ty width =
    case prune ty of
      TIntLit (_, _, cell) => cell := Link (TInt width)
    | _ => ()

  fun widthToString width =
    if width = 32 then "int" else "int" ^ Int.toString width

  fun toString ty =
    case prune ty of
      TInt width => widthToString width
    | TIntLit _ => "int"
    | TFloat width =>
        if width = 64 then "float" else "float" ^ Int.toString width
    | TFloatLit _ => "float"
    | TNumVar (id, _) => "'n" ^ Int.toString id
    | TBool => "bool"
    | TUnit => "unit"
    | TToken => "token"
    | TResource => "resource"
    | TString => "string"
    | TCPtr => "c_ptr"
    | TArray inner =>
        let
          val text = toString inner
          val text =
            case prune inner of
              TTuple _ => "(" ^ text ^ ")"
            | TFun _ => "(" ^ text ^ ")"
            | _ => text
        in
          text ^ " array"
        end
    | TMap inner =>
        let
          val text = toString inner
          val text =
            case prune inner of
              TTuple _ => "(" ^ text ^ ")"
            | TFun _ => "(" ^ text ^ ")"
            | _ => text
        in
          text ^ " map"
        end
    | TCon {name, ...} => name
    | TRecord fields =>
        "{ " ^
        String.concatWith ", "
          (List.map (fn (field, fieldTy) => field ^ " : " ^ toString fieldTy) fields) ^
        " }"
    | TTuple fields =>
        String.concatWith " * " (List.map toString fields)
    | TData (Data {name, ...}) => name
    | TDataApp (Data {name, ...}, arguments) =>
        let
          val argumentText =
            case arguments of
              [argument] =>
                (case prune argument of
                   TTuple _ => "(" ^ toString argument ^ ")"
                 | TFun _ => "(" ^ toString argument ^ ")"
                 | _ => toString argument)
            | _ =>
                "(" ^ String.concatWith ", " (List.map toString arguments) ^ ")"
        in
          argumentText ^ " " ^ name
        end
    | TConsume inner => "consume " ^ toString inner
    | TBorrow (inner, mutable, provenance) =>
        "borrow " ^ (if mutable then "mut " else "") ^ toString inner ^
        (case provenance of
           NONE => ""
         | SOME index => " from " ^ Int.toString index)
    | TEqVar (id, _) => "''t" ^ Int.toString id
    | TVar (id, _) => "'t" ^ Int.toString id
    | TFun (params, result) =>
        "(" ^ String.concatWith ", " (List.map toString params) ^ ") -> " ^
        toString result

  fun dedupVars vars =
    List.foldl
      (fn ((id, equality), acc) =>
        if List.exists (fn (candidate, _) => candidate = id) acc
        then acc
        else (id, equality) :: acc)
      []
      vars

  fun freeVars ty =
    case prune ty of
      TVar (id, _) => [(id, Ordinary)]
    | TEqVar (id, _) => [(id, Equality)]
    | TNumVar (id, _) => [(id, Numeric)]
    | TFun (params, result) =>
        dedupVars
          (List.concat
             (List.map freeVars params @ [freeVars result]))
    | TCon {fields, ...} =>
        dedupVars
          (List.concat
             (List.map (fn (_, fieldTy) => freeVars fieldTy) fields))
    | TRecord fields =>
        dedupVars
          (List.concat
             (List.map (fn (_, fieldTy) => freeVars fieldTy) fields))
    | TTuple fields =>
        dedupVars (List.concat (List.map freeVars fields))
    | TBorrow (inner, _, _) => freeVars inner
    | TConsume inner => freeVars inner
    | TArray inner => freeVars inner
    | TMap inner => freeVars inner
    | TData _ => []
    | TDataApp (_, arguments) =>
        dedupVars (List.concat (List.map freeVars arguments))
    | _ => []

  fun freeVarsOfTypes types = dedupVars (List.concat (List.map freeVars types))

  fun makeSubstitution context variables =
    List.map
      (fn (id, kind) =>
        ( id
        , case kind of
            Ordinary => fresh context
          | Equality => freshEquality context
          | Numeric => freshNumeric context
        ))
      variables

  fun instantiateWith substitution ty =
    let
      fun lookup id =
        case List.find (fn (candidate, _) => candidate = id) substitution of
          SOME (_, replacement) => SOME replacement
        | NONE => NONE
      fun walk ty' =
        case prune ty' of
          TVar (id, _) =>
            (case lookup id of
               SOME replacement => replacement
             | NONE => ty')
        | TEqVar (id, _) =>
            (case lookup id of
               SOME replacement => replacement
             | NONE => ty')
        | TNumVar (id, _) =>
            (case lookup id of
               SOME replacement => replacement
             | NONE => ty')
        | TFun (params, result) =>
            TFun (List.map walk params, walk result)
        | TCon {name, property, fields} =>
            TCon
              { name = name
              , property = property
              , fields = List.map (fn (field, fieldTy) => (field, walk fieldTy)) fields
              }
        | TRecord fields =>
            TRecord (List.map (fn (field, fieldTy) => (field, walk fieldTy)) fields)
        | TTuple fields => TTuple (List.map walk fields)
        | TDataApp (data, arguments) =>
            TDataApp (data, List.map walk arguments)
        | TBorrow (inner, mutable, provenance) =>
            TBorrow (walk inner, mutable, provenance)
        | TConsume inner => TConsume (walk inner)
        | TArray inner => TArray (walk inner)
        | TMap inner => TMap (walk inner)
        | _ => ty'
    in
      walk ty
    end

  fun instantiate context variables ty =
    instantiateWith (makeSubstitution context variables) ty

  fun instantiateParameters parameters arguments ty =
    let
      fun paramId parameter =
        case prune parameter of
          TVar (id, _) => id
        | TEqVar (id, _) => id
        | TNumVar (id, _) => id
        | _ => ~1
      val substitution =
        ListPair.map
          (fn (parameter, argument) => (paramId parameter, argument))
          (parameters, arguments)
    in
      instantiateWith substitution ty
    end

  fun instantiateData (Data {params, ...}) arguments ty =
    let
      fun paramId param =
        case prune param of
          TVar (id, _) => id
        | TEqVar (id, _) => id
        | TNumVar (id, _) => id
        | _ => ~1
      val substitution =
        ListPair.map
          (fn (param, argument) => (paramId param, argument))
          (params, arguments)
    in
      instantiateWith substitution ty
    end

  fun instantiateSignature context variables params result =
    let val substitution = makeSubstitution context variables in
      ( List.map (instantiateWith substitution) params
      , instantiateWith substitution result
      )
    end

  fun sameConsumeConventions left right =
    case (prune left, prune right) of
      (TVar _, _) => true
    | (_, TVar _) => true
    | (TEqVar _, _) => true
    | (_, TEqVar _) => true
    | (TNumVar _, _) => true
    | (_, TNumVar _) => true
    | (TConsume inner1, TConsume inner2) =>
        sameConsumeConventions inner1 inner2
    | (TConsume _, _) => false
    | (_, TConsume _) => false
    | (TFun (params1, result1), TFun (params2, result2)) =>
        List.length params1 = List.length params2 andalso
        ListPair.all (fn (left, right) => sameConsumeConventions left right) (params1, params2) andalso
        sameConsumeConventions result1 result2
    | (TArray inner1, TArray inner2) =>
        sameConsumeConventions inner1 inner2
    | (TMap inner1, TMap inner2) =>
        sameConsumeConventions inner1 inner2
    | (TTuple fields1, TTuple fields2) =>
        List.length fields1 = List.length fields2 andalso
        ListPair.all (fn (left, right) => sameConsumeConventions left right) (fields1, fields2)
    | (TRecord fields1, TRecord fields2) =>
        recordConventions (fields1, fields2)
    | (TRecord fields1, TCon {fields = fields2, ...}) =>
        recordConventions (fields1, fields2)
    | (TCon {fields = fields1, ...}, TRecord fields2) =>
        recordConventions (fields1, fields2)
    | (TCon {name = name1, fields = fields1, ...},
       TCon {name = name2, fields = fields2, ...}) =>
        name1 = name2 andalso recordConventions (fields1, fields2)
    | (TDataApp (_, arguments1), TDataApp (_, arguments2)) =>
        List.length arguments1 = List.length arguments2 andalso
        ListPair.all (fn (left, right) => sameConsumeConventions left right) (arguments1, arguments2)
    | (TBorrow (inner1, _, _), TBorrow (inner2, _, _)) =>
        sameConsumeConventions inner1 inner2
    | _ => true

  and recordConventions (fields1, fields2) =
    List.all
      (fn (name, fieldTy1) =>
        case List.find (fn (candidate, _) => candidate = name) fields2 of
          SOME (_, fieldTy2) => sameConsumeConventions fieldTy1 fieldTy2
        | NONE => true)
      fields1

  fun containsBorrow ty =
    let
      fun loop visited ty =
        case prune ty of
          TBorrow _ => true
        | TConsume inner => loop visited inner
        | TArray inner => loop visited inner
        | TMap inner => loop visited inner
        | TRecord fields =>
            List.exists (fn (_, fieldTy) => loop visited fieldTy) fields
        | TTuple fields => List.exists (loop visited) fields
        | TCon {fields, ...} =>
            List.exists (fn (_, fieldTy) => loop visited fieldTy) fields
        | TData (Data {name, constructors, ...}) =>
            not (List.exists (fn candidate => candidate = name) visited) andalso
            let
              val next = name :: visited
              fun constructorHasBorrow (_, NONE) = false
                | constructorHasBorrow (_, SOME fieldTy) =
                    loop next fieldTy
            in
              List.exists constructorHasBorrow (!constructors)
            end
        | TDataApp (Data {name, params, constructors, ...}, arguments) =>
            not (List.exists (fn candidate => candidate = name) visited) andalso
            let
              val next = name :: visited
              fun constructorHasBorrow (_, NONE) = false
                | constructorHasBorrow (_, SOME fieldTy) =
                    loop next (instantiateParameters params arguments fieldTy)
            in
              List.exists constructorHasBorrow (!constructors)
            end
        | _ => false
    in
      loop [] ty
    end

  fun payloadFields ty =
    case prune ty of
      TCon {fields, ...} => SOME fields
    | TRecord fields => SOME fields
    | _ => NONE

  fun commonFields constructors =
    let
      fun intersect (current, fields) =
        List.filter
          (fn (name, ty) =>
            case List.find (fn (candidate, _) => candidate = name) current of
              SOME (_, previous) => unify previous ty
            | NONE => false)
          fields
      fun loop ([], result) = result
        | loop ((_, NONE) :: _, _) = NONE
        | loop ((_, SOME payload) :: rest, result) =
            (case payloadFields payload of
               NONE => NONE
             | SOME fields =>
                 let
                   val next =
                     case result of
                       NONE => SOME fields
                     | SOME current => SOME (intersect (current, fields))
                 in
                   (case next of
                      NONE => NONE
                    | SOME _ => loop (rest, next))
                 end)
    in
      loop (constructors, NONE)
    end

  fun commonField field constructors =
    case commonFields constructors of
      NONE => NONE
    | SOME fields =>
        (case List.find (fn (name, _) => name = field) fields of
           SOME (_, ty) => SOME ty
         | NONE => NONE)

  fun dataCommonFields (data as Data {constructors, ...}) arguments =
    commonFields
      (List.map
         (fn (name, argument) =>
           (name, Option.map (instantiateData data arguments) argument))
         (!constructors))

  fun dataCommonField field (data as Data {constructors, ...}) arguments =
    commonField field
      (List.map
         (fn (name, argument) =>
           (name, Option.map (instantiateData data arguments) argument))
         (!constructors))

  fun propertyToString Copy = "Copy"
    | propertyToString Affine = "Affine"
    | propertyToString Linear = "Linear"
end
