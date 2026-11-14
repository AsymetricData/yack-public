structure Ast =
struct
  datatype property =
      PCopy
    | PAffine
    | PLinear

  datatype provenance =
      ProvIndex of int
    | ProvName of string

  datatype ty =
      TyInt of int
    | TyFloat of int
    | TyBool
    | TyUnit
    | TyToken
    | TyResource
    | TyString
    | TyName of string
    | TyTuple of ty list
    | TyRecord of (string * ty) list

    | TyBorrow of bool * bool * ty * provenance option
    | TyApp of string * ty
    | TyVar of string * bool

    | TyConsume of bool * ty
    | TyFun of ty list * ty

  type param =
    { name : string
    , annot : ty option
    , span : Diag.span
    }

  datatype pattern =
      PVar of string * Diag.span
    | PWildcard of Diag.span
    | PInt of Int64.int * Diag.span
    | PBool of bool * Diag.span

    | PString of string * pattern option * Diag.span

    | PBytes of string * pattern option * Diag.span
    | PConstructor of string * pattern option * Diag.span
    | PTuple of pattern list * Diag.span
    | PRecord of (string * pattern) list * Diag.span

  datatype expr =
      EInt of Int64.int * Diag.span
    | EFloat of real * Diag.span
    | EString of string * Diag.span
    | EBool of bool * Diag.span
    | EUnit of Diag.span
    | EVar of string * Diag.span
    | ECall of string * expr list * Diag.span
    | EBin of string * expr * expr * Diag.span
    | ENeg of expr * Diag.span
    | EIf of expr * expr * expr * Diag.span
    | ELet of binding list * expr * Diag.span
    | EArena of binding list * expr * Diag.span
    | ERecord of (string * expr) list * Diag.span
    | EField of string * expr * Diag.span
    | ETuple of expr list * Diag.span
    | ETupleIndex of int * expr * Diag.span
    | ECase of expr * (pattern * expr option * expr) list * Diag.span

    | EBorrow of bool * bool * expr * Diag.span

    | EFn of param list * ty option * expr * Diag.span

  and binding =
      BVal of
        { name : string
        , annot : ty option
        , value : expr
        , span : Diag.span
        }
    | BPat of
        { pattern : pattern
        , value : expr
        , span : Diag.span
        }
    | BFun of
        { name : string
        , nameSpan : Diag.span
        , params : param list
        , result : ty option
        , body : expr
        , span : Diag.span
        }

    | BGuard of
        { pattern : pattern option
        , value : expr
        , fallback : expr
        , span : Diag.span
        }

  datatype importItem =
      ImportValue of string * string option
    | ImportType of string * string option

  datatype decl =
      DFun of
        { name : string
        , nameSpan : Diag.span
        , params : param list
        , result : ty option
        , recursive : bool
        , body : expr
        , span : Diag.span
        }
    | DFunClause of
        { name : string
        , nameSpan : Diag.span
        , params : (pattern * ty option) list
        , result : ty option
        , recursive : bool
        , body : expr
        , span : Diag.span
        }
    | DType of
        { name : string
        , declared : property option
        , fields : (string * ty) list
        , span : Diag.span
        }
    | DAlias of
        { name : string
        , params : (string * bool) list
        , body : ty
        , span : Diag.span
        }
    | DDatatype of
        { name : string
        , params : (string * bool) list
        , declared : property option
        , constructors : (string * ty option) list
        , span : Diag.span
        }
    | DImport of
        { path : string list
        , alias : string option
        , unqualified : importItem list
        , span : Diag.span
        }

  type program = decl list

  fun exprSpan (EInt (_, span)) = span
    | exprSpan (EFloat (_, span)) = span
    | exprSpan (EString (_, span)) = span
    | exprSpan (EBool (_, span)) = span
    | exprSpan (EUnit span) = span
    | exprSpan (EVar (_, span)) = span
    | exprSpan (ECall (_, _, span)) = span
    | exprSpan (EBin (_, _, _, span)) = span
    | exprSpan (ENeg (_, span)) = span
    | exprSpan (EIf (_, _, _, span)) = span
    | exprSpan (ELet (_, _, span)) = span
    | exprSpan (EArena (_, _, span)) = span
    | exprSpan (ERecord (_, span)) = span
    | exprSpan (EField (_, _, span)) = span
    | exprSpan (ETuple (_, span)) = span
    | exprSpan (ETupleIndex (_, _, span)) = span
    | exprSpan (ECase (_, _, span)) = span
    | exprSpan (EBorrow (_, _, _, span)) = span
    | exprSpan (EFn (_, _, _, span)) = span

  fun patternSpan (PVar (_, span)) = span
    | patternSpan (PWildcard span) = span
    | patternSpan (PInt (_, span)) = span
    | patternSpan (PBool (_, span)) = span
    | patternSpan (PString (_, _, span)) = span
    | patternSpan (PBytes (_, _, span)) = span
    | patternSpan (PConstructor (_, _, span)) = span
    | patternSpan (PTuple (_, span)) = span
    | patternSpan (PRecord (_, span)) = span

  fun declSpan (DFun {span, ...}) = span
    | declSpan (DFunClause {span, ...}) = span
    | declSpan (DType {span, ...}) = span
    | declSpan (DAlias {span, ...}) = span
    | declSpan (DDatatype {span, ...}) = span
    | declSpan (DImport {span, ...}) = span

  fun tyToString (TyInt width) =
        if width = 32 then "int" else "int" ^ Int.toString width
    | tyToString (TyFloat width) =
        if width = 64 then "float" else "float" ^ Int.toString width
    | tyToString TyBool = "bool"
    | tyToString TyUnit = "unit"
    | tyToString TyToken = "token"
    | tyToString TyResource = "resource"
    | tyToString TyString = "string"
    | tyToString (TyName name) = name
    | tyToString (TyTuple tys) =
        String.concatWith " * " (List.map tyToString tys)
    | tyToString (TyRecord fields) =
        if List.null fields then "{}"
        else
          "{ " ^
          String.concatWith ", "
            (List.map
               (fn (field, ty) => field ^ " : " ^ tyToString ty)
               fields) ^
          " }"
    | tyToString (TyBorrow (mutable, ampersand, ty, provenance)) =
        (if ampersand then "&" else "borrow ") ^
        (if mutable then "mut " else "") ^ tyToString ty ^
        (case provenance of
           NONE => ""
         | SOME (ProvIndex index) => " from " ^ Int.toString index
         | SOME (ProvName name) => " from " ^ name)
    | tyToString (TyApp (name, argument)) =
        let
          val text = tyToString argument
          val text =
            case argument of
              TyTuple _ => "(" ^ text ^ ")"
            | TyApp _ => "(" ^ text ^ ")"
            | _ => text
        in
          text ^ " " ^ name
        end
    | tyToString (TyConsume (bang, ty)) =
        (if bang then "!" else "consume ") ^ tyToString ty
    | tyToString (TyFun (params, result)) =
        (case params of
           [param] => tyToString param
         | _ =>
             "(" ^ String.concatWith ", " (List.map tyToString params) ^ ")") ^
        " -> " ^ tyToString result
    | tyToString (TyVar (name, equality)) =
        (if equality then "''" else "'") ^ name

  fun propertyToString PCopy = "Copy"
    | propertyToString PAffine = "Affine"
    | propertyToString PLinear = "Linear"
end
