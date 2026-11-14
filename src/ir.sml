structure Ir =
struct

  datatype discipline = DCopy | DAffine | DLinear

  datatype ty =
      Int of int
    | Float of int
    | Bool
    | Unit
    | TTuple of ty list
    | TRecord of (string * ty) list * discipline
    | TData of string * ty list * bool
    | TBorrow of ty * bool
    | TToken
    | TResource
    | TString
    | TArray of ty
    | TMap of ty
    | TFun of ty list * ty

  type drop =
    { name : string
    , ty : ty
    , skip : int list list
    , site : Diag.span option
    }

  datatype pattern =
      PWildcard
    | PVar of string * ty
    | PBool of bool
    | PInt of Int64.int * ty

    | PString of string * pattern option * ty

    | PBytes of string * pattern option * ty
    | PTuple of pattern list * ty
    | PRecord of (string * pattern) list * ty
    | PCtor of string * string * pattern option * ty

  datatype allocSite =
      NativeStack
    | Current
    | Destination

  datatype expr =
      IntLit of Int64.int * ty
    | FloatLit of real * ty
    | StringLit of string * ty
    | BoolLit of bool
    | UnitLit
    | Var of string * ty * Diag.span option
    | Bin of string * expr * expr * ty * allocSite ref
    | Neg of expr * ty
    | Call of string * expr list * ty * bool ref
    | If of expr * expr * expr * ty
    | Let of (string * ty * expr) list * expr * drop list
    | Tuple of expr list * ty
    | Record of (string * expr) list * ty
    | Field of expr * int * ty
    | TupleIndex of expr * int * ty
    | Ctor of string * string * expr option * ty * allocSite ref
    | Match of expr * (pattern * expr option * expr * drop list) list * ty
    | PrintString of expr * bool
    | StringBuiltin of string * (expr * bool) list * ty * allocSite ref
    | BytesBuiltin of string * (expr * bool) list * ty * allocSite ref
    | SysBuiltin of string * expr list * ty
    | ArrayBuiltin of string * expr list * ty * allocSite ref
    | MapBuiltin of string * (expr * bool) list * ty * allocSite ref
    | Copy of expr * ty
    | Borrow of bool * expr * ty
    | Closure of string * ty
    | ClosureRef of string * ty
    | Apply of expr * expr list * ty

  type dataDef =
    { name : string
    , sourceName : string

    , prelude : bool
    , parameters : ty list
    , constructors : (string * ty option) list

    , linear : bool
    }

  type funDef =
    { sourceName : string
    , name : string
    , params : (string * ty) list
    , result : ty
    , recursive : bool
    , body : expr
    , drops : drop list

    , borrowed : string list
    }

  type program =
    { datatypes : dataDef list
    , functions : funDef list
    }

  fun linearPaths isLinearData (ty, skip) =
    let
      fun skipped path = List.exists (fn moved => moved = path) skip
      fun startsWith ([], _) = true
        | startsWith (_, []) = false
        | startsWith (x :: xs, y :: ys) = x = y andalso startsWith (xs, ys)
      fun partlyMoved path =
        List.exists
          (fn moved => moved <> path andalso startsWith (path, moved)) skip
      fun walk (path, ty) =
        if skipped path then []
        else
          case ty of
            TToken => [path]
          | TTuple types =>
              List.concat
                (List.tabulate
                   (List.length types,
                    fn i => walk (path @ [i], List.nth (types, i))))
          | TRecord (fields, DLinear) =>

              if partlyMoved path then walkFields (path, fields)
              else [path]
          | TRecord (fields, _) => walkFields (path, fields)
          | TData (name, _, copy) =>
              if not copy andalso isLinearData name then [path] else []
          | TArray inner => if null (walk ([], inner)) then [] else [path]
          | TMap inner => if null (walk ([], inner)) then [] else [path]
          | _ => []
      and walkFields (path, fields) =
        List.concat
          (List.tabulate
             (List.length fields,
              fn i => walk (path @ [i], #2 (List.nth (fields, i)))))
    in
      walk ([], ty)
    end

  fun pathToString path =
    String.concatWith "." (List.map Int.toString path)

  fun skipToString skip =
    String.concatWith ", " (List.map pathToString skip)

  fun structuralDiscipline fields =
    let
      fun copy ty =
        case ty of
          Int _ => true
        | Float _ => true
        | Bool => true
        | Unit => true
        | TBorrow (_, false) => true
        | TFun _ => true
        | TData (_, _, isCopy) => isCopy
        | TTuple types => List.all copy types
        | TRecord (_, discipline) => discipline = DCopy
        | _ => false
    in
      if List.all (fn (_, ty) => copy ty) fields then DCopy else DAffine
    end

  fun disciplinePrefix DCopy = "copy "
    | disciplinePrefix DAffine = ""
    | disciplinePrefix DLinear = "linear "

  fun tyToString (Int width) =
        if width = 32 then "int" else "int" ^ Int.toString width
    | tyToString (Float width) =
        if width = 64 then "float" else "float" ^ Int.toString width
    | tyToString Bool = "bool"
    | tyToString Unit = "unit"
    | tyToString (TTuple types) =
        String.concatWith " * " (List.map tyToString types)
    | tyToString (TRecord (fields, discipline)) =
        disciplinePrefix discipline ^ "{ " ^
        String.concatWith ", "
          (List.map (fn (name, ty) => name ^ " : " ^ tyToString ty) fields) ^
        " }"
    | tyToString (TData (name, parameters, _)) =
        (case parameters of
           [] => name
         | [parameter] => tyToString parameter ^ " " ^ name
         | _ =>
             "(" ^ String.concatWith ", " (List.map tyToString parameters) ^
             ") " ^ name)
    | tyToString (TBorrow (inner, mutable)) =
        "&" ^ (if mutable then "mut " else "") ^ tyToString inner
    | tyToString TToken = "token"
    | tyToString TResource = "resource"
    | tyToString TString = "string"
    | tyToString (TArray ty) = tyToString ty ^ " array"
    | tyToString (TMap ty) = tyToString ty ^ " map"
    | tyToString (TFun (params, result)) =
        (case params of
           [param] => tyToString param
         | _ =>
             "(" ^ String.concatWith ", " (List.map tyToString params) ^ ")") ^
        " -> " ^ tyToString result

  fun patternToString pattern =
    case pattern of
      PWildcard => "_"
    | PVar (name, _) => name
    | PBool true => "true"
    | PBool false => "false"
    | PInt (value, _) => Int64.toString value
    | PString (literal, NONE, _) => "\"" ^ literal ^ "\""
    | PString (literal, SOME rest, _) =>
        "\"" ^ literal ^ "\" ^ " ^ patternToString rest
    | PBytes (bytes, NONE, _) =>
        "<<" ^ Int.toString (String.size bytes) ^ " bytes>>"
    | PBytes (bytes, SOME rest, _) =>
        "<<" ^ Int.toString (String.size bytes) ^ " bytes, " ^
        patternToString rest ^ ">>"
    | PTuple (patterns, _) =>
        "(" ^ String.concatWith ", " (List.map patternToString patterns) ^ ")"
    | PRecord (fields, _) =>
        "{ " ^
        String.concatWith ", "
          (List.map (fn (name, p) => name ^ " = " ^ patternToString p) fields) ^
        " }"
    | PCtor (_, constructor, NONE, _) => constructor
    | PCtor (_, constructor, SOME argument, _) =>
        constructor ^ " (" ^ patternToString argument ^ ")"

  fun exprToString expr =
    case expr of
      IntLit (value, _) => Int64.toString value
    | FloatLit (value, _) => Real.toString value
    | StringLit (value, _) => "\"" ^ value ^ "\""
    | BoolLit true => "true"
    | BoolLit false => "false"
    | UnitLit => "()"
    | Var (name, _, _) => name
    | Bin (operator, left, right, _, _) =>
        "(" ^ exprToString left ^ " " ^ operator ^ " " ^
        exprToString right ^ ")"
    | Neg (operand, _) => "-" ^ exprToString operand
    | Call (name, arguments, _, _) =>
        name ^ "(" ^
        String.concatWith ", " (List.map exprToString arguments) ^ ")"
    | If (condition, thenBranch, elseBranch, _) =>
        "if " ^ exprToString condition ^ " then " ^
        exprToString thenBranch ^ " else " ^ exprToString elseBranch
    | Let (bindings, body, drops) =>
        "let " ^
        String.concatWith " "
          (List.map
             (fn (name, ty, value) =>
               "val " ^ name ^ " : " ^ tyToString ty ^ " = " ^
               exprToString value)
             bindings) ^
        " in " ^ exprToString body ^
        (if List.null drops then ""
         else " drop " ^
              String.concatWith ", "
                (List.map
                   (fn {name, ty, skip, ...} =>
                     name ^ " : " ^ tyToString ty ^
                     (if List.null skip then ""
                      else " skip " ^ skipToString skip))
                   drops)) ^
        " end"
    | Tuple (values, _) =>
        "(" ^ String.concatWith ", " (List.map exprToString values) ^ ")"
    | Record (fields, _) =>
        "{ " ^
        String.concatWith ", "
          (List.map (fn (name, value) => name ^ " = " ^ exprToString value)
             fields) ^
        " }"
    | Field (base, index, _) =>
        exprToString base ^ "." ^ Int.toString index
    | TupleIndex (base, index, _) =>
        exprToString base ^ "." ^ Int.toString index
    | Ctor (_, constructor, NONE, _, _) => constructor
    | Ctor (_, constructor, SOME argument, _, _) =>
        constructor ^ " (" ^ exprToString argument ^ ")"
    | Match (scrutinee, branches, _) =>
        "case " ^ exprToString scrutinee ^ " of " ^
        String.concatWith " | "
          (List.map
             (fn (pattern, guard, body, drops) =>
               patternToString pattern ^
               (case guard of
                  NONE => ""
                | SOME condition => " if " ^ exprToString condition) ^
               " => " ^ exprToString body ^
               (if List.null drops then ""
                else " drop " ^
                     String.concatWith ", "
                       (List.map
                          (fn {name, ty, skip, ...} =>
                            name ^ " : " ^ tyToString ty ^
                            (if List.null skip then ""
                             else " skip " ^ skipToString skip))
                          drops)))
             branches)
    | PrintString (operand, consume) =>
        "print_string (" ^ exprToString operand ^
        (if consume then ", consume)" else ", borrow)")
    | StringBuiltin (name, arguments, _, _) =>
        name ^ "(" ^
        String.concatWith ", "
          (List.map
             (fn (argument, consume) =>
               exprToString argument ^ (if consume then "!" else ""))
             arguments) ^ ")"
    | BytesBuiltin (name, arguments, _, _) =>
        name ^ "(" ^
        String.concatWith ", "
          (List.map
             (fn (argument, consume) =>
               exprToString argument ^ (if consume then "!" else ""))
             arguments) ^ ")"
    | SysBuiltin (name, arguments, _) =>
        name ^ "(" ^
        String.concatWith ", " (List.map exprToString arguments) ^ ")"
    | ArrayBuiltin (name, arguments, _, _) =>
        name ^ "(" ^
        String.concatWith ", " (List.map exprToString arguments) ^ ")"
    | MapBuiltin (name, arguments, _, _) =>
        name ^ "(" ^
        String.concatWith ", "
          (List.map
             (fn (argument, consume) =>
               exprToString argument ^ (if consume then "!" else ""))
             arguments) ^ ")"
    | Copy (operand, _) =>
        "copy (" ^ exprToString operand ^ ")"
    | Borrow (mutable, operand, _) =>
        "&" ^ (if mutable then "mut " else "") ^ exprToString operand
    | Closure (name, _) => "closure " ^ name
    | ClosureRef (name, _) => "closure_ref " ^ name
    | Apply (callee, arguments, _) =>
        exprToString callee ^ "(" ^
        String.concatWith ", " (List.map exprToString arguments) ^ ")"

  fun callsName target expression =
    let
      fun inList expressions = List.exists loop expressions
      and loop expr =
        case expr of
          IntLit _ => false
        | FloatLit _ => false
        | StringLit _ => false
        | BoolLit _ => false
        | UnitLit => false
        | Var _ => false
        | Closure _ => false
        | ClosureRef _ => false
        | Bin (_, left, right, _, _) => loop left orelse loop right
        | Neg (value, _) => loop value
        | Call (name, arguments, _, _) =>
            name = target orelse inList arguments
        | If (condition, thenBranch, elseBranch, _) =>
            loop condition orelse loop thenBranch orelse loop elseBranch
        | Let (bindings, body, _) =>
            List.exists (fn (_, _, value) => loop value) bindings orelse
            loop body
        | Tuple (values, _) => inList values
        | Record (fields, _) =>
            List.exists (fn (_, value) => loop value) fields
        | Field (base, _, _) => loop base
        | TupleIndex (base, _, _) => loop base
        | Ctor (_, _, NONE, _, _) => false
        | Ctor (_, _, SOME value, _, _) => loop value
        | Match (scrutinee, branches, _) =>
            loop scrutinee orelse
            List.exists
              (fn (_, guard, body, _) =>
                (case guard of
                   SOME condition => loop condition
                 | NONE => false) orelse
                loop body)
              branches
        | PrintString (value, _) => loop value
        | StringBuiltin (_, arguments, _, _) =>
            List.exists (fn (value, _) => loop value) arguments
        | BytesBuiltin (_, arguments, _, _) =>
            List.exists (fn (value, _) => loop value) arguments
        | MapBuiltin (_, arguments, _, _) =>
            List.exists (fn (value, _) => loop value) arguments
        | SysBuiltin (_, arguments, _) => inList arguments
        | ArrayBuiltin (_, arguments, _, _) => inList arguments
        | Copy (value, _) => loop value
        | Borrow (_, value, _) => loop value
        | Apply (callee, arguments, _) =>
            loop callee orelse inList arguments
    in
      loop expression
    end

  fun funToString ({sourceName, name, params, result, body, drops, ...} : funDef) =
    "fun " ^ name ^ " (" ^
    String.concatWith ", "
      (List.map (fn (param, ty) => param ^ " : " ^ tyToString ty) params) ^
    ") : " ^ tyToString result ^ " = " ^ exprToString body ^
    (if List.null drops then ""
     else " drop " ^
          String.concatWith ", "
            (List.map
               (fn {name, ty, skip, ...} =>
                 name ^ " : " ^ tyToString ty ^
                 (if List.null skip then ""
                  else " skip " ^ skipToString skip))
               drops)) ^
    "  (* " ^ sourceName ^ " *)"

  fun dataToString
        ({name, sourceName, parameters, constructors, linear,
          prelude = _} : dataDef) =
    "datatype " ^ (if linear then "linear " else "") ^ name ^ " = " ^
    String.concatWith " | "
      (List.map
         (fn (constructor, argument) =>
           case argument of
             NONE => constructor
           | SOME ty => constructor ^ " of " ^ tyToString ty)
         constructors) ^
    "  (* " ^ sourceName ^
    (case parameters of
       [] => ""
     | _ => " " ^ String.concatWith ", " (List.map tyToString parameters)) ^
    " *)"
end
