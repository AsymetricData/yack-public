structure OwnershipTypes =
struct

  datatype status = Available | Moved | Partial of int list list | Borrowed

  type state = (int * status) list

  type place = { root : int, path : int list }

  type loan = { id : int, place : place, mutable : bool }

  type input =
    { program : Ast.program
    , resolution : Resolve.result
    , functions : (string * Check.funSig) list
    , bindingTypes : (Diag.span * Types.ty) list
    , expressionTypes : (Diag.span * Types.ty) list
    }

  type earlyDrop =
    { name : string
    , ty : Types.ty
    , skip : int list list
    }

  type result =
    { diagnostics : Diag.t list
    , drops : Diag.span list
    , partialDrops : (Diag.span * int list list) list
    , earlyDrops : (Diag.span * earlyDrop list) list
    , readonlyParams : (Diag.span * string * Types.ty) list
    , borrowedLambdaParams : (Diag.span * string) list
    }
end
