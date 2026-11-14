structure DriverTypes =
struct
  type sourceEntry =
    { id : int
    , path : string
    , map : Source.t
    }

  val diagnosticStyle : Diag.style ref = ref Diag.plain

  type checked =
    { source : string
    , sources : sourceEntry list
    , program : Ast.program
    , resolution : Resolve.result
    , functions : (string * Check.funSig) list
    , bindingTypes : (Diag.span * Types.ty) list
    , expressionTypes : (Diag.span * Types.ty) list
    , calls : Check.callInfo list
    , functionValues : Check.functionValue list
    , applications : (Diag.span * Types.ty) list
    , drops : Diag.span list
    , partialDrops : (Diag.span * int list list) list
    , earlyDrops : (Diag.span * Ownership.earlyDrop list) list
    , borrowedLambdaParams : (Diag.span * string) list
    , types : Check.declaredType list
    , aliases : Check.declaredAlias list
    , datatypes : Check.declaredData list
    }

  type loadOptions =
    { read : string -> string option
    , requireMain : bool
    , memory : Memory.mode

    , tolerant : bool
    , report : sourceEntry list * Diag.t list -> unit
    , note : string -> unit
    }
end
