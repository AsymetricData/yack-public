structure Token =
struct
  datatype kind =
      IntLit of string
    | FloatLit of string
    | StringLit of string
    | Ident of string
    | Keyword of string
    | Punct of string
    | TyVar of string * bool
    | Eof

  type t = {kind : kind, span : Diag.span}

  fun kindToString (IntLit text) = text
    | kindToString (FloatLit text) = text
    | kindToString (StringLit text) = "\"" ^ text ^ "\""
    | kindToString (Ident text) = text
    | kindToString (Keyword text) = text
    | kindToString (Punct text) = text
    | kindToString (TyVar (name, equality)) =
        (if equality then "''" else "'") ^ name
    | kindToString Eof = "end of file"

  fun describe (t : t) = "`" ^ kindToString (#kind t) ^ "`"

  fun isKeyword word ({kind = Keyword actual, ...} : t) = word = actual
    | isKeyword _ _ = false
  fun isPunct punct ({kind = Punct actual, ...} : t) = punct = actual
    | isPunct _ _ = false
  fun isIdent ({kind = Ident _, ...} : t) = true
    | isIdent _ = false

  fun identOf ({kind = Ident text, ...} : t) = SOME text
    | identOf _ = NONE

  fun tyVarOf ({kind = TyVar (name, equality), ...} : t) =
        SOME (name, equality)
    | tyVarOf _ = NONE

  fun startOf ({span = {start, ...}, ...} : t) = start
  fun stopOf ({span = {stop, ...}, ...} : t) = stop
end
