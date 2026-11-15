structure LlvmTypes =
struct
  type value =
    { text : string
    , ty : Ir.ty
    , const : bool
    }

  type localEntry =
    { name : string
    , value : value
    , address : string option
    }

  datatype cleanup =
      CleanupDrop of value
    | CleanupFree of value

  fun llvmTy (Ir.Int width) = "i" ^ Int.toString width
    | llvmTy (Ir.Float 64) = "double"
    | llvmTy (Ir.Float 32) = "float"
    | llvmTy (Ir.Float _) = "float"
    | llvmTy Ir.Bool = "i1"
    | llvmTy Ir.Unit = "void"
    | llvmTy (Ir.TTuple types) =
        "{ " ^ String.concatWith ", " (List.map llvmTy types) ^ " }"
    | llvmTy (Ir.TRecord (fields, _)) =
        "{ " ^
        String.concatWith ", "
          (List.map (fn (_, ty) => llvmTy ty) fields) ^
        " }"
    | llvmTy (Ir.TData _) = "ptr"
    | llvmTy (Ir.TBorrow (Ir.TString, _)) = "{ ptr, i64, i1 }"
    | llvmTy (Ir.TBorrow _) = "ptr"
    | llvmTy Ir.TToken = "i8"
    | llvmTy Ir.TResource = "i8"
    | llvmTy Ir.TString = "{ ptr, i64, i1 }"
    | llvmTy Ir.TCPtr = "ptr"
    | llvmTy (Ir.TArray _) = "ptr"
    | llvmTy (Ir.TMap _) = "ptr"
    | llvmTy (Ir.TFun _) = "ptr"

  fun storageTy Ir.Unit = "i8"
    | storageTy ty = llvmTy ty

  fun isUnit (Ir.Unit) = true
    | isUnit _ = false

  fun boxedBorrowType ty =
    case ty of
      Ir.TData _ => true
    | Ir.TArray _ => true
    | Ir.TMap _ => true
    | _ => false

  fun floatText value =
    String.translate
      (fn #"E" => "e"
        | #"~" => "-"
        | ch => Char.toString ch)
      (Real.fmt (StringCvt.SCI (SOME 17)) value)

  fun typed (value : value) =
    if isUnit (#ty value) then "i8 0"
    else llvmTy (#ty value) ^ " " ^ #text value
end
