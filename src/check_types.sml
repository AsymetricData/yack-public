structure CheckTypes =
struct
  type funSig =
    { name : string
    , params : Types.ty list
    , result : Types.ty
    , span : Diag.span
    , arity : int
    , paramSpans : Diag.span list
    , typeVars : (string * Types.ty) list
    , generalized : (int * Types.varKind) list ref
    }

  type declaredType =
    { name : string
    , declared : Types.property
    , effective : Types.property
    , fields : (string * Types.ty) list
    , span : Diag.span
    }

  type declaredData =
    { name : string
    , params : (string * bool) list
    , paramTypes : Types.ty list
    , declared : Types.property
    , effective : Types.property
    , constructors : (string * Types.ty option) list
    , span : Diag.span

    , prelude : bool
    }

  type declaredAlias =
    { name : string
    , property : Types.property
    , body : string
    , span : Diag.span
    }

  type callInfo =
    { span : Diag.span
    , name : string
    , substitution : (int * Types.ty) list
    , params : Types.ty list
    , result : Types.ty
    }

  type functionValue =
    { span : Diag.span
    , name : string
    , substitution : (int * Types.ty) list
    }

  type aliasInfo =
    { params : Types.ty list
    , body : Types.ty
    , span : Diag.span
    }

  type result =
    { diagnostics : Diag.t list
    , functions : (string * funSig) list
    , bindingTypes : (Diag.span * Types.ty) list
    , expressionTypes : (Diag.span * Types.ty) list
    , calls : callInfo list
    , functionValues : functionValue list
    , applications : (Diag.span * Types.ty) list
    , types : declaredType list
    , aliases : declaredAlias list
    , datatypes : declaredData list
    }

  val conversionWidths = [1, 2, 4, 8, 16, 32, 64]

  fun intToStringAliases width =
    if width = 32 then ["int_to_string"]
    else ["int" ^ Int.toString width ^ "_to_string"]

  fun stringToIntAliases width =
    if width = 32 then ["string_to_int"]
    else ["string_to_int" ^ Int.toString width]

  val conversionBuiltinNames =
    List.concat
      (List.map
         (fn width => intToStringAliases width @ stringToIntAliases width)
         conversionWidths)

  val builtins =
    [ "print_int", "print_int64", "print_float", "print_bool", "print_string"
    , "int64_to_int", "int_to_int64", "int64_to_int_wrapping", "int64_wrapping_add", "int64_wrapping_sub", "int64_wrapping_mul"
    , "float_to_string", "string_copy", "sprintf"
    , "os.exit"
    , "new_token", "consume_token"
    , "new_resource", "drop_resource"
    , "sys.thread_spawn", "sys.thread_join"
    , "sys.install_stop_signals", "sys.stop_requested", "sys.sleep_ms", "sys.monotonic_ms"
    , "sys.realtime_ms"
    , "array_new", "array_make", "array_make_uninit", "array_length"
    , "array_get", "array_get_unchecked", "array_get_borrow", "array_take", "array_put"
    , "array_put_arena"
    , "array_set", "array_set_unchecked", "array_push", "array_push_unchecked"
    , "extern_view", "extern_view_ptr"
    , "c_ptr.of_int", "c_ptr.is_null"
    , "map_new", "map_length", "map_has", "map_get", "map_get_borrow"
    , "map_set", "map_remove", "map_keys"
    , "sqlite_open", "sqlite_close", "sqlite_exec", "sqlite_prepare"
    , "sqlite_finalize", "sqlite_step", "sqlite_column_count"
    , "sqlite_column_name", "sqlite_column_type", "sqlite_column_int"
    , "sqlite_column_text", "sqlite_changes", "sqlite_errmsg"
    ] @ conversionBuiltinNames

  fun lookup name functions =
    List.find (fn (candidate, _) => candidate = name) functions
end
