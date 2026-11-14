structure CheckBuiltins =
struct
  open CheckTypes

val builtinSpan = {start = Pos.start, stop = Pos.start}

fun isConversionBuiltin name =
  List.exists
    (fn width =>
      List.exists (fn candidate => candidate = name)
        (intToStringAliases width @ stringToIntAliases width))
    conversionWidths

fun parameterNames name =
  let
    fun oneOf names = List.exists (fn candidate => candidate = name) names
  in
    if isConversionBuiltin name then ["value"]
    else if oneOf
      [ "print_int", "print_int64", "print_float", "print_bool"
      , "print_string", "int64_to_int", "int64_to_int_wrapping"
      , "int_to_int64", "float_to_string", "string_copy", "string_of_int"
      ] then ["value"]
    else if oneOf
      [ "int64_wrapping_add", "int64_wrapping_sub", "int64_wrapping_mul"
      ] then ["left", "right"]
    else if name = "sprintf" then ["format", "value"]
    else if name = "os.exit" then ["code"]
    else if name = "io.read_file" then ["path"]
    else if name = "io.write_file" then ["path", "text"]
    else if oneOf
      [ "sys.reuseaddr", "sys.reuseport", "sys.nodelay", "sys.local_port"
      , "sys.close", "sys.set_nonblocking"
      ] then ["socket"]
    else if oneOf
      [ "sys.bind_loopback", "sys.bind_any", "sys.connect_loopback"
      ] then ["socket", "port"]
    else if name = "sys.listen" then ["socket", "backlog"]
    else if name = "sys.accept" then ["listener"]
    else if name = "sys.epoll_add" then ["epoll", "fd", "events"]
    else if oneOf ["sys.epoll_add_token", "sys.epoll_mod_token"] then
      ["epoll", "fd", "events", "token"]
    else if name = "sys.epoll_del" then ["epoll", "fd"]
    else if name = "sys.epoll_wait" then
      ["epoll", "fds", "events", "count", "timeout"]
    else if name = "sys.thread_spawn" then ["worker"]
    else if name = "sys.thread_join" then ["handle"]
    else if oneOf
      [ "sys.recv_into_raw", "sys.send_from_raw", "recv_into", "send_from"
      ] then ["socket", "bytes", "offset", "length"]
    else if oneOf ["sys.send_string_raw", "send_string"] then
      ["socket", "text", "offset", "length"]
    else if name = "bytes_from_string" then ["text"]
    else if name = "bytes_to_string" then ["bytes"]
    else if oneOf ["bytes_to_string_range", "bytes_header_end"] then
      ["bytes", "start", "length"]
    else if name = "bytes_write_string" then
      ["bytes", "offset", "length", "text"]
    else if name = "bytes_write_int" then
      ["bytes", "offset", "value", "width"]
    else if name = "string_length" then ["text"]
    else if name = "string_of_char" then ["character"]
    else if oneOf ["string_char_at", "string_char_at_unchecked"] then
      ["text", "index"]
    else if name = "string_slice" then ["text", "start", "length"]
    else if name = "string_eq" then ["left", "right"]
    else if name = "string_find_char" then ["text", "character"]
    else if name = "string_find_char_from" then
      ["text", "character", "start"]
    else if name = "print_span" then ["text", "start", "length"]
    else if name = "consume_token" then ["token"]
    else if name = "drop_resource" then ["resource"]
    else if name = "array_make" then ["length", "value"]
    else if name = "array_make_uninit" then ["length"]
    else if oneOf ["array_length", "array_length_borrow"] then ["array"]
    else if oneOf
      [ "array_get", "array_get_unchecked", "array_get_borrow"
      , "array_get_borrow_view", "array_get_copy_borrow", "array_take"
      , "array_kernel_load", "array_kernel_borrow", "array_kernel_take"
      ] then ["array", "index"]
    else if oneOf
      [ "array_set", "array_set_unchecked", "array_put", "array_put_arena"
      , "array_kernel_write"
      ] then ["array", "index", "value"]
    else if oneOf ["array_push", "array_push_unchecked"] then ["array", "value"]
    else if name = "array_kernel_new" then ["capacity"]
    else if oneOf ["array_kernel_length", "array_kernel_capacity"] then ["array"]
    else if name = "array_kernel_reserve" then ["array", "capacity"]
    else if name = "array_kernel_set_length" then ["array", "length"]
    else if name = "array_kernel_bounds_abort" then ["index", "length"]
    else if name = "sqlite_open" then ["path"]
    else if oneOf ["sqlite_close", "sqlite_changes", "sqlite_errmsg"] then
      ["database"]
    else if oneOf ["sqlite_exec", "sqlite_prepare"] then ["database", "sql"]
    else if oneOf
      [ "sqlite_finalize", "sqlite_step", "sqlite_column_count"
      ] then ["statement"]
    else if oneOf
      [ "sqlite_column_name", "sqlite_column_type", "sqlite_column_int"
      , "sqlite_column_text"
      ] then ["statement", "index"]
    else if oneOf ["map_length", "map_length_borrow", "map_keys"] then ["map"]
    else if oneOf
      [ "map_has", "map_get", "map_has_borrow", "map_get_borrow"
      , "map_get_borrow_view", "map_get_copy_borrow", "map_remove"
      ] then ["map", "key"]
    else if name = "map_set" then ["map", "key", "value"]
    else []
  end

fun conversionBuiltins optionOf () =
  List.concat
    (List.map
       (fn width =>
         List.map
           (fn name =>
             ( name
             , { name = name
               , params = [Types.TInt width]
               , result = Types.TString
               , span = builtinSpan
               , arity = 1
               , paramSpans = []
               , typeVars = []
               , generalized = ref []
               }
             ))
           (intToStringAliases width)
         @ List.map
             (fn name =>
               ( name
               , { name = name
                 , params = [Types.TBorrow (Types.TString, false, NONE)]
                 , result = optionOf (Types.TInt width)
                 , span = builtinSpan
                 , arity = 1
                 , paramSpans = []
                 , typeVars = []
                 , generalized = ref []
                 }
               ))
             (stringToIntAliases width))
       conversionWidths)

fun builtinFunctions optionOf ioResultOf () : (string * funSig) list =
  [ ( "print_int"
    , { name = "print_int"
      , params = [Types.TInt 32]
      , result = Types.TUnit
      , span = builtinSpan
      , arity = 1
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "print_int64"
    , { name = "print_int64"
      , params = [Types.TInt 64]
      , result = Types.TUnit
      , span = builtinSpan
      , arity = 1
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "int64_to_int"
    , { name = "int64_to_int"
      , params = [Types.TInt 64]
      , result = Types.TInt 32
      , span = builtinSpan
      , arity = 1
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "int64_to_int_wrapping"
    , { name = "int64_to_int_wrapping"
      , params = [Types.TInt 64]
      , result = Types.TInt 32
      , span = builtinSpan
      , arity = 1
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "int64_wrapping_add"
    , { name = "int64_wrapping_add"
      , params = [Types.TInt 64, Types.TInt 64]
      , result = Types.TInt 64
      , span = builtinSpan
      , arity = 2
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "int64_wrapping_sub"
    , { name = "int64_wrapping_sub"
      , params = [Types.TInt 64, Types.TInt 64]
      , result = Types.TInt 64
      , span = builtinSpan
      , arity = 2
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "int64_wrapping_mul"
    , { name = "int64_wrapping_mul"
      , params = [Types.TInt 64, Types.TInt 64]
      , result = Types.TInt 64
      , span = builtinSpan
      , arity = 2
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "int_to_int64"
    , { name = "int_to_int64"
      , params = [Types.TInt 32]
      , result = Types.TInt 64
      , span = builtinSpan
      , arity = 1
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "float_to_string"
    , { name = "float_to_string"
      , params = [Types.TFloat 64]
      , result = Types.TString
      , span = builtinSpan
      , arity = 1
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "sprintf"
    , { name = "sprintf"
      , params =
          [ Types.TString
          , Types.TVar (9200, ref Types.Unbound)
          ]
      , result = Types.TString
      , span = builtinSpan
      , arity = 2
      , paramSpans = []
      , typeVars = []
      , generalized = ref [(9200, Types.Ordinary)]
      }
    )
  , ( "print_float"
    , { name = "print_float"
      , params = [Types.TFloat 64]
      , result = Types.TUnit
      , span = builtinSpan
      , arity = 1
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "print_bool"
    , { name = "print_bool"
      , params = [Types.TBool]
      , result = Types.TUnit
      , span = builtinSpan
      , arity = 1
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "print_string"
    , { name = "print_string"
      , params = [Types.TString]
      , result = Types.TUnit
      , span = builtinSpan
      , arity = 1
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "os.exit"
    , { name = "os.exit"
      , params = [Types.TInt 32]
      , result = Types.TUnit
      , span = builtinSpan
      , arity = 1
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "io.read_file"
    , { name = "io.read_file"
      , params = [Types.TString]
      , result = ioResultOf (Types.TString)
      , span = builtinSpan
      , arity = 1
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "io.read_stdin"
    , { name = "io.read_stdin"
      , params = []
      , result = ioResultOf (Types.TString)
      , span = builtinSpan
      , arity = 0
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "io.write_file"
    , { name = "io.write_file"
      , params = [Types.TString, Types.TString]
      , result = ioResultOf (Types.TUnit)
      , span = builtinSpan
      , arity = 2
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "sys.socket_tcp"
    , { name = "sys.socket_tcp"
      , params = []
      , result = Types.TInt 32
      , span = builtinSpan
      , arity = 0
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "sys.reuseaddr"
    , { name = "sys.reuseaddr"
      , params = [Types.TInt 32]
      , result = Types.TInt 32
      , span = builtinSpan
      , arity = 1
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "sys.reuseport"
    , { name = "sys.reuseport"
      , params = [Types.TInt 32]
      , result = Types.TInt 32
      , span = builtinSpan
      , arity = 1
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "sys.nodelay"
    , { name = "sys.nodelay"
      , params = [Types.TInt 32]
      , result = Types.TInt 32
      , span = builtinSpan
      , arity = 1
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "sys.bind_loopback"
    , { name = "sys.bind_loopback"
      , params = [Types.TInt 32, Types.TInt 32]
      , result = Types.TInt 32
      , span = builtinSpan
      , arity = 2
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "sys.bind_any"
    , { name = "sys.bind_any"
      , params = [Types.TInt 32, Types.TInt 32]
      , result = Types.TInt 32
      , span = builtinSpan
      , arity = 2
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "sys.listen"
    , { name = "sys.listen"
      , params = [Types.TInt 32, Types.TInt 32]
      , result = Types.TInt 32
      , span = builtinSpan
      , arity = 2
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "sys.accept"
    , { name = "sys.accept"
      , params = [Types.TInt 32]
      , result = Types.TInt 32
      , span = builtinSpan
      , arity = 1
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "sys.connect_loopback"
    , { name = "sys.connect_loopback"
      , params = [Types.TInt 32, Types.TInt 32]
      , result = Types.TInt 32
      , span = builtinSpan
      , arity = 2
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "sys.local_port"
    , { name = "sys.local_port"
      , params = [Types.TInt 32]
      , result = Types.TInt 32
      , span = builtinSpan
      , arity = 1
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "sys.close"
    , { name = "sys.close"
      , params = [Types.TInt 32]
      , result = Types.TInt 32
      , span = builtinSpan
      , arity = 1
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "sys.set_nonblocking"
    , { name = "sys.set_nonblocking"
      , params = [Types.TInt 32]
      , result = Types.TInt 32
      , span = builtinSpan
      , arity = 1
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "sys.epoll_create"
    , { name = "sys.epoll_create"
      , params = []
      , result = Types.TInt 32
      , span = builtinSpan
      , arity = 0
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "sys.epoll_add"
    , { name = "sys.epoll_add"
      , params = [Types.TInt 32, Types.TInt 32, Types.TInt 32]
      , result = Types.TInt 32
      , span = builtinSpan
      , arity = 3
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "sys.epoll_add_token"
    , { name = "sys.epoll_add_token"
      , params =
          [ Types.TInt 32
          , Types.TInt 32
          , Types.TInt 32
          , Types.TInt 32
          ]
      , result = Types.TInt 32
      , span = builtinSpan
      , arity = 4
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "sys.epoll_mod_token"
    , { name = "sys.epoll_mod_token"
      , params =
          [ Types.TInt 32
          , Types.TInt 32
          , Types.TInt 32
          , Types.TInt 32
          ]
      , result = Types.TInt 32
      , span = builtinSpan
      , arity = 4
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "sys.epoll_del"
    , { name = "sys.epoll_del"
      , params = [Types.TInt 32, Types.TInt 32]
      , result = Types.TInt 32
      , span = builtinSpan
      , arity = 2
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "sys.epoll_wait"
    , { name = "sys.epoll_wait"
      , params =
          [ Types.TInt 32
          , Types.TArray (Types.TInt 32)
          , Types.TArray (Types.TInt 32)
          , Types.TInt 32
          , Types.TInt 32
          ]
      , result = Types.TInt 32
      , span = builtinSpan
      , arity = 5
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "sys.errno"
    , { name = "sys.errno"
      , params = []
      , result = Types.TInt 32
      , span = builtinSpan
      , arity = 0
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "sys.install_stop_signals"
    , { name = "sys.install_stop_signals"
      , params = []
      , result = Types.TUnit
      , span = builtinSpan
      , arity = 0
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "sys.stop_requested"
    , { name = "sys.stop_requested"
      , params = []
      , result = Types.TBool
      , span = builtinSpan
      , arity = 0
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "sys.sleep_ms"
    , { name = "sys.sleep_ms"
      , params = [Types.TInt 32]
      , result = Types.TUnit
      , span = builtinSpan
      , arity = 1
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "sys.scratch_enter"
    , { name = "sys.scratch_enter"
      , params = []
      , result = Types.TUnit
      , span = builtinSpan
      , arity = 0
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "sys.scratch_enter_all"
    , { name = "sys.scratch_enter_all"
      , params = []
      , result = Types.TUnit
      , span = builtinSpan
      , arity = 0
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "sys.scratch_leave"
    , { name = "sys.scratch_leave"
      , params = []
      , result = Types.TUnit
      , span = builtinSpan
      , arity = 0
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "sys.thread_spawn"
    , { name = "sys.thread_spawn"
      , params =
          [ Types.TFun ([Types.TInt 64], Types.TUnit)
          , Types.TInt 64
          ]
      , result = Types.TInt 64
      , span = builtinSpan
      , arity = 2
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "sys.thread_join"
    , { name = "sys.thread_join"
      , params = [Types.TInt 64]
      , result = Types.TUnit
      , span = builtinSpan
      , arity = 1
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "sys.recv_into_raw"
    , { name = "sys.recv_into_raw"
      , params =
          [ Types.TInt 32
          , Types.TArray (Types.TInt 8)
          , Types.TInt 32
          , Types.TInt 32
          ]
      , result = Types.TInt 32
      , span = builtinSpan
      , arity = 4
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "sys.send_string_raw"
    , { name = "sys.send_string_raw"
      , params =
          [ Types.TInt 32
          , Types.TBorrow (Types.TString, false, NONE)
          , Types.TInt 32
          , Types.TInt 32
          ]
      , result = Types.TInt 32
      , span = builtinSpan
      , arity = 4
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "bytes_from_string"
    , { name = "bytes_from_string"
      , params = [Types.TBorrow (Types.TString, false, NONE)]
      , result = Types.TArray (Types.TInt 8)
      , span = builtinSpan
      , arity = 1
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "bytes_to_string"
    , { name = "bytes_to_string"
      , params = [Types.TArray (Types.TInt 8)]
      , result = Types.TString
      , span = builtinSpan
      , arity = 1
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "bytes_to_string_range"
    , { name = "bytes_to_string_range"
      , params =
          [ Types.TArray (Types.TInt 8)
          , Types.TInt 32
          , Types.TInt 32
          ]
      , result = Types.TString
      , span = builtinSpan
      , arity = 3
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "send_string"
    , { name = "send_string"
      , params =
          [ Types.TInt 32
          , Types.TBorrow (Types.TString, false, NONE)
          , Types.TInt 32
          , Types.TInt 32
          ]
      , result = ioResultOf (Types.TInt 32)
      , span = builtinSpan
      , arity = 4
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "bytes_header_end"
    , { name = "bytes_header_end"
      , params =
          [ Types.TArray (Types.TInt 8)
          , Types.TInt 32
          , Types.TInt 32
          ]
      , result = Types.TInt 32
      , span = builtinSpan
      , arity = 3
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "bytes_write_string"
    , { name = "bytes_write_string"
      , params =
          [ Types.TArray (Types.TInt 8)
          , Types.TInt 32
          , Types.TInt 32
          , Types.TBorrow (Types.TString, false, NONE)
          ]
      , result = Types.TInt 32
      , span = builtinSpan
      , arity = 4
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "bytes_write_int"
    , { name = "bytes_write_int"
      , params =
          [ Types.TArray (Types.TInt 8)
          , Types.TInt 32
          , Types.TInt 32
          , Types.TInt 32
          ]
      , result = Types.TInt 32
      , span = builtinSpan
      , arity = 4
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "sys.send_from_raw"
    , { name = "sys.send_from_raw"
      , params =
          [ Types.TInt 32
          , Types.TArray (Types.TInt 8)
          , Types.TInt 32
          , Types.TInt 32
          ]
      , result = Types.TInt 32
      , span = builtinSpan
      , arity = 4
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "recv_into"
    , { name = "recv_into"
      , params =
          [ Types.TInt 32
          , Types.TArray (Types.TInt 8)
          , Types.TInt 32
          , Types.TInt 32
          ]
      , result = ioResultOf (Types.TInt 32)
      , span = builtinSpan
      , arity = 4
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "send_from"
    , { name = "send_from"
      , params =
          [ Types.TInt 32
          , Types.TArray (Types.TInt 8)
          , Types.TInt 32
          , Types.TInt 32
          ]
      , result = ioResultOf (Types.TInt 32)
      , span = builtinSpan
      , arity = 4
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "string_copy"
    , { name = "string_copy"
      , params = [Types.TBorrow (Types.TString, false, NONE)]
      , result = Types.TString
      , span = builtinSpan
      , arity = 1
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "string_length"
    , { name = "string_length"
      , params = [Types.TBorrow (Types.TString, false, NONE)]
      , result = Types.TInt 32
      , span = builtinSpan
      , arity = 1
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "string_of_char"
    , { name = "string_of_char"
      , params = [Types.TInt 32]
      , result = Types.TString
      , span = builtinSpan
      , arity = 1
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "string_of_int"
    , { name = "string_of_int"
      , params = [Types.TInt 32]
      , result = Types.TString
      , span = builtinSpan
      , arity = 1
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "string_char_at"
    , { name = "string_char_at"
      , params =
          [ Types.TBorrow (Types.TString, false, NONE)
          , Types.TInt 32
          ]
      , result = Types.TInt 32
      , span = builtinSpan
      , arity = 2
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "string_char_at_unchecked"
    , { name = "string_char_at_unchecked"
      , params =
          [ Types.TBorrow (Types.TString, false, NONE)
          , Types.TInt 32
          ]
      , result = Types.TInt 32
      , span = builtinSpan
      , arity = 2
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "string_slice"
    , { name = "string_slice"
      , params =
          [ Types.TBorrow (Types.TString, false, NONE)
          , Types.TInt 32
          , Types.TInt 32
          ]
      , result = Types.TString
      , span = builtinSpan
      , arity = 3
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "string_eq"
    , { name = "string_eq"
      , params =
          [ Types.TBorrow (Types.TString, false, NONE)
          , Types.TBorrow (Types.TString, false, NONE)
          ]
      , result = Types.TBool
      , span = builtinSpan
      , arity = 2
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "string_find_char_from"
    , { name = "string_find_char_from"
      , params =
          [ Types.TBorrow (Types.TString, false, NONE)
          , Types.TInt 32
          , Types.TInt 32
          ]
      , result = Types.TInt 32
      , span = builtinSpan
      , arity = 3
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "string_find_char"
    , { name = "string_find_char"
      , params =
          [ Types.TBorrow (Types.TString, false, NONE)
          , Types.TInt 32
          ]
      , result = Types.TInt 32
      , span = builtinSpan
      , arity = 2
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "print_span"
    , { name = "print_span"
      , params =
          [ Types.TBorrow (Types.TString, false, NONE)
          , Types.TInt 32
          , Types.TInt 32
          ]
      , result = Types.TUnit
      , span = builtinSpan
      , arity = 3
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "new_token"
    , { name = "new_token"
      , params = []
      , result = Types.TToken
      , span = builtinSpan
      , arity = 0
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "consume_token"
    , { name = "consume_token"
      , params = [Types.TToken]
      , result = Types.TUnit
      , span = builtinSpan
      , arity = 1
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "new_resource"
    , { name = "new_resource"
      , params = []
      , result = Types.TResource
      , span = builtinSpan
      , arity = 0
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "drop_resource"
    , { name = "drop_resource"
      , params = [Types.TResource]
      , result = Types.TUnit
      , span = builtinSpan
      , arity = 1
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "array_make"
    , { name = "array_make"
      , params = [Types.TVar (9001, ref Types.Unbound), Types.TInt 32]
      , result = Types.TArray (Types.TVar (9001, ref Types.Unbound))
      , span = builtinSpan
      , arity = 2
      , paramSpans = []
      , typeVars = []
      , generalized = ref [(9001, Types.Ordinary)]
      }
    )
  , ( "array_make_uninit"
    , { name = "array_make_uninit"
      , params = [Types.TInt 32]
      , result = Types.TArray (Types.TVar (9011, ref Types.Unbound))
      , span = builtinSpan
      , arity = 1
      , paramSpans = []
      , typeVars = []
      , generalized = ref [(9011, Types.Ordinary)]
      }
    )
  , ( "array_length"
    , { name = "array_length"
      , params = [Types.TArray (Types.TVar (9002, ref Types.Unbound))]
      , result = Types.TInt 32
      , span = builtinSpan
      , arity = 1
      , paramSpans = []
      , typeVars = []
      , generalized = ref [(9002, Types.Ordinary)]
      }
    )
  , ( "array_length_borrow"
    , { name = "array_length_borrow"
      , params =
          [ Types.TBorrow
              (Types.TArray (Types.TVar (9011, ref Types.Unbound)),
               false, NONE)
          ]
      , result = Types.TInt 32
      , span = builtinSpan
      , arity = 1
      , paramSpans = []
      , typeVars = []
      , generalized = ref [(9011, Types.Ordinary)]
      }
    )
  , ( "array_get"
    , { name = "array_get"
      , params = [Types.TArray (Types.TVar (9003, ref Types.Unbound)), Types.TInt 32]
      , result = Types.TVar (9003, ref Types.Unbound)
      , span = builtinSpan
      , arity = 2
      , paramSpans = []
      , typeVars = []
      , generalized = ref [(9003, Types.Ordinary)]
      }
    )
  , ( "array_get_unchecked"
    , { name = "array_get_unchecked"
      , params = [Types.TArray (Types.TVar (9004, ref Types.Unbound)), Types.TInt 32]
      , result = Types.TVar (9004, ref Types.Unbound)
      , span = builtinSpan
      , arity = 2
      , paramSpans = []
      , typeVars = []
      , generalized = ref [(9004, Types.Ordinary)]
      }
    )
  , ( "array_set"
    , { name = "array_set"
      , params = [Types.TArray (Types.TVar (9005, ref Types.Unbound)), Types.TInt 32, Types.TVar (9005, ref Types.Unbound)]
      , result = Types.TUnit
      , span = builtinSpan
      , arity = 3
      , paramSpans = []
      , typeVars = []
      , generalized = ref [(9005, Types.Ordinary)]
      }
    )
  , ( "array_set_unchecked"
    , { name = "array_set_unchecked"
      , params = [Types.TArray (Types.TVar (9006, ref Types.Unbound)), Types.TInt 32, Types.TVar (9006, ref Types.Unbound)]
      , result = Types.TUnit
      , span = builtinSpan
      , arity = 3
      , paramSpans = []
      , typeVars = []
      , generalized = ref [(9006, Types.Ordinary)]
      }
    )
  , ( "array_push"
    , { name = "array_push"
      , params = [Types.TArray (Types.TVar (9007, ref Types.Unbound)), Types.TVar (9007, ref Types.Unbound)]
      , result = Types.TUnit
      , span = builtinSpan
      , arity = 2
      , paramSpans = []
      , typeVars = []
      , generalized = ref [(9007, Types.Ordinary)]
      }
    )
  , ( "array_push_unchecked"
    , { name = "array_push_unchecked"
      , params = [Types.TArray (Types.TVar (9008, ref Types.Unbound)), Types.TVar (9008, ref Types.Unbound)]
      , result = Types.TUnit
      , span = builtinSpan
      , arity = 2
      , paramSpans = []
      , typeVars = []
      , generalized = ref [(9008, Types.Ordinary)]
      }
    )
  , ( "array_get_borrow"
    , { name = "array_get_borrow"
      , params = [Types.TArray (Types.TVar (9009, ref Types.Unbound)), Types.TInt 32]
      , result = Types.TBorrow (Types.TVar (9009, ref Types.Unbound), false, SOME 1)
      , span = builtinSpan
      , arity = 2
      , paramSpans = []
      , typeVars = []
      , generalized = ref [(9009, Types.Ordinary)]
      }
    )
  , ( "array_get_borrow_view"
    , { name = "array_get_borrow_view"
      , params =
          [ Types.TBorrow
              (Types.TArray (Types.TVar (9010, ref Types.Unbound)),
               false, NONE)
          , Types.TInt 32
          ]
      , result =
          Types.TBorrow
            (Types.TVar (9010, ref Types.Unbound), false, SOME 1)
      , span = builtinSpan
      , arity = 2
      , paramSpans = []
      , typeVars = []
      , generalized = ref [(9010, Types.Ordinary)]
      }
    )
  , ( "array_get_copy_borrow"
    , { name = "array_get_copy_borrow"
      , params =
          [ Types.TBorrow
              (Types.TArray (Types.TVar (9012, ref Types.Unbound)),
               false, NONE)
          , Types.TInt 32
          ]
      , result = Types.TVar (9012, ref Types.Unbound)
      , span = builtinSpan
      , arity = 2
      , paramSpans = []
      , typeVars = []
      , generalized = ref [(9012, Types.Ordinary)]
      }
    )
  , ( "array_put"
    , { name = "array_put"
      , params =
          [ Types.TArray (Types.TVar (9012, ref Types.Unbound))
          , Types.TInt 32
          , Types.TVar (9012, ref Types.Unbound)
          ]
      , result = Types.TUnit
      , span = builtinSpan
      , arity = 3
      , paramSpans = []
      , typeVars = []
      , generalized = ref [(9012, Types.Ordinary)]
      }
    )
  , ( "array_put_arena"
    , { name = "array_put_arena"
      , params =
          [ Types.TArray (Types.TVar (9013, ref Types.Unbound))
          , Types.TInt 32
          , Types.TVar (9013, ref Types.Unbound)
          ]
      , result = Types.TUnit
      , span = builtinSpan
      , arity = 3
      , paramSpans = []
      , typeVars = []
      , generalized = ref [(9013, Types.Ordinary)]
      }
    )
  , ( "array_take"
    , { name = "array_take"
      , params = [Types.TArray (Types.TVar (9011, ref Types.Unbound)), Types.TInt 32]
      , result = Types.TVar (9011, ref Types.Unbound)
      , span = builtinSpan
      , arity = 2
      , paramSpans = []
      , typeVars = []
      , generalized = ref [(9011, Types.Ordinary)]
      }
    )

  , ( "array_kernel_new"
    , { name = "array_kernel_new"
      , params = [Types.TInt 32]
      , result = Types.TArray (Types.TVar (9020, ref Types.Unbound))
      , span = builtinSpan
      , arity = 1
      , paramSpans = []
      , typeVars = []
      , generalized = ref [(9020, Types.Ordinary)]
      }
    )
  , ( "array_kernel_length"
    , { name = "array_kernel_length"
      , params =
          [ Types.TBorrow
              (Types.TArray (Types.TVar (9021, ref Types.Unbound)),
               false, NONE)
          ]
      , result = Types.TInt 32
      , span = builtinSpan
      , arity = 1
      , paramSpans = []
      , typeVars = []
      , generalized = ref [(9021, Types.Ordinary)]
      }
    )
  , ( "array_kernel_capacity"
    , { name = "array_kernel_capacity"
      , params =
          [ Types.TBorrow
              (Types.TArray (Types.TVar (9022, ref Types.Unbound)),
               false, NONE)
          ]
      , result = Types.TInt 32
      , span = builtinSpan
      , arity = 1
      , paramSpans = []
      , typeVars = []
      , generalized = ref [(9022, Types.Ordinary)]
      }
    )
  , ( "array_kernel_load"
    , { name = "array_kernel_load"
      , params =
          [ Types.TBorrow
              (Types.TArray (Types.TVar (9023, ref Types.Unbound)),
               false, NONE)
          , Types.TInt 32
          ]
      , result = Types.TVar (9023, ref Types.Unbound)
      , span = builtinSpan
      , arity = 2
      , paramSpans = []
      , typeVars = []
      , generalized = ref [(9023, Types.Ordinary)]
      }
    )
  , ( "array_kernel_borrow"
    , { name = "array_kernel_borrow"
      , params =
          [ Types.TBorrow
              (Types.TArray (Types.TVar (9024, ref Types.Unbound)),
               false, NONE)
          , Types.TInt 32
          ]
      , result =
          Types.TBorrow
            (Types.TVar (9024, ref Types.Unbound), false, SOME 1)
      , span = builtinSpan
      , arity = 2
      , paramSpans = []
      , typeVars = []
      , generalized = ref [(9024, Types.Ordinary)]
      }
    )
  , ( "array_kernel_take"
    , { name = "array_kernel_take"
      , params =
          [ Types.TArray (Types.TVar (9025, ref Types.Unbound))
          , Types.TInt 32
          ]
      , result = Types.TVar (9025, ref Types.Unbound)
      , span = builtinSpan
      , arity = 2
      , paramSpans = []
      , typeVars = []
      , generalized = ref [(9025, Types.Ordinary)]
      }
    )
  , ( "array_kernel_write"
    , { name = "array_kernel_write"
      , params =
          [ Types.TArray (Types.TVar (9026, ref Types.Unbound))
          , Types.TInt 32
          , Types.TVar (9026, ref Types.Unbound)
          ]
      , result = Types.TUnit
      , span = builtinSpan
      , arity = 3
      , paramSpans = []
      , typeVars = []
      , generalized = ref [(9026, Types.Ordinary)]
      }
    )
  , ( "array_kernel_reserve"
    , { name = "array_kernel_reserve"
      , params =
          [ Types.TArray (Types.TVar (9027, ref Types.Unbound))
          , Types.TInt 32
          ]
      , result = Types.TUnit
      , span = builtinSpan
      , arity = 2
      , paramSpans = []
      , typeVars = []
      , generalized = ref [(9027, Types.Ordinary)]
      }
    )
  , ( "array_kernel_set_length"
    , { name = "array_kernel_set_length"
      , params =
          [ Types.TArray (Types.TVar (9028, ref Types.Unbound))
          , Types.TInt 32
          ]
      , result = Types.TUnit
      , span = builtinSpan
      , arity = 2
      , paramSpans = []
      , typeVars = []
      , generalized = ref [(9028, Types.Ordinary)]
      }
    )
  , ( "array_kernel_bounds_abort"
    , { name = "array_kernel_bounds_abort"
      , params = [Types.TInt 32, Types.TInt 32]
      , result = Types.TUnit
      , span = builtinSpan
      , arity = 2
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "sqlite_open"
    , { name = "sqlite_open"
      , params = [Types.TString]
      , result = Types.TTuple [Types.TInt 32, Types.TInt 64]
      , span = builtinSpan
      , arity = 1
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "sqlite_close"
    , { name = "sqlite_close"
      , params = [Types.TInt 64]
      , result = Types.TInt 32
      , span = builtinSpan
      , arity = 1
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "sqlite_exec"
    , { name = "sqlite_exec"
      , params = [Types.TInt 64, Types.TString]
      , result = Types.TInt 32
      , span = builtinSpan
      , arity = 2
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "sqlite_prepare"
    , { name = "sqlite_prepare"
      , params = [Types.TInt 64, Types.TString]
      , result = Types.TInt 64
      , span = builtinSpan
      , arity = 2
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "sqlite_finalize"
    , { name = "sqlite_finalize"
      , params = [Types.TInt 64]
      , result = Types.TInt 32
      , span = builtinSpan
      , arity = 1
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "sqlite_step"
    , { name = "sqlite_step"
      , params = [Types.TInt 64]
      , result = Types.TInt 32
      , span = builtinSpan
      , arity = 1
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "sqlite_column_count"
    , { name = "sqlite_column_count"
      , params = [Types.TInt 64]
      , result = Types.TInt 32
      , span = builtinSpan
      , arity = 1
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "sqlite_column_name"
    , { name = "sqlite_column_name"
      , params = [Types.TInt 64, Types.TInt 32]
      , result = Types.TString
      , span = builtinSpan
      , arity = 2
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "sqlite_column_type"
    , { name = "sqlite_column_type"
      , params = [Types.TInt 64, Types.TInt 32]
      , result = Types.TInt 32
      , span = builtinSpan
      , arity = 2
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "sqlite_column_int"
    , { name = "sqlite_column_int"
      , params = [Types.TInt 64, Types.TInt 32]
      , result = Types.TInt 32
      , span = builtinSpan
      , arity = 2
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "sqlite_column_text"
    , { name = "sqlite_column_text"
      , params = [Types.TInt 64, Types.TInt 32]
      , result = Types.TString
      , span = builtinSpan
      , arity = 2
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "sqlite_changes"
    , { name = "sqlite_changes"
      , params = [Types.TInt 64]
      , result = Types.TInt 32
      , span = builtinSpan
      , arity = 1
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "sqlite_errmsg"
    , { name = "sqlite_errmsg"
      , params = [Types.TInt 64]
      , result = Types.TString
      , span = builtinSpan
      , arity = 1
      , paramSpans = []
      , typeVars = []
      , generalized = ref []
      }
    )
  , ( "array_new"
    , { name = "array_new"
      , params = []
      , result = Types.TArray (Types.TVar (9010, ref Types.Unbound))
      , span = builtinSpan
      , arity = 0
      , paramSpans = []
      , typeVars = []
      , generalized = ref [(9010, Types.Ordinary)]
      }
    )
  , ( "map_new"
    , { name = "map_new"
      , params = []
      , result = Types.TMap (Types.TVar (9100, ref Types.Unbound))
      , span = builtinSpan
      , arity = 0
      , paramSpans = []
      , typeVars = []
      , generalized = ref [(9100, Types.Ordinary)]
      }
    )
  , ( "map_length"
    , { name = "map_length"
      , params = [Types.TMap (Types.TVar (9101, ref Types.Unbound))]
      , result = Types.TInt 32
      , span = builtinSpan
      , arity = 1
      , paramSpans = []
      , typeVars = []
      , generalized = ref [(9101, Types.Ordinary)]
      }
    )
  , ( "map_length_borrow"
    , { name = "map_length_borrow"
      , params =
          [ Types.TBorrow
              (Types.TMap (Types.TVar (9106, ref Types.Unbound)),
               false, NONE)
          ]
      , result = Types.TInt 32
      , span = builtinSpan
      , arity = 1
      , paramSpans = []
      , typeVars = []
      , generalized = ref [(9106, Types.Ordinary)]
      }
    )
  , ( "map_has"
    , { name = "map_has"
      , params =
          [ Types.TMap (Types.TVar (9102, ref Types.Unbound))
          , Types.TString
          ]
      , result = Types.TBool
      , span = builtinSpan
      , arity = 2
      , paramSpans = []
      , typeVars = []
      , generalized = ref [(9102, Types.Ordinary)]
      }
    )
  , ( "map_get"
    , { name = "map_get"
      , params =
          [ Types.TMap (Types.TVar (9103, ref Types.Unbound))
          , Types.TString
          ]
      , result = Types.TVar (9103, ref Types.Unbound)
      , span = builtinSpan
      , arity = 2
      , paramSpans = []
      , typeVars = []
      , generalized = ref [(9103, Types.Ordinary)]
      }
    )
  , ( "map_has_borrow"
    , { name = "map_has_borrow"
      , params =
          [ Types.TBorrow
              (Types.TMap (Types.TVar (9107, ref Types.Unbound)),
               false, NONE)
          , Types.TString
          ]
      , result = Types.TBool
      , span = builtinSpan
      , arity = 2
      , paramSpans = []
      , typeVars = []
      , generalized = ref [(9107, Types.Ordinary)]
      }
    )
  , ( "map_get_borrow"
    , { name = "map_get_borrow"
      , params =
          [ Types.TMap (Types.TVar (9104, ref Types.Unbound))
          , Types.TString
          ]
      , result =
          Types.TBorrow
            (Types.TVar (9104, ref Types.Unbound), false, SOME 1)
      , span = builtinSpan
      , arity = 2
      , paramSpans = []
      , typeVars = []
      , generalized = ref [(9104, Types.Ordinary)]
      }
    )
  , ( "map_set"
    , { name = "map_set"
      , params =
          [ Types.TMap (Types.TVar (9105, ref Types.Unbound))
          , Types.TString
          , Types.TVar (9105, ref Types.Unbound)
          ]
      , result = Types.TUnit
      , span = builtinSpan
      , arity = 3
      , paramSpans = []
      , typeVars = []
      , generalized = ref [(9105, Types.Ordinary)]
      }
    )
  , ( "map_get_borrow_view"
    , { name = "map_get_borrow_view"
      , params =
          [ Types.TBorrow
              (Types.TMap (Types.TVar (9108, ref Types.Unbound)),
               false, NONE)
          , Types.TString
          ]
      , result =
          Types.TBorrow
            (Types.TVar (9108, ref Types.Unbound), false, SOME 1)
      , span = builtinSpan
      , arity = 2
      , paramSpans = []
      , typeVars = []
      , generalized = ref [(9108, Types.Ordinary)]
      }
    )
  , ( "map_get_copy_borrow"
    , { name = "map_get_copy_borrow"
      , params =
          [ Types.TBorrow
              (Types.TMap (Types.TVar (9109, ref Types.Unbound)),
               false, NONE)
          , Types.TString
          ]
      , result = Types.TVar (9109, ref Types.Unbound)
      , span = builtinSpan
      , arity = 2
      , paramSpans = []
      , typeVars = []
      , generalized = ref [(9109, Types.Ordinary)]
      }
    )
  , ( "map_remove"
    , { name = "map_remove"
      , params =
          [ Types.TMap (Types.TVar (9106, ref Types.Unbound))
          , Types.TString
          ]
      , result = Types.TUnit
      , span = builtinSpan
      , arity = 2
      , paramSpans = []
      , typeVars = []
      , generalized = ref [(9106, Types.Ordinary)]
      }
    )
  , ( "map_keys"
    , { name = "map_keys"
      , params = [Types.TMap (Types.TVar (9110, ref Types.Unbound))]
      , result = Types.TArray Types.TString
      , span = builtinSpan
      , arity = 1
      , paramSpans = []
      , typeVars = []
      , generalized = ref [(9110, Types.Ordinary)]
      }
    )
  ] @ conversionBuiltins optionOf ()

  fun functions optionOf ioResultOf : (string * funSig) list =
    builtinFunctions optionOf ioResultOf ()

end
