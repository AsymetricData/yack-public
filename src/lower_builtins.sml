structure LowerBuiltins =
struct
datatype arrayColor =
    ArrayAlloc
  | ArrayAllocRooted
  | ArrayAllocUninit
  | ArrayRead
  | ArrayBorrowRead
  | ArrayMoveOut
  | ArrayStoreRooted
  | ArrayStoreDeferred

  | ArrayMutate

fun arrayColor name =
  case name of
    "array_new" => SOME ArrayAlloc
  | "array_make" => SOME ArrayAllocRooted
  | "array_make_uninit" => SOME ArrayAllocUninit
  | "array_length" => SOME ArrayRead
  | "array_length_borrow" => SOME ArrayRead
  | "array_get" => SOME ArrayRead
  | "array_get_unchecked" => SOME ArrayRead
  | "array_get_borrow" => SOME ArrayBorrowRead
  | "array_get_borrow_view" => SOME ArrayBorrowRead
  | "array_get_copy_borrow" => SOME ArrayRead
  | "array_take" => SOME ArrayMoveOut
  | "array_set" => SOME ArrayStoreRooted
  | "array_set_unchecked" => SOME ArrayStoreRooted
  | "array_push" => SOME ArrayStoreRooted
  | "array_push_unchecked" => SOME ArrayStoreRooted
  | "array_put" => SOME ArrayStoreRooted
  | "array_put_arena" => SOME ArrayStoreDeferred

  | "array_kernel_new" => SOME ArrayAllocUninit
  | "array_kernel_length" => SOME ArrayRead
  | "array_kernel_capacity" => SOME ArrayRead
  | "array_kernel_load" => SOME ArrayRead
  | "array_kernel_borrow" => SOME ArrayBorrowRead
  | "array_kernel_take" => SOME ArrayMoveOut
  | "array_kernel_drop" => SOME ArrayMutate
  | "array_kernel_write" => SOME ArrayStoreRooted
  | "array_kernel_reserve" => SOME ArrayMutate
  | "array_kernel_set_length" => SOME ArrayMutate
  | "array_kernel_bounds_abort" => SOME ArrayRead
  | _ => NONE

fun isArrayBuiltin name = Option.isSome (arrayColor name)

fun isReservedArrayKernel name =
  String.isPrefix "array_kernel_" name

fun isReservedBytes name =
  name = "bytes_to_string_range" orelse
  name = "bytes_header_end" orelse
  name = "bytes_write_string" orelse
  name = "bytes_write_int"

fun isReservedScratch name =
  name = "sys.scratch_enter" orelse
  name = "sys.scratch_enter_all" orelse
  name = "sys.scratch_leave"

fun isReservedExternView name =
  name = "extern_view" orelse name = "extern_view_ptr"

fun arrayStoreIndex name =
  case name of
    "array_make" => SOME 0
  | "array_push" => SOME 1
  | "array_push_unchecked" => SOME 1
  | "array_set" => SOME 2
  | "array_set_unchecked" => SOME 2
  | "array_put" => SOME 2
  | "array_put_arena" => SOME 2
  | "array_kernel_write" => SOME 2
  | _ => NONE

fun arrayMutatesHeader name =
  name = "array_kernel_reserve" orelse
  name = "array_kernel_set_length" orelse
  name = "array_kernel_take" orelse
  name = "array_kernel_drop"

fun arrayRootsStoredValue name =
  case arrayColor name of
    SOME ArrayAllocRooted => true
  | SOME ArrayStoreRooted => true
  | _ => false

fun arrayEscapesStoredValue name =
  case arrayColor name of
    SOME ArrayAllocRooted => true
  | SOME ArrayStoreRooted => true
  | SOME ArrayStoreDeferred => true
  | _ => false

fun arrayMovesStoredValue name =
  case arrayColor name of
    SOME ArrayStoreRooted => true
  | SOME ArrayStoreDeferred => true
  | _ => false

fun isMapBuiltin name =
  name = "map_new" orelse
  name = "map_length" orelse
  name = "map_length_borrow" orelse
  name = "map_has" orelse
  name = "map_has_borrow" orelse
  name = "map_get" orelse
  name = "map_get_borrow" orelse
  name = "map_get_borrow_view" orelse
  name = "map_get_copy_borrow" orelse
  name = "map_set" orelse
  name = "map_remove" orelse
  name = "map_keys"

fun isConversionBuiltin name =
  name = "int_to_string" orelse name = "string_to_int" orelse
  name = "float_to_string" orelse
  List.exists
    (fn width =>
      width <> 32 andalso
      (name = "int" ^ Int.toString width ^ "_to_string" orelse
       name = "string_to_int" ^ Int.toString width))
    [1, 2, 4, 8, 16, 32, 64]

fun isStringBuiltin name =
  name = "io.read_file" orelse
  name = "io.read_stdin" orelse
  name = "io.write_file" orelse
  name = "string_copy" orelse
  name = "string_length" orelse
  name = "string_of_char" orelse
  name = "string_of_int" orelse
  name = "string_char_at" orelse
  name = "string_char_at_unchecked" orelse
  name = "string_slice" orelse
  name = "string_eq" orelse
  name = "string_find_char" orelse
  name = "string_find_char_from" orelse
  isConversionBuiltin name orelse
  name = "print_span"

fun stringProduces name =
  name = "string_copy" orelse
  name = "string_slice" orelse
  name = "string_of_char" orelse
  name = "string_of_int" orelse
  name = "float_to_string" orelse
  name = "int_to_string" orelse
  name = "io.read_file" orelse
  name = "io.read_stdin" orelse
  List.exists
    (fn width =>
      width <> 32 andalso
      name = "int" ^ Int.toString width ^ "_to_string")
    [1, 2, 4, 8, 16, 32, 64]

fun stringProducesEscaping name =
  name = "io.read_file" orelse name = "io.read_stdin"

fun bytesProducesString name =
  name = "bytes_to_string" orelse name = "bytes_to_string_range"

fun stringNeedsRegion name =
  stringProduces name orelse
  name = "io.write_file" orelse
  (isConversionBuiltin name andalso not (stringProduces name))

fun isBytesBuiltin name =
  name = "recv_into" orelse
  name = "send_from" orelse
  name = "bytes_from_string" orelse
  name = "bytes_to_string" orelse
  name = "bytes_to_string_range" orelse
  name = "bytes_header_end" orelse
  name = "bytes_write_string" orelse
  name = "bytes_write_int"

fun isSysBuiltin name =
  name = "sys.socket_tcp" orelse
  name = "sys.reuseaddr" orelse
  name = "sys.reuseport" orelse
  name = "sys.nodelay" orelse
  name = "sys.bind_loopback" orelse
  name = "sys.bind_any" orelse
  name = "sys.listen" orelse
  name = "sys.accept" orelse
  name = "sys.connect_loopback" orelse
  name = "sys.local_port" orelse
  name = "sys.close" orelse
  name = "sys.set_nonblocking" orelse
  name = "sys.epoll_create" orelse
  name = "sys.epoll_add" orelse
  name = "sys.epoll_add_token" orelse
  name = "sys.epoll_mod_token" orelse
  name = "sys.epoll_del" orelse
  name = "sys.epoll_wait" orelse
  name = "sys.errno" orelse
  name = "sys.install_stop_signals" orelse
  name = "sys.stop_requested" orelse
  name = "sys.sleep_ms" orelse
  name = "sys.monotonic_ms" orelse
  name = "sys.realtime_ms" orelse
  name = "sys.scratch_enter" orelse
  name = "sys.scratch_enter_all" orelse
  name = "sys.scratch_leave" orelse
  name = "sys.thread_spawn" orelse
  name = "sys.thread_join" orelse
  name = "sys.recv_into_raw" orelse
  name = "sys.send_string_raw" orelse
  name = "sys.send_from_raw" orelse
  name = "sqlite_open" orelse
  name = "sqlite_close" orelse
  name = "sqlite_exec" orelse
  name = "sqlite_prepare" orelse
  name = "sqlite_finalize" orelse
  name = "sqlite_step" orelse
  name = "sqlite_column_count" orelse
  name = "sqlite_column_name" orelse
  name = "sqlite_column_type" orelse
  name = "sqlite_column_int" orelse
  name = "sqlite_column_text" orelse
  name = "sqlite_changes" orelse
  name = "sqlite_errmsg"

fun isBuiltinCall name =
  name = "print_int" orelse
  name = "extern_view" orelse
  name = "extern_view_ptr" orelse
  name = "c_ptr.of_int" orelse
  name = "c_ptr.is_null" orelse
  name = "print_int64" orelse
  name = "print_float" orelse
  name = "print_bool" orelse
  name = "int64_to_int" orelse
  name = "int_to_int64" orelse
  name = "int64_to_int_wrapping" orelse
  name = "int64_wrapping_add" orelse
  name = "int64_wrapping_sub" orelse
  name = "int64_wrapping_mul" orelse
  name = "sprintf" orelse
  name = "os.exit" orelse
  isStringBuiltin name orelse
  isBytesBuiltin name orelse
  isSysBuiltin name orelse
  isArrayBuiltin name orelse
  isMapBuiltin name orelse
  name = "new_token" orelse
  name = "consume_token" orelse
  name = "new_resource" orelse
  name = "drop_resource"
end
