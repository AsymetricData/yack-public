structure OwnershipSummaries =
struct
  fun isContainerGetBorrow name =
    name = "array_get_borrow" orelse
    name = "array_get_borrow_view" orelse
    name = "array_kernel_borrow" orelse
    name = "map_get_borrow" orelse
    name = "map_get_borrow_view"

  fun isMapGetBorrow name =
    name = "map_get_borrow" orelse name = "map_get_borrow_view"

  fun resultBorrowSource name =
    if isContainerGetBorrow name then SOME 1 else NONE

  fun releasesArgumentLoans name =
    let
      val operations =
        [ "local_port"
        , "listener_fd"
        , "stream_fd"
        , "set_nonblocking"
        , "epoll_add_read"
        , "accept"
        , "send_string"
        , "sys.send_string_raw"
        , "string_length"
        , "string_eq"
        , "string_char_at"
        , "string_char_at_unchecked"
        , "string_find_char"
        , "string_find_char_from"
        , "print_span"
        , "string_of_char"
        , "string_of_int"
        , "parse_url"
        , "query_value"
        , "send_hello"
        , "header_end"
        , "find_header_end_from"
        , "query_value_in_range"
        , "bytes_write_string"
        ]
      fun matches operation =
        name = operation orelse
        String.isSuffix ("." ^ operation) name
    in
      List.exists matches operations
    end
end
