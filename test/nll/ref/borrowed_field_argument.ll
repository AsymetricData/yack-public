define void @yack_main() {
entry:
%str0 = insertvalue { ptr, i64, i1 } undef, ptr @.str0, 0
%str1 = insertvalue { ptr, i64, i1 } %str0, i64 4, 1
%str2 = insertvalue { ptr, i64, i1 } %str1, i1 false, 2
%str.copy.data3 = extractvalue { ptr, i64, i1 } %str2, 0
%str.copy.len4 = extractvalue { ptr, i64, i1 } %str2, 1
%str.copy.size5 = add i64 %str.copy.len4, 1
%string.mem10 = call ptr @yack_string_alloc(i64 %str.copy.size5)
call ptr @memcpy(ptr %string.mem10, ptr %str.copy.data3, i64 %str.copy.len4)
%str.copy.end6 = getelementptr i8, ptr %string.mem10, i64 %str.copy.len4
store i8 0, ptr %str.copy.end6
%str.copy.v07 = insertvalue { ptr, i64, i1 } undef, ptr %string.mem10, 0
%str.copy.v18 = insertvalue { ptr, i64, i1 } %str.copy.v07, i64 %str.copy.len4, 1
%str.copy.v29 = insertvalue { ptr, i64, i1 } %str.copy.v18, i1 true, 2
%call11 = call i32 @yack_length_of({ ptr, i64, i1 } %str.copy.v29)
call i32 (ptr, ...) @printf(ptr @.fmt_int, i32 %call11)
ret void
}

define i32 @yack_length_of({ ptr, i64, i1 } %arg0) {
entry:
%slot12 = alloca { ptr, i64, i1 }
store { ptr, i64, i1 } %arg0, ptr %slot12
%place.load13 = load { ptr, i64, i1 }, ptr %slot12
%agg14 = insertvalue { { ptr, i64, i1 }, i32 } undef, { ptr, i64, i1 } %place.load13, 0
%agg15 = insertvalue { { ptr, i64, i1 }, i32 } %agg14, i32 7, 1
%field16 = extractvalue { { ptr, i64, i1 }, i32 } %agg15, 0
%str.len17 = extractvalue { ptr, i64, i1 } %field16, 1
%str.len3218 = trunc i64 %str.len17 to i32
%dropfield19 = extractvalue { { ptr, i64, i1 }, i32 } %agg15, 0
%dropfield20 = extractvalue { { ptr, i64, i1 }, i32 } %agg15, 1
call void @yack_drop_string({ ptr, i64, i1 } %arg0)
ret i32 %str.len3218
}
