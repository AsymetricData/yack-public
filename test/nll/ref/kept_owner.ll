define void @yack_main() {
entry:
%slot11 = alloca { ptr, i64, i1 }
%str0 = insertvalue { ptr, i64, i1 } undef, ptr @.str0, 0
%str1 = insertvalue { ptr, i64, i1 } %str0, i64 3, 1
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
store { ptr, i64, i1 } %str.copy.v29, ptr %slot11
%place.load12 = load { ptr, i64, i1 }, ptr %slot11
%str.len13 = extractvalue { ptr, i64, i1 } %place.load12, 1
%str.len3214 = trunc i64 %str.len13 to i32
call i32 (ptr, ...) @printf(ptr @.fmt_int, i32 %str.len3214)
call void @yack_drop_string({ ptr, i64, i1 } %str.copy.v29)
ret void
}
