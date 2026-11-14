define void @yack_main() {
entry:
%slot25 = alloca { ptr, i64, i1 }
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
%str11 = insertvalue { ptr, i64, i1 } undef, ptr @.str1, 0
%str12 = insertvalue { ptr, i64, i1 } %str11, i64 3, 1
%str13 = insertvalue { ptr, i64, i1 } %str12, i1 false, 2
%str.copy.data14 = extractvalue { ptr, i64, i1 } %str13, 0
%str.copy.len15 = extractvalue { ptr, i64, i1 } %str13, 1
%str.copy.size16 = add i64 %str.copy.len15, 1
%string.mem21 = call ptr @yack_string_alloc(i64 %str.copy.size16)
call ptr @memcpy(ptr %string.mem21, ptr %str.copy.data14, i64 %str.copy.len15)
%str.copy.end17 = getelementptr i8, ptr %string.mem21, i64 %str.copy.len15
store i8 0, ptr %str.copy.end17
%str.copy.v018 = insertvalue { ptr, i64, i1 } undef, ptr %string.mem21, 0
%str.copy.v119 = insertvalue { ptr, i64, i1 } %str.copy.v018, i64 %str.copy.len15, 1
%str.copy.v220 = insertvalue { ptr, i64, i1 } %str.copy.v119, i1 true, 2
%agg22 = insertvalue { { ptr, i64, i1 }, { ptr, i64, i1 } } undef, { ptr, i64, i1 } %str.copy.v29, 0
%agg23 = insertvalue { { ptr, i64, i1 }, { ptr, i64, i1 } } %agg22, { ptr, i64, i1 } %str.copy.v220, 1
%field24 = extractvalue { { ptr, i64, i1 }, { ptr, i64, i1 } } %agg23, 0
store { ptr, i64, i1 } %field24, ptr %slot25
%place.load26 = load { ptr, i64, i1 }, ptr %slot25
%str.len27 = extractvalue { ptr, i64, i1 } %place.load26, 1
%str.len3228 = trunc i64 %str.len27 to i32
call i32 (ptr, ...) @printf(ptr @.fmt_int, i32 %str.len3228)
call void @yack_drop_string({ ptr, i64, i1 } %field24)
%dropfield29 = extractvalue { { ptr, i64, i1 }, { ptr, i64, i1 } } %agg23, 1
call void @yack_drop_string({ ptr, i64, i1 } %dropfield29)
ret void
}
