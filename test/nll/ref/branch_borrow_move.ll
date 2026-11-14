define void @yack_main() {
entry:
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
%call11 = call { ptr, i64, i1 } @yack_choose(i1 true, { ptr, i64, i1 } %str.copy.v29)
%strdata12 = extractvalue { ptr, i64, i1 } %call11, 0
%strlen13 = extractvalue { ptr, i64, i1 } %call11, 1
%strlen3215 = trunc i64 %strlen13 to i32
call i32 (ptr, ...) @printf(ptr @.fmt_string, i32 %strlen3215, ptr %strdata12)
call void @yack_drop_string({ ptr, i64, i1 } %call11)
ret void
}

define { ptr, i64, i1 } @yack_choose(i1 %arg0, { ptr, i64, i1 } %arg1) {
entry:
%slot25 = alloca { ptr, i64, i1 }
%slot18 = alloca { ptr, i64, i1 }
store { ptr, i64, i1 } %arg1, ptr %slot18
%place.load19 = load { ptr, i64, i1 }, ptr %slot18
%str.len20 = extractvalue { ptr, i64, i1 } %place.load19, 1
%str.len3221 = trunc i64 %str.len20 to i32
br i1 %arg0, label %if.then22, label %if.else23
if.then22:
store { ptr, i64, i1 } %arg1, ptr %slot25
br label %if.join24
if.else23:
store { ptr, i64, i1 } %arg1, ptr %slot25
br label %if.join24
if.join24:
%if26 = load { ptr, i64, i1 }, ptr %slot25
ret { ptr, i64, i1 } %if26
}
