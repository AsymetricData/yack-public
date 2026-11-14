define void @yack_main() {
entry:
%str0 = insertvalue { ptr, i64, i1 } undef, ptr @.str0, 0
%str1 = insertvalue { ptr, i64, i1 } %str0, i64 6, 1
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
%call11 = call i32 @yack_branch_move(i1 true, { ptr, i64, i1 } %str.copy.v29)
call i32 (ptr, ...) @printf(ptr @.fmt_int, i32 %call11)
ret void
}

define i32 @yack_branch_move(i1 %arg0, { ptr, i64, i1 } %arg1) {
entry:
%slot20 = alloca { ptr, i64, i1 }
%slot16 = alloca i32
%slot12 = alloca { ptr, i64, i1 }
store { ptr, i64, i1 } %arg1, ptr %slot12
br i1 %arg0, label %if.then13, label %if.else14
if.then13:
%place.load17 = load { ptr, i64, i1 }, ptr %slot12
%str.len18 = extractvalue { ptr, i64, i1 } %place.load17, 1
%str.len3219 = trunc i64 %str.len18 to i32
call void @yack_drop_string({ ptr, i64, i1 } %arg1)
store i32 %str.len3219, ptr %slot16
br label %if.join15
if.else14:
store { ptr, i64, i1 } %arg1, ptr %slot20
%place.load21 = load { ptr, i64, i1 }, ptr %slot20
%str.len22 = extractvalue { ptr, i64, i1 } %place.load21, 1
%str.len3223 = trunc i64 %str.len22 to i32
call void @yack_drop_string({ ptr, i64, i1 } %arg1)
store i32 %str.len3223, ptr %slot16
br label %if.join15
if.join15:
%if24 = load i32, ptr %slot16
ret i32 %if24
}
