; ModuleID = 'mini-c'
source_filename = "mini-c"

declare i32 @print_int(i32)

define i32 @fibonacci(i32 %n1) {
entry:
  %n = alloca i32, align 4
  %ret_val = alloca i32, align 4
  store i32 %n1, ptr %n, align 4
  %0 = load i32, ptr %n, align 4
  %i_eqtmp = icmp eq i32 %0, 0
  %1 = zext i1 %i_eqtmp to i32
  %2 = icmp ne i32 %1, 0
  br i1 %2, label %then, label %end

then:                                             ; preds = %entry
  store i32 0, ptr %ret_val, align 4
  br i1 true, label %ret, label %deadcode

deadcode:                                         ; preds = %then
  br label %end

end:                                              ; preds = %deadcode, %entry
  %3 = load i32, ptr %n, align 4
  %i_eqtmp3 = icmp eq i32 %3, 1
  %4 = zext i1 %i_eqtmp3 to i32
  %5 = icmp ne i32 %4, 0
  br i1 %5, label %then2, label %end5

then2:                                            ; preds = %end
  store i32 1, ptr %ret_val, align 4
  br i1 true, label %ret, label %deadcode4

deadcode4:                                        ; preds = %then2
  br label %end5

end5:                                             ; preds = %deadcode4, %end
  %6 = load i32, ptr %n, align 4
  %i_subtmp = sub i32 %6, 1
  %7 = call i32 @fibonacci(i32 %i_subtmp)
  %8 = load i32, ptr %n, align 4
  %i_subtmp6 = sub i32 %8, 2
  %9 = call i32 @fibonacci(i32 %i_subtmp6)
  %i_addtmp = add i32 %7, %9
  store i32 %i_addtmp, ptr %ret_val, align 4
  br i1 true, label %ret, label %deadcode7

deadcode7:                                        ; preds = %end5
  br label %ret

ret:                                              ; preds = %deadcode7, %end5, %then2, %then
  %10 = load i32, ptr %ret_val, align 4
  ret i32 %10
}
