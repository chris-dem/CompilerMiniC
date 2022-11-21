; ModuleID = 'mini-c'
source_filename = "mini-c"

declare i32 @print_int(i32)

define i32 @addition(i32 %n1, i32 %m2) {
entry:
  %result = alloca i32, align 4
  %m = alloca i32, align 4
  %n = alloca i32, align 4
  %ret_val = alloca i32, align 4
  store i32 %n1, ptr %n, align 4
  store i32 %m2, ptr %m, align 4
  %0 = load i32, ptr %n, align 4
  %1 = load i32, ptr %m, align 4
  %i_addtmp = add i32 %0, %1
  store i32 %i_addtmp, ptr %result, align 4
  %2 = load i32, ptr %n, align 4
  %i_eqtmp = icmp eq i32 %2, 4
  %3 = zext i1 %i_eqtmp to i32
  %4 = icmp ne i32 %3, 0
  br i1 %4, label %then, label %else

then:                                             ; preds = %entry
  %5 = load i32, ptr %n, align 4
  %6 = load i32, ptr %m, align 4
  %i_addtmp3 = add i32 %5, %6
  %7 = call i32 @print_int(i32 %i_addtmp3)
  br label %end

else:                                             ; preds = %entry
  %8 = load i32, ptr %n, align 4
  %9 = load i32, ptr %m, align 4
  %i_multmp = mul i32 %8, %9
  %10 = call i32 @print_int(i32 %i_multmp)
  br label %end

end:                                              ; preds = %else, %then
  %11 = load i32, ptr %result, align 4
  store i32 %11, ptr %ret_val, align 4
  br i1 true, label %ret, label %deadcode

deadcode:                                         ; preds = %end
  br label %ret

ret:                                              ; preds = %deadcode, %end
  %12 = load i32, ptr %ret_val, align 4
  ret i32 %12
}
