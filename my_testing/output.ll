; ModuleID = 'mini-c'
source_filename = "mini-c"

declare i32 @print_int(i32)

define i32 @fibonacci(i32 %n1) {
entry:
  %n = alloca i32, align 4
  %ret_val = alloca i32, align 4
  store i32 %n1, ptr %n, align 4
  %0 = call i32 @print_int(i32 10000)
  %1 = call i32 @print_int(i32 10000)
  store i32 88, ptr %ret_val, align 4
  br i1 true, label %ret, label %deadcode

deadcode:                                         ; preds = %entry
  br label %ret

ret:                                              ; preds = %deadcode, %entry
  %2 = load i32, ptr %ret_val, align 4
  ret i32 %2
}
