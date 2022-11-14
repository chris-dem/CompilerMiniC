; ModuleID = 'mini-c'
source_filename = "mini-c"

@0 = common global i32 0
@1 = common global float 0.000000e+00
@2 = common global i32 0

declare i32 @print_int(i32)

define i32 @While(i32 %n1) {
entry:
  %result = alloca i32, align 4
  %n = alloca i32, align 4
  %ret_val = alloca i32, align 4
  store i32 %n1, ptr %n, align 4
  store i32 12, ptr @0, align 4
  store i32 0, ptr %result, align 4
  %0 = load i32, ptr @0, align 4
  %print_int = call i32 @print_int(i32 %0)
  br label %header2

header2:                                          ; preds = %body, %entry
  %1 = load i32, ptr %result, align 4
  %i_lttmp = icmp slt i32 %1, 10
  %2 = sext i1 %i_lttmp to i32
  %while_comp = icmp ne i32 %2, 0
  br i1 %while_comp, label %body, label %after

body:                                             ; preds = %header2
  %3 = load i32, ptr %result, align 4
  %i_addtmp = add i32 %3, 1
  store i32 %i_addtmp, ptr %result, align 4
  br label %header2

after:                                            ; preds = %header2
  %4 = load i32, ptr %result, align 4
  store i32 %4, ptr %ret_val, align 4
  %5 = load i32, ptr %ret_val, align 4
  ret i32 %5
}
