; ModuleID = 'mini-c'
source_filename = "mini-c"

declare i32 @print_int(i32)

define i32 @always() {
entry:
  %ret_val = alloca i32, align 4
  store i32 3, ptr %ret_val, align 4
  br i1 true, label %ret, label %deadcode

deadcode:                                         ; preds = %entry
  br label %ret

ret:                                              ; preds = %deadcode, %entry
  %0 = load i32, ptr %ret_val, align 4
  ret i32 %0
}

define void @prime() {
entry:
  %nom = alloca i32, align 4
  br i1 true, label %true_, label %false_

true_:                                            ; preds = %entry
  br label %merge_

false_:                                           ; preds = %entry
  br label %merge_

merge_:                                           ; preds = %false_, %true_
  %lazy_and = phi i32 [ 1, %true_ ], [ 0, %false_ ]
  %i_addtmp = add i32 -2, %lazy_and
  %0 = call i32 @print_int(i32 %i_addtmp)
  br label %ret

ret:                                              ; preds = %merge_
  ret void
}

define void @pi() {
entry:
  call void @prime()
  br label %ret

ret:                                              ; preds = %entry
  ret void
}
