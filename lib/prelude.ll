; LLVM prelude that is linked against when building the module
@balloon_int = constant i32 0
@balloon_float = constant i32 1
@balloon_boolean = constant i32 2

; Function Attrs: argmemonly nounwind
declare void @llvm.memset.p0i8.i32(i8* nocapture writeonly, i8, i32, i32, i1) #0

declare i8* @malloc(i32)

declare void @free(i8*)

declare i32 @write(i32, i8*, i32)

declare i32 @putchar(i32)

declare i32 @getchar()

define { i32, i64 }* @balloon_box_i64(i64 %input) {
entry:
  %mallocmem = call i8* @malloc(i32 ptrtoint ({ i32, i64 }* getelementptr ({ i32, i64 }, { i32, i64 }* null, i32 1) to i32))
  %box = bitcast i8* %mallocmem to { i32, i64 }*
  %slotp = getelementptr inbounds { i32, i64 }, { i32, i64 }* %box, i32 0, i32 1
  store i64 %input, i64* %slotp
  %tagval = load i32, i32* @balloon_int
  %box_tag = getelementptr inbounds { i32, i64 }, { i32, i64 }* %box, i32 0, i32 0
  store i32 %tagval, i32* %box_tag
  ret { i32, i64 }* %box
}

define i64 @balloon_unbox_i64({ i32, i64 }* %boxp) {
entry:
  %slotp = getelementptr inbounds { i32, i64 }, { i32, i64 }* %boxp, i32 0, i32 1
  %rawval = load i64, i64* %slotp
  ret i64 %rawval
}

define { i32, i64 }* @balloon_box_f64(double %input) {
entry:
  %f64_to_i64 = bitcast double %input to i64
  %mallocmem = call i8* @malloc(i32 ptrtoint ({ i32, i64 }* getelementptr ({ i32, i64 }, { i32, i64 }* null, i32 1) to i32))
  %box = bitcast i8* %mallocmem to { i32, i64 }*
  %slotp = getelementptr inbounds { i32, i64 }, { i32, i64 }* %box, i32 0, i32 1
  store i64 %f64_to_i64, i64* %slotp
  %tagval = load i32, i32* @balloon_float
  %box_tag = getelementptr inbounds { i32, i64 }, { i32, i64 }* %box, i32 0, i32 0
  store i32 %tagval, i32* %box_tag
  ret { i32, i64 }* %box
}

define double @balloon_unbox_f64({ i32, i64 }* %boxp) {
entry:
  %slotp = getelementptr inbounds { i32, i64 }, { i32, i64 }* %boxp, i32 0, i32 1
  %rawval = load i64, i64* %slotp
  %i64_to_f64 = bitcast i64 %rawval to double
  ret double %i64_to_f64
}

define i32 @balloon_unbox_tag({ i32, i64 }* %boxp) {
entry:
  %tagp = getelementptr inbounds { i32, i64 }, { i32, i64 }* %boxp, i32 0, i32 0
  %tag = load i32, i32* %tagp
  ret i32 %tag
}

define { i32, i64 }* @balloon_add_box_f64_box_f64({ i32, i64 }* %box1, { i32, i64 }* %box2) {
entry:
  %raw1 = call double @balloon_unbox_f64({ i32, i64 }* %box1)
  %raw2 = call double @balloon_unbox_f64({ i32, i64 }* %box2)
  %sum = fadd double %raw1, %raw2
  %boxed_sum = call { i32, i64 }* @balloon_box_f64(double %sum)
  ret { i32, i64 }* %boxed_sum
}

define { i32, i64 }* @balloon_add_box_i64_box_f64({ i32, i64 }* %box1, { i32, i64 }* %box2) {
entry:
  %raw1 = call i64 @balloon_unbox_i64({ i32, i64 }* %box1)
  %raw2 = call double @balloon_unbox_f64({ i32, i64 }* %box2)
  %raw1_float = sitofp i64 %raw1 to double
  %sum = fadd double %raw1_float, %raw2
  %boxed_sum = call { i32, i64 }* @balloon_box_f64(double %sum)
  ret { i32, i64 }* %boxed_sum
}

define { i32, i64 }* @balloon_add_box_i64_box_i64({ i32, i64 }* %box1, { i32, i64 }* %box2) {
entry:
  %raw1 = call i64 @balloon_unbox_i64({ i32, i64 }* %box1)
  %raw2 = call i64 @balloon_unbox_i64({ i32, i64 }* %box2)
  %sum = add i64 %raw1, %raw2
  %boxed_sum = call { i32, i64 }* @balloon_box_i64(i64 %sum)
  ret { i32, i64 }* %boxed_sum
}

define { i32, i64 }* @balloon_add_box_box({ i32, i64 }* %box1, { i32, i64 }* %box2) {
entry:
  %tag1 = call i32 @balloon_unbox_tag({ i32, i64 }* %box1)
  %tag2 = call i32 @balloon_unbox_tag({ i32, i64 }* %box2)
  switch i32 %tag1, label %unknown_add_types [
    i32 0, label %int_wildcard
    i32 1, label %float_wildcard
  ]

unknown_add_types:                                ; preds = %entry, %float_wildcard, %int_wildcard
  %errorbox = call { i32, i64 }* @balloon_box_i64(i64 -42)
  ret { i32, i64 }* %errorbox

float_float:                                      ; preds = %float_wildcard
  %sum = call { i32, i64 }* @balloon_add_box_f64_box_f64({ i32, i64 }* %box1, { i32, i64 }* %box2)
  ret { i32, i64 }* %sum

int_int:                                          ; preds = %int_wildcard
  %sum1 = call { i32, i64 }* @balloon_add_box_i64_box_i64({ i32, i64 }* %box1, { i32, i64 }* %box2)
  ret { i32, i64 }* %sum1

float_int:                                        ; preds = %float_wildcard
  %sum2 = call { i32, i64 }* @balloon_add_box_i64_box_f64({ i32, i64 }* %box2, { i32, i64 }* %box1)
  ret { i32, i64 }* %sum2

int_float:                                        ; preds = %int_wildcard
  %sum3 = call { i32, i64 }* @balloon_add_box_i64_box_f64({ i32, i64 }* %box1, { i32, i64 }* %box2)
  ret { i32, i64 }* %sum3

int_wildcard:                                     ; preds = %entry
  switch i32 %tag2, label %unknown_add_types [
    i32 0, label %int_int
    i32 1, label %int_float
  ]

float_wildcard:                                   ; preds = %entry
  switch i32 %tag2, label %unknown_add_types [
    i32 0, label %float_int
    i32 1, label %float_float
  ]
}
