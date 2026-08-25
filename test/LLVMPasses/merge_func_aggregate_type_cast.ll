; RUN: %swift-llvm-opt -passes='swift-merge-functions' -swiftmergefunc-threshold=4 %s | %FileCheck %s

; REQUIRES: PTRSIZE=64

; The function comparator considers a pointer and a pointer-sized integer to be
; equivalent, and it compares aggregates and vectors element-wise. Therefore two
; merged functions can differ in their argument and return types, e.g.
; `[2 x i64]` vs. `[2 x ptr]`. As there is no cast between aggregates, the thunks
; have to convert such types element-wise.

@g1 = external global i32
@g2 = external global i32

; CHECK-LABEL: define void @array_param_int(
; CHECK:         tail call void @array_param_intTm(ptr %p, [2 x i64] %a, ptr @g1)
define void @array_param_int(ptr %p, [2 x i64] %a) {
  %l1 = load i32, ptr @g1, align 4
  %l2 = load i32, ptr @g1, align 4
  %s = add i32 %l1, %l2
  store i32 %s, ptr %p, align 4
  ret void
}

; CHECK-LABEL: define void @array_param_ptr(
; CHECK:         %[[E0:.*]] = extractvalue [2 x ptr] %a, 0
; CHECK:         %[[I0:.*]] = ptrtoint ptr %[[E0]] to i64
; CHECK:         %[[A0:.*]] = insertvalue [2 x i64] undef, i64 %[[I0]], 0
; CHECK:         %[[E1:.*]] = extractvalue [2 x ptr] %a, 1
; CHECK:         %[[I1:.*]] = ptrtoint ptr %[[E1]] to i64
; CHECK:         %[[A1:.*]] = insertvalue [2 x i64] %[[A0]], i64 %[[I1]], 1
; CHECK:         tail call void @array_param_intTm(ptr %p, [2 x i64] %[[A1]], ptr @g2)
define void @array_param_ptr(ptr %p, [2 x ptr] %a) {
  %l1 = load i32, ptr @g2, align 4
  %l2 = load i32, ptr @g2, align 4
  %s = add i32 %l1, %l2
  store i32 %s, ptr %p, align 4
  ret void
}

; Vectors of pointers and vectors of integers don't need to be converted
; element-wise: a single ptrtoint/inttoptr does it.

; CHECK-LABEL: define void @vector_param_int(
; CHECK:         tail call void @vector_param_intTm(ptr %p, <2 x i64> %a, i32 %x, ptr @g1)
define void @vector_param_int(ptr %p, <2 x i64> %a, i32 %x) {
  %l1 = load i32, ptr @g1, align 4
  %l2 = load i32, ptr @g1, align 4
  %s1 = add i32 %l1, %l2
  %s2 = add i32 %s1, %x
  store i32 %s2, ptr %p, align 4
  ret void
}

; CHECK-LABEL: define void @vector_param_ptr(
; CHECK:         %[[V:.*]] = ptrtoint <2 x ptr> %a to <2 x i64>
; CHECK:         tail call void @vector_param_intTm(ptr %p, <2 x i64> %[[V]], i32 %x, ptr @g2)
define void @vector_param_ptr(ptr %p, <2 x ptr> %a, i32 %x) {
  %l1 = load i32, ptr @g2, align 4
  %l2 = load i32, ptr @g2, align 4
  %s1 = add i32 %l1, %l2
  %s2 = add i32 %s1, %x
  store i32 %s2, ptr %p, align 4
  ret void
}

; CHECK-LABEL: define [2 x ptr] @array_return_ptr(
; CHECK:         %[[C:.*]] = tail call [2 x ptr] @array_return_ptrTm(ptr %p, [2 x ptr] %a, ptr @g2)
; CHECK:         ret [2 x ptr] %[[C]]
define [2 x ptr] @array_return_ptr(ptr %p, [2 x ptr] %a) {
  %l1 = load i32, ptr @g2, align 4
  %l2 = load i32, ptr @g2, align 4
  %s1 = add i32 %l1, %l2
  %s2 = add i32 %s1, %s1
  %s3 = add i32 %s2, %s1
  store i32 %s3, ptr %p, align 4
  ret [2 x ptr] %a
}

; CHECK-LABEL: define [2 x i64] @array_return_int(
; CHECK:         %[[AE0:.*]] = extractvalue [2 x i64] %a, 0
; CHECK:         %[[AP0:.*]] = inttoptr i64 %[[AE0]] to ptr
; CHECK:         %[[AA0:.*]] = insertvalue [2 x ptr] undef, ptr %[[AP0]], 0
; CHECK:         %[[AE1:.*]] = extractvalue [2 x i64] %a, 1
; CHECK:         %[[AP1:.*]] = inttoptr i64 %[[AE1]] to ptr
; CHECK:         %[[AA1:.*]] = insertvalue [2 x ptr] %[[AA0]], ptr %[[AP1]], 1
; CHECK:         %[[RC:.*]] = tail call [2 x ptr] @array_return_ptrTm(ptr %p, [2 x ptr] %[[AA1]], ptr @g1)
; CHECK:         %[[RE0:.*]] = extractvalue [2 x ptr] %[[RC]], 0
; CHECK:         %[[RI0:.*]] = ptrtoint ptr %[[RE0]] to i64
; CHECK:         %[[RA0:.*]] = insertvalue [2 x i64] undef, i64 %[[RI0]], 0
; CHECK:         %[[RE1:.*]] = extractvalue [2 x ptr] %[[RC]], 1
; CHECK:         %[[RI1:.*]] = ptrtoint ptr %[[RE1]] to i64
; CHECK:         %[[RA1:.*]] = insertvalue [2 x i64] %[[RA0]], i64 %[[RI1]], 1
; CHECK:         ret [2 x i64] %[[RA1]]
define [2 x i64] @array_return_int(ptr %p, [2 x i64] %a) {
  %l1 = load i32, ptr @g1, align 4
  %l2 = load i32, ptr @g1, align 4
  %s1 = add i32 %l1, %l2
  %s2 = add i32 %s1, %s1
  %s3 = add i32 %s2, %s1
  store i32 %s3, ptr %p, align 4
  ret [2 x i64] %a
}
