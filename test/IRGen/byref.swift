// RUN: %swift -triple x86_64-apple-darwin10 -I %S/.. %s -emit-llvm | FileCheck %s

import swift

// CHECK: [[REFCOUNT:%swift.refcounted]] = type { i8* }
// CHECK: [[INT:%.*]] = type { { i64 } }

func f0() {
  var f : ([byref(heap)] int) -> int
  var i : int = 10
  f(&i)
}
// CHECK:    define void @_T5byref2f0FT_T_() {
// CHECK:      [[F:%.*]] = alloca { i8*, [[REFCOUNT]]* }, align 8
// CHECK:      getelementptr inbounds { i8*, [[REFCOUNT]]* }* [[F]], i32 0, i32 0
// CHECK:      getelementptr inbounds { i8*, [[REFCOUNT]]* }* [[F]], i32 0, i32 1
// CHECK:      [[I_ALLOC:%.*]] = call [[REFCOUNT]]* @swift_alloc(i64 8) nounwind
// CHECK-NEXT: [[T0:%.*]] = bitcast [[REFCOUNT]]* [[I_ALLOC]] to { [[INT]] }*
// CHECK-NEXT: [[I:%.*]] = getelementptr inbounds { [[INT]] }* [[T0]], i32 0, i32 0
// CHECK-NEXT: [[I_VAL:%.*]] = call i64 @_TNSs5int6425convertFromIntegerLiteralFT3vali64_S_(i64 10)
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds [[INT]]* [[I]], i32 0, i32 0
// CHECK-NEXT: [[T2:%.*]] = getelementptr inbounds { i64 }* [[T1]], i32 0, i32 0
// CHECK-NEXT: store i64 [[I_VAL]], i64* [[T2]]
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds { i8*, [[REFCOUNT]]* }* [[F]], i32 0, i32 0
// CHECK-NEXT: [[FN_RAW:%.*]] = load i8** [[T0]], align 8
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds { i8*, [[REFCOUNT]]* }* [[F]], i32 0, i32 1
// CHECK-NEXT: [[DATA_RAW:%.*]] = load [[REFCOUNT]]** [[T0]], align 8
// CHECK-NEXT: [[DATA:%.*]] = call [[REFCOUNT]]* @swift_retain([[REFCOUNT]]* [[DATA_RAW]]) nounwind
// CHECK-NEXT: [[FN:%.*]] = bitcast i8* [[FN_RAW]] to i64 ([[INT]]*, [[REFCOUNT]]*, [[REFCOUNT]]*)*
// CHECK-NEXT: [[I_RETAINED:%.*]] = call [[REFCOUNT]]* @swift_retain([[REFCOUNT]]* [[I_ALLOC]]) nounwind
// CHECK-NEXT: call i64 [[FN]]([[INT]]* [[I]], [[REFCOUNT]]* [[I_RETAINED]], [[REFCOUNT]]* [[DATA]])
// FIXME: release
// CHECK-NEXT: ret void

func f1(i : int) {
  var f : ([byref(heap)] int) -> int
  f(&i)
}
// CHECK:    define void @_T5byref2f1FT1iNSs5int64_T_(i64 [[I_VAL:%.*]]) {
// CHECK:      [[F:%.*]] = alloca { i8*, [[REFCOUNT]]* }, align 8
// CHECK:      [[I_ALLOC:%.*]] = call [[REFCOUNT]]* @swift_alloc(i64 8) nounwind
// CHECK-NEXT: [[T0:%.*]] = bitcast [[REFCOUNT]]* [[I_ALLOC]] to { [[INT]] }*
// CHECK-NEXT: [[I:%.*]] = getelementptr inbounds { [[INT]] }* [[T0]], i32 0, i32 0
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds [[INT]]* [[I]], i32 0, i32 0
// CHECK-NEXT: [[T2:%.*]] = getelementptr inbounds { i64 }* [[T1]], i32 0, i32 0
// CHECK-NEXT: store i64 [[I_VAL]], i64* [[T2]]
// CHECK:      getelementptr inbounds { i8*, [[REFCOUNT]]* }* [[F]], i32 0, i32 0
// CHECK:      getelementptr inbounds { i8*, [[REFCOUNT]]* }* [[F]], i32 0, i32 1
// CHECK:      [[T0:%.*]] = getelementptr inbounds { i8*, [[REFCOUNT]]* }* [[F]], i32 0, i32 0
// CHECK-NEXT: [[FN_RAW:%.*]] = load i8** [[T0]], align 8
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds { i8*, [[REFCOUNT]]* }* [[F]], i32 0, i32 1
// CHECK-NEXT: [[DATA_RAW:%.*]] = load [[REFCOUNT]]** [[T0]], align 8
// CHECK-NEXT: [[DATA:%.*]] = call [[REFCOUNT]]* @swift_retain([[REFCOUNT]]* [[DATA_RAW]]) nounwind
// CHECK-NEXT: [[FN:%.*]] = bitcast i8* [[FN_RAW]] to i64 ([[INT]]*, [[REFCOUNT]]*, [[REFCOUNT]]*)*
// CHECK-NEXT: [[I_RETAINED:%.*]] = call [[REFCOUNT]]* @swift_retain([[REFCOUNT]]* [[I_ALLOC]]) nounwind
// CHECK-NEXT: call i64 [[FN]]([[INT]]* [[I]], [[REFCOUNT]]* [[I_RETAINED]], [[REFCOUNT]]* [[DATA]])
// FIXME: release
// CHECK-NEXT: ret void
