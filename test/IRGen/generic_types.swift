// RUN: %swift -triple x86_64-apple-darwin10 %s -emit-llvm | FileCheck %s

// CHECK: [[OPAQUE:%swift.opaque]] = type opaque
// CHECK: [[A:%_T13generic_types1A]] = type { [[REF:%swift.refcounted]], [[INT:%_TSs5Int64]] }
// CHECK: [[INT]] = type { i64 }

class A<T> {
  var x : Int

  func run(t : T) {}
// CHECK:    define void @_TC13generic_types1A3runU__fGS0_Q__FT1tQ__T_([[OPAQUE]]* %t, [[A]]* %this, i8** %T) {
// CHECK:      [[THIS:%.*]] = alloca [[A]]*, align 8
// CHECK-NEXT: store [[A]]* %this, [[A]]** [[THIS]], align 8
// CHECK-NEXT: load [[A]]** [[THIS]], align 8
// CHECK-NEXT: @swift_release
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds i8** %T, i32 4
// CHECK-NEXT: [[T1:%.*]] = load i8** [[T0]], align 8
// CHECK-NEXT: [[T2:%.*]] = bitcast i8* [[T1]] to void ([[OPAQUE]]*, i8**)*
// CHECK-NEXT: call void [[T2]]([[OPAQUE]]* %t, i8** %T) nounwind
// CHECK-NEXT: ret void
}

func foo(y : A<Int>) {
  y.run(15)
}
// CHECK:    define void @_T13generic_types3fooFT1yGCS_1ASi__T_([[A]]* %y) {
// CHECK:      [[Y:%.*]] = alloca [[A]]*, align 8
// CHECK-NEXT: [[TEMP:%.*]] = alloca [[INT]], align 8
// CHECK-NEXT: store [[A]]* %y, [[A]]** [[Y]], align 8
// CHECK-NEXT: [[T0:%.*]] = load [[A]]** [[Y]], align 8
// CHECK-NEXT: [[T1:%.*]] = bitcast [[A]]* [[T0]] to [[REF]]*
// CHECK-NEXT: call void @swift_retain_noresult([[REF]]* [[T1]]) nounwind
// CHECK-NEXT: [[T1:%.*]] = call i64 @_TSi25convertFromIntegerLiteralFT3valBi64__Si(i64 15)
// CHECK-NEXT: [[T2:%.*]] = getelementptr inbounds [[INT]]* [[TEMP]], i32 0, i32 0
// CHECK-NEXT: store i64 [[T1]], i64* [[T2]], align 8
// CHECK-NEXT: [[T1:%.*]] = bitcast [[INT]]* [[TEMP]] to [[OPAQUE]]*
// CHECK-NEXT: call void @_TC13generic_types1A3runU__fGS0_Q__FT1tQ__T_([[OPAQUE]]* [[T1]], [[A]]* [[T0]], i8** getelementptr inbounds ([13 x i8* 
// CHECK-NEXT: load [[A]]** [[Y]], align 8
// CHECK-NEXT: @swift_release
// CHECK-NEXT: ret void
