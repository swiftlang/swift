// RUN: %swift -triple x86_64-apple-darwin10 %s -emit-llvm | FileCheck %s

// CHECK: [[OPAQUE:%swift.opaque]] = type opaque

func test0<T,U>(t : T, u : U) {
  var x = t
  var y = u
}
// CHECK:    define void @_T8generics5test0UFT1tV1uV_T_([[OPAQUE]]* %t, [[OPAQUE]]* %u, i8** %T, i8** %U) {
// CHECK:      [[XBUF:%.*]] = alloca [[BUFFER:.*]], align 8
// CHECK-NEXT: [[YBUF:%.*]] = alloca [[BUFFER]], align 8
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds i8** %T, i32 11
// CHECK-NEXT: [[T1:%.*]] = load i8** [[T0]], align 8
// CHECK-NEXT: [[T2:%.*]] = bitcast i8* [[T1]] to [[OPAQUE]]* ([[BUFFER]]*, i8**)*
// CHECK-NEXT: [[X:%.*]] = call [[OPAQUE]]* [[T2]]([[BUFFER]]* [[XBUF]], i8** %T) nounwind
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds i8** %T, i32 6
// CHECK-NEXT: [[T1:%.*]] = load i8** [[T0]], align 8
// CHECK-NEXT: [[T2:%.*]] = bitcast i8* [[T1]] to [[OPAQUE]]* ([[OPAQUE]]*, [[OPAQUE]]*, i8**)*
// CHECK-NEXT: call [[OPAQUE]]* [[T2]]([[OPAQUE]]* [[X]], [[OPAQUE]]* %t, i8** %T) nounwind
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds i8** %U, i32 11
// CHECK-NEXT: [[T1:%.*]] = load i8** [[T0]], align 8
// CHECK-NEXT: [[T2:%.*]] = bitcast i8* [[T1]] to [[OPAQUE]]* ([[BUFFER]]*, i8**)*
// CHECK-NEXT: [[Y:%.*]] = call [[OPAQUE]]* [[T2]]([[BUFFER]]* [[YBUF]], i8** %U) nounwind
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds i8** %U, i32 6
// CHECK-NEXT: [[T1:%.*]] = load i8** [[T0]], align 8
// CHECK-NEXT: [[T2:%.*]] = bitcast i8* [[T1]] to [[OPAQUE]]* ([[OPAQUE]]*, [[OPAQUE]]*, i8**)*
// CHECK-NEXT: call [[OPAQUE]]* [[T2]]([[OPAQUE]]* [[Y]], [[OPAQUE]]* %u, i8** %U) nounwind
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds i8** %U, i32 4
// CHECK-NEXT: [[T1:%.*]] = load i8** [[T0]], align 8
// CHECK-NEXT: [[T2:%.*]] = bitcast i8* [[T1]] to void ([[OPAQUE]]*, i8**)*
// CHECK-NEXT: call void [[T2]]([[OPAQUE]]* [[Y]], i8** %U) nounwind
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds i8** %T, i32 4
// CHECK-NEXT: [[T1:%.*]] = load i8** [[T0]], align 8
// CHECK-NEXT: [[T2:%.*]] = bitcast i8* [[T1]] to void ([[OPAQUE]]*, i8**)*
// CHECK-NEXT: call void [[T2]]([[OPAQUE]]* [[X]], i8** %T) nounwind
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds i8** %U, i32 4
// CHECK-NEXT: [[T1:%.*]] = load i8** [[T0]], align 8
// CHECK-NEXT: [[T2:%.*]] = bitcast i8* [[T1]] to void ([[OPAQUE]]*, i8**)*
// CHECK-NEXT: call void [[T2]]([[OPAQUE]]* %u, i8** %U) nounwind
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds i8** %T, i32 4
// CHECK-NEXT: [[T1:%.*]] = load i8** [[T0]], align 8
// CHECK-NEXT: [[T2:%.*]] = bitcast i8* [[T1]] to void ([[OPAQUE]]*, i8**)*
// CHECK-NEXT: call void [[T2]]([[OPAQUE]]* %t, i8** %T) nounwind
// CHECK-NEXT: ret void

func test1<T>(t : T) -> T {
  return t
}
// CHECK:    define void @_T8generics5test1UFT1tV_V([[OPAQUE]]* noalias sret, [[OPAQUE]]* %t, i8** %T) {
// CHECK:      [[T0:%.*]] = getelementptr inbounds i8** %T, i32 6
// CHECK-NEXT: [[T1:%.*]] = load i8** [[T0]], align 8
// CHECK-NEXT: [[T2:%.*]] = bitcast i8* [[T1]] to [[OPAQUE]]* ([[OPAQUE]]*, [[OPAQUE]]*, i8**)*
// CHECK-NEXT: call [[OPAQUE]]* [[T2]]([[OPAQUE]]* %0, [[OPAQUE]]* %t, i8** %T) nounwind
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds i8** %T, i32 4
// CHECK-NEXT: [[T1:%.*]] = load i8** [[T0]], align 8
// CHECK-NEXT: [[T2:%.*]] = bitcast i8* [[T1]] to void ([[OPAQUE]]*, i8**)*
// CHECK-NEXT: call void [[T2]]([[OPAQUE]]* %t, i8** %T) nounwind
// CHECK-NEXT: ret void
