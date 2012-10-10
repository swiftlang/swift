// RUN: %swift -triple x86_64-apple-darwin10 %s -emit-llvm | FileCheck %s

// CHECK: [[REF:%swift.refcounted]] = type {
// CHECK: [[HEAPMETADATA:%swift.heapmetadata]] = type {
// CHECK: [[TYPE:%swift.type]] = type {
// CHECK: [[A:%_T7virtual1A]] = type opaque
// CHECK: [[B:%_T7virtual1B]] = type opaque

class A {
  func f() {}
  func g() {}
}

class B : A {
  func g() {}
}

func test0(x : B) {
  x.f()
}
// CHECK:    define void @_T7virtual5test0FT1xCS_1B_T_([[B]]* %x) {
// CHECK:      [[X:%.*]] = alloca [[B]]*, align 8
// CHECK-NEXT: store [[B]]* %x, [[B]]** [[X]], align 8
// CHECK-NEXT: [[T0:%.*]] = load [[B]]** [[X]], align 8
// CHECK-NEXT: [[T1:%.*]] = bitcast [[B]]* [[T0]] to [[REF]]*
// CHECK-NEXT: call void @swift_retain_noresult([[REF]]* [[T1]]) nounwind
// CHECK-NEXT: [[THIS:%.*]] = bitcast [[B]]* [[T0]] to [[A]]*
// CHECK-NEXT: [[T0:%.*]] = bitcast [[A]]* [[THIS]] to [[HEAPMETADATA]]**
// CHECK-NEXT: [[META:%.*]] = load [[HEAPMETADATA]]** [[T0]], align 8
// CHECK-NEXT: [[T0:%.*]] = bitcast [[HEAPMETADATA]]* [[META]] to void ([[A]]*)**
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds void ([[A]]*)** [[T0]], i32 6
// CHECK-NEXT: [[FN:%.*]] = load void ([[A]]*)** [[T1]], align 8
// CHECK-NEXT: call void [[FN]]([[A]]* [[THIS]])
// CHECK-NEXT: [[T0:%.*]] = load [[B]]** [[X]], align 8
// CHECK-NEXT: call void bitcast (void ([[REF]]*)* @swift_release to void ([[B]]*)*)([[B]]* [[T0]]) nounwind
// CHECK-NEXT: ret void

func test1(x : B) {
  x.g()
}
// CHECK:    define void @_T7virtual5test1FT1xCS_1B_T_([[B]]* %x) {
// CHECK:      [[X:%.*]] = alloca [[B]]*, align 8
// CHECK-NEXT: store [[B]]* %x, [[B]]** [[X]], align 8
// CHECK-NEXT: [[THIS:%.*]] = load [[B]]** [[X]], align 8
// CHECK-NEXT: [[T1:%.*]] = bitcast [[B]]* [[THIS]] to [[REF]]*
// CHECK-NEXT: call void @swift_retain_noresult([[REF]]* [[T1]]) nounwind
// CHECK-NEXT: [[T0:%.*]] = bitcast [[B]]* [[THIS]] to [[HEAPMETADATA]]**
// CHECK-NEXT: [[META:%.*]] = load [[HEAPMETADATA]]** [[T0]], align 8
// CHECK-NEXT: [[T0:%.*]] = bitcast [[HEAPMETADATA]]* [[META]] to void ([[B]]*)**
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds void ([[B]]*)** [[T0]], i32 7
// CHECK-NEXT: [[FN:%.*]] = load void ([[B]]*)** [[T1]], align 8
// CHECK-NEXT: call void [[FN]]([[B]]* [[THIS]])
// CHECK-NEXT: [[T0:%.*]] = load [[B]]** [[X]], align 8
// CHECK-NEXT: call void bitcast (void ([[REF]]*)* @swift_release to void ([[B]]*)*)([[B]]* [[T0]]) nounwind
// CHECK-NEXT: ret void