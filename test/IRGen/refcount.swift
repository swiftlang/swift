// RUN: %swift -triple x86_64-apple-darwin10 -I %S/.. %s -emit-llvm | FileCheck %s

// CHECK: [[REFCOUNT:%.*]] = type { i8* }

// Test basic functionality for Builtin.ObjectPointer
func f0() {
  var x : Builtin.ObjectPointer
  var y = x
  x = y
}
// CHECK:    define void @_T8refcount2f0FT_T_()
// CHECK:      [[X:%.*]] = alloca [[REFCOUNT]]*, align 8
// CHECK-NEXT: [[Y:%.*]] = alloca [[REFCOUNT]]*, align 8
// CHECK-NEXT: store [[REFCOUNT]]* null, [[REFCOUNT]]** [[X]], align 8
// CHECK-NEXT: [[T0:%.*]] = load [[REFCOUNT]]** [[X]], align 8
// CHECK-NEXT: [[T1:%.*]] = call [[REFCOUNT]]* @swift_retain([[REFCOUNT]]* [[T0]]) nounwind
// CHECK-NEXT: store [[REFCOUNT]]* [[T1]], [[REFCOUNT]]** [[Y]]
// CHECK-NEXT: [[T0:%.*]] = load [[REFCOUNT]]** [[Y]], align 8
// CHECK-NEXT: [[T1:%.*]] = call [[REFCOUNT]]* @swift_retain([[REFCOUNT]]* [[T0]]) nounwind
// CHECK-NEXT: [[T2:%.*]] = load [[REFCOUNT]]** [[X]], align 8
// CHECK-NEXT: store [[REFCOUNT]]* [[T1]], [[REFCOUNT]]** [[X]]
// CHECK-NEXT: call void @swift_release([[REFCOUNT]]* [[T2]]) nounwind
// CHECK-NEXT: ret void
