// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -triple x86_64-apple-darwin10 -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs %s -emit-llvm | FileCheck %s
import gizmo

// CHECK: [[TYPE:%swift.type]] = type
// CHECK: [[REFCOUNT:%swift.refcounted]] = type {
// CHECK: [[GIZMO:%CSo5Gizmo]] = type
// CHECK: [[OBJC:%objc_object]] = type

func dumpGizmo(g:Gizmo) {}

// CHECK: define void @_T11objc_blocks13dumpSubGizmosFT1gCSo5Gizmo_T_([[GIZMO]]* %g) {
func dumpSubGizmos(g:Gizmo) {
  g.enumerateSubGizmos({ dumpGizmo($0) })
  // CHECK: [[ENUMERATE_SUB_GIZMOS:%.*]] = load i8** @"\01L_selector(enumerateSubGizmos:)"
  // CHECK: [[BRIDGED_BLOCK:%.*]] = call [[OBJC]]* @_TTbbCSo5GizmoT_
  // CHECK: call void {{.*}} @objc_msgSend {{.*}}([[GIZMO]]* {{%.*}}, i8* [[ENUMERATE_SUB_GIZMOS]], [[OBJC]]* [[BRIDGED_BLOCK]])
}
