// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -triple x86_64-apple-darwin10 -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs %s -constraint-checker -emit-llvm | FileCheck %s
import gizmo

// CHECK: [[CLASS:%objc_class]] = type
// CHECK: [[TYPE:%swift.type]] = type
// CHECK: [[REFCOUNT:%swift.refcounted]] = type {
// CHECK: [[HOOZIT:%CSo6Hoozit]] = type
// CHECK: [[NSRECT:%V6NSRect]] = type
// CHECK: [[SUPER:%objc_super]] = type
// CHECK: [[OBJC:%objc_object]] = type
// CHECK: [[GIZMO:%CSo5Gizmo]] = type

class [objc] Hoozit : Gizmo {
  // CHECK: define void @_TCSo6Hoozit4frobfS_FT_T_([[HOOZIT]]* %this) {
  func [objc] frob() {
    // CHECK: store [[CLASS]]* bitcast ([[TYPE]]* getelementptr {{.*}} @_TMdCSo6Hoozit {{.*}} to [[CLASS]]*), [[CLASS]]** {{.*}}, align 8
    // CHECK: load i8** @"\01L_selector(frob)"
    // CHECK: call void bitcast (void ()* @objc_msgSendSuper2 to void ([[SUPER]]*, i8*)*)([[SUPER]]* {{.*}}, i8* {{.*}})
    super.frob()
  }
  // CHECK: }

  // CHECK: define void @_TCSo6Hoozit5runcefMS_FT_T_([[TYPE]]* %this) {
  static func [objc] runce() {
    // CHECK: store [[CLASS]]* @"OBJC_METACLASS_$_Hoozit", [[CLASS]]** {{.*}}, align 8
    // CHECK: load i8** @"\01L_selector(runce)"
    // CHECK: call void bitcast (void ()* @objc_msgSendSuper2 to void ([[SUPER]]*, i8*)*)([[SUPER]]* {{.*}}, i8* {{.*}})
    super.runce()
  }
  // CHECK: }

  // CHECK: define void @_TCSo6Hoozit5framefS_FT_V6NSRect(%V6NSRect* noalias sret, %CSo6Hoozit* %this) {
  func [objc] frame() -> NSRect {
    // CHECK: store [[CLASS]]* bitcast ([[TYPE]]* getelementptr {{.*}} @_TMdCSo6Hoozit {{.*}} to [[CLASS]]*), [[CLASS]]** {{.*}}, align 8
    // CHECK: load i8** @"\01L_selector(frame)"
    // CHECK: call void bitcast (void ()* @objc_msgSendSuper2_stret to void ([[NSRECT]]*, [[SUPER]]*, i8*)*)([[NSRECT]]* noalias sret {{.*}}, [[SUPER]]* {{.*}}, i8* {{.*}})
    return NSInsetRect(super.frame(), 2.0, 2.0)
  }
  // CHECK: }
}
