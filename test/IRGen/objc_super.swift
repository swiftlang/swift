// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -target x86_64-apple-darwin10 -module-cache-path %t/clang-module-cache -sdk %S/Inputs -I=%S/Inputs -enable-source-import %s -emit-ir | FileCheck %s
import gizmo

// CHECK: [[CLASS:%objc_class]] = type
// CHECK: [[TYPE:%swift.type]] = type
// CHECK: [[HOOZIT:%C10objc_super6Hoozit]] = type
// CHECK: [[NSRECT:%VSC6NSRect]] = type
// CHECK: [[SUPER:%objc_super]] = type
// CHECK: [[OBJC:%objc_object]] = type
// CHECK: [[GIZMO:%CSo5Gizmo]] = type

class Hoozit : Gizmo {
  // CHECK: define void @_TFC10objc_super6Hoozit4frobfS0_FT_T_([[HOOZIT]]*) {
  override func frob() {
    // CHECK: store [[CLASS]]* bitcast ([[TYPE]]* getelementptr {{.*}} @_TMdC10objc_super6Hoozit {{.*}} to [[CLASS]]*), [[CLASS]]** {{.*}}, align 8
    // CHECK: load i8** @"\01L_selector(frob)"
    // CHECK: call void bitcast (void ()* @objc_msgSendSuper2 to void ([[SUPER]]*, i8*)*)([[SUPER]]* {{.*}}, i8* {{.*}})
    super.frob()
  }
  // CHECK: }

  // CHECK: define void @_TFC10objc_super6Hoozit5runcefMS0_FT_T_([[TYPE]]*) {
  override class func runce() {
    // CHECK: store [[CLASS]]* @"OBJC_METACLASS_$__TtC10objc_super6Hoozit", [[CLASS]]** {{.*}}, align 8
    // CHECK: load i8** @"\01L_selector(runce)"
    // CHECK: call void bitcast (void ()* @objc_msgSendSuper2 to void ([[SUPER]]*, i8*)*)([[SUPER]]* {{.*}}, i8* {{.*}})
    super.runce()
  }
  // CHECK: }

  // CHECK: define void @_TFC10objc_super6Hoozit5framefS0_FT_VSC6NSRect(%VSC6NSRect* noalias sret, %C10objc_super6Hoozit*) {
  override func frame() -> NSRect {
    // CHECK: store [[CLASS]]* bitcast ([[TYPE]]* getelementptr {{.*}} @_TMdC10objc_super6Hoozit {{.*}} to [[CLASS]]*), [[CLASS]]** {{.*}}, align 8
    // CHECK: load i8** @"\01L_selector(frame)"
    // CHECK: call void bitcast (void ()* @objc_msgSendSuper2_stret to void ([[NSRECT]]*, [[SUPER]]*, i8*)*)([[NSRECT]]* noalias sret {{.*}}, [[SUPER]]* {{.*}}, i8* {{.*}})
    return NSInsetRect(super.frame(), 2.0, 2.0)
  }
  // CHECK: }

  // CHECK: define [[HOOZIT]]* @_TFC10objc_super6HoozitcfMS0_FT1xSi_S0_(i64, %C10objc_super6Hoozit*) {
  init(x:Int) {
    // CHECK: load i8** @"\01L_selector(init)"
    // CHECK: call [[OPAQUE:.*]]* bitcast (void ()* @objc_msgSendSuper2 to [[OPAQUE:.*]]* (%objc_super*, i8*)*)([[SUPER]]* {{.*}}, i8* {{.*}})
    // CHECK-NOT: @swift_dynamicCastClassUnconditional
    // CHECK: ret
    super.init()
  }
  // CHECK: }

  // CHECK: define [[HOOZIT]]* @_TFC10objc_super6HoozitcfMS0_FT1ySi_S0_(i64, %C10objc_super6Hoozit*) {
  init(y:Int) {
    // CHECK: load i8** @"\01L_selector(initWithBellsOn:)"
    // CHECK: call [[OPAQUE:.*]]* bitcast (void ()* @objc_msgSendSuper2 to [[OPAQUE:.*]]* (%objc_super*, i8*, i64)*)([[SUPER]]* {{.*}}, i8* {{.*}}, i64 {{.*}})
    // CHECK-NOT: swift_dynamicCastClassUnconditional
    // CHECK: ret
    super.init(bellsOn:y)
  }
  // CHECK: }
}
