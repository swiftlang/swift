// RUN: rm -rf %t && mkdir -p %t
// RUN: %build-irgen-test-overlays
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t) %s -emit-ir | %FileCheck %s

// REQUIRES: CPU=x86_64
// REQUIRES: objc_interop

import gizmo

// CHECK: [[CLASS:%objc_class]] = type
// CHECK: [[TYPE:%swift.type]] = type
// CHECK: [[HOOZIT:%C10objc_super6Hoozit]] = type
// CHECK: [[NSRECT:%VSC6NSRect]] = type
// CHECK: [[PARTIAL_APPLY_CLASS:%C10objc_super12PartialApply]] = type
// CHECK: [[SUPER:%objc_super]] = type
// CHECK: [[OBJC:%objc_object]] = type
// CHECK: [[GIZMO:%CSo5Gizmo]] = type

class Hoozit : Gizmo {
  // CHECK: define hidden void @_TFC10objc_super6Hoozit4frobfT_T_([[HOOZIT]]*) {{.*}} {
  override func frob() {
    // CHECK: [[T0:%.*]] = call [[TYPE]]* @_TMaC10objc_super6Hoozit()
    // CHECK: [[T1:%.*]] = bitcast [[TYPE]]* [[T0]] to [[CLASS]]*
    // CHECK: store [[CLASS]]* [[T1]], [[CLASS]]** {{.*}}, align 8
    // CHECK: load i8*, i8** @"\01L_selector(frob)"
    // CHECK: call void bitcast (void ()* @objc_msgSendSuper2 to void ([[SUPER]]*, i8*)*)([[SUPER]]* {{.*}}, i8* {{.*}})
    super.frob()
  }
  // CHECK: }

  // CHECK: define hidden void @_TZFC10objc_super6Hoozit5runcefT_T_([[TYPE]]*) {{.*}} {
  override class func runce() {
    // CHECK: store [[CLASS]]* @"OBJC_METACLASS_$__TtC10objc_super6Hoozit", [[CLASS]]** {{.*}}, align 8
    // CHECK: load i8*, i8** @"\01L_selector(runce)"
    // CHECK: call void bitcast (void ()* @objc_msgSendSuper2 to void ([[SUPER]]*, i8*)*)([[SUPER]]* {{.*}}, i8* {{.*}})
    super.runce()
  }
  // CHECK: }

  // CHECK: define hidden void @_TFC10objc_super6Hoozit5framefT_VSC6NSRect(%VSC6NSRect* noalias nocapture sret, %C10objc_super6Hoozit*) {{.*}} {
  override func frame() -> NSRect {
    // CHECK: [[T0:%.*]] = call [[TYPE]]* @_TMaC10objc_super6Hoozit()
    // CHECK: [[T1:%.*]] = bitcast [[TYPE]]* [[T0]] to [[CLASS]]*
    // CHECK: store [[CLASS]]* [[T1]], [[CLASS]]** {{.*}}, align 8
    // CHECK: load i8*, i8** @"\01L_selector(frame)"
    // CHECK: call void bitcast (void ()* @objc_msgSendSuper2_stret to void ([[NSRECT]]*, [[SUPER]]*, i8*)*)([[NSRECT]]* noalias nocapture sret {{.*}}, [[SUPER]]* {{.*}}, i8* {{.*}})
    return NSInsetRect(super.frame(), 2.0, 2.0)
  }
  // CHECK: }

  // CHECK: define hidden [[HOOZIT]]* @_TFC10objc_super6HoozitcfT1xSi_S0_(i64, %C10objc_super6Hoozit*) {{.*}} {
  init(x:Int) {
    // CHECK: load i8*, i8** @"\01L_selector(init)"
    // CHECK: call [[OPAQUE:.*]]* bitcast (void ()* @objc_msgSendSuper2 to [[OPAQUE:.*]]* (%objc_super*, i8*)*)([[SUPER]]* {{.*}}, i8* {{.*}})
    // CHECK-NOT: @swift_dynamicCastClassUnconditional
    // CHECK: ret
    super.init()
  }
  // CHECK: }

  // CHECK: define hidden [[HOOZIT]]* @_TFC10objc_super6HoozitcfT1ySi_S0_(i64, %C10objc_super6Hoozit*) {{.*}} {
  init(y:Int) {
    // CHECK: load i8*, i8** @"\01L_selector(initWithBellsOn:)"
    // CHECK: call [[OPAQUE:.*]]* bitcast (void ()* @objc_msgSendSuper2 to [[OPAQUE:.*]]* (%objc_super*, i8*, i64)*)([[SUPER]]* {{.*}}, i8* {{.*}}, i64 {{.*}})
    // CHECK-NOT: swift_dynamicCastClassUnconditional
    // CHECK: ret
    super.init(bellsOn:y)
  }
  // CHECK: }
}

func acceptFn(_ fn: () -> Void) { }

class PartialApply : Gizmo {
  // CHECK: define hidden void @_TFC10objc_super12PartialApply4frobfT_T_([[PARTIAL_APPLY_CLASS]]*) {{.*}} {
  override func frob() {
    // CHECK: call void @_TF10objc_super8acceptFnFFT_T_T_(i8* bitcast (void (%swift.refcounted*)* [[PARTIAL_FORWARDING_THUNK:@[A-Za-z0-9_]+]] to i8*), %swift.refcounted* %3)
    acceptFn(super.frob)
  }
  // CHECK: }

  // CHECK: define internal void [[PARTIAL_FORWARDING_THUNK]](%swift.refcounted*) #0 {
  // CHECK: call %swift.type* @_TMaC10objc_super12PartialApply()
  // CHECK: @"\01L_selector(frob)"
  // CHECK: call void bitcast (void ()* @objc_msgSendSuper2
  // CHECK: }
}
