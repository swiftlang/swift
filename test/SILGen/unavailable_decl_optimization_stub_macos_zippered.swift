// RUN: %target-swift-emit-silgen -target %target-cpu-apple-macosx13 -target-variant %target-cpu-apple-ios16-macabi -module-name Test -parse-as-library %s -verify -unavailable-decl-optimization=stub | %FileCheck %s

// REQUIRES: OS=macosx

// CHECK-LABEL:     sil{{.*}}@$s4Test15unavailableFuncyyF
// CHECK:             [[FNREF:%.*]] = function_ref @$ss36_diagnoseUnavailableCodeReached_aeics5NeverOyF : $@convention(thin) () -> Never
// CHECK-NEXT:        [[APPLY:%.*]] = apply [[FNREF]]()
// CHECK:           } // end sil function '$s4Test15unavailableFuncyyF'
@available(*, unavailable)
public func unavailableFunc() {}

// CHECK-LABEL:     sil{{.*}}@$s4Test22unavailableOnMacOSFuncyyF
// CHECK-NOT:         _diagnoseUnavailableCodeReached
// CHECK:           } // end sil function '$s4Test22unavailableOnMacOSFuncyyF'
@available(macOS, unavailable)
public func unavailableOnMacOSFunc() {}

// CHECK-LABEL:     sil{{.*}}@$s4Test20unavailableOniOSFuncyyF
// CHECK-NOT:         _diagnoseUnavailableCodeReached
// CHECK:           } // end sil function '$s4Test20unavailableOniOSFuncyyF'
@available(iOS, unavailable)
public func unavailableOniOSFunc() {}

// FIXME: This function should diagnose (rdar://125930716)
// CHECK-LABEL:     sil{{.*}}@$s4Test28unavailableOnMacCatalystFuncyyF
// CHECK-NOT:         _diagnoseUnavailableCodeReached
// CHECK:           } // end sil function '$s4Test28unavailableOnMacCatalystFuncyyF'
@available(macCatalyst, unavailable)
public func unavailableOnMacCatalystFunc() {}

// FIXME: This function should diagnose (rdar://125930716)
// CHECK-LABEL:     sil{{.*}}@$s4Test28unavailableOnMacOSAndiOSFuncyyF
// CHECK-NOT:         _diagnoseUnavailableCodeReached
// CHECK:           } // end sil function '$s4Test28unavailableOnMacOSAndiOSFuncyyF'
@available(macOS, unavailable)
@available(iOS, unavailable)
public func unavailableOnMacOSAndiOSFunc() {}

@available(iOS, unavailable)
public struct UnavailableOniOS {
  // CHECK-LABEL:     sil{{.*}}@$s4Test16UnavailableOniOSV15availableMethodyyF
  // CHECK-NOT:         _diagnoseUnavailableCodeReached
  // CHECK:           } // end sil function '$s4Test16UnavailableOniOSV15availableMethodyyF'
  public func availableMethod() {}

  // FIXME: This function should diagnose (rdar://125930716)
  // CHECK-LABEL:     sil{{.*}}@$s4Test16UnavailableOniOSV24unavailableOnMacOSMethodyyF
  // CHECK-NOT:         _diagnoseUnavailableCodeReached
  // CHECK:           } // end sil function '$s4Test16UnavailableOniOSV24unavailableOnMacOSMethodyyF'
  @available(macOS, unavailable)
  public func unavailableOnMacOSMethod() {}
}

// CHECK-LABEL:     sil{{.*}}@$s4Test37unavailableOnMacCatalystExtensionFuncyyF
// CHECK-NOT:         _diagnoseUnavailableCodeReached
// CHECK:           } // end sil function '$s4Test37unavailableOnMacCatalystExtensionFuncyyF'
@available(macCatalystApplicationExtension, unavailable)
public func unavailableOnMacCatalystExtensionFunc() {}
