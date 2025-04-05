// RUN: %target-swift-emit-silgen -target %target-cpu-apple-macosx13 -target-variant %target-cpu-apple-ios16-macabi -module-name Test -parse-as-library %s -verify -unavailable-decl-optimization=stub | %FileCheck %s --check-prefixes=CHECK,CHECK-NO-EXTENSION
// RUN: %target-swift-emit-silgen -target %target-cpu-apple-macosx13 -target-variant %target-cpu-apple-ios16-macabi -module-name Test -parse-as-library %s -verify -unavailable-decl-optimization=stub -application-extension | %FileCheck %s --check-prefixes=CHECK,CHECK-EXTENSION

// REQUIRES: OS=macosx

// CHECK-LABEL:     sil{{.*}}@$s4Test15unavailableFuncyyF
// CHECK:             [[FNREF:%.*]] = function_ref @$ss31_diagnoseUnavailableCodeReacheds5NeverOyFTwb : $@convention(thin) () -> Never
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

// CHECK-LABEL:     sil{{.*}}@$s4Test28unavailableOnMacCatalystFuncyyF
// CHECK-NOT:         _diagnoseUnavailableCodeReached
// CHECK:           } // end sil function '$s4Test28unavailableOnMacCatalystFuncyyF'
@available(macCatalyst, unavailable)
public func unavailableOnMacCatalystFunc() {}

// CHECK-LABEL:     sil{{.*}}@$s4Test28unavailableOnMacOSAndiOSFuncyyF
// CHECK:             [[FNREF:%.*]] = function_ref @$ss31_diagnoseUnavailableCodeReacheds5NeverOyFTwb : $@convention(thin) () -> Never
// CHECK-NEXT:        [[APPLY:%.*]] = apply [[FNREF]]()
// CHECK:           } // end sil function '$s4Test28unavailableOnMacOSAndiOSFuncyyF'
@available(macOS, unavailable)
@available(iOS, unavailable)
public func unavailableOnMacOSAndiOSFunc() {}

// CHECK-LABEL:             sil{{.*}}@$s4Test33availableOnMacOSExtensionOnlyFuncyyF
// CHECK-NO-EXTENSION:        [[FNREF:%.*]] = function_ref @$ss31_diagnoseUnavailableCodeReacheds5NeverOyFTwb : $@convention(thin) () -> Never
// CHECK-NO-EXTENSION-NEXT:   [[APPLY:%.*]] = apply [[FNREF]]()
// CHECK-EXTENSION-NOT:       _diagnoseUnavailableCodeReached
// CHECK:                   } // end sil function '$s4Test33availableOnMacOSExtensionOnlyFuncyyF'
@available(macOS, unavailable)
@available(iOS, unavailable)
@available(macOSApplicationExtension, introduced: 10.9)
public func availableOnMacOSExtensionOnlyFunc() {}

// CHECK-LABEL:             sil{{.*}}@$s4Test31availableOniOSExtensionOnlyFuncyyF
// CHECK-NO-EXTENSION:        [[FNREF:%.*]] = function_ref @$ss31_diagnoseUnavailableCodeReacheds5NeverOyFTwb : $@convention(thin) () -> Never
// CHECK-NO-EXTENSION-NEXT:   [[APPLY:%.*]] = apply [[FNREF]]()
// FIXME: [availability] This should not diagnose when building with -application-extension
// CHECK-EXTENSION:           _diagnoseUnavailableCodeReached
// CHECK:                   } // end sil function '$s4Test31availableOniOSExtensionOnlyFuncyyF'
@available(macOS, unavailable)
@available(iOS, unavailable)
@available(iOSApplicationExtension, introduced: 9.0)
public func availableOniOSExtensionOnlyFunc() {}

@available(iOS, unavailable)
public struct UnavailableOniOS {
  // CHECK-LABEL:     sil{{.*}}@$s4Test16UnavailableOniOSV15availableMethodyyF
  // CHECK-NOT:         _diagnoseUnavailableCodeReached
  // CHECK:           } // end sil function '$s4Test16UnavailableOniOSV15availableMethodyyF'
  public func availableMethod() {}

  // FIXME: [availability] This method should diagnose
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
