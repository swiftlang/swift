// RUN: %target-swift-emit-silgen-ossa -o /dev/null -enable-sil-opaque-values -parse-as-library %s
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
// CHECK-EXTENSION-NOT:       _diagnoseUnavailableCodeReached
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

public struct S {}

// CHECK-LABEL:     sil{{.*}}@$s4Test30availableOnMacCatalystOnlyFuncyyF
// CHECK-NOT:         _diagnoseUnavailableCodeReached
// CHECK:           } // end sil function '$s4Test30availableOnMacCatalystOnlyFuncyyF'
@available(macOS, unavailable)
@available(iOS, unavailable)
@available(macCatalyst, introduced: 13.1)
public func availableOnMacCatalystOnlyFunc() {}

// CHECK-LABEL:     sil private [global_init_once_fn]{{.*}}@$s4Test32availableOnMacCatalystOnlyGlobal_WZ
// CHECK-NOT:         _diagnoseUnavailableCodeReached
// CHECK:           } // end sil function '$s4Test32availableOnMacCatalystOnlyGlobal_WZ'
// CHECK-LABEL:     sil [global_init]{{.*}}@$s4Test32availableOnMacCatalystOnlyGlobalAA1SVvau
// CHECK-NOT:         _diagnoseUnavailableCodeReached
// CHECK:           } // end sil function '$s4Test32availableOnMacCatalystOnlyGlobalAA1SVvau'
@available(macOS, unavailable)
@available(iOS, unavailable)
@available(macCatalyst, introduced: 13.1)
public let availableOnMacCatalystOnlyGlobal = S()

// CHECK-LABEL:     sil private [global_init_once_fn]{{.*}}@$s4Test30unavailableOnMacOSAndiOSGlobal_WZ
// CHECK:             [[FNREF:%.*]] = function_ref @$ss31_diagnoseUnavailableCodeReacheds5NeverOyFTwb : $@convention(thin) () -> Never
// CHECK-NEXT:        [[APPLY:%.*]] = apply [[FNREF]]()
// CHECK:           } // end sil function '$s4Test30unavailableOnMacOSAndiOSGlobal_WZ'
// CHECK-LABEL:     sil [global_init]{{.*}}@$s4Test30unavailableOnMacOSAndiOSGlobalAA1SVvau
// CHECK:             [[FNREF:%.*]] = function_ref @$ss31_diagnoseUnavailableCodeReacheds5NeverOyFTwb : $@convention(thin) () -> Never
// CHECK-NEXT:        [[APPLY:%.*]] = apply [[FNREF]]()
// CHECK:           } // end sil function '$s4Test30unavailableOnMacOSAndiOSGlobalAA1SVvau'
@available(macOS, unavailable)
@available(iOS, unavailable)
public let unavailableOnMacOSAndiOSGlobal = S()

public struct HasStaticProperties {
  // CHECK-LABEL:     sil private [global_init_once_fn]{{.*}}@$s4Test19HasStaticPropertiesV026availableOnMacCatalystOnlyC3Let_WZ
  // CHECK-NOT:         _diagnoseUnavailableCodeReached
  // CHECK:           } // end sil function '$s4Test19HasStaticPropertiesV026availableOnMacCatalystOnlyC3Let_WZ'
  @available(macOS, unavailable)
  @available(iOS, unavailable)
  @available(macCatalyst, introduced: 13.1)
  public static let availableOnMacCatalystOnlyStaticLet = S()
}
