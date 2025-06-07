// RUN: %target-swift-emit-silgen -module-name Test -parse-as-library %s -verify -unavailable-decl-optimization=stub | %FileCheck %s --check-prefixes=CHECK,CHECK-NO-EXTENSION
// RUN: %target-swift-emit-silgen -target %target-swift-5.8-abi-triple -module-name Test -parse-as-library %s -verify -unavailable-decl-optimization=stub -application-extension | %FileCheck %s --check-prefixes=CHECK,CHECK-EXTENSION

// REQUIRES: OS=macosx

// CHECK-LABEL:     sil{{.*}}@$s4Test15unavailableFuncyyF
// CHECK:             [[FNREF:%.*]] = function_ref @$ss31_diagnoseUnavailableCodeReacheds5NeverOyFTwb : $@convention(thin) () -> Never
// CHECK-NEXT:        [[APPLY:%.*]] = apply [[FNREF]]()
// CHECK:           } // end sil function '$s4Test15unavailableFuncyyF'
@available(*, unavailable)
public func unavailableFunc() {}

// CHECK-LABEL:     sil{{.*}}@$s4Test24unavailableInlinableFuncyyF
// CHECK:             [[FNREF:%.*]] = function_ref @$ss31_diagnoseUnavailableCodeReacheds5NeverOyFTwb : $@convention(thin) () -> Never
// CHECK-NEXT:        [[APPLY:%.*]] = apply [[FNREF]]()
// CHECK:           } // end sil function '$s4Test24unavailableInlinableFuncyyF'
@available(*, unavailable)
@inlinable public func unavailableInlinableFunc() {}

// CHECK-LABEL:     sil{{.*}}@$s4Test22unavailableOnMacOSFuncyyF
// CHECK:             [[FNREF:%.*]] = function_ref @$ss31_diagnoseUnavailableCodeReacheds5NeverOyFTwb : $@convention(thin) () -> Never
// CHECK-NEXT:        [[APPLY:%.*]] = apply [[FNREF]]()
// CHECK:           } // end sil function '$s4Test22unavailableOnMacOSFuncyyF'
@available(macOS, unavailable)
public func unavailableOnMacOSFunc() {}

// CHECK-LABEL:     sil{{.*}}@$s4Test31unavailableOnMacOSExtensionFuncyyF
// CHECK-NOT:         _diagnoseUnavailableCodeReached
// CHECK:           } // end sil function '$s4Test31unavailableOnMacOSExtensionFuncyyF'
@available(macOSApplicationExtension, unavailable)
public func unavailableOnMacOSExtensionFunc() {}

// CHECK-LABEL:     sil{{.*}}@$s4Test021unavailableOnMacOSAndD15OSExtensionFuncyyF
// CHECK:             [[FNREF:%.*]] = function_ref @$ss31_diagnoseUnavailableCodeReacheds5NeverOyFTwb : $@convention(thin) () -> Never
// CHECK-NEXT:        [[APPLY:%.*]] = apply [[FNREF]]()
// CHECK:           } // end sil function '$s4Test021unavailableOnMacOSAndD15OSExtensionFuncyyF'
@available(macOS, unavailable)
@available(macOSApplicationExtension, unavailable) // FIXME: Seems like this should be diagnosed as redundant
public func unavailableOnMacOSAndMacOSExtensionFunc() {}

// CHECK-LABEL:             sil{{.*}}@$s4Test33availableOnMacOSExtensionOnlyFuncyyF
// CHECK-NO-EXTENSION:        [[FNREF:%.*]] = function_ref @$ss31_diagnoseUnavailableCodeReacheds5NeverOyFTwb : $@convention(thin) () -> Never
// CHECK-NO-EXTENSION-NEXT:   [[APPLY:%.*]] = apply [[FNREF]]()
// CHECK-EXTENSION-NOT:       _diagnoseUnavailableCodeReached
// CHECK:           } // end sil function '$s4Test33availableOnMacOSExtensionOnlyFuncyyF'
@available(macOS, unavailable)
@available(macOSApplicationExtension, introduced: 10.9)
public func availableOnMacOSExtensionOnlyFunc() {}

// CHECK-LABEL:     sil{{.*}}@$s4Test20unavailableOniOSFuncyyF
// CHECK-NOT:         _diagnoseUnavailableCodeReached
// CHECK:           } // end sil function '$s4Test20unavailableOniOSFuncyyF'
@available(iOS, unavailable)
public func unavailableOniOSFunc() {}

// CHECK-LABEL:     sil{{.*}}@$s4Test20obsoletedOnMacOS10_9yyF
// CHECK-NOT:         _diagnoseUnavailableCodeReached
// CHECK:           } // end sil function '$s4Test20obsoletedOnMacOS10_9yyF'
@available(macOS, obsoleted: 10.9)
public func obsoletedOnMacOS10_9() {}

// CHECK-LABEL:     sil{{.*}}@$s4Test19introducedInMacOS99yyF
// CHECK-NOT:         _diagnoseUnavailableCodeReached
// CHECK:           } // end sil function '$s4Test19introducedInMacOS99yyF'
@available(macOS, introduced: 99)
public func introducedInMacOS99() {}

// CHECK-LABEL:     sil{{.*}}@$s4Test28unavailableIntroducedInMacOSyyF
// CHECK:             [[FNREF:%.*]] = function_ref @$ss31_diagnoseUnavailableCodeReacheds5NeverOyFTwb : $@convention(thin) () -> Never
// CHECK-NEXT:        [[APPLY:%.*]] = apply [[FNREF]]()
// CHECK:           } // end sil function '$s4Test28unavailableIntroducedInMacOSyyF'
@available(macOS, unavailable, introduced: 10.9)
public func unavailableIntroducedInMacOS() {}

// CHECK-LABEL:     sil{{.*}}@$s4Test31unavailableAndIntroducedInMacOSyyF
// CHECK:             [[FNREF:%.*]] = function_ref @$ss31_diagnoseUnavailableCodeReacheds5NeverOyFTwb : $@convention(thin) () -> Never
// CHECK-NEXT:        [[APPLY:%.*]] = apply [[FNREF]]()
// CHECK:           } // end sil function '$s4Test31unavailableAndIntroducedInMacOSyyF'
@available(macOS, unavailable)
@available(macOS, introduced: 10.9)
public func unavailableAndIntroducedInMacOS() {}

// CHECK-LABEL:     sil{{.*}}@$s4Test31introducedAndUnavailableInMacOSyyF
// CHECK:             [[FNREF:%.*]] = function_ref @$ss31_diagnoseUnavailableCodeReacheds5NeverOyFTwb : $@convention(thin) () -> Never
// CHECK-NEXT:        [[APPLY:%.*]] = apply [[FNREF]]()
// CHECK:           } // end sil function '$s4Test31introducedAndUnavailableInMacOSyyF'
@available(macOS, introduced: 10.9)
@available(macOS, unavailable)
public func introducedAndUnavailableInMacOS() {}

// CHECK-LABEL:     sil{{.*}}@$s4Test17deprecatedInMacOSyyF
// CHECK-NOT:         _diagnoseUnavailableCodeReached
// CHECK:           } // end sil function '$s4Test17deprecatedInMacOSyyF'
@available(macOS, deprecated)
public func deprecatedInMacOS() {}

// CHECK-LABEL:     sil{{.*}}@$s4Test21deprecatedInMacOS10_9yyF
// CHECK-NOT:         _diagnoseUnavailableCodeReached
// CHECK:           } // end sil function '$s4Test21deprecatedInMacOS10_9yyF'
@available(macOS, deprecated: 10.9)
public func deprecatedInMacOS10_9() {}
