// RUN: %target-swift-emit-silgen -target %target-swift-abi-5.8-triple -module-name Test -parse-as-library %s -verify -unavailable-decl-optimization=stub | %FileCheck %s --check-prefixes=CHECK,CHECK-SWIFT5_8
// RUN: %target-swift-emit-silgen -target %target-swift-abi-5.8-triple -module-name Test -parse-as-library %s -verify -unavailable-decl-optimization=stub -application-extension | %FileCheck %s --check-prefixes=CHECK,CHECK-SWIFT5_8
// RUN: %target-swift-emit-silgen -target %target-swift-abi-5.9-triple -module-name Test -parse-as-library %s -verify -unavailable-decl-optimization=stub | %FileCheck %s --check-prefixes=CHECK,CHECK-SWIFT5_9
// RUN: %target-swift-emit-silgen -target %target-swift-abi-5.9-triple -module-name Test -parse-as-library %s -verify -unavailable-decl-optimization=stub -application-extension | %FileCheck %s --check-prefixes=CHECK,CHECK-SWIFT5_9

// REQUIRES: OS=macosx

// CHECK-LABEL:     sil{{.*}}@$s4Test15unavailableFuncyyF
// CHECK-SWIFT5_8:    [[FNREF:%.*]] = function_ref @$ss31_diagnoseUnavailableCodeReacheds5NeverOyFTwb : $@convention(thin) () -> Never
// CHECK-SWIFT5_9:    [[FNREF:%.*]] = function_ref @$ss31_diagnoseUnavailableCodeReacheds5NeverOyF : $@convention(thin) () -> Never
// CHECK-NEXT:        [[APPLY:%.*]] = apply [[FNREF]]()
// CHECK:           } // end sil function '$s4Test15unavailableFuncyyF'
@available(*, unavailable)
public func unavailableFunc() {}

// CHECK-LABEL:     sil{{.*}}@$s4Test24unavailableInlinableFuncyyF
// CHECK-SWIFT5_8:    [[FNREF:%.*]] = function_ref @$ss31_diagnoseUnavailableCodeReacheds5NeverOyFTwb : $@convention(thin) () -> Never
// CHECK-SWIFT5_9:    [[FNREF:%.*]] = function_ref @$ss31_diagnoseUnavailableCodeReacheds5NeverOyFTwb : $@convention(thin) () -> Never
// CHECK-NEXT:        [[APPLY:%.*]] = apply [[FNREF]]()
// CHECK:           } // end sil function '$s4Test24unavailableInlinableFuncyyF'
@available(*, unavailable)
@inlinable public func unavailableInlinableFunc() {}

// CHECK-LABEL:     sil{{.*}}@$s4Test22unavailableOnMacOSFuncyyF
// CHECK-SWIFT5_8:    [[FNREF:%.*]] = function_ref @$ss31_diagnoseUnavailableCodeReacheds5NeverOyFTwb : $@convention(thin) () -> Never
// CHECK-SWIFT5_9:    [[FNREF:%.*]] = function_ref @$ss31_diagnoseUnavailableCodeReacheds5NeverOyF : $@convention(thin) () -> Never
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
// CHECK-SWIFT5_8:    [[FNREF:%.*]] = function_ref @$ss31_diagnoseUnavailableCodeReacheds5NeverOyFTwb : $@convention(thin) () -> Never
// CHECK-SWIFT5_9:    [[FNREF:%.*]] = function_ref @$ss31_diagnoseUnavailableCodeReacheds5NeverOyF : $@convention(thin) () -> Never
// CHECK-NEXT:        [[APPLY:%.*]] = apply [[FNREF]]()
// CHECK:           } // end sil function '$s4Test021unavailableOnMacOSAndD15OSExtensionFuncyyF'
@available(macOS, unavailable)
@available(macOSApplicationExtension, unavailable) // FIXME: Seems like this should be diagnosed as redundant
public func unavailableOnMacOSAndMacOSExtensionFunc() {}

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
