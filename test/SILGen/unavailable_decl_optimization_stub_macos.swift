// RUN: %target-swift-emit-silgen -target %target-swift-abi-5.8-triple -module-name Test -parse-as-library %s -verify -unavailable-decl-optimization=stub | %FileCheck %s --check-prefixes=CHECK-SWIFT5_8
// RUN: %target-swift-emit-silgen -target %target-swift-abi-5.9-triple -module-name Test -parse-as-library %s -verify -unavailable-decl-optimization=stub | %FileCheck %s --check-prefixes=CHECK-SWIFT5_9

// REQUIRES: OS=macosx

// CHECK-LABEL:     sil{{.*}}@$s4Test15unavailableFuncyyF
// CHECK-SWIFT5_8:    [[FNREF:%.*]] = function_ref @$ss36_diagnoseUnavailableCodeReached_aeics5NeverOyF : $@convention(thin) () -> Never
// CHECK-SWIFT5_9:    [[FNREF:%.*]] = function_ref @$ss31_diagnoseUnavailableCodeReacheds5NeverOyF : $@convention(thin) () -> Never
// CHECK-NEXT:        [[APPLY:%.*]] = apply [[FNREF]]()
// CHECK:           } // end sil function '$s4Test15unavailableFuncyyF'
@available(*, unavailable)
public func unavailableFunc() {}

// CHECK-LABEL:     sil{{.*}}@$s4Test24unavailableInlinableFuncyyF
// CHECK-SWIFT5_8:    [[FNREF:%.*]] = function_ref @$ss36_diagnoseUnavailableCodeReached_aeics5NeverOyF : $@convention(thin) () -> Never
// CHECK-SWIFT5_9:    [[FNREF:%.*]] = function_ref @$ss31_diagnoseUnavailableCodeReacheds5NeverOyFTwb : $@convention(thin) () -> Never
// CHECK-NEXT:        [[APPLY:%.*]] = apply [[FNREF]]()
// CHECK:           } // end sil function '$s4Test24unavailableInlinableFuncyyF'
@available(*, unavailable)
@inlinable public func unavailableInlinableFunc() {}

