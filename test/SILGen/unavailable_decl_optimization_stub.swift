// RUN: %target-swift-emit-silgen -module-name Test -parse-as-library %s -verify -unavailable-decl-optimization=stub | %FileCheck %s --check-prefixes=CHECK

public struct S {}

// CHECK-LABEL: sil{{.*}}@$s4Test15unavailableFuncAA1SVyF
// CHECK:         [[FNREF:%.*]] = function_ref @$[[DIAGNOSEFN:(ss31_diagnoseUnavailableCodeReacheds5NeverOyF|ss31_diagnoseUnavailableCodeReacheds5NeverOyFTwb)]] : $@convention(thin) () -> Never
// CHECK-NEXT:    [[APPLY:%.*]] = apply [[FNREF]]()
// CHECK:         function_ref @$s4Test1SVACycfC
// CHECK:       } // end sil function '$s4Test15unavailableFuncAA1SVyF'
@available(*, unavailable)
public func unavailableFunc() -> S {
  return S()
}

// CHECK-LABEL: sil{{.*}}@$s4Test025unavailableFuncWithNestedC0yyF
// CHECK:         [[FNREF:%.*]] = function_ref @$[[DIAGNOSEFN:(ss31_diagnoseUnavailableCodeReacheds5NeverOyF|ss31_diagnoseUnavailableCodeReacheds5NeverOyFTwb)]] : $@convention(thin) () -> Never
// CHECK-NEXT:    [[APPLY:%.*]] = apply [[FNREF]]()
// CHECK:         function_ref @$s4Test025unavailableFuncWithNestedC0yyF6nestedL_SiyF
// CHECK:       } // end sil function '$s4Test025unavailableFuncWithNestedC0yyF'
@available(*, unavailable)
public func unavailableFuncWithNestedFunc() {
  func nested() -> Int { 1 }
  _ = nested()
}

// CHECK-LABEL: sil{{.*}}@$s4Test025unavailableFuncWithNestedC0yyF6nestedL_SiyF
// CHECK:         [[FNREF:%.*]] = function_ref @$[[DIAGNOSEFN:(ss31_diagnoseUnavailableCodeReacheds5NeverOyF|ss31_diagnoseUnavailableCodeReacheds5NeverOyFTwb)]] : $@convention(thin) () -> Never
// CHECK-NEXT:    [[APPLY:%.*]] = apply [[FNREF]]()
// CHECK:       } // end sil function '$s4Test025unavailableFuncWithNestedC0yyF6nestedL_SiyF'

// CHECK-LABEL: sil{{.*}}@$s4Test033unavailableFuncWithObsoleteNestedC0yyF
// CHECK:         [[FNREF:%.*]] = function_ref @$[[DIAGNOSEFN:(ss31_diagnoseUnavailableCodeReacheds5NeverOyF|ss31_diagnoseUnavailableCodeReacheds5NeverOyFTwb)]] : $@convention(thin) () -> Never
// CHECK-NEXT:    [[APPLY:%.*]] = apply [[FNREF]]()
// CHECK:       } // end sil function '$s4Test033unavailableFuncWithObsoleteNestedC0yyF'
@available(*, unavailable)
public func unavailableFuncWithObsoleteNestedFunc() {
  @available(swift, obsoleted: 1)
  func nested() -> Int { 1 }
}

// CHECK-LABEL: sil{{.*}}@$s4Test033unavailableFuncWithObsoleteNestedC0yyF6nestedL_SiyF
// CHECK:         [[FNREF:%.*]] = function_ref @$[[DIAGNOSEFN:(ss31_diagnoseUnavailableCodeReacheds5NeverOyF|ss31_diagnoseUnavailableCodeReacheds5NeverOyFTwb)]] : $@convention(thin) () -> Never
// CHECK-NEXT:    [[APPLY:%.*]] = apply [[FNREF]]()
// CHECK:       } // end sil function '$s4Test033unavailableFuncWithObsoleteNestedC0yyF6nestedL_SiyF'

enum SomeError: Error { case generic }

// CHECK-LABEL: sil{{.*}}@$s4Test23unavailableThrowingFuncyyKF
// CHECK:         [[FNREF:%.*]] = function_ref @$[[DIAGNOSEFN]] : $@convention(thin) () -> Never
// CHECK-NEXT:    [[APPLY:%.*]] = apply [[FNREF]]()
// CHECK:         throw
// CHECK:       } // end sil function '$s4Test23unavailableThrowingFuncyyKF'
@available(*, unavailable)
public func unavailableThrowingFunc() throws {
  throw SomeError.generic
}

// one-time initialization function for globalVar
// CHECK-LABEL: sil{{.*}}@$s4Test9globalVar_WZ
// CHECK:         [[FNREF:%.*]] = function_ref @$[[DIAGNOSEFN]] : $@convention(thin) () -> Never
// CHECK-NEXT:    [[APPLY:%.*]] = apply [[FNREF]]()
// CHECK:         alloc_global
// CHECK:       } // end sil function '$s4Test9globalVar_WZ'
//
// globalVar.unsafeMutableAddressor
// CHECK-LABEL: sil{{.*}}@$s4Test9globalVarAA1SVvau
// CHECK:         [[FNREF:%.*]] = function_ref @$[[DIAGNOSEFN]] : $@convention(thin) () -> Never
// CHECK-NEXT:    [[APPLY:%.*]] = apply [[FNREF]]()
// CHECK:         global_addr @$s4Test9globalVar_Wz
// CHECK:       } // end sil function '$s4Test9globalVarAA1SVvau'
@available(*, unavailable)
public var globalVar = S()

public enum Uninhabited {}

// CHECK-LABEL: sil{{.*}}@$s4Test28unavailableTakingUninhabitedyyAA0D0OF : $@convention(thin) (Uninhabited) -> () {
// CHECK:         unreachable
// CHECK:       } // end sil function '$s4Test28unavailableTakingUninhabitedyyAA0D0OF'
@available(*, unavailable)
public func unavailableTakingUninhabited(_ u: Uninhabited) {}

// CHECK-LABEL: sil{{.*}}@$s4Test17obsoletedInSwift1yyF : $@convention(thin) () -> () {
// CHECK-NOT:     ss36_diagnoseUnavailableCodeReached
// CHECK:       } // end sil function '$s4Test17obsoletedInSwift1yyF'
@available(swift, obsoleted: 1)
public func obsoletedInSwift1() {}

// CHECK-LABEL: sil{{.*}}@$s4Test17obsoletedInSwift5yyF : $@convention(thin) () -> () {
// CHECK-NOT:     ss36_diagnoseUnavailableCodeReached
// CHECK:       } // end sil function '$s4Test17obsoletedInSwift5yyF'
@available(swift, obsoleted: 5)
public func obsoletedInSwift5() {}

// CHECK-LABEL: sil{{.*}}@$s4Test19introducedInSwift99yyF : $@convention(thin) () -> () {
// CHECK-NOT:     ss36_diagnoseUnavailableCodeReached
// CHECK:       } // end sil function '$s4Test19introducedInSwift99yyF'
@available(swift, introduced: 99)
public func introducedInSwift99() {}
