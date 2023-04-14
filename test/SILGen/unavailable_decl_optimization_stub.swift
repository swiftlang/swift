// RUN: %target-swift-emit-silgen -module-name Test -parse-as-library %s -verify -unavailable-decl-optimization=stub | %FileCheck %s --check-prefixes=CHECK

public struct S {}

// CHECK-LABEL: sil {{.*}} @$s4Test15unavailableFuncAA1SVyF
// CHECK:         [[FNREF:%.*]] = function_ref @$ss31_diagnoseUnavailableCodeReacheds5NeverOyF
// CHECK-NEXT:    [[APPLY:%.*]] = apply [[FNREF]]()
// CHECK:         function_ref @$s4Test1SVACycfC
// CHECK:       } // end sil function '$s4Test15unavailableFuncAA1SVyF'
@available(*, unavailable)
public func unavailableFunc() -> S {
  return S()
}

enum SomeError: Error { case generic }

// CHECK-LABEL: sil {{.*}} @$s4Test23unavailableThrowingFuncyyKF
// CHECK:         [[FNREF:%.*]] = function_ref @$ss31_diagnoseUnavailableCodeReacheds5NeverOyF
// CHECK-NEXT:    [[APPLY:%.*]] = apply [[FNREF]]()
// CHECK:         throw
// CHECK:       } // end sil function '$s4Test23unavailableThrowingFuncyyKF'
@available(*, unavailable)
public func unavailableThrowingFunc() throws {
  throw SomeError.generic
}
