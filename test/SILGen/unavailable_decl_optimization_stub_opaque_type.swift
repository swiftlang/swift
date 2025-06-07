// RUN: %target-swift-emit-silgen -target %target-swift-5.1-abi-triple -module-name Test -parse-as-library %s -verify -unavailable-decl-optimization=stub | %FileCheck %s --check-prefixes=CHECK

public protocol P {}
public struct S {}
extension S: P {}

// CHECK-LABEL: sil{{.*}}@$s4Test27unavailableOpaqueReturnFuncQryF
// CHECK:         [[FNREF:%.*]] = function_ref @$[[DIAGNOSEFN:(ss31_diagnoseUnavailableCodeReacheds5NeverOyF|ss31_diagnoseUnavailableCodeReacheds5NeverOyFTwb)]] : $@convention(thin) () -> Never
// CHECK-NEXT:    [[APPLY:%.*]] = apply [[FNREF]]()
// CHECK:         function_ref @$s4Test1SVACycfC
// CHECK:       } // end sil function '$s4Test27unavailableOpaqueReturnFuncQryF'
@available(*, unavailable)
public func unavailableOpaqueReturnFunc() -> some P {
  return S()
}
