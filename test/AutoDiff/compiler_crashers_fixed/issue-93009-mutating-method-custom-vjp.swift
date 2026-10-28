// RUN: %target-swift-frontend -emit-sil -sil-verify-all -module-name main %s -o - | %FileCheck %s

// This used to crash in getThunkedAutoDiffLinearMap when emitting the custom
// VJP thunk for a mutating method. SILGen incorrectly treated the first
// pullback result as the self tangent and reordered (Float, Double) to
// (Double, Float), causing a result-type mismatch. The self tangent is passed
// inout to the pullback, not returned, so there is no self result to reorder.
// See also: https://github.com/swiftlang/swift/issues/93009

import _Differentiation

extension Float {
  mutating func foo(_ x: Float, _ y: Double) {
    self += x + 2 * Float(y)
  }

  // The thunk must preserve the pullback signature and return the result of
  // calling vjpFoo directly, without wrapping it in a self-reordering thunk.

  // CHECK-LABEL: sil hidden [thunk]{{.*}} @$sSf4mainE3fooyySf_SdtFTJrSSSpSr : $@convention(method) (Float, Double, @inout Float) -> @owned @callee_guaranteed (@inout Float) -> (Float, Double) {
  // CHECK:       bb0(%0 : $Float, %1 : $Double, %2 : $*Float):
  // CHECK:         %[[#VJP:]] = function_ref @$sSf4mainE6vjpFooyyt5value_Sf_SdtSfzc8pullbacktSf_SdtF : $@convention(method) (Float, Double, @inout Float) -> @owned @callee_guaranteed (@inout Float) -> (Float, Double)
  // CHECK-NEXT:    %[[#PULLBACK:]] = apply %[[#VJP]](%0, %1, %2) : $@convention(method) (Float, Double, @inout Float) -> @owned @callee_guaranteed (@inout Float) -> (Float, Double)
  // CHECK-NEXT:    return %[[#PULLBACK]]
  // CHECK-NEXT:  } // end sil function '$sSf4mainE3fooyySf_SdtFTJrSSSpSr'
  @derivative(of: foo, wrt: (self, x, y))
  mutating func vjpFoo(_ x: Float, _ y: Double)
    -> (value: Void, pullback: (inout Float) -> (Float, Double)) {
    foo(x, y)
    return ((), { ($0, Double(2 * $0)) })
  }
}
