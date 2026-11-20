// REQUIRES: executable_test
// RUN: %target-run-simple-swift(-Onone)
// RUN: %target-run-simple-swift(-O)
// RUN: %target-swift-frontend -emit-sil -O -sil-verify-all -module-name src %s -o %t.sil
// RUN: %FileCheck %s < %t.sil

import _Differentiation

// The getter's derivative names its branch tracing enum after the accessor.
public struct GetterCase: Differentiable {
  public var x: Float

  public init(_ x: Float) { self.x = x }

  @differentiable(reverse)
  @inline(never)
  public var value: Float {
    if x > 0 { return x * x }
    return x + x
  }
}

public let branchClosure: @differentiable(reverse) (Float) -> Float = { x in
  if x > 0 { return x * x }
  return x + x
}

// The closure's mangling also contains the name of its enclosing function.
public func makeBranchClosure() -> @differentiable(reverse) (Float) -> Float {
  { x in
    if x > 0 { return x * x }
    return x + x
  }
}

// CHECK-LABEL: // reverse-mode derivative of GetterCase.value.getter
// CHECK-NOT:     = partial_apply
// CHECK:         // function_ref specialized pullback of GetterCase.value.getter
// CHECK-NEXT:    %[[#GETTER_FN:]] = function_ref
// CHECK-NEXT:    %[[#GETTER_PB:]] = partial_apply [callee_guaranteed] %[[#GETTER_FN]](%[[#]]) : $@convention(thin) (Float, @owned {{_AD__[^ ]+GetterCaseV5valueSfvg[^ ]+_spec[^ )]+}}) -> GetterCase.TangentVector
// CHECK-NEXT:    %[[#GETTER_RESULT:]] = tuple (%[[#]], %[[#GETTER_PB]])
// CHECK-NEXT:    return %[[#GETTER_RESULT]]
// CHECK-NEXT:  } // end sil function

// CHECK-LABEL: // reverse-mode derivative of closure #1 in makeBranchClosure()
// CHECK-NOT:     = partial_apply
// CHECK:         // function_ref specialized pullback of closure #1 in makeBranchClosure()
// CHECK-NEXT:    %[[#CLOSURE_FN:]] = function_ref
// CHECK-NEXT:    %[[#CLOSURE_PB:]] = partial_apply [callee_guaranteed] %[[#CLOSURE_FN]](%[[#]]) : $@convention(thin) (Float, @owned {{_AD__[^ ]+makeBranchClosure[^ ]+cfU__bb[^ ]+_spec[^ )]+}}) -> Float
// CHECK-NEXT:    %[[#CLOSURE_RESULT:]] = tuple (%[[#]], %[[#CLOSURE_PB]])
// CHECK-NEXT:    return %[[#CLOSURE_RESULT]]
// CHECK-NEXT:  } // end sil function

// The global initializer's closure has no enclosing named function.
// CHECK-LABEL: sil private @$s3srcS2fcfU_TJrSpSr :
// CHECK-NOT:     = partial_apply
// CHECK:         // function_ref specialized pullback of closure #1 in
// CHECK-NEXT:    %[[#GLOBAL_FN:]] = function_ref
// CHECK-NEXT:    %[[#GLOBAL_PB:]] = partial_apply [callee_guaranteed] %[[#GLOBAL_FN]](%[[#]]) : $@convention(thin) (Float, @owned {{_AD__[^ ]+S2fcfU__bb[^ ]+_spec[^ )]+}}) -> Float
// CHECK-NEXT:    %[[#GLOBAL_RESULT:]] = tuple (%[[#]], %[[#GLOBAL_PB]])
// CHECK-NEXT:    return %[[#GLOBAL_RESULT]]
// CHECK-NEXT:  } // end sil function

public func getterGradient(_ x: Float) -> Float {
  gradient(at: x, of: { GetterCase($0).value })
}

public func closureGradient(_ x: Float) -> Float {
  gradient(at: x, of: branchClosure)
}

for (x, expectedValue, expectedDerivative): (Float, Float, Float) in [
  (-3, -6, 2), (0, 0, 2), (3, 9, 6)
] {
  let getter = valueWithPullback(at: GetterCase(x), of: { $0.value })
  precondition(getter.value == expectedValue)
  precondition(getter.pullback(1).x == expectedDerivative)
  precondition(getter.pullback(3).x == 3 * expectedDerivative)
  precondition(getterGradient(x) == expectedDerivative)

  let closure = valueWithPullback(at: x, of: branchClosure)
  precondition(closure.value == expectedValue)
  precondition(closure.pullback(1) == expectedDerivative)
  precondition(closure.pullback(3) == 3 * expectedDerivative)
  precondition(closureGradient(x) == expectedDerivative)

  let nestedClosure = valueWithPullback(at: x, of: makeBranchClosure())
  precondition(nestedClosure.value == expectedValue)
  precondition(nestedClosure.pullback(1) == expectedDerivative)
  precondition(nestedClosure.pullback(3) == 3 * expectedDerivative)
}
