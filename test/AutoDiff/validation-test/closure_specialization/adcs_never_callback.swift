// REQUIRES: executable_test
// RUN: %target-run-simple-swift
// RUN: %target-swift-frontend -emit-sil -O -sil-verify-all -module-name adcs_never_callback %s -o %t.sil
// RUN: %FileCheck %s < %t.sil

// CHECK-LABEL: // specialized pullback of branchGenericNever(_:_:)
// CHECK:       sil private @{{[^ ]*_spec[^ ]*}} :
// CHECK-NOT:     } // end sil function
// CHECK:       bb[[#]](%[[#PAYLOAD:]] : $(predecessor: {{[^,]+}}, (_: Float))):
// CHECK-NEXT:    %[[#CAPTURES:]] = tuple_extract %[[#PAYLOAD]], 1
// CHECK-NEXT:    %[[#CAPTURE:]] = tuple_extract %[[#CAPTURES]], 0
// CHECK:         %[[#X:]] = struct_extract %[[#CAPTURE]], #Float._value
// CHECK-NEXT:    %[[#]] = builtin "fmul_FPIEEE32"(%[[#]], %[[#X]])
// CHECK:       } // end sil function

import _Differentiation
import StdlibUnittest

// Instantiating the generic error type with Never leaves an indirect error
// result, so the nonthrowing pullback needs a reabstraction thunk.
@inline(__always)
public func genericPullback<E: Error>(_ x: Float, _: E.Type)
  -> (Float) throws(E) -> Float {
  { (seed: Float) throws(E) -> Float in 2 * x * seed }
}

@differentiable(reverse)
public func genericNeverSquare(_ x: Float) -> Float { x * x }

@derivative(of: genericNeverSquare)
@inline(__always)
public func vjpGenericNeverSquare(_ x: Float)
  -> (value: Float, pullback: (Float) -> Float) {
  (x * x, genericPullback(x, Never.self))
}

@differentiable(reverse, wrt: x)
public func branchGenericNever(_ x: Float, _ flag: Bool) -> Float {
  if flag { return genericNeverSquare(x) }
  return x + x
}

var tests = TestSuite("ADCSNeverCallback")
tests.test("NonthrowingAdapter") {
  for flag in [true, false] {
    let result = valueWithPullback(at: Float(3), of: {
      branchGenericNever($0, flag)
    })
    expectEqual(flag ? 9 : 6, result.value)
    for seed in [Float(1), 5, -2] {
      expectEqual((flag ? 6 : 2) * seed, result.pullback(seed))
    }
  }
}
runAllTests()
