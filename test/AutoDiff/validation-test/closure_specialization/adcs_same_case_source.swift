// REQUIRES: executable_test
// RUN: %target-run-simple-swift
// RUN: %target-swift-frontend -emit-sil -O -D DEFINITIONS_ONLY -module-name adcs_same_case_source %s -o %t.sil
// RUN: %FileCheck %s < %t.sil

// CFG duplication creates distinct tuples for the same predecessor case while
// preserving every payload SSA operand. ADCS must recognize their identical
// ordered values and continue eliminating the taped pullback closures.

import _Differentiation

@differentiable(reverse)
public func twoSelections(_ x: Float, _ first: Float?, _ second: Float?) -> Float {
  let square = x * x
  let intermediate: Float
  if let value = first {
    if value < square { intermediate = value }
    else { intermediate = square }
  } else { intermediate = square }
  if let value = second {
    if value < intermediate { return value }
    return intermediate
  }
  return intermediate
}

// Mid-level optimization sinks the specialized capture tuple into each branch.
// The second ADCS visit sees distinct SSA tuples for the same bound tape case.
@differentiable(reverse)
public func squaredSelection(_ x: Float, _ first: Float?, _ second: Float?) -> Float {
  let square = x * x
  var intermediate = square
  if let value = first {
    if value < square { intermediate = value }
  }
  let shifted = intermediate * intermediate
  if let value = second {
    if value < shifted { return value }
    return shifted
  }
  return shifted
}

// CHECK-LABEL: // reverse-mode derivative of twoSelections
// CHECK-NOT: } // end sil function
// CHECK: // function_ref specialized pullback of twoSelections
// CHECK-NEXT: %[[PB:[0-9]+]] = function_ref
// CHECK-NEXT: %[[PULLBACK:[0-9]+]] = partial_apply {{.*}} %[[PB]]{{[<(]}}
// CHECK-NEXT: %[[RESULT:[0-9]+]] = tuple (%{{[0-9]+}}, %[[PULLBACK]])
// CHECK-NOT: } // end sil function
// CHECK: return %[[RESULT]]
// CHECK-NEXT: } // end sil function

// CHECK-LABEL: // specialized pullback of twoSelections
// CHECK-NOT: = partial_apply
// CHECK-NOT: apply {{.*}} : $@callee_guaranteed
// CHECK: } // end sil function

#if !DEFINITIONS_ONLY
import StdlibUnittest
var Tests = TestSuite("adcs_same_case_source")
Tests.test("DuplicatedPredecessorPayloads") {
  let cases: [(Float?, Float?, Float, Float)] = [
    (nil, nil, 9, 6),
    (4, nil, 4, 0),
    (12, 20, 9, 6),
    (nil, 2, 2, 0),
    (4, 6, 4, 0),
  ]
  for (first, second, expectedValue, expectedDerivative) in cases {
    let result = valueWithPullback(at: Float(3), of: {
      twoSelections($0, first, second)
    })
    expectEqual(expectedValue, result.value)
    expectEqual(expectedDerivative, result.pullback(1))
    expectEqual(2 * expectedDerivative, result.pullback(2))
  }
}
Tests.test("DistinctSpecializedCaptureTuples") {
  for (first, second, value, derivative): (Float?, Float?, Float, Float) in [
    (nil, nil, 81, 108),
    (4, nil, 16, 0),
    (12, 200, 81, 108),
    (nil, 2, 2, 0),
    (4, 6, 6, 0),
  ] {
    let result = valueWithPullback(at: Float(3), of: {
      squaredSelection($0, first, second)
    })
    expectEqual(value, result.value)
    expectEqual(derivative, result.pullback(1))
    expectEqual(2 * derivative, result.pullback(2))
  }
}
runAllTests()
#endif
