// REQUIRES: executable_test
// RUN: %target-run-simple-swift
// RUN: %target-swift-frontend -emit-sil -O -sil-verify-all -module-name adcs_same_case_source %s -o %t.sil
// RUN: %FileCheck %s < %t.sil

// CFG duplication can produce distinct tuples with identical SSA operands
// for the same predecessor case.

import _Differentiation
import StdlibUnittest

// CHECK-LABEL: {{^}}// specialized pullback of twoSelections(_:_:_:)
// CHECK:       bb0(%[[#]] : $Float, %[[#TAPE:]] : $[[ENUM:_AD__[^ )]*_spec[^ )]*]]):
// CHECK-NOT:     = apply {{.*}} : $@callee_guaranteed
// CHECK:         switch_enum %[[#TAPE]], case #[[ENUM]].
// CHECK-NOT:     = apply {{.*}} : $@callee_guaranteed
// CHECK:       } // end sil function
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

// The first ADCS visit specializes x * x. Before the second visit its
// capture tuple sinks into each branch and inlining exposes delayedSquare.
@differentiable(reverse)
public func squaredSelection(_ x: Float, _ first: Float?, _ second: Float?) -> Float {
  let square = x * x
  var intermediate = square
  if let value = first {
    if value < square { intermediate = value }
  }
  let shifted = delayedSquare(intermediate)
  if let value = second {
    if value < shifted { return value }
    return shifted
  }
  return shifted
}

@inline(never)
@differentiable(reverse)
public func delayedSquare(_ x: Float) -> Float { x * x }

@_semantics("inline_late")
@derivative(of: delayedSquare)
public func delayedSquareVJP(_ x: Float) -> (value: Float, pullback: (Float) -> Float) {
  (x * x, { seed in 2 * x * seed })
}

@inline(never)
@differentiable(reverse, wrt: x)
public func maybeSquare(_ x: Float, _ square: Bool) -> Float { square ? x * x : x }

@inline(__always)
@derivative(of: maybeSquare, wrt: x)
public func maybeSquareVJP(_ x: Float, _ square: Bool) -> (value: Float, pullback: (Float) -> Float) {
  if square { return (x * x, { seed in 2 * x * seed }) }
  return (x, { seed in seed })
}

@inline(__always)
func either(_ lhs: Bool, _ rhs: Bool) -> Bool { lhs || rhs }

// Both constructors save the delayed square alongside a path-specific callback.
// CHECK-LABEL: {{^}}// specialized pullback of mixedConstructors(_:_:_:)
// CHECK:         switch_enum %[[#]], case #{{_AD__[^.]+_spec[^.]+}}.
// CHECK:         = apply %[[#]](%[[#]]) : $@callee_guaranteed (Float) -> Float
// CHECK-NOT:     = apply {{.*}} : $@callee_guaranteed
// CHECK:       } // end sil function
@differentiable(reverse, wrt: x)
public func mixedConstructors(_ x: Float, _ negate: Bool, _ squareAgain: Bool) -> Float {
  let square = delayedSquare(x)
  let value = maybeSquare(square, squareAgain)
  if either(squareAgain, negate) {
    if squareAgain { return value }
    return -value
  }
  return 0
}

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
  for (first, second, value, dx, dFirst, dSecond): (Float?, Float?, Float, Float, Float, Float) in [
    (nil, nil, 81, 108, 0, 0),
    (4, nil, 16, 0, 8, 0),
    (12, 200, 81, 108, 0, 0),
    (nil, 2, 2, 0, 0, 1),
    (4, 6, 6, 0, 0, 1),
  ] {
    let result = valueWithPullback(at: Float(3), first, second, of: squaredSelection)
    expectEqual(value, result.value)
    for seed: Float in [1, -2] {
      let gradient = result.pullback(seed)
      expectEqual(seed * dx, gradient.0)
      expectEqual(seed * dFirst, gradient.1.value)
      expectEqual(seed * dSecond, gradient.2.value)
    }
  }
}
Tests.test("DistinctCallbacksInSameField") {
  for negate in [false, true] {
    for squareAgain in [false, true] {
      let result = valueWithPullback(at: Float(3), of: { mixedConstructors($0, negate, squareAgain) })
      expectEqual(squareAgain ? 81 : negate ? -9 : 0, result.value)
      for seed: Float in [1, -2] {
        expectEqual(seed * (squareAgain ? 108 : negate ? -6 : 0), result.pullback(seed))
      }
    }
  }
}
runAllTests()
