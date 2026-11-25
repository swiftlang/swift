// REQUIRES: executable_test
// RUN: %target-run-simple-swift
// RUN: %target-run-simple-swift(-O)
// RUN: %target-swift-frontend -emit-sil -O -sil-verify-all -module-name src %s -o %t.sil
// RUN: %FileCheck %s < %t.sil

import _Differentiation
import DifferentiationUnittest
import StdlibUnittest

var tests = TestSuite("PayloadFlow")

// MARK: - Projected and whole payloads

// Aggregate unwrapping moves tuple projections across branches before ADCS.
// Mid-level optimization then folds trivial capture tuples before ADCS runs again.
// CHECK-LABEL: {{^}}// specialized pullback of branching(_:_:)
// CHECK:       bb[[#]](%[[#]] : $(predecessor: _AD__$s3src9branchingyS2f_SbtF_bb2__Pred__src_0_wrt_0_spec_bb0_0_a00cb06f542e6ceabd1635d284a6ded1, (Float, Float))):
// CHECK-NOT:     = apply {{.*}} : $@callee_guaranteed
// CHECK:       } // end sil function
@inline(never)
@differentiable(reverse, wrt: x)
public func branching(_ x: Float, _ flag: Bool) -> Float {
  var result = x * x
  if flag {
    result = result + result
    result = result - x
  } else {
    result = result / x
  }
  return result
}

@inline(never)
@differentiable(reverse)
public func nested(_ x: Float, _ y: Float) -> Float {
  if x > 0 {
    if y > 10 {
      let z = x * y
      if z > 100 { return x + z }
      if y == 20 { return z + z }
    } else {
      return x + y
    }
  }
  return -y
}

@inline(never)
@differentiable(reverse, wrt: x)
public func generic<T>(_ x: Float, _ metatype: T.Type) -> Float {
  if metatype is Int.Type { return x + x }
  return x * x
}

// The inline_late derivative leaves the projected callback opaque until the
// mid-level inliner exposes it for the second ADCS run.
@inline(never)
@differentiable(reverse)
public func delayedSquare(_ x: Float) -> Float { x * x }

@_semantics("inline_late")
@derivative(of: delayedSquare)
public func delayedSquareVJP(_ x: Float) -> (value: Float, pullback: (Float) -> Float) {
  (x * x, { seed in 2 * x * seed })
}

// CHECK-LABEL: {{^}}// specialized pullback of lateBranch(_:_:)
// CHECK:         switch_enum %[[#]], case #_AD__$s3src10lateBranchyS2f_SbtF_bb3__Pred__src_0_wrt_0_spec_fc975e52b38dd96c021ce9ace14ae9fe.
// CHECK-NOT:     = apply {{.*}} : $@callee_guaranteed
// CHECK:       } // end sil function
@inline(never)
@differentiable(reverse, wrt: x)
public func lateBranch(_ x: Float, _ flag: Bool) -> Float {
  let result = delayedSquare(x)
  if flag { return result }
  return x
}

// The identity/constant branch keeps a trivial predecessor payload live
// through the second ADCS run.
@inline(never)
@differentiable(reverse, wrt: x)
public func lateIdentity(_ x: Float, _ first: Bool, _ second: Bool) -> Float {
  let result: Float
  if first {
    if second { result = x } else { result = 3 }
  } else {
    result = delayedSquare(x)
  }
  return result
}

// CHECK-LABEL: {{^}}// specialized pullback of mixedCallbacks(_:_:_:)
// CHECK:       bb[[#]](%[[#]] : $(predecessor: _AD__$s3src14mixedCallbacksyS2f_SbS2fYjrXEtF_bb2__Pred__src_0_wrt_0, (Float, Float))):
// CHECK:       bb[[#]](%[[#]] : $(predecessor: _AD__$s3src14mixedCallbacksyS2f_SbS2fYjrXEtF_bb1__Pred__src_0_wrt_0, @callee_guaranteed (Float) -> Float)):
// CHECK:         = apply %[[#]](%[[#]]) : $@callee_guaranteed (Float) -> Float
// CHECK:       } // end sil function
@inline(never)
@differentiable(reverse, wrt: x)
public func mixedCallbacks(
  _ x: Float, _ useTransform: Bool,
  _ transform: @differentiable(reverse) (Float) -> Float
) -> Float {
  if useTransform { return transform(x) }
  return x * x
}

public func branchingGradient(_ x: Float, _ flag: Bool) -> Float {
  gradient(at: x, of: { branching($0, flag) })
}
public func nestedGradient(_ x: Float, _ y: Float) -> (Float, Float) {
  gradient(at: x, y, of: nested)
}
public func genericGradient<T>(_ x: Float, _ metatype: T.Type) -> Float {
  gradient(at: x, of: { generic($0, metatype) })
}

public func lateGradient(_ x: Float, _ flag: Bool) -> Float {
  gradient(at: x, of: { lateBranch($0, flag) })
}

public func lateIdentityGradient(_ x: Float, _ first: Bool, _ second: Bool) -> Float {
  gradient(at: x, of: { lateIdentity($0, first, second) })
}

tests.testWithLeakChecking("ProjectedAndWholePayloads") {
  for x: Float in [2, 4] {
    for flag in [false, true] {
      let result = valueWithPullback(at: x, of: { branching($0, flag) })
      let value = flag ? 2 * x * x - x : x
      let slope = flag ? 4 * x - 1 : 1
      expectEqual(value, result.value)
      expectEqual(slope, result.pullback(1))
      expectEqual(2 * slope, result.pullback(2))
      expectEqual(slope, branchingGradient(x, flag))
      let late = valueWithPullback(at: x, of: { lateBranch($0, flag) })
      let lateSlope = flag ? 2 * x : 1
      expectEqual(flag ? x * x : x, late.value)
      expectEqual(lateSlope, late.pullback(1))
      expectEqual(2 * lateSlope, late.pullback(2))
      expectEqual(lateSlope, lateGradient(x, flag))
    }
  }
  expectEqual((40, 8), nestedGradient(4, 20))
  expectEqual((0, -1), nestedGradient(4, 21))
  expectEqual((1, 1), nestedGradient(4, 5))
  expectEqual((0, -1), nestedGradient(-3, -2))
  expectEqual(2, genericGradient(3, Int.self))
  expectEqual(6, genericGradient(3, Float.self))
}
tests.testWithLeakChecking("MixedCallbacks") {
  let transform: @differentiable(reverse) (Float) -> Float = { 3 * $0 }
  for useTransform in [false, true] {
    let result = valueWithPullback(at: Float(4), of: {
      mixedCallbacks($0, useTransform, transform)
    })
    expectEqual(useTransform ? 12 : 16, result.value)
    expectEqual(useTransform ? 3 : 8, result.pullback(1))
    expectEqual(useTransform ? 6 : 16, result.pullback(2))
  }
}
tests.testWithLeakChecking("LateWholeTupleExtract") {
  for first in [false, true] {
    for second in [false, true] {
      let result = valueWithPullback(at: Float(4), of: {
        lateIdentity($0, first, second)
      })
      let slope: Float = first ? (second ? 1 : 0) : 8
      expectEqual(first ? (second ? 4 : 3) : 16, result.value)
      expectEqual(slope, result.pullback(1))
      expectEqual(2 * slope, result.pullback(2))
      expectEqual(slope, lateIdentityGradient(4, first, second))
    }
  }
}

// CFG duplication can produce distinct tuples with identical SSA operands
// for the same predecessor case.

// MARK: - Predecessors that construct the same case

// CHECK-LABEL: {{^}}// specialized pullback of twoSelections(_:_:_:)
// CHECK:       bb0(%[[#]] : $Float, %[[#TAPE:]] : $_AD__$s3src13twoSelectionsyS2f_SfSgACtF_bb11__Pred__src_0_wrt_0_1_2_spec_9025df53bf21adc964c2bfd1cc4e1c85):
// CHECK-NOT:     = apply {{.*}} : $@callee_guaranteed
// CHECK:         switch_enum %[[#TAPE]], case #_AD__$s3src13twoSelectionsyS2f_SfSgACtF_bb11__Pred__src_0_wrt_0_1_2_spec_9025df53bf21adc964c2bfd1cc4e1c85.
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
// CHECK:         switch_enum %[[#]], case #_AD__$s3src17mixedConstructorsyS2f_S2btF_bb5__Pred__src_0_wrt_0_spec_bb3_1_20144e46edf0a1bb04b212e3e157bb23.
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

tests.test("DuplicatedPredecessorPayloads") {
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
tests.test("DistinctSpecializedCaptureTuples") {
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
tests.test("DistinctCallbacksInSameField") {
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

// MARK: - Merged predecessor payloads

// The pullback merges payloads from different predecessor enums for the
// shared addition and subtraction.
@differentiable(reverse)
public func mergedPayload(_ x: Float, _ y: Float) -> Float {
  let sum = x + y
  let difference = x - y
  let selected: Float
  if x < y {
    selected = sum + y
  } else {
    selected = difference - y
  }
  let shifted = selected + x
  return x > 0 ? shifted * x : shifted + y
}

func checkMergedPayload(_ x: Float, _ y: Float, _ value: Float, _ dx: Float, _ dy: Float) {
  let result = valueWithGradient(at: x, y, of: mergedPayload)
  precondition(result.value == value)
  precondition(result.gradient.0 == dx)
  precondition(result.gradient.1 == dy)
}

tests.test("MergedPredecessorPayloads") {
  checkMergedPayload(1, 2, 6, 8, 2)
  checkMergedPayload(3, 2, 6, 8, -6)
  checkMergedPayload(-1, 2, 4, 2, 3)
  checkMergedPayload(-1, -2, 0, 2, -1)
}

runAllTests()
