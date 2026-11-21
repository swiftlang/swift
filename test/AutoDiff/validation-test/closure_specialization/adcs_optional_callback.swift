// REQUIRES: executable_test
// RUN: %target-run-simple-swift
// RUN: %target-swift-frontend -emit-sil -O -sil-verify-all -module-name adcs_optional_callback %s -o %t.sil
// RUN: %FileCheck %s < %t.sil

// CHECK-LABEL: enum _AD__{{[^ ]*tupleRecovery[^ ]*_spec_bb1_1_[^ ]*}} {
// CHECK-NEXT:    case bb1((predecessor: {{[^,]+}}, (_: Float)?))

// CHECK-LABEL: // specialized pullback of recoveringSquare(_:_:)
// CHECK:       bb[[#]](%[[#]] : $Float, %[[#SAVED:]] : $Optional<@callee_guaranteed (Float) -> Float>):
// CHECK-NEXT:    switch_enum %[[#SAVED]], case #Optional.some!enumelt: bb[[#SOME:]], case #Optional.none!enumelt:
// CHECK:       bb[[#SOME]](%[[#CALLBACK:]] : $@callee_guaranteed (Float) -> Float):
// CHECK-NEXT:    %[[#]] = apply %[[#CALLBACK]](%[[#]]) : $@callee_guaranteed (Float) -> Float
// CHECK-LABEL: // specialized pullback of tupleRecovery(_:_:_:)
// CHECK:       sil private {{.*}} : $@convention(thin) (Float, Float) -> (Float, Float) {
// CHECK-NOT:     release_value
// CHECK:       } // end sil function

import _Differentiation
import DifferentiationUnittest
import StdlibUnittest

public enum Negative: Error {
  case input
}

@differentiable(reverse)
public func positiveSquare(_ x: Float) throws -> Float {
  if x < 0 { throw Negative.input }
  return x * x
}

@derivative(of: positiveSquare)
@inline(__always)
public func positiveSquareVJP(_ x: Float) throws -> (value: Float, pullback: (Float) -> Float) {
  if x < 0 { throw Negative.input }
  return (x * x, { 2 * x * $0 })
}

@differentiable(reverse, wrt: x)
public func recoveringSquare(_ x: Float, _ flag: Bool) -> Float {
  if flag {
    do { return try positiveSquare(x) }
    catch { return x }
  }
  return x * x
}

// Selecting the constant second result leaves a pullback whose only use of
// the branch tracing enum is to destroy it.
@differentiable(reverse, wrt: (x, y))
@inline(never)
public func tupleRecovery(_ x: Float, _ y: Float, _ flag: Bool) -> (Float, Float) {
  if flag {
    do { return (try positiveSquare(y), 1) }
    catch { return (x, 1) }
  }
  return (y * y, 6)
}

@differentiable(reverse, wrt: (x, y))
@inline(never)
public func recoverySecond(_ x: Float, _ y: Float, _ flag: Bool) -> Float {
  tupleRecovery(x, y, flag).1
}

var tests = TestSuite("ADCSOptionalCallback")
tests.testWithLeakChecking("NormalAndError") {
  let normal = valueWithPullback(at: Float(3), of: { recoveringSquare($0, true) })
  expectEqual(9, normal.value)
  expectEqual(12, normal.pullback(2))

  let error = valueWithPullback(at: Float(-3), of: { recoveringSquare($0, true) })
  expectEqual(-3, error.value)
  expectEqual(2, error.pullback(2))

  let fallback = valueWithPullback(at: Float(-3), of: { recoveringSquare($0, false) })
  expectEqual(9, fallback.value)
  expectEqual(-12, fallback.pullback(2))
}
tests.testWithLeakChecking("OptionalCaptureWithUnusedTape") {
  for (y, flag, first, second): (Float, Bool, Float, Float) in [
    (2, true, 4, 1), (-2, true, 3, 1), (2, false, 4, 6)
  ] {
    let pair = tupleRecovery(3, y, flag)
    expectEqual((first, second), pair)
    let result = valueWithPullback(at: Float(3), y, of: {
      recoverySecond($0, $1, flag)
    })
    expectEqual(second, result.value)
    expectEqual((0, 0), result.pullback(1))
    expectEqual((0, 0), result.pullback(2))
  }
}
runAllTests()
