// REQUIRES: executable_test
// RUN: %target-run-simple-swift

import _Differentiation

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
  return 2
}

// Selecting the constant second result leaves a pullback that only destroys
// its tape. ADCS can still replace its saved optional callback with captures.
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

#if !DEFINITIONS_ONLY
import DifferentiationUnittest
import StdlibUnittest

var tests = TestSuite("ADCSOptionalCallback")
tests.testWithLeakChecking("NormalAndError") {
  let normal = valueWithPullback(at: Float(3), of: { recoveringSquare($0, true) })
  expectEqual(9, normal.value)
  expectEqual(12, normal.pullback(2))

  let error = valueWithPullback(at: Float(-3), of: { recoveringSquare($0, true) })
  expectEqual(-3, error.value)
  expectEqual(2, error.pullback(2))
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
#endif
