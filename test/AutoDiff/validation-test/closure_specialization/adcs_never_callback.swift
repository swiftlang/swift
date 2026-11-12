// REQUIRES: executable_test
// RUN: %target-run-simple-swift

import _Differentiation

// Instantiating the generic error type with Never leaves an indirect error
// result on the root closure. A nonthrowing reabstraction thunk adapts it to
// the pullback's function type.
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

#if !DEFINITIONS_ONLY
import StdlibUnittest

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
#endif
