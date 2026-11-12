// REQUIRES: executable_test
// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -Onone -o %t/none.out
// RUN: %target-build-swift %s -O -o %t/opt.out
// RUN: %target-run %t/none.out
// RUN: %target-run %t/opt.out

import _Differentiation

@differentiable(reverse)
@inline(never)
public func boxedSquare(_ x: Float) -> Float { x * x }

@derivative(of: boxedSquare)
@inline(__always)
public func boxedSquareVJP(_ x: Float) -> (value: Float, pullback: (Float) -> Float) {
  var factor = 2 * x
  return (x * x, { seed in
    // Keep a mutable capture while returning the correct derivative on every call.
    factor = 2 * x
    return factor * seed
  })
}

@differentiable(reverse, wrt: x)
public func branch(_ x: Float, _ flag: Bool) -> Float {
  if flag { return boxedSquare(x) }
  return x * x * x
}

#if !DEFINITIONS_ONLY
import StdlibUnittest

var tests = TestSuite("ADCSBoxedCapture")
tests.test("RepeatedPullbackApplications") {
  for x: Float in [-3, 2] {
    for flag in [false, true] {
      let result = valueWithPullback(at: x, of: { branch($0, flag) })
      expectEqual(flag ? x * x : x * x * x, result.value)
      let derivative = flag ? 2 * x : 3 * x * x
      for seed: Float in [1, 2, -1] {
        expectEqual(derivative * seed, result.pullback(seed))
      }
    }
  }
}
runAllTests()
#endif
