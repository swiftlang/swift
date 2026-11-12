// REQUIRES: executable_test
// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -Onone -o %t/none.out
// RUN: %target-build-swift %s -O -o %t/opt.out
// RUN: %target-build-swift %s -Osize -o %t/size.out
// RUN: %target-codesign %t/none.out
// RUN: %target-codesign %t/opt.out
// RUN: %target-codesign %t/size.out
// RUN: %target-run %t/none.out
// RUN: %target-run %t/opt.out
// RUN: %target-run %t/size.out

import _Differentiation

// Specialization and early inlining put the Int branch tape inside the String
// and Double VJPs as a nested pullback capture. ADCS must keep that tape valid
// while specializing the outer predecessor graph.
@differentiable(reverse, wrt: x)
@inline(never)
public func nestedTape<T>(_ x: Float, _ type: T.Type, _ flag: Bool) -> Float {
  if T.self == Int.self { return x + x }
  if flag { return nestedTape(x * x, Int.self, false) }
  return x * x
}

// These concrete wrappers also instantiate the VJPs when the paired SIL test
// compiles this source without its runtime checks.
@differentiable(reverse, wrt: x)
@inline(never)
public func stringNestedTape(_ x: Float, _ flag: Bool) -> Float {
  nestedTape(x, String.self, flag)
}

@differentiable(reverse, wrt: x)
@inline(never)
public func doubleNestedTape(_ x: Float, _ flag: Bool) -> Float {
  nestedTape(x, Double.self, flag)
}

#if !DEFINITIONS_ONLY
for flag in [false, true] {
  for x: Float in [-3, 0, 3] {
    let stringResult = valueWithPullback(at: x, of: { stringNestedTape($0, flag) })
    let doubleResult = valueWithPullback(at: x, of: { doubleNestedTape($0, flag) })
    let intResult = valueWithPullback(at: x, of: { nestedTape($0, Int.self, flag) })
    let scale: Float = flag ? 2 : 1
    let expectedValue = scale * x * x
    let expectedSlope = scale * 2 * x
    precondition(stringResult.value == expectedValue)
    precondition(doubleResult.value == expectedValue)
    precondition(intResult.value == 2 * x)

    // Reuse each pullback, including after a zero and a negative seed.
    for seed: Float in [0, 1, -2, 1] {
      precondition(stringResult.pullback(seed) == expectedSlope * seed)
      precondition(doubleResult.pullback(seed) == expectedSlope * seed)
      precondition(intResult.pullback(seed) == 2 * seed)
    }
  }
}
#endif
