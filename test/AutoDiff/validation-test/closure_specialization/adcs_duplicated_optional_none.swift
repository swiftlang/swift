// REQUIRES: executable_test
// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -Onone -Xfrontend -sil-verify-all -o %t/none.out
// RUN: %target-build-swift %s -O -Xfrontend -sil-verify-all -o %t/opt.out
// RUN: %target-build-swift %s -Osize -Xfrontend -sil-verify-all -o %t/size.out
// RUN: %target-codesign %t/none.out
// RUN: %target-codesign %t/opt.out
// RUN: %target-codesign %t/size.out
// RUN: %target-run %t/none.out
// RUN: %target-run %t/opt.out
// RUN: %target-run %t/size.out

import _Differentiation

public enum Failure: Error { case first, second }

@differentiable(reverse, wrt: x)
public func square(_ x: Float, _ flag: Bool) throws -> Float {
  if x < 0 { throw Failure.first }
  if flag { throw Failure.second }
  return x * x
}

@derivative(of: square, wrt: x)
@inline(__always)
public func squareVJP(_ x: Float, _ flag: Bool) throws
  -> (value: Float, pullback: (Float) -> Float) {
  if x < 0 { throw Failure.first }
  if flag { throw Failure.second }
  return (x * x, { 2 * x * $0 })
}

// The first error path supplies an Optional.none value which dominates the
// second throwing call. CSE reuses it when jump threading duplicates the
// second error tape constructor before the second ADCS invocation. The
// duplicated constructors have the same case and saved SSA values.
@differentiable(reverse, wrt: (x, y))
@inline(never)
public func recover(_ x: Float, _ y: Float, _ a: Bool, _ b: Bool) -> Float {
  do { return try square(y, a) }
  catch {
    do { return try square(x, b) }
    catch { return x < 0 ? y : x }
  }
}

#if !DEFINITIONS_ONLY
for a in [false, true] {
  for b in [false, true] {
    for x: Float in [-2, 0, 2] {
      for y: Float in [-2, 0, 2] {
        let result = valueWithPullback(at: x, y, of: { recover($0, $1, a, b) })
        let expectedValue: Float
        let dx: Float
        let dy: Float
        if y >= 0 && !a {
          expectedValue = y * y
          dx = 0
          dy = 2 * y
        } else if x >= 0 && !b {
          expectedValue = x * x
          dx = 2 * x
          dy = 0
        } else if x < 0 {
          expectedValue = y
          dx = 0
          dy = 1
        } else {
          expectedValue = x
          dx = 1
          dy = 0
        }
        precondition(result.value == expectedValue)
        for seed: Float in [0, 1, -2, 1] {
          let gradient = result.pullback(seed)
          precondition(gradient.0 == dx * seed && gradient.1 == dy * seed)
        }
      }
    }
  }
}
#endif
