// REQUIRES: executable_test
// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -Onone -o %t/none.out
// RUN: %target-build-swift %s -O -o %t/opt.out
// RUN: %target-codesign %t/none.out
// RUN: %target-codesign %t/opt.out
// RUN: %target-run %t/none.out
// RUN: %target-run %t/opt.out

import _Differentiation

// The primal entity in a derivative's mangling can be an accessor or a closure,
// rather than a named function. These derivatives save multiplication pullbacks
// in branch-tracing enums that ADCS must associate with the correct primal.
public struct GetterCase: Differentiable {
  public var x: Float

  public init(_ x: Float) { self.x = x }

  @differentiable(reverse)
  @inline(never)
  public var value: Float {
    if x > 0 { return x * x }
    return x + x
  }
}

public let branchClosure: @differentiable(reverse) (Float) -> Float = { x in
  if x > 0 { return x * x }
  return x + x
}

// This primal's mangling contains its enclosing named function as well as
// the closure entity. The enclosing function must not be mistaken for it.
public func makeBranchClosure() -> @differentiable(reverse) (Float) -> Float {
  { x in
    if x > 0 { return x * x }
    return x + x
  }
}

// Keep both derivative bodies available when the SILOptimizer test compiles
// just the definitions from this file.
public func getterGradient(_ x: Float) -> Float {
  gradient(at: x, of: { GetterCase($0).value })
}

public func closureGradient(_ x: Float) -> Float {
  gradient(at: x, of: branchClosure)
}

#if !DEFINITIONS_ONLY
for (x, expectedValue, expectedDerivative): (Float, Float, Float) in [
  (-3, -6, 2), (0, 0, 2), (3, 9, 6)
] {
  let getter = valueWithPullback(at: GetterCase(x), of: { $0.value })
  precondition(getter.value == expectedValue)
  precondition(getter.pullback(1).x == expectedDerivative)
  precondition(getter.pullback(3).x == 3 * expectedDerivative)
  precondition(getterGradient(x) == expectedDerivative)

  let closure = valueWithPullback(at: x, of: branchClosure)
  precondition(closure.value == expectedValue)
  precondition(closure.pullback(1) == expectedDerivative)
  precondition(closure.pullback(3) == 3 * expectedDerivative)
  precondition(closureGradient(x) == expectedDerivative)

  let nestedClosure = valueWithPullback(at: x, of: makeBranchClosure())
  precondition(nestedClosure.value == expectedValue)
  precondition(nestedClosure.pullback(1) == expectedDerivative)
  precondition(nestedClosure.pullback(3) == 3 * expectedDerivative)
}
#endif
