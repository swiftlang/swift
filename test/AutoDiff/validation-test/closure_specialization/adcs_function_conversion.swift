// REQUIRES: executable_test
// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -Onone -o %t/none.out
// RUN: %target-build-swift %s -O -o %t/opt.out
// RUN: %target-codesign %t/none.out
// RUN: %target-codesign %t/opt.out
// RUN: %target-run %t/none.out
// RUN: %target-run %t/opt.out

import _Differentiation

@differentiable(reverse, wrt: x)
@inline(never)
public func tagged(_ x: Float, _ label: String) -> Float { x * x }

// Dropping @Sendable produces convert_function between the known closure and
// its branch-tape payload. Use a closure expression so debug_value uses of a
// named local callback do not independently obstruct specialization.
@derivative(of: tagged, wrt: x)
@inline(__always)
public func vjpTagged(_ x: Float, _ label: String)
  -> (value: Float, pullback: (Float) -> Float) {
  (x * x, { @Sendable seed in
    withExtendedLifetime(label) { 2 * x * seed }
  })
}

@differentiable(reverse, wrt: x)
public func branchConverted(_ x: Float, _ label: String, _ flag: Bool) -> Float {
  if flag { return tagged(x, label) }
  return x + x
}

@differentiable(reverse, wrt: (x, y))
@inline(never)
public func taggedPair(_ x: Float, _ y: Float, _ label: String) -> Float {
  x * x + y * y
}

@derivative(of: taggedPair, wrt: (x, y))
@inline(__always)
public func vjpTaggedPair(_ x: Float, _ y: Float, _ label: String)
  -> (value: Float, pullback: (Float) -> (Float, Float)) {
  (x * x + y * y, { @Sendable seed in
    withExtendedLifetime(label) { (2 * x * seed, 2 * y * seed) }
  })
}

// The converted callback is captured by a subset-parameters thunk before
// reaching the payload. Conversion handling must compose with that adapter.
@differentiable(reverse, wrt: x)
public func branchSubset(_ x: Float, _ y: Float, _ label: String, _ flag: Bool) -> Float {
  if flag { return taggedPair(x, y, label) }
  return x + x
}

// An address-only callback also needs a generic reabstraction thunk. Its
// conversions must be recreated in the specialized pullback's environment.
@differentiable(reverse, wrt: x)
@inline(never)
public func genericTagged<T: BinaryFloatingPoint & Differentiable & Sendable>(
  _ x: T, _ label: String
) -> T where T.TangentVector == T {
  x + x
}

@derivative(of: genericTagged, wrt: x)
@inline(__always)
public func vjpGenericTagged<T: BinaryFloatingPoint & Differentiable & Sendable>(
  _ x: T, _ label: String
) -> (value: T, pullback: (T) -> T) where T.TangentVector == T {
  (x + x, { @Sendable seed in
    withExtendedLifetime(label) { seed + seed }
  })
}

@differentiable(reverse, wrt: x)
public func branchGeneric<T: BinaryFloatingPoint & Differentiable & Sendable>(
  _ x: T, _ label: String, _ flag: Bool
) -> T where T.TangentVector == T {
  if flag { return genericTagged(x, label) }
  return x + x
}

#if !DEFINITIONS_ONLY
let label = String(repeating: "a captured label ", count: 5)
for flag in [false, true] {
  for x: Float in [-3, 0, 3] {
    let expectedDerivative: Float = flag ? 2 * x : 2
    let direct = valueWithPullback(at: x, of: { branchConverted($0, label, flag) })
    precondition(direct.value == (flag ? x * x : 2 * x))
    precondition(direct.pullback(1) == expectedDerivative)
    precondition(direct.pullback(3) == 3 * expectedDerivative)

    let subset = valueWithPullback(at: x, of: { branchSubset($0, 4, label, flag) })
    precondition(subset.value == (flag ? x * x + 16 : 2 * x))
    precondition(subset.pullback(1) == expectedDerivative)
    precondition(subset.pullback(3) == 3 * expectedDerivative)

    let genericFloat = valueWithPullback(at: x, of: { branchGeneric($0, label, flag) })
    precondition(genericFloat.value == 2 * x)
    precondition(genericFloat.pullback(1) == 2)
    precondition(genericFloat.pullback(3) == 6)

    let genericDouble = valueWithPullback(at: Double(x), of: { branchGeneric($0, label, flag) })
    precondition(genericDouble.value == 2 * Double(x))
    precondition(genericDouble.pullback(1) == 2)
    precondition(genericDouble.pullback(3) == 6)
  }
}
#endif
