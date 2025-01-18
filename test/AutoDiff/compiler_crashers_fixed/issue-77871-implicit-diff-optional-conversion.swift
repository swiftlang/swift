// RUN: %target-swift-frontend -emit-sil -verify %s

// https://github.com/swiftlang/swift/issues/77871
// Ensure we are correctl generating reabstraction thunks for Double <-> Optional<Double>
// conversion for derivatives: for differential and pullback we need
// to emit thunks to convert T.TangentVector <-> Optional<T>.TangentVector.

import _Differentiation

@differentiable(reverse)
func testFunc(_ x: Double?) -> Double? {
    x! * x! * x!
}
print(pullback(at: 1.0, of: testFunc)(.init(1.0)) == 3.0)

func foo<T>(_ fn: @escaping @differentiable(reverse) (T?) -> Double) {
  let _: @differentiable(reverse) (T) -> Double = fn
}
