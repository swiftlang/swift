// RUN: %target-swift-frontend -emit-sil -verify %s

// https://github.com/swiftlang/swift/issues/75280
// Ensure we accumulate adjoints properly for inject_enum_addr instructions and
// handle `nil` case (no adjoint value to propagate)


import _Differentiation
@differentiable(reverse) func a<F, A>(_ f: Optional<F>, c: @differentiable(reverse) (F) -> A) -> Optional<A> where F: Differentiable, A: Differentiable
{
    guard let f else {return nil}
    return c(f)
}
