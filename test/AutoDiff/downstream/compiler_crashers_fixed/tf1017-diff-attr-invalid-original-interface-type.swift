// RUN: %target-swift-frontend -typecheck %s -verify
// REQUIRES: asserts

// TF-1017: `@differentiable` attribute type-checking crash when original
// declaration has an error type.

protocol P: Differentiable {
  @differentiable
  init(x: Float)
}
extension P {
  @differentiable
  // expected-error @+1 {{generic parameter 'U' is not used in function signature}}
  init<U>(_ x: Float) {
    self.init(x: x)
  }
}
extension P where Self: FloatingPoint {
  @differentiable
  func hello(_ x: Float) -> Self {
    .init(x: x)
  }
}

// Assertion failed: (isa<X>(Val) && "cast<Ty>() argument of incompatible type!"), function cast, file llvm/include/llvm/Support/Casting.h, line 264.
// Assertion failed: (D), function printDifferentiableAttrArguments, file swift/lib/AST/Attr.cpp, line 493.
