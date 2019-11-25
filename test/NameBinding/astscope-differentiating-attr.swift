// SWIFT_ENABLE_TENSORFLOW
// Check that ASTScope lookup works for `@derivative` attribute.

// NOTE(TF-835): This test is only necessary because `@derivative` attribute
// type-checking generates implicit `@differentiable` attributes on the
// referenced declaration. Robust lowering for `@derivative` attributes should
// make special logic regarding implicit `@differentiable` attributes
// unnecessary.

// RUN: %target-swift-frontend -typecheck %s -enable-astscope-lookup

struct Test<Element> {
  var element: Element
}
extension Test: Differentiable where Element: Differentiable {}
extension Test {
  static func +(lhs: Self, rhs: Self) -> Self {
    lhs
  }
  static func -(lhs: Self, rhs: Self) -> Self {
    lhs
  }
}

extension Test where Element : Differentiable {
  @derivative(of: +)
  internal static func _vjpAdd(lhs: Self, rhs: Self)
      -> (value: Self, pullback: (TangentVector) -> (TangentVector, TangentVector)) {
    return (lhs + rhs, { v in (v, v) })
  }

  @derivative(of: -)
  internal static func _vjpSubtract(lhs: Self, rhs: Self)
      -> (value: Self, pullback: (TangentVector) -> (TangentVector, TangentVector)) {
    return (lhs + rhs, { v in (v, v) })
  }
}
