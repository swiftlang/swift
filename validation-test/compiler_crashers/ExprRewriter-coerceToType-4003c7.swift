// {"kind":"typecheck","original":"004f6135","signature":"(anonymous namespace)::ExprRewriter::coerceToType(swift::Expr*, swift::Type, swift::constraints::ConstraintLocatorBuilder)","signatureNext":"ExprRewriter::coerceOptionalToOptional"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b
  func c -> b?
}
protocol d: a
  protocol e: d where f == b
    func g(h: e) {
      h.c
