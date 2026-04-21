// {"kind":"typecheck","original":"0212eb4f","signature":"(anonymous namespace)::ExprRewriter::coerceToType(swift::Expr*, swift::Type, swift::constraints::ConstraintLocatorBuilder)","signatureNext":"ExprRewriter::coerceOptionalToOptional"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol b {
  associatedtype c where d == c
  func e() -> c?
}
func f(a: b) {
  a.e
}
