// {"kind":"typecheck","original":"3ccf2663","signature":"(anonymous namespace)::ExprRewriter::coerceToType(swift::Expr*, swift::Type, swift::constraints::ConstraintLocatorBuilder)","signatureNext":"ExprRewriter::closeExistentials"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct b<each c
  protocol d {
    associatedtype c
    func e -> b<c>
  }
  func f(a: d) {
    a.e(
