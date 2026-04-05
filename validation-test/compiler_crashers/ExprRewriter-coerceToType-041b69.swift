// {"kind":"typecheck","original":"a8b912a3","signature":"(anonymous namespace)::ExprRewriter::coerceToType(swift::Expr*, swift::Type, swift::constraints::ConstraintLocatorBuilder)","signatureNext":"ExprRewriter::coerceTupleToTuple"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a<each b> {
  var c: (repeat each b)
  func d() -> (repeat each b?) {
    c
  }
}
