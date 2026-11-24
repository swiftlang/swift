// {"kind":"typecheck","original":"c35f6b6c","signature":"(anonymous namespace)::ExprRewriter::coerceToType(swift::Expr*, swift::Type, swift::constraints::ConstraintLocatorBuilder)","signatureNext":"ExprRewriter::coerceTupleToTuple"}
// RUN: not %target-swift-frontend -typecheck %s
func a<each b>(builder: () -> (repeat each b)) -> (repeat each b?) {
  builder(
