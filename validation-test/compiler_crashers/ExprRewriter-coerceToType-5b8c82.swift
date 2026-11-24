// {"kind":"typecheck","original":"3f0747f7","signature":"(anonymous namespace)::ExprRewriter::coerceToType(swift::Expr*, swift::Type, swift::constraints::ConstraintLocatorBuilder)","signatureNext":"ExprRewriter::coerceTupleToTuple"}
// RUN: not --crash %target-swift-frontend -typecheck %s
class c < each d {
  e : (repeat each d) {
    func f<each h, each g>(a: (repeat each h) b: (repeat Optional<each g>))
    f(a: e
    b: e
