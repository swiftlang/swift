// {"kind":"typecheck","original":"0251a12c","signature":"(anonymous namespace)::ExprRewriter::coerceToType(swift::Expr*, swift::Type, swift::constraints::ConstraintLocatorBuilder)","signatureNext":"ExprRewriter::coerceCallArguments"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b
}
struct c: a
  func d<e: a>(e ,  [e.b])
    d(c (),  [
    4
