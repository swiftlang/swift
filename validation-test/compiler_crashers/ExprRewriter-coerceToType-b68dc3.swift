// {"kind":"typecheck","original":"22afa209","signature":"(anonymous namespace)::ExprRewriter::coerceToType(swift::Expr*, swift::Type, swift::constraints::ConstraintLocatorBuilder)","signatureNext":"ExprRewriter::visitAssignExpr"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b
}
struct c: a
  struct d<e {
    g: e
  }
  extension d: a where e: a {
    typealias b = d<e.b>
  }
  func h<e: a>(e ) -> e.b
  let f = c(
  let i j  _ = h(d(g: f
