// {"kind":"typecheck","original":"456e5f49","signature":"(anonymous namespace)::ExprRewriter::coerceToType(swift::Expr*, swift::Type, swift::constraints::ConstraintLocatorBuilder)","signatureNext":"ExprRewriter::visitUnresolvedMemberChainResultExpr"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b
}
struct c<d>: a {
  e: d
}
enum f<d {
  case g(d)
  struct h<i: a {
    j: f<i.b>
    let e: i
  }
  k = h(j: .g(0)
  e  :  c(e: 0.0
  let
