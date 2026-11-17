// {"extraArgs":["-solver-disable-diagnose-valid-salvage"],"kind":"typecheck","original":"5d99a920","signature":"(anonymous namespace)::ExprRewriter::coerceToType(swift::Expr*, swift::Type, swift::constraints::ConstraintLocatorBuilder)","signatureNext":"ExprRewriter::visitUnresolvedMemberChainResultExpr"}
// RUN: not --crash %target-swift-frontend -typecheck -solver-disable-diagnose-valid-salvage %s
protocol a {
  associatedtype b
}
struct c: a {
  enum d<e, f> {
    case g(f)
    struct h<e, f: a> {
      var i: d<e, f.b>
    }
    func
      j<k>() -> h<k, c>
    {
      h(i: .g(0))
    }
  }
}
