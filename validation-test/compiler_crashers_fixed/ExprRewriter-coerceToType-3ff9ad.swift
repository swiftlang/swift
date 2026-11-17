// {"kind":"typecheck","original":"272ca52c","signature":"(anonymous namespace)::ExprRewriter::coerceToType(swift::Expr*, swift::Type, swift::constraints::ConstraintLocatorBuilder)","signatureNext":"ExprRewriter::visitUnresolvedMemberChainResultExpr"}
// RUN: not %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b
}
enum c: Error {
  enum d<e: a> {
    case f(Result<e.b, c>)
    struct g: a {
      let h = d<g>.f(.success(0))
    }
  }
}
