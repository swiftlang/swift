// {"kind":"typecheck","original":"9da0c2b1","signature":"(anonymous namespace)::ExprRewriter::coerceToType(swift::Expr*, swift::Type, swift::constraints::ConstraintLocatorBuilder)","signatureNext":"ExprRewriter::buildSingleCurryThunk"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b
  func c(b )
}
protocol d: a where b == ab
  func bridge<e: d>(f: e) {
    f.c
