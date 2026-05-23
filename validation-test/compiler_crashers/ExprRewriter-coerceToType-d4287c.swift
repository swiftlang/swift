// {"kind":"typecheck","original":"6579aa42","signature":"(anonymous namespace)::ExprRewriter::coerceToType(swift::Expr*, swift::Type, swift::constraints::ConstraintLocatorBuilder)","signatureNext":"buildOpaqueElementConversion"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b
  func c( Collection<b>)
}
func d<e: a>(f: e)
where
  e.b
    ==
{
  f.c
