// {"kind":"typecheck","original":"44bc1b5a","signature":"(anonymous namespace)::ExprRewriter::coerceToType(swift::Expr*, swift::Type, swift::constraints::ConstraintLocatorBuilder)","signatureNext":"ExprRewriter::coerceSelfArgumentToType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct c<d, e {
    f<g>(c      <e, g>)
  protocol h {
    associatedtype i
    associatedtype j
    o
    k: c<i, j>
  }
  func l<m: h, n: h>(a: m b: n)
  where
    m.j
      ==
  {
    a.k.f(b.k
