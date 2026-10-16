// {"kind":"typecheck","original":"62bca45e","signature":"(anonymous namespace)::ExprRewriter::coerceToType(swift::Expr*, swift::Type, swift::constraints::ConstraintLocatorBuilder)","signatureNext":"ExprRewriter::coerceCallArguments"}
// RUN: not --crash %target-swift-frontend -typecheck -solver-disable-diagnose-valid-salvage %s
protocol a {
  associatedtype b
}
struct c<d, e>: a {
  f: d -> e
  func g<h: a>(m: [h.b] j: h
  func k(n: [Int]) {
    let l = c { i in
      0
      ""
    }
    g(
      m: n.map {
        $0
      }
    j
    : l
