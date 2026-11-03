// {"extraArgs":["-solver-disable-diagnose-valid-salvage"],"kind":"typecheck","original":"1265a8d3","signature":"(anonymous namespace)::ExprRewriter::coerceToType(swift::Expr*, swift::Type, swift::constraints::ConstraintLocatorBuilder)","signatureNext":"ExprWalker::rewriteTarget"}
// RUN: not --crash %target-swift-frontend -typecheck -solver-disable-diagnose-valid-salvage %s
class a<b {
  init(_: b
}
protocol c {
  associatedtype k
}
actor d<e
  struct f: c
    typealias g = [String]
    func h<i: c>(l: i m: () -> a<i.k>, n: d<g>, o: Int
    let l = f(
    let p = d<[String]>(
    let j = h(
      l: l,
      m: {
        a(j
      }
      n: p, o: 0
