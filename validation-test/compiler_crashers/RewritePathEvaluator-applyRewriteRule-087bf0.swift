// {"kind":"typecheck","signature":"swift::rewriting::RewritePathEvaluator::applyRewriteRule(swift::rewriting::RewriteStep const&, swift::rewriting::RewriteSystem const&)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b : a where b.b == c
}
protocol d {
  associatedtype e : a
  associatedtype c : d
  struct f<e : a> : d {
    typealias c = f<e.b>
    protocol g : d where c == f<e> {
    }
  }
}
