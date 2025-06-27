// {"signature":"swift::rewriting::RewriteSystem::buildRewritePathForJoiningTerms(swift::rewriting::MutableTerm, swift::rewriting::MutableTerm, swift::rewriting::RewritePath*) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a : b
  protocol c {
    associatedtype d
    associatedtype e: f where e.g == Self
  }
  protocol f: a
    protocol b {
      associatedtype g: c where g.d == Self!
