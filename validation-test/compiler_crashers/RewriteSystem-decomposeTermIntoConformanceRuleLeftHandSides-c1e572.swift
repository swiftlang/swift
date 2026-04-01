// {"kind":"typecheck","original":"52e69db6","signature":"swift::rewriting::RewriteSystem::decomposeTermIntoConformanceRuleLeftHandSides(swift::rewriting::MutableTerm, llvm::SmallVectorImpl<unsigned int>&) const","signatureNext":"RewriteSystem::computeCandidateConformancePaths"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol
  a
{
  associatedtype b
  associatedtype c: d where c.e == b, c.e.f.c == c
}
protocol
  g
{
  associatedtype f: a where f.b == h
}
protocol d {
  associatedtype e: g
  protocol i: a {
  }
}
