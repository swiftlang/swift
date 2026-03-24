// {"kind":"typecheck","original":"36e8e6bf","signature":"swift::rewriting::RewriteSystem::decomposeTermIntoConformanceRuleLeftHandSides(swift::rewriting::MutableTerm, llvm::SmallVectorImpl<unsigned int>&) const","signatureAssert":"Assertion failed: (rule.isAnyConformanceRule()), function decomposeTermIntoConformanceRuleLeftHandSides","signatureNext":"RewriteSystem::computeCandidateConformancePaths"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b : c
  protocol c {
    associatedtype d : a where d.b == Self
    struct e < f : a, g where f.b == g,
    g.d == f, f.b ==0.0
      extension e
