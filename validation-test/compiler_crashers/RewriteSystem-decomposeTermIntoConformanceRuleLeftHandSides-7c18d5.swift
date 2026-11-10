// {"kind":"typecheck","original":"36e8e6bf","signature":"swift::rewriting::RewriteSystem::decomposeTermIntoConformanceRuleLeftHandSides(swift::rewriting::MutableTerm, llvm::SmallVectorImpl<unsigned int>&) const","signatureAssert":"Assertion failed: (rule.isAnyConformanceRule()), function decomposeTermIntoConformanceRuleLeftHandSides","signatureNext":"RewriteSystem::computeCandidateConformancePaths"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b : c
  protocol c {
    associatedtype d : a where d.b == Self
    struct e < f : a, g where f.b == g,
    g.d == f, f.b ==0.0
      extension e
