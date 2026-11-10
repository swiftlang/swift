// {"kind":"typecheck","original":"ffecb26f","signature":"swift::rewriting::RewriteSystem::verifyRedundantConformances(llvm::DenseSet<unsigned int, llvm::DenseMapInfo<unsigned int, void>> const&) const","signatureNext":"RewriteSystem::minimizeRewriteSystem"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype Collapsed
}
protocol b : a {
  associatedtype c : b where c.Collapsed == Collapsed
  protocol d : b
    struct e
    : b {
      typealias Collapsed = e
      typealias c = e
      func f < g : d where g.c == e
