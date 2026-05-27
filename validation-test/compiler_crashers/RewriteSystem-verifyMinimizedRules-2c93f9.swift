// {"kind":"typecheck","original":"2fa033ee","signature":"swift::rewriting::RewriteSystem::verifyMinimizedRules(llvm::DenseSet<unsigned int, llvm::DenseMapInfo<unsigned int, void>> const&) const","signatureNext":"RewriteSystem::minimizeRewriteSystem"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol e {
  associatedtype a
  associatedtype d: e where d.a == Self
}
struct f<b, c>: e
where
  c: e,
  b.a == f,
  b.d == c,
  c.a == b,
  c.d == f
{
}
