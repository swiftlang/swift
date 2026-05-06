// {"kind":"typecheck","original":"7b7da4dc","signature":"swift::rewriting::RewriteSystem::verifyMinimizedRules(llvm::DenseSet<unsigned int, llvm::DenseMapInfo<unsigned int, void>> const&) const","signatureNext":"RewriteSystem::minimizeRewriteSystem"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol
  a
{
  associatedtype b: a
  associatedtype c: d where c.e == Self
}
protocol d {
  associatedtype e: a
}
struct f<g: d>: a where g.e.b == f {
}
