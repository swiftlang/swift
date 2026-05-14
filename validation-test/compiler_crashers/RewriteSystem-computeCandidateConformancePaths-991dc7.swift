// {"kind":"typecheck","original":"e45c86c4","signature":"swift::rewriting::RewriteSystem::computeCandidateConformancePaths(swift::rewriting::PropertyMap const&, llvm::MapVector<unsigned int, std::__1::vector<llvm::SmallVector<unsigned int, 2u>, std::__1::allocator<llvm::SmallVector<unsigned int, 2u>>>, llvm::DenseMap<unsigned int, unsigned int, llvm::DenseMapInfo<unsigned int, void>, llvm::detail::DenseMapPair<unsigned int, unsigned int>>, llvm::SmallVector<std::__1::pair<unsigned int, std::__1::vector<llvm::SmallVector<unsigned int, 2u>, std::__1::allocator<llvm::SmallVector<unsigned int, 2u>>>>, 0u>>&) const::$_0::operator()(unsigned int) const","signatureNext":"Trie::visitChildren"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b: c where b.d == e
}
protocol c {
  associatedtype f: a
  associatedtype d: g where d.h == Self
}
protocol g {
  associatedtype h
  associatedtype K: g where K.K == Self
}
struct e: g {
  struct i<j: c> {
  }
}
