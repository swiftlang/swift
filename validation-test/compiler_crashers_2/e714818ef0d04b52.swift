// {"signature":"(anonymous namespace)::AssociatedTypeInference::findSolutionsRec(llvm::ArrayRef<swift::AssociatedTypeDecl*>, llvm::SmallVectorImpl<(anonymous namespace)::InferredTypeWitnessesSolution>&, llvm::SmallVectorImpl<(anonymous namespace)::InferredTypeWitnessesSolution>&, llvm::SmallVector<std::__1::pair<swift::ValueDecl*, swift::ValueDecl*>, 4u>&, unsigned int, unsigned int, unsigned int)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
class a : b protocol b {
  typealias c typealias c : = c
