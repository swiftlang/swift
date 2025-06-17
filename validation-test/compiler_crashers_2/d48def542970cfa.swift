// {"signature":"swift::rewriting::performConcreteContraction(llvm::ArrayRef<swift::StructuralRequirement>, llvm::SmallVectorImpl<swift::StructuralRequirement>&, llvm::SmallVectorImpl<swift::rewriting::RequirementError>&, bool)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a < each b, each c where(repeat(each c each b)) : {
  typealias d<each b> = () func e where d<repeat each b> ==
