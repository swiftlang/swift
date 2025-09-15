// {"signature":"swift::rewriting::RequirementMachine::buildRequirementsFromRules(llvm::ArrayRef<unsigned int>, llvm::ArrayRef<unsigned int>, llvm::ArrayRef<swift::GenericTypeParamType*>, bool, std::__1::vector<swift::Requirement, std::__1::allocator<swift::Requirement>>&, std::__1::vector<swift::ProtocolTypeAlias, std::__1::allocator<swift::ProtocolTypeAlias>>&) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
class a class b class c protocol d {
  associatedtype e where e : a, e : c, e : b
