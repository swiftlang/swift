// {"kind":"typecheck","original":"92855ece","signature":"swift::rewriting::RequirementMachine::buildRequirementsFromRules(llvm::ArrayRef<unsigned int>, llvm::ArrayRef<unsigned int>, llvm::ArrayRef<swift::GenericTypeParamType*>, bool, std::__1::vector<swift::Requirement, std::__1::allocator<swift::Requirement>>&, std::__1::vector<swift::ProtocolTypeAlias, std::__1::allocator<swift::ProtocolTypeAlias>>&) const","signatureNext":"RequirementMachine::computeMinimalGenericSignature"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b: c
  associatedtype d: a
}
protocol
  c
{
  associatedtype d: a where d.b == Self
}
extension Dictionary: a
where
  Value: c,
  Value.d == Dictionary
{
}
