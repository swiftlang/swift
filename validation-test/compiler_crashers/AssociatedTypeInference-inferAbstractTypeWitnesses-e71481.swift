// {"kind":"typecheck","signature":"(anonymous namespace)::AssociatedTypeInference::inferAbstractTypeWitnesses(llvm::ArrayRef<swift::AssociatedTypeDecl*>, unsigned int)","signatureAssert":"Assertion failed: (!tyWitness.DefaultedAssocType && \"already recorded a default type witness\"), function addDefaultTypeWitness"}
// RUN: not --crash %target-swift-frontend -typecheck %s
class a : b protocol b {
  typealias c typealias c : = c
