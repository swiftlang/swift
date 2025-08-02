// {"kind":"typecheck","signature":"(anonymous namespace)::AssociatedTypeInference::inferAbstractTypeWitnesses(llvm::ArrayRef<swift::AssociatedTypeDecl*>, unsigned int)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
class a : b protocol b {
  typealias c typealias c : = c
