// {"kind":"typecheck","original":"9b29b85d","signature":"swift::constraints::ConstraintSystem::simplifyKeyPathConstraint(swift::Type, swift::Type, swift::Type, llvm::ArrayRef<swift::TypeVariableType*>, swift::optionset::OptionSet<swift::constraints::ConstraintSystem::TypeMatchFlags, unsigned int>, swift::constraints::ConstraintLocatorBuilder)","signatureAssert":"Assertion failed: (contextualRootTy && contextualValueTy), function operator()","signatureNext":"ConstraintSystem::simplifyConstraint"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a<each b> {
  c->(repeat(each b).Type) { (repeat \d
  }
