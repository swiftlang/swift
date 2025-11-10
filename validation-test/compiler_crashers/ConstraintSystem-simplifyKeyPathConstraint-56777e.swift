// {"kind":"typecheck","original":"bb182de2","signature":"swift::constraints::ConstraintSystem::simplifyKeyPathConstraint(swift::Type, swift::Type, swift::Type, llvm::ArrayRef<swift::TypeVariableType*>, swift::optionset::OptionSet<swift::constraints::ConstraintSystem::TypeMatchFlags, unsigned int>, swift::constraints::ConstraintLocatorBuilder)","signatureAssert":"Assertion failed: (contextualRootTy && contextualValueTy), function operator()"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a < each b > {
  var
    c : (repeat each b) {
      (repeat \ .)
  }
}
