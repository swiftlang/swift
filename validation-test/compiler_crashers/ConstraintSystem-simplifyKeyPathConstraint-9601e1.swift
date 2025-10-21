// {"kind":"typecheck","signature":"swift::constraints::ConstraintSystem::simplifyKeyPathConstraint(swift::Type, swift::Type, swift::Type, llvm::ArrayRef<swift::TypeVariableType*>, swift::optionset::OptionSet<swift::constraints::ConstraintSystem::TypeMatchFlags, unsigned int>, swift::constraints::ConstraintLocatorBuilder)","signatureAssert":"Assertion failed: (bgt->isKeyPath() || bgt->isWritableKeyPath() || bgt->isReferenceWritableKeyPath()), function operator()"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a<each b>() -> (repeat Array<each b>) {
  (repeat \ [<#type#>])
}
