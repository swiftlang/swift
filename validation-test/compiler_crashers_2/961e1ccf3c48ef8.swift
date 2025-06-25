// {"signature":"swift::constraints::ConstraintSystem::simplifyKeyPathConstraint(swift::Type, swift::Type, swift::Type, llvm::ArrayRef<swift::TypeVariableType*>, swift::optionset::OptionSet<swift::constraints::ConstraintSystem::TypeMatchFlags, unsigned int>, swift::constraints::ConstraintLocatorBuilder)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a<each b>() -> (repeat Array<each b>) {
  (repeat \ [<#type#>])
}
