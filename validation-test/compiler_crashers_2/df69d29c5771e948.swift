// {"signature":"swift::constraints::ConstraintSystem::repairFailures(swift::Type, swift::Type, swift::constraints::ConstraintKind, swift::optionset::OptionSet<swift::constraints::ConstraintSystem::TypeMatchFlags, unsigned int>, llvm::SmallVectorImpl<swift::constraints::RestrictionOrFix>&, swift::constraints::ConstraintLocatorBuilder)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a< b , c : Sequence, d >( b, c) where b== c.Element a( ( 0 , [ (0 0), (
