// {"kind":"typecheck","original":"f7121195","signature":"swift::constraints::ConstraintSystem::repairFailures(swift::Type, swift::Type, swift::constraints::ConstraintKind, swift::optionset::OptionSet<swift::constraints::ConstraintSystem::TypeMatchFlags, unsigned int>, llvm::SmallVectorImpl<swift::constraints::RestrictionOrFix>&, swift::constraints::ConstraintLocatorBuilder)","signatureAssert":"Assertion failed: (Index < this->size() && \"Invalid index!\"), function operator[]","signatureNext":"ConstraintSystem::matchTypes"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a<b>( (b, b...) -> b)
a(+)
