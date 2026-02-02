// {"signature":"swift::constraints::ConstraintSystem::simplifyRestrictedConstraintImpl(swift::constraints::ConversionRestrictionKind, swift::Type, swift::Type, swift::constraints::ConstraintKind, swift::optionset::OptionSet<swift::constraints::ConstraintSystem::TypeMatchFlags, unsigned int>, swift::constraints::ConstraintLocatorBuilder)"}
// RUN: not %target-swift-frontend -typecheck %s
struct a < b struct c func d .0 c<a> struct c < a, e
