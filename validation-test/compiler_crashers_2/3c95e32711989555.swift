// {"kind":"typecheck","signature":"swift::constraints::ConstraintSystem::simplifyConformsToConstraint(swift::Type, swift::ProtocolDecl*, swift::constraints::ConstraintKind, swift::constraints::ConstraintLocatorBuilder, swift::optionset::OptionSet<swift::constraints::ConstraintSystem::TypeMatchFlags, unsigned int>)","signatureAssert":"Assertion failed: (Index < Length && \"Invalid index!\"), function operator[]"}
// RUN: not --crash %target-swift-frontend -typecheck %s
\_(error)
