// {"kind":"typecheck","original":"0f52c8c0","signature":"swift::constraints::ConstraintSystem::simplifyExplicitGenericArgumentsConstraint(swift::Type, swift::Type, swift::optionset::OptionSet<swift::constraints::ConstraintSystem::TypeMatchFlags, unsigned int>, swift::constraints::ConstraintLocatorBuilder)","signatureAssert":"Assertion failed: (found != openedOverloadTypes.end()), function simplifyExplicitGenericArgumentsConstraint"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a {
  struct b
    struct b< a {
      struct c<d {
        e = b< f>.c< >
