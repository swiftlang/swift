// {"kind":"typecheck","original":"874cdeef","signature":"swift::constraints::ConstraintSystem::simplifyExplicitGenericArgumentsConstraint(swift::Type, swift::Type, swift::optionset::OptionSet<swift::constraints::ConstraintSystem::TypeMatchFlags, unsigned int>, swift::constraints::ConstraintLocatorBuilder)","signatureAssert":"Assertion failed: (found != openedOverloadTypes.end()), function simplifyExplicitGenericArgumentsConstraint","signatureNext":"ConstraintSystem::simplifyConstraint"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b
  struct c < d {
    struct e < f
      struct g : a {
        typealias b = e
        func h {
          b< >
