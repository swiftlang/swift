// {"kind":"typecheck","signature":"swift::constraints::RequirementFailure::RequirementFailure(swift::constraints::Solution const&, swift::Type, swift::Type, swift::constraints::ConstraintLocator*)","signatureAssert":"Assertion failed: (AffectedDecl), function RequirementFailure","signatureNext":"MissingConformanceFailure::MissingConformanceFailure"}
// The crash depends on the C++ parser's handling of this input.
// UNSUPPORTED: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a
  struct b<c: a {
    d: c
  }
  let : () -> b = {
    b(0
