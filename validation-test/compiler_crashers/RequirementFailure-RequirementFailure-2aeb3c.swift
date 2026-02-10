// {"kind":"typecheck","original":"1f910c5a","signature":"swift::constraints::RequirementFailure::RequirementFailure(swift::constraints::Solution const&, swift::Type, swift::Type, swift::constraints::ConstraintLocator*)","signatureAssert":"Assertion failed: (getRequirementDC() && \"Couldn't find where the requirement came from?\"), function RequirementFailure"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: OS=macosx
[].reduce?(Int) {
  a in
  a {
  }
  []
}
