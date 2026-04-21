// {"kind":"typecheck","original":"74453967","signature":"swift::constraints::RequirementFailure::RequirementFailure(swift::constraints::Solution const&, swift::Type, swift::Type, swift::constraints::ConstraintLocator*)","signatureAssert":"Assertion failed: (getRequirementDC() && \"Couldn't find where the requirement came from?\"), function RequirementFailure","signatureNext":"SkipSameTypeRequirement::diagnose"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b where c == d
  associatedtype d
}
protocol e: a {
  associatedtype f
}
struct g<h: e
  extension g where h.d == h {
    heading: h.f
  }
  struct h: e {
    i: g<h> {
      .heading
