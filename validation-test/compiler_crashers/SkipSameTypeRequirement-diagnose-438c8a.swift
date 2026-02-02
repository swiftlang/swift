// {"kind":"typecheck","signature":"swift::constraints::SkipSameTypeRequirement::diagnose(swift::constraints::Solution const&, bool) const","signatureAssert":"Assertion failed: (getGenericContext() && \"Affected decl not within a generic context?\"), function RequirementFailure"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a struct b extension Dictionary : a where Value == let func !c {
  let d : [Int:b] let : a = d
