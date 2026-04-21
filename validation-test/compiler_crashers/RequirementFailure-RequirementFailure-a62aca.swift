// {"kind":"typecheck","signature":"swift::constraints::RequirementFailure::RequirementFailure(swift::constraints::Solution const&, swift::Type, swift::Type, swift::constraints::ConstraintLocator*)","signatureAssert":"Assertion failed: (AffectedDecl), function RequirementFailure","signatureNext":"MissingConformanceFailure::MissingConformanceFailure"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  typealias b<c> = <#type#>
}
a.b
