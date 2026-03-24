// {"kind":"typecheck","original":"7dccdf84","signature":"swift::constraints::ConstraintSystem::recordAppliedDisjunction(swift::constraints::ConstraintLocator*, swift::FunctionType*)","signatureAssert":"Assertion failed: (inserted), function recordAppliedDisjunction","signatureNext":"ConstraintSystem::simplifyAppliedOverloads"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@dynamicMemberLookup struct a<b> {
  subscript<c>(dynamicMember d: KeyPath<b, c>) -> c
  subscript<c>(dynamicMember d: WritableKeyPath<b, c>) -> a<c>
  func callAsFunction(e: <#type#>)
  let f = f {
  }
}
