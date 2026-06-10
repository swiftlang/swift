// {"kind":"typecheck","original":"41e08f3a","signature":"swift::constraints::ConstraintSystem::getType(swift::ASTNode) const","signatureAssert":"Assertion failed: (found != NodeTypes.end() && \"Expected type to have been set!\"), function getType","signatureNext":"RequirementFailure::getOwnerType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@propertyWrapper struct a<b: Equatable> {
  var wrappedValue: b
}
let <#pattern#>: (a) -> Int = {
  (@a ) in
}
