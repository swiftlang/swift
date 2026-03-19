// {"kind":"typecheck","original":"8832fe33","signature":"swift::constraints::ConstraintSystem::getType(swift::ASTNode) const","signatureAssert":"Assertion failed: (found != NodeTypes.end() && \"Expected type to have been set!\"), function getType","signatureNext":"ExtraneousArgumentsFailure::diagnoseAsError"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@propertyWrapper struct a<b> {
  var wrappedValue: () -> b {
    @a var c: (Double) -> String
  }
}
