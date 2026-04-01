// {"kind":"typecheck","original":"5c6513e6","signature":"swift::constraints::ConstraintSystem::getType(swift::ASTNode) const","signatureAssert":"Assertion failed: (found != NodeTypes.end() && \"Expected type to have been set!\"), function getType","signatureNext":"MissingArgumentsFailure::diagnoseAsError"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@propertyWrapper struct a<b, c> {
  var wrappedValue: (b) -> c {
    @a var d: () -> Int
  }
}
