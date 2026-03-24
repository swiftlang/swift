// {"kind":"typecheck","original":"80dccfdd","signature":"swift::constraints::ConstraintSystem::getType(swift::ASTNode) const","signatureAssert":"Assertion failed: (found != NodeTypes.end() && \"Expected type to have been set!\"), function getType","signatureNext":"getStructuralTypeContext"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@propertyWrapper struct a<b, c> {
  var wrappedValue: (b) -> c {
    @a var d: (Int) -> Int
  }
}
