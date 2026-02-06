// {"kind":"typecheck","original":"17428765","signature":"swift::constraints::getLoc(swift::ASTNode)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@propertyWrapper struct a {
  wrappedValue : Bool  var projectedValue  init(projectedValue  Bool
}
func b(@a  Bool)
b
