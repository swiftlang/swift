// {"kind":"typecheck","signature":"swift::constraints::getLoc(swift::ASTNode)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"ExtraneousPropertyWrapperUnwrapFailure::diagnoseAsError"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@propertyWrapper struct a {
  wrappedValue: Int?
  var projectedValue
}
func b(@a  Int?)
b
