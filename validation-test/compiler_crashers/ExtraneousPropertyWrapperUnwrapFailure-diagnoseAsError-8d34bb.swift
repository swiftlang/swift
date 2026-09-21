// {"kind":"typecheck","signature":"swift::constraints::ExtraneousPropertyWrapperUnwrapFailure::diagnoseAsError()","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"UsePropertyWrapper::diagnose"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@propertyWrapper struct a {
  wrappedValue: Int?
  var projectedValue
}
func b(@a  Int?)
b
