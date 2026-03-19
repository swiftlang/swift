// {"kind":"typecheck","signature":"swift::constraints::ContextualFailure::diagnoseConversionToBool() const","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"ContextualFailure::diagnoseAsError"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@propertyWrapper struct a {
  wrappedValue : Bool {
    @a var b : Int
