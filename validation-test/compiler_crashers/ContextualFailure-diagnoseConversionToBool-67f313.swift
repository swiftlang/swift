// {"kind":"typecheck","signature":"swift::constraints::ContextualFailure::diagnoseConversionToBool() const","signatureAssert":"Assertion failed: (isa<X>(Val) && \"cast_if_present<Ty>() argument of incompatible type!\"), function cast_if_present","signatureNext":"ContextualFailure::diagnoseAsError"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@propertyWrapper struct a {
  wrappedValue : Bool {
    @a var b : Int
