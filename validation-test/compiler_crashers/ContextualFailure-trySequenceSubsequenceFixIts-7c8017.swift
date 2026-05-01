// {"kind":"typecheck","original":"716806ab","signature":"swift::constraints::ContextualFailure::trySequenceSubsequenceFixIts(swift::InFlightDiagnostic&) const","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"ContextualFailure::diagnoseAsError"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@propertyWrapper struct a {
  var wrappedValue: String {
    @a var b: Substring
  }
}
