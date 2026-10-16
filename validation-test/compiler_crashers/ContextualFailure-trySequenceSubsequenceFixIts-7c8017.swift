// {"kind":"typecheck","original":"716806ab","signature":"swift::constraints::ContextualFailure::trySequenceSubsequenceFixIts(swift::InFlightDiagnostic&) const","signatureAssert":"Assertion failed: (isa<X>(Val) && \"cast_if_present<Ty>() argument of incompatible type!\"), function cast_if_present","signatureNext":"ContextualFailure::tryFixIts"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@propertyWrapper struct a {
  var wrappedValue: String {
    @a var b: Substring
  }
}
