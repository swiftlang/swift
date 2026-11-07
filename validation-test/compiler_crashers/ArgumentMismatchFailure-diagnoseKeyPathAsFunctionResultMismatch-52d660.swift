// {"kind":"typecheck","original":"fc2877a1","signature":"swift::constraints::ArgumentMismatchFailure::diagnoseKeyPathAsFunctionResultMismatch() const","signatureAssert":"Assertion failed: (Index < Length && \"Invalid index!\"), function operator[]","signatureNext":"ArgumentMismatchFailure::diagnoseAsError"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@propertyWrapper struct a<b> {
  var c: PartialKeyPath<AnyObject>
  var wrappedValue: b
  struct d {
    @a(\e) var e
  }
}
