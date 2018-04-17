// RUN: %target-typecheck-verify-swift

class NotEquatable {}

func test_ArrayOfNotEquatableIsNotEquatable() {
  var a = [ NotEquatable(), NotEquatable() ]
  // FIXME: This is an awful error.
  if a == a {} // expected-error {{'<Self where Self : Equatable> (Self.Type) -> (Self, Self) -> Bool' requires that 'NotEquatable' conform to 'Equatable'}}
  // expected-error@-1{{type 'NotEquatable' does not conform to protocol 'Equatable'}}
  // expected-note @-2 {{requirement specified as 'NotEquatable' : 'Equatable'}}
  // expected-note@-3{{requirement from conditional conformance of '[NotEquatable]' to 'Equatable'}}
}

