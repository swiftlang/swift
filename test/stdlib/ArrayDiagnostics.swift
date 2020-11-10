// RUN: %target-typecheck-verify-swift

class NotEquatable {}

func test_ArrayOfNotEquatableIsNotEquatable() {
  var a = [ NotEquatable(), NotEquatable() ]
  // There is also a note attached to declaration - requirement from conditional conformance of '[NotEquatable]' to 'Equatable'
  if a == a {} // expected-error {{operator function '==' requires that 'NotEquatable' conform to 'Equatable'}}
}
