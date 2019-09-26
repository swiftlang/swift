// RUN: %target-typecheck-verify-swift

class NotEquatable {}

func test_ArrayOfNotEquatableIsNotEquatable() {
  var a = [ NotEquatable(), NotEquatable() ]
  if a == a {} // expected-error {{operator function '==' requires that 'NotEquatable' conform to 'Equatable'}}
}
