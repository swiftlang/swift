// RUN: %target-typecheck-verify-swift

class NotEquatable {}

func test_ArrayOfNotEquatableIsNotEquatable() {
  var a = [ NotEquatable(), NotEquatable() ]
  if a == a {} // expected-error {{referencing operator function '==' on 'Array' requires that 'NotEquatable' conform to 'Equatable'}}
}
