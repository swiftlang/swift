// RUN: %target-typecheck-verify-swift

class NotEquatable {}

func test_ArrayOfNotEquatableIsNotEquatable() {
  var a = [ NotEquatable(), NotEquatable() ]
  // FIXME: This is an awful error.
  if a == a {} // expected-error {{binary operator '==' cannot be applied to two '[NotEquatable]' operands}}
  // expected-note @-1 {{overloads for '==' exist with these partially matching parameter lists: }}
}

