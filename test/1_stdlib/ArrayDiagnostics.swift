// RUN: %swift %s -parse -verify

class NotEquatable {}

func test_ArrayOfNotEquatableIsNotEquatable() {
  var a = [ NotEquatable(), NotEquatable() ]
  // FIXME: This is an awful error.
  if a == a {} // expected-error {{cannot invoke '==' with an argument list of type '(@lvalue [NotEquatable], @lvalue [NotEquatable])'}}
}

