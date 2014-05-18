// RUN: %swift %s -parse -verify

class NotEquatable {}

func test_ArrayOfNotEquatableIsNotEquatable() {
  var a = [ NotEquatable(), NotEquatable() ]
  if a == a {} // expected-error {{could not find an overload for '==' that accepts the supplied arguments}}
}

