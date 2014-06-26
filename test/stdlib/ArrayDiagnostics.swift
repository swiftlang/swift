// RUN: %swift %s -parse -verify

class NotEquatable {}

func test_ArrayOfNotEquatableIsNotEquatable() {
  var a = [ NotEquatable(), NotEquatable() ]
  if a == a {} // expected-error {{'Array<NotEquatable>' is not convertible to '_ArrayCastKind'}}
}

