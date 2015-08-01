// RUN: %target-parse-verify-swift

func rejectsStringLiteral() {
  assert("foo") // expected-error {{cannot convert value of type 'String' to expected argument type '@autoclosure () -> Bool'}}
  precondition("foo") // expected-error {{cannot convert value of type 'String' to expected argument type '@autoclosure () -> Bool'}}
}

