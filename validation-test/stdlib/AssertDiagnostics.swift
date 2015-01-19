// RUN: %target-parse-verify-swift

func rejectsStringLiteral() {
  assert("foo") // expected-error {{cannot invoke 'assert' with an argument list of type '(String)'}} expected-note{{expected an argument list of type '(@autoclosure () -> Bool, @autoclosure () -> String, StaticString, UWord)'}}
  precondition("foo") // expected-error {{cannot invoke 'precondition' with an argument list of type '(String)'}} expected-note{{expected an argument list of type '(@autoclosure () -> Bool, @autoclosure () -> String, StaticString, UWord)'}}
}

