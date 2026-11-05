// RUN: %target-typecheck-verify-swift -module-name M

protocol P {
  var v: String { get }
}

func foo(bar: [P], baz: [P]) {
// expected-cparser-note@-1 {{'foo(bar:baz:)' declared here}}
}

struct S {
  static func bar(fiz: [P], baz: [P]) {}
// expected-cparser-note@-1 {{'bar(fiz:baz:)' declared here}}
}

do {
  let _ = M.foo(bar & // expected-cparser-error {{missing arguments for parameters 'bar', 'baz' in call}}
  // expected-astgen-error@-1 {{expected expression after operator}}
  // expected-astgen-error@-2 {{expected ')' to end function call}}
  // expected-astgen-note@-3 {{to match this opening '('}}
} // expected-cparser-error {{expected expression after operator}}

do {
  let _ = S.bar(fiz & // expected-cparser-error {{missing arguments for parameters 'fiz', 'baz' in call}}
  // expected-astgen-error@-1 {{expected expression after operator}}
  // expected-astgen-error@-2 {{expected ')' to end function call}}
  // expected-astgen-note@-3 {{to match this opening '('}}
} // expected-cparser-error {{expected expression after operator}}
