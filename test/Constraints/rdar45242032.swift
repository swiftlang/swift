// RUN: %target-typecheck-verify-swift -module-name M

protocol P {
  var v: String { get }
}

func foo(bar: [P], baz: [P]) {
// expected-note@-1 {{'foo(bar:baz:)' declared here}}
}

struct S {
  static func bar(fiz: [P], baz: [P]) {}
// expected-note@-1 {{'bar(fiz:baz:)' declared here}}
}

do {
  let _ = M.foo(bar & // expected-error {{missing argument for parameter 'bar' in call}}
} // expected-error {{expected expression after operator}}

do {
  let _ = S.bar(fiz & // expected-error {{missing argument for parameter 'fiz' in call}}
} // expected-error {{expected expression after operator}}
