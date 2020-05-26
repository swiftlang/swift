// RUN: %target-swift-frontend -typecheck -verify -primary-file %s

// rdar://problem/57930643
struct School {
  var name: String
}
func test<A, B>(_: (A, B) -> Bool) {} // expected-note {{in call to function 'test'}}
test(\School.name) // expected-error {{generic parameter 'A' could not be inferred}} // expected-error {{generic parameter 'B' could not be inferred}} // expected-error {{cannot convert key path into a multi-argument function type '(A, B) -> Bool'}}

