// RUN: %target-swift-frontend -typecheck -verify -primary-file %s

// rdar://problem/57930643
struct School {
  var name: String
}
func test<A, B>(_: (A, B) -> Bool) {} // expected-note {{in call to function 'test'}}
test(\School.name)
// expected-error@-1 {{generic parameter 'A' could not be inferred}}
// expected-error@-2 {{generic parameter 'B' could not be inferred}}
// expected-error@-3 {{cannot convert key path literal to '(A, B) -> Bool', expected single-parameter function type '(School) -> String'}}

