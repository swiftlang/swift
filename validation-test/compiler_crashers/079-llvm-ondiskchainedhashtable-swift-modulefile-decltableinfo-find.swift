// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing
// RUN: %swift -parse %s -verify

func b((Any, e))(e: (Any) -> <d>(()-> d) -> f // expected-error{{use of undeclared type}}
// expected-error@-1{{expected ',' separator}}
// expected-error@-2{{expected ',' separator}}
// expected-error@-3{{expected parameter typ}}
