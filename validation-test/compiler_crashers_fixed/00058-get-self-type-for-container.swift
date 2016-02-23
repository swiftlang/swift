// RUN: %target-swift-frontend %s -parse -verify

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

protocol c : b { // expected-error {{use of undeclared type 'b'}} expected-note {{in declaration of 'c'}}
	func b // expected-error {{expected '(' in argument list of function declaration}}
// expected-error@+1 {{expected declaration}}
