// RUN: %swift %s -parse -verify

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)

func x(x) { // expected-error {{use of undeclared type 'x'}}
}
