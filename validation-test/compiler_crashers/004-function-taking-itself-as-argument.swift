// RUN: %swift %s -parse -verify
// Test case submitted to project by https://github.com/practicalswift (practicalswift)

func x(x) { // expected-error {{use of undeclared type 'x'}}
}
