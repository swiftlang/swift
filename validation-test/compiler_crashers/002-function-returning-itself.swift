// RUN: %swift %s -parse -verify
// Test case submitted to project by https://github.com/practicalswift (practicalswift)

func a() -> a { // expected-error {{use of undeclared type 'a'}}
}
