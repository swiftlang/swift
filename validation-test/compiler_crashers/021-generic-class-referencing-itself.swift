// RUN: %swift %s -parse -verify
// Test case submitted to project by https://github.com/practicalswift (practicalswift)

class A<T : A> { // expected-error {{reference to generic type 'A' requires arguments in <...>}} expected-note {{generic type 'A' declared here}}
}
