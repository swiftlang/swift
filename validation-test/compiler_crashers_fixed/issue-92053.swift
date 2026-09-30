// RUN: %target-swift-frontend -typecheck -verify %s
// https://github.com/swiftlang/swift/issues/92053

// The redeclaration makes 'E' ambiguous, so the thrown error type of the
// accessor resolves to an error type. Effects checking used to assert on it.

enum E: Error {}
// expected-note@-1 {{'E' previously declared here}}
// expected-note@-2 {{found this candidate}}
enum E: Error {}
// expected-error@-1 {{invalid redeclaration of 'E'}}
// expected-note@-2 {{found this candidate}}

var h: String { get throws(E) { h } }
// expected-error@-1 {{'E' is ambiguous for type lookup in this context}}
// expected-warning@-2 {{attempting to access 'h' within its own getter}}
