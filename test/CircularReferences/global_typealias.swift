// RUN: %target-typecheck-verify-swift -iterative-type-checker

typealias A = B // expected-error{{circular reference}}
typealias C = D // expected-note{{through reference here}}
typealias D = (A, Int) // expected-note{{through reference here}}
typealias B = C // expected-note{{through reference here}}
