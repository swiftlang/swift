// RUN: %target-typecheck-verify-swift

protocol P { }
struct S { }

// Concrete contraction must substitute through the metatype subject rather than
// leaving `T.Type : P` unreduced and aborting signature verification.
func f<T>(_: T.Type) where T == S, T.Type: P { }
// expected-warning@-1 {{same-type requirement makes generic parameter 'T' non-generic; this is an error in the Swift 6 language mode}}
// expected-error@-2 {{type 'S.Type' in conformance requirement does not refer to a generic parameter or associated type}}
