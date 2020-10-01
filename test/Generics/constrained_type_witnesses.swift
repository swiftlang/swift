// RUN: %target-typecheck-verify-swift

protocol P {
  associatedtype A
  // expected-note@-1 3{{protocol requires nested type 'A'; do you want to add it?}}
}

struct S1<T> {}

extension S1 where T : P {
  typealias A = Int
}

// This is rejected because S1.A is not a suitable witness for P.A.
extension S1 : P {}
// expected-error@-1 {{type 'S1<T>' does not conform to protocol 'P'}}

struct S2<T> {}

extension S2 where T : P {
  typealias A = Never
}

// Hack: This is OK to make SwiftUI work, which accidentally relies on the
// incorrect behavior with a typealias whose underlying type is 'Never'
// (so it didn't hit the compiler crash).
extension S2 : P {}

// Here we have a suitable witness
struct S3<T> {}

extension S3 where T == Int {
  typealias A = Int
}

extension S3 : P where T == Int {}

// Check where clause on the type itself

struct S4<T> {
  typealias A = Int where T : P
}

extension S4 : P {}
// expected-error@-1 {{type 'S4<T>' does not conform to protocol 'P'}}

struct S5<T> {
  typealias A = Never where T : P
}

extension S5 : P {}

struct S6<T> {
  typealias A = Int where T == Int
}

extension S6 : P where T == Int {}

// Witness in a constrained protocol extension
protocol Q {
  associatedtype B
}

extension Q where B == Int {
  typealias A = Int
}

struct S7 : Q, P {
  typealias B = Int
}

struct S8 : Q, P {
// expected-error@-1 {{type 'S8' does not conform to protocol 'P'}}
  typealias B = String
}
