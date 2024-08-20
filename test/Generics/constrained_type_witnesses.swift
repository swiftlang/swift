// RUN: %target-typecheck-verify-swift

protocol P {
  associatedtype A
  // expected-note@-1 5{{protocol requires nested type 'A'}}
}

struct S1<T> {}

extension S1 where T : P {
  typealias A = Int
}

// This is rejected because S1.A is not a suitable witness for P.A.
extension S1 : P {}
// expected-error@-1 {{type 'S1<T>' does not conform to protocol 'P'}}
// expected-note@-2 {{add stubs for conformance}}

struct S2<T> {}

extension S2 where T : P {
  typealias A = Never
}
extension S2 : P {} 
// expected-error@-1 {{type 'S2<T>' does not conform to protocol 'P'}} 
// expected-note@-2 {{add stubs for conformance}}

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
// expected-note@-2 {{add stubs for conformance}}

struct S5<T> {
  typealias A = Never where T : P
}

extension S5 : P {} 
// expected-error@-1 {{type 'S5<T>' does not conform to protocol 'P'}}
// expected-note@-2 {{add stubs for conformance}}

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
// expected-note@-2 {{add stubs for conformance}}
  typealias B = String
}
