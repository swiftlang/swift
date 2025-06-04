// RUN: %target-typecheck-verify-swift

protocol P<A, B> {
  associatedtype A  // expected-note {{multiple matching types named 'A'}}
  associatedtype B

  func a(_: A)
}

extension P {
  func a(_: A) {}
}

struct S: P<Int, String> {}

func f(_: Int.Type) {}
func g(_: String.Type) {}

f(S.A.self)
g(S.B.self)

struct G<T, U>: P<T, U> {}

f(G<Int, String>.A.self)
g(G<Int, String>.B.self)

protocol Q<B> {
  associatedtype B  // expected-note {{protocol requires nested type 'B'}}
}

struct S2: P, Q<String> {
  func a(_: Int) {}
}

struct S3: P & Q<String> {
  func a(_: Int) {}
}

protocol R {}

struct S4: (P & Q<String>) & R {
  func a(_: Int) {}
}

struct Bad: P<Int, Float> { // expected-error {{type 'Bad' does not conform to protocol 'P'}}
  typealias A = String  // expected-note {{possibly intended match}}
}

let x = Bad.A.self
g(x)

struct Circle: Q<Circle.A> {}
// expected-note@-1 {{'Circle' declared here}}
// expected-error@-2 {{type 'Circle' does not conform to protocol 'Q'}}
// expected-note@-3 {{add stubs for conformance}}
// expected-error@-4 {{'A' is not a member type of struct 'type_witness_from_parameterized_protocol.Circle'}}