// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated

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

struct S4: P<Int, String>, Q<String> {
  func a(_: Int) {}
}

struct S5: P<Int, String> & Q<String> {
  func a(_: Int) {}
}

protocol R {}

struct S6: (P & Q<String>) & R {
  func a(_: Int) {}
}

struct Bad: P<Int, Float> { // expected-error {{type 'Bad' does not conform to protocol 'P'}}
  typealias A = String
  // expected-note@-1 {{possibly intended match}}
}

struct Bad2: P<Int, Float>, Q<String> {
// expected-error@-1 {{type 'Bad2' does not conform to protocol 'P'}}
// expected-error@-2 {{type 'Bad2' does not conform to protocol 'Q'}}
// expected-error@-3 {{ambiguous associated type 'B' in conformance to 'P'}}
// expected-error@-4 {{ambiguous associated type 'B' in conformance to 'Q'}}
// expected-note@-5 4{{candidate inferred from parameterized protocol here}}
}

struct Bad3: P<Int, Float> & Q<String> {
// expected-error@-1 {{type 'Bad3' does not conform to protocol 'P'}}
// expected-error@-2 {{type 'Bad3' does not conform to protocol 'Q'}}
// expected-error@-3 {{ambiguous associated type 'B' in conformance to 'P'}}
// expected-error@-4 {{ambiguous associated type 'B' in conformance to 'Q'}}
// expected-note@-5 4{{candidate inferred from parameterized protocol here}}
}

protocol W<B> {
  associatedtype B
}

protocol W2<B> {
  associatedtype B
}

protocol W3<B> {
  associatedtype B
}

struct Bad4: P<Int, String>, Q<String>, W<Float>, W2<Float>, W3<String> {
// expected-error@-1 {{type 'Bad4' does not conform to protocol 'P'}}
// expected-error@-2 {{type 'Bad4' does not conform to protocol 'Q'}}
// expected-error@-3 {{type 'Bad4' does not conform to protocol 'W'}}
// expected-error@-4 {{type 'Bad4' does not conform to protocol 'W2'}}
// expected-error@-5 {{type 'Bad4' does not conform to protocol 'W3'}}
// expected-error@-6 {{ambiguous associated type 'B' in conformance to 'P'}}
// expected-error@-7 {{ambiguous associated type 'B' in conformance to 'Q'}}
// expected-error@-8 {{ambiguous associated type 'B' in conformance to 'W'}}
// expected-error@-9 {{ambiguous associated type 'B' in conformance to 'W2'}}
// expected-error@-10 {{ambiguous associated type 'B' in conformance to 'W3'}}
// expected-note@-11 10{{candidate inferred from parameterized protocol here}}
}

let x = Bad.A.self
g(x)

struct Circle: Q<Circle.A> {}
// expected-note@-1 {{'Circle' declared here}}
// expected-error@-2 {{type 'Circle' does not conform to protocol 'Q'}}
// expected-note@-3 {{add stubs for conformance}}
// expected-error@-4 {{'A' is not a member type of struct 'type_witness_from_parameterized_protocol.Circle'}}
