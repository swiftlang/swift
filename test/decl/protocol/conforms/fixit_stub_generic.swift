// RUN: %target-typecheck-verify-swift

protocol P1 {
  associatedtype A
}

protocol P2 {
  func f<T>(_: T, _: T) // expected-note {{protocol requires function 'f' with type '<T> (T, T) -> ()'}}
  func f<T: P1>(_: T, _: T.A) // expected-note {{protocol requires function 'f' with type '<T> (T, T.A) -> ()'}}
}

struct S: P2 {} // expected-error {{type 'S' does not conform to protocol 'P2'}}
// expected-note@-1 {{add stubs for conformance}}
