// RUN: %target-typecheck-verify-swift -enable-experimental-feature Lifetimes

// REQUIRES: swift_feature_Lifetimes

struct NE: ~Escapable {}

protocol P {
  // One note is produced for each failed conformance for each method.

  // We don't see lifetime annotations on the type of foo because they are attached to 
  @_lifetime(copy a, copy c)
  func foo(a: NE, b: NE, c: NE) -> NE
  // expected-note@-1 {{protocol requires function 'foo(a:b:c:)' with type '(NE, NE, NE) -> NE'}}
  // expected-note@-2 {{protocol requires function 'foo(a:b:c:)' with type '(NE, NE, NE) -> NE'}}
  // expected-note@-3 {{protocol requires function 'foo(a:b:c:)' with type '(NE, NE, NE) -> NE'}}
  // expected-note@-4 {{protocol requires function 'foo(a:b:c:)' with type '(NE, NE, NE) -> NE'}}
  @_lifetime(copy a, copy c)
  static func baz(a: NE, b: NE, c: NE) -> NE
  // expected-note@-1 {{protocol requires function 'baz(a:b:c:)' with type '(NE, NE, NE) -> NE'}}
  // expected-note@-2 {{protocol requires function 'baz(a:b:c:)' with type '(NE, NE, NE) -> NE'}}
  // expected-note@-3 {{protocol requires function 'baz(a:b:c:)' with type '(NE, NE, NE) -> NE'}}
  // expected-note@-4 {{protocol requires function 'baz(a:b:c:)' with type '(NE, NE, NE) -> NE'}}
}

struct ExactMatchP: P {
  @_lifetime(copy a, copy c)
  func foo(a: NE, b: NE, c: NE) -> NE { a } // OK
  @_lifetime(copy a, copy c)
  static func baz(a: NE, b: NE, c: NE) -> NE { a } // OK
}

struct DisjointCopyP: P { // expected-error {{type 'DisjointCopyP' does not conform to protocol 'P'}} expected-note {{add stubs for conformance}}
  @_lifetime(copy b, copy c)
  func foo(a: NE, b: NE, c: NE) -> NE { b } // expected-note {{candidate's lifetime dependencies do not match those specified by protocol}}
  @_lifetime(copy b, copy c)
  static func baz(a: NE, b: NE, c: NE) -> NE { b } // expected-note {{candidate's lifetime dependencies do not match those specified by protocol}}
}

struct DisjointBorrowP: P { // expected-error {{type 'DisjointBorrowP' does not conform to protocol 'P'}} expected-note {{add stubs for conformance}}
  @_lifetime(borrow a)
  func foo(a: NE, b: NE, c: NE) -> NE { a } // expected-note {{candidate's lifetime dependencies do not match those specified by protocol}}
  @_lifetime(borrow a)
  static func baz(a: NE, b: NE, c: NE) -> NE { a } // expected-note {{candidate's lifetime dependencies do not match those specified by protocol}}
}

struct SupersetCopyP: P { // expected-error {{type 'SupersetCopyP' does not conform to protocol 'P'}} expected-note {{add stubs for conformance}}
  @_lifetime(copy a, copy b, copy c)
  func foo(a: NE, b: NE, c: NE) -> NE { b } // expected-note {{candidate's lifetime dependencies do not match those specified by protocol}}
  @_lifetime(copy a, copy b, copy c)
  static func baz(a: NE, b: NE, c: NE) -> NE { b } // expected-note {{candidate's lifetime dependencies do not match those specified by protocol}}
}

struct SupersetBorrowP: P { // expected-error {{type 'SupersetBorrowP' does not conform to protocol 'P'}} expected-note {{add stubs for conformance}}
  @_lifetime(copy a, copy c, borrow b)
  func foo(a: NE, b: NE, c: NE) -> NE { b } // expected-note {{candidate's lifetime dependencies do not match those specified by protocol}}
  @_lifetime(copy a, copy c, borrow b)
  static func baz(a: NE, b: NE, c: NE) -> NE { b } // expected-note {{candidate's lifetime dependencies do not match those specified by protocol}}
}

// Witnesses can conform to the protocol if they have a subset of the lifetimes specified by the protocol.

struct SubsetImmortalP: P { // OK
  @_lifetime(immortal)
  func foo(a: NE, b: NE, c: NE) -> NE { fatalError() } // OK
  @_lifetime(immortal)
  static func baz(a: NE, b: NE, c: NE) -> NE { fatalError() } // OK
}

struct SubsetAP: P { // OK
  @_lifetime(copy a)
  func foo(a: NE, b: NE, c: NE) -> NE { a } // OK
  @_lifetime(copy a)
  static func baz(a: NE, b: NE, c: NE) -> NE { a } // OK
}

// Whether the dependencies were explicit or inferred should not affect
// conformance.
protocol Q {
  func baz(ne: NE) -> NE
  @_lifetime(copy ne)
  func qux(ne: NE) -> NE
}

struct ExplicitQ: Q { // OK
  @_lifetime(copy ne)
  func baz(ne: NE) -> NE { ne }
  @_lifetime(copy ne)
  func qux(ne: NE) -> NE { ne }
}

struct ImplicitQ: Q { // OK
  func baz(ne: NE) -> NE { ne }
  func qux(ne: NE) -> NE { ne }
}

// Conformances with function-type parameters.
//
// A witness method taking a function-type parameter f conforms if f's lifetime
// dependencies in the witness are a *superset* of the dependencies specified by
// the protocol. This ensures that the witness will respect the lifetime
// dependencies for any function that could be passed as a parameter to the
// protocol's method.

protocol R {
  func bar(f: @_lifetime(copy a, copy c) (_ a: NE, _ b: NE, _ c: NE) -> NE)
  // expected-note@-1 {{protocol requires function 'bar(f:)' with type '(@_lifetime(copy a, copy c) (_ a: NE, _ b: NE, _ c: NE) -> NE) -> ()'}}
  // expected-note@-2 {{protocol requires function 'bar(f:)' with type '(@_lifetime(copy a, copy c) (_ a: NE, _ b: NE, _ c: NE) -> NE) -> ()'}}
  // expected-note@-3 {{protocol requires function 'bar(f:)' with type '(@_lifetime(copy a, copy c) (_ a: NE, _ b: NE, _ c: NE) -> NE) -> ()'}}
  // expected-note@-4 {{protocol requires function 'bar(f:)' with type '(@_lifetime(copy a, copy c) (_ a: NE, _ b: NE, _ c: NE) -> NE) -> ()'}}
}

struct ExactMatchR: R {
  func bar( // OK
    f: @_lifetime(copy a, copy c) (_ a: NE, _ b: NE, _ c: NE) -> NE) {}
}

struct DisjointCopyR: R { // expected-error {{type 'DisjointCopyR' does not conform to protocol 'R'}} expected-note {{add stubs for conformance}}
  func bar( // expected-note {{candidate has non-matching type '(@_lifetime(copy b, copy c) (_ a: NE, _ b: NE, _ c: NE) -> NE) -> ()'}}
    f: @_lifetime(copy b, copy c) (_ a: NE, _ b: NE, _ c: NE) -> NE) {}
}

struct DisjointBorrowR: R { // expected-error {{type 'DisjointBorrowR' does not conform to protocol 'R'}} expected-note {{add stubs for conformance}}
  func bar( // expected-note {{candidate has non-matching type '(@_lifetime(borrow a) (_ a: NE, _ b: NE, _ c: NE) -> NE) -> ()'}}
    f: @_lifetime(borrow a) (_ a: NE, _ b: NE, _ c: NE) -> NE) {}
}

struct SupersetCopyR: R { // OK
  func bar( // OK
    f: @_lifetime(copy a, copy b, copy c) (_ a: NE, _ b: NE, _ c: NE) -> NE) {}
}

struct SupersetBorrowR: R { // OK
  func bar( // OK
    f: @_lifetime(copy a, copy c, borrow b) (_ a: NE, _ b: NE, _ c: NE) -> NE) {}
}

struct SubsetImmortalR: R { // expected-error {{type 'SubsetImmortalR' does not conform to protocol 'R'}} expected-note {{add stubs for conformance}}
  func bar( // expected-note {{candidate has non-matching type '(@_lifetime(immortal) (_ a: NE, _ b: NE, _ c: NE) -> NE) -> ()'}}
    f: @_lifetime(immortal) (_ a: NE, _ b: NE, _ c: NE) -> NE) {}
}

struct SubsetAR: R { // expected-error {{type 'SubsetAR' does not conform to protocol 'R'}} expected-note {{add stubs for conformance}}
  func bar( // expected-note {{candidate has non-matching type '(@_lifetime(copy a) (_ a: NE, _ b: NE, _ c: NE) -> NE) -> ()'}}
    f: @_lifetime(copy a) (_ a: NE, _ b: NE, _ c: NE) -> NE) {}
}

// *TODO*: Functions with multiple lifetime attributes/targets
// TODO: Tests for conditionally escapable associated types?
