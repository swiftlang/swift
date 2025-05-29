// RUN: %target-typecheck-verify-swift

protocol P1 {
  func foo() // expected-note {{protocol requires function 'foo()'}}
  func bar() // okay
}

extension P1 {
  func bar() {} // okay
}

struct P1Conformer : P1 {} // expected-error {{does not conform}} expected-note {{add stubs for conformance}} 


protocol P2 {
  func foo() // expected-note {{protocol requires function 'foo()'}}
  func bar() // expected-note {{protocol requires function 'bar()'}}
}
protocol P2Helper {}

extension P2 where Self : P2Helper {
  func bar() {} // expected-note {{candidate}}
}

struct P2Conformer : P2 {} // expected-error {{does not conform}} expected-note {{add stubs for conformance}} 


protocol P3 {
  func foo() // expected-note {{protocol requires function 'foo()'}}
  func bar() // okay
  func baz() -> Baz
  associatedtype Baz
}

extension P3 {
  func bar() {} // okay
}

struct P3Conformer : P3 { // expected-error {{does not conform}} expected-note {{add stubs for conformance}} 
  func baz() -> Int { return 0 }
}


protocol P4 {
  func foo() // expected-note {{protocol requires function 'foo()'}}
  func bar() // expected-note {{protocol requires function 'bar()'}}
  func baz() -> Baz // okay
  associatedtype Baz
}
protocol P4Helper {}

extension P4 where Self : P4Helper {
  func bar() {} // expected-note {{candidate}}
}

struct P4Conformer : P4 { // expected-error {{does not conform}} expected-note {{add stubs for conformance}} 
  func baz() -> Int { return 0 }
}


protocol P5 {
  associatedtype Foo
  func foo() -> Foo // expected-note {{protocol requires function 'foo()'}}
  func bar() -> Foo // okay
  func baz() -> Foo // okay
}

extension P5 {
  func bar() -> Foo { return foo() } // okay
}

struct P5Conformer : P5 { // expected-error {{does not conform}} expected-note {{add stubs for conformance}} 
  func baz() -> Int { return 0 }
}


protocol P6Base {
  associatedtype Foo // expected-note{{protocol requires nested type 'Foo'}}
  func foo()
  func bar() -> Foo
}
extension P6Base {
}
protocol P6 : P6Base {
  associatedtype Bar // expected-note {{protocol requires nested type 'Bar'}}
}
extension P6 {
  func bar() -> Bar? { return nil }
}

struct P6Conformer : P6 { // expected-error 2 {{does not conform}} expected-note {{add stubs for conformance}} 
  func foo() {}
}

// rdar://problem/23033862
// expected-note@+3 {{add stubs for conformance}} 
// expected-error@+2{{type 'A' does not conform to protocol 'OptionSet'}}
// expected-error@+1{{type 'A' does not conform to protocol 'RawRepresentable'}}
struct A: OptionSet {
  let rawValue = 0
  init() { }
}

// Type witness cannot have its own generic parameters
// FIXME: Crappy diagnostic
protocol PA {
  associatedtype A // expected-note 3 {{protocol requires nested type 'A'}}
}

struct BadCase1 : PA { // expected-error {{type 'BadCase1' does not conform to protocol 'PA'}} expected-note {{add stubs for conformance}} 
  struct A<T> {}
}

struct BadCase2 : PA { // expected-error {{type 'BadCase2' does not conform to protocol 'PA'}} expected-note {{add stubs for conformance}} 
  typealias A<T> = T
}

// Variation on the above
struct G<T> {}

struct BadCase3 : PA { // expected-error {{type 'BadCase3' does not conform to protocol 'PA'}} expected-note {{add stubs for conformance}} 
  typealias A = G
}

// rdar://problem/32215763
extension UInt32: @retroactive ExpressibleByStringLiteral {}
// expected-error@-1 {{type 'UInt32' does not conform to protocol 'ExpressibleByStringLiteral'}} 
// expected-error@-2 {{type 'UInt32' does not conform to protocol 'ExpressibleByExtendedGraphemeClusterLiteral'}}
// expected-error@-3 {{type 'UInt32' does not conform to protocol 'ExpressibleByUnicodeScalarLiteral'}}
// expected-note@-4 {{add stubs for conformance}} 

// After successfully type-checking this (due to the presumption of
// the type actually conforming), do not crash when failing to find
// the associated witness type.
let diagnose: UInt32 = "reta"

// Test that we attempt to resolve a value witness unless mapping its interface
// type into the conformance context produces an invalid type.
protocol P7 {
  associatedtype A: Sequence // expected-note {{protocol requires nested type 'A'}}

  subscript(subscript1 _: A) -> Never { get }
  subscript<T>(subscript2 _: T) -> Never where T: Sequence, T.Element == A { get }
  // expected-note@-1 {{protocol requires subscript with type '<T> (subscript2: T) -> Never'}}

  func method1(_: A)
  func method2() -> A.Element
  func method3<T>(_: T) where T: Sequence, T.Element == A
  // expected-note@-1 {{protocol requires function 'method3' with type '<T> (T) -> ()'}}
  func method4(_: Never) // expected-note {{protocol requires function 'method4' with type '(Never) -> ()'}}
}
do {
  struct Conformer: P7 {} // expected-error {{type 'Conformer' does not conform to protocol 'P7'}} expected-note {{add stubs for conformance}} 
}

protocol P8 {
  associatedtype A: Sequence where A.Element == Never

  func method1(_: A) // expected-note {{protocol requires function 'method1' with type '(Conformer.A) -> ()' (aka '(Array<Bool>) -> ()')}}
  func method2() -> A.Element // expected-note {{protocol requires function 'method2()' with type '() -> Bool'}}
  func method3<T>(_: T) where T: Sequence, T.Element == A
  // expected-note@-1 {{protocol requires function 'method3' with type '<T> (T) -> ()'}}
  func method4(_: Never) // expected-note {{protocol requires function 'method4' with type '(Never) -> ()'}}
}
do {
  struct Conformer: P8 {
    // expected-error@-1 {{type 'Conformer' does not conform to protocol 'P8'}}
    // expected-error@-2 {{'P8' requires the types 'Bool' and 'Never' be equivalent}}
    // expected-note@-3 {{requirement specified as 'Self.A.Element' == 'Never' [with Self = Conformer]}}
    // expected-note@-4 {{add stubs for conformance}} 
    typealias A = Array<Bool>
  }
}

protocol P9a {
  associatedtype A
}
protocol P9b: P9a where A: Sequence {
  subscript(subscript1 _: A.Element) -> Never { get }
  subscript<T>(subscript2 _: T) -> Never where T: Sequence, T.Element == A.Element { get }
  // expected-note@-1 {{protocol requires subscript with type '<T> (subscript2: T) -> Never'}}

  func method1(_: A) // expected-note {{protocol requires function 'method1' with type '(Conformer.A) -> ()' (aka '(Bool) -> ()')}}
  func method2() -> A.Element
  func method3<T>(_: T) where T: Sequence, T.Element == A
  // expected-note@-1 {{protocol requires function 'method3' with type '<T> (T) -> ()'}}
  func method4(_: Never) // expected-note {{protocol requires function 'method4' with type '(Never) -> ()'}}
}
do {
  struct Conformer: P9b {
    // expected-error@-1 {{type 'Conformer' does not conform to protocol 'P9b'}}
    // expected-error@-2 {{type 'Conformer.A' (aka 'Bool') does not conform to protocol 'Sequence'}}
    // expected-note@-3 {{add stubs for conformance}} 
    typealias A = Bool
  }
}

protocol P10a {
  associatedtype A
}
protocol P10b: P10a where A: Sequence, A.Element == Never {
  func method1(_: A) // expected-note {{protocol requires function 'method1' with type '(Conformer.A) -> ()' (aka '(Array<Bool>) -> ()')}}
  func method2() -> A.Element // expected-note {{protocol requires function 'method2()' with type '() -> Bool'}}
  func method3<T>(_: T) where T: Sequence, T.Element == A
  // expected-note@-1 {{protocol requires function 'method3' with type '<T> (T) -> ()'}}
  func method4(_: Never) // expected-note {{protocol requires function 'method4' with type '(Never) -> ()'}}
}
do {
  struct Conformer: P10b {
    // expected-error@-1 {{type 'Conformer' does not conform to protocol 'P10b'}}
    // expected-error@-2 {{'P10b' requires the types 'Bool' and 'Never' be equivalent}}
    // expected-note@-3 {{requirement specified as 'Self.A.Element' == 'Never' [with Self = Conformer]}}
    // expected-note@-4 {{add stubs for conformance}} 
    typealias A = Array<Bool>
  }
}

protocol P11 {
  associatedtype A: Equatable
  // FIXME: Should not resolve witness for 'method', but Type::subst doesn't care
  // about conditional requirements when querying type witnesses.
  // expected-note@+1 {{protocol requires function 'method()' with type '() -> Conformer.A' (aka '() -> Array<any P11>')}}
  func method() -> A
}
do {
  struct Conformer: P11 {
    // expected-error@-1 {{type 'Conformer' does not conform to protocol 'P11'}}
    // expected-error@-2 {{type 'any P11' does not conform to protocol 'Equatable'}} // FIXME: Crappy diagnostics
    // expected-error@-3 {{'P11' requires that 'any P11' conform to 'Equatable'}}
    // expected-note@-4 {{requirement specified as 'any P11' : 'Equatable'}}
    // expected-note@-5 {{requirement from conditional conformance of 'Conformer.A' (aka 'Array<any P11>') to 'Equatable'}}
    // expected-note@-6 {{add stubs for conformance}} 
    typealias A = Array<any P11>
  }
}

protocol P12 {
  associatedtype A: Sequence where A.Element == Never // expected-note {{protocol requires nested type 'A'}}
  // FIXME: Should not resolve witness for 'prop', but getInterfaceType() returns
  // '(Self) -> Never' instead of '(Self) -> Self.A.Element', and the invalid
  // type parameter is never found (see 'hasInvalidTypeInConformanceContext').
  // This happens because getInterfaceType() on properties returns contextual
  // types that have been mapped out of context, but mapTypeOutOfContext() does
  // not reconstitute type parameters that were substituted with concrete types.
  // Instead, patterns should be refactored to use interface types, at least if
  // they appear in type contexts.
  //
  // expected-note@+1 {{protocol requires property 'prop' with type '(Conformer) -> Never'}}
  var prop: (Self) -> A.Element { get }
}
do {
  struct Conformer: P12 { // expected-error {{type 'Conformer' does not conform to protocol 'P12'}} expected-note {{add stubs for conformance}} 
    typealias A = Bool // expected-note {{possibly intended match 'Conformer.A' (aka 'Bool') does not conform to 'Sequence'}}
  }
}

class Class13 {}
protocol P13a {
  associatedtype A
}
protocol P13b: P13a where A: Class13 {}
do {
  struct Conformer: P13b {
    // expected-error@-1 {{type 'Conformer' does not conform to protocol 'P13b'}}
    // expected-error@-2 {{'P13b' requires that 'Conformer.A' (aka 'Array<Bool>') inherit from 'Class13'}}
    // expected-note@-3 {{requirement specified as 'Self.A' : 'Class13' [with Self = Conformer]}}
    typealias A = Array<Bool>
  }
}
