// RUN: %target-typecheck-verify-swift -enable-experimental-feature Lifetimes -enable-experimental-feature SuppressedAssociatedTypesWithDefaults

// REQUIRES: swift_feature_Lifetimes
// REQUIRES: swift_feature_SuppressedAssociatedTypesWithDefaults

struct NE: ~Escapable {}

// MARK: Instance methods

protocol P {
  // One note is produced for each failed conformance for each method.

  // TODO: Update expected notes once we add closure context dependencies and
  // can attach method lifetime dependence information to the function type.
  @_lifetime(copy a, copy c)
  func foo(a: NE, b: NE, c: NE) -> NE
  // expected-note@-1 {{protocol requires function 'foo(a:b:c:)' with type '(NE, NE, NE) -> NE'}}
  // expected-note@-2 {{protocol requires function 'foo(a:b:c:)' with type '(NE, NE, NE) -> NE'}}
  // expected-note@-3 {{protocol requires function 'foo(a:b:c:)' with type '(NE, NE, NE) -> NE'}}
  // expected-note@-4 {{protocol requires function 'foo(a:b:c:)' with type '(NE, NE, NE) -> NE'}}
}

struct ExactMatchP: P {
  @_lifetime(copy a, copy c)
  func foo(a: NE, b: NE, c: NE) -> NE { a } // OK
}

struct DisjointCopyP: P { // expected-error {{type 'DisjointCopyP' does not conform to protocol 'P'}} expected-note {{add stubs for conformance}}
  @_lifetime(copy b, copy c)
  func foo(a: NE, b: NE, c: NE) -> NE { b } // expected-note {{candidate has a lifetime dependence not specified by the protocol}}
}

struct DisjointBorrowP: P { // expected-error {{type 'DisjointBorrowP' does not conform to protocol 'P'}} expected-note {{add stubs for conformance}}
  @_lifetime(borrow a)
  func foo(a: NE, b: NE, c: NE) -> NE { a } // expected-note {{candidate has a lifetime dependence not specified by the protocol}}
}

struct SupersetCopyP: P { // expected-error {{type 'SupersetCopyP' does not conform to protocol 'P'}} expected-note {{add stubs for conformance}}
  @_lifetime(copy a, copy b, copy c)
  func foo(a: NE, b: NE, c: NE) -> NE { b } // expected-note {{candidate has a lifetime dependence not specified by the protocol}}
}

struct SupersetBorrowP: P { // expected-error {{type 'SupersetBorrowP' does not conform to protocol 'P'}} expected-note {{add stubs for conformance}}
  @_lifetime(copy a, copy c, borrow b)
  func foo(a: NE, b: NE, c: NE) -> NE { b } // expected-note {{candidate has a lifetime dependence not specified by the protocol}}
}

// Witnesses can conform to the protocol if they have a subset of the lifetimes specified by the protocol.

struct SubsetImmortalP: P { // OK
  @_lifetime(immortal)
  func foo(a: NE, b: NE, c: NE) -> NE { fatalError() } // OK
}

struct SubsetAP: P { // OK
  @_lifetime(copy a)
  func foo(a: NE, b: NE, c: NE) -> NE { a } // OK
}

// MARK: Static methods
protocol PS {
  @_lifetime(copy a, copy c)
  static func baz(a: NE, b: NE, c: NE) -> NE
  // expected-note@-1 {{protocol requires function 'baz(a:b:c:)' with type '@_lifetime(copy a, copy c) (a a: NE, b b: NE, c c: NE) -> NE'}}
  // expected-note@-2 {{protocol requires function 'baz(a:b:c:)' with type '@_lifetime(copy a, copy c) (a a: NE, b b: NE, c c: NE) -> NE'}}
  // expected-note@-3 {{protocol requires function 'baz(a:b:c:)' with type '@_lifetime(copy a, copy c) (a a: NE, b b: NE, c c: NE) -> NE'}}
  // expected-note@-4 {{protocol requires function 'baz(a:b:c:)' with type '@_lifetime(copy a, copy c) (a a: NE, b b: NE, c c: NE) -> NE'}}
}

struct ExactMatchPS: PS {
  @_lifetime(copy a, copy c)
  static func baz(a: NE, b: NE, c: NE) -> NE { a } // OK
}

struct DisjointCopyPS: PS { // expected-error {{type 'DisjointCopyPS' does not conform to protocol 'PS'}} expected-note {{add stubs for conformance}}
  @_lifetime(copy b, copy c)
  static func baz(a: NE, b: NE, c: NE) -> NE { b } // expected-note {{candidate has a lifetime dependence not specified by the protocol}}
}

struct DisjointBorrowPS: PS { // expected-error {{type 'DisjointBorrowPS' does not conform to protocol 'PS'}} expected-note {{add stubs for conformance}}
  @_lifetime(borrow a)
  static func baz(a: NE, b: NE, c: NE) -> NE { a } // expected-note {{candidate has a lifetime dependence not specified by the protocol}}
}

struct SupersetCopyPS: PS { // expected-error {{type 'SupersetCopyPS' does not conform to protocol 'PS'}} expected-note {{add stubs for conformance}}
  @_lifetime(copy a, copy b, copy c)
  static func baz(a: NE, b: NE, c: NE) -> NE { b } // expected-note {{candidate has a lifetime dependence not specified by the protocol}}
}

struct SupersetBorrowPS: PS { // expected-error {{type 'SupersetBorrowPS' does not conform to protocol 'PS'}} expected-note {{add stubs for conformance}}
  @_lifetime(copy a, copy c, borrow b)
  static func baz(a: NE, b: NE, c: NE) -> NE { b } // expected-note {{candidate has a lifetime dependence not specified by the protocol}}
}

struct SubsetImmortalPS: PS { // OK
  @_lifetime(immortal)
  static func baz(a: NE, b: NE, c: NE) -> NE { fatalError() } // OK
}

struct SubsetAPS: PS { // OK
  @_lifetime(copy a)
  static func baz(a: NE, b: NE, c: NE) -> NE { a } // OK
}

// MARK: Inferred defaults equivalent to explicit spelling.
//
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

// MARK: Conformances with function-type parameters.
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
  // expected-note@-5 {{protocol requires function 'bar(f:)' with type '(@_lifetime(copy a, copy c) (_ a: NE, _ b: NE, _ c: NE) -> NE) -> ()'}}
}

struct ExactMatchR: R {
  func bar( // OK
    f: @_lifetime(copy a, copy c) (_ a: NE, _ b: NE, _ c: NE) -> NE) {}
}

struct DisjointCopyR: R { // expected-error {{type 'DisjointCopyR' does not conform to protocol 'R'}} expected-note {{add stubs for conformance}}
  func bar( // expected-note {{candidate has non-matching type '(@_lifetime(copy b, copy c) (_ a: NE, _ b: NE, _ c: NE) -> NE) -> ()'}}
    f: @_lifetime(copy b, copy c) (_ a: NE, _ b: NE, _ c: NE) -> NE) {}
}

struct MismatchBorrow: R { // expected-error {{type 'MismatchBorrow' does not conform to protocol 'R'}} expected-note {{add stubs for conformance}}
  func bar( // expected-note {{candidate has non-matching type '(@_lifetime(borrow a, borrow c) (_ a: NE, _ b: NE, _ c: NE) -> NE) -> ()'}}
    f: @_lifetime(borrow a, borrow c) (_ a: NE, _ b: NE, _ c: NE) -> NE) {}
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

// MARK: Types conforming to non-Escapable protocols
protocol S: ~Escapable {
  associatedtype T: ~Escapable
  @_lifetime(a: copy a, copy b)
  @_lifetime(b: copy b)
  @_lifetime(copy a, copy c)
  func foo(a: inout T, b: inout T, c: T) -> T
  // expected-note@-1 {{protocol requires function 'foo(a:b:c:)' with type '(inout BorrowMismatchS.T, inout BorrowMismatchS.T, BorrowMismatchS.T) -> BorrowMismatchS.T' (aka '(inout NE, inout NE, NE) -> NE')}}
  // expected-note@-2 {{protocol requires function 'foo(a:b:c:)' with type '(inout BorrowMismatchS2.T, inout BorrowMismatchS2.T, BorrowMismatchS2.T) -> BorrowMismatchS2.T' (aka '(inout NE, inout NE, NE) -> NE')}}
  // expected-note@-3 {{protocol requires function 'foo(a:b:c:)' with type '(inout SupersetResultS.T, inout SupersetResultS.T, SupersetResultS.T) -> SupersetResultS.T' (aka '(inout NE, inout NE, NE) -> NE')}}
  // expected-note@-4 {{protocol requires function 'foo(a:b:c:)' with type '(inout SupersetAS.T, inout SupersetAS.T, SupersetAS.T) -> SupersetAS.T' (aka '(inout NE, inout NE, NE) -> NE')}}
}

struct ExactMatchS: S { // OK
  typealias T = NE
  @_lifetime(a: copy a, copy b)
  @_lifetime(b: copy b)
  @_lifetime(copy a, copy c)
  func foo(a: inout T, b: inout T, c: T) -> T { fatalError() }
}

struct BorrowMismatchS: S { // expected-error {{type 'BorrowMismatchS' does not conform to protocol 'S'}} expected-note {{add stubs for conformance}}
  typealias T = NE
  @_lifetime(a: copy a, copy b)
  @_lifetime(b: copy b)
  @_lifetime(copy a, borrow c)
  func foo(a: inout T, b: inout T, c: T) -> T { fatalError() }
  // expected-note@-1 {{candidate has a lifetime dependence not specified by the protocol}}
}

struct BorrowMismatchS2: S { // expected-error {{type 'BorrowMismatchS2' does not conform to protocol 'S'}} expected-note {{add stubs for conformance}}
  typealias T = NE
  @_lifetime(a: copy a, copy b)
  @_lifetime(b: &a, copy b)
  @_lifetime(copy a, copy c)
  func foo(a: inout T, b: inout T, c: T) -> T { fatalError() }
  // expected-note@-1 {{candidate has a lifetime dependence not specified by the protocol}}
}

struct SubsetImmortalS: S { // OK
  typealias T = NE
  @_lifetime(a: immortal)
  @_lifetime(b: immortal)
  @_lifetime(immortal)
  func foo(a: inout T, b: inout T, c: T) -> T { fatalError() }
}

struct SubsetAS: S { // OK
  typealias T = NE
  @_lifetime(a: copy a)
  @_lifetime(b: copy b)
  @_lifetime(copy c)
  func foo(a: inout T, b: inout T, c: T) -> T { fatalError() }
}

struct SubsetCS: S { // OK
  typealias T = NE
  @_lifetime(a: copy a, copy b)
  @_lifetime(b: copy b)
  @_lifetime(copy c)
  func foo(a: inout T, b: inout T, c: T) -> T { fatalError() }
}

struct SupersetResultS: S { // expected-error {{type 'SupersetResultS' does not conform to protocol 'S'}} expected-note {{add stubs for conformance}}
  typealias T = NE
  @_lifetime(a: copy a, copy b)
  @_lifetime(b: copy b)
  @_lifetime(copy a, copy b, copy c)
  func foo(a: inout T, b: inout T, c: T) -> T { fatalError() }
  // expected-note@-1 {{candidate has a lifetime dependence not specified by the protocol}}
}

struct SupersetAS: S { // expected-error {{type 'SupersetAS' does not conform to protocol 'S'}} expected-note {{add stubs for conformance}}
  typealias T = NE
  @_lifetime(a: copy a, copy b, copy c)
  @_lifetime(b: copy b)
  @_lifetime(copy a, copy c)
  func foo(a: inout T, b: inout T, c: T) -> T { fatalError() }
  // expected-note@-1 {{candidate has a lifetime dependence not specified by the protocol}}
}

struct EscapableS: S { // OK
  func foo(a: inout Int, b: inout Int, c: Int) -> Int { a }
}

// MARK: Conditionally escapable types conforming to protocols
protocol IGet { // expected-note{{type 'NEGet' does not conform to inherited protocol 'Escapable'}}
  associatedtype T
  func get() -> T
}

public struct CNEGetCopy<T: ~Escapable>: ~Escapable {
  let ne: T

  @_lifetime(copy ne)
  init(ne: T) { self.ne = ne }

  @_lifetime(copy self)
  func get() -> T { ne }
}
extension CNEGetCopy: Escapable where T: Escapable {}
extension CNEGetCopy: IGet { } // OK: Dependencies are ignored when the target is Escapable.

public struct CNEGetBorrow<T: ~Escapable>: ~Escapable {
  let e: T

  @_lifetime(borrow e)
  init(e: borrowing T) { self.e = e }

  @_lifetime(borrow self)
  func get() -> T { e }
}
extension CNEGetBorrow: Escapable where T: Escapable {}
extension CNEGetBorrow: IGet { } // OK: Dependencies are ignored when the target is Escapable.

public struct NEGet: ~Escapable {
  @_lifetime(copy self)
  func get() -> NE { fatalError() }
}
extension NEGet: IGet { } // expected-error{{type 'NEGet' does not conform to protocol 'Escapable'}}

// MARK: rdar://157801454 ([nonescapable] [typechecker] Unexpected lifetime error while conforming a method with a different lifetime annotation)
protocol P157801454 {
  @_lifetime(array1)
  func lifetimetest(array1: Int, array2: Int) -> NE // expected-note{{protocol requires function 'lifetimetest(array1:array2:)' with type '(Int, Int) -> NE'}}
}

struct S157801454 : P157801454 { // expected-error{{type 'S157801454' does not conform to protocol 'P157801454'}} expected-note {{add stubs for conformance}}
  @_lifetime(array2)
  func lifetimetest(array1: Int, array2: Int) -> NE { // expected-note{{candidate has a lifetime dependence not specified by the protocol}}
    fatalError()
  }
}

// MARK: rdar://173716835, Associated type inference with lifetimes
protocol IterProtocol<Element> {
  associatedtype Element
  mutating func next() -> Self.Element?
}

struct Iter<T: ~Escapable>: ~Escapable {
  @_lifetime(copy self)
  mutating func next() -> T? { fatalError() }
}
extension Iter: Escapable where T: Escapable {}
extension Iter: IterProtocol where T: Escapable {} // OK: Dependencies are ignored when the target is Escapable


// MARK: Simultaneous conformance to conflicting protocols
protocol CopyF: ~Escapable {
  @_lifetime(copy self)
  func f() -> NE // expected-note{{protocol requires function 'f()' with type '() -> NE'}}
}

protocol BorrowF: ~Escapable {
  @_lifetime(borrow self)
  func f() -> NE
}

struct OverloadF: ~Escapable {
  @_lifetime(borrow self)
  func f() -> NE { fatalError() } // expected-note{{candidate has a lifetime dependence not specified by the protocol}}
}
extension OverloadF: BorrowF {}
extension OverloadF: CopyF {} // expected-error{{type 'OverloadF' does not conform to protocol 'CopyF'}} expected-note{{add stubs for conformance}}

// MARK: Static protocol methods
protocol SM {
  @_lifetime(ne0: copy ne0)
  static func f(ne0: inout NE, ne1: borrowing NE)
  //expected-note@-1 {{protocol requires function 'f(ne0:ne1:)' with type '@_lifetime(ne0: copy ne0) (ne0 ne0: inout NE, ne1 ne1: borrowing NE) -> ()'}}
}
struct ExactMatchSM: SM { // OK
  @_lifetime(ne0: copy ne0)
  static func f(ne0: inout NE, ne1: borrowing NE) {}
}
struct SubsetSM: SM { // OK
  @_lifetime(ne0: immortal)
  static func f(ne0: inout NE, ne1: borrowing NE) { fatalError() }
}
struct SupersetSM: SM { // expected-error{{type 'SupersetSM' does not conform to protocol 'SM'}} expected-note{{add stubs for conformance}}
  @_lifetime(ne0: copy ne0, copy ne1)
  static func f(ne0: inout NE, ne1: borrowing NE) { fatalError() } // expected-note{{candidate has a lifetime dependence not specified by the protocol}}
}
