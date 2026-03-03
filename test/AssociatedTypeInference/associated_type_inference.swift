// RUN: %target-typecheck-verify-swift

protocol P0 {
  associatedtype Assoc1 : PSimple // expected-note{{ambiguous inference of associated type 'Assoc1': 'Double' vs. 'Int'}}
  // expected-note@-1{{ambiguous inference of associated type 'Assoc1': 'Double' vs. 'Int'}}
  // expected-note@-2{{unable to infer associated type 'Assoc1' for protocol 'P0'}}
  // expected-note@-3{{unable to infer associated type 'Assoc1' for protocol 'P0'}}
  func f0(_: Assoc1)
  func g0(_: Assoc1)
}

protocol PSimple { }
extension Int : PSimple { }
extension Double : PSimple { }

struct X0a : P0 { // okay: Assoc1 == Int
  func f0(_: Int) { }
  func g0(_: Int) { }
}

struct X0b : P0 { // expected-error{{type 'X0b' does not conform to protocol 'P0'}}
  func f0(_: Int) { } // expected-note{{matching requirement 'f0' to this declaration inferred associated type to 'Int'}}
  func g0(_: Double) { } // expected-note{{matching requirement 'g0' to this declaration inferred associated type to 'Double'}}
}

struct X0c : P0 { // okay: Assoc1 == Int
  func f0(_: Int) { }
  func g0(_: Float) { }
  func g0(_: Int) { }
}

struct X0d : P0 { // okay: Assoc1 == Int
  func f0(_: Int) { }
  func g0(_: Double) { } // viable, but no corresponding f0
  func g0(_: Int) { }
}

struct X0e : P0 { // expected-error{{type 'X0e' does not conform to protocol 'P0'}}
  func f0(_: Double) { } // expected-note{{matching requirement 'f0' to this declaration inferred associated type to 'Double}}
  func f0(_: Int) { } // expected-note{{matching requirement 'f0' to this declaration inferred associated type to 'Int'}}
  func g0(_: Double) { }
  func g0(_: Int) { } 
}

struct X0f : P0 { // okay: Assoc1 = Int because Float doesn't conform to PSimple
  func f0(_: Float) { }
  func f0(_: Int) { }
  func g0(_: Float) { }
  func g0(_: Int) { }
}

struct X0g : P0 { // expected-error{{type 'X0g' does not conform to protocol 'P0'}}
  func f0(_: Float) { } // expected-note{{candidate would match and infer 'Assoc1' = 'Float' if 'Float' conformed to 'PSimple'}}
  func g0(_: Float) { } // expected-note{{candidate would match and infer 'Assoc1' = 'Float' if 'Float' conformed to 'PSimple'}}
}

struct X0h<T : PSimple> : P0 {
  func f0(_: T) { }
}

extension X0h {
  func g0(_: T) { }
}

struct X0i<T : PSimple> {
}

extension X0i {
  func g0(_: T) { }
}

extension X0i : P0 { }

extension X0i {
  func f0(_: T) { }
}

// Protocol extension used to infer requirements
protocol P1 {
}

extension P1 {
  func f0(_ x: Int) { }
  func g0(_ x: Int) { }
}

struct X0j : P0, P1 { }

protocol P2 {
  associatedtype P2Assoc

  func h0(_ x: P2Assoc)
}

extension P2 where Self.P2Assoc : PSimple {
  func f0(_ x: P2Assoc) { } // expected-note{{candidate would match and infer 'Assoc1' = 'Float' if 'Float' conformed to 'PSimple'}}
  func g0(_ x: P2Assoc) { } // expected-note{{candidate would match and infer 'Assoc1' = 'Float' if 'Float' conformed to 'PSimple'}}
}

struct X0k : P0, P2 {
  func h0(_ x: Int) { }
}

struct X0l : P0, P2 { // expected-error{{type 'X0l' does not conform to protocol 'P0'}}
  func h0(_ x: Float) { }
}

// Prefer declarations in the type to those in protocol extensions
struct X0m : P0, P2 {
  func f0(_ x: Double) { }
  func g0(_ x: Double) { }
  func h0(_ x: Double) { }
}

// Inference from properties.
protocol PropertyP0 {
  associatedtype Prop : PSimple // expected-note{{unable to infer associated type 'Prop' for protocol 'PropertyP0'}}
  var property: Prop { get }
}

struct XProp0a : PropertyP0 { // okay PropType = Int
  var property: Int
}

struct XProp0b : PropertyP0 { // expected-error{{type 'XProp0b' does not conform to protocol 'PropertyP0'}}
  var property: Float // expected-note{{candidate would match and infer 'Prop' = 'Float' if 'Float' conformed to 'PSimple'}}
}

// Inference from subscripts
protocol SubscriptP0 {
  associatedtype Index
  // expected-note@-1 2 {{protocol requires nested type 'Index'}}

  associatedtype Element : PSimple
  // expected-note@-1 {{unable to infer associated type 'Element' for protocol 'SubscriptP0'}}
  // expected-note@-2 2 {{protocol requires nested type 'Element'}}

  subscript (i: Index) -> Element { get }
}

struct XSubP0a : SubscriptP0 {
  subscript (i: Int) -> Int { get { return i } }
}

struct XSubP0b : SubscriptP0 {
// expected-error@-1{{type 'XSubP0b' does not conform to protocol 'SubscriptP0'}}
  subscript (i: Int) -> Float { get { return Float(i) } } // expected-note{{candidate would match and infer 'Element' = 'Float' if 'Float' conformed to 'PSimple'}}
}

struct XSubP0c : SubscriptP0 {
// expected-error@-1 {{type 'XSubP0c' does not conform to protocol 'SubscriptP0'}} 
// expected-note@-2 {{add stubs for conformance}}
  subscript (i: Index) -> Element { get { } }
}

struct XSubP0d : SubscriptP0 {
// expected-error@-1 {{type 'XSubP0d' does not conform to protocol 'SubscriptP0'}} 
// expected-note@-2 {{add stubs for conformance}}
  subscript (i: XSubP0d.Index) -> XSubP0d.Element { get { } }
}

// Inference from properties and subscripts
protocol CollectionLikeP0 {
  associatedtype Index
  // expected-note@-1 {{protocol requires nested type 'Index'}}
  associatedtype Element
  // expected-note@-1 {{protocol requires nested type 'Element'}}

  var startIndex: Index { get }
  var endIndex: Index { get }

  subscript (i: Index) -> Element { get }
}

struct SomeSlice<T> { }

struct XCollectionLikeP0a<T> : CollectionLikeP0 {
  var startIndex: Int
  var endIndex: Int

  subscript (i: Int) -> T { get { fatalError("blah") } }

  subscript (r: Range<Int>) -> SomeSlice<T> { get { return SomeSlice() } }
}

struct XCollectionLikeP0b : CollectionLikeP0 {
// expected-error@-1 {{type 'XCollectionLikeP0b' does not conform to protocol 'CollectionLikeP0'}}
// expected-note@-2 {{add stubs for conformance}}
  var startIndex: XCollectionLikeP0b.Index
  // There was an error @-1 ("'startIndex' used within its own type"),
  // but it disappeared and doesn't seem like much of a loss.
  var startElement: XCollectionLikeP0b.Element
}

// rdar://problem/21304164
public protocol Thenable {
    associatedtype T // expected-note{{protocol requires nested type 'T'}}
    func then(_ success: (_: T) -> T) -> Self
}

public class CorePromise<U> : Thenable { // expected-error{{type 'CorePromise<U>' does not conform to protocol 'Thenable'}} expected-note {{add stubs for conformance}}
    public func then(_ success: @escaping (_ t: U, _: CorePromise<U>) -> U) -> Self {
        return self.then() { (t: U) -> U in // expected-error{{contextual closure type '(U, CorePromise<U>) -> U' expects 2 arguments, but 1 was used in closure body}}
            return success(t: t, self)
            // expected-error@-1 {{extraneous argument label 't:' in call}}
        }
    }
}

// rdar://problem/21559670
protocol P3 {
  associatedtype Assoc = Int
  associatedtype Assoc2
  func foo(_ x: Assoc2) -> Assoc?
}

protocol P4 : P3 { }

extension P4 {
  func foo(_ x: Int) -> Float? { return 0 }
}

extension P3 where Assoc == Int {
  func foo(_ x: Int) -> Assoc? { return nil }
}


struct X4 : P4 { }

// rdar://problem/21738889
protocol P5 {
  associatedtype A = Int
}

struct X5<T : P5> : P5 {
  typealias A = T.A
}

protocol P6 : P5 {
  associatedtype A : P5 = X5<Self>
}

extension P6 where A == X5<Self> { }

// rdar://problem/21774092
protocol P7 {
  associatedtype A
  associatedtype B
  func f() -> A
  func g() -> B
}

struct X7<T> { }

extension P7 {
  func g() -> X7<A> { return X7() }
}

struct Y7<T> : P7 {
  func f() -> Int { return 0 }
}

struct MyAnySequence<Element> : MySequence {
  typealias SubSequence = MyAnySequence<Element>
  func makeIterator() -> MyAnyIterator<Element> {
    return MyAnyIterator<Element>()
  }
}

struct MyAnyIterator<T> : MyIteratorType {
  typealias Element = T
}

protocol MyIteratorType {
  associatedtype Element
}

protocol MySequence {
  associatedtype Iterator : MyIteratorType
  associatedtype SubSequence

  func foo() -> SubSequence
  func makeIterator() -> Iterator
}

extension MySequence {
  func foo() -> MyAnySequence<Iterator.Element> {
    return MyAnySequence()
  }
}

struct SomeStruct<Element> : MySequence {
  let element: Element
  init(_ element: Element) {
    self.element = element
  }

  func makeIterator() -> MyAnyIterator<Element> {
    return MyAnyIterator<Element>()
  }
}

// rdar://problem/21883828 - ranking of solutions
protocol P8 {
}

protocol P9 : P8 {
}

protocol P10 {
  associatedtype A

  func foo() -> A
}

struct P8A { }
struct P9A { }

extension P8 {
  func foo() -> P8A { return P8A() }
}

extension P9 {
  func foo() -> P9A { return P9A() }
}

struct Z10 : P9, P10 {
}

func testZ10() -> Z10.A {
  var zA: Z10.A
  zA = P9A()
  return zA
}

// rdar://problem/21926788
protocol P11 {
  associatedtype A
  associatedtype B
  func foo() -> B
}

extension P11 where A == Int {
  func foo() -> Int { return 0 }
}

protocol P12 : P11 {

}

extension P12 {
  func foo() -> String { return "" }
}

struct X12 : P12 {
  typealias A = String
}

// Infinite recursion -- we would try to use the extension
// initializer's argument type of 'Dough' as a candidate for
// the associated type
protocol Cookie {
  associatedtype Dough
  // expected-note@-1 {{protocol requires nested type 'Dough'}}

  init(t: Dough)
}

extension Cookie {
  init(t: Dough?) {}
}

struct Thumbprint : Cookie {}
// expected-error@-1 {{type 'Thumbprint' does not conform to protocol 'Cookie'}}
// expected-note@-2 {{add stubs for conformance}}

// Looking through typealiases
protocol Vector {
  associatedtype Element
  typealias Elements = [Element]

  func process(elements: Elements)
}

struct Int8Vector : Vector {
  func process(elements: [Int8]) { }
}

// https://github.com/apple/swift/issues/47063

protocol P13 {
  associatedtype Arg // expected-note{{protocol requires nested type 'Arg'}}
  func foo(arg: Arg)
}

struct S13 : P13 { // expected-error{{type 'S13' does not conform to protocol 'P13'}} expected-note {{add stubs for conformance}}
  func foo(arg: inout Int) {}
}

// "Infer" associated type from generic parameter.
protocol P14 {
  associatedtype Value
}

struct P14a<Value>: P14 { }

struct P14b<Value> { }
extension P14b: P14 { }

// Associated type defaults in overridden associated types.
struct X15 { }
struct OtherX15 { }

protocol P15a {
  associatedtype A = X15
}

protocol P15b : P15a {
  associatedtype A
}

protocol P15c : P15b {
  associatedtype A
}

protocol P15d {
  associatedtype A = X15
}

protocol P15e : P15b, P15d {
  associatedtype A
}

protocol P15f {
  associatedtype A = OtherX15
}

protocol P15g: P15c, P15f {
  associatedtype A // expected-note{{protocol requires nested type 'A'}}
}


struct X15a : P15a { }
struct X15b : P15b { }
struct X15c : P15c { }
struct X15d : P15d { }

// Ambiguity.
// FIXME: Better diagnostic here?
struct X15g : P15g { } // expected-error{{type 'X15g' does not conform to protocol 'P15g'}} expected-note {{add stubs for conformance}}

// Associated type defaults in overidden associated types that require
// substitution.
struct X16<T> { }

protocol P16 {
  associatedtype A = X16<Self>
}

protocol P16a : P16 {
  associatedtype A
}

protocol P16b : P16a {
  associatedtype A
}

struct X16b : P16b { }

// Refined protocols that tie associated types to a fixed type.
protocol P17 {
  associatedtype T
}

protocol Q17 : P17 where T == Int { }

struct S17 : Q17 { }

// Typealiases from protocol extensions should not inhibit associated type
// inference.
protocol P18 {
  associatedtype A
}

protocol P19 : P18 {
  associatedtype B
}

extension P18 where Self: P19 {
  typealias A = B
}

struct X18<A> : P18 { }

// rdar://problem/16316115
protocol HasAssoc {
  associatedtype Assoc
}

struct DefaultAssoc {}

protocol RefinesAssocWithDefault: HasAssoc {
  associatedtype Assoc = DefaultAssoc
}

struct Foo: RefinesAssocWithDefault {
}

protocol P20 {
  associatedtype T // expected-note{{protocol requires nested type 'T'}}
  typealias TT = T?
}
struct S19 : P20 {  // expected-error{{type 'S19' does not conform to protocol 'P20'}} expected-note {{add stubs for conformance}}
  typealias TT = Int?
}

// rdar://problem/44777661
struct S30<T> where T : P30 {}

protocol P30 {
  static func bar()
}

protocol P31 {
  associatedtype T : P30
}

extension S30 : P31 where T : P31 {}

extension S30 {
  func foo() {
    T.bar()
  }
}

protocol P32 {
  associatedtype A
  associatedtype B
  associatedtype C

  func foo(arg: A) -> C
  var bar: B { get }
}
protocol P33 {
  associatedtype A // expected-note {{protocol requires nested type 'A'}}

  var baz: A { get }
}
protocol P34 {
  associatedtype A  // expected-note {{protocol requires nested type 'A'}}

  func boo() -> A
}
struct S31<T> {}
extension S31: P32 where T == Int {} // OK
extension S31 where T == Int {
  func foo(arg: Never) {}
}
extension S31 where T: Equatable {
  var bar: Bool { true }
}
extension S31: P33 where T == Never {} // expected-error {{type 'S31<T>' does not conform to protocol 'P33'}} expected-note {{add stubs for conformance}}
extension S31 where T == String {
  var baz: Bool { true }
}
extension S31: P34 {} // expected-error {{type 'S31<T>' does not conform to protocol 'P34'}} expected-note {{add stubs for conformance}}
extension S31 where T: P32 {
  func boo() -> Void {}
}

/** References to type parameters in type witnesses. */

// Circular reference through a fixed type witness.
protocol P35a {
  associatedtype A = Array<B> // expected-note {{protocol requires nested type 'A'}}
  associatedtype B // expected-note {{protocol requires nested type 'B'}}
}
protocol P35b: P35a where B == A {}
// expected-error@+2 {{type 'S35' does not conform to protocol 'P35a'}}
// expected-note@+1 {{add stubs for conformance}}
struct S35: P35b {}

// Circular reference through a value witness.
protocol P36a {
  associatedtype A // expected-note {{protocol requires nested type 'A'}}

  func foo(arg: A)
}
protocol P36b: P36a {
  associatedtype B = (Self) -> A // expected-note {{protocol requires nested type 'B'}}
}
// expected-error@+3 {{type 'S36' does not conform to protocol 'P36a'}}
// expected-error@+2 {{type 'S36' does not conform to protocol 'P36b'}}
// expected-note@+1 {{add stubs for conformance}}
struct S36: P36b {
  func foo(arg: Array<B>) {}
}

// Test that we can resolve abstract type witnesses that reference
// other abstract type witnesses.
protocol P37 {
  associatedtype A = Array<B>
  associatedtype B: Equatable = Never
}
struct S37: P37 {}

protocol P38a {
  associatedtype A = Never
  associatedtype B: Equatable
}
protocol P38b: P38a where B == Array<A> {}
struct S38: P38b {}

protocol P39 where A: Sequence {
  associatedtype A = Array<B>
  associatedtype B
}
struct S39<B>: P39 {}

// Test that we can handle an analogous complex case involving all kinds of
// type witness resolution.
//
// FIXME: Except there's too much circularity here, and this really can't
// work. The problem is that S40 conforms to P40c, and we can't check the
// conformance without computing the requirement signature of P40c, but the
// requirement signature computation depends on the conformance, via the
// 'D == S40<Never>' requirement.
protocol P40a {
  associatedtype A
  associatedtype B: P40a

  func foo(arg: A)
}
protocol P40b: P40a {
  associatedtype C = (A, B.A, D.D, E) -> Self // expected-note {{protocol requires nested type 'C'}}
  associatedtype D: P40b // expected-note {{protocol requires nested type 'D'}}
  associatedtype E: Equatable // expected-note {{protocol requires nested type 'E'}}
}
protocol P40c: P40b where D == S40<Never> {}
struct S40<E: Equatable>: P40c {
  // expected-error@-1 {{type 'S40<E>' does not conform to protocol 'P40b'}}
  // expected-note@-2 {{add stubs for conformance}}
  func foo(arg: Never) {}

  typealias B = Self
}

protocol P41a {
  associatedtype A
  associatedtype B

  func bar(_: B) -> A?
}
protocol P42b: P41a {
  associatedtype A
  associatedtype B

  func foo(_: A, _: B)
}
extension P42b {
  func bar(_: B) -> A? {}
}
do {
  class Conformer: P42b {
    func foo(_: Bool, _: String) {}
  }
}

// https://github.com/apple/swift/issues/55151

class GenClass<T> {}

// Inference in the adoptee
protocol P43a {
  associatedtype A
  associatedtype B: GenClass<(A, Self)>

  func foo(arg: B)
}
struct S43a: P43a {
  typealias A = Never

  func foo(arg: GenClass<(A, S43a)>) {}
}

// Inference in protocol extension
protocol P43b: P43a {}
extension P43b {
  func foo(arg: GenClass<(A, Self)>) {}
}
struct S43b: P43b {
  typealias A = Never
}

// https://github.com/apple/swift/issues/55614
// Inference when witness is an enum case

protocol P44 {
  associatedtype Bar
  static func bar(_ value: Bar) -> Self
}
enum E44: P44 {
  case bar(String) // Okay
}

protocol P45 {
  associatedtype Bar
  static var bar: Bar { get }
}
enum E45: P45 {
  case bar // Okay
}

// Fails to find the fixed type witness B == FIXME_S1<A>.
protocol FIXME_P1a {
  associatedtype A: Equatable = Never // expected-note {{protocol requires nested type 'A'}}
  associatedtype B: FIXME_P1a // expected-note {{protocol requires nested type 'B'}}
}
protocol FIXME_P1b: FIXME_P1a where B == FIXME_S1<A> {}
// expected-error@+2 {{type 'FIXME_S1<T>' does not conform to protocol 'FIXME_P1a'}}
// expected-note@+1 {{add stubs for conformance}}
struct FIXME_S1<T: Equatable>: FIXME_P1b {}
