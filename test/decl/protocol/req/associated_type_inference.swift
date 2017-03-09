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
  func f0(_: Float) { } // expected-note{{inferred type 'Float' (by matching requirement 'f0') is invalid: does not conform to 'PSimple'}}
  func g0(_: Float) { } // expected-note{{inferred type 'Float' (by matching requirement 'g0') is invalid: does not conform to 'PSimple'}}
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
  func f0(_ x: P2Assoc) { } // expected-note{{inferred type 'Float' (by matching requirement 'f0') is invalid: does not conform to 'PSimple'}}
  func g0(_ x: P2Assoc) { } // expected-note{{inferred type 'Float' (by matching requirement 'g0') is invalid: does not conform to 'PSimple'}}
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
  var property: Float // expected-note{{inferred type 'Float' (by matching requirement 'property') is invalid: does not conform to 'PSimple'}}
}

// Inference from subscripts
protocol SubscriptP0 {
  associatedtype Index
  associatedtype Element : PSimple // expected-note{{unable to infer associated type 'Element' for protocol 'SubscriptP0'}}

  subscript (i: Index) -> Element { get }
}

struct XSubP0a : SubscriptP0 {
  subscript (i: Int) -> Int { get { return i } }
}

struct XSubP0b : SubscriptP0 { // expected-error{{type 'XSubP0b' does not conform to protocol 'SubscriptP0'}}
  subscript (i: Int) -> Float { get { return Float(i) } } // expected-note{{inferred type 'Float' (by matching requirement 'subscript') is invalid: does not conform to 'PSimple'}}
}

// Inference from properties and subscripts
protocol CollectionLikeP0 {
  associatedtype Index
  associatedtype Element

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

// rdar://problem/21304164
public protocol Thenable {
    associatedtype T // expected-note{{protocol requires nested type 'T'}}
    func then(_ success: (_: T) -> T) -> Self
}

public class CorePromise<T> : Thenable { // expected-error{{type 'CorePromise<T>' does not conform to protocol 'Thenable'}}
    public func then(_ success: @escaping (_ t: T, _: CorePromise<T>) -> T) -> Self {
        return self.then() { (t: T) -> T in
            return success(t: t, self)
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
  // expected-note@-1 {{protocol requires nested type 'Dough'; do you want to add it?}}

  init(t: Dough)
}

extension Cookie {
  init(t: Dough?) {}
}

struct Thumbprint : Cookie {}
// expected-error@-1 {{type 'Thumbprint' does not conform to protocol 'Cookie'}}

// Looking through typealiases
protocol Vector {
  associatedtype Element
  typealias Elements = [Element]

  func process(elements: Elements)
}

struct Int8Vector : Vector {
  func process(elements: [Int8]) { }
}
