// RUN: %target-parse-verify-swift

infix operator +++

protocol ConcatToAnything {
  static func +++ <T>(lhs: Self, other: T)
}

func min<T : Comparable>(_ x: T, y: T) -> T {
  if y < x { return y }
  return x
}

func weirdConcat<T : ConcatToAnything, U>(_ t: T, u: U) {
  t +++ u
  t +++ 1
  u +++ t // expected-error{{argument type 'U' does not conform to expected type 'ConcatToAnything'}}
}

// Make sure that the protocol operators don't get in the way.
var b1, b2 : Bool
_ = b1 != b2

extension UnicodeScalar {
  func isAlpha2() -> Bool {
    return (self >= "A" && self <= "Z") || (self >= "a" && self <= "z")
  }
}

protocol P {
  static func foo(_ arg: Self) -> Self
}
struct S : P {
  static func foo(_ arg: S) -> S {
    return arg
  }
}

func foo<T : P>(_ arg: T) -> T {
  return T.foo(arg)
}

// Associated types and metatypes
protocol SomeProtocol {
  associatedtype SomeAssociated
}

func generic_metatypes<T : SomeProtocol>(_ x: T)
  -> (T.Type, T.SomeAssociated.Type)
{
  return (type(of: x), type(of: x).SomeAssociated.self)
}

// Inferring a variable's type from a call to a generic.
struct Pair<T, U> { }

func pair<T, U>(_ x: T, _ y: U) -> Pair<T, U> { }

var i : Int, f : Float
var p = pair(i, f)

// Conformance constraints on static variables.
func f1<S1 : Sequence>(_ s1: S1) {}
var x : Array<Int> = [1]
f1(x)

// Inheritance involving generics and non-generics.
class X {
  func f() {}
}

class Foo<T> : X { 
  func g() { }
}

class Y<U> : Foo<Int> {
}

func genericAndNongenericBases(_ x: Foo<Int>, y: Y<()>) {
  x.f()
  y.f()
  y.g()
}

func genericAndNongenericBasesTypeParameter<T : Y<()>>(_ t: T) {
  t.f()
  t.g()
}

protocol P1 {}
protocol P2 {}

func foo<T : P1>(_ t: T) -> P2 {
  return t // expected-error{{return expression of type 'T' does not conform to 'P2'}}
}

func foo2(_ p1: P1) -> P2 {
  return p1 // expected-error{{return expression of type 'P1' does not conform to 'P2'}}
}

// <rdar://problem/14005696>
protocol BinaryMethodWorkaround {
  associatedtype MySelf
}

protocol Squigglable : BinaryMethodWorkaround {
}

infix operator ~~~

func ~~~ <T : Squigglable>(lhs: T, rhs: T) -> Bool where T.MySelf == T {
  return true
}

extension UInt8 : Squigglable {
  typealias MySelf = UInt8
}

var rdar14005696 : UInt8
_ = rdar14005696 ~~~ 5

// <rdar://problem/15168483>
public struct SomeIterator<C: Collection, Indices: Sequence>
  : IteratorProtocol, Sequence
  where C.Index == Indices.Iterator.Element {
  var seq : C
  var indices : Indices.Iterator

  public typealias Element = C.Iterator.Element

  public mutating func next() -> Element? {
    fatalError()
  }

  public init(elements: C, indices: Indices) {
    fatalError()
  }
}
func f1<T>(seq: Array<T>) {
  let x = (seq.indices).lazy.reversed()
  SomeIterator(elements: seq, indices: x) // expected-warning{{unused}}
  SomeIterator(elements: seq, indices: seq.indices.reversed()) // expected-warning{{unused}}
}


// <rdar://problem/16078944>
func count16078944<T>(_ x: Range<T>) -> Int { return 0 }

func test16078944 <T: Comparable>(lhs: T, args: T) -> Int {
  return count16078944(lhs..<args) // don't crash
}


// <rdar://problem/22409190> QoI: Passing unsigned integer to ManagedBuffer elements.destroy()
class r22409190ManagedBuffer<Value, Element> {
  final var value: Value { get {} set {}}
  func withUnsafeMutablePointerToElements<R>(
    _ body: (UnsafeMutablePointer<Element>) -> R) -> R {
  }
}
class MyArrayBuffer<Element>: r22409190ManagedBuffer<UInt, Element> {
  deinit {
    self.withUnsafeMutablePointerToElements { elems -> Void in
      elems.deinitialize(count: self.value)  // expected-error {{cannot convert value of type 'UInt' to expected argument type 'Int'}}
    }
  }
}

// <rdar://problem/22459135> error: 'print' is unavailable: Please wrap your tuple argument in parentheses: 'print((...))'
func r22459135() {
  func h<S : Sequence>(_ sequence: S) -> S.Iterator.Element
    where S.Iterator.Element : Integer {
    return 0
  }

  func g(_ x: Any) {}
  func f(_ x: Int) {
    g(h([3]))
  }

  func f2<TargetType: AnyObject>(_ target: TargetType, handler: @escaping (TargetType) -> ()) {
    let _: (AnyObject) -> () = { internalTarget in
      handler(internalTarget as! TargetType)
    }
  }
}


// <rdar://problem/19710848> QoI: Friendlier error message for "[] as Set"
// <rdar://problem/22326930> QoI: "argument for generic parameter 'Element' could not be inferred" lacks context
_ = [] as Set  // expected-error {{generic parameter 'Element' could not be inferred in cast to 'Set<_>}}


//<rdar://problem/22509125> QoI: Error when unable to infer generic archetype lacks greatness
func r22509125<T>(_ a : T?) { // expected-note {{in call to function 'r22509125'}}
  r22509125(nil) // expected-error {{generic parameter 'T' could not be inferred}}
}


// <rdar://problem/24267414> QoI: error: cannot convert value of type 'Int' to specified type 'Int'
struct R24267414<T> {  // expected-note {{'T' declared as parameter to type 'R24267414'}}
  static func foo() -> Int {}
}
var _ : Int = R24267414.foo() // expected-error {{generic parameter 'T' could not be inferred}}


// https://bugs.swift.org/browse/SR-599
func SR599<T: Integer>() -> T.Type { return T.self }  // expected-note {{in call to function 'SR599'}}
_ = SR599()         // expected-error {{generic parameter 'T' could not be inferred}}




// <rdar://problem/19215114> QoI: Poor diagnostic when we are unable to infer type
protocol Q19215114 {}
protocol P19215114 {}

// expected-note @+1 {{in call to function 'body9215114'}}
func body9215114<T: P19215114, U: Q19215114>(_ t: T) -> (_ u: U) -> () {}

func test9215114<T: P19215114, U: Q19215114>(_ t: T) -> (U) -> () {
  //Should complain about not being able to infer type of U.
  let f = body9215114(t)  // expected-error {{generic parameter 'T' could not be inferred}}
  return f
}

// <rdar://problem/21718970> QoI: [uninferred generic param] cannot invoke 'foo' with an argument list of type '(Int)'
class Whatever<A: IntegerArithmetic, B: IntegerArithmetic> {  // expected-note 2 {{'A' declared as parameter to type 'Whatever'}}
  static func foo(a: B) {}
  
  static func bar() {}

}
Whatever.foo(a: 23) // expected-error {{generic parameter 'A' could not be inferred}}

// <rdar://problem/21718955> Swift useless error: cannot invoke 'foo' with no arguments
Whatever.bar()  // expected-error {{generic parameter 'A' could not be inferred}}

// <rdar://problem/27515965> Type checker doesn't enforce same-type constraint if associated type is Any
protocol P27515965 {
  associatedtype R
  func f() -> R
}

struct S27515965 : P27515965 {
  func f() -> Any { return self }
}

struct V27515965 {
  init<T : P27515965>(_ tp: T) where T.R == Float {}
}

func test(x: S27515965) -> V27515965 {
  return V27515965(x) // expected-error {{generic parameter 'T' could not be inferred}}
}
