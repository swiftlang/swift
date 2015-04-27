// RUN: %target-parse-verify-swift

infix operator +++ {}

protocol ConcatToAnything {
  func +++ <T>(lhs: Self, other: T)
}

func min<T : Comparable>(x: T, y: T) -> T {
  if y < x { return y }
  return x
}

func weirdConcat<T : ConcatToAnything, U>(t: T, u: U) {
  t +++ u
  t +++ 1
  u +++ t // expected-error{{cannot pass 'let' value 'u' to mutating binary operator '+++'}}
}

// Make sure that the protocol operators don't get in the way.
var b1, b2 : Bool
b1 != b2

extension UnicodeScalar {
  func isAlpha2() -> Bool {
    return (self >= "A" && self <= "Z") || (self >= "a" && self <= "z")
  }
}

protocol P {
  static func foo(arg: Self) -> Self
}
struct S : P {
  static func foo(arg: S) -> S {
    return arg
  }
}

func foo<T : P>(arg: T) -> T {
  return T.foo(arg)
}

// Associated types and metatypes
protocol SomeProtocol {
  typealias SomeAssociated
}

func generic_metatypes<T : SomeProtocol>(x: T)
  -> (T.Type, T.SomeAssociated.Type)
{
  return (x.dynamicType, x.dynamicType.SomeAssociated.self)
}

// Inferring a variable's type from a call to a generic.
struct Pair<T, U> { }

func pair<T, U>(x: T, _ y: U) -> Pair<T, U> { }

var i : Int, f : Float
var p = pair(i, f)

// Conformance constraints on static variables.
func f1<S1 : SequenceType>(s1: S1) {}
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

func genericAndNongenericBases(x: Foo<Int>, y: Y<()>) {
  x.f()
  y.f()
  y.g()
}

func genericAndNongenericBasesTypeParameter<T : Y<()>>(t: T) {
  t.f()
  t.g()
}

protocol P1 {}
protocol P2 {}

func foo<T : P1>(t: T) -> P2 {
  return t // expected-error{{type 'T' does not conform to protocol 'P2'}}
}

func foo2(p1: P1) -> P2 {
  return p1 // expected-error{{type 'P1' does not conform to protocol 'P2'}}
}

// <rdar://problem/14005696>
protocol BinaryMethodWorkaround {
  typealias MySelf
}

protocol Squigglable : BinaryMethodWorkaround {
}

infix operator ~~~ { }

func ~~~ <T : Squigglable where T.MySelf == T>(lhs: T, rhs: T) -> Bool {
  return true
}

extension UInt8 : Squigglable {
  typealias MySelf = UInt8
}

var rdar14005696 : UInt8
rdar14005696 ~~~ 5

// <rdar://problem/15168483>
func f1<
  S: CollectionType 
  where S.Index: BidirectionalIndexType
>(seq: S) {
  let x = lazy(indices(seq)).reverse()
  PermutationGenerator(elements: seq, indices: x)
  PermutationGenerator(elements: seq, indices: lazy(indices(seq)).reverse())
}

// <rdar://problem/16078944>
func count16078944<C: CollectionType>(x: C) -> Int { return 0 }

func test16078944 <T: ForwardIndexType>(lhs: T, args: T) -> Int {
    return count16078944(lhs..<args) // don't crash
}
