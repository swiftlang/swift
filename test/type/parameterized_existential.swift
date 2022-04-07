// RUN: %target-typecheck-verify-swift -requirement-machine-protocol-signatures=on -requirement-machine-inferred-signatures=on -disable-availability-checking -enable-parameterized-existential-types

protocol Sequence<Element> {
  associatedtype Element
}

struct ConcreteSequence<Element> : Sequence {}

extension Sequence {
  func map<Other>(_ transform: (Self.Element) -> Other) -> ConcreteSequence<Other> {
    return ConcreteSequence<Other>()
  }
}

protocol DoubleWide<X, Y> {
  associatedtype X
  associatedtype Y

  var x: X { get }
  var y: Y { get }
}

extension Int: DoubleWide {
  typealias X = Int
  typealias Y = Int

  var x: X { 0 }
  var y: X { 0 }
}

struct Collapse<T: DoubleWide>: DoubleWide {
  typealias X = T
  typealias Y = T

  var x: X
  var y: X { self.x }
}

func test() -> any DoubleWide<some DoubleWide<Int, Int>, some DoubleWide<Int, Int>> { return Collapse<Int>(x: 42) }

func diagonalizeAny(_ x: any Sequence<Int>) -> any Sequence<(Int, Int)> {
  return x.map { ($0, $0) }
}

func erase<T>(_ x: ConcreteSequence<T>) -> any Sequence<T> {
  return x as any Sequence<T>
}

protocol Sponge<A, B> {
  associatedtype A
  associatedtype B
}

func saturation(_ dry: any Sponge, _ wet: any Sponge<Int, Int>) {
  _ = dry as any Sponge<Int, Int>
  // expected-error@-1 {{'any Sponge' is not convertible to 'any Sponge<Int, Int>'}}
  // expected-note@-2 {{did you mean to use 'as!' to force downcast?}}
  _ = dry as any Sponge

  _ = wet as any Sponge<Int, Int> // Ok
  _ = wet as any Sponge // Ok
  _ = wet as any Sponge<String, String> // expected-error {{'any Sponge<Int, Int>' is not convertible to 'any Sponge<String, String>'}}
  // expected-note@-1 {{did you mean to use 'as!' to force downcast?}}
}
