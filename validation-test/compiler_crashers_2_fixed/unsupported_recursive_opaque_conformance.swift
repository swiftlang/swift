// RUN: %target-swift-frontend -disable-availability-checking -emit-ir -enable-parameterized-existential-types %s

protocol P<X, Y> {
  associatedtype X : P
  associatedtype Y : P

  var x: X { get }
  var y: Y { get }
}

extension Int: P {
  typealias X = Int
  typealias Y = Int

  var x: X { 0 }
  var y: X { 0 }
}

struct Foo<T: P>: P {
  typealias X = T
  typealias Y = T

  var x: X
  var y: X { self.x }
}

func test() -> any P<some P<any P, any P>, some P<any P, any P>> { return Foo<Int>(x: 42) }
