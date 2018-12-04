// RUN: %target-swift-frontend -emit-sil %s | %FileCheck %s
// XFAIL: *

@_fixed_layout
public struct Vector<T> : VectorNumeric {
  public var x: T
  public var y: T

  public typealias ScalarElement = T
  public typealias Dimensionality = ()

  public init(dimensionality: (), repeating repeatedValue: T) {
    self.init(repeatedValue)
  }

  public init(_ scalar: T) {
    self.x = scalar
    self.y = scalar
  }
}

// This exists to minimize generated SIL.
@inline(never) func abort() -> Never { fatalError() }

@differentiable(reverse, adjoint: fakeAdj)
public func + <T>(lhs: Vector<T>, rhs: Vector<T>) -> Vector<T> {
  abort()
}
@differentiable(reverse, adjoint: fakeAdj)
public func - <T>(lhs: Vector<T>, rhs: Vector<T>) -> Vector<T> {
  abort()
}
@differentiable(reverse, adjoint: fakeAdj)
public func * <T>(lhs: Vector<T>, rhs: Vector<T>) -> Vector<T> {
  abort()
}

public func fakeAdj<T>(seed: Vector<T>, y: Vector<T>, lhs: Vector<T>, rhs: Vector<T>) -> (Vector<T>, Vector<T>) {
  abort()
}

public func test1() {
  func foo(_ x: Vector<Float>) -> Vector<Float> {
    return x + x
  }
  _ = #gradient(foo)
}
