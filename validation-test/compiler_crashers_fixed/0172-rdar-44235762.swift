// RUN: not %target-swift-frontend -typecheck %s

// Was crashing in associated type inference.

protocol P {
  associatedtype Assoc

  subscript(i: Int) -> Assoc { get }
  func f() -> Assoc
}

struct X<T, U> { }

extension P {
  subscript<T>(i: T) -> X<T, Self> { return X<T, Self>() }
  func f<T>() -> X<T, Self> { return X<T, Self> }
}

struct Y<T>: P { }

