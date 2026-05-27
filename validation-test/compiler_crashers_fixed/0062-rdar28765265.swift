// RUN: %target-swift-frontend %s -typecheck

protocol P0 {}

struct Y : P0 {}

protocol P1 {
  associatedtype A: P0 = Y
  func f() -> A
}

extension P1 {
  func f() -> A {
    fatalError()
  }
}

protocol P2 : P1 {}

struct Z<T: P1> : P0 {}

extension P2 {
  func f() -> Z<Self> {
    return Z()
  }
}

struct X : P2 {}
let s: P0 = Z<X>()
