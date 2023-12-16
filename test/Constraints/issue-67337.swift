// RUN: %target-typecheck-verify-swift -disable-availability-checking

protocol P<T> {
  associatedtype T
  func foo() -> any P<T>
}

struct A: P {
  typealias T = Int
  func foo() -> any P<T> {
      self
  }
}

struct B: P {
  typealias T = Int
  func foo() -> any P<T> {
      self
  }
}

struct G<T> {
  let t: T
}

let p: any P = A()
let g = G(t: p.foo())
let gg: G<any P> = g
