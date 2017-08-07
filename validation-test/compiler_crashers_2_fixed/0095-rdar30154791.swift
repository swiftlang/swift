// RUN: not %target-swift-frontend %s -typecheck

struct X<T> {}
struct Y<T> {}

protocol P {
  associatedtype T = X<U>
  associatedtype U

  func foo() -> T
}

protocol Q: P {
  func bar() -> T
  func bas() -> U
}

extension P {
  func foo() -> X<U> { fatalError() }
}

extension Q {
  func foo() -> Y<U> { fatalError() }
  func bar() -> Y<U> { fatalError() }
}

struct S {}

extension S {
  func bas() -> Int {}
}
extension S: Q {}

let x: Y = S().foo()

