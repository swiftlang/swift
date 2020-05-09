// RUN: not %target-swift-frontend %s -typecheck

protocol DC {
  init()
}

protocol P {
  associatedtype A: DC

  func f() -> A
}

protocol Q: P {
  associatedtype A
}

extension Q {
  func f() -> A { return A() }
}

struct X<T> { }

extension X: P where T: P {
  typealias A = T.A
}

extension X: Q where T: Q {
}
