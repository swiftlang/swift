// RUN: %target-swift-frontend -emit-ir %s

public func foo<T : P & Q>(_: T, _: S<T>.A) {}

public protocol P {
  associatedtype A

  func foo() -> A
}

public protocol Q {
  associatedtype A

  func bar() -> A
}

public struct S<T> {}

extension S : P where T : P {
  public func foo() -> Int {
    return 0
  }
}

extension S : Q where T : Q {
  public func bar() -> Int {
    return 0
  }
}
