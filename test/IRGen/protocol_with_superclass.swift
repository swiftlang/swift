// RUN: %target-swift-frontend -emit-ir %s

public class Base<T> {}

public protocol Proto<T>: Base<Self.T> {
  associatedtype T
}

extension Proto {
  public func f() {}
}

public class Derived<T>: Base<T>, Proto {}

public func erase() {
  let p1: any Proto = Derived<Int>()
  p1.f()

  let p2: any Proto<Int> = Derived<Int>()
  p2.f()
}
