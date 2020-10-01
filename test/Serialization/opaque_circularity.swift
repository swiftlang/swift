// RUN: %target-swiftc_driver -Xfrontend -disable-availability-checking -emit-module %s

// rdar://problem/49829836

public protocol P {
  associatedtype Assoc: Collection
  func foo() -> Assoc
}

public struct S : P {
  public func foo() -> some Collection {
    return [1,2,3]
  }
}
