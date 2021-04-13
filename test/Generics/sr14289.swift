// RUN: %target-swift-frontend -emit-ir %s

public protocol One {
    associatedtype MyType
}

public protocol Two {
    associatedtype MyType
}

public protocol Three: One, Two where MyType: Three {}

extension Three {
  public func doStuff(_: MyType.MyType.MyType) {}
}
