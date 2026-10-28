public class Box {
  public init() {}
}

public struct S {
  public var value = 0
  public var box = Box()
  public init() {}
}

public func owned(_ s: S) -> yield Box { yield s.box }
public func borrowed(_ s: S) -> yield borrow Int { yield s.value }
public func mutable(_ s: inout S) -> yield inout Int { yield &s.value }
