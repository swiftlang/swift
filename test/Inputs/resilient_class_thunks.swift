public class Object {
  public init() {}
}

public class Base<T> {
  public func takesT(_: T) {}

  public func takesInt(_: Int) {}

  public func takesReference(_: Object) {}
}

public class Derived : Base<Int> {
  // Re-abstraction required, no new vtable entry
  public override func takesT(_: Int) {}

  // New vtable entry
  public override func takesInt(_: Int?) {}

  // Override has different formal type but is ABI-compatible
  public override func takesReference(_: Object?) {}
}
