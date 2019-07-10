public class Object {
  public init() {}
}

public class Subclass : Object {}

open class Base<T> {
  open func takesT(_: T) {}

  open func takesInt(_: Int) {}

  open func takesReference(_: Object) {}

  open func returnsSuperclass() -> Object {
    fatalError()
  }
}

open class Derived : Base<Int> {
  // Re-abstraction required, no new vtable entry
  open override func takesT(_: Int) {}

  // New vtable entry
  open override func takesInt(_: Int?) {}

  // Override has different formal type but is ABI-compatible
  open override func takesReference(_: Object?) {}

  // Override has a more specific return type but is ABI-compatible
  open override func returnsSuperclass() -> Subclass {
    fatalError()
  }
}
