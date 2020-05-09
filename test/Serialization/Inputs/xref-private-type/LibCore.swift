private class TopLevelInternalClass {}
public struct Foo {
  private var ref: TopLevelInternalClass
  
  public init() { self.ref = .init() }
}

public struct Bar {
  private class NestedInternalClass {}

  private var ref: NestedInternalClass
  
  public init() { self.ref = .init() }
}

public struct Baz {
  fileprivate class NestedInternalClass {
    fileprivate class DoublyNestedInternalClass {}
  }

  private var ref: NestedInternalClass.DoublyNestedInternalClass
  
  public init() { self.ref = .init() }
}
