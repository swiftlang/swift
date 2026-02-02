import Def

extension Super: @retroactive Hello {
  public func hello() {
    print("Hello from Ext")
  }
}

extension GenericSuperClass: @retroactive Hello {
  public func hello() {
    print("Hello from Ext")
  }
}

extension GenericStruct: @retroactive Hello {
  public func hello() {
    print("Hello from Ext")
  }
}
