import A

extension AStruct {
  public struct BStruct {
    public func foo() -> Double {
      return 1.0
    }
  }
}

extension AStruct.BStruct: P where Thing == Double {
  public func bar() {}
}
