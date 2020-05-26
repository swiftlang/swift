import A
import B

extension AStruct.BStruct {
  public struct CStruct: P {
    public func foo() -> UInt8 {
      return 0
    }
  }
}

extension AStruct.BStruct {
  public func baz() {}
}

extension AStruct.BStruct.CStruct where Thing: Equatable {
  public func baz() {}
}
