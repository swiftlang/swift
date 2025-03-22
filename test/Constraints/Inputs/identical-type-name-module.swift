public protocol SomeProtocol {}

public struct SomeStruct: SomeProtocol {
  public init() {}
}

public func returnsOpaqueInstance() -> some SomeProtocol {
  SomeStruct()
}
