@available(macOS 10.50, *)
public func conditionallyAvailableFunction() {}

@available(macOS 10.50, *)
public var conditionallyAvailableGlobal: Int {
  get {return 0}
  set {}
}

@available(macOS 10.50, *)
public struct ConditionallyAvailableStruct {
  public func conditionallyAvailableMethod() {}
}

public protocol AlwaysAvailableProtocol {}

public struct AlwaysAvailableStruct {}

@available(macOS 10.50, *)
extension AlwaysAvailableStruct : AlwaysAvailableProtocol {}

public enum AlwaysAvailableEnum {
  case alwaysAvailableCase

  @available(macOS 10.50, *)
  case conditionallyAvailableCase
}