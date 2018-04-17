public struct PrivateConformance : PrivateProtocol {}

private protocol PrivateProtocol {}

extension PrivateProtocol {
  public func publicExtensionMember() {}

  internal func internalExtensionMember() {}
}

public struct InternalConformance : InternalProtocol {}

internal protocol InternalProtocol {}

extension InternalProtocol {
  public func publicExtensionMember() {}

  internal func internalExtensionMember() {}
}
