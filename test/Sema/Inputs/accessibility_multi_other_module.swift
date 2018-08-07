public struct PrivateConformance : PrivateProtocol, FilePrivateProtocol {}

private protocol PrivateProtocol {}

extension PrivateProtocol {
  public func publicExtensionMember() {}

  internal func internalExtensionMember() {}
}

fileprivate protocol FilePrivateProtocol {}

extension FilePrivateProtocol {
  public func publicFPExtensionMember() {}
  // expected-note@-1 {{'publicFPExtensionMember' declared here}}

  internal func internalFPExtensionMember() {}
  // expected-note@-1 {{'internalFPExtensionMember' declared here}}
}


public struct InternalConformance : InternalProtocol {}

internal protocol InternalProtocol {}

extension InternalProtocol {
  public func publicExtensionMember() {}

  internal func internalExtensionMember() {}
}
