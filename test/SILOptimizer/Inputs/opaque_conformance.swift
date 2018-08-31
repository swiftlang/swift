public protocol PublicProtocol {
  func publicRequirement()
}

private protocol PrivateProtocol : PublicProtocol {}

public struct Conformer : PrivateProtocol {}

extension PrivateProtocol {
  // This implementation is opaque to callers, since we can
  // only find it via a private protocol.
  public func publicRequirement() {}
}
