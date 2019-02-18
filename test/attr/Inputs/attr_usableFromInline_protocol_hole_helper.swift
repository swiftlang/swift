public protocol PublicProtocol {
}
extension PublicProtocol {
  public func publicExtensionMethod() {}
  @usableFromInline internal func ufiExtensionMethod() {}
  internal func internalExtensionMethod() {}
}

public struct PublicImpl: PublicProtocol {}


@usableFromInline internal protocol UFIProtocol {
}
extension UFIProtocol {
  public func publicExtensionMethod() {}
  @usableFromInline internal func ufiExtensionMethod() {}
  internal func internalExtensionMethod() {}
}

public struct UFIImpl: PublicProtocol {}
