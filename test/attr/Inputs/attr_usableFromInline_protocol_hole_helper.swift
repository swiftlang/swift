prefix operator ^
prefix operator ^^
prefix operator ^^^

public protocol PublicProtocol {
}
extension PublicProtocol {
  public func publicExtensionMethod() {}
  @usableFromInline internal func ufiExtensionMethod() {}
  internal func internalExtensionMethod() {}

  public static prefix func ^(_: Self) {}
  @usableFromInline internal static prefix func ^^(_: Self) {}
  internal static prefix func ^^^(_: Self) {}
}

public struct PublicImpl: PublicProtocol {}


@usableFromInline internal protocol UFIProtocol {
}
extension UFIProtocol {
  public func publicExtensionMethod() {}
  @usableFromInline internal func ufiExtensionMethod() {}
  internal func internalExtensionMethod() {}

  public static prefix func ^(_: Self) {}
  @usableFromInline internal static prefix func ^^(_: Self) {}
  internal static prefix func ^^^(_: Self) {}
}

public struct UFIImpl: PublicProtocol {}
