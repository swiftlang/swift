/// Check that SPI declarations can be exported from an SPI module.

// RUN: %empty-directory(%t)
// RUN: %target-typecheck-verify-swift -I %t -verify-ignore-unknown -enable-library-evolution -swift-version 5 -library-level spi

@_spi(S) public func spiFunc() {}
@_spi(S) public class SPIClass {
  public init() {}
}
@_spi(S) public class SPIStruct {
  public init() {}
}
@_spi(S) public protocol SPIProtocol {}
public enum PublicEnum {
  case publicCase
  @_spi(S) case spiCase
}

public func useOfSPITypeOk(_ p0: SPIProtocol, p1: SPIClass) -> SPIClass { fatalError() }

@inlinable
public func inlinable() -> SPIClass {
  spiFunc()
  _ = SPIClass()
}

@inlinable
public func inlinable(_ e: PublicEnum) {
  switch e {
  case .publicCase: break
  case .spiCase: break
  @unknown default: break
  }

  if case .spiCase = e {}

  _ = PublicEnum.spiCase
}

@frozen public struct FrozenStruct {
  public var spiInFrozen = SPIStruct()
  var spiTypeInFrozen = SPIStruct()
  private var spiTypeInFrozen1: SPIClass
}
