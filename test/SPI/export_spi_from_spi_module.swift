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

public func useOfSPITypeOk(_ p0: SPIProtocol, p1: SPIClass) -> SPIClass { fatalError() }

@inlinable
func inlinable() -> SPIClass {
  spiFunc()
  _ = SPIClass()
}

@frozen public struct FrozenStruct {
  public var spiInFrozen = SPIStruct()
  var spiTypeInFrozen = SPIStruct()
  private var spiTypeInFrozen1: SPIClass
}
