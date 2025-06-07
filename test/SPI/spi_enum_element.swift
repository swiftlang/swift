// RUN: %target-typecheck-verify-swift -swift-version 5
// RUN: %target-typecheck-verify-swift -swift-version 5 -enable-library-evolution

public enum PublicEnum {
  case publicCase
  @_spi(S) case spiCase
}

@inlinable public func publicInlinableFunc(_ e: PublicEnum) {
  switch e {
  case .publicCase: break
  case .spiCase: break   // FIXME: this should be diagnosed with "cannot use enum case 'spiCase' here; it is SPI"
  @unknown default: break
  }

  if case .spiCase = e {} // FIXME: this should be diagnosed with "cannot use enum case 'spiCase' here; it is SPI"

  _ = PublicEnum.spiCase // expected-error {{enum case 'spiCase' cannot be used in an '@inlinable' function because it is SPI}}
}

@_spi(S)
@inlinable public func spiInlinableFunc(_ e: PublicEnum) {
  switch e {
  case .publicCase: break
  case .spiCase: break
  @unknown default: break
  }

  if case .spiCase = e {}

  _ = PublicEnum.spiCase
}

public struct PublicStruct {}
@_spi(S) public struct SPIStruct {} // expected-note {{struct declared here}}

public enum PublicEnumWithPayloads {
  case publicCasePublicPayload(_ s: PublicStruct)
  case publicCaseSPIPayload(_ s: SPIStruct) // expected-error {{cannot use struct 'SPIStruct' here; it is SPI}}
  @_spi(S) case spiCasePublicPayload(_ s: PublicStruct)
  @_spi(S) case spiCaseSPIPayload(_ s: SPIStruct)
}

@_spi(S)
public enum SPIEnumWithPayloads {
  case publicPayloadCase(_ s: PublicStruct)
  case spiPayloadCase(_ s: SPIStruct)
}
