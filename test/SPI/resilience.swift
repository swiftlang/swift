// Test limits of SPI members in frozen types.

// RUN: %target-typecheck-verify-swift

@frozen
public struct FrozenStruct {
  public var okProperty: Int

  @_spi(S)
  public var okComputedProperty: Int { get { return 42 } }

  @_spi(S) // expected-error {{stored property 'spiProperty' cannot be declared '@_spi' in a '@frozen' struct}}
  public var spiProperty: Int

  @_spi(S) // expected-error {{stored property 'spiPropertySet' cannot be declared '@_spi' in a '@frozen' struct}}
  public var spiPropertySet = 4

  @_spi(S) // expected-error {{stored property 'spiPropertyDoubleErrors' cannot be declared '@_spi' in a '@frozen' struct}}
  // expected-error @-1 {{internal property cannot be declared '@_spi' because only public and open declarations can be '@_spi'}}
  var spiPropertyDoubleErrors: Int
}

public struct UnfrozenStruct {
  @_spi(S)
  public var spiProperty: Int

  @_spi(S)
  public var spiPropertySet = 4
}
