// RUN: %target-typecheck-verify-swift

@_spi_available(*, deprecated, renamed: "another") // expected-error {{SPI available only supports introducing version on specific platform}}
public class SPIClass1 {} // expected-warning {{symbols that are '@_spi_available' on all platforms should use '@_spi' instead}}

@_spi_available(*, unavailable) // expected-error {{SPI available only supports introducing version on specific platform}}
public class SPIClass2 {} // expected-warning {{symbols that are '@_spi_available' on all platforms should use '@_spi' instead}}

@_spi_available(AlienPlatform 5.2, *) // expected-warning {{unrecognized platform name 'AlienPlatform'}}
public class SPIClass3 {}

@_spi_available(macOS 10.4, *)
public class SPIClass4 {} // expected-warning {{symbols that are '@_spi_available' on all platforms should use '@_spi' instead}}

@_spi_available(macos 10.15, *) // expected-warning {{unrecognized platform name 'macos'; did you mean 'macOS'?}} {{17-22=macOS}}
public class SPIClass5 {}

@_spi_available(mscos 10.15, *) // expected-warning {{unrecognized platform name 'mscos'; did you mean 'macOS'?}} {{17-22=macOS}}
public class SPIClass6 {}

public class ClassWithMembers {
  @_spi_available(macOS 10.15, *)
  public func spiFunc() {} // Ok, this declaration is not top level
}
