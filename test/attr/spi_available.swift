// RUN: %target-typecheck-verify-swift

@_spi_available(*, deprecated, renamed: "another") // expected-error {{SPI available only supports introducing version on specific platform}}
public class SPIClass1 {}

@_spi_available(*, unavailable) // expected-error {{SPI available only supports introducing version on specific platform}}
public class SPIClass2 {}

@_spi_available(AlienPlatform 5.2, *) // expected-warning {{unrecognized platform name 'AlienPlatform'}}
public class SPIClass3 {}
