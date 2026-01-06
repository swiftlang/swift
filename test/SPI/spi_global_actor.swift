// RUN: %target-typecheck-verify-swift

// REQUIRES: concurrency

@available(SwiftStdlib 5.1, *)
@_spi(Foo)
@globalActor
public struct SPIGA { // expected-note {{struct declared here}}
  public actor Actor {}
  public static let shared = Actor()
}

@available(SwiftStdlib 5.1, *)
@SPIGA // expected-error {{cannot use struct 'SPIGA' here; it is SPI}}
public struct PublicStruct {}

@available(SwiftStdlib 5.1, *)
@_spi(Foo)
@SPIGA
public struct SPIStruct {}
