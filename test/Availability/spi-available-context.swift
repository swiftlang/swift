// REQUIRES: OS=macosx
// RUN: %target-typecheck-verify-swift -library-level api

@_spi_available(macOS 10.10, *)
@available(iOS 8.0, *)
public protocol MacOSSPIProto {} // expected-note {{protocol declared here}}

@_spi_available(iOS 8.0, *)
@available(macOS 10.10, *)
public protocol iOSSPIProto {}

@_spi_available(macOS 10.10, *)
@available(iOS 8.0, *)
public class Bar {
  public var macos: MacOSSPIProto?
  public var ios: iOSSPIProto?
}

@available(macOS 10.10, iOS 8.0, *)
public class Baz {
  public var macos: MacOSSPIProto? // expected-error {{cannot use protocol 'MacOSSPIProto' here; it is SPI}}
  public var ios: iOSSPIProto?
}
