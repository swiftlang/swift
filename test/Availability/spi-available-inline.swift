// REQUIRES: OS=macosx
// RUN: %target-typecheck-verify-swift -library-level api

@_spi_available(macOS 10.10, *)
@available(iOS 8.0, *)
public class MacOSSPIClass { public init() {} }

@_spi_available(iOS 8.0, *)
@available(macOS 10.10, *)
public class iOSSPIClass { public init() {} }

@available(macOS 10.10, iOS 8.0, *)
@inlinable public func foo() {
	_ = MacOSSPIClass() // expected-error {{class 'MacOSSPIClass' cannot be used in an '@inlinable' function because it is SPI}}
	_ = iOSSPIClass()
}
