// REQUIRES: VENDOR=apple
// REQUIRES: OS=macosx
// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx11.9 -library-level api

@_spi_available(macOS 10.4, *)
public class MacOSSPIClass { public init() {} }

@_spi_available(iOS 8.0, *)
public class iOSSPIClass { public init() {} }

@inlinable public func foo() {
	_ = MacOSSPIClass() // expected-error {{class 'MacOSSPIClass' cannot be used in an '@inlinable' function because it is SPI}}
	_ = iOSSPIClass()
}
