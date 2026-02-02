// RUN: %empty-directory(%t)
// RUN: %target-typecheck-verify-swift \
// RUN:   -enable-library-evolution -swift-version 5 \
// RUN:   -library-level=api -require-explicit-availability=ignore

// REQUIRES: OS=macosx

@_spi_available(macOS, introduced: 12.0) @available(iOS 12.0, *)
public struct SPIAvailableType { // expected-note {{struct declared here}}
    public init() {}
}

@_spi(S)
public struct NormalSPIType {
    public init() {}
}

// SPI available in SPI available should be accepted.

@_spi_available(macOS, introduced: 12.0) @available(iOS 12.0, *)
public struct OtherSPIAvailableType {
    public func foo(s: SPIAvailableType) {}
}

extension OtherSPIAvailableType {
    public func bar(s: SPIAvailableType) {} // expected-error {{cannot use struct 'SPIAvailableType' here; it is SPI}} // FIXME We should allow this.
}

// Normal SPI in normal SPI should be accepted.

@_spi(S)
public struct OtherNormalSPIType {
    public func foo(s: SPIAvailableType) {}
}

extension OtherNormalSPIType {
    public func bar2(s: SPIAvailableType) {}
}

// Normal SPI in SPI available should be rejected.

@_spi_available(macOS, introduced: 12.0) @available(iOS 12.0, *)
public func SPIAvailableToSPI(s: NormalSPIType) {} // FIXME This should be an error

@inlinable
@_spi_available(macOS, introduced: 12.0) @available(iOS 12.0, *)
public func inlinableSPIAvailable() {
 let _: NormalSPIType = NormalSPIType() // FIXME There should be many errors here
}

// SPI available in normal SPI is currently accepted.

@_spi(S)
public func SPIToSPIAvailable(s: NormalSPIType) {}

@inlinable
@_spi(S)
public func inlinableSPI() {
 let _: SPIAvailableType = SPIAvailableType()
}
