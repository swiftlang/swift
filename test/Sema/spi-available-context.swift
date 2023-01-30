// REQUIRES: VENDOR=apple
// REQUIRES: OS=macosx
// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx11.9 -library-level api

@_spi_available(macOS 10.4, *)
@available(iOS 8.0, *)
public protocol Foo { }

@_spi_available(macOS 10.4, *)
@available(iOS 8.0, *)
public class Bar {
    public var foo: Foo?
}
