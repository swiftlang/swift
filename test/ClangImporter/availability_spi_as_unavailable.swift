// REQUIRES: OS=macosx
// RUN: %target-swift-frontend -typecheck %s -F %S/Inputs/frameworks -verify -DNOT_UNDERLYING -library-level api -parse-as-library -require-explicit-availability=ignore
// RUN: %target-swift-frontend -typecheck %s -F %S/Inputs/frameworks -module-name SPIContainer -import-underlying-module -verify -library-level api -parse-as-library  -require-explicit-availability=ignore

#if NOT_UNDERLYING
import SPIContainer
#endif

@_spi(a) public let a: SPIInterface1 = .init()
@_spi(a) public let b: SPIInterface2 = .init()

public let c: SPIInterface1 = .init() // expected-error{{cannot use class 'SPIInterface1' here; it is an SPI imported from 'SPIContainer'}}
public let d: SPIInterface2 = .init() // expected-error{{cannot use class 'SPIInterface2' here; it is an SPI imported from 'SPIContainer'}}

@inlinable
public func inlinableUsingSPI() {
  SharedInterface.foo() // expected-error{{class method 'foo()' cannot be used in an '@inlinable' function because it is an SPI imported from 'SPIContainer'}}
}

@available(macOS, unavailable)
public let e: SPIInterface2 = .init()

@available(iOS, unavailable)
public let f: SPIInterface2 = .init() // expected-error{{cannot use class 'SPIInterface2' here; it is an SPI imported from 'SPIContainer'}}

@inlinable
@available(macOS, unavailable)
public func inlinableUnavailableUsingSPI() {
  SharedInterface.foo()
}
