// REQUIRES: OS=macosx
// RUN: %target-swift-frontend -typecheck %s -F %S/Inputs/frameworks -verify -DNOT_UNDERLYING
// RUN: %target-swift-frontend -typecheck %s -F %S/Inputs/frameworks -module-name SPIContainer -import-underlying-module -verify

#if NOT_UNDERLYING
import SPIContainer
#endif

@_spi(a) public let a: SPIInterface1
@_spi(a) public let b: SPIInterface2

public let c: SPIInterface1 // expected-error{{cannot use class 'SPIInterface1' here; it is an SPI imported from 'SPIContainer'}}
public let d: SPIInterface2 // expected-error{{cannot use class 'SPIInterface2' here; it is an SPI imported from 'SPIContainer'}}

@inlinable
public func inlinableUsingSPI() {
  SharedInterface.foo() // expected-error{{class method 'foo()' cannot be used in an '@inlinable' function because it is an SPI imported from 'SPIContainer'}}
}
