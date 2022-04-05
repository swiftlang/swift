// REQUIRES: OS=macosx

// RUN: %target-swift-frontend -typecheck %s -F %S/Inputs/frameworks -verify -DNOT_UNDERLYING
// RUN: %target-swift-frontend -typecheck %s -F %S/Inputs/frameworks -verify -DNOT_UNDERLYING -library-level spi

// RUN: %target-swift-frontend -typecheck %s -F %S/Inputs/frameworks -module-name SPIContainer -import-underlying-module -verify
// RUN: %target-swift-frontend -typecheck %s -F %S/Inputs/frameworks -module-name SPIContainer -import-underlying-module -verify -library-level spi


#if NOT_UNDERLYING
import SPIContainer
#endif

@_spi(a) public let a: SPIInterface1
@_spi(a) public let b: SPIInterface2

public let c: SPIInterface1
public let d: SPIInterface2

@inlinable
public func inlinableUsingSPI() {
  SharedInterface.foo()
}

@available(macOS, unavailable)
public let e: SPIInterface2

@available(iOS, unavailable)
public let f: SPIInterface2

@inlinable
@available(macOS, unavailable)
public func inlinableUnavailableUsingSPI() {
  SharedInterface.foo()
}
