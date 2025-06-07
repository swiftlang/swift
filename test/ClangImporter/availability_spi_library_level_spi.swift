// REQUIRES: OS=macosx

// RUN: %target-swift-frontend -typecheck %s -F %S/Inputs/frameworks -verify -DNOT_UNDERLYING -parse-as-library -require-explicit-availability=ignore
// RUN: %target-swift-frontend -typecheck %s -F %S/Inputs/frameworks -verify -DNOT_UNDERLYING -library-level spi -parse-as-library -require-explicit-availability=ignore

// RUN: %target-swift-frontend -typecheck %s -F %S/Inputs/frameworks -module-name SPIContainer -import-underlying-module -verify -parse-as-library -require-explicit-availability=ignore
// RUN: %target-swift-frontend -typecheck %s -F %S/Inputs/frameworks -module-name SPIContainer -import-underlying-module -verify -library-level spi -parse-as-library -require-explicit-availability=ignore


#if NOT_UNDERLYING
import SPIContainer
#endif

@_spi(a) public let a: SPIInterface1 = .init()
@_spi(a) public let b: SPIInterface2 = .init()

public let c: SPIInterface1 = .init()
public let d: SPIInterface2 = .init()

@inlinable
public func inlinableUsingSPI() {
  SharedInterface.foo()
}

@available(macOS, unavailable)
public let e: SPIInterface2 = .init()

@available(iOS, unavailable)
public let f: SPIInterface2 = .init()

@inlinable
@available(macOS, unavailable)
public func inlinableUnavailableUsingSPI() {
  SharedInterface.foo()
}
