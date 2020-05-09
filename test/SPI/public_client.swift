// Check a normal import of an SPI library to detect SPI leaks

// RUN: %empty-directory(%t)

/// Compile the SPI lib
// RUN: %target-swift-frontend -emit-module %S/Inputs/spi_helper.swift -module-name SPIHelper -emit-module-path %t/SPIHelper.swiftmodule -emit-module-interface-path %t/SPIHelper.swiftinterface -emit-private-module-interface-path %t/SPIHelper.private.swiftinterface -enable-library-evolution -swift-version 5 -parse-as-library

/// Reading from swiftmodule
// RUN: %target-typecheck-verify-swift -I %t -verify-ignore-unknown

/// Reading from .private.swiftinterface
// RUN: rm %t/SPIHelper.swiftmodule
// RUN: %target-typecheck-verify-swift -I %t -verify-ignore-unknown

/// Reading from .swiftinterface should still produce the same failures
// RUN: rm %t/SPIHelper.private.swiftinterface
// RUN: not %target-swift-frontend -typecheck -I %t

import SPIHelper

// Use the public API
publicFunc()
spiFunc() // expected-error {{cannot find 'spiFunc' in scope}}
internalFunc() // expected-error {{cannot find 'internalFunc' in scope}}

let c = SPIClass() // expected-error {{cannot find 'SPIClass' in scope}}
let s = SPIStruct() // expected-error {{cannot find 'SPIStruct' in scope}}
SPIEnum().spiMethod() // expected-error {{cannot find 'SPIEnum' in scope}}

var ps = PublicStruct()
let _ = PublicStruct(alt_init: 1) // expected-error {{argument passed to call that takes no arguments}}
ps.spiMethod() // expected-error {{'spiMethod' is inaccessible due to '@_spi' protection level}} // TODO SPI specific diagnostics and suggest SPI to import
ps.spiVar = "write" // expected-error {{'spiVar' is inaccessible due to '@_spi' protection level}}
print(ps.spiVar) // expected-error {{'spiVar' is inaccessible due to '@_spi' protection level}}

otherApiFunc() // expected-error {{cannot find 'otherApiFunc' in scope}}

public func publicUseOfSPI(param: SPIClass) -> SPIClass {} // expected-error 2{{cannot find type 'SPIClass' in scope}}
public func publicUseOfSPI2() -> [SPIClass] {} // expected-error {{cannot find type 'SPIClass' in scope}}

@inlinable
func inlinable() -> SPIClass { // expected-error {{cannot find type 'SPIClass' in scope}}
  spiFunc() // expected-error {{cannot find 'spiFunc' in scope}}
  _ = SPIClass() // expected-error {{cannot find 'SPIClass' in scope}}
}
