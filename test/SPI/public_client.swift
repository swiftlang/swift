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
spiFunc() // expected-error {{use of unresolved identifier}}
internalFunc() // expected-error {{use of unresolved identifier}}

let c = SPIClass() // expected-error {{use of unresolved identifier}}
let s = SPIStruct() // expected-error {{use of unresolved identifier}}
SPIEnum().spiMethod() // expected-error {{use of unresolved identifier}}

var ps = PublicStruct()
let _ = PublicStruct(alt_init: 1) // expected-error {{argument passed to call that takes no arguments}}
ps.spiMethod() // expected-error {{'spiMethod' is inaccessible due to '@_spi' protection level}} // TODO SPI specific diagnostics and suggest SPI to import
ps.spiVar = "write" // expected-error {{'spiVar' is inaccessible due to '@_spi' protection level}}
print(ps.spiVar) // expected-error {{'spiVar' is inaccessible due to '@_spi' protection level}}

otherApiFunc() // expected-error {{use of unresolved identifier}}

public func publicUseOfSPI(param: SPIClass) -> SPIClass {} // expected-error 2{{use of undeclared type}}
public func publicUseOfSPI2() -> [SPIClass] {} // expected-error {{use of undeclared type}}

@inlinable
func inlinable() -> SPIClass { // expected-error {{use of undeclared type}}
  spiFunc() // expected-error {{use of unresolved identifier}}
  _ = SPIClass() // expected-error {{use of unresolved identifier}}
}
