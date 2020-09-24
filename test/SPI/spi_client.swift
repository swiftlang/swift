/// Check an SPI import of an SPI library to detect correct SPI access

// RUN: %empty-directory(%t)

// /// Compile the SPI lib
// RUN: %target-swift-frontend -emit-module %S/Inputs/spi_helper.swift -module-name SPIHelper -emit-module-path %t/SPIHelper.swiftmodule -emit-module-interface-path %t/SPIHelper.swiftinterface -emit-private-module-interface-path %t/SPIHelper.private.swiftinterface -enable-library-evolution -swift-version 5 -parse-as-library

/// Reading from swiftmodule
// RUN: %target-typecheck-verify-swift -I %t -verify-ignore-unknown

/// Reading from .private.swiftinterface
// RUN: rm %t/SPIHelper.swiftmodule
// RUN: %target-typecheck-verify-swift -I %t -verify-ignore-unknown

//// Reading from .swiftinterface should fail as it won't find the decls
// RUN: rm %t/SPIHelper.private.swiftinterface
// RUN: not %target-swift-frontend -typecheck -I %t

@_spi(HelperSPI) import SPIHelper

// Use as SPI
publicFunc()
spiFunc()
internalFunc() // expected-error {{cannot find 'internalFunc' in scope}}

let c = SPIClass()
c.spiMethod()
c.spiVar = "write"
print(c.spiVar)

let s = SPIStruct()
s.spiMethod()
s.spiInherit()
s.spiDontInherit() // expected-error {{'spiDontInherit' is inaccessible due to '@_spi' protection level}}
s.spiExtensionHidden()
s.spiExtension()
s.spiExtensionInherited()
s.spiVar = "write"
print(s.spiVar)

SPIEnum().spiMethod()
SPIEnum.A.spiMethod()

var ps = PublicStruct()
let _ = PublicStruct(alt_init: 1)
ps.spiMethod()
ps.spiVar = "write"
print(ps.spiVar)

otherApiFunc() // expected-error {{cannot find 'otherApiFunc' in scope}}

public func publicUseOfSPI(param: SPIClass) -> SPIClass {} // expected-error 2{{cannot use class 'SPIClass' here; it is an SPI imported from 'SPIHelper'}}
public func publicUseOfSPI2() -> [SPIClass] {} // expected-error {{cannot use class 'SPIClass' here; it is an SPI imported from 'SPIHelper'}}

@inlinable
public func inlinable() -> SPIClass { // expected-error {{class 'SPIClass' is '@_spi' and cannot be referenced from an '@inlinable' function}}
  spiFunc() // expected-error {{global function 'spiFunc()' is '@_spi' and cannot be referenced from an '@inlinable' function}}
  _ = SPIClass() // expected-error {{class 'SPIClass' is '@_spi' and cannot be referenced from an '@inlinable' function}}
  _ = [SPIClass]() // expected-error {{class 'SPIClass' is '@_spi' and cannot be referenced from an '@inlinable' function}}
}

@_spi(S)
@inlinable
public func inlinable() -> SPIClass {
  spiFunc()
  _ = SPIClass()
  _ = [SPIClass]()
}
