/// Check an SPI import of an SPI library to detect correct SPI access

// RUN: %empty-directory(%t)

// /// Compile the SPI lib
// RUN: %target-swift-frontend -enable-experimental-prespecialization -emit-module %S/Inputs/spi_helper.swift -module-name SPIHelper -emit-module-path %t/SPIHelper.swiftmodule -emit-module-interface-path %t/SPIHelper.swiftinterface -emit-private-module-interface-path %t/SPIHelper.private.swiftinterface -enable-library-evolution -swift-version 5 -parse-as-library

/// Reading from swiftmodule
// RUN: %target-typecheck-verify-swift -enable-experimental-prespecialization -I %t -verify-ignore-unknown

/// Reading from .private.swiftinterface
// RUN: rm %t/SPIHelper.swiftmodule
// RUN: %target-typecheck-verify-swift -enable-experimental-prespecialization -I %t -verify-ignore-unknown

//// Reading from .swiftinterface should fail as it won't find the decls
// RUN: rm %t/SPIHelper.private.swiftinterface
// RUN: not %target-swift-frontend -enable-experimental-prespecialization -typecheck -I %t

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
public func inlinable1() -> SPIClass { // expected-error {{class 'SPIClass' cannot be used in an '@inlinable' function because it is an SPI imported from 'SPIHelper'}}
  spiFunc() // expected-error {{global function 'spiFunc()' cannot be used in an '@inlinable' function because it is an SPI imported from 'SPIHelper'}}
  _ = SPIClass() // expected-error {{class 'SPIClass' cannot be used in an '@inlinable' function because it is an SPI imported from 'SPIHelper'}}
  // expected-error@-1 {{initializer 'init()' cannot be used in an '@inlinable' function because it is an SPI imported from 'SPIHelper'}}
  _ = [SPIClass]() // expected-error {{class 'SPIClass' cannot be used in an '@inlinable' function because it is an SPI imported from 'SPIHelper'}}
}

@_spi(S)
@inlinable
public func inlinable2() -> SPIClass {
  spiFunc()
  _ = SPIClass()
  _ = [SPIClass]()
}
