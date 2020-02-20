// Checks for SPI declarations and limited exposability

// RUN: %empty-directory(%t)
// RUN: %target-typecheck-verify-swift -I %t -verify-ignore-unknown -enable-library-evolution -swift-version 5

// SPI declarations
@_spi(MySPI) public func spiFunc() {} // expected-note {{global function 'spiFunc()' is not '@usableFromInline' or public}}
@_spi(+) public func invalidSPIName() {} // expected-error {{expected an SPI identifier as subject of the '@_spi' attribute}}
@_spi(🤔) public func emojiNamedSPI() {}
@_spi() public func emptyParensSPI() {} // expected-error {{expected an SPI identifier as subject of the '@_spi' attribute}}
@_spi(set) public func keywordSPI() {} // expected-error {{expected an SPI identifier as subject of the '@_spi' attribute}}

@_spi(S) public class SPIClass {} // expected-note 2 {{type declared here}}
  // expected-note @-1 2 {{class 'SPIClass' is not '@usableFromInline' or public}}
class InternalClass {} // expected-note 2 {{type declared here}}
private class PrivateClass {} // expected-note 2 {{type declared here}}

@_spi(S) public protocol SPIProtocol {} // expected-note {{type declared here}}

@_spi(S) public func useOfSPITypeOk(_ p0: SPIProtocol, p1: SPIClass) -> SPIClass { fatalError() } // OK
public func useOfSPITypeInvalid() -> SPIClass { fatalError() } // expected-error {{function cannot be declared public because its result uses an internal type}} // TODO misleading "internal" message
@_spi(S) public func spiUseOfInternalType() -> InternalClass { fatalError() } // expected-error{{function cannot be declared '@_spi' because its result uses an internal type without a compatible '@_spi'}}
@_spi(S) public func spiUseOfPrivateType(_ a: PrivateClass)  { fatalError() } // expected-error{{function cannot be declared public because its parameter uses a private type}}

@inlinable
func inlinable() -> SPIClass { // expected-error {{class 'SPIClass' is internal and cannot be referenced from an '@inlinable' function}}
  spiFunc() // expected-error {{global function 'spiFunc()' is internal and cannot be referenced from an '@inlinable' function}}
  _ = SPIClass() // expected-error {{class 'SPIClass' is internal and cannot be referenced from an '@inlinable' function}}
}

@_spi(S) public struct SPIStruct {} // expected-note 2 {{struct 'SPIStruct' is not '@usableFromInline' or public}}

@frozen public struct FrozenStruct {
  @_spi(S) public var asdf = SPIStruct() // expected-error {{struct 'SPIStruct' is internal and cannot be referenced from a property initializer in a '@frozen' type}}
  var asdf = SPIStruct() // expected-error {{struct 'SPIStruct' is internal and cannot be referenced from a property initializer in a '@frozen' type}}
}

private protocol PrivateProtocol {} // expected-note {{type declared here}}

@_spi(S) public class BadSubclass : InternalClass {} // expected-error{{class cannot be declared public because its superclass is internal}}
@_spi(S) public class OkSPISubclass : SPIClass {} // OK
public class BadPublicClass : SPIClass {} // expected-error {{class cannot be declared public because its superclass is internal}}
@_spi(S) public class BadSPIClass : PrivateClass {} // expected-error {{class cannot be declared public because its superclass is private}}

@_spi(s) public func genFunc<T: PrivateProtocol>(_ t: T) {} // expected-error {{global function cannot be declared public because its generic parameter uses a private type}}
public func genFuncBad<T: SPIProtocol>(_ t: T) {} // expected-error {{global function cannot be declared public because its generic parameter uses an internal type}}
@_spi(S) public func genFuncSPI<T: SPIProtocol>(_ t: T) {} // OK
