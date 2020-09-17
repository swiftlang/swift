// Checks for SPI declarations and limited exportability.

// RUN: %empty-directory(%t)
// RUN: %target-typecheck-verify-swift -I %t -verify-ignore-unknown -enable-library-evolution -swift-version 5

// Without -enable-library-evolution the exportability check looks at struct internal properties.
// RUN: %target-typecheck-verify-swift -I %t -verify-ignore-unknown -swift-version 5

// SPI declarations
@_spi(MySPI) public func spiFunc() {} // expected-note 2 {{global function 'spiFunc()' is not '@usableFromInline' or public}}
@_spi(+) public func invalidSPIName() {} // expected-error {{expected an SPI identifier as subject of the '@_spi' attribute}}
@_spi(🤔) public func emojiNamedSPI() {}
@_spi() public func emptyParensSPI() {} // expected-error {{expected an SPI identifier as subject of the '@_spi' attribute}}
@_spi(set) public func keywordSPI() {} // expected-error {{expected an SPI identifier as subject of the '@_spi' attribute}}

@_spi(S) public class SPIClass { // expected-note 5 {{type declared here}}
  // expected-note @-1 3 {{class 'SPIClass' is not '@usableFromInline' or public}}
  // expected-note @-2 {{class 'SPIClass' is not public}}
  public init() {}
}
class InternalClass {} // expected-note 2 {{type declared here}}
private class PrivateClass {} // expected-note 2 {{type declared here}}

@_spi(S) public protocol SPIProtocol {} // expected-note {{type declared here}}

@_spi(S) public func useOfSPITypeOk(_ p0: SPIProtocol, p1: SPIClass) -> SPIClass { fatalError() } // OK
public func useOfSPITypeInvalid() -> SPIClass { fatalError() } // expected-error {{cannot use class 'SPIClass' here; it is SPI}}
@_spi(S) public func spiUseOfInternalType() -> InternalClass { fatalError() } // expected-error{{function cannot be declared public because its result uses an internal type}}
@_spi(S) public func spiUseOfPrivateType(_ a: PrivateClass)  { fatalError() } // expected-error{{function cannot be declared public because its parameter uses a private type}}

@inlinable
func inlinable() -> SPIClass { // expected-error {{class 'SPIClass' is '@_spi' and cannot be referenced from an '@inlinable' function}}
  spiFunc() // expected-error {{global function 'spiFunc()' is '@_spi' and cannot be referenced from an '@inlinable' function}}
  _ = SPIClass() // expected-error {{class 'SPIClass' is '@_spi' and cannot be referenced from an '@inlinable' function}}
}

@_spi(S) public struct SPIStruct { // expected-note 2 {{struct 'SPIStruct' is not '@usableFromInline' or public}}
  public init() {}
}

@frozen public struct FrozenStruct {
  @_spi(S) public var spiInFrozen = SPIStruct() // expected-error {{struct 'SPIStruct' is '@_spi' and cannot be referenced from a property initializer in a '@frozen' type}}
  // expected-error @-1 {{stored property 'spiInFrozen' cannot be declared '@_spi' in a '@frozen' struct}}

  var spiTypeInFrozen = SPIStruct() // expected-error {{struct 'SPIStruct' is '@_spi' and cannot be referenced from a property initializer in a '@frozen' type}}
  private var spiTypeInFrozen1: SPIClass // expected-error {{cannot use class 'SPIClass' here; it is SPI}}
}

@_spi(S)
@frozen public struct SPIFrozenStruct {
  var spiTypeInFrozen = SPIStruct()
  private var spiTypeInFrozen1: SPIClass

  @_spi(S)
  private var privateSPIInFrozenSPI = SPIStruct()
}

private protocol PrivateProtocol {} // expected-note {{type declared here}}

@_spi(S) public class BadSubclass : InternalClass {} // expected-error{{class cannot be declared public because its superclass is internal}}
@_spi(S) public class OkSPISubclass : SPIClass {} // OK
public class BadPublicClass : SPIClass {} // expected-error {{cannot use class 'SPIClass' here; it is SPI}}
@_spi(S) public class BadSPIClass : PrivateClass {} // expected-error {{class cannot be declared public because its superclass is private}}

@_spi(s) public func genFunc<T: PrivateProtocol>(_ t: T) {} // expected-error {{global function cannot be declared public because its generic parameter uses a private type}}
public func genFuncBad<T: SPIProtocol>(_ t: T) {} // expected-error {{cannot use protocol 'SPIProtocol' here; it is SPI}}
@_spi(S) public func genFuncSPI<T: SPIProtocol>(_ t: T) {} // OK

@_spi(S) private func privateCantBeSPI() {} // expected-error{{private global function cannot be declared '@_spi' because only public and open declarations can be '@_spi'}} {{1-10=}}
@_spi(S) func internalCantBeSPI() {} // expected-error{{internal global function cannot be declared '@_spi' because only public and open declarations can be '@_spi'}} {{1-10=}}

public struct PublicStructWithProperties {
  public var a: SPIClass // expected-error {{cannot use class 'SPIClass' here; it is SPI}}
  public var b = SPIClass() // expected-error {{cannot use class 'SPIClass' here; it is SPI}}
}

@_spi(S)
@usableFromInline
func usableFromInlineFunc(_ a: SPIStruct) -> SPIStruct {
  fatalError()
}

@_spi(S)
public final class ClassWithUsables {
    @usableFromInline
    var usableFromInlineVar = SPIClass()

    @usableFromInline
    func usableFromInlineFunc(_ a: SPIStruct) -> SPIStruct {
      fatalError()
    }
}

@_spi(S)
public struct NestedParent {
    public struct Nested { }
    let nested: Nested
}

public func publicFuncWithDefaultValue(_ p: SPIClass = SPIClass()) {} // expected-error {{cannot use class 'SPIClass' here; it is SPI}}
// expected-error @-1 {{class 'SPIClass' is '@_spi' and cannot be referenced from a default argument value}}

@_spi(S)
public func spiFuncWithDefaultValue(_ p: SPIClass = SPIClass()) {}

@inlinable
public func inlinablePublic() {
  spiFunc() // expected-error {{global function 'spiFunc()' is '@_spi' and cannot be referenced from an '@inlinable' function}}
  let _ = SPIClass() // expected-error {{class 'SPIClass' is '@_spi' and cannot be referenced from an '@inlinable' function}}
}

@_spi(S)
@inlinable
public func inlinableSPI() {
  spiFunc()
  let _ = SPIClass()
}
