// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify \
// RUN:   -import-objc-header %S/Inputs/availability_domains_bridging_header.h \
// RUN:   -I %S/../Inputs/custom-modules/availability-domains \
// RUN:   -enable-experimental-feature CustomAvailability \
// RUN:   -experimental-spi-only-imports -parse-as-library -swift-version 4 \
// RUN:   %s

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify \
// RUN:   -import-objc-header %S/Inputs/availability_domains_bridging_header.h \
// RUN:   -I %S/../Inputs/custom-modules/availability-domains \
// RUN:   -enable-experimental-feature CustomAvailability \
// RUN:   -experimental-spi-only-imports -parse-as-library -swift-version 5 \
// RUN:   %s -verify-additional-prefix swift5-

// REQUIRES: swift_feature_CustomAvailability

private import Rivers // also re-exported by Oceans
internal import Oceans
// expected-note@-1 22 {{availability domain 'Arctic' imported as 'internal' from 'Oceans' here}}
// expected-swift5-note@-2 2 {{availability domain 'Arctic' imported as 'internal' from 'Oceans' here}}
// expected-note@-3 23 {{availability domain 'Colorado' imported as 'internal' from 'Oceans' here}}
// expected-swift5-note@-4 2 {{availability domain 'Colorado' imported as 'internal' from 'Oceans' here}}
// expected-note@-5 22 {{availability domain 'Grand' imported as 'internal' from 'Oceans' here}}
// expected-swift5-note@-6 2 {{availability domain 'Grand' imported as 'internal' from 'Oceans' here}}
public import Seas
@_spiOnly import Lakes

@available(Colorado) // expected-error {{availability domain 'Colorado' is internal and cannot be used in '@available' on public global function 'publicFunc()'}}
@available(Grand) // expected-error {{availability domain 'Grand' is internal and cannot be used in '@available' on public global function 'publicFunc()'}}
// expected-warning@-1 {{availability domain 'Grand' is deprecated: Use Colorado instead}}
@available(Arctic) // expected-error {{availability domain 'Arctic' is internal and cannot be used in '@available' on public global function 'publicFunc()'}}
@available(Baltic)
@available(BayBridge)
@available(Salt) // expected-error {{cannot use availability domain 'Salt' here; 'Lakes' was imported for SPI only}}
public func publicFunc() { }

@available(Colorado) // expected-swift5-error {{availability domain 'Colorado' used in '@available' on global function 'usableFromInlineFunc()' must be '@usableFromInline' or public}}
@available(Grand) // expected-swift5-error {{availability domain 'Grand' used in '@available' on global function 'usableFromInlineFunc()' must be '@usableFromInline' or public}}
// expected-warning@-1 {{availability domain 'Grand' is deprecated: Use Colorado instead}}
@available(Arctic) // expected-swift5-error {{availability domain 'Arctic' used in '@available' on global function 'usableFromInlineFunc()' must be '@usableFromInline' or public}}
@available(Baltic)
@available(BayBridge)
@available(Salt) // expected-error {{cannot use availability domain 'Salt' here; 'Lakes' was imported for SPI only}}
@usableFromInline func usableFromInlineFunc() { }

@available(Colorado) // expected-error {{availability domain 'Colorado' is internal and cannot be used in '@available' on public global function 'spiFunc()'}}
@available(Grand) // expected-error {{availability domain 'Grand' is internal and cannot be used in '@available' on public global function 'spiFunc()'}}
// expected-warning@-1 {{availability domain 'Grand' is deprecated: Use Colorado instead}}
@available(Arctic) // expected-error {{availability domain 'Arctic' is internal and cannot be used in '@available' on public global function 'spiFunc()'}}
@available(Baltic)
@available(BayBridge)
@available(Salt)
@_spi(Private) public func spiFunc() { }

@available(Colorado)
@available(Grand) // expected-warning {{availability domain 'Grand' is deprecated: Use Colorado instead}}
@available(Arctic)
@available(Baltic)
@available(BayBridge)
@available(Salt)
func internalFunc() { }

@available(Colorado)
@available(Grand) // expected-warning {{availability domain 'Grand' is deprecated: Use Colorado instead}}
@available(Arctic)
@available(Baltic)
@available(BayBridge)
@available(Salt)
private func privateFunc() { }

@available(Colorado) // expected-error {{availability domain 'Colorado' is internal and cannot be used in '@available' on public var 'publicGlobalVar'}}
@available(Grand) // expected-error {{availability domain 'Grand' is internal and cannot be used in '@available' on public var 'publicGlobalVar'}}
// expected-warning@-1 {{availability domain 'Grand' is deprecated: Use Colorado instead}}
@available(Arctic) // expected-error {{availability domain 'Arctic' is internal and cannot be used in '@available' on public var 'publicGlobalVar'}}
@available(Baltic)
@available(BayBridge)
@available(Salt) // expected-error {{cannot use availability domain 'Salt' here; 'Lakes' was imported for SPI only}}
public var publicGlobalVar: Int {
  get { 0 }
  set { }
}

@available(Colorado) // expected-error {{availability domain 'Colorado' is internal and cannot be used in '@available' on public struct 'PublicStruct'}}
@available(Grand) // expected-error {{availability domain 'Grand' is internal and cannot be used in '@available' on public struct 'PublicStruct'}}
// expected-warning@-1 {{availability domain 'Grand' is deprecated: Use Colorado instead}}
@available(Arctic) // expected-error {{availability domain 'Arctic' is internal and cannot be used in '@available' on public struct 'PublicStruct'}}
@available(Baltic)
@available(BayBridge)
@available(Salt) // expected-error {{cannot use availability domain 'Salt' here; 'Lakes' was imported for SPI only}}
public struct PublicStruct { }

public struct PublicGenericStruct<T> {
  var value: T

  @available(Colorado) // expected-error {{availability domain 'Colorado' is internal and cannot be used in '@available' on public initializer 'init(value:)'}}
  @available(Grand) // expected-error {{availability domain 'Grand' is internal and cannot be used in '@available' on public initializer 'init(value:)'}}
  // expected-warning@-1 {{availability domain 'Grand' is deprecated: Use Colorado instead}}
  @available(Arctic) // expected-error {{availability domain 'Arctic' is internal and cannot be used in '@available' on public initializer 'init(value:)'}}
  @available(Baltic)
  @available(BayBridge)
  @available(Salt) // expected-error {{cannot use availability domain 'Salt' here; 'Lakes' was imported for SPI only}}
  public init(value: T) {
    self.value = value
  }

  @available(Colorado) // expected-error {{availability domain 'Colorado' is internal and cannot be used in '@available' on public property 'publicProperty'}}
  @available(Grand) // expected-error {{availability domain 'Grand' is internal and cannot be used in '@available' on public property 'publicProperty'}}
  // expected-warning@-1 {{availability domain 'Grand' is deprecated: Use Colorado instead}}
  @available(Arctic) // expected-error {{availability domain 'Arctic' is internal and cannot be used in '@available' on public property 'publicProperty'}}
  @available(Baltic)
  @available(BayBridge)
  @available(Salt) // expected-error {{cannot use availability domain 'Salt' here; 'Lakes' was imported for SPI only}}
  public var publicProperty: T { value }

  @available(Colorado) // expected-swift5-error {{availability domain 'Colorado' used in '@available' on property 'usableFromInlineProperty' must be '@usableFromInline' or public}}
  @available(Grand) // expected-swift5-error {{availability domain 'Grand' used in '@available' on property 'usableFromInlineProperty' must be '@usableFromInline' or public}}
  // expected-warning@-1 {{availability domain 'Grand' is deprecated: Use Colorado instead}}
  @available(Arctic) // expected-swift5-error {{availability domain 'Arctic' used in '@available' on property 'usableFromInlineProperty' must be '@usableFromInline' or public}}
  @available(Baltic)
  @available(BayBridge)
  @available(Salt) // expected-error {{cannot use availability domain 'Salt' here; 'Lakes' was imported for SPI only}}
  @usableFromInline var usableFromInlineProperty: T { value }

  public var publicPropertyWithSetter: T {
    get { value }

    @available(Colorado) // expected-error {{availability domain 'Colorado' is internal and cannot be used in '@available' on public setter for property 'publicPropertyWithSetter'}}
    @available(Grand) // expected-error {{availability domain 'Grand' is internal and cannot be used in '@available' on public setter for property 'publicPropertyWithSetter'}}
    // expected-warning@-1 {{availability domain 'Grand' is deprecated: Use Colorado instead}}
    @available(Arctic) // expected-error {{availability domain 'Arctic' is internal and cannot be used in '@available' on public setter for property 'publicPropertyWithSetter'}}
    @available(Baltic)
    @available(BayBridge)
    @available(Salt) // expected-error {{cannot use availability domain 'Salt' here; 'Lakes' was imported for SPI only}}
    set { value = newValue }
  }

  @available(Colorado) // expected-error {{availability domain 'Colorado' is internal and cannot be used in '@available' on public instance method 'publicMethod()'}}
  @available(Grand) // expected-error {{availability domain 'Grand' is internal and cannot be used in '@available' on public instance method 'publicMethod()'}}
  // expected-warning@-1 {{availability domain 'Grand' is deprecated: Use Colorado instead}}
  @available(Arctic) // expected-error {{availability domain 'Arctic' is internal and cannot be used in '@available' on public instance method 'publicMethod()'}}
  @available(Baltic)
  @available(BayBridge)
  @available(Salt) // expected-error {{cannot use availability domain 'Salt' here; 'Lakes' was imported for SPI only}}
  public func publicMethod() { }

  @available(Colorado) // expected-error {{availability domain 'Colorado' is internal and cannot be used in '@available' on public subscript 'subscript(_:)'}}
  @available(Colorado) // expected-error {{availability domain 'Colorado' is internal and cannot be used in '@available' on public subscript 'subscript(_:)'}}
  @available(Grand) // expected-error {{availability domain 'Grand' is internal and cannot be used in '@available' on public subscript 'subscript(_:)'}}
  // expected-warning@-1 {{availability domain 'Grand' is deprecated: Use Colorado instead}}
  @available(Arctic) // expected-error {{availability domain 'Arctic' is internal and cannot be used in '@available' on public subscript 'subscript(_:)'}}
  @available(Baltic)
  @available(BayBridge)
  @available(Salt) // expected-error {{cannot use availability domain 'Salt' here; 'Lakes' was imported for SPI only}}
  public subscript(indexForSubscriptInColorado: T) -> T { value }
}

@available(Colorado) // expected-error {{availability domain 'Colorado' is internal and cannot be used in '@available' on public enum 'PublicEnum'}}
@available(Grand) // expected-error {{availability domain 'Grand' is internal and cannot be used in '@available' on public enum 'PublicEnum'}}
// expected-warning@-1 {{availability domain 'Grand' is deprecated: Use Colorado instead}}
@available(Arctic) // expected-error {{availability domain 'Arctic' is internal and cannot be used in '@available' on public enum 'PublicEnum'}}
@available(Baltic)
@available(BayBridge)
@available(Salt) // expected-error {{cannot use availability domain 'Salt' here; 'Lakes' was imported for SPI only}}
public enum PublicEnum { }

public enum PublicEnumWithCase {
  @available(Colorado) // expected-error {{availability domain 'Colorado' is internal and cannot be used in '@available' on public enum case 'colorado'}}
  @available(Grand) // expected-error {{availability domain 'Grand' is internal and cannot be used in '@available' on public enum case 'colorado'}}
  // expected-warning@-1 {{availability domain 'Grand' is deprecated: Use Colorado instead}}
  @available(Arctic) // expected-error {{availability domain 'Arctic' is internal and cannot be used in '@available' on public enum case 'colorado'}}
  @available(Baltic)
  @available(BayBridge)
  @available(Salt) // expected-error {{cannot use availability domain 'Salt' here; 'Lakes' was imported for SPI only}}
  case colorado
}

@available(Colorado) // expected-error {{availability domain 'Colorado' is internal and cannot be used in '@available' on public class 'PublicClass'}}
@available(Grand) // expected-error {{availability domain 'Grand' is internal and cannot be used in '@available' on public class 'PublicClass'}}
// expected-warning@-1 {{availability domain 'Grand' is deprecated: Use Colorado instead}}
@available(Arctic) // expected-error {{availability domain 'Arctic' is internal and cannot be used in '@available' on public class 'PublicClass'}}
@available(Baltic)
@available(BayBridge)
@available(Salt) // expected-error {{cannot use availability domain 'Salt' here; 'Lakes' was imported for SPI only}}
public class PublicClass { }

@available(Colorado) // expected-error {{availability domain 'Colorado' is internal and cannot be used in '@available' on public protocol 'PublicProtocol'}}
@available(Grand) // expected-error {{availability domain 'Grand' is internal and cannot be used in '@available' on public protocol 'PublicProtocol'}}
// expected-warning@-1 {{availability domain 'Grand' is deprecated: Use Colorado instead}}
@available(Arctic) // expected-error {{availability domain 'Arctic' is internal and cannot be used in '@available' on public protocol 'PublicProtocol'}}
@available(Baltic)
@available(BayBridge)
@available(Salt) // expected-error {{cannot use availability domain 'Salt' here; 'Lakes' was imported for SPI only}}
public protocol PublicProtocol { }

public protocol PublicProtocolWithAssociatedType {
  @available(Colorado) // expected-error {{availability domain 'Colorado' is internal and cannot be used in '@available' on public associated type 'AssociatedType'}}
  @available(Grand) // expected-error {{availability domain 'Grand' is internal and cannot be used in '@available' on public associated type 'AssociatedType'}}
  // expected-warning@-1 {{availability domain 'Grand' is deprecated: Use Colorado instead}}
  @available(Arctic) // expected-error {{availability domain 'Arctic' is internal and cannot be used in '@available' on public associated type 'AssociatedType'}}
  @available(Baltic)
  @available(BayBridge)
  @available(Salt) // expected-error {{cannot use availability domain 'Salt' here; 'Lakes' was imported for SPI only}}
  associatedtype AssociatedType

  @available(Colorado) // expected-error {{availability domain 'Colorado' is internal and cannot be used in '@available' on public instance method 'requirement()'}}
  @available(Grand) // expected-error {{availability domain 'Grand' is internal and cannot be used in '@available' on public instance method 'requirement()'}}
  // expected-warning@-1 {{availability domain 'Grand' is deprecated: Use Colorado instead}}
  @available(Arctic) // expected-error {{availability domain 'Arctic' is internal and cannot be used in '@available' on public instance method 'requirement()'}}
  @available(Baltic)
  @available(BayBridge)
  @available(Salt) // expected-error {{cannot use availability domain 'Salt' here; 'Lakes' was imported for SPI only}}
  func requirement() -> AssociatedType
}

@available(Colorado) // expected-error {{availability domain 'Colorado' is internal and cannot be used in '@available' on public type alias 'PublicTypealias'}}
@available(Grand) // expected-error {{availability domain 'Grand' is internal and cannot be used in '@available' on public type alias 'PublicTypealias'}}
// expected-warning@-1 {{availability domain 'Grand' is deprecated: Use Colorado instead}}
@available(Arctic) // expected-error {{availability domain 'Arctic' is internal and cannot be used in '@available' on public type alias 'PublicTypealias'}}
@available(Baltic)
@available(BayBridge)
@available(Salt) // expected-error {{cannot use availability domain 'Salt' here; 'Lakes' was imported for SPI only}}
public typealias PublicTypealias = Int

@available(Colorado)
@available(Grand) // expected-warning {{availability domain 'Grand' is deprecated: Use Colorado instead}}
@available(Arctic)
@available(Baltic)
@available(BayBridge)
@available(Salt)
extension PublicGenericStruct { }

@available(Colorado) // expected-error {{availability domain 'Colorado' is internal and cannot be used in '@available' on public extension of generic struct 'PublicGenericStruct'}}
@available(Grand) // expected-error {{availability domain 'Grand' is internal and cannot be used in '@available' on public extension of generic struct 'PublicGenericStruct'}}
// expected-warning@-1 {{availability domain 'Grand' is deprecated: Use Colorado instead}}
@available(Arctic) // expected-error {{availability domain 'Arctic' is internal and cannot be used in '@available' on public extension of generic struct 'PublicGenericStruct'}}
@available(Baltic)
@available(BayBridge)
@available(Salt) // FIXME: Should be diangosed
public extension PublicGenericStruct { }

@available(Colorado)
@available(Grand) // expected-warning {{availability domain 'Grand' is deprecated: Use Colorado instead}}
@available(Arctic)
@available(Baltic)
@available(BayBridge)
@available(Salt)
extension PublicGenericStruct {
  func internalMethodInExtensionInColorado() { }
}

@available(Colorado) // expected-error {{cannot use availability domain 'Colorado' in an extension with public or '@usableFromInline' members; 'Rivers' was not imported publicly}}
@available(Grand) // expected-error {{cannot use availability domain 'Grand' in an extension with public or '@usableFromInline' members; 'Rivers' was not imported publicly}}
// expected-warning@-1 {{availability domain 'Grand' is deprecated: Use Colorado instead}}
@available(Arctic) // expected-error {{cannot use availability domain 'Arctic' in an extension with public or '@usableFromInline' members; 'Oceans' was not imported publicly}}
@available(Baltic)
@available(BayBridge)
// FIXME: Duplicate error
@available(Salt) // expected-error {{cannot use availability domain 'Salt' here; 'Lakes' was imported for SPI only}}
// expected-error@-1 {{cannot use availability domain 'Salt' in an extension with public or '@usableFromInline' members; 'Lakes' was imported for SPI only}}
extension PublicGenericStruct {
  public func publicMethodInExtensionInColorado() { }
}

@available(Colorado)
@available(Grand) // expected-warning {{availability domain 'Grand' is deprecated: Use Colorado instead}}
@available(Arctic)
@available(Baltic)
@available(BayBridge)
@available(Salt)
extension PublicGenericStruct where T: PublicProtocolWithAssociatedType { }

@available(Colorado) // expected-error {{cannot use availability domain 'Colorado' in an extension with conditional conformances; 'Rivers' was not imported publicly}}
@available(Grand) // expected-error {{cannot use availability domain 'Grand' in an extension with conditional conformances; 'Rivers' was not imported publicly}}
// expected-warning@-1 {{availability domain 'Grand' is deprecated: Use Colorado instead}}
@available(Arctic) // expected-error {{cannot use availability domain 'Arctic' in an extension with conditional conformances; 'Oceans' was not imported publicly}}
@available(Baltic)
@available(BayBridge)
// FIXME: Duplicate error
@available(Salt) // expected-error {{cannot use availability domain 'Salt' in an extension with conditional conformances; 'Lakes' was imported for SPI only}}
// expected-error@-1 {{cannot use availability domain 'Salt' here; 'Lakes' was imported for SPI only}}
extension PublicGenericStruct: PublicProtocol {}

@available(Colorado) // expected-error {{cannot use availability domain 'Colorado' in an extension with public or '@usableFromInline' members; 'Rivers' was not imported publicly}}
@available(Grand) // expected-error {{cannot use availability domain 'Grand' in an extension with public or '@usableFromInline' members; 'Rivers' was not imported publicly}}
// expected-warning@-1 {{availability domain 'Grand' is deprecated: Use Colorado instead}}
@available(Arctic) // expected-error {{cannot use availability domain 'Arctic' in an extension with public or '@usableFromInline' members; 'Oceans' was not imported publicly}}
@available(Baltic)
@available(BayBridge)
// FIXME: Duplicate error
@available(Salt) // expected-error {{cannot use availability domain 'Salt' here; 'Lakes' was imported for SPI only}}
// expected-error@-1 {{cannot use availability domain 'Salt' in an extension with public or '@usableFromInline' members; 'Lakes' was imported for SPI only}}
extension PublicGenericStruct: PublicProtocolWithAssociatedType {
  public func requirement() -> Int { 0 }
}

@inlinable public func inlinableFunc() {
  if #available(Colorado) { } // expected-error {{availability domain 'Colorado' is internal and cannot be referenced from an '@inlinable' function}}
  if #available(Grand) { } // expected-error {{availability domain 'Grand' is internal and cannot be referenced from an '@inlinable' function}}
  if #available(Arctic) { } // expected-error {{availability domain 'Arctic' is internal and cannot be referenced from an '@inlinable' function}}
  if #available(Baltic) { }
  if #available(BayBridge) { }
  if #available(Salt) { } // expected-error {{availability domain 'Salt' cannot be used in an '@inlinable' function because 'Lakes' was imported for SPI only}}

  @available(Colorado) // expected-error {{availability domain 'Colorado' is internal and cannot be referenced from an '@inlinable' function}}
  @available(Grand) // expected-error {{availability domain 'Grand' is internal and cannot be referenced from an '@inlinable' function}}
  @available(Arctic) // expected-error {{availability domain 'Arctic' is internal and cannot be referenced from an '@inlinable' function}}
  @available(Baltic)
  @available(BayBridge)
  @available(Salt) // expected-error {{availability domain 'Salt' cannot be used in an '@inlinable' function because 'Lakes' was imported for SPI only}}
  func nestedFunc() { }
}

public func nonInlinablePublicFunc() {
  if #available(Colorado) { }
  if #available(Grand) { } // expected-warning {{availability domain 'Grand' is deprecated: Use Colorado instead}}
  if #available(Arctic) { }
  if #available(Baltic) { }
  if #available(BayBridge) { }
  if #available(Salt) { }

  @available(Colorado)
  @available(Grand) // expected-warning {{availability domain 'Grand' is deprecated: Use Colorado instead}}
  @available(Arctic)
  @available(Baltic)
  @available(BayBridge)
  @available(Salt)
  func nestedFunc() { }
}
