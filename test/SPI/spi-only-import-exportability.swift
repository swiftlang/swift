/// Test @_spiOnly exportability type-checking

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// Generate dependencies.
// RUN: %target-swift-frontend -emit-module %t/PublicLib.swift \
// RUN:   -module-name PublicLib -emit-module-path %t/PublicLib.swiftmodule \
// RUN:   -swift-version 5 -enable-library-evolution
// RUN: %target-swift-frontend -emit-module %t/SPIOnlyImportedLib.swift \
// RUN:   -module-name SPIOnlyImportedLib \
// RUN:   -emit-module-path %t/SPIOnlyImportedLib.swiftmodule \
// RUN:   -swift-version 5 -enable-library-evolution -I %t

/// Test the client.
// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t -verify \
// RUN:   -experimental-spi-only-imports
// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t -verify \
// RUN:   -experimental-spi-only-imports \
// RUN:   -enable-library-evolution

/// Generate the swiftinterface of the working code and verify it.
// RUN: %target-swift-emit-module-interfaces(%t/Client.swiftinterface, %t/Client.private.swiftinterface) \
// RUN:   %t/Client.swift -I %t -DSKIP_ERRORS \
// RUN:   -experimental-spi-only-imports
// RUN: %target-swift-typecheck-module-from-interface(%t/Client.swiftinterface) -I %t
// RUN: %target-swift-typecheck-module-from-interface(%t/Client.private.swiftinterface) -I %t


//--- PublicLib.swift

public struct PublicType {
    public init() {}
}

public protocol PublicProtocol {}
public func conformanceUse(_ a: PublicProtocol) {}

@propertyWrapper
public struct PublicPropertyWrapper<T> {
  public var wrappedValue: T

  public init(wrappedValue value: T) { self.wrappedValue = value }
  public init(_ value: T) { self.wrappedValue = value }
}

//--- SPIOnlyImportedLib.swift
import PublicLib

public func spiOnlyFunc() -> String { fatalError() }

public protocol SPIOnlyProto {
}

public struct SPIOnlyStruct {
    public init() {}
    public func structMethod() {}
}

public class SPIOnlyClass<P: PublicProtocol> {
    public init(p: P) {}
    public func classMethod() {}
}

public enum SPIOnlyEnum {
    case A
    case B
    public func enumMethod() {}
}

extension PublicType {
    public func spiOnlyExtensionMethod() {}
}

extension PublicType : PublicProtocol {}

@propertyWrapper
public struct SPIOnlyPropertyWrapper<T> {
  public var wrappedValue: T

  public init(wrappedValue value: T) { self.wrappedValue = value }
  public init(_ value: T) { self.wrappedValue = value }
}

//--- Client.swift

import PublicLib
@_spiOnly import SPIOnlyImportedLib

#if !SKIP_ERRORS
public func publicUser<T: SPIOnlyProto>(_ a: SPIOnlyStruct, t: T) -> SPIOnlyStruct { fatalError() }
// expected-error @-1 2 {{cannot use struct 'SPIOnlyStruct' here; 'SPIOnlyImportedLib' was imported for SPI only}}
// expected-error @-2 {{cannot use protocol 'SPIOnlyProto' here; 'SPIOnlyImportedLib' was imported for SPI only}}
#endif

@_spi(X) public func spiUser(_ a: SPIOnlyStruct) -> SPIOnlyStruct { fatalError() }

internal func internalUser(_ a: SPIOnlyStruct) -> SPIOnlyStruct { fatalError() }

#if !SKIP_ERRORS
@inlinable
public func publicInlinableUser() {
    _ = spiOnlyFunc() // expected-error {{global function 'spiOnlyFunc()' cannot be used in an '@inlinable' function because 'SPIOnlyImportedLib' was imported for SPI only}}

    var x: SPIOnlyStruct // expected-error {{struct 'SPIOnlyStruct' cannot be used in an '@inlinable' function because 'SPIOnlyImportedLib' was imported for SPI only}}
    x = SPIOnlyStruct() // expected-error {{struct 'SPIOnlyStruct' cannot be used in an '@inlinable' function because 'SPIOnlyImportedLib' was imported for SPI only}}
    // expected-error @-1 {{initializer 'init()' cannot be used in an '@inlinable' function because 'SPIOnlyImportedLib' was imported for SPI only}}
    x.structMethod() // expected-error {{instance method 'structMethod()' cannot be used in an '@inlinable' function because 'SPIOnlyImportedLib' was imported for SPI only}}

    var c: SPIOnlyClass<PublicType> // expected-error {{generic class 'SPIOnlyClass' cannot be used in an '@inlinable' function because 'SPIOnlyImportedLib' was imported for SPI only}}
    c = SPIOnlyClass(p: PublicType()) // expected-error {{generic class 'SPIOnlyClass' cannot be used in an '@inlinable' function because 'SPIOnlyImportedLib' was imported for SPI only}}
    // expected-error @-1 {{initializer 'init(p:)' cannot be used in an '@inlinable' function because 'SPIOnlyImportedLib' was imported for SPI only}}
    c.classMethod() // expected-error {{instance method 'classMethod()' cannot be used in an '@inlinable' function because 'SPIOnlyImportedLib' was imported for SPI only}}

    var e: SPIOnlyEnum // expected-error {{enum 'SPIOnlyEnum' cannot be used in an '@inlinable' function because 'SPIOnlyImportedLib' was imported for SPI only}}
    e = .A // expected-error {{enum case 'A' cannot be used in an '@inlinable' function because 'SPIOnlyImportedLib' was imported for SPI only}}
    e.enumMethod() // expected-error {{instance method 'enumMethod()' cannot be used in an '@inlinable' function because 'SPIOnlyImportedLib' was imported for SPI only}}

    let p: PublicType = PublicType()
    p.spiOnlyExtensionMethod() // expected-error {{instance method 'spiOnlyExtensionMethod()' cannot be used in an '@inlinable' function because 'SPIOnlyImportedLib' was imported for SPI only}}
    conformanceUse(p) // expected-error{{cannot use conformance of 'PublicType' to 'PublicProtocol' here; 'SPIOnlyImportedLib' was imported for SPI only}}
}
#endif

@inlinable @_spi(X)
public func spiInlinableUser() {
    _ = spiOnlyFunc()

    var x: SPIOnlyStruct
    x = SPIOnlyStruct()
    x.structMethod()

    let p: PublicType = PublicType()
    p.spiOnlyExtensionMethod()
    conformanceUse(p)
}

public func implementationDetailsUser() {
    _ = spiOnlyFunc()

    var x: SPIOnlyStruct
    x = SPIOnlyStruct()
    x.structMethod()

    let p: PublicType = PublicType()
    p.spiOnlyExtensionMethod()
    conformanceUse(p)
}

public struct ClientStruct {
#if !SKIP_ERRORS
  public var a: SPIOnlyStruct // expected-error {{cannot use struct 'SPIOnlyStruct' here; 'SPIOnlyImportedLib' was imported for SPI only}}
  // expected-error@+1 {{cannot use property 'wrappedValue' here; 'SPIOnlyImportedLib' was imported for SPI only}}
  @SPIOnlyPropertyWrapper(42) public var aWrapped: Any // expected-error {{cannot use generic struct 'SPIOnlyPropertyWrapper' as property wrapper here; 'SPIOnlyImportedLib' was imported for SPI only}}
#endif
  @PublicPropertyWrapper(SPIOnlyStruct()) public var bWrapped: Any
}
