/// Test CheckImplementationOnly with the internal bridging header.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t --leading-lines

// Test with the normal bridging header.
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck %t/main.swift \
// RUN:   -verify -verify-ignore-unrelated -verify-ignore-unknown \
// RUN:   -internal-import-bridging-header %t/objc-bridging-header.h \
// RUN:   -swift-version 6 -verify-additional-prefix non-library-evolution-

// Test with a precompiled bridging header.
// RUN: %target-swift-frontend -emit-pch -o %t/objc-bridging-header.pch \
// RUN:    %t/objc-bridging-header.h -sdk %clang-importer-sdk -swift-version 6
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk)-typecheck %t/main.swift \
// RUN:   -verify -verify-ignore-unrelated -verify-ignore-unknown \
// RUN:   -internal-import-bridging-header %t/objc-bridging-header.pch \
// RUN:   -swift-version 6 -verify-additional-prefix non-library-evolution-

// Test library-evolution differences.
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck %t/main.swift \
// RUN:   -verify -verify-ignore-unrelated -verify-ignore-unknown \
// RUN:   -internal-import-bridging-header %t/objc-bridging-header.h \
// RUN:   -swift-version 6 -enable-library-evolution

// REQUIRES: objc_interop

//--- objc-bridging-header.h
#import <Foundation.h>
@protocol RestrictedProtocol <NSObject>
@end

typedef struct {
  double x, y;
} RestrictedType;

//--- main.swift

/// Referenced types

public struct ExposedLayoutPublic {
  public init() { fatalError() }
}

internal struct ExposedLayoutInternal {
// expected-note @-1 3 {{type declared here}}
}

private struct ExposedLayoutPrivate {
// expected-note @-1 2 {{struct 'ExposedLayoutPrivate' is not '@usableFromInline' or public}}
// expected-note @-2 4 {{type declared here}}
  init() { fatalError() } // expected-note {{initializer 'init()' is not '@usableFromInline' or public}}
}

public class ExposedClassPublic {
  public init() { fatalError() }
}

internal class ExposedClassInternal {
// expected-note @-1 2 {{type declared here}}
}

private class ExposedClassPrivate {
// expected-note @-1 2 {{class 'ExposedClassPrivate' is not '@usableFromInline' or public}}
// expected-note @-2 3 {{type declared here}}
  init() { fatalError() } // expected-note {{initializer 'init()' is not '@usableFromInline' or public}}
}

@_implementationOnly
private class HiddenClass {
// expected-note @-1 2 {{class 'HiddenClass' is not '@usableFromInline' or public}}
// expected-note @-2 1 {{initializer 'init()' is not '@usableFromInline' or public}}
// expected-note @-3 3 {{type declared here}}
// expected-note @-4 2 {{class declared here}}
// expected-non-library-evolution-note @-5 5 {{class declared here}}
}

@_implementationOnly
private struct HiddenLayout {
// expected-note @-1 2 {{struct 'HiddenLayout' is not '@usableFromInline' or public}}
// expected-note @-2 1 {{initializer 'init()' is not '@usableFromInline' or public}}
// expected-note @-3 4 {{type declared here}}
// expected-note @-4 3 {{struct declared here}}
// expected-non-library-evolution-note @-5 6 {{struct declared here}}
}

public enum ExposedEnumPublic {
  case A
  case B
}

private enum ExposedEnumPrivate {
// expected-note @-1 2 {{enum 'ExposedEnumPrivate' is not '@usableFromInline' or public}}
// expected-note @-2 2 {{type declared here}}
  case A
// expected-note @-1 1 {{enum case 'A' is not '@usableFromInline' or public}}
  case B
}

@_implementationOnly
private enum HiddenEnum {
// expected-note @-1 2 {{enum 'HiddenEnum' is not '@usableFromInline' or public}}
// expected-note @-2 2 {{type declared here}}
// expected-note @-3 2 {{enum declared here}}
// expected-non-library-evolution-note @-4 4 {{enum declared here}}
  case A
// expected-note @-1 {{enum case 'A' is not '@usableFromInline' or public}}
  case B
}

public protocol ExposedProtocolPublic {
}

internal protocol ExposedProtocolInternal {
// expected-note @-1 {{protocol 'ExposedProtocolInternal' is not '@usableFromInline' or public}}
// expected-note @-2 3 {{type declared here}}
}

private protocol ExposedProtocolPrivate {
// expected-note @-1 {{protocol 'ExposedProtocolPrivate' is not '@usableFromInline' or public}}
// expected-note @-2 4 {{type declared here}}
}

@_implementationOnly
private protocol HiddenProtocol {
// expected-note @-1 {{protocol 'HiddenProtocol' is not '@usableFromInline' or public}}
// expected-note @-2 4 {{type declared here}}
// expected-note @-3 3 {{protocol declared here}}
// expected-non-library-evolution-note @-4 6 {{protocol declared here}}
}

@_spi(S) public struct SPIStruct {}
// expected-note @-1 {{struct declared here}}

/// Function use sites

@inlinable
public func explicitlyInlinable() {
  let _: ExposedLayoutPublic = ExposedLayoutPublic()
  let _: ExposedLayoutPrivate = ExposedLayoutPrivate()
  // expected-error @-1 2 {{struct 'ExposedLayoutPrivate' is private and cannot be referenced from an '@inlinable' function}}
  // expected-error @-2 {{initializer 'init()' is private and cannot be referenced from an '@inlinable' function}}

  let _: HiddenLayout = HiddenLayout()
  // expected-error @-1 2 {{struct 'HiddenLayout' is private and cannot be referenced from an '@inlinable' function}}
  // expected-error @-2 {{initializer 'init()' is private and cannot be referenced from an '@inlinable' function}}

  let _: ExposedClassPublic = ExposedClassPublic()
  let _: ExposedClassPrivate = ExposedClassPrivate()
  // expected-error @-1 2 {{class 'ExposedClassPrivate' is private and cannot be referenced from an '@inlinable' function}}
  // expected-error @-2 {{initializer 'init()' is private and cannot be referenced from an '@inlinable' function}}

  let _: HiddenClass = HiddenClass()
  // expected-error @-1 2 {{class 'HiddenClass' is private and cannot be referenced from an '@inlinable' function}}
  // expected-error @-2 {{initializer 'init()' is private and cannot be referenced from an '@inlinable' function}}

  let _: ExposedEnumPublic = ExposedEnumPublic.A
  let _: ExposedEnumPrivate = ExposedEnumPrivate.A
  // expected-error @-1 2 {{enum 'ExposedEnumPrivate' is private and cannot be referenced from an '@inlinable' function}}
  // expected-error @-2 {{enum case 'A' is private and cannot be referenced from an '@inlinable' function}}
  let _: HiddenEnum = HiddenEnum.A
  // expected-error @-1 2 {{enum 'HiddenEnum' is private and cannot be referenced from an '@inlinable' function}}
  // expected-error @-2 {{enum case 'A' is private and cannot be referenced from an '@inlinable' function}}

  let _: ExposedProtocolPublic
  let _: ExposedProtocolInternal
  // expected-error @-1 {{protocol 'ExposedProtocolInternal' is internal and cannot be referenced from an '@inlinable' function}}
  let _: ExposedProtocolPrivate
  // expected-error @-1 {{protocol 'ExposedProtocolPrivate' is private and cannot be referenced from an '@inlinable' function}}
  let _: HiddenProtocol
  // expected-error @-1 {{protocol 'HiddenProtocol' is private and cannot be referenced from an '@inlinable' function}}
}

public func implicitlyInlinablePublic() {
  let _: ExposedLayoutPublic = ExposedLayoutPublic()
  let _: ExposedLayoutPrivate = ExposedLayoutPrivate()
  let _: HiddenLayout = HiddenLayout()
  // expected-embedded-error @-1 2 {{struct 'HiddenLayout' cannot be used in an embedded function not marked '@export(interface)' because 'HiddenLayout' is marked '@_implementationOnly'}}

  let _: ExposedClassPublic = ExposedClassPublic()
  let _: ExposedClassPrivate = ExposedClassPrivate()
  let _: HiddenClass = HiddenClass()
  // expected-embedded-error @-1 2 {{class 'HiddenClass' cannot be used in an embedded function not marked '@export(interface)' because 'HiddenClass' is marked '@_implementationOnly'}}

  let _: ExposedEnumPublic = ExposedEnumPublic.A
  let _: ExposedEnumPrivate = ExposedEnumPrivate.A
  let _: HiddenEnum = HiddenEnum.A
  // expected-embedded-error @-1 2 {{enum 'HiddenEnum' cannot be used in an embedded function not marked '@export(interface)' because 'HiddenEnum' is marked '@_implementationOnly'}}

  let _: ExposedProtocolPublic
  let _: ExposedProtocolInternal
  let _: ExposedProtocolPrivate
  let _: HiddenProtocol
  // expected-embedded-error @-1 {{protocol 'HiddenProtocol' cannot be used in an embedded function not marked '@export(interface)' because 'HiddenProtocol' is marked '@_implementationOnly'}}
}

private func implicitlyInlinablePrivate() {
  let _: ExposedLayoutPublic = ExposedLayoutPublic()
  let _: ExposedLayoutPrivate = ExposedLayoutPrivate()
  let _: HiddenLayout = HiddenLayout()
  // expected-embedded-error @-1 2 {{struct 'HiddenLayout' cannot be used in an embedded function not marked '@export(interface)' because 'HiddenLayout' is marked '@_implementationOnly'}}

  let _: ExposedClassPublic = ExposedClassPublic()
  let _: ExposedClassPrivate = ExposedClassPrivate()
  let _: HiddenClass = HiddenClass()
  // expected-embedded-error @-1 2 {{class 'HiddenClass' cannot be used in an embedded function not marked '@export(interface)' because 'HiddenClass' is marked '@_implementationOnly'}}

  let _: ExposedEnumPublic = ExposedEnumPublic.A
  let _: ExposedEnumPrivate = ExposedEnumPrivate.A
  let _: HiddenEnum = HiddenEnum.A
  // expected-embedded-error @-1 2 {{enum 'HiddenEnum' cannot be used in an embedded function not marked '@export(interface)' because 'HiddenEnum' is marked '@_implementationOnly'}}

  let _: ExposedProtocolPublic
  let _: ExposedProtocolInternal
  let _: ExposedProtocolPrivate
  let _: HiddenProtocol
  // expected-embedded-error @-1 {{protocol 'HiddenProtocol' cannot be used in an embedded function not marked '@export(interface)' because 'HiddenProtocol' is marked '@_implementationOnly'}}
}

@export(interface)
public func explicitNonInliable() {
  let _: ExposedLayoutPublic = ExposedLayoutPublic()
  let _: ExposedLayoutPrivate = ExposedLayoutPrivate()
  let _: HiddenLayout = HiddenLayout()

  let _: ExposedClassPublic = ExposedClassPublic()
  let _: ExposedClassPrivate = ExposedClassPrivate()
  let _: HiddenClass = HiddenClass()

  let _: ExposedEnumPublic = ExposedEnumPublic.A
  let _: ExposedEnumPrivate = ExposedEnumPrivate.A
  let _: HiddenEnum = HiddenEnum.A

  let _: ExposedProtocolPublic
  let _: ExposedProtocolInternal
  let _: ExposedProtocolPrivate
  let _: HiddenProtocol
}

@export(interface)
internal func explicitNonInliableInternal() {
  let _: ExposedLayoutPublic = ExposedLayoutPublic()
  let _: ExposedLayoutPrivate = ExposedLayoutPrivate()
  let _: HiddenLayout = HiddenLayout()

  let _: ExposedClassPublic = ExposedClassPublic()
  let _: ExposedClassPrivate = ExposedClassPrivate()
  let _: HiddenClass = HiddenClass()

  let _: ExposedEnumPublic = ExposedEnumPublic.A
  let _: ExposedEnumPrivate = ExposedEnumPrivate.A
  let _: HiddenEnum = HiddenEnum.A

  let _: ExposedProtocolPublic
  let _: ExposedProtocolInternal
  let _: ExposedProtocolPrivate
  let _: HiddenProtocol
}

/// Struct use sites

typealias TA = RestrictedType // expected-note 3 {{type declared here}}

@frozen
public struct ExposedLayoutFrozenUser {

  private var ta: TA
  // expected-error @-1 {{'TA' aliases '__ObjC.RestrictedType' and cannot be used in a property declaration marked public or in a '@frozen' or '@usableFromInline' context because it was imported via the internal bridging header}}
  // expected-error @-2 {{type referenced from a stored property in a '@frozen' struct must be '@usableFromInline' or public}}

  public var publicField: RestrictedType
  // expected-error @-1 {{property cannot be declared public because its type uses an internal type}}
  // expected-note @-2 2 {{struct 'RestrictedType' is imported by this file as 'internal' from bridging header}}
  // expected-error @-3 {{type referenced from a stored property in a '@frozen' struct must be '@usableFromInline' or public}}

  private var privateField: RestrictedType
  // expected-error @-1 {{type referenced from a stored property in a '@frozen' struct must be '@usableFromInline' or public}}
  // expected-note @-2 {{struct 'RestrictedType' is imported by this file as 'internal' from bridging header}}

  private var a: ExposedLayoutPublic
  private var aa: ExposedLayoutInternal
  // expected-error @-1 {{type referenced from a stored property in a '@frozen' struct must be '@usableFromInline' or public}}
  private var b: ExposedLayoutPrivate
  // expected-error @-1 {{type referenced from a stored property in a '@frozen' struct must be '@usableFromInline' or public}}
  private var c: HiddenLayout
  // expected-error @-1 {{cannot use struct 'HiddenLayout' in a property declaration marked public or in a '@frozen' or '@usableFromInline' context; 'HiddenLayout' is marked '@_implementationOnly'}}
  // expected-error @-2 {{type referenced from a stored property in a '@frozen' struct must be '@usableFromInline' or public}}

  private var ca: ExposedClassPublic
  private var cb: ExposedClassInternal
  // expected-error @-1 {{type referenced from a stored property in a '@frozen' struct must be '@usableFromInline' or public}}
  private var cc: ExposedClassPrivate
  // expected-error @-1 {{type referenced from a stored property in a '@frozen' struct must be '@usableFromInline' or public}}
  private var cd: HiddenClass
  // expected-error @-1 {{cannot use class 'HiddenClass' in a property declaration marked public or in a '@frozen' or '@usableFromInline' context; 'HiddenClass' is marked '@_implementationOnly'}}
  // expected-error @-2 {{type referenced from a stored property in a '@frozen' struct must be '@usableFromInline' or public}}

  private var d: ExposedEnumPublic
  private var e: ExposedEnumPrivate
  // expected-error @-1 {{type referenced from a stored property in a '@frozen' struct must be '@usableFromInline' or public}}
  private var f: HiddenEnum
  // expected-error @-1 {{cannot use enum 'HiddenEnum' in a property declaration marked public or in a '@frozen' or '@usableFromInline' context; 'HiddenEnum' is marked '@_implementationOnly'}}
  // expected-error @-2 {{type referenced from a stored property in a '@frozen' struct must be '@usableFromInline' or public}}

  private var pp: RestrictedProtocol
  // expected-error @-1 {{type referenced from a stored property in a '@frozen' struct must be '@usableFromInline' or public}}
  // expected-note @-2 {{protocol 'RestrictedProtocol' is imported by this file as 'internal' from bridging header}}

  private var g: ExposedProtocolPublic
  private var h: ExposedProtocolInternal
  // expected-error @-1 {{type referenced from a stored property in a '@frozen' struct must be '@usableFromInline' or public}}
  private var i: ExposedProtocolPrivate
  // expected-error @-1 {{type referenced from a stored property in a '@frozen' struct must be '@usableFromInline' or public}}
  private var j: HiddenProtocol
  // expected-error @-1 {{cannot use protocol 'HiddenProtocol' in a property declaration marked public or in a '@frozen' or '@usableFromInline' context; 'HiddenProtocol' is marked '@_implementationOnly'}}
  // expected-error @-2 {{type referenced from a stored property in a '@frozen' struct must be '@usableFromInline' or public}}

  private func privateFunc(h: HiddenLayout) {}
  // expected-embedded-error @-1 {{struct 'HiddenLayout' cannot be used in an embedded function not marked '@export(interface)' because 'HiddenLayout' is marked '@_implementationOnly'}}
  private func privateFuncClass(h: HiddenClass) {}
  // expected-embedded-error @-1 {{class 'HiddenClass' cannot be used in an embedded function not marked '@export(interface)' because 'HiddenClass' is marked '@_implementationOnly'}}

  @_spi(S) public var s: SPIStruct
  // expected-error @-1 {{stored property 's' cannot be declared '@_spi' in a '@frozen' struct}}
  // expected-error @-2 {{cannot use struct 'SPIStruct' in a property declaration marked public or in a '@frozen' or '@usableFromInline' context; it is SPI}}
}

public struct ExposedLayoutPublicUser {

  private var ta: TA
  // expected-non-library-evolution-error @-1 {{'TA' aliases '__ObjC.RestrictedType' and cannot be used in a property declaration member of a type not marked '@_implementationOnly' because it was imported via the internal bridging header}}

  public var publicField: RestrictedType
  // expected-error @-1 {{property cannot be declared public because its type uses an internal type}}
  // expected-note @-2 {{struct 'RestrictedType' is imported by this file as 'internal' from bridging header}}

  private var privateField: RestrictedType
  // expected-non-library-evolution-error @-1 {{cannot use struct 'RestrictedType' in a property declaration member of a type not marked '@_implementationOnly'; it was imported via the internal bridging header}}

  private var a: ExposedLayoutPublic
  private var aa: ExposedLayoutInternal
  private var b: ExposedLayoutPrivate
  private var c: HiddenLayout
  // expected-non-library-evolution-error @-1 {{cannot use struct 'HiddenLayout' in a property declaration member of a type not marked '@_implementationOnly'; 'HiddenLayout' is marked '@_implementationOnly'}}

  private var ca: ExposedClassPublic
  private var cb: ExposedClassInternal
  private var cc: ExposedClassPrivate
  private var cd: HiddenClass
  // expected-non-library-evolution-error @-1 {{cannot use class 'HiddenClass' in a property declaration member of a type not marked '@_implementationOnly'; 'HiddenClass' is marked '@_implementationOnly'}}

  private var d: ExposedEnumPublic
  private var e: ExposedEnumPrivate
  private var f: HiddenEnum
  // expected-non-library-evolution-error @-1 {{cannot use enum 'HiddenEnum' in a property declaration member of a type not marked '@_implementationOnly'; 'HiddenEnum' is marked '@_implementationOnly'}}

  private var pp: RestrictedProtocol
  // expected-non-library-evolution-error @-1 {{cannot use protocol 'RestrictedProtocol' in a property declaration member of a type not marked '@_implementationOnly'; it was imported via the internal bridging header}}

  private var g: ExposedProtocolPublic
  private var h: ExposedProtocolInternal
  private var i: ExposedProtocolPrivate
  private var j: HiddenProtocol
  // expected-non-library-evolution-error @-1 {{cannot use protocol 'HiddenProtocol' in a property declaration member of a type not marked '@_implementationOnly'; 'HiddenProtocol' is marked '@_implementationOnly'}}

  private func privateFunc(h: HiddenLayout) {}
  // expected-embedded-error @-1 {{struct 'HiddenLayout' cannot be used in an embedded function not marked '@export(interface)' because 'HiddenLayout' is marked '@_implementationOnly'}}
  private func privateFuncClass(h: HiddenClass) {}
  // expected-embedded-error @-1 {{class 'HiddenClass' cannot be used in an embedded function not marked '@export(interface)' because 'HiddenClass' is marked '@_implementationOnly'}}

  @_spi(S) public var s: SPIStruct
}

internal struct ExposedLayoutInternalUser {

  private var ta: TA
  // expected-non-library-evolution-error @-1 {{'TA' aliases '__ObjC.RestrictedType' and cannot be used in a property declaration member of a type not marked '@_implementationOnly' because it was imported via the internal bridging header}}

  private var privateField: RestrictedType
  // expected-non-library-evolution-error @-1 {{cannot use struct 'RestrictedType' in a property declaration member of a type not marked '@_implementationOnly'; it was imported via the internal bridging header}}

  private var a: ExposedLayoutPublic
  private var aa: ExposedLayoutInternal
  private var b: ExposedLayoutPrivate
  private var c: HiddenLayout
  // expected-non-library-evolution-error @-1 {{cannot use struct 'HiddenLayout' in a property declaration member of a type not marked '@_implementationOnly'; 'HiddenLayout' is marked '@_implementationOnly'}}

  private var ca: ExposedClassPublic
  private var cb: ExposedClassInternal
  private var cc: ExposedClassPrivate
  private var cd: HiddenClass
  // expected-non-library-evolution-error @-1 {{cannot use class 'HiddenClass' in a property declaration member of a type not marked '@_implementationOnly'; 'HiddenClass' is marked '@_implementationOnly'}}

  private var d: ExposedEnumPublic
  private var e: ExposedEnumPrivate
  private var f: HiddenEnum
  // expected-non-library-evolution-error @-1 {{cannot use enum 'HiddenEnum' in a property declaration member of a type not marked '@_implementationOnly'; 'HiddenEnum' is marked '@_implementationOnly'}}

  private var g: ExposedProtocolPublic
  private var h: ExposedProtocolInternal
  private var i: ExposedProtocolPrivate
  private var j: HiddenProtocol
  // expected-non-library-evolution-error @-1 {{cannot use protocol 'HiddenProtocol' in a property declaration member of a type not marked '@_implementationOnly'; 'HiddenProtocol' is marked '@_implementationOnly'}}

  private func privateFunc(h: HiddenLayout) {}
  // expected-embedded-error @-1 {{struct 'HiddenLayout' cannot be used in an embedded function not marked '@export(interface)' because 'HiddenLayout' is marked '@_implementationOnly'}}
  private func privateFuncClass(h: HiddenClass) {}
  // expected-embedded-error @-1 {{class 'HiddenClass' cannot be used in an embedded function not marked '@export(interface)' because 'HiddenClass' is marked '@_implementationOnly'}}

  @_spi(S) public var s: SPIStruct
}

private struct ExposedLayoutPrivateUser {

  private var ta: TA
  // expected-non-library-evolution-error @-1 {{'TA' aliases '__ObjC.RestrictedType' and cannot be used in a property declaration member of a type not marked '@_implementationOnly' because it was imported via the internal bridging header}}

  private var privateField: RestrictedType
  // expected-non-library-evolution-error @-1 {{cannot use struct 'RestrictedType' in a property declaration member of a type not marked '@_implementationOnly'; it was imported via the internal bridging header}}

  private var a: ExposedLayoutPublic
  private var aa: ExposedLayoutInternal
  private var b: ExposedLayoutPrivate
  private var c: HiddenLayout
  // expected-non-library-evolution-error @-1 {{cannot use struct 'HiddenLayout' in a property declaration member of a type not marked '@_implementationOnly'; 'HiddenLayout' is marked '@_implementationOnly'}}

  private var ca: ExposedClassPublic
  private var cb: ExposedClassInternal
  private var cc: ExposedClassPrivate
  private var cd: HiddenClass
  // expected-non-library-evolution-error @-1 {{cannot use class 'HiddenClass' in a property declaration member of a type not marked '@_implementationOnly'; 'HiddenClass' is marked '@_implementationOnly'}}

  private var d: ExposedEnumPublic
  private var e: ExposedEnumPrivate
  private var f: HiddenEnum
  // expected-non-library-evolution-error @-1 {{cannot use enum 'HiddenEnum' in a property declaration member of a type not marked '@_implementationOnly'; 'HiddenEnum' is marked '@_implementationOnly'}}

  private var g: ExposedProtocolPublic
  private var h: ExposedProtocolInternal
  private var i: ExposedProtocolPrivate
  private var j: HiddenProtocol
  // expected-non-library-evolution-error @-1 {{cannot use protocol 'HiddenProtocol' in a property declaration member of a type not marked '@_implementationOnly'; 'HiddenProtocol' is marked '@_implementationOnly'}}

  private func privateFunc(h: HiddenLayout) {}
  // expected-embedded-error @-1 {{struct 'HiddenLayout' cannot be used in an embedded function not marked '@export(interface)' because 'HiddenLayout' is marked '@_implementationOnly'}}
  private func privateFuncClass(h: HiddenClass) {}
  // expected-embedded-error @-1 {{class 'HiddenClass' cannot be used in an embedded function not marked '@export(interface)' because 'HiddenClass' is marked '@_implementationOnly'}}

  @_spi(S) public var s: SPIStruct
}

@_implementationOnly
private struct HiddenLayoutUser {
  private var ta: TA

  public var publicField: RestrictedType
  private var privateField: RestrictedType
  private var a: ExposedLayoutPublic
  private var aa: ExposedLayoutInternal
  private var b: ExposedLayoutPrivate

  private var ca: ExposedClassPublic
  private var cb: ExposedClassInternal
  private var cc: ExposedClassPrivate

  private var c: HiddenLayout
  private var cd: HiddenClass

  private var d: ExposedEnumPublic
  private var e: ExposedEnumPrivate
  private var f: HiddenEnum

  private var g: ExposedProtocolPublic
  private var h: ExposedProtocolInternal
  private var i: ExposedProtocolPrivate
  private var j: HiddenProtocol

  @export(interface)
  private func privateFunc(h: HiddenLayout) {}
  @export(interface)
  private func privateFuncClass(h: HiddenClass) {}
}

@_implementationOnly // expected-error {{'@_implementationOnly' may not be used on public declarations}}
public struct PublicHiddenStruct {}

/// Enums use sites

public enum PublicEnumUser {
    case a(RestrictedType) // expected-error {{enum case in a public enum uses an internal type}}
  // expected-note @-1 {{struct 'RestrictedType' is imported by this file as 'internal' from bridging header}}

    case e(ExposedLayoutPublic)
    case c(ExposedLayoutInternal) // expected-error {{enum case in a public enum uses an internal type}}
    case d(ExposedLayoutPrivate) // expected-error {{enum case in a public enum uses a private type}}
    case b(HiddenLayout) // expected-error {{enum case in a public enum uses a private type}}
    // expected-error @-1 {{cannot use struct 'HiddenLayout' in an associated value of a public or '@usableFromInline' enum; 'HiddenLayout' is marked '@_implementationOnly'}}

    case ce(ExposedClassPublic)
    case cc(ExposedClassInternal) // expected-error {{enum case in a public enum uses an internal type}}
    case cd(ExposedClassPrivate) // expected-error {{enum case in a public enum uses a private type}}
    case cb(HiddenClass) // expected-error {{enum case in a public enum uses a private type}}
    // expected-error @-1 {{cannot use class 'HiddenClass' in an associated value of a public or '@usableFromInline' enum; 'HiddenClass' is marked '@_implementationOnly'}}

    case f(ExposedProtocolPublic)
    case g(ExposedProtocolInternal) // expected-error {{enum case in a public enum uses an internal type}}
    case h(ExposedProtocolPrivate) // expected-error {{enum case in a public enum uses a private type}}
    case i(HiddenProtocol) // expected-error {{cannot use protocol 'HiddenProtocol' in an associated value of a public or '@usableFromInline' enum; 'HiddenProtocol' is marked '@_implementationOnly'}}
    // expected-error @-1 {{enum case in a public enum uses a private type}}

    case ta(TA) // expected-error {{aliases '__ObjC.RestrictedType' and cannot be used in an associated value of a public or '@usableFromInline' enum because it was imported via the internal bridging header}}
    // expected-error @-1 {{enum case in a public enum uses an internal type}}
}

internal enum InternalEnumUser {
    case a(RestrictedType) // expected-non-library-evolution-error {{cannot use struct 'RestrictedType' in an associated value of an enum not marked '@_implementationOnly'; it was imported via the internal bridging header}}

    case e(ExposedLayoutPublic)
    case c(ExposedLayoutInternal)
    case d(ExposedLayoutPrivate) // expected-error {{enum case in an internal enum uses a private type}}
    case b(HiddenLayout) // expected-non-library-evolution-error {{cannot use struct 'HiddenLayout' in an associated value of an enum not marked '@_implementationOnly'; 'HiddenLayout' is marked '@_implementationOnly'}}
    // expected-error @-1 {{enum case in an internal enum uses a private type}}

    case ce(ExposedClassPublic)
    case cc(ExposedClassInternal)
    case cd(ExposedClassPrivate) // expected-error {{enum case in an internal enum uses a private type}}
    case cb(HiddenClass) // expected-non-library-evolution-error {{cannot use class 'HiddenClass' in an associated value of an enum not marked '@_implementationOnly'; 'HiddenClass' is marked '@_implementationOnly'}}
    // expected-error @-1 {{enum case in an internal enum uses a private type}}

    case f(ExposedProtocolPublic)
    case g(ExposedProtocolInternal)
    case h(ExposedProtocolPrivate) // expected-error {{enum case in an internal enum uses a private type}}
    case i(HiddenProtocol) // expected-non-library-evolution-error {{cannot use protocol 'HiddenProtocol' in an associated value of an enum not marked '@_implementationOnly'; 'HiddenProtocol' is marked '@_implementationOnly'}}
    // expected-error @-1 {{enum case in an internal enum uses a private type}}

    case ta(TA)
    // expected-non-library-evolution-error @-1 {{'TA' aliases '__ObjC.RestrictedType' and cannot be used in an associated value of an enum not marked '@_implementationOnly' because it was imported via the internal bridging header}}
}

private enum PrivateEnumUser {
    case a(RestrictedType) // expected-non-library-evolution-error {{cannot use struct 'RestrictedType' in an associated value of an enum not marked '@_implementationOnly'; it was imported via the internal bridging header}}

    case e(ExposedLayoutPublic)
    case c(ExposedLayoutInternal)
    case d(ExposedLayoutPrivate)
    case b(HiddenLayout) // expected-non-library-evolution-error {{cannot use struct 'HiddenLayout' in an associated value of an enum not marked '@_implementationOnly'; 'HiddenLayout' is marked '@_implementationOnly'}}

    case ce(ExposedClassPublic)
    case cc(ExposedClassInternal)
    case cd(ExposedClassPrivate)
    case cb(HiddenClass) // expected-non-library-evolution-error {{cannot use class 'HiddenClass' in an associated value of an enum not marked '@_implementationOnly'; 'HiddenClass' is marked '@_implementationOnly'}}

    case f(ExposedProtocolPublic)
    case g(ExposedProtocolInternal)
    case h(ExposedProtocolPrivate)
    case i(HiddenProtocol) // expected-non-library-evolution-error {{cannot use protocol 'HiddenProtocol' in an associated value of an enum not marked '@_implementationOnly'; 'HiddenProtocol' is marked '@_implementationOnly'}}

    case ta(TA)
    // expected-non-library-evolution-error @-1 {{'TA' aliases '__ObjC.RestrictedType' and cannot be used in an associated value of an enum not marked '@_implementationOnly' because it was imported via the internal bridging header}}
}

@_implementationOnly
private enum PrivateHiddenEnumUser {
    case a(RestrictedType)

    case e(ExposedLayoutPublic)
    case c(ExposedLayoutInternal)
    case d(ExposedLayoutPrivate)
    case b(HiddenLayout)

    case ce(ExposedClassPublic)
    case cc(ExposedClassInternal)
    case cd(ExposedClassPrivate)
    case cb(HiddenClass)

    case f(ExposedProtocolPublic)
    case g(ExposedProtocolInternal)
    case h(ExposedProtocolPrivate)
    case i(HiddenProtocol)
}

@_implementationOnly // expected-error {{'@_implementationOnly' may not be used on public declarations}}
public enum PublicHiddenEnum {}

/// Classes use sites

public class PublicClassUser {

  public init() { fatalError() }

  public var publicField: RestrictedType
  // expected-error @-1 {{property cannot be declared public because its type uses an internal type}}
  // expected-note @-2 {{struct 'RestrictedType' is imported by this file as 'internal' from bridging header}}

  private var privateField: RestrictedType
  private var a: ExposedLayoutPublic
  private var aa: ExposedLayoutInternal
  private var b: ExposedLayoutPrivate
  private var c: HiddenLayout

  private var d: ExposedEnumPublic
  private var e: ExposedEnumPrivate
  private var f: HiddenEnum

  private var g: ExposedProtocolPublic
  private var h: ExposedProtocolInternal
  private var i: ExposedProtocolPrivate
  private var j: HiddenProtocol

  @export(interface)
  private func privateFunc(h: HiddenLayout) {}
}

open class OpenClassUser {

  public init() { fatalError() }

  private var ta: TA
  // expected-non-library-evolution-error @-1 {{'TA' aliases '__ObjC.RestrictedType' and cannot be used in a property declaration member of a type not marked '@_implementationOnly' because it was imported via the internal bridging header}}

  public var publicField: RestrictedType
  // expected-error @-1 {{property cannot be declared public because its type uses an internal type}}
  // expected-note @-2 {{struct 'RestrictedType' is imported by this file as 'internal' from bridging header}}

  private var privateField: RestrictedType
  // expected-non-library-evolution-error @-1 {{cannot use struct 'RestrictedType' in a property declaration member of a type not marked '@_implementationOnly'; it was imported via the internal bridging header}}
  private var a: ExposedLayoutPublic
  private var aa: ExposedLayoutInternal
  private var b: ExposedLayoutPrivate
  private var c: HiddenLayout
  // expected-non-library-evolution-error @-1 {{cannot use struct 'HiddenLayout' in a property declaration member of a type not marked '@_implementationOnly'; 'HiddenLayout' is marked '@_implementationOnly'}}

  private var d: ExposedEnumPublic
  private var e: ExposedEnumPrivate
  private var f: HiddenEnum
  // expected-non-library-evolution-error @-1 {{cannot use enum 'HiddenEnum' in a property declaration member of a type not marked '@_implementationOnly'; 'HiddenEnum' is marked '@_implementationOnly'}}

  private var g: ExposedProtocolPublic
  private var h: ExposedProtocolInternal
  private var i: ExposedProtocolPrivate
  private var j: HiddenProtocol
  // expected-non-library-evolution-error @-1 {{cannot use protocol 'HiddenProtocol' in a property declaration member of a type not marked '@_implementationOnly'; 'HiddenProtocol' is marked '@_implementationOnly'}}

  @export(interface)
  private func privateFunc(h: HiddenLayout) {}
}

@_fixed_layout
public class FixedClassUser {

  public init() { fatalError() }

  private var ta: TA
  // expected-error @-1 {{'TA' aliases '__ObjC.RestrictedType' and cannot be used in a property declaration marked public or in a '@frozen' or '@usableFromInline' context because it was imported via the internal bridging header}}
  // expected-error @-2 {{type referenced from a stored property in a '@frozen' struct must be '@usableFromInline' or public}}

  public var publicField: RestrictedType
  // expected-error @-1 {{property cannot be declared public because its type uses an internal type}}
  // expected-error @-2 {{type referenced from a stored property in a '@frozen' struct must be '@usableFromInline' or public}}
  // expected-note @-3 2 {{struct 'RestrictedType' is imported by this file as 'internal' from bridging header}}

  private var privateField: RestrictedType
  // expected-error @-1 {{type referenced from a stored property in a '@frozen' struct must be '@usableFromInline' or public}}
  // expected-note @-2 {{struct 'RestrictedType' is imported by this file as 'internal' from bridging header}}
  private var a: ExposedLayoutPublic
  private var aa: ExposedLayoutInternal
  // expected-error @-1 {{type referenced from a stored property in a '@frozen' struct must be '@usableFromInline' or public}}
  private var b: ExposedLayoutPrivate
  // expected-error @-1 {{type referenced from a stored property in a '@frozen' struct must be '@usableFromInline' or public}}
  private var c: HiddenLayout
  // expected-error @-1 {{cannot use struct 'HiddenLayout' in a property declaration marked public or in a '@frozen' or '@usableFromInline' context; 'HiddenLayout' is marked '@_implementationOnly'}}
  // expected-error @-2 {{type referenced from a stored property in a '@frozen' struct must be '@usableFromInline' or public}}

  private var d: ExposedEnumPublic
  private var e: ExposedEnumPrivate
  // expected-error @-1 {{type referenced from a stored property in a '@frozen' struct must be '@usableFromInline' or public}}
  private var f: HiddenEnum
  // expected-error @-1 {{cannot use enum 'HiddenEnum' in a property declaration marked public or in a '@frozen' or '@usableFromInline' context; 'HiddenEnum' is marked '@_implementationOnly'}}
  // expected-error @-2 {{type referenced from a stored property in a '@frozen' struct must be '@usableFromInline' or public}}

  private var g: ExposedProtocolPublic
  private var h: ExposedProtocolInternal
  // expected-error @-1 {{type referenced from a stored property in a '@frozen' struct must be '@usableFromInline' or public}}
  private var i: ExposedProtocolPrivate
  // expected-error @-1 {{type referenced from a stored property in a '@frozen' struct must be '@usableFromInline' or public}}
  private var j: HiddenProtocol
  // expected-error @-1 {{cannot use protocol 'HiddenProtocol' in a property declaration marked public or in a '@frozen' or '@usableFromInline' context; 'HiddenProtocol' is marked '@_implementationOnly'}}
  // expected-error @-2 {{type referenced from a stored property in a '@frozen' struct must be '@usableFromInline' or public}}

  @export(interface)
  private func privateFunc(h: HiddenLayout) {}
}

internal class InternalClassUser {

  public init() { fatalError() }

  private var ta: TA

  public var publicField: RestrictedType
  private var privateField: RestrictedType

  private var a: ExposedLayoutPublic
  private var aa: ExposedLayoutInternal
  private var b: ExposedLayoutPrivate
  private var c: HiddenLayout

  private var d: ExposedEnumPublic
  private var e: ExposedEnumPrivate
  private var f: HiddenEnum

  private var g: ExposedProtocolPublic
  private var h: ExposedProtocolInternal
  private var i: ExposedProtocolPrivate
  private var j: HiddenProtocol

  private func privateFunc(h: HiddenLayout) {} // expected-embedded-error {{struct 'HiddenLayout' cannot be used in an embedded function not marked '@export(interface)' because 'HiddenLayout' is marked '@_implementationOnly'}}
}

private class PrivateClassUser {

  public init() { fatalError() }

  private var ta: TA

  public var publicField: RestrictedType
  private var privateField: RestrictedType

  private var a: ExposedLayoutPublic
  private var aa: ExposedLayoutInternal
  private var b: ExposedLayoutPrivate
  private var c: HiddenLayout

  private var d: ExposedEnumPublic
  private var e: ExposedEnumPrivate
  private var f: HiddenEnum

  private var g: ExposedProtocolPublic
  private var h: ExposedProtocolInternal
  private var i: ExposedProtocolPrivate
  private var j: HiddenProtocol

  private func privateFunc(h: HiddenLayout) {} // expected-embedded-error {{struct 'HiddenLayout' cannot be used in an embedded function not marked '@export(interface)' because 'HiddenLayout' is marked '@_implementationOnly'}}
}

@_implementationOnly
internal class HiddenClassUser {
  public init() { fatalError() }

  private var ta: TA

  public var publicField: RestrictedType
  private var privateField: RestrictedType

  private var a: ExposedLayoutPublic
  private var aa: ExposedLayoutInternal
  private var b: ExposedLayoutPrivate
  private var c: HiddenLayout

  private var ca: ExposedClassPublic
  private var cb: ExposedClassInternal
  private var cc: ExposedClassPrivate
  private var cd: HiddenClass

  private var d: ExposedEnumPublic
  private var e: ExposedEnumPrivate
  private var f: HiddenEnum

  private var g: ExposedProtocolPublic
  private var h: ExposedProtocolInternal
  private var i: ExposedProtocolPrivate
  private var j: HiddenProtocol
}

@_implementationOnly // expected-error {{'@_implementationOnly' may not be used on public declarations}}
public enum PublicHiddenClass {}

/// Protocol use sites

public protocol PublicProtocol : RestrictedProtocol {
// expected-error @-1 {{public protocol cannot refine an internal protocol}}
// expected-note @-2 {{protocol 'RestrictedProtocol' is imported by this file as 'internal' from bridging header}}
}

internal protocol InternalProtocol : RestrictedProtocol {
// expected-non-library-evolution-error @-1 {{cannot use protocol 'RestrictedProtocol' here; it was imported via the internal bridging header}}
}

private protocol PrivateProtocol : RestrictedProtocol {
// expected-non-library-evolution-error @-1 {{cannot use protocol 'RestrictedProtocol' here; it was imported via the internal bridging header}}
}

@_implementationOnly
internal protocol PrivateProtocolHidden : RestrictedProtocol {
}
