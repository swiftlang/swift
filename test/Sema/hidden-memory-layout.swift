/// Test @_implementationOnly import exportability diagnostics in non-library-evolution mode

/// Standard / non-embedded

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/indirects.swiftmodule \
// RUN:   %S/Inputs/implementation-only-imports/indirects.swift \
// RUN:   -swift-version 5
// RUN: %target-swift-frontend -emit-module -o %t/directs.swiftmodule -I %t\
// RUN:    %S/Inputs/implementation-only-imports/directs.swift \
// RUN:   -swift-version 5

/// Old diags
// RUN: %target-swift-frontend -emit-module -verify -verify-ignore-unrelated %s -I %t \
// RUN:   -swift-version 5 \
// RUN:   -verify-additional-prefix not-opt-in-

/// New diags
// RUN: %target-swift-frontend -emit-module -verify -verify-ignore-unrelated %s -I %t \
// RUN:   -swift-version 5 \
// RUN:   -verify-additional-prefix opt-in- -DUseImplementationOnly \
// RUN:   -enable-experimental-feature CheckImplementationOnly

/// Embedded
/// Will also show errors in non-never-emit-into-client functions.

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/indirects.swiftmodule \
// RUN:   %S/Inputs/implementation-only-imports/indirects.swift \
// RUN:   -swift-version 5 -target arm64-apple-none-macho \
// RUN:   -enable-experimental-feature Embedded
// RUN: %target-swift-frontend -emit-module -o %t/directs.swiftmodule -I %t\
// RUN:    %S/Inputs/implementation-only-imports/directs.swift \
// RUN:   -swift-version 5 -target arm64-apple-none-macho \
// RUN:   -enable-experimental-feature Embedded

/// Old diags
// RUN: %target-swift-frontend -emit-module -verify -verify-ignore-unrelated %s -I %t \
// RUN:   -swift-version 5 -target arm64-apple-none-macho \
// RUN:   -enable-experimental-feature Embedded \
// RUN:   -verify-additional-prefix not-opt-in-

/// New diags
// RUN: %target-swift-frontend -emit-module -verify -verify-ignore-unrelated %s -I %t \
// RUN:   -swift-version 5 -target arm64-apple-none-macho \
// RUN:   -enable-experimental-feature Embedded \
// RUN:   -verify-additional-prefix opt-in- -DUseImplementationOnly \
// RUN:   -verify-additional-prefix embedded-opt-in- \
// RUN:   -enable-experimental-feature CheckImplementationOnly

// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_CheckImplementationOnly
// REQUIRES: embedded_stdlib_cross_compiling

@_implementationOnly import directs
// expected-warning @-1 {{using '@_implementationOnly' without enabling library evolution for 'main' may lead to instability during execution}}
import indirects

/// Referenced types

public struct ExposedLayoutPublic {
  public init() { fatalError() }
}

internal struct ExposedLayoutInternal {
// expected-note @-1 {{type declared here}}
}

private struct ExposedLayoutPrivate {
// expected-note @-1 2 {{struct 'ExposedLayoutPrivate' is not '@usableFromInline' or public}}
// expected-note @-2 2 {{type declared here}}
  init() { fatalError() } // expected-note {{initializer 'init()' is not '@usableFromInline' or public}}
}

#if UseImplementationOnly
@_implementationOnly
private struct HiddenLayout {
// expected-opt-in-note @-1 2 {{struct 'HiddenLayout' is not '@usableFromInline' or public}}
// expected-opt-in-note @-2 1 {{initializer 'init()' is not '@usableFromInline' or public}}
// expected-opt-in-note @-3 9 {{struct declared here}}
// expected-opt-in-note @-4 2 {{type declared here}}
}
#else
private struct HiddenLayout {
// expected-not-opt-in-note @-1 2 {{struct 'HiddenLayout' is not '@usableFromInline' or public}}
// expected-not-opt-in-note @-2 1 {{initializer 'init()' is not '@usableFromInline' or public}}
// expected-not-opt-in-note @-3 2 {{type declared here}}
}
#endif

public enum ExposedEnumPublic {
  case A
  case B
}

private enum ExposedEnumPrivate {
// expected-note @-1 2 {{enum 'ExposedEnumPrivate' is not '@usableFromInline' or public}}
  case A
// expected-note @-1 1 {{enum case 'A' is not '@usableFromInline' or public}}
  case B
}

#if UseImplementationOnly
@_implementationOnly
private enum HiddenEnum {
// expected-opt-in-note @-1 6 {{enum declared here}}
// expected-opt-in-note @-2 2 {{enum 'HiddenEnum' is not '@usableFromInline' or public}}
  case A
// expected-opt-in-note @-1 {{enum case 'A' is not '@usableFromInline' or public}}
  case B
}
#else
private enum HiddenEnum {
// expected-not-opt-in-note @-1 2 {{enum 'HiddenEnum' is not '@usableFromInline' or public}}
  case A
// expected-not-opt-in-note @-1 {{enum case 'A' is not '@usableFromInline' or public}}
  case B
}
#endif

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

  let _: ExposedEnumPublic = ExposedEnumPublic.A
  let _: ExposedEnumPrivate = ExposedEnumPrivate.A
  // expected-error @-1 2 {{enum 'ExposedEnumPrivate' is private and cannot be referenced from an '@inlinable' function}}
  // expected-error @-2 {{enum case 'A' is private and cannot be referenced from an '@inlinable' function}}
  let _: HiddenEnum = HiddenEnum.A
  // expected-error @-1 2 {{enum 'HiddenEnum' is private and cannot be referenced from an '@inlinable' function}}
  // expected-error @-2 {{enum case 'A' is private and cannot be referenced from an '@inlinable' function}}
}

public func implicitlyInlinablePublic() {
  let _: ExposedLayoutPublic = ExposedLayoutPublic()
  let _: ExposedLayoutPrivate = ExposedLayoutPrivate()
  let _: HiddenLayout = HiddenLayout()
  // expected-embedded-opt-in-error @-1 2 {{struct 'HiddenLayout' cannot be used in an embedded function not marked '@export(interface)' because 'HiddenLayout' is marked '@_implementationOnly'}}

  let _: ExposedEnumPublic = ExposedEnumPublic.A
  let _: ExposedEnumPrivate = ExposedEnumPrivate.A
  let _: HiddenEnum = HiddenEnum.A
  // expected-embedded-opt-in-error @-1 2 {{enum 'HiddenEnum' cannot be used in an embedded function not marked '@export(interface)' because 'HiddenEnum' is marked '@_implementationOnly'}}
}

private func implicitlyInlinablePrivate() {
  let _: ExposedLayoutPublic = ExposedLayoutPublic()
  let _: ExposedLayoutPrivate = ExposedLayoutPrivate()
  let _: HiddenLayout = HiddenLayout()
  // expected-embedded-opt-in-error @-1 2 {{struct 'HiddenLayout' cannot be used in an embedded function not marked '@export(interface)' because 'HiddenLayout' is marked '@_implementationOnly'}}

  let _: ExposedEnumPublic = ExposedEnumPublic.A
  let _: ExposedEnumPrivate = ExposedEnumPrivate.A
  let _: HiddenEnum = HiddenEnum.A
  // expected-embedded-opt-in-error @-1 2 {{enum 'HiddenEnum' cannot be used in an embedded function not marked '@export(interface)' because 'HiddenEnum' is marked '@_implementationOnly'}}
}

@export(interface)
public func explicitNonInliable() {
  let _: ExposedLayoutPublic = ExposedLayoutPublic()
  let _: ExposedLayoutPrivate = ExposedLayoutPrivate()
  let _: HiddenLayout = HiddenLayout()
  let _: ExposedEnumPublic = ExposedEnumPublic.A
  let _: ExposedEnumPrivate = ExposedEnumPrivate.A
  let _: HiddenEnum = HiddenEnum.A
}

@export(interface)
internal func explicitNonInliableInternal() {
  let _: ExposedLayoutPublic = ExposedLayoutPublic()
  let _: ExposedLayoutPrivate = ExposedLayoutPrivate()
  let _: HiddenLayout = HiddenLayout()
  let _: ExposedEnumPublic = ExposedEnumPublic.A
  let _: ExposedEnumPrivate = ExposedEnumPrivate.A
  let _: HiddenEnum = HiddenEnum.A
}

/// Struct use sites

public struct ExposedLayoutPublicUser: ProtocolFromDirect {
// expected-error @-1 {{cannot use protocol 'ProtocolFromDirect' in a public or '@usableFromInline' conformance; 'directs' has been imported as implementation-only}}

  public var publicField: StructFromDirect
  // expected-error @-1 {{cannot use struct 'StructFromDirect' in a property declaration marked public or in a '@frozen' or '@usableFromInline' context; 'directs' has been imported as implementation-only}}

  private var privateField: StructFromDirect
  // expected-opt-in-error @-1 {{cannot use struct 'StructFromDirect' in a property declaration marked public or in a '@frozen' or '@usableFromInline' context; 'directs' has been imported as implementation-only}}

  private var a: ExposedLayoutPublic
  private var aa: ExposedLayoutInternal
  private var b: ExposedLayoutPrivate

  private var c: HiddenLayout
  // expected-opt-in-error @-1 {{cannot use struct 'HiddenLayout' in a property declaration marked public or in a '@frozen' or '@usableFromInline' context; 'HiddenLayout' is marked '@_implementationOnly'}}

  private var d: ExposedEnumPublic
  private var e: ExposedEnumPrivate
  private var f: HiddenEnum
  // expected-opt-in-error @-1 {{cannot use enum 'HiddenEnum' in a property declaration marked public or in a '@frozen' or '@usableFromInline' context; 'HiddenEnum' is marked '@_implementationOnly'}}

  private func privateFunc(h: HiddenLayout) {}
  // expected-embedded-opt-in-error @-1 {{struct 'HiddenLayout' cannot be used in an embedded function not marked '@export(interface)' because 'HiddenLayout' is marked '@_implementationOnly'}}
}

internal struct ExposedLayoutInternalUser: ProtocolFromDirect {
// expected-opt-in-error @-1 {{cannot use protocol 'ProtocolFromDirect' in a public or '@usableFromInline' conformance; 'directs' has been imported as implementation-only}}

  private var privateField: StructFromDirect
  // expected-opt-in-error @-1 {{cannot use struct 'StructFromDirect' in a property declaration marked public or in a '@frozen' or '@usableFromInline' context; 'directs' has been imported as implementation-only}}

  private var a: ExposedLayoutPublic
  private var aa: ExposedLayoutInternal
  private var b: ExposedLayoutPrivate
  private var c: HiddenLayout
  // expected-opt-in-error @-1 {{cannot use struct 'HiddenLayout' in a property declaration marked public or in a '@frozen' or '@usableFromInline' context; 'HiddenLayout' is marked '@_implementationOnly'}}

  private var d: ExposedEnumPublic
  private var e: ExposedEnumPrivate
  private var f: HiddenEnum
  // expected-opt-in-error @-1 {{cannot use enum 'HiddenEnum' in a property declaration marked public or in a '@frozen' or '@usableFromInline' context; 'HiddenEnum' is marked '@_implementationOnly'}}

  private func privateFunc(h: HiddenLayout) {}
  // expected-embedded-opt-in-error @-1 {{struct 'HiddenLayout' cannot be used in an embedded function not marked '@export(interface)' because 'HiddenLayout' is marked '@_implementationOnly'}}
}

private struct ExposedLayoutPrivateUser: ProtocolFromDirect {
// expected-opt-in-error @-1 {{cannot use protocol 'ProtocolFromDirect' in a public or '@usableFromInline' conformance; 'directs' has been imported as implementation-only}}

  private var privateField: StructFromDirect
  // expected-opt-in-error @-1 {{cannot use struct 'StructFromDirect' in a property declaration marked public or in a '@frozen' or '@usableFromInline' context; 'directs' has been imported as implementation-only}}

  private var a: ExposedLayoutPublic
  private var aa: ExposedLayoutInternal
  private var b: ExposedLayoutPrivate
  private var c: HiddenLayout
  // expected-opt-in-error @-1 {{cannot use struct 'HiddenLayout' in a property declaration marked public or in a '@frozen' or '@usableFromInline' context; 'HiddenLayout' is marked '@_implementationOnly'}}

  private var d: ExposedEnumPublic
  private var e: ExposedEnumPrivate
  private var f: HiddenEnum
  // expected-opt-in-error @-1 {{cannot use enum 'HiddenEnum' in a property declaration marked public or in a '@frozen' or '@usableFromInline' context; 'HiddenEnum' is marked '@_implementationOnly'}}

  private func privateFunc(h: HiddenLayout) {}
  // expected-embedded-opt-in-error @-1 {{struct 'HiddenLayout' cannot be used in an embedded function not marked '@export(interface)' because 'HiddenLayout' is marked '@_implementationOnly'}}
}

#if UseImplementationOnly
@_implementationOnly
private struct HiddenLayoutUser {
  public var publicField: StructFromDirect
  private var privateField: StructFromDirect
  private var a: ExposedLayoutPublic
  private var aa: ExposedLayoutInternal
  private var b: ExposedLayoutPrivate
  private var c: HiddenLayout
  private var d: ExposedEnumPublic
  private var e: ExposedEnumPrivate
  private var f: HiddenEnum

  @export(interface)
  private func privateFunc(h: HiddenLayout) {}
}

@_implementationOnly // expected-opt-in-error {{'@_implementationOnly' may not be used on public declarations}}
public struct PublicHiddenStruct {}


#endif

/// Enums use sites

public enum PublicEnumUser: ProtocolFromDirect {
// expected-error @-1 {{cannot use protocol 'ProtocolFromDirect' in a public or '@usableFromInline' conformance; 'directs' has been imported as implementation-only}}
    case a(StructFromDirect) // expected-error {{cannot use struct 'StructFromDirect' here; 'directs' has been imported as implementation-only}}
    case b(HiddenLayout) // expected-error {{enum case in a public enum uses a private type}}
    // expected-opt-in-error @-1 {{cannot use struct 'HiddenLayout' here; 'HiddenLayout' is marked '@_implementationOnly'}}
    case c(ExposedLayoutInternal) // expected-error {{enum case in a public enum uses an internal type}}
    case d(ExposedLayoutPrivate) // expected-error {{enum case in a public enum uses a private type}}
    case e(ExposedLayoutPublic)
}

internal enum InternalEnumUser: ProtocolFromDirect {
// expected-opt-in-error @-1 {{cannot use protocol 'ProtocolFromDirect' in a public or '@usableFromInline' conformance; 'directs' has been imported as implementation-only}}
    case a(StructFromDirect) // expected-opt-in-error {{cannot use struct 'StructFromDirect' here; 'directs' has been imported as implementation-only}}
    case b(HiddenLayout) // expected-opt-in-error {{cannot use struct 'HiddenLayout' here; 'HiddenLayout' is marked '@_implementationOnly'}}
    // expected-error @-1 {{enum case in an internal enum uses a private type}}
    case c(ExposedLayoutInternal)
    case d(ExposedLayoutPrivate) // expected-error {{enum case in an internal enum uses a private type}}
    case e(ExposedLayoutPublic)
}

private enum PrivateEnumUser: ProtocolFromDirect {
// expected-opt-in-error @-1 {{cannot use protocol 'ProtocolFromDirect' in a public or '@usableFromInline' conformance; 'directs' has been imported as implementation-only}}
    case a(StructFromDirect) // expected-opt-in-error {{cannot use struct 'StructFromDirect' here; 'directs' has been imported as implementation-only}}
    case b(HiddenLayout) // expected-opt-in-error {{cannot use struct 'HiddenLayout' here; 'HiddenLayout' is marked '@_implementationOnly'}}
    case c(ExposedLayoutInternal)
    case d(ExposedLayoutPrivate)
    case e(ExposedLayoutPublic)
}

internal enum InternalEnumWithRawType : RawTypeFromDirect { // expected-opt-in-error {{cannot use struct 'RawTypeFromDirect' in a public or '@usableFromInline' conformance; 'directs' has been imported as implementation-only}}
  typealias RawValue = RawTypeFromDirect
  case a
}

#if UseImplementationOnly
@_implementationOnly
private enum PrivateHiddenEnumUser: ProtocolFromDirect {
    case a(StructFromDirect)
    case b(HiddenLayout)
    case c(ExposedLayoutInternal)
    case d(ExposedLayoutPrivate)
    case e(ExposedLayoutPublic)
}

@_implementationOnly // expected-opt-in-error {{'@_implementationOnly' may not be used on public declarations}}
public enum PublicHiddenEnum {}

@_implementationOnly
internal enum InternalEnumWithRawTypeIO : RawTypeFromDirect {
  typealias RawValue = RawTypeFromDirect
  case a
}

#endif

/// Classes use sites

public class PublicClass: ProtocolFromDirect {
// expected-error @-1 {{cannot use protocol 'ProtocolFromDirect' in a public or '@usableFromInline' conformance; 'directs' has been imported as implementation-only}}

  public init() { fatalError() }

  public var publicField: StructFromDirect
  // expected-error @-1 {{cannot use struct 'StructFromDirect' in a property declaration marked public or in a '@frozen' or '@usableFromInline' context; 'directs' has been imported as implementation-only}}

  private var privateField: StructFromDirect
  // expected-opt-in-error @-1 {{cannot use struct 'StructFromDirect' in a property declaration marked public or in a '@frozen' or '@usableFromInline' context; 'directs' has been imported as implementation-only}}
  private var a: ExposedLayoutPublic
  private var aa: ExposedLayoutInternal
  private var b: ExposedLayoutPrivate
  private var c: HiddenLayout
  // expected-opt-in-error @-1 {{cannot use struct 'HiddenLayout' in a property declaration marked public or in a '@frozen' or '@usableFromInline' context; 'HiddenLayout' is marked '@_implementationOnly'}}

  private var d: ExposedEnumPublic
  private var e: ExposedEnumPrivate
  private var f: HiddenEnum
  // expected-opt-in-error @-1 {{cannot use enum 'HiddenEnum' in a property declaration marked public or in a '@frozen' or '@usableFromInline' context; 'HiddenEnum' is marked '@_implementationOnly'}}

  @export(interface)
  private func privateFunc(h: HiddenLayout) {}
}

internal class InternalClass: ProtocolFromDirect {
// expected-opt-in-error @-1 {{cannot use protocol 'ProtocolFromDirect' in a public or '@usableFromInline' conformance; 'directs' has been imported as implementation-only}}

  public init() { fatalError() }

  public var publicField: StructFromDirect
  // expected-opt-in-error @-1 {{cannot use struct 'StructFromDirect' in a property declaration marked public or in a '@frozen' or '@usableFromInline' context; 'directs' has been imported as implementation-only}}
  private var privateField: StructFromDirect
  // expected-opt-in-error @-1 {{cannot use struct 'StructFromDirect' in a property declaration marked public or in a '@frozen' or '@usableFromInline' context; 'directs' has been imported as implementation-only}}

  private var a: ExposedLayoutPublic
  private var aa: ExposedLayoutInternal
  private var b: ExposedLayoutPrivate
  private var c: HiddenLayout
  // expected-opt-in-error @-1 {{cannot use struct 'HiddenLayout' in a property declaration marked public or in a '@frozen' or '@usableFromInline' context; 'HiddenLayout' is marked '@_implementationOnly'}}

  private var d: ExposedEnumPublic
  private var e: ExposedEnumPrivate
  private var f: HiddenEnum
  // expected-opt-in-error @-1 {{cannot use enum 'HiddenEnum' in a property declaration marked public or in a '@frozen' or '@usableFromInline' context; 'HiddenEnum' is marked '@_implementationOnly'}}

  private func privateFunc(h: HiddenLayout) {} // expected-embedded-opt-in-error {{struct 'HiddenLayout' cannot be used in an embedded function not marked '@export(interface)' because 'HiddenLayout' is marked '@_implementationOnly'}}
}

private class PrivateClass: ProtocolFromDirect {
// expected-opt-in-error @-1 {{cannot use protocol 'ProtocolFromDirect' in a public or '@usableFromInline' conformance; 'directs' has been imported as implementation-only}}

  public init() { fatalError() }

  public var publicField: StructFromDirect
  // expected-opt-in-error @-1 {{cannot use struct 'StructFromDirect' in a property declaration marked public or in a '@frozen' or '@usableFromInline' context; 'directs' has been imported as implementation-only}}
  private var privateField: StructFromDirect
  // expected-opt-in-error @-1 {{cannot use struct 'StructFromDirect' in a property declaration marked public or in a '@frozen' or '@usableFromInline' context; 'directs' has been imported as implementation-only}}

  private var a: ExposedLayoutPublic
  private var aa: ExposedLayoutInternal
  private var b: ExposedLayoutPrivate
  private var c: HiddenLayout
  // expected-opt-in-error @-1 {{cannot use struct 'HiddenLayout' in a property declaration marked public or in a '@frozen' or '@usableFromInline' context; 'HiddenLayout' is marked '@_implementationOnly'}}

  private var d: ExposedEnumPublic
  private var e: ExposedEnumPrivate
  private var f: HiddenEnum
  // expected-opt-in-error @-1 {{cannot use enum 'HiddenEnum' in a property declaration marked public or in a '@frozen' or '@usableFromInline' context; 'HiddenEnum' is marked '@_implementationOnly'}}

  private func privateFunc(h: HiddenLayout) {} // expected-embedded-opt-in-error {{struct 'HiddenLayout' cannot be used in an embedded function not marked '@export(interface)' because 'HiddenLayout' is marked '@_implementationOnly'}}
}

#if UseImplementationOnly
@_implementationOnly
internal class HiddenClass: ProtocolFromDirect {
  public init() { fatalError() }

  public var publicField: StructFromDirect
  private var privateField: StructFromDirect

  private var a: ExposedLayoutPublic
  private var aa: ExposedLayoutInternal
  private var b: ExposedLayoutPrivate
  private var c: HiddenLayout

  private var d: ExposedEnumPublic
  private var e: ExposedEnumPrivate
  private var f: HiddenEnum
}

@_implementationOnly // expected-opt-in-error {{'@_implementationOnly' may not be used on public declarations}}
public enum PublicHiddenClass {}
#endif
