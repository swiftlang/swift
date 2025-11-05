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
/// Will also show errors in non-@_neverEmitIntoClient functions.

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
}

private struct ExposedLayoutPrivate {
// expected-note @-1 2 {{struct 'ExposedLayoutPrivate' is not '@usableFromInline' or public}}
  init() { fatalError() } // expected-note {{initializer 'init()' is not '@usableFromInline' or public}}
}

#if UseImplementationOnly
@_implementationOnly
private struct HiddenLayout {
// expected-opt-in-note @-1 2 {{struct 'HiddenLayout' is not '@usableFromInline' or public}}
// expected-opt-in-note @-2 1 {{initializer 'init()' is not '@usableFromInline' or public}}
// expected-opt-in-note @-3 2 {{struct declared here}}
// expected-opt-in-note @-4 {{struct declared here}}
}
#else
private struct HiddenLayout {
// expected-not-opt-in-note @-1 2 {{struct 'HiddenLayout' is not '@usableFromInline' or public}}
// expected-not-opt-in-note @-2 1 {{initializer 'init()' is not '@usableFromInline' or public}}
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
}

public func implicitlyInlinablePublic() {
  let _: ExposedLayoutPublic = ExposedLayoutPublic()
  let _: ExposedLayoutPrivate = ExposedLayoutPrivate()
  let _: HiddenLayout = HiddenLayout()
  // expected-embedded-opt-in-error @-1 2 {{struct 'HiddenLayout' cannot be used in an embedded function not marked '@_neverEmitIntoClient' because it is a struct marked '@_implementationOnly'}}

  let _: ExposedEnumPublic = ExposedEnumPublic.A
  let _: ExposedEnumPrivate = ExposedEnumPrivate.A
}

private func implicitlyInlinablePrivate() {
  let _: ExposedLayoutPublic = ExposedLayoutPublic()
  let _: ExposedLayoutPrivate = ExposedLayoutPrivate()
  let _: HiddenLayout = HiddenLayout()
  // expected-embedded-opt-in-error @-1 2 {{struct 'HiddenLayout' cannot be used in an embedded function not marked '@_neverEmitIntoClient' because it is a struct marked '@_implementationOnly'}}

  let _: ExposedEnumPublic = ExposedEnumPublic.A
  let _: ExposedEnumPrivate = ExposedEnumPrivate.A
}

@_neverEmitIntoClient
public func explicitNonInliable() {
  let _: ExposedLayoutPublic = ExposedLayoutPublic()
  let _: ExposedLayoutPrivate = ExposedLayoutPrivate()
  let _: HiddenLayout = HiddenLayout()
  let _: ExposedEnumPublic = ExposedEnumPublic.A
  let _: ExposedEnumPrivate = ExposedEnumPrivate.A
}

@_neverEmitIntoClient
internal func explicitNonInliableInternal() {
  let _: ExposedLayoutPublic = ExposedLayoutPublic()
  let _: ExposedLayoutPrivate = ExposedLayoutPrivate()
  let _: HiddenLayout = HiddenLayout()
  let _: ExposedEnumPublic = ExposedEnumPublic.A
  let _: ExposedEnumPrivate = ExposedEnumPrivate.A
}

/// Struct use sites

public struct ExposedLayoutPublicUser {

  public var publicField: StructFromDirect
  // expected-error @-1 {{cannot use struct 'StructFromDirect' in a property declaration marked public or in a '@frozen' or '@usableFromInline' context; 'directs' has been imported as implementation-only}}

  private var privateField: StructFromDirect
  // expected-opt-in-error @-1 {{cannot use struct 'StructFromDirect' in a property declaration marked public or in a '@frozen' or '@usableFromInline' context; 'directs' has been imported as implementation-only}}

  private var a: ExposedLayoutPublic
  private var aa: ExposedLayoutInternal
  private var b: ExposedLayoutPrivate

  private var c: HiddenLayout
  // expected-opt-in-error @-1 {{cannot use struct 'HiddenLayout' in a property declaration marked public or in a '@frozen' or '@usableFromInline' context; it is a struct marked '@_implementationOnly'}}

  private func privateFunc(h: HiddenLayout) {}
  // expected-embedded-opt-in-error @-1 {{struct 'HiddenLayout' cannot be used in an embedded function not marked '@_neverEmitIntoClient' because it is a struct marked '@_implementationOnly'}}
}

private struct ExposedLayoutInternalUser {

  private var privateField: StructFromDirect
  // expected-opt-in-error @-1 {{cannot use struct 'StructFromDirect' in a property declaration marked public or in a '@frozen' or '@usableFromInline' context; 'directs' has been imported as implementation-only}}

  private var a: ExposedLayoutPublic
  private var aa: ExposedLayoutInternal
  private var b: ExposedLayoutPrivate
  private var c: HiddenLayout
  // expected-opt-in-error @-1 {{cannot use struct 'HiddenLayout' in a property declaration marked public or in a '@frozen' or '@usableFromInline' context; it is a struct marked '@_implementationOnly'}}

  private func privateFunc(h: HiddenLayout) {}
  // expected-embedded-opt-in-error @-1 {{struct 'HiddenLayout' cannot be used in an embedded function not marked '@_neverEmitIntoClient' because it is a struct marked '@_implementationOnly'}}
}

private struct ExposedLayoutPrivateUser {

  private var privateField: StructFromDirect
  // expected-opt-in-error @-1 {{cannot use struct 'StructFromDirect' in a property declaration marked public or in a '@frozen' or '@usableFromInline' context; 'directs' has been imported as implementation-only}}

  private var a: ExposedLayoutPublic
  private var aa: ExposedLayoutInternal
  private var b: ExposedLayoutPrivate
  private var c: HiddenLayout
  // expected-opt-in-error @-1 {{cannot use struct 'HiddenLayout' in a property declaration marked public or in a '@frozen' or '@usableFromInline' context; it is a struct marked '@_implementationOnly'}}

  private func privateFunc(h: HiddenLayout) {}
  // expected-embedded-opt-in-error @-1 {{struct 'HiddenLayout' cannot be used in an embedded function not marked '@_neverEmitIntoClient' because it is a struct marked '@_implementationOnly'}}
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

  @_neverEmitIntoClient
  private func privateFunc(h: HiddenLayout) {}
}

@_implementationOnly // expected-opt-in-error {{'@_implementationOnly' may not be used on public declarations}}
public struct PublicHiddenStruct {}
#endif
