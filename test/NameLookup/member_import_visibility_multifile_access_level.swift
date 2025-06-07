// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module -o %t %t/InternalUsesOnly.swift
// RUN: %target-swift-frontend -emit-module -o %t %t/InternalUsesOnlyDefaultedImport.swift
// RUN: %target-swift-frontend -emit-module -o %t %t/PackageUsesOnly.swift
// RUN: %target-swift-frontend -emit-module -o %t %t/PublicUsesOnly.swift
// RUN: %target-swift-frontend -emit-module -o %t %t/PublicUsesOnlyDefaultedImport.swift
// RUN: %target-swift-frontend -emit-module -o %t %t/MixedUses.swift
// RUN: %target-swift-frontend -emit-module -o %t %t/InternalUsesOnlyReexported.swift
// RUN: %target-swift-frontend -emit-module -o %t %t/InternalUsesOnlyTransitivelyImported.swift
// RUN: %target-swift-frontend -emit-module -o %t %t/ImportsOtherModules.swift -I %t
// RUN: %target-swift-frontend -emit-module -o %t %t/InternalUsesOnlySPIOnly.swift -I %t
// RUN: %target-swift-frontend -emit-module -o %t %t/InternalUsesOnlyDefaultedImportSPIOnly.swift -I %t
// RUN: %target-swift-frontend -emit-module -o %t %t/PublicUsesOnlySPIOnly.swift -I %t

// RUN: %target-swift-frontend -typecheck -verify -swift-version 5 \
// RUN:   -primary-file %t/function_bodies.swift \
// RUN:   -primary-file %t/function_signatures_unqualified.swift \
// RUN:   -primary-file %t/function_signatures_qualified.swift \
// RUN:   -primary-file %t/extensions.swift \
// RUN:   %t/imports.swift \
// RUN:   -I %t -package-name Package \
// RUN:   -enable-upcoming-feature MemberImportVisibility \
// RUN:   -verify-additional-prefix public-by-default-

// RUN: %target-swift-frontend -typecheck -verify -swift-version 5 \
// RUN:   -primary-file %t/function_bodies.swift \
// RUN:   -primary-file %t/function_signatures_unqualified.swift \
// RUN:   -primary-file %t/function_signatures_qualified.swift \
// RUN:   -primary-file %t/extensions.swift \
// RUN:   %t/imports.swift \
// RUN:   -I %t -package-name Package \
// RUN:   -enable-upcoming-feature MemberImportVisibility \
// RUN:   -enable-upcoming-feature InternalImportsByDefault \
// RUN:   -verify-additional-prefix internal-by-default-

// REQUIRES: swift_feature_InternalImportsByDefault
// REQUIRES: swift_feature_MemberImportVisibility

//--- function_bodies.swift

// FIXME: The access level on the fix-it for PackageUsesOnly is wrong.
import Swift // Just here to anchor the fix-its
// expected-note      {{add import of module 'InternalUsesOnly'}}{{1-1=internal import InternalUsesOnly\n}}
// expected-note@-1   {{add import of module 'InternalUsesOnlyDefaultedImport'}}{{1-1=import InternalUsesOnlyDefaultedImport\n}}
// expected-note@-2   {{add import of module 'PackageUsesOnly'}}{{1-1=public import PackageUsesOnly\n}}
// expected-note@-3   {{add import of module 'PublicUsesOnly'}}{{1-1=public import PublicUsesOnly\n}}
// expected-note@-4   {{add import of module 'PublicUsesOnlyDefaultedImport'}}{{1-1=import PublicUsesOnlyDefaultedImport\n}}
// expected-note@-5 3 {{add import of module 'MixedUses'}}{{1-1=public import MixedUses\n}}
// expected-public-by-default-note@-6   {{add import of module 'InternalUsesOnlyReexported'}}{{1-1=internal import InternalUsesOnlyReexported\n}}
// expected-internal-by-default-note@-7   {{add import of module 'InternalUsesOnlyReexported'}}{{1-1=import InternalUsesOnlyReexported\n}}
// expected-public-by-default-note@-8   {{add import of module 'InternalUsesOnlyTransitivelyImported'}}{{1-1=internal import InternalUsesOnlyTransitivelyImported\n}}
// expected-internal-by-default-note@-9   {{add import of module 'InternalUsesOnlyTransitivelyImported'}}{{1-1=import InternalUsesOnlyTransitivelyImported\n}}
// expected-note@-10   {{add import of module 'InternalUsesOnlySPIOnly'}}{{1-1=internal import InternalUsesOnlySPIOnly\n}}
// expected-public-by-default-note@-11     {{add import of module 'InternalUsesOnlyDefaultedImportSPIOnly'}}{{1-1=@_spiOnly import InternalUsesOnlyDefaultedImportSPIOnly\n}}
// expected-internal-by-default-note@-12   {{add import of module 'InternalUsesOnlyDefaultedImportSPIOnly'}}{{1-1=import InternalUsesOnlyDefaultedImportSPIOnly\n}}
// expected-note@-13  {{add import of module 'PublicUsesOnlySPIOnly'}}{{1-1=@_spiOnly public import PublicUsesOnlySPIOnly\n}}

func internalFunc(_ x: Int) {
  _ = x.memberInInternalUsesOnly // expected-error {{property 'memberInInternalUsesOnly' is not available due to missing import of defining module 'InternalUsesOnly'}}
  _ = x.memberInInternalUsesOnlyDefaultedImport // expected-error {{property 'memberInInternalUsesOnlyDefaultedImport' is not available due to missing import of defining module 'InternalUsesOnlyDefaultedImport'}}
  _ = x.memberInMixedUses // expected-error {{property 'memberInMixedUses' is not available due to missing import of defining module 'MixedUses'}}
  _ = x.memberInInternalUsesOnlyReexported // expected-error {{property 'memberInInternalUsesOnlyReexported' is not available due to missing import of defining module 'InternalUsesOnlyReexported'}}
  _ = x.memberInInternalUsesOnlySPIOnly // expected-error {{property 'memberInInternalUsesOnlySPIOnly' is not available due to missing import of defining module 'InternalUsesOnlySPIOnly'}}
  _ = x.memberInInternalUsesOnlyDefaultedImportSPIOnly // expected-error {{property 'memberInInternalUsesOnlyDefaultedImportSPIOnly' is not available due to missing import of defining module 'InternalUsesOnlyDefaultedImportSPIOnly'}}
  _ = x.memberInInternalUsesOnlyTransitivelyImported // expected-error {{property 'memberInInternalUsesOnlyTransitivelyImported' is not available due to missing import of defining module 'InternalUsesOnlyTransitivelyImported'}}
}

@inlinable package func packageInlinableFunc(_ x: Int) {
  _ = x.memberInPackageUsesOnly // expected-error {{property 'memberInPackageUsesOnly' is not available due to missing import of defining module 'PackageUsesOnly'}}
  _ = x.memberInMixedUses // expected-error {{property 'memberInMixedUses' is not available due to missing import of defining module 'MixedUses'}}
}

@inlinable public func inlinableFunc(_ x: Int) {
  _ = x.memberInPublicUsesOnly // expected-error {{property 'memberInPublicUsesOnly' is not available due to missing import of defining module 'PublicUsesOnly'}}
  _ = x.memberInPublicUsesOnlyDefaultedImport // expected-error {{property 'memberInPublicUsesOnlyDefaultedImport' is not available due to missing import of defining module 'PublicUsesOnlyDefaultedImport'}}
  _ = x.memberInMixedUses // expected-error {{property 'memberInMixedUses' is not available due to missing import of defining module 'MixedUses'}}
  _ = x.memberInPublicUsesOnlySPIOnly // expected-error {{property 'memberInPublicUsesOnlySPIOnly' is not available due to missing import of defining module 'PublicUsesOnlySPIOnly'}}
}

//--- function_signatures_unqualified.swift

import Swift // Just here to anchor the fix-its
// expected-note    2 {{add import of module 'InternalUsesOnly'}}{{1-1=internal import InternalUsesOnly\n}}
// expected-note@-1   {{add import of module 'PackageUsesOnly'}}{{1-1=package import PackageUsesOnly\n}}
// expected-note@-2   {{add import of module 'PublicUsesOnly'}}{{1-1=public import PublicUsesOnly\n}}
// expected-note@-3 2 {{add import of module 'MixedUses'}}{{1-1=public import MixedUses\n}}

extension Int {
  private func usesTypealiasInInternalUsesOnly_Private(x: TypealiasInInternalUsesOnly) {} // expected-error {{type alias 'TypealiasInInternalUsesOnly' is not available due to missing import of defining module 'InternalUsesOnly'}}
  internal func usesTypealiasInInternalUsesOnly(x: TypealiasInInternalUsesOnly) {} // expected-error {{type alias 'TypealiasInInternalUsesOnly' is not available due to missing import of defining module 'InternalUsesOnly'}}
  package func usesTypealiasInPackageUsesOnly(x: TypealiasInPackageUsesOnly) {} // expected-error {{type alias 'TypealiasInPackageUsesOnly' is not available due to missing import of defining module 'PackageUsesOnly'}}
  public func usesTypealiasInPublicUsesOnly(x: TypealiasInPublicUsesOnly) {} // expected-error {{type alias 'TypealiasInPublicUsesOnly' is not available due to missing import of defining module 'PublicUsesOnly'}}
  public func usesTypealiasInMixedUses(x: TypealiasInMixedUses) {} // expected-error {{type alias 'TypealiasInMixedUses' is not available due to missing import of defining module 'MixedUses'}}
  internal func usesTypealiasInMixedUses_Internal(x: TypealiasInMixedUses) {} // expected-error {{type alias 'TypealiasInMixedUses' is not available due to missing import of defining module 'MixedUses'}}
}

//--- function_signatures_qualified.swift

import Swift // Just here to anchor the fix-its
// expected-note    2 {{add import of module 'InternalUsesOnly'}}{{1-1=internal import InternalUsesOnly\n}}
// expected-note@-1   {{add import of module 'PackageUsesOnly'}}{{1-1=package import PackageUsesOnly\n}}
// expected-note@-2   {{add import of module 'PublicUsesOnly'}}{{1-1=public import PublicUsesOnly\n}}
// expected-note@-3 2 {{add import of module 'MixedUses'}}{{1-1=public import MixedUses\n}}

private func usesTypealiasInInternalUsesOnly_Private(x: Int.TypealiasInInternalUsesOnly) {} // expected-error {{type alias 'TypealiasInInternalUsesOnly' is not available due to missing import of defining module 'InternalUsesOnly'}}
internal func usesTypealiasInInternalUsesOnly(x: Int.TypealiasInInternalUsesOnly) {} // expected-error {{type alias 'TypealiasInInternalUsesOnly' is not available due to missing import of defining module 'InternalUsesOnly'}}
package func usesTypealiasInPackageUsesOnly(x: Int.TypealiasInPackageUsesOnly) {} // expected-error {{type alias 'TypealiasInPackageUsesOnly' is not available due to missing import of defining module 'PackageUsesOnly'}}
public func usesTypealiasInPublicUsesOnly(x: Int.TypealiasInPublicUsesOnly) {} // expected-error {{type alias 'TypealiasInPublicUsesOnly' is not available due to missing import of defining module 'PublicUsesOnly'}}
public func usesTypealiasInMixedUses(x: Int.TypealiasInMixedUses) {} // expected-error {{type alias 'TypealiasInMixedUses' is not available due to missing import of defining module 'MixedUses'}}
internal func usesTypealiasInMixedUses_Internal(x: Int.TypealiasInMixedUses) {} // expected-error {{type alias 'TypealiasInMixedUses' is not available due to missing import of defining module 'MixedUses'}}

//--- extensions.swift

import Swift // Just here to anchor the fix-its
// expected-note    2 {{add import of module 'InternalUsesOnly'}}{{1-1=internal import InternalUsesOnly\n}}
// expected-note@-1   {{add import of module 'PackageUsesOnly'}}{{1-1=package import PackageUsesOnly\n}}
// expected-note@-2   {{add import of module 'PublicUsesOnly'}}{{1-1=public import PublicUsesOnly\n}}
// expected-note@-3 2 {{add import of module 'MixedUses'}}{{1-1=public import MixedUses\n}}

extension Int.NestedInInternalUsesOnly { // expected-error {{struct 'NestedInInternalUsesOnly' is not available due to missing import of defining module 'InternalUsesOnly'}}
  private func privateMethod() {}
}

extension Int.NestedInInternalUsesOnly { // expected-error {{struct 'NestedInInternalUsesOnly' is not available due to missing import of defining module 'InternalUsesOnly'}}
  internal func internalMethod() {}
}

extension Int.NestedInPackageUsesOnly { // expected-error {{struct 'NestedInPackageUsesOnly' is not available due to missing import of defining module 'PackageUsesOnly'}}
  package func packageMethod() {}
}

extension Int.NestedInPublicUsesOnly { // expected-error {{struct 'NestedInPublicUsesOnly' is not available due to missing import of defining module 'PublicUsesOnly'}}
  public func publicMethod() {}
}

extension Int.NestedInMixedUses { // expected-error {{struct 'NestedInMixedUses' is not available due to missing import of defining module 'MixedUses'}}
  public func publicMethod() {}
}

extension Int.NestedInMixedUses { // expected-error {{struct 'NestedInMixedUses' is not available due to missing import of defining module 'MixedUses'}}
  internal func internalMethod() {}
}

//--- imports.swift

internal import InternalUsesOnly
import InternalUsesOnlyDefaultedImport
internal import PackageUsesOnly
internal import PublicUsesOnly
import PublicUsesOnlyDefaultedImport
internal import MixedUses
internal import ImportsOtherModules
@_spiOnly public import InternalUsesOnlySPIOnly
@_spiOnly import InternalUsesOnlyDefaultedImportSPIOnly
@_spiOnly public import PublicUsesOnlySPIOnly

//--- InternalUsesOnly.swift

extension Int {
  public typealias TypealiasInInternalUsesOnly = Self
  public struct NestedInInternalUsesOnly {}
  public var memberInInternalUsesOnly: Int { return self }
}

//--- InternalUsesOnlyDefaultedImport.swift

extension Int {
  public typealias TypealiasInInternalUsesOnlyDefaultedImport = Self
  public struct NestedInInternalUsesOnlyDefaultedImport {}
  public var memberInInternalUsesOnlyDefaultedImport: Int { return self }
}

//--- PackageUsesOnly.swift

extension Int {
  public typealias TypealiasInPackageUsesOnly = Self
  public struct NestedInPackageUsesOnly {}
  public var memberInPackageUsesOnly: Int { return self }
}

//--- PublicUsesOnly.swift

extension Int {
  public typealias TypealiasInPublicUsesOnly = Self
  public struct NestedInPublicUsesOnly {}
  public var memberInPublicUsesOnly: Int { return self }
}

//--- PublicUsesOnlyDefaultedImport.swift

extension Int {
  public typealias TypealiasInPublicUsesOnlyDefaultedImport = Self
  public struct NestedInPublicUsesOnlyDefaultedImport {}
  public var memberInPublicUsesOnlyDefaultedImport: Int { return self }
}

//--- MixedUses.swift

extension Int {
  public typealias TypealiasInMixedUses = Self
  public struct NestedInMixedUses {}
  public var memberInMixedUses: Int { return self }
}

//--- InternalUsesOnlyReexported.swift

extension Int {
  public typealias TypealiasInInternalUsesOnlyReexported = Self
  public struct NestedInInternalUsesOnlyReexported {}
  public var memberInInternalUsesOnlyReexported: Int { return self }
}

//--- InternalUsesOnlyTransitivelyImported.swift

extension Int {
  public typealias TypealiasInInternalUsesOnlyTransitivelyImported = Self
  public struct NestedInInternalUsesOnlyTransitivelyImported {}
  public var memberInInternalUsesOnlyTransitivelyImported: Int { return self }
}

//--- ImportsOtherModules.swift

@_exported import InternalUsesOnlyReexported
import InternalUsesOnlyTransitivelyImported

//--- InternalUsesOnlySPIOnly.swift

extension Int {
  public var memberInInternalUsesOnlySPIOnly: Int { return self }
}

//--- InternalUsesOnlyDefaultedImportSPIOnly.swift

extension Int {
  public var memberInInternalUsesOnlyDefaultedImportSPIOnly: Int { return self }
}

//--- PublicUsesOnlySPIOnly.swift

extension Int {
  public var memberInPublicUsesOnlySPIOnly: Int { return self }
}
