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
// RUN:   -primary-file %t/main.swift \
// RUN:   %t/imports.swift \
// RUN:   -I %t -package-name Package \
// RUN:   -enable-upcoming-feature MemberImportVisibility:migrate

// REQUIRES: swift_feature_MemberImportVisibility

//--- main.swift

// FIXME: The access level on the fix-it for PackageUsesOnly is wrong.
import Swift // Just here to anchor the fix-its
// expected-warning      {{import of module 'InternalUsesOnly' is required}}{{1-1=internal import InternalUsesOnly\n}}
// expected-warning@-1   {{import of module 'InternalUsesOnlyDefaultedImport' is required}}{{1-1=import InternalUsesOnlyDefaultedImport\n}}
// expected-warning@-2   {{import of module 'PackageUsesOnly' is required}}{{1-1=public import PackageUsesOnly\n}}
// expected-warning@-3   {{import of module 'PublicUsesOnly' is required}}{{1-1=public import PublicUsesOnly\n}}
// expected-warning@-4   {{import of module 'PublicUsesOnlyDefaultedImport' is required}}{{1-1=import PublicUsesOnlyDefaultedImport\n}}
// expected-warning@-5   {{import of module 'MixedUses' is required}}{{1-1=public import MixedUses\n}}
// expected-warning@-6   {{import of module 'InternalUsesOnlyReexported' is required}}{{1-1=internal import InternalUsesOnlyReexported\n}}
// expected-warning@-7   {{import of module 'InternalUsesOnlyTransitivelyImported' is required}}{{1-1=internal import InternalUsesOnlyTransitivelyImported\n}}
// expected-warning@-8   {{import of module 'InternalUsesOnlySPIOnly' is required}}{{1-1=internal import InternalUsesOnlySPIOnly\n}}
// expected-warning@-9   {{import of module 'InternalUsesOnlyDefaultedImportSPIOnly' is required}}{{1-1=@_spiOnly import InternalUsesOnlyDefaultedImportSPIOnly\n}}
// expected-warning@-10  {{import of module 'PublicUsesOnlySPIOnly' is required}}{{1-1=@_spiOnly public import PublicUsesOnlySPIOnly\n}}

func internalFunc(_ x: Int) {
  _ = x.memberInInternalUsesOnly // expected-note {{property 'memberInInternalUsesOnly' from 'InternalUsesOnly' used here}}
  _ = x.memberInInternalUsesOnlyDefaultedImport // expected-note {{property 'memberInInternalUsesOnlyDefaultedImport' from 'InternalUsesOnlyDefaultedImport' used here}}
  _ = x.memberInMixedUses // expected-note {{property 'memberInMixedUses' from 'MixedUses' used here}}
  _ = x.memberInInternalUsesOnlyReexported // expected-note {{property 'memberInInternalUsesOnlyReexported' from 'InternalUsesOnlyReexported' used here}}
  _ = x.memberInInternalUsesOnlySPIOnly // expected-note {{property 'memberInInternalUsesOnlySPIOnly' from 'InternalUsesOnlySPIOnly' used here}}
  _ = x.memberInInternalUsesOnlyDefaultedImportSPIOnly // expected-note {{property 'memberInInternalUsesOnlyDefaultedImportSPIOnly' from 'InternalUsesOnlyDefaultedImportSPIOnly' used here}}
  _ = x.memberInInternalUsesOnlyTransitivelyImported // expected-note {{property 'memberInInternalUsesOnlyTransitivelyImported' from 'InternalUsesOnlyTransitivelyImported' used here}}
}

@inlinable package func packageInlinableFunc(_ x: Int) {
  _ = x.memberInPackageUsesOnly // expected-note {{property 'memberInPackageUsesOnly' from 'PackageUsesOnly' used here}}
  _ = x.memberInMixedUses // expected-note {{property 'memberInMixedUses' from 'MixedUses' used here}}
}

@inlinable public func inlinableFunc(_ x: Int) {
  _ = x.memberInPublicUsesOnly // expected-note {{property 'memberInPublicUsesOnly' from 'PublicUsesOnly' used here}}
  _ = x.memberInPublicUsesOnlyDefaultedImport // expected-note {{property 'memberInPublicUsesOnlyDefaultedImport' from 'PublicUsesOnlyDefaultedImport' used here}}
  _ = x.memberInMixedUses // expected-note {{property 'memberInMixedUses' from 'MixedUses' used here}}
  _ = x.memberInPublicUsesOnlySPIOnly // expected-note {{property 'memberInPublicUsesOnlySPIOnly' from 'PublicUsesOnlySPIOnly' used here}}
}

extension Int {
  private func usesTypealiasInInternalUsesOnly_Private(x: TypealiasInInternalUsesOnly) {} // expected-note {{type alias 'TypealiasInInternalUsesOnly' from 'InternalUsesOnly' used here}}
  internal func usesTypealiasInInternalUsesOnly(x: TypealiasInInternalUsesOnly) {} // expected-note {{type alias 'TypealiasInInternalUsesOnly' from 'InternalUsesOnly' used here}}
  package func usesTypealiasInPackageUsesOnly(x: TypealiasInPackageUsesOnly) {} // expected-note {{type alias 'TypealiasInPackageUsesOnly' from 'PackageUsesOnly' used here}}
  public func usesTypealiasInPublicUsesOnly(x: TypealiasInPublicUsesOnly) {} // expected-note {{type alias 'TypealiasInPublicUsesOnly' from 'PublicUsesOnly' used here}}
  public func usesTypealiasInMixedUses(x: TypealiasInMixedUses) {} // expected-note {{type alias 'TypealiasInMixedUses' from 'MixedUses' used here}}
  internal func usesTypealiasInMixedUses_Internal(x: TypealiasInMixedUses) {} // expected-note {{type alias 'TypealiasInMixedUses' from 'MixedUses' used here}}
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
  public var memberInInternalUsesOnly: Int { return self }
}

//--- InternalUsesOnlyDefaultedImport.swift

extension Int {
  public typealias TypealiasInInternalUsesOnlyDefaultedImport = Self
  public var memberInInternalUsesOnlyDefaultedImport: Int { return self }
}

//--- PackageUsesOnly.swift

extension Int {
  public typealias TypealiasInPackageUsesOnly = Self
  public var memberInPackageUsesOnly: Int { return self }
}

//--- PublicUsesOnly.swift

extension Int {
  public typealias TypealiasInPublicUsesOnly = Self
  public var memberInPublicUsesOnly: Int { return self }
}

//--- PublicUsesOnlyDefaultedImport.swift

extension Int {
  public typealias TypealiasInPublicUsesOnlyDefaultedImport = Self
  public var memberInPublicUsesOnlyDefaultedImport: Int { return self }
}

//--- MixedUses.swift

extension Int {
  public typealias TypealiasInMixedUses = Self
  public var memberInMixedUses: Int { return self }
}

//--- InternalUsesOnlyReexported.swift

extension Int {
  public typealias TypealiasInInternalUsesOnlyReexported = Self
  public var memberInInternalUsesOnlyReexported: Int { return self }
}

//--- InternalUsesOnlyTransitivelyImported.swift

extension Int {
  public typealias TypealiasInInternalUsesOnlyTransitivelyImported = Self
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
