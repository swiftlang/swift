/// Report the most restricted types when many types are problematic.

// RUN: %empty-directory(%t)
// RUN: split-file --leading-lines %s %t

/// Build the libraries.
// RUN: %target-swift-frontend -emit-module %t/PublicLib.swift -o %t \
// RUN:   -package-name pkg
// RUN: %target-swift-frontend -emit-module %t/PackageLib.swift -o %t \
// RUN:   -package-name pkg
// RUN: %target-swift-frontend -emit-module %t/InternalLib.swift -o %t
// RUN: %target-swift-frontend -emit-module %t/FileprivateLib.swift -o %t
// RUN: %target-swift-frontend -emit-module %t/PrivateLib.swift -o %t

/// Check diagnostics.
// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   -package-name pkg -verify
// RUN: %target-swift-frontend -typecheck %t/PackageTypeImportedAsPackageClient.swift -I %t \
// RUN:   -package-name pkg -verify
// RUN: %target-swift-frontend -typecheck %t/LocalVsImportClient.swift -I %t \
// RUN:   -package-name pkg -verify

//--- PublicLib.swift
public struct PublicImportType {}
package struct PackageLevelPublicImportedType {}

//--- PackageLib.swift
public struct PackageImportType {}
package struct PackageLevelPackageImportedType {}

//--- InternalLib.swift
public struct InternalImportType {}

//--- FileprivateLib.swift
public struct FileprivateImportType {}

//--- PrivateLib.swift
public struct PrivateImportType {}

//--- Client.swift
public import PublicLib
package import PackageLib // expected-note {{struct 'PackageImportType' imported as 'package' from 'PackageLib' here}}
internal import InternalLib // expected-note {{struct 'InternalImportType' imported as 'internal' from 'InternalLib' here}}
fileprivate import FileprivateLib // expected-note 3 {{struct 'FileprivateImportType' imported as 'fileprivate' from 'FileprivateLib' here}}
private import PrivateLib // expected-note 1 {{struct 'PrivateImportType' imported as 'private' from 'PrivateLib' here}}

/// Simple ordering
public func publicFuncUsesPrivate(_ a: PublicImportType, b: PackageImportType, c: InternalImportType, d: FileprivateImportType, e: PrivateImportType) { // expected-error {{function cannot be declared public because its parameter uses a fileprivate type}}
// expected-note @-1 {{struct 'FileprivateImportType' is imported by this file as 'fileprivate' from 'FileprivateLib'}}
    var _: PrivateImportType
}
public func publicFuncUsesFileprivate(_ a: PublicImportType, b: PackageImportType, c: InternalImportType, d: FileprivateImportType) { // expected-error {{function cannot be declared public because its parameter uses a fileprivate type}}
// expected-note @-1 {{struct 'FileprivateImportType' is imported by this file as 'fileprivate' from 'FileprivateLib'}}
    var _: PrivateImportType
}
public func publicFuncUsesInternal(_ a: PublicImportType, b: PackageImportType, c: InternalImportType) { // expected-error {{function cannot be declared public because its parameter uses an internal type}}
// expected-note @-1 {{struct 'InternalImportType' is imported by this file as 'internal' from 'InternalLib'}}
    var _: PrivateImportType
}
public func publicFuncUsesPackage(_ a: PublicImportType, b: PackageImportType) { // expected-error {{function cannot be declared public because its parameter uses a package type}}
// expected-note @-1 {{struct 'PackageImportType' is imported by this file as 'package' from 'PackageLib'}}
    var _: PrivateImportType
}

/// Disordered
public func publicFuncUsesPrivateScambled(_ a: PublicImportType, b: PackageImportType, e: PrivateImportType, c: InternalImportType, d: FileprivateImportType) { // expected-error {{function cannot be declared public because its parameter uses a private type}}
// expected-note @-1 {{struct 'PrivateImportType' is imported by this file as 'private' from 'PrivateLib'}}
    var _: PrivateImportType
}
public func publicFuncUsesPrivateScambled(_ a: PublicImportType, d: FileprivateImportType, b: PackageImportType, c: InternalImportType) { // expected-error {{function cannot be declared public because its parameter uses a fileprivate type}}
// expected-note @-1 {{struct 'FileprivateImportType' is imported by this file as 'fileprivate' from 'FileprivateLib'}}
    var _: PrivateImportType
}

//--- PackageTypeImportedAsPackageClient.swift
/// Report errors about using package decls in public but don't note the import
/// as it doesn't affect the access level of the decls.

public import PublicLib
package import PackageLib

public func publicFuncUsesPackageLevelPublicImportedType(_ a: PackageLevelPublicImportedType) {} // expected-error {{function cannot be declared public because its parameter uses a package type}}
public func publicFuncUsesPackageLevelPackageImportedType(_ a: PackageLevelPackageImportedType) {} // expected-error {{function cannot be declared public because its parameter uses a package type}}

@inlinable public func funcInlinableReferenceToPublicImportedType() {
  var _: PackageLevelPublicImportedType // expected-error {{struct 'PackageLevelPublicImportedType' is package and cannot be referenced from an '@inlinable' function}}
  var _: Array<PackageLevelPublicImportedType> // expected-error {{struct 'PackageLevelPublicImportedType' is package and cannot be referenced from an '@inlinable' function}}
}

@inlinable public func funcInlinableReferenceToPackageImportedType() {
  var _: PackageLevelPackageImportedType // expected-error {{struct 'PackageLevelPackageImportedType' is package and cannot be referenced from an '@inlinable' function}}
  var _: Array<PackageLevelPackageImportedType> // expected-error {{struct 'PackageLevelPackageImportedType' is package and cannot be referenced from an '@inlinable' function}}
}

/// Local vs imports
//--- LocalVsImportClient.swift
internal import InternalLib
fileprivate import FileprivateLib // expected-note {{struct 'FileprivateImportType' imported as 'fileprivate' from 'FileprivateLib' here}}

fileprivate struct LocalType {} // expected-note 3 {{type declared here}}
public func localVsImportedType1(a: LocalType, b: InternalImportType) {} // expected-error {{function cannot be declared public because its parameter uses a fileprivate type}}
public func localVsImportedType2(a: InternalImportType, b: LocalType) {} // expected-error {{function cannot be declared public because its parameter uses a fileprivate type}}
public func localVsImportedType3(a: LocalType, b: FileprivateImportType) {} // expected-error {{function cannot be declared public because its parameter uses a fileprivate type}}

/// Only this one points to the imported type.
public func localVsImportedType4(a: FileprivateImportType, b: LocalType) {} // expected-error {{function cannot be declared public because its parameter uses a fileprivate type}}
// expected-note @-1 {{struct 'FileprivateImportType' is imported by this file as 'fileprivate' from 'FileprivateLib'}}
