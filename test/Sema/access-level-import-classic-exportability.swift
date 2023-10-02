// RUN: %empty-directory(%t)
// RUN: split-file --leading-lines %s %t

/// Build the libraries.
// RUN: %target-swift-frontend -emit-module %t/PublicLib.swift -o %t \
// RUN:   -enable-library-evolution
// RUN: %target-swift-frontend -emit-module %t/PackageLib.swift -o %t \
// RUN:   -enable-library-evolution
// RUN: %target-swift-frontend -emit-module %t/InternalLib.swift -o %t \
// RUN:   -enable-library-evolution
// RUN: %target-swift-frontend -emit-module %t/FileprivateLib.swift -o %t \
// RUN:   -enable-library-evolution
// RUN: %target-swift-frontend -emit-module %t/PrivateLib.swift -o %t \
// RUN:   -enable-library-evolution

/// Check diagnostics.
// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   -package-name TestPackage -swift-version 5 \
// RUN:   -enable-experimental-feature AccessLevelOnImport -verify

/// Check diagnostics with library-evolution.
// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   -package-name TestPackage -swift-version 5 \
// RUN:   -enable-library-evolution \
// RUN:   -enable-experimental-feature AccessLevelOnImport -verify

//--- PublicLib.swift
public struct PublicImportType {
    public init() {}
}

//--- PackageLib.swift
public struct PackageImportType {
    public init() {}
}

//--- InternalLib.swift
public struct InternalImportType {
    public init() {}
}

//--- FileprivateLib.swift
public struct FileprivateImportType {
    public init() {}
}

//--- PrivateLib.swift
public struct PrivateImportType {
    public init() {}
}

//--- Client.swift
public import PublicLib
package import PackageLib // expected-note 2 {{struct 'PackageImportType' imported as 'package' from 'PackageLib' here}}
internal import InternalLib // expected-note 2 {{struct 'InternalImportType' imported as 'internal' from 'InternalLib' here}}
fileprivate import FileprivateLib // expected-note 2 {{struct 'FileprivateImportType' imported as 'fileprivate' from 'FileprivateLib' here}}
private import PrivateLib // expected-note 2 {{struct 'PrivateImportType' imported as 'private' from 'PrivateLib' here}}

public protocol PublicConstrainedExtensionProto {}
extension Array: PublicConstrainedExtensionProto where Element == PublicImportType {}

extension PublicImportType {
    public func foo() {}
}

public protocol PackageConstrainedExtensionProto {}
extension Array: PackageConstrainedExtensionProto where Element == PackageImportType {} // expected-error {{cannot use struct 'PackageImportType' in an extension with conditional conformances; 'PackageLib' was not imported publicly}}

extension PackageImportType { // expected-error {{cannot use struct 'PackageImportType' in an extension with public or '@usableFromInline' members; 'PackageLib' was not imported publicly}}
    public func foo() {}
}

public protocol InternalConstrainedExtensionProto {}
extension Array: InternalConstrainedExtensionProto where Element == InternalImportType {} // expected-error {{cannot use struct 'InternalImportType' in an extension with conditional conformances; 'InternalLib' was not imported publicly}}

extension InternalImportType { // expected-error {{cannot use struct 'InternalImportType' in an extension with public or '@usableFromInline' members; 'InternalLib' was not imported publicly}}
    public func foo() {}
}

public protocol FileprivateConstrainedExtensionProto {}
extension Array: FileprivateConstrainedExtensionProto where Element == FileprivateImportType {} // expected-error {{cannot use struct 'FileprivateImportType' in an extension with conditional conformances; 'FileprivateLib' was not imported publicly}}

extension FileprivateImportType { // expected-error {{cannot use struct 'FileprivateImportType' in an extension with public or '@usableFromInline' members; 'FileprivateLib' was not imported publicly}}
    public func foo() {}
}

public protocol PrivateConstrainedExtensionProto {}
extension Array: PrivateConstrainedExtensionProto where Element == PrivateImportType {} // expected-error {{cannot use struct 'PrivateImportType' in an extension with conditional conformances; 'PrivateLib' was not imported publicly}}

extension PrivateImportType { // expected-error {{cannot use struct 'PrivateImportType' in an extension with public or '@usableFromInline' members; 'PrivateLib' was not imported publicly}}
    public func foo() {}
}
