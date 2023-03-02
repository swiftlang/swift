/// Simple use case of a package only import.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// Build the libraries.
// RUN: %target-swift-frontend -emit-module %t/PackageImportedLib.swift -o %t \
// RUN:   -swift-version 5 -enable-library-evolution \
// RUN:   -enable-experimental-feature AccessLevelOnImport
// RUN: %target-swift-frontend -emit-module %t/PackageLib.swift -o %t \
// RUN:   -swift-version 5 -enable-library-evolution -I %t \
// RUN:   -enable-experimental-feature AccessLevelOnImport \
// RUN:   -package-name MyPackage

/// A client in the same package builds fine.
// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   -package-name MyPackage -I %t \
// RUN:   -enable-experimental-feature AccessLevelOnImport

/// A client outside of the package raises errors.
// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   -package-name NotMyPackage -I %t \
// RUN:   -enable-experimental-feature AccessLevelOnImport -verify

//--- PackageImportedLib.swift
public struct IndirectType {
    public init() {}
    public func someMethod() {}
}
public protocol IndirectProtocol {}

//--- PackageLib.swift
package import PackageImportedLib

package func getIndirectType() -> IndirectType {
    return IndirectType()
}

package func packageFunc(a: IndirectType) {
}

package struct PackageStruct : IndirectProtocol {
    package init() {}

    package var a = IndirectType()
}

//--- Client.swift
public import PackageLib

let t = getIndirectType() // expected-error {{cannot find 'getIndirectType' in scope}}
t.someMethod()
packageFunc(a: t) // expected-error {{cannot find 'packageFunc' in scope}}

let s = PackageStruct() // expected-error {{cannot find 'PackageStruct' in scope}}
s.a.someMethod()
