/// Report the use in API of indirectly or implicitly imported decls.

// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend -emit-module %t/empty.swift -module-name empty -o %t/empty.swiftmodule
// RUN: %target-swift-frontend -emit-module %t/libA.swift -module-name libA -o %t/libA.swiftmodule
// RUN: %target-swift-frontend -emit-module %t/libB.swift -module-name libB -o %t/libB.swiftmodule -I %t

/// In pre-Swift 6, this is a warning where there's no implementation-only import present.
// RUN: %target-swift-frontend -emit-module %t/clientFileA-Swift5.swift %t/clientFileB.swift -module-name client -o %t/client.swiftmodule -I %t -verify

/// In pre-Swift 6, this remains an error when there's an implementation-only import present.
// RUN: %target-swift-frontend -emit-module %t/clientFileA-OldCheck.swift %t/clientFileB.swift -module-name client -o %t/client.swiftmodule -I %t -verify

/// In Swift 6, it's an error.
// RUN: %target-swift-frontend -emit-module %t/clientFileA-Swift6.swift %t/clientFileB.swift -module-name client -o %t/client.swiftmodule -I %t -verify -swift-version 6

// BEGIN empty.swift

// BEGIN libA.swift
public struct ImportedType {
    public init() {}
}

// BEGIN libB.swift
import libA

extension ImportedType {
    public func implicitlyImportedMethod() {}
}

/// Client module
// BEGIN clientFileA-Swift5.swift
import libA

@inlinable public func bar() {
  let a = ImportedType()
  a.implicitlyImportedMethod() // expected-warning {{instance method 'implicitlyImportedMethod()' cannot be used in an '@inlinable' function because 'libB' was implicitly imported; this is an error in Swift 6}}

  // Expected implicit imports are still fine
  a.localModuleMethod()
}

// BEGIN clientFileA-OldCheck.swift
import libA
@_implementationOnly import empty

@inlinable public func bar() {
  let a = ImportedType()
  a.implicitlyImportedMethod() // expected-error {{instance method 'implicitlyImportedMethod()' cannot be used in an '@inlinable' function because 'libB' was implicitly imported}}

  // Expected implicit imports are still fine
  a.localModuleMethod()
}

// BEGIN clientFileA-Swift6.swift
import libA

@inlinable public func bar() {
  let a = ImportedType()
  a.implicitlyImportedMethod() // expected-error {{instance method 'implicitlyImportedMethod()' cannot be used in an '@inlinable' function because 'libB' was implicitly imported}}

  // Expected implicit imports are still fine
  a.localModuleMethod()
}

// BEGIN clientFileB.swift
@_implementationOnly import libB
import libA
extension ImportedType {
    public func localModuleMethod() {}
}

