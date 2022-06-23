/// Report the use in API of indirectly or implicitly imported decls.

// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend -emit-module %t/libA.swift -module-name libA -o %t/libA.swiftmodule
// RUN: %target-swift-frontend -emit-module %t/libB.swift -module-name libB -o %t/libB.swiftmodule -I %t

/// In pre-Swift 6 mode, this is a warning.
// RUN: %target-swift-frontend -emit-module %t/clientFileA-Swift5.swift %t/clientFileB.swift -module-name client -o %t/client.swiftmodule -I %t -import-objc-header %t/bridging-header.h -verify

/// In Swift 6 mode, it's an error.
// RUN: %target-swift-frontend -emit-module %t/clientFileA-Swift6.swift %t/clientFileB.swift -module-name client -o %t/client.swiftmodule -I %t -import-objc-header %t/bridging-header.h -verify -swift-version 6

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
// BEGIN bridging-header.h
struct CStruct {};

// BEGIN clientFileA-Swift5.swift
import libA

@inlinable public func bar() {
  let a = ImportedType()
  a.implicitlyImportedMethod() // expected-warning {{instance method 'implicitlyImportedMethod()' cannot be used in an '@inlinable' function because 'libB' was implicitly imported; this is an error in Swift 6}}

  // Expected implicit imports are still fine
  a.localModuleMethod()
  let _ = CStruct()
}

// BEGIN clientFileA-Swift6.swift
import libA

@inlinable public func bar() {
  let a = ImportedType()
  a.implicitlyImportedMethod() // expected-error {{instance method 'implicitlyImportedMethod()' cannot be used in an '@inlinable' function because 'libB' was implicitly imported}}

  // Expected implicit imports are still fine
  a.localModuleMethod()
  let _ = CStruct()
}

// BEGIN clientFileB.swift
@_implementationOnly import libB
import libA
extension ImportedType {
    public func localModuleMethod() {}
}

