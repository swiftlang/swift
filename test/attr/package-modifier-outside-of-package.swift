/// Report uses of the package access-level modifier without a package name.

// RUN: %empty-directory(%t)
// RUN: split-file --leading-lines %s %t

// RUN: %target-swift-frontend -emit-module %t/PackageLib.swift -o %t
// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t -verify
// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   -package-name pkg

//--- PackageLib.swift
public struct PackageImportType {}

//--- Client.swift
package import PackageLib // expected-error {{the package access level requires a package name; set it with the compiler flag -package-name}}
// expected-warning @-1 {{package import of 'PackageLib' was not used in package declarations}}

package struct PackageStruct { // expected-error {{the package access level used on 'PackageStruct' requires a package name; set it with the compiler flag -package-name}}
  package func explicitlyPackage() {} // expected-error {{the package access level used on 'explicitlyPackage()' requires a package name; set it with the compiler flag -package-name}}
}

public struct PublicStruct {}

package extension PublicStruct { // expected-error {{the package access level requires a package name; set it with the compiler flag -package-name}}
  func implicitlyPackage() {}
}
