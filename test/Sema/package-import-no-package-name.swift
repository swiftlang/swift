/// Report uses of package import without a package.

// RUN: %empty-directory(%t)
// RUN: split-file --leading-lines %s %t

// RUN: %target-swift-frontend -emit-module %t/PackageLib.swift -o %t
// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   -enable-experimental-feature AccessLevelOnImport -verify
// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   -enable-experimental-feature AccessLevelOnImport \
// RUN:   -package-name pkg

//--- PackageLib.swift
public struct PackageImportType {}

//--- Client.swift
package import PackageLib // expected-error {{package import can only be used from a module with a package name; set it with the compiler flag -package-name}}
