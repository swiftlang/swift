// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/clang-module-cache)
// RUN: %empty-directory(%t/Modules)
// RUN: %empty-directory(%t/PackageModules)
// RUN: %empty-directory(%t/PackageModules/Foo.swiftmodule)
// RUN: %empty-directory(%t/Modules/Bar.swiftmodule)
// RUN: %empty-directory(%t/TextualInterfaces)
// RUN: %empty-directory(%t/TextualInterfaces/Bar.swiftmodule)
// RUN: split-file %s %t

// Step 1: Build Foo Swift binary module
// RUN: %target-swift-frontend -emit-module %t/Foo.swift -emit-module-path %t/PackageModules/Foo.swiftmodule/%target-swiftmodule-name -module-name Foo

// Step 2: Build Bar Swift binary module and textual interface with a package-only import
// RUN: %target-swift-frontend -emit-module %t/Bar.swift -emit-module-path %t/Modules/Bar.swiftmodule/%target-swiftmodule-name -module-name Bar -enable-library-evolution -emit-module-interface-path %t/TextualInterfaces/Bar.swiftmodule/%target-swiftinterface-name -emit-private-module-interface-path %t/TextualInterfaces/Bar.swiftmodule/%target-private-swiftinterface-name -emit-package-module-interface-path %t/TextualInterfaces/Bar.swiftmodule/%target-package-swiftinterface-name -I %t/PackageModules/ -package-name BarTest

// Step 3: Now that Bar has been built, remove package-only dependency 'Foo' so that any clients of 'Bar' fail to build if they search for it
// RUN: rm -rf %t/PackageModules/*


// Test 1: Build a textual interface client which imports Bar and is in Bar's package but not being built from a package interface, therefore it must not import Bar's package-only dependencies
// RUN: %target-swift-frontend -compile-module-from-interface -explicit-interface-module-build %t/Client.swiftinterface -o %t/Modules/Client.swiftmodule -module-name Client -I %t/Modules/ -package-name BarTest 

// Test 2: Build a textual interface client which imports Bar and is in Bar's package and is being built from a package interface, therefore it must import Bar's package-only dependencies
// RUN: not %target-swift-frontend -compile-module-from-interface -explicit-interface-module-build %t/Client.package.swiftinterface -o %t/Modules/Client.package.swiftmodule -module-name Client -I %t/Modules/ -package-name BarTest &> %t/error.txt
// RUN %FileCheck --check-prefix=CHECK-MISSING-FOO %s < %t/error.txt

// Test 3: Build a source client which imports Bar but is not in Bar's package, therefore it must not import Bar's package-only dependencies
// RUN: %target-swift-frontend -emit-module %t/Client.swift -emit-module-path %t/Modules/SourceClient.swiftmodule/%target-swiftmodule-name -module-name Client -I %t/Modules/

// Test 4: Build a source client which imports Bar but and is in Bar's package, therefore it must import Bar's package-only dependencies
// RUN: not %target-swift-frontend -emit-module %t/Client.swift -emit-module-path %t/Modules/SourceClient.swiftmodule/%target-swiftmodule-name -module-name Client -I %t/Modules/ -package-name BarTest &> %t/source_error.txt
// RUN %FileCheck --check-prefix=CHECK-MISSING-FOO %s < %t/source_error.txt

// CHECK-MISSING-FOO: error: missing required module 'Foo'

//--- Foo.swift
public func foo() {}

//--- Bar.swift
package import Foo

//--- Client.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -swift-version 5 -enable-library-evolution -module-name Client
import Bar
public func test() {}

//--- Client.package.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -swift-version 5 -enable-library-evolution -module-name Client -package-name BarTest
import Bar
public func test() {}

//--- Client.swift
import Bar



