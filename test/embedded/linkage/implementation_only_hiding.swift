// This elaborate test ensures that @_implementationOnly with Embedded Swift
// can hide some dependencies if you are very, very careful.

// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/Dependencies
// RUN: mkdir -p %t/Files
// RUN: mkdir -p %t/Modules

// Copy Swift file + C header + module map into a separate directory we'll use
// when building the module whose implementation we want to hide.
// RUN: cp %S/Inputs/SwiftDependency.swift %t/Dependencies/
// RUN: cp %S/Inputs/CHeader.h %t/Dependencies/
// RUN: cp %S/Inputs/module.modulemap %t/Dependencies/

// RUN: split-file %s %t/Files --leading-lines

// Compile the Swift dependencies into that same location.
// RUN: %target-swift-frontend -parse-as-library -emit-module %t/Dependencies/SwiftDependency.swift -enable-experimental-feature Embedded -o %t/Dependencies/SwiftDependency.swiftmodule

// Build the library (which is supposed to encapsulate those dependencies)
// against the dependencies.
// RUN: %target-swift-frontend -parse-as-library -emit-module %t/Files/Library.swift -enable-experimental-feature Embedded -I %t/Dependencies/ -o %t/Modules/Library.swiftmodule

// Building the library with invalid uses trigger errors.
// RUN: %target-swift-frontend -parse-as-library -typecheck -verify %t/Files/Library.swift -enable-experimental-feature Embedded -I %t/Dependencies/ -o %t/Modules/Library.swiftmodule -DBAD_IOI_USAGE

// Remove the dependencies so there is no way we can find them later.
// RUN: rm -rf %t/Dependencies

// Build the application against the library. This is expected to work because
// @export(interface) hides the body of test().
// RUN: %target-swift-frontend -emit-ir -parse-as-library %t/Files/Application.swift -enable-experimental-feature Embedded -I %t/Modules -o %t/Application.ir

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded

//--- Library.swift
@_implementationOnly import CDependency // expected-warning {{safely use '@_implementationOnly' without library evolution by setting '-enable-experimental-feature CheckImplementationOnly' for 'Library'}}
@_implementationOnly import SwiftDependency // expected-warning {{safely use '@_implementationOnly' without library evolution by setting '-enable-experimental-feature CheckImplementationOnly' for 'Library'}}

@export(interface)
public func test() {
  _ = getPoint(3.14159, 2.71828)
  A().doSomething()
}

#if BAD_IOI_USAGE
public func badCLibraryUsage() {
  _ = getPoint(3.14159, 2.71828) // expected-error {{global function 'getPoint' cannot be used in an embedded function not marked '@export(interface)' because 'CDependency' was imported implementation-only}}
}

public func badSwiftLibraryUsage() {
  A().doSomething() // expected-error {{struct 'A' cannot be used in an embedded function not marked '@export(interface)' because 'SwiftDependency' was imported implementation-only}}
  // expected-error @-1 {{initializer 'init()' cannot be used in an embedded function not marked '@export(interface)' because 'SwiftDependency' was imported implementation-only}}
  // expected-error @-2 {{instance method 'doSomething()' cannot be used in an embedded function not marked '@export(interface)' because 'SwiftDependency' was imported implementation-only}}
}
#endif

//--- Application.swift

import Library

public func useTest() {
  test()
}
