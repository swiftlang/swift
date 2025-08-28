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

// RUN: split-file %s %t/Files

// Compile the Swift dependencies into that same location.
// RUN: %target-swift-frontend -parse-as-library -emit-module %t/Dependencies/SwiftDependency.swift -enable-experimental-feature Embedded -o %t/Dependencies/SwiftDependency.swiftmodule

// Build the library (which is supposed to encapsulate those dependencies)
// against the dependencies.
// RUN: %target-swift-frontend -parse-as-library -emit-module %t/Files/Library.swift -enable-experimental-feature Embedded -I %t/Dependencies/ -o %t/Modules/Library.swiftmodule

// Remove the dependencies so there is no way we can find them later.
// RUN: rm -rf %t/Dependencies

// Build the application against the library. This is expected to work because
// @_neverEmitIntoClient hides the body of test().
// RUN: %target-swift-frontend -emit-ir -parse-as-library %t/Files/Application.swift -enable-experimental-feature Embedded -I %t/Modules -o %t/Application.ir

// Build the application against the library, but intentionally trigger
// deserialization of some serialized SIL that refers to an implementation-only
// dependency. Right now, these fail spectacularly. Over time, we want them to
// become compile-time errors or start working.
// RUN: not --crash %target-swift-frontend -emit-ir -parse-as-library %t/Files/Application.swift -enable-experimental-feature Embedded -I %t/Modules -o %t/Application.ir -DBAD_C_USAGE
// RUN: not --crash %target-swift-frontend -emit-ir -parse-as-library %t/Files/Application.swift -enable-experimental-feature Embedded -I %t/Modules -o %t/Application.ir -DBAD_SWIFT_USAGE

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded

//--- Library.swift
@_implementationOnly import CDependency
@_implementationOnly import SwiftDependency

@_neverEmitIntoClient
public func test() {
  _ = getPoint(3.14159, 2.71828)
  A().doSomething()
}

public func badCLibraryUsage() {
  _ = getPoint(3.14159, 2.71828)
}

public func badSwiftLibraryUsage() {
  A().doSomething()
}

//--- Application.swift

import Library

public func useTest() {
  test()

#if BAD_C_USAGE
  badCLibraryUsage()
#endif

#if BAD_SWIFT_USAGE
  badSwiftLibraryUsage()
#endif
}
