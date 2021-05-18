// RUN: %empty-directory(%t)
// RUN: touch %t/empty.swift

// RUN: %target-swift-frontend -emit-module -o %t/errors.partial.swiftmodule -module-name errors -experimental-allow-module-with-compiler-errors %s
// RUN: %target-swift-frontend -emit-module -o %t/errorsempty.partial.swiftmodule -module-name errors %t/empty.swift

// Note - running the merge without allow errors to force a crash, we want
// to check if there's a message about allowing compiler errors for the
// deserialized module
// RUN: not --crash %target-swift-frontend -module-name errors -emit-module -o %t/errors.swiftmodule %t/errors.partial.swiftmodule %t/errorsempty.partial.swiftmodule 2>&1 | %FileCheck %s

// REQUIRES: asserts

typealias AnAlias = SomeStruct

// CHECK: While reading from 'errors' (built while allowing compiler errors)
