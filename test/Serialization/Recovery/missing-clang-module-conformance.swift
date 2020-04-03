/// Recover from reading witness table involving a synthesized conformance
/// rdar://problem/58924131

// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/custom-modules %t/
// RUN: %target-swift-frontend -emit-module %s -module-name MyModule -emit-module-path %t/MyModule.swiftmodule -I %t/custom-modules -swift-version 5

/// Delete the clang module
// RUN: rm -r %t/custom-modules/

// RUN: not %target-sil-opt %t/MyModule.swiftmodule 2>&1 | %FileCheck %s

@_implementationOnly import Conformance
// CHECK: missing required module 'Conformance'

public func foo<T: OptionSet>(_ t: T) {}

foo(CEnum.A)
