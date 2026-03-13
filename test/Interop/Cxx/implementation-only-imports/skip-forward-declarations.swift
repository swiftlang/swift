// RUN: %empty-directory(%t)
// RUN: not %target-swiftxx-frontend -emit-module -o %t/FortyTwo.swiftmodule -I %S/Inputs %s 2>&1 | %FileCheck %s

// This test checks that forward declarations are not considered
// when determining the visibility of the decl.

@_implementationOnly import UserA
import UserC

@inlinable
public func createAWrapper() {
  let _ = MagicWrapper()
}

// CHECK: struct 'MagicWrapper' cannot be used in an '@inlinable' function because 'Helper' was imported implementation-only
