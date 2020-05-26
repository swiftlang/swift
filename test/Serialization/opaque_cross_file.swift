// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-implicit-dynamic -disable-availability-checking -emit-module-path %t/OpaqueCrossFileB.swiftmodule -module-name OpaqueCrossFileB %S/Inputs/OpaqueCrossFileB.swift
// RUN: %target-swift-frontend -enable-implicit-dynamic -disable-availability-checking -I %t -emit-ir -verify %s | %FileCheck %s

import OpaqueCrossFileB

dump(anyFoo())
dump(anyFooProp)
dump(Subscript()[])

public struct UsesAdapterMethod: Foo {
  // Ensure that the mangling of the result type of adaptFoo correctly captures
  // both the Self type and the parameter type.
  // CHECK: @"symbolic _____y______SdQo_ 16OpaqueCrossFileB3FooPAAE8identityyQrqd__lFQO 17opaque_cross_file17UsesAdapterMethodV" =
  public func adaptFoo(_ d: Double) -> some Foo {
    return identity(d)
  }
}
