// NOTE: This file has a dependency on test/ParseableInterface/property_wrappers.swift
//       It ensures that we properly synthesize the hidden _val backing property
//       even if we omit it in the module interface.

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck %S/property_wrappers.swift -emit-module-interface-path %t/PropertyWrappers.swiftinterface -module-name PropertyWrappers -enable-library-evolution
// RUN: %target-swift-frontend -emit-sil %t/PropertyWrappers.swiftinterface -module-name PropertyWrappers -enable-library-evolution -o %t/layout.sil
// RUN: %FileCheck %s < %t/layout.sil

// CHECK: @frozen public struct FrozenWithWrappers {
// CHECK-NEXT: @Wrapper public var val: Int { get set }
// CHECK-NEXT: @_hasStorage var _val: Wrapper<Int> { get set }
// CHECK-NEXT: }
