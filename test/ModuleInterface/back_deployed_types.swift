// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/Lib.swiftinterface) %s -module-name Lib -enable-experimental-feature BackDeployFrozenValueTypes
// RUN: %target-swift-typecheck-module-from-interface(%t/Lib.swiftinterface) -module-name Lib -enable-experimental-feature BackDeployFrozenValueTypes
// RUN: %FileCheck %s < %t/Lib.swiftinterface
// RUN: %empty-directory(%t/NoFlag)
// RUN: sed -e 's/ -enable-experimental-feature BackDeployFrozenValueTypes//' %t/Lib.swiftinterface > %t/NoFlag/Lib.swiftinterface
// RUN: %target-swift-typecheck-module-from-interface(%t/NoFlag/Lib.swiftinterface) -module-name Lib

// REQUIRES: swift_feature_BackDeployFrozenValueTypes
// REQUIRES: OS=macosx

// The attribute is only printed where the compiler that reads the interface
// supports back-deploying types. Other compilers see the type without it.

// CHECK: #if compiler(>=5.3) && $BackDeployFrozenValueTypes
// CHECK-NEXT: @available(macOS 10.15, *)
// CHECK-NEXT: @backDeployed(before: macOS 99)
// CHECK-NEXT: @frozen public struct S {
// CHECK: #else
// CHECK-NEXT: @available(macOS 10.15, *)
// CHECK-NEXT: @frozen public struct S {
// CHECK: #endif
@available(macOS 10.15, *)
@backDeployed(before: macOS 99)
@frozen
public struct S {
  public var x: Int
}

// A function doesn't need the feature.
// CHECK-NOT: $BackDeployFrozenValueTypes
// CHECK: @backDeployed(before: macOS 99)
// CHECK-NEXT: public func f()
@available(macOS 10.15, *)
@backDeployed(before: macOS 99)
public func f() {}
