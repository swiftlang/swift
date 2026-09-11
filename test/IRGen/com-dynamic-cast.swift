// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -disable-llvm-optzns -emit-ir %s | %FileCheck %s

@com(interface: "10000000-0000-0000-0000-000000000001")
public protocol ISource { }

@com(interface: "10000000-0000-0000-0000-000000000002")
public protocol ITarget { }

// A cast to a COM existential uses the general dynamic-cast entry point so
// the runtime can perform QueryInterface and return its interface pointer.

// CHECK-LABEL: define{{.*}} swiftcc ptr @"$s{{.*}}11conditional
// CHECK-NOT: swift_dynamicCastClass
// CHECK: call zeroext i1 @swift_dynamicCast(
public func conditional(_ source: consuming any ISource) -> (any ITarget)? {
  source as? any ITarget
}
