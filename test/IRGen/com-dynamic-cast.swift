// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -module-name M -sil-verify-all -disable-llvm-optzns -emit-ir %s | %FileCheck %s
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -module-name M -sil-verify-all -O -emit-ir %s | %FileCheck %s

@com(interface: "10000000-0000-0000-0000-000000000001")
public protocol ISource {}
@com(interface: "10000000-0000-0000-0000-000000000002")
public protocol ITarget: AnyObject {}

// CHECK-LABEL: define{{.*}} @"$s1M11conditional
// CHECK: call zeroext i1 @swift_dynamicCast(
public func conditional(_ source: borrowing any ISource) -> (any ITarget)? {
  source as? any ITarget
}

// CHECK-LABEL: define{{.*}} @"$s1M6forced
// CHECK: call zeroext i1 @swift_dynamicCast(
public func forced(_ source: borrowing any ISource) -> any ITarget {
  source as! any ITarget
}

// CHECK-LABEL: define{{.*}} @"$s1M6erased
// CHECK: call zeroext i1 @swift_dynamicCast(
public func erased(_ source: borrowing Any) -> (any ITarget)? {
  source as? any ITarget
}

// CHECK-LABEL: define{{.*}} @"$s1M7generic
// CHECK: call zeroext i1 @swift_dynamicCast(
public func generic<T>(_ source: borrowing T) -> (any ITarget)? {
  source as? any ITarget
}

// CHECK-LABEL: define{{.*}} @"$s1M7pattern
// CHECK: call zeroext i1 @swift_dynamicCast(
public func pattern(_ source: borrowing any ISource) -> Bool {
  switch source {
  case is any ITarget: return true
  default: return false
  }
}
