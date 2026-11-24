// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -O -emit-sil -sil-verify-all %s | %FileCheck %s

@com(interface: "10000000-0000-0000-0000-000000000001")
public protocol ISource: AnyObject {}
@com(interface: "10000000-0000-0000-0000-000000000002")
public protocol ITarget: AnyObject {}

// QueryInterface creates an owned result even when the source is borrowed.
// CHECK-LABEL: sil{{.*}} @$s{{.*}}11conditional
// CHECK: checked_cast_addr_br copy_on_success
public func conditional(_ source: borrowing any ISource) -> (any ITarget)? {
  source as? any ITarget
}

// CHECK-LABEL: sil{{.*}} @$s{{.*}}8optional
// CHECK: checked_cast_addr_br copy_on_success
public func optional(_ source: (any ISource)?) -> (any ITarget)? {
  source as? any ITarget
}

// Metatypes do not represent object interface pointers.
// CHECK-LABEL: sil{{.*}} @$s{{.*}}8metatype
// CHECK: checked_cast_br
public func metatype(_ source: Any.Type) -> (any ITarget.Type)? {
  source as? any ITarget.Type
}

public protocol NativeSource: AnyObject {}
public protocol NativeTarget: AnyObject {}

// Native class-bound existentials retain their scalar cast optimization.
// CHECK-LABEL: sil{{.*}} @$s{{.*}}6native
// CHECK: checked_cast_br
public func native(_ source: any NativeSource) -> (any NativeTarget)? {
  source as? any NativeTarget
}
