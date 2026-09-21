// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -enable-library-evolution -emit-module-path %t/COM.swiftmodule -module-name COM %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -enable-library-evolution -I %t -module-name Interfaces -emit-ir %s | %FileCheck %s --check-prefixes=CHECK,RESILIENT
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -module-name Interfaces -emit-ir %s | %FileCheck %s --check-prefixes=CHECK,FRAGILE

// COM descriptors have the COM special-protocol flag, but no resilience bit:
// 0x90043 = Any class constraint | COM | unique | protocol.
// CHECK-LABEL: @"$s10Interfaces5IBaseMp" =
// CHECK-SAME: <{ i32 589891,
@com(interface: "10000000-0000-0000-0000-000000000001")
public protocol IBase {
  func base() -> CInt
}

// CHECK-LABEL: @"$s10Interfaces8IDerivedMp" =
// CHECK-SAME: <{ i32 589891,
@com(interface: "10000000-0000-0000-0000-000000000002")
public protocol IDerived: IBase {
  func derived() -> CInt
}

// The ordinary Swift protocol retains its resilience bit when enabled.
// CHECK-LABEL: @"$s10Interfaces6NativeMp" =
// RESILIENT-SAME: <{ i32 196675,
// FRAGILE-SAME: <{ i32 65603,
public protocol Native {
  func native() -> CInt
}
