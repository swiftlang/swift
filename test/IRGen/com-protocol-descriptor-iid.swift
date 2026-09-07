// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -emit-module-path %t/COM.swiftmodule -module-name COM %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -module-name Interfaces -I %t -emit-ir %s | %FileCheck %s

@com(interface: "01010101-0202-0303-0404-050505050505")
public protocol IWidget {
  func draw()
}

public protocol Protocol {
  func draw()
}

// A COM protocol has exactly one IID value stored inline in the descriptor. The
// COMInterface IID requirement reads from this storage.

// CHECK: @"$s10Interfaces7IWidgetMp" = {{.*}}constant <{ i32, i32, i32, i32, i32, i32, [16 x i8],
// CHECK-SAME: [16 x i8] c"\01\01\01\01\02\02\03\03\04\04\05\05\05\05\05\05"

// An ordinary protocol retains its existing descriptor layout: the requirement
// follows the six-word fixed header without an intervening IID.

// CHECK: @"$s10Interfaces8ProtocolMp" = {{.*}}constant <{ i32, i32, i32, i32, i32, i32, %swift.protocol_requirement }>
