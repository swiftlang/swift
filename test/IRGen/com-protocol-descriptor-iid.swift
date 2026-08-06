// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -com-interop-model=microsoft -emit-module-path %t/COM.swiftmodule -module-name COM %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -com-interop-model=microsoft -module-name Interfaces -I %t -emit-ir %s | %FileCheck %s

@com(interface: "01010101-0202-0303-0404-050505050505")
public protocol IWidget {
  func draw()
}

public protocol Protocol {
  func draw()
}

// A COM protocol owns exactly one addressable 16-byte IID, stored inline in its
// descriptor. Accessing the COMInterface IID requirement through the
// compiler-managed metatype conformance reads this storage directly.

// CHECK-NOT: IWidgetMp.iid

// SpecialProtocol::COM is the presence discriminator. The target-native 16-byte
// IID is the first conditional trailing field, immediately after the six fixed
// protocol-descriptor words and before the requirement signature.

// CHECK: @"$s10Interfaces7IWidgetMp" = {{.*}}constant <{ i32, i32, i32, i32, i32, i32, [16 x i8],
// CHECK-SAME: [16 x i8] c"\01\01\01\01\02\02\03\03\04\04\05\05\05\05\05\05"
// CHECK-NOT: IWidgetMp.iid

// An ordinary protocol retains its existing descriptor layout: the requirement
// follows the six-word fixed header without an intervening IID.

// CHECK: @"$s10Interfaces8ProtocolMp" = {{.*}}constant <{ i32, i32, i32, i32, i32, i32, %swift.protocol_requirement }>
// CHECK-NOT: ProtocolMp.iid
