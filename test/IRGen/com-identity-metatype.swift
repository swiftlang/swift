// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/COM.swiftmodule -module-name COM -enable-experimental-com-interop -com-interop-model=microsoft %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -com-interop-model=microsoft -I %t -emit-ir %s | %FileCheck %s --implicit-check-not=WidgetC5CLSID
// RUN: %target-swift-frontend -enable-experimental-com-interop -com-interop-model=microsoft -I %t -emit-ir -O %s -o %t/optimized.ll

@com(interface: "10203040-5060-7080-90a0-b0c0d0e0f001")
protocol IWidget: IUnknown {}

@com(implementation: "01020304-0506-0708-090a-0b0c0d0e0f10")
class Widget: IWidget {}

// The activation witness is a GUID constant, not an ordinary witness table.
// CHECK: @"CLSID_{{.*}}6WidgetCMn" = linkonce_odr hidden unnamed_addr constant [16 x i8]

// CHECK-LABEL: define{{.*}} swiftcc {{.*}}@"$s{{.*}}11interfaceID
// CHECK-SAME: ptr %Identity.COMInterface
// CHECK: getelementptr inbounds {{.*}}, ptr %Identity.COMInterface, i32 0, i32 0
// CHECK: load i32
func interfaceID<Identity: COMInterface>(_ type: Identity) -> IID {
  type.IID
}

// CHECK-LABEL: define{{.*}} swiftcc {{.*}}@"$s{{.*}}12activationID
// CHECK-SAME: ptr %Identity.COMActivatable
// CHECK: getelementptr inbounds {{.*}}, ptr %Identity.COMActivatable, i32 0, i32 0
// CHECK: load i32
func activationID<Identity: COMActivatable>(_ type: Identity) -> CLSID {
  type.CLSID
}

// The interface witness points into the protocol descriptor's IID payload.
// CHECK-LABEL: define{{.*}} swiftcc {{.*}}@"$s{{.*}}9interface
// CHECK: call swiftcc {{.*}}@"$s{{.*}}11interfaceID
// CHECK-SAME: ptr getelementptr inbounds (i8, ptr @"{{.*}}7IWidgetMp", i{{32|64}} 24)
func interface() -> IID { interfaceID(IWidget.self) }

// CHECK-LABEL: define{{.*}} swiftcc {{.*}}@"$s{{.*}}10activation
// CHECK: call swiftcc {{.*}}@"$s{{.*}}12activationID
// CHECK-SAME: ptr @"CLSID_{{.*}}6WidgetCMn"
func activation() -> CLSID { activationID(Widget.self) }

// CHECK-LABEL: define{{.*}} swiftcc {{.*}}@"$s{{.*}}16directActivation
// CHECK: load i32, ptr @"CLSID_{{.*}}6WidgetCMn"
func directActivation() -> CLSID { Widget.CLSID }
