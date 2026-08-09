// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -com-interop-model=microsoft -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -com-interop-model=microsoft -I %t -emit-ir %s | %FileCheck %s

@com(interface: "10000000-0000-0000-0000-000000000001")
public protocol IWidget: IUnknown { }

@com(implementation: "12345678-9abc-def0-1234-56789abcdef0")
final class Widget: IWidget { }

func __uuidof<Implementation>(_: Implementation.Type) -> CLSID
    where Implementation.Type: COMActivatable {
  Implementation.CLSID
}

func f() -> CLSID {
  __uuidof(Widget.self)
}

// The class has one target-native CLSID constant. There is no synthesized
// `Widget.CLSID` accessor or ordinary conformance record.

// CHECK: @"CLSID_$s{{.*}}6WidgetCMn" = linkonce_odr hidden unnamed_addr constant [16 x i8] c"xV4\12\BC\9A\F0\DE\124Vx\9A\BC\DE\F0"
// CHECK-NOT: WidgetC5CLSIDvvg
// CHECK-NOT: COMActivatableACMc

// Generic code receives the address of that identity as its conformance witness
// and loads the GUID directly.

// CHECK-LABEL: define{{.*}} swiftcc {{.*}}@"$s{{.*}}8__uuidof
// CHECK-SAME:  ptr %Implementation.Type.COMActivatable
// CHECK:         getelementptr inbounds {{.*}}, ptr %Implementation.Type.COMActivatable, i32 0, i32 0
// CHECK:         load i32
// CHECK:         getelementptr inbounds {{.*}}, ptr %Implementation.Type.COMActivatable, i32 0, i32 1
// CHECK:         load i16

// CHECK-LABEL: define{{.*}} swiftcc {{.*}}@"$s{{.*}}1f
// CHECK:         call swiftcc {{.*}}@"$s{{.*}}8__uuidof
// CHECK-SAME:    ptr @"CLSID_$s{{.*}}6WidgetCMn"
