// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -emit-ir %s | %FileCheck %s

@com(interface: "43000000-0000-0000-0000-000000000001")
protocol IWidget {
}

// CHECK-LABEL: define hidden swiftcc ptr @"$s{{.*}}11dynamicType2ofypXpAA7IWidget_p_tF"
// CHECK:       [[TYPE:%.*]] = call ptr @swift_getCOMDynamicType(ptr %0, ptr {{.*}})
// CHECK:       ret ptr [[TYPE]]
func dynamicType(of value: borrowing any IWidget) -> Any.Type {
  type(of: value)
}
