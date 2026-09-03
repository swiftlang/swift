// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -emit-silgen %s | %FileCheck %s

@com(interface: "43000000-0000-0000-0000-000000000001")
protocol IWidget {
}

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}11dynamicType2ofypXpAA7IWidget_p_tF
// CHECK-SAME:  @guaranteed any IWidget
// CHECK:       [[TYPE:%.*]] = existential_metatype $@thick any Any.Type, %{{.*}}
// CHECK:       return [[TYPE]]
func dynamicType(of value: borrowing any IWidget) -> Any.Type {
  type(of: value)
}
