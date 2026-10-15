// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/COM.swiftmodule -module-name COM -enable-experimental-com-interop -com-interop-model=microsoft %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -com-interop-model=microsoft -I %t -emit-silgen %s | %FileCheck %s --implicit-check-not=WidgetC5CLSID

@com(implementation: "01020304-0506-0708-090a-0b0c0d0e0f10")
class Widget: IUnknown {}

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}10activation
// CHECK: [[GETTER:%.*]] = witness_method $Widget.Type, #COMActivatable.CLSID!getter
// CHECK: apply [[GETTER]]<Widget.Type>
func activation() -> CLSID { Widget.CLSID }

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}5value
// CHECK: [[GETTER:%.*]] = witness_method $Widget.Type, #COMActivatable.CLSID!getter
// CHECK: apply [[GETTER]]<Widget.Type>
func value(_ type: Widget.Type) -> CLSID { type.CLSID }
