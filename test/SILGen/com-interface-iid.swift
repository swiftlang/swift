// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/COM.swiftmodule -module-name COM -enable-experimental-com-interop %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -emit-silgen %s | %FileCheck %s --implicit-check-not=IWidgetPAA3IID

@com(interface: "10203040-5060-7080-90a0-b0c0d0e0f001")
protocol IWidget: IUnknown {}

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}3iid
// CHECK: [[GETTER:%.*]] = witness_method $(any IWidget).Type, #COMInterface.IID!getter
// CHECK: apply [[GETTER]]<(any IWidget).Type>
func iid() -> IID { IWidget.IID }

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}5value
// CHECK: [[GETTER:%.*]] = witness_method $(any IWidget).Type, #COMInterface.IID!getter
// CHECK: apply [[GETTER]]<(any IWidget).Type>
func value(_ type: (any IWidget).Type) -> IID { type.IID }
