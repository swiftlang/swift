// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/COM.swiftmodule -module-name COM -enable-experimental-com-interop %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -emit-silgen %s | %FileCheck %s

@com(interface: "10203040-5060-7080-90a0-b0c0d0e0f001")
protocol IWidget {
}

func iid() -> IID {
  IWidget.IID
}

// A concrete interface metatype uses the COMInterface requirement directly:
// there is no synthesized interface-specific IID accessor.

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}3iid
// CHECK-DAG:     [[WITNESS:%.*]] = witness_method $(any IWidget).Type, #COMInterface.IID!getter
// CHECK:         apply [[WITNESS]]<(any IWidget).Type>
// CHECK-NOT:     IWidgetPAA3IID
