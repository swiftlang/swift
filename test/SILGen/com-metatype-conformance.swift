// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -emit-module-path %t/COM.swiftmodule -module-name COM %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -emit-silgen %s | %FileCheck %s

@com(interface: "10203040-5060-7080-90a0-b0c0d0e0f001")
protocol IWidget {
}

func __uuidof<Interface>(_: Interface.Type) -> IID
    where Interface.Type: COMInterface {
  Interface.IID
}

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}8__uuidof
// CHECK:         witness_method $Interface.Type, #COMInterface.IID!getter
// CHECK:         apply {{.*}}<Interface.Type>

func f() -> IID {
  __uuidof(IWidget.self)
}

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}1f
// CHECK:         apply {{.*}}<any IWidget>

func g<Interface>(_ value: borrowing Interface)
    where Interface.Type: COMInterface {
}

func h(_ value: borrowing IWidget) {
  g(value)
}

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}1h
// CHECK:         function_ref @$s{{.*}}1g
// CHECK:         apply {{.*}}<any IWidget>
// CHECK-NOT:     open_existential

// CHECK-NOT:     protocol witness for COMInterface.IID.getter
// CHECK-NOT:     sil_witness_table
