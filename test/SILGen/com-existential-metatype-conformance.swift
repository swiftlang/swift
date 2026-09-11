// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/COM.swiftmodule -module-name COM -enable-experimental-com-interop %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -emit-silgen %s | %FileCheck %s

@com(interface: "10203040-5060-7080-90a0-b0c0d0e0f001")
protocol IWidget {
}

func f<Interface>(_ value: borrowing Interface)
    where Interface.Type: COMInterface {
}

func g(_ value: borrowing IWidget) {
  f(value)
}

// A metatype requirement preserves the exist interface existential instead of
// opening it and losing the interface identity carried by its conformance.

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}1g
// CHECK:         function_ref @$s{{.*}}1f
// CHECK:         apply {{.*}}<any IWidget>
// CHECK-NOT:     open_existential
