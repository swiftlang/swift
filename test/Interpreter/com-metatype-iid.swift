// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/%target-library-name(COM)) -Xfrontend -enable-experimental-com-interop -emit-module-path %t/COM.swiftmodule -module-name COM %S/../Inputs/COM.swift
// RUN: %target-build-swift -Xfrontend -enable-experimental-com-interop -I %t -L %t -module-name main %s -o %t/test.exe %target-rpath(%t)
// RUN: %target-codesign %t/test.exe %t/%target-library-name(COM)
// RUN: %target-run %t/test.exe | %FileCheck %s

// REQUIRES: executable_test

@com(interface: "10203040-5060-7080-90a0-b0c0d0e0f001")
protocol IWidget {
}

@com(interface: "a1a2a3a4-b1b2-c1c2-d1d2-e1e2e3e4e5e6")
protocol IRefinedWidget: IWidget {
}

func __uuidof<Interface>(_: Interface.Type) -> IID
    where Interface.Type: COMInterface {
  Interface.IID
}

let widget = __uuidof(IWidget.self)
print("widget.data1 = \(widget.data1)")
// CHECK: widget.data1 = 270544960
print("widget.data2 = \(widget.data2)")
// CHECK: widget.data2 = 20576
print("widget.data3 = \(widget.data3)")
// CHECK: widget.data3 = 28800
print("widget.data4.7 = \(widget.data4.7)")
// CHECK: widget.data4.7 = 1

let refined = __uuidof(IRefinedWidget.self)
print("refined.data1 = \(refined.data1)")
// CHECK: refined.data1 = 2711790500
print("refined.data4.7 = \(refined.data4.7)")
// CHECK: refined.data4.7 = 230
