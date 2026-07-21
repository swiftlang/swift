// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/%target-library-name(COM)) -emit-module-path %t/COM.swiftmodule -module-name COM -Xfrontend -enable-experimental-com-interop %S/../Inputs/COM.swift
// RUN: %target-build-swift-dylib(%t/%target-library-name(Widget)) -emit-module-path %t/Widget.swiftmodule -module-name Widget -Xfrontend -enable-experimental-com-interop -I %t -L %t -lCOM %target-rpath(%t) %S/Inputs/com_widget.swift
// RUN: %target-build-swift %s -o %t/a.out -module-name main -Xfrontend -enable-experimental-com-interop -I %t -L %t -lCOM -lWidget %target-rpath(%t)
// RUN: %target-codesign %t/a.out %t/%target-library-name(COM) %t/%target-library-name(Widget)
// RUN: %target-run %t/a.out | %FileCheck %s
// REQUIRES: executable_test

// A client resolves an imported `@com` interface's `IID` and class's `CLSID`
// from the deserialized synthesized extension and reaches them at runtime;
// without routing that extension through a file unit, `IWidget.IID` was never
// emitted and this failed to link.
//
// `data2`/`data3` are non-zero to catch a host-dependent field decomposition:
// `swift::UUID` is native-endian on Windows, which would bake wrong literals.

import COM
import Widget

// IID  10203040-5060-7080-90a0-b0c0d0e0f001
print("iid.data1 = \(IWidget.IID.data1)")
// CHECK: iid.data1 = 270544960
print("iid.data2 = \(IWidget.IID.data2)")
// CHECK: iid.data2 = 20576
print("iid.data3 = \(IWidget.IID.data3)")
// CHECK: iid.data3 = 28800
print("iid.data4.7 = \(IWidget.IID.data4.7)")
// CHECK: iid.data4.7 = 1

// CLSID 01020304-0506-0708-090a-0b0c0d0e0f10
print("clsid.data1 = \(CWidget.CLSID.data1)")
// CHECK: clsid.data1 = 16909060
print("clsid.data2 = \(CWidget.CLSID.data2)")
// CHECK: clsid.data2 = 1286
print("clsid.data3 = \(CWidget.CLSID.data3)")
// CHECK: clsid.data3 = 1800
print("clsid.data4.7 = \(CWidget.CLSID.data4.7)")
// CHECK: clsid.data4.7 = 16
