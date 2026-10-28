// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -com-interop-model=microsoft -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-swift-emit-module-interface(%t/Library.swiftinterface) -enable-experimental-com-interop -com-interop-model=microsoft -module-name Library -I %t %s
// RUN: %FileCheck %s --input-file %t/Library.swiftinterface
// RUN: %target-swift-frontend -enable-experimental-com-interop -com-interop-model=microsoft -module-name Library -I %t -compile-module-from-interface %t/Library.swiftinterface -o %t/Library.swiftmodule

@com(implementation: "20000000-0000-0000-0000-000000000002",
     threading: .both)
public class Widget: IUnknown { }

// CHECK: @com(implementation: "20000000-0000-0000-0000-000000000002", threading: .both)
// CHECK: public class Widget : COM::IUnknown
