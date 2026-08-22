// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -com-interop-model=microsoft -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -com-interop-model=microsoft -module-name Library -I %t -typecheck -enable-library-evolution -emit-module-interface-path %t/Library.swiftinterface %s
// RUN: %FileCheck %s --input-file %t/Library.swiftinterface
// RUN: %target-swift-frontend -enable-experimental-com-interop -com-interop-model=microsoft -module-name Library -I %t -compile-module-from-interface %t/Library.swiftinterface -o %t/Library.swiftmodule
// RUN: %target-swift-frontend -enable-experimental-com-interop -com-interop-model=microsoft -I %t -typecheck %S/Inputs/com-class-identity-client.swift

import COM

@com(interface: "10000000-0000-0000-0000-000000000001")
public protocol IWidget: IUnknown { }

@com(implementation: "20000000-0000-0000-0000-000000000002",
     threading: .both)
public class Widget: IWidget { }

// CHECK: @com(interface: "10000000-0000-0000-0000-000000000001")
// CHECK: @com(implementation: "20000000-0000-0000-0000-000000000002", threading: .both)
// CHECK: public class Widget : Library::IWidget
