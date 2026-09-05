// RUN: %empty-directory(%t)
// RUN: split-file --leading-lines %s %t
// RUN: %target-swift-frontend -emit-module -module-name TestModule -emit-module-path %t/TestModule.swiftmodule %t/TestModule.swift
// RUN: %target-swift-ide-test -print-indexed-symbols -source-filename %t/main.swift -I %t | %FileCheck %s

//--- TestModule.swift
public struct Widget {
  public init() {}
  public func method(label: Int) {}
}

//--- main.swift
import TestModule

let unbound: () = TestModule::Widget.TestModule::method(Widget())(label: 1)
// CHECK: [[@LINE-1]]:31 | struct/Swift | Widget | s:10TestModule6WidgetV | Ref
// CHECK: [[@LINE-2]]:50 | instance-method/Swift | method(label:) | s:10TestModule6WidgetV6method5labelySi_tF | Ref

let compound: () = TestModule::Widget.TestModule::method(label:)(Widget())(1)
// CHECK: [[@LINE-1]]:32 | struct/Swift | Widget | s:10TestModule6WidgetV | Ref
// CHECK: [[@LINE-2]]:51 | instance-method/Swift | method(label:) | s:10TestModule6WidgetV6method5labelySi_tF | Ref
