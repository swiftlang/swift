// RUN: %empty-directory(%t)
// RUN: split-file --leading-lines %s %t
// RUN: %target-swift-frontend -emit-module -module-name TestModule -emit-module-path %t/TestModule.swiftmodule %t/TestModule.swift
// RUN: %target-swift-frontend -emit-module -module-name Facade -emit-module-path %t/Facade.swiftmodule %t/Facade.swift -I %t
// RUN: %target-swift-ide-test -print-indexed-symbols -source-filename %t/main.swift -I %t | %FileCheck %s

//--- TestModule.swift
public struct Widget {
  public init() {}
  public func method(label: Int) {}
}

//--- Facade.swift
@_exported import TestModule

//--- main.swift
import TestModule
import Facade

let unbound: () = TestModule::Widget.TestModule::method(Widget())(label: 1)
// CHECK: [[@LINE-1]]:19 | module/Swift | TestModule | c:@M@TestModule | Ref
// CHECK: [[@LINE-2]]:31 | struct/Swift | Widget | s:10TestModule6WidgetV | Ref
// CHECK: [[@LINE-3]]:38 | module/Swift | TestModule | c:@M@TestModule | Ref
// CHECK: [[@LINE-4]]:50 | instance-method/Swift | method(label:) | s:10TestModule6WidgetV6method5labelySi_tF | Ref

let compound: () = TestModule::Widget.TestModule::method(label:)(Widget())(1)
// CHECK: [[@LINE-1]]:20 | module/Swift | TestModule | c:@M@TestModule | Ref
// CHECK: [[@LINE-2]]:32 | struct/Swift | Widget | s:10TestModule6WidgetV | Ref
// CHECK: [[@LINE-3]]:39 | module/Swift | TestModule | c:@M@TestModule | Ref
// CHECK: [[@LINE-4]]:51 | instance-method/Swift | method(label:) | s:10TestModule6WidgetV6method5labelySi_tF | Ref

let reexported = Facade::Widget.self
// CHECK: [[@LINE-1]]:18 | module/Swift | Facade | c:@M@Facade | Ref
// CHECK: [[@LINE-2]]:26 | struct/Swift | Widget | s:10TestModule6WidgetV | Ref

let dotted = TestModule.Widget.self
// CHECK: [[@LINE-1]]:14 | module/Swift | TestModule | c:@M@TestModule | Ref
// CHECK: [[@LINE-2]]:25 | struct/Swift | Widget | s:10TestModule6WidgetV | Ref
