// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module -module-name TestModule -emit-module-path %t/TestModule.swiftmodule %t/TestModule.swift

// RUN: %sourcekitd-test -req=cursor -pos=3:50 -print-raw-response %t/main.swift -- %t/main.swift -I %t -target %target-triple | %FileCheck %s --check-prefix=UNBOUND
// RUN: %sourcekitd-test -req=cursor -pos=4:51 -print-raw-response %t/main.swift -- %t/main.swift -I %t -target %target-triple | %FileCheck %s --check-prefix=COMPOUND

// UNBOUND: key.kind: source.lang.swift.ref.function.method.instance
// UNBOUND: key.name: "method(label:)"
// UNBOUND: key.usr: "s:10TestModule6WidgetV6method5labelySi_tF"
// UNBOUND: key.modulename: "TestModule"

// COMPOUND: key.kind: source.lang.swift.ref.function.method.instance
// COMPOUND: key.name: "method(label:)"
// COMPOUND: key.usr: "s:10TestModule6WidgetV6method5labelySi_tF"
// COMPOUND: key.modulename: "TestModule"

//--- TestModule.swift
public struct Widget {
  public init() {}
  public func method(label: Int) {}
}

//--- main.swift
import TestModule

let unbound: () = TestModule::Widget.TestModule::method(Widget())(label: 1)
let compound: () = TestModule::Widget.TestModule::method(label:)(Widget())(1)
