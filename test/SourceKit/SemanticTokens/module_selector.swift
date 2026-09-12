// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module -module-name TestModule -emit-module-path %t/TestModule.swiftmodule %t/TestModule.swift

// RUN: %sourcekitd-test -req=semantic-tokens %t/main.swift -- %t/main.swift -I %t -target %target-triple | %FileCheck %s --check-prefix=RANGES
// RUN: %sourcekitd-test -req=open %t/main.swift -- %t/main.swift -I %t -target %target-triple == -req=print-annotations %t/main.swift | %FileCheck %s --check-prefix=RANGES

// RANGES:      key.kind: source.lang.swift.ref.struct
// RANGES-NEXT: key.offset: 49
// RANGES-NEXT: key.length: 6
// RANGES:      key.kind: source.lang.swift.ref.function.method.instance
// RANGES-NEXT: key.offset: 68
// RANGES-NEXT: key.length: 6
// RANGES:      key.kind: source.lang.swift.ref.struct
// RANGES-NEXT: key.offset: 75
// RANGES-NEXT: key.length: 6
// RANGES:      key.kind: source.lang.swift.ref.struct
// RANGES-NEXT: key.offset: 126
// RANGES-NEXT: key.length: 6
// RANGES:      key.kind: source.lang.swift.ref.function.method.instance
// RANGES-NEXT: key.offset: 145
// RANGES-NEXT: key.length: 14
// RANGES:      key.kind: source.lang.swift.ref.struct
// RANGES-NEXT: key.offset: 160
// RANGES-NEXT: key.length: 6

//--- TestModule.swift
public struct Widget {
  public init() {}
  public func method(label: Int) {}
}

//--- main.swift
import TestModule

let unbound: () = TestModule::Widget.TestModule::method(Widget())(label: 1)
let compound: () = TestModule::Widget.TestModule::method(label:)(Widget())(1)
