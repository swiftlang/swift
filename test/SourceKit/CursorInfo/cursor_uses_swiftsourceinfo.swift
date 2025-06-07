// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/build)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -module-name MyModule -emit-module-path %t/build/MyModule.swiftmodule -emit-module-source-info-path %t/build/MyModule.swiftsourceinfo %t/Action.swift
// RUN: %sourcekitd-test -req=cursor -req-opts=retrieve_symbol_graph=1 -pos=5:14 %t/test.swift -- %t/test.swift -I %t/build -target %target-triple | %FileCheck %s

// We should also get source doc info if we execute a code completion request (which doesn't need source doc info) first
// RUN: %sourcekitd-test \
// RUN: -req=complete %t/test.swift -pos=5:14 -- %t/test.swift -I %t/build -target %target-triple == \
// RUN: -req=cursor -req-opts=retrieve_symbol_graph=1 -pos=5:14 %t/test.swift -- %t/test.swift -I %t/build -target %target-triple

// CHECK: REFERENCED DECLS BEGIN
// CHECK-NEXT: s:8MyModule6ActionP | public | {{.*}}Action.swift | MyModule | User | NonSPI | source.lang.swift
// CHECK-NEXT: Action swift.protocol s:8MyModule6ActionP
// CHECK-NEXT: REFERENCED DECLS END

//--- Action.swift
public protocol Action {}

//--- test.swift
import MyModule

func test(action: Action) {
    switch action {
    case let action

