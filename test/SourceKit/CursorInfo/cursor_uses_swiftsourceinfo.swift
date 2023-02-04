// RUN: %empty-directory(%t/split)
// RUN: %{python} %utils/split_file.py -o %t/split %s
// RUN: %empty-directory(%t/build)
// RUN: %target-swift-frontend -emit-module -module-name MyModule -emit-module-path %t/build/MyModule.swiftmodule -emit-module-source-info-path %t/build/MyModule.swiftsourceinfo %t/split/Action.swift
// RUN: %sourcekitd-test -req=cursor -req-opts=retrieve_symbol_graph=1 -pos=5:14 %t/split/test.swift -- %t/split/test.swift -I %t/build -target %target-triple | %FileCheck %s

// We should also get source doc info if we execute a code completion request (which doesn't need source doc info) first
// RUN: %sourcekitd-test \
// RUN: -req=complete %t/split/test.swift -pos=5:14 -- %t/split/test.swift -I %t/build -target %target-triple == \
// RUN: -req=cursor -req-opts=retrieve_symbol_graph=1 -pos=5:14 %t/split/test.swift -- %t/split/test.swift -I %t/build -target %target-triple

// BEGIN Action.swift
public protocol Action {}

// BEGIN test.swift
import MyModule

func test(action: Action) {
    switch action {
    case let action

// CHECK: REFERENCED DECLS BEGIN
// CHECK-NEXT: s:8MyModule6ActionP | public | {{.*}}/split/Action.swift | MyModule | User | NonSPI | source.lang.swift
// CHECK-NEXT: Action swift.protocol s:8MyModule6ActionP
// CHECK-NEXT: REFERENCED DECLS END
