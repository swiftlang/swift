// RUN: %empty-directory(%t/split)
// RUN: %{python} %utils/split_file.py -o %t/split %s
// RUN: %empty-directory(%t/build)
// RUN: %target-swift-frontend -emit-module -module-name MyModule -emit-module-path %t/build/MyModule.swiftmodule -emit-module-source-info-path %t/build/MyModule.swiftsourceinfo %t/split/Action.swift
// RUN: %sourcekitd-test -req=cursor -req-opts=retrieve_symbol_graph=1 -pos=5:14 %t/split/test.swift -- %t/split/test.swift -I %t/build -target %target-triple | %FileCheck %s

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
