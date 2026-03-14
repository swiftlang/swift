// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/build)
// RUN: split-file --leading-lines %s %t

// RUN: %target-swift-frontend -emit-module -module-name MyModule -emit-module-path %t/build/MyModule.swiftmodule -emit-module-source-info-path %t/build/MyModule.swiftsourceinfo %t/Action.swift

//--- Action.swift
public protocol Action {}

//--- test.swift
import MyModule

// RUN: %sourcekitd-test -req=cursor -pos=%(line+1):19 -length=6 -cursor-action %t/test.swift -- %t/test.swift -target %target-triple -I %t/build | %FileCheck %s
func test(action: Action) { }
// CHECK: ACTIONS BEGIN
// CHECK-NEXT: source.refactoring.kind.rename.global
// CHECK-NEXT: Global Rename
// CHECK-NOT: cannot be renamed
