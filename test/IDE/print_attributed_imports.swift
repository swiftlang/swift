// RUN: %empty-directory(%t)

// RUN: echo 'public func module1() {}' >> %t/module1.swift
// RUN: echo 'public func module2() {}' >> %t/module2.swift
// RUN: %target-swift-frontend -emit-module -module-name Module1 -o %t %t/module1.swift
// RUN: %target-swift-frontend -emit-module -module-name Module2 -o %t %t/module2.swift

// RUN: %target-swift-frontend -I %t -emit-module -o %t/AttrImports.swiftmodule %S/print_attributed_imports.swift
// RUN: %target-swift-ide-test -I %t -print-module -source-filename %s -module-to-print=AttrImports | %FileCheck %s

@_exported import Module1
@_implementationOnly import Module2

// CHECK: import Module1
// CHECK-NOT: import Module2
