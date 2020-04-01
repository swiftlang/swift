// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module %S/Inputs/nontransparent.swift -O -parse-stdlib -parse-as-library -emit-module -o %t/Swift.swiftmodule -module-name=Swift -module-link-name swiftCore
// RUN: %target-swift-frontend %s -O -I %t -emit-sil -o - | %FileCheck %s

import Swift

// Make sure we inline everything.

// CHECK-LABEL: sil @main
// CHECK: bb0({{.*}}):
// CHECK-NEXT: alloc_global
// CHECK-NEXT: global_addr
// CHECK-NEXT: struct
// CHECK-NEXT: struct
// CHECK-NEXT: store
// CHECK-NEXT: integer_literal
// CHECK-NEXT: return

public var a = doSomething()
a.isBConfused()
