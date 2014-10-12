// RUN: rm -rf %t; mkdir -p %t; %swift -emit-module %S/Inputs/nontransparent.swift -O -sil-serialize-all -parse-stdlib -parse-as-library -emit-module -o %t/Swift.swiftmodule -module-name=Swift -module-link-name swiftCore
// RUN: %swift %s -O -I=%t -emit-sil -o - | FileCheck %s

import Swift

// Make sure we inline everything.

// CHECK-LABEL: sil @main
// CHECK: bb0({{.*}}):
// CHECK-NEXT: sil_global_addr
// CHECK-NEXT: struct
// CHECK-NEXT: struct
// CHECK-NEXT: store
// CHECK-NEXT: integer_literal
// CHECK-NEXT: return

var a = doSomething()
a.isBConfused()
