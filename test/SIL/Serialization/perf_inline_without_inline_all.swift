// RUN: rm -rf %t; mkdir -p %t; %swift -emit-module %S/Inputs/nontransparent.swift -O2 -sil-serialize-all -parse-stdlib -parse-as-library -emit-module -o %t/Swift.swiftmodule -module-name=Swift -module-link-name swift_stdlib_core
// RUN: %swift %s -O2 -I=%t -emit-sil -o - | FileCheck %s

import Swift

// Make sure we inline everything.

// CHECK-LABEL: sil private @top_level_code : $@thin () -> () {
// CHECK: bb0:
// CHECK-NEXT: global_addr
// CHECK-NEXT: struct
// CHECK-NEXT: struct
// CHECK-NEXT: store
// CHECK-NEXT: tuple
// CHECK-NEXT: return

var a = doSomething()
a.isBConfused()
