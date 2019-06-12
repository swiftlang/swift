// RUN: %target-swift-frontend -emit-sil %s -Onone -Xllvm \
// RUN:   -sil-print-after=mandatory-inlining \
// RUN:   -enable-ownership-stripping-after-serialization \
// RUN:   -Xllvm -sil-print-debuginfo -o /dev/null 2>&1 | %FileCheck %s

// CHECK: begin_borrow {{.*}} : $OSLog, loc {{.*}}, scope 3
// CHECK: tuple (), loc {{.*}}, scope 3
// CHECK: end_borrow %3 : $OSLog, loc {{.*}}, scope 3

import os

func bar() {
  foo(OSLog.default)
}

@_transparent
func foo(_ logObject: OSLog) { }
