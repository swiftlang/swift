// REQUIRES: objc_interop

// RUN: %target-swift-frontend -emit-sil %s -Onone -Xllvm \
// RUN:   -sil-print-after=mandatory-inlining \
// RUN:   -Xllvm -sil-print-debuginfo -o /dev/null 2>&1 | %FileCheck %s

// CHECK: begin_borrow {{.*}} : $OSLog, loc {{.*}}, scope 5
// CHECK: tuple (), loc {{.*}}, scope 5
// CHECK: end_borrow %9 : $OSLog, loc {{.*}}, scope 5

import os

func bar() {
  if #available(macOS 10.14, iOS 12.0, watchOS 5.0, tvOS 12.0, *) {
    foo(OSLog.default)
  }
}

@_transparent
func foo(_ logObject: OSLog) { }
