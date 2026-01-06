// REQUIRES: objc_interop

// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-sil %s -Onone -Xllvm \
// RUN:   -sil-print-after=mandatory-inlining \
// RUN:   -Xllvm -sil-print-types -Xllvm -sil-print-debuginfo -o /dev/null 2>&1 | %FileCheck %s

// CHECK: sil_scope [[S0:[0-9]+]] { {{.*}} parent @$s4null3baryyF
// CHECK: sil_scope [[S1:[0-9]+]] { loc "{{.*}}":18:3 parent [[S0]] }
// CHECK: sil_scope [[S2:[0-9]+]] { loc "{{.*}}":18:82 parent [[S1]] }

// CHECK: begin_borrow {{.*}} : $OSLog, loc {{.*}}, scope [[S2]]
// CHECK: tuple (), loc {{.*}}, scope [[S2]]
// CHECK: end_borrow %9 : $OSLog, loc {{.*}}, scope [[S2]]

import os

func bar() {
  if #available(macOS 10.14, iOS 12.0, watchOS 5.0, tvOS 12.0, visionOS 9999, *) {
    foo(OSLog.default)
  }
}

@_transparent
func foo(_ logObject: OSLog) { }
