func test() {
  // RUN: %sourcekitd-test -req=cursor -pos=%(line + 1):3 %s -- %s | %FileCheck %s
  Swift.min
}

// CHECK: source.lang.swift.ref.module ()
// CHECK-NEXT: Swift
