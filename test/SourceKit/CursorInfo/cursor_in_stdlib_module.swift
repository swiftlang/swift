func test() {
  // RUN: %sourcekitd-test -req=cursor -pos=%(line + 1):9 %s -- %s | %FileCheck %s
  Swift.min(1, 2)
}

// CHECK: source.lang.swift.ref.function.free ()
// CHECK-NEXT: min(_:_:)
// CHECK-NEXT: s:s3minyxx_xtSLRzlF
// CHECK-NEXT: source.lang.swift
// CHECK-NEXT: <T where T : Comparable> (T, T) -> T
// CHECK-NEXT: $syxx_xtcSLRzluD
// CHECK-NEXT: Swift
