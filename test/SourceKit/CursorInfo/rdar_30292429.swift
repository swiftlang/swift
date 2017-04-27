// Checks that we don't crash.
// RUN: %sourcekitd-test -req=cursor -pos=10:17 %s -- %s | %FileCheck -check-prefix=CHECK %s
// RUN: %sourcekitd-test -req=cursor -pos=13:22 %s -- %s | %FileCheck -check-prefix=CHECK2 %s

class A {
  var field: Bool = true
  var items: [Int] = []

  func method() {
    if /*here:*/field {}
    // CHECK: source.lang.swift.ref.var.instance

    for _ in /*here*/items {}
    // CHECK2: source.lang.swift.ref.var.instance
  }
}
