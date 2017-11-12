// Checks that we don't crash.
// RUN: %sourcekitd-test -req=cursor -pos=9:25 %s -- %s | %FileCheck %s
// RUN: %sourcekitd-test -req=cursor -pos=11:13 %s -- %s | %FileCheck -check-prefix=CHECK2 %s

class A {
  static prefix func +(_:A) {}

  func method() {
    let _ = "" /*here:*/== ""
    // CHECK: source.lang.swift.ref.function.operator.infix
    /*here*/+A()
    // CHECK2: source.lang.swift.ref.function.operator.prefix
  }
}
