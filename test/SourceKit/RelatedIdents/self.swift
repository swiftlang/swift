struct Test {
  func foo() {
// RUN: %sourcekitd-test -req=related-idents -pos=%(line + 1):6 %s -- %s | %FileCheck %s
    self
  }
}

// CHECK: START RANGES
// CHECK-NEXT: END RANGES
