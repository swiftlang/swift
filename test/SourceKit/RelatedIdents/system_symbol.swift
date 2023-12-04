// REQUIRES: swift_swift_parser
// RUN: %sourcekitd-test -req=related-idents -pos=%(line + 1):13 %s -- %s | %FileCheck %s
func foo(x: String) {
  let a: String = "abc"
}

// CHECK: START RANGES
// CHECK-NEXT: 3:13 - 6
// CHECK-NEXT: 4:10 - 6
// CHECK-NEXT: END RANGES
