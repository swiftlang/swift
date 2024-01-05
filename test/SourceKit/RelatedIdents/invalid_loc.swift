// REQUIRES: swift_swift_parser
// RUN: %sourcekitd-test -req=related-idents -pos=%(line + 1):1 %s -- %s | %FileCheck %s

// CHECK: START RANGES
// CHECK-NEXT: END RANGES

