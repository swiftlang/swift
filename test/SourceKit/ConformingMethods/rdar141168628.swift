@resultBuilder struct Builder {
  static func buildBlock<T>(_ x: T) -> T { x }
}

struct S {}

struct R<T> {
  init(@Builder fn: () -> T) {}
}

// rdar://141168628 - Make sure we can compute the USR here without crashing.
R {
  S()
  // RUN: %sourcekitd-test -req=conformingmethods -pos %(line + 1):2 %s  -- %s | %FileCheck %s
}
// CHECK: key.typeusr: "$s13rdar1411686281RVyAA1SVGD"
