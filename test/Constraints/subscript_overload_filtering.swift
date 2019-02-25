// RUN: %target-typecheck-verify-swift -debug-constraints 2>%t.err
// RUN: %FileCheck %s < %t.err

// CHECK-NOT: disjunction{{.*}}subscript
struct X {
  subscript(_: Int) -> Int { return 0 }
  subscript(_: Int, _: Int) -> Double { return 0 }
  subscript(_: Int, _: Int, _: Int) -> String { return "" }
}

func testFoo(x: X, i: Int) {
  _ = x[i]
  _ = x[i, i]
  _ = x[i, i, i]
}

