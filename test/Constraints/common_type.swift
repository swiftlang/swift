// RUN: %target-typecheck-verify-swift -debug-constraints 2>%t.err
// RUN: %FileCheck %s < %t.err

struct X {
  func g(_: Int) -> Int { return 0 }
  func g(_: Double) -> Int { return 0 }

  subscript(_: Int) -> String { return "" }
  subscript(_: Double) -> String { return "" }
}

struct Y {
  func g(_: Int) -> Double { return 0 }
  func g(_: Double) -> Double { return 0 }

  subscript(_: Int) -> Substring { return "" }
  subscript(_: Double) -> Substring { return "" }
}

func f(_: Int) -> X { return X() }
func f(_: Double) -> Y { return Y() }

func testCallCommonType() {
  // CHECK: overload set choice binding $T{{[0-9]+}} := (Int) -> X
  // CHECK-NEXT: (common result type for $T{{[0-9]+}} is Int)
  // CHECK: (overload set choice binding $T{{[0-9]+}} := (Double) -> Y)
  // CHECK-NEXT: (common result type for $T{{[0-9]+}} is Double)
  _ = f(0).g(0)
}

func testSubscriptCommonType() {
  // FIXME: This will work once we have more filtering of subscripts.
  // CHECK: subscript_expr
  // CHECK: overload set choice binding $T{{[0-9]+}} := (Int) -> X
  // CHECK-NOT: (common result type for $T{{[0-9]+}} is String)
  // CHECK: (overload set choice binding $T{{[0-9]+}} := (Double) -> Y)
  // CHECK-NOT: (common result type for $T{{[0-9]+}} is Substring)
  _ = f(0)[0]
}
