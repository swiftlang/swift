// RUN: %target-typecheck-verify-swift -debug-constraints 2>%t.err
// RUN: %FileCheck %s < %t.err

struct X {
  func g(_: Inting) -> Int { return 0 }
  func g(_: Doubling) -> Int { return 0 }

  subscript(_: Inting) -> String { return "" }
  subscript(_: Doubling) -> String { return "" }

  func iuo(_: Inting) -> Int! { return 0 }
  func iuo(_: Doubling) -> Int! { return 0 }
}

struct Y {
  func g(_: Inting) -> Double { return 0 }
  func g(_: Doubling) -> Double { return 0 }

  subscript(_: Inting) -> Substring { return "" }
  subscript(_: Doubling) -> Substring { return "" }

  func iuo(_: Inting) -> Double! { return 0 }
  func iuo(_: Doubling) -> Double! { return 0 }
}

protocol Inting { }
extension Int: Inting { }

protocol Doubling { }
extension Double: Doubling { }

func f(_: Inting) -> X { return X() }
func f(_: Doubling) -> Y { return Y() }

func testCallCommonType() {
  // CHECK: overload set choice binding $T{{[0-9]+}} := (Inting) -> X
  // CHECK-NEXT: (common result type for $T{{[0-9]+}} is Int)
  // CHECK: (overload set choice binding $T{{[0-9]+}} := (Doubling) -> Y)
  // CHECK-NEXT: (common result type for $T{{[0-9]+}} is Double)
  _ = f(0).g(0)
}

func testSubscriptCommonType() {
  // CHECK: subscript_expr
  // CHECK: overload set choice binding $T{{[0-9]+}} := (Inting) -> X
  // CHECK: (common result type for $T{{[0-9]+}} is String)
  // CHECK: (overload set choice binding $T{{[0-9]+}} := (Doubling) -> Y)
  // CHECK: (common result type for $T{{[0-9]+}} is Substring)
  _ = f(0)[0]
}

func testCommonTypeIUO() {
  // CHECK: overload set choice binding $T{{[0-9]+}} := (Inting) -> X
  // CHECK-NOT: common result type
    _ = f(0).iuo(0)
}

struct Z {
  init(a: Int) { }
  init(a: Double) { }

  init(b: Int) { }
  init?(b: Double) { }
}

func testCommonTypeInit() {
  // CHECK: common result type for {{.*}} is Z
  _ = Z(a: 0)

  // CHECK-NOT: common result type
  _ = Z(b: 0)
}

class DynamicSelf {
  func foo(_ a: Int) -> Self { return self }
  func foo(_ a: Double) -> Self { return self }
}

class InheritsDynamicSelf: DynamicSelf {
}

func testCommonTypeDynamicSelf(ds: DynamicSelf, ids: InheritsDynamicSelf) {
  // CHECK: common result type for {{.*}} is DynamicSelf
  _ = ds.foo(0)
  // CHECK: common result type for {{.*}} is InheritsDynamicSelf
  _ = ids.foo(0)
}

