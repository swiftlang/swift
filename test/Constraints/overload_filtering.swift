// RUN: %target-typecheck-verify-swift -debug-constraints 2>%t.err
// RUN: %FileCheck %s < %t.err

// This test ensures that we are filtering out overloads based on argument
// labels, arity, etc., before those terms are visited. 

func foo(_: Int) { }
func foo(_: Int, _: Int) { }
func foo(_: Int, _: Int, _: Int) { }

func testModuleScope(i: Int) {
  // CHECK: (disabled disjunction term {{.*}} (Int) -> ()
  // CHECK-NEXT: (disabled disjunction term {{.*}} (Int, Int, Int) -> ()
  // CHECK: (introducing single enabled disjunction term {{.*}} (Int, Int) -> ()
  foo(i, i)
}

struct X {
  subscript(_: Int) -> Int { return 0 }
  subscript(_: Int, _: Int) -> Double { return 0 }
  subscript(_: Int, _: Int, _: Int) -> String { return "" }

  init(_: Int) { }
  init(_: Int, _: Int) { }
  init(_: Int, _: Int, _: Int) { }
}

func testSubscript(x: X, i: Int) {
  // CHECK: disabled disjunction term {{.*}}X.subscript(_:)
  // CHECK-NEXT: disabled disjunction term {{.*}}X.subscript(_:_:_:)
  // CHECK-NEXT: introducing single enabled disjunction term {{.*}} bound to decl overload_filtering.(file).X.subscript(_:_:)
  _ = x[i, i]
}

func testUnresolvedMember(i: Int) -> X {
  // CHECK: disabled disjunction term {{.*}} bound to decl overload_filtering.(file).X.init(_:)
  // CHECK-NEXT: disabled disjunction term {{.*}} bound to decl overload_filtering.(file).X.init(_:_:_:)
  // CHECK-NEXT: introducing single enabled disjunction term {{.*}} bound to decl overload_filtering.(file).X.init(_:_:)
  return .init(i, i)
}

func test_member_filtering() {
  struct S {
    // Result types here are different intentionally,
    // if there were the same simplification logic would
    // trigger and disable overloads during constraint
    // generation.
    func foo(_: Int) -> S { S() }
    func foo(_: String) -> Int { 42 }

    func bar(v: String) {}
    func bar(_: Int) {}
    func bar(a: Double, b: Int) {}
  }

  func test(s: S) {
    // CHECK: disabled disjunction term {{.*}} bound to decl overload_filtering.(file).test_member_filtering().S.bar(v:)
    // CHECK-NEXT: disabled disjunction term {{.*}} bound to decl overload_filtering.(file).test_member_filtering().S.bar(a:b:)
    // CHECK-NEXT: introducing single enabled disjunction term {{.*}} bound to decl overload_filtering.(file).test_member_filtering().S.bar
    s.foo(42).bar(42)
  }
}
