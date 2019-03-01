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
