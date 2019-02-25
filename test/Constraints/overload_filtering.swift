// RUN: %target-typecheck-verify-swift -debug-constraints 2>%t.err
// RUN: %FileCheck %s < %t.err

// This test ensures that we are eagerly filtering out overloads based on
// argument labels, arity, etc., before they become disjunctions in the
// constraint system. The presence of the word "disjunction" in the output of
// -debug-constraints indicates that a disjunction constraint was formed,
// indicating a problem with the eager filtering.
// CHECK-NOT: disjunction

// RUN: %target-typecheck-verify-swift -debug-constraints -DCANARY 2>%t.canary
// RUN: %FileCheck -check-prefix=CHECK-CANARY %s < %t.canary

// The "canary" test ensures that the output of -debug-constraints includes the
// word "disjunction" in a case where we do have a disjunction constraint. A
// failure on the line below implies that either (1) the debugging output
// changed, or (2) the solver got smart enough to eliminate more overloads
// earlier, in which case the canary test should be made more difficult.

// CHECK-CANARY: disjunction

#if CANARY
extension X {
  subscript(_: Int?) -> Float { return 0 }
}
#endif

struct X {
  subscript(_: Int) -> Int { return 0 }
  subscript(_: Int, _: Int) -> Double { return 0 }
  subscript(_: Int, _: Int, _: Int) -> String { return "" }

  init(_: Int) { }
  init(_: Int, _: Int) { }
  init(_: Int, _: Int, _: Int) { }
}

func testSubscript(x: X, i: Int) {
  _ = x[i]
  _ = x[i, i]
  _ = x[i, i, i]
}

func testUnresolvedMember(i: Int) -> X {
  return .init(i, i)
}

func foo(_: Int) { }
func foo(_: Int, _: Int) { }
func foo(_: Int, _: Int, _: Int) { }

func testModuleScope(i: Int) {
  foo(i)
  foo(i, i)
  foo(i, i, i)
}
