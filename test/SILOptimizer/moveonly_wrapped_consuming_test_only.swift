// Verify that MoveOnlyTypeEliminator no longer crashes when we have
// `is` on a `consuming` parameter

// RUN: %target-swift-frontend -emit-sil -module-name main %s | %FileCheck %s
//
// The remaining lines have no FileCheck on purpose: the assertion is that the
// compiler does not abort, and `-sil-verify-all` additionally checks the SIL the
// eliminator leaves behind is well formed. There is nothing textual to match.
// RUN: %target-swift-frontend -emit-sil -O -module-name main -o /dev/null %s
// RUN: %target-swift-frontend -emit-sil -sil-verify-all -module-name main -o /dev/null %s
// RUN: %target-swift-frontend -emit-ir -module-name main -o /dev/null %s

// The cast's operand must be unwrapped by the time the pass is finished: the
// source type prints as `S`, not `@moveOnly S`. (The `alloc_stack` above it keeps
// `type $@moveOnly S` as a debug annotation, which is expected and is why these
// checks look at the cast line specifically.)

// CHECK-LABEL: sil hidden @$s4main7genericySbxn_q_mtr0_lF
// CHECK: checked_cast_addr_br test_only S in %{{[0-9]+}} to T
// CHECK: } // end sil function '$s4main7genericySbxn_q_mtr0_lF'
func generic<S, T>(_ x: consuming S, _: T.Type) -> Bool { x is T }

// Concrete `Any` source, generic target.
// CHECK-LABEL: sil hidden @$s4main7fromAnyySbypn_xmtlF
// CHECK: checked_cast_addr_br test_only Any in %{{[0-9]+}} to T
func fromAny<T>(_ x: consuming Any, _: T.Type) -> Bool { x is T }

// `if case is` takes a different SILGen path than `switch`, and was the other
// form that crashed.
// CHECK-LABEL: sil hidden @$s4main6ifCaseySbxn_q_mtr0_lF
// CHECK: checked_cast_addr_br test_only S in %{{[0-9]+}} to T
func ifCase<S, T>(_ x: consuming S, _: T.Type) -> Bool {
  if case is T = x { true } else { false }
}

// `switch` did not crash, but belongs here so a future change cannot regress one
// form without the other noticing.
// CHECK-LABEL: sil hidden @$s4main8switchOnySbxn_q_mtr0_lF
// CHECK: checked_cast_addr_br test_only S in %{{[0-9]+}} to T
func switchOn<S, T>(_ x: consuming S, _: T.Type) -> Bool {
  switch x { case is T: true; default: false }
}

// The test does not take the source, so consuming it afterwards is legal and must
// not be diagnosed

// CHECK-LABEL: sil hidden @$s4main12testThenSinkySbxn_q_mtr0_lF
// CHECK: checked_cast_addr_br test_only S in %{{[0-9]+}} to T
func testThenSink<S, T>(_ x: consuming S, _: T.Type) -> Bool {
  let answer = x is T
  sink(x)
  return answer
}

func sink<S>(_ x: consuming S) {}

// CHECK-NOT: checked_cast_addr_br test_only @moveOnly
