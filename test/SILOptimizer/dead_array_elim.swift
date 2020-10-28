// RUN: %target-swift-frontend -O -emit-sil -primary-file %s | %FileCheck %s

// REQUIRES: swift_stdlib_no_asserts

// These tests check whether DeadObjectElimination pass runs as a part of the
// optimization pipeline and eliminates dead array literals in Swift code.
// Note that DeadObjectElimination pass relies on @_semantics annotations on
// the array initializer that is used by the compiler to create array literals.
// This test would fail if in case the initializer used by the compiler to
// initialize array literals doesn't match the one expected by the pass.

// testDeadArrayElimination requires swift_stdlib_no_asserts because,
// with runtime verification enabled, "array.finalize" becomes a
// mutating operation, preventing SILCombine from deleting it when it
// removes dead pure instructions. After inlining,
// DeadObjectElimination is still unable to remove the array because a
// second array is initialized by copying the first. This problem can be
// overcome by handling non-trivial stores in OSSA, as described here:
//   [OSSA] Improve DeadObjectElimination to handle array copies
//   https://bugs.swift.org/browse/SR-13782
// Once that bug is fixed, remove the requirement: swift_stdlib_no_asserts.

// CHECK-LABEL: sil hidden @$s15dead_array_elim24testDeadArrayEliminationyyF
func testDeadArrayElimination() {
  _ = [1, 2, 3]
    // CHECK: bb0:
    // CHECK-NEXT: %{{.*}} = tuple ()
    // CHECK-NEXT: return %{{.*}} : $()
}

// CHECK-LABEL: sil hidden @$s15dead_array_elim29testEmptyDeadArrayEliminationyyF
func testEmptyDeadArrayElimination() {
  _ = []
    // CHECK: bb0:
    // CHECK-NEXT: %{{.*}} = tuple ()
    // CHECK-NEXT: return %{{.*}} : $()
}

// The use case tested by the following test, where a _fixLifetime call is
// invoked on an array, appears when new os log APIs are  used.
// CHECK-LABEL: sil hidden @$s15dead_array_elim35testDeadArrayElimWithFixLifetimeUseyyF
func testDeadArrayElimWithFixLifetimeUse() {
  let a: [Int] = []
  _fixLifetime(a)
    // CHECK: bb0:
    // CHECK-NEXT: %{{.*}} = tuple ()
    // CHECK-NEXT: return %{{.*}} : $()
}

// FIXME: DeadObjectElimination doesn't optimize this yet.
func testDeadArrayElimWithAddressOnlyValues<T>(x: T, y: T) {
  _ = [x, y]
}
