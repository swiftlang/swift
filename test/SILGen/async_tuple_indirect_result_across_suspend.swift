// RUN: %target-swift-frontend -emit-silgen %s -module-name test -disable-availability-checking | %FileCheck %s

// REQUIRES: concurrency

// Regression test for a -O miscompile of an async function that returns a tuple
// literal (into an @out indirect result) whose *later* element awaits.
//
// SILGen used to initialize the @out result buffer element-by-element, in
// source order, storing an earlier element into the @out buffer *before* the
// suspension caused by a later element:
//
//   // INCORRECT SIL (before the fix):
//   %2 = tuple_element_addr %0 : $*(Int, Int), 0
//   %3 = tuple_element_addr %0 : $*(Int, Int), 1
//   store %1 to [trivial] %2 : $*Int                     // elt 0, BEFORE the suspend
//   %4 = function_ref @echo : $@convention(thin) @async (Int) -> Int
//   %5 = apply %4(%1) : $@convention(thin) @async (Int) -> Int  // suspension point
//   hop_to_executor %e
//   store %5 to [trivial] %3 : $*Int                     // elt 1, after resuming

func echo(_ i: Int) async -> Int { i }

@inline(never)
func sink<T>(_ body: @Sendable () async -> T) async -> T { await body() }

func twoInts(_ index: Int) async -> (Int, Int) {
  await sink {
    (index, await echo(index))
  }
}

// The already-available first element (index, %1) must not be written into the
// @out buffer before the suspension; the whole tuple is built and stored once,
// after resuming.
// CHECK-LABEL: sil {{.*}}twoInts{{.*}}yYaYbXEfU_ : $@convention(thin) @Sendable @async @substituted <τ_0_0> (Int) -> @out τ_0_0 for <(Int, Int)> {
// CHECK:         bb0(%0 : $*(Int, Int), %1 : @closureCapture $Int):
// CHECK-NOT:     tuple_element_addr
// CHECK-NOT:     store %1 to
// CHECK:         [[R:%[0-9]+]] = apply {{%[0-9]+}}(%1)
// CHECK:         [[T:%[0-9]+]] = tuple (%1, [[R]])
// CHECK:         store [[T]] to [trivial] %0
