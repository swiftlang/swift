// RUN: %target-swift-emit-silgen %s | %FileCheck %s

// Ownership specifiers on subscript parameters.
//
// A `borrowing` index is borrowed for the whole formal access on the storage,
// so every accessor that access runs sees the same borrow -- the index is
// never copied, which is what lets a noncopyable type be used as an index.

struct NC: ~Copyable {
  var value: Int
}

struct Table {
  var slots: [Int] = [0]

  subscript(nc: borrowing NC) -> Int {
    get { slots[nc.value] }
    set { slots[nc.value] = newValue }
  }

  subscript(coro nc: borrowing NC) -> Int {
    _read { yield slots[nc.value] }
    _modify { yield &slots[nc.value] }
  }
}

// The accessors take the index `@guaranteed`, not `@owned`.
// CHECK-LABEL: sil hidden [ossa] @$s26subscript_ownership_params5TableVySiAA2NCVcig
// CHECK-SAME:  $@convention(method) (@guaranteed NC, @guaranteed Table) -> Int
// CHECK-LABEL: sil hidden [ossa] @$s26subscript_ownership_params5TableVySiAA2NCVcis
// CHECK-SAME:  $@convention(method) (Int, @guaranteed NC, @inout Table) -> ()

// A read borrows the index and does not copy it.
// CHECK-LABEL: sil hidden [ossa] @$s26subscript_ownership_params4read1t1iSiAA5TableV_AA2NCVtF
// CHECK-NOT:     copy_value {{.*}} : $NC
// CHECK-NOT:     explicit_copy_value {{.*}} : $NC
// CHECK:       } // end sil function
func read(t: Table, i: borrowing NC) -> Int {
  return t[i]
}

// A read-modify-write runs the getter and then the setter, and both receive the
// *same* borrow of the index rather than separate copies.
// CHECK-LABEL: sil hidden [ossa] @$s26subscript_ownership_params9readWrite1t1iyAA5TableVz_AA2NCVtF
// CHECK:         [[IDX:%[0-9]+]] = begin_borrow
// CHECK:         [[GET:%[0-9]+]] = function_ref @$s26subscript_ownership_params5TableVySiAA2NCVcig
// CHECK:         apply [[GET]]([[IDX]], {{%[0-9]+}})
// CHECK:         [[SET:%[0-9]+]] = function_ref @$s26subscript_ownership_params5TableVySiAA2NCVcis
// CHECK:         apply [[SET]]({{%[0-9]+}}, [[IDX]], {{%[0-9]+}})
// CHECK-NOT:     copy_value {{.*}} : $NC
// CHECK:       } // end sil function
func readWrite(t: inout Table, i: borrowing NC) {
  t[i] += 1
}

// Same for coroutine accessors.
// CHECK-LABEL: sil hidden [ossa] @$s26subscript_ownership_params4coro1t1iyAA5TableVz_AA2NCVtF
// CHECK-NOT:     copy_value {{.*}} : $NC
// CHECK:       } // end sil function
func coro(t: inout Table, i: borrowing NC) {
  t[coro: i] += 1
}

// A copyable index still works, and a trivial one is passed directly.
// CHECK-LABEL: sil hidden [ossa] @$s26subscript_ownership_params7TrivialVyS2icig
// CHECK-SAME:  $@convention(method) (Int, @guaranteed Trivial) -> Int
struct Trivial {
  var slots: [Int] = [0]
  subscript(i: borrowing Int) -> Int {
    get { slots[i] }
    set { slots[i] = newValue }
  }
}

// A generic subscript over a possibly-noncopyable index.
struct Generic {
  subscript<T: ~Copyable>(t: borrowing T) -> Int { return 0 }
}

// Protocol requirement and witness.
protocol HasBorrowingSubscript {
  subscript(i: borrowing Int) -> Int { get }
}
struct Witness: HasBorrowingSubscript {
  subscript(i: borrowing Int) -> Int { return i }
}

// Class override.
class Base {
  subscript(i: borrowing Int) -> Int { return i }
}
class Derived: Base {
  override subscript(i: borrowing Int) -> Int { return i + 1 }
}

// An `inout` index is a single exclusive access that spans the whole formal
// access on the storage, so every accessor the access runs mutates through the
// same access -- the argument is passed as an address, not re-evaluated.

struct Counting {
  var slots: [Int] = [0]

  subscript(i: inout Int) -> Int {
    get { slots[i] }
    set { slots[i] = newValue; i += 1 }
  }

  subscript(coro i: inout Int) -> Int {
    _read { yield slots[i] }
    _modify { yield &slots[i] }
  }
}

// The accessors take the index `@inout`.
// CHECK-LABEL: sil hidden [ossa] @$s26subscript_ownership_params8CountingVyS2izcig
// CHECK-SAME:  $@convention(method) (@inout Int, @guaranteed Counting) -> Int
// CHECK-LABEL: sil hidden [ossa] @$s26subscript_ownership_params8CountingVyS2izcis
// CHECK-SAME:  $@convention(method) (Int, @inout Int, @inout Counting) -> ()

// A read opens one access on the index and passes it to the getter.
// CHECK-LABEL: sil hidden [ossa] @$s26subscript_ownership_params9inOutRead1c1iSiAA8CountingV_SiztF
// CHECK:         [[ACCESS:%[0-9]+]] = begin_access [modify] [unknown] %1
// CHECK:         [[GET:%[0-9]+]] = function_ref @$s26subscript_ownership_params8CountingVyS2izcig
// CHECK:         apply [[GET]]([[ACCESS]], {{%[0-9]+}})
// CHECK:         end_access [[ACCESS]]
// CHECK:       } // end sil function
func inOutRead(c: Counting, i: inout Int) -> Int {
  return c[&i]
}

// A read-modify-write runs the getter and then the setter, and both mutate
// through the *same* access -- it is opened once and ended once.
// CHECK-LABEL: sil hidden [ossa] @$s26subscript_ownership_params14inOutReadWrite1c1iyAA8CountingVz_SiztF
// CHECK:         [[ACCESS:%[0-9]+]] = begin_access [modify] [unknown] %1
// CHECK:         [[GET:%[0-9]+]] = function_ref @$s26subscript_ownership_params8CountingVyS2izcig
// CHECK:         apply [[GET]]([[ACCESS]], {{%[0-9]+}})
// CHECK:         [[SET:%[0-9]+]] = function_ref @$s26subscript_ownership_params8CountingVyS2izcis
// CHECK:         apply [[SET]]({{%[0-9]+}}, [[ACCESS]], {{%[0-9]+}})
// CHECK:         end_access
// CHECK:       } // end sil function
func inOutReadWrite(c: inout Counting, i: inout Int) {
  c[&i] += 1
}

// Same for a coroutine accessor pair.
// CHECK-LABEL: sil hidden [ossa] @$s26subscript_ownership_params9inOutCoro1c1iyAA8CountingVz_SiztF
// CHECK:         [[ACCESS:%[0-9]+]] = begin_access [modify] [unknown] %1
// CHECK:         [[MODIFY:%[0-9]+]] = function_ref @$s26subscript_ownership_params8CountingV4coroS2iz_tciM
// CHECK:         begin_apply [[MODIFY]]([[ACCESS]], {{%[0-9]+}})
// CHECK:       } // end sil function
func inOutCoro(c: inout Counting, i: inout Int) {
  c[coro: &i] += 1
}

// A noncopyable `inout` index.
struct NCTable {
  var slots: [Int] = [0]
  subscript(nc: inout NC) -> Int {
    get { slots[nc.value] }
    set { slots[nc.value] = newValue }
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s26subscript_ownership_params7NCTableVySiAA2NCVzcig
// CHECK-SAME:  $@convention(method) (@inout NC, @guaranteed NCTable) -> Int

// CHECK-LABEL: sil hidden [ossa] @$s26subscript_ownership_params12inOutNCIndex1t1iyAA7NCTableVz_AA2NCVztF
// CHECK-NOT:     copy_value {{.*}} : $NC
// CHECK:       } // end sil function
func inOutNCIndex(t: inout NCTable, i: inout NC) {
  t[&i] += 1
}

// A `consuming` index is handed to the accessor `@owned`: the accessor takes
// ownership of it. That is only allowed where a single accessor performs a
// whole access, so the index is consumed exactly once.

struct Consuming {
  var slots: [Int] = [0]
  subscript(nc: consuming NC) -> Int {
    _read { yield slots[nc.value] }
    _modify { yield &slots[nc.value] }
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s26subscript_ownership_params9ConsumingVySiAA2NCVncir
// CHECK-SAME:  $@yield_once @convention(method) (@owned NC, @guaranteed Consuming) -> @yields Int
// CHECK-LABEL: sil hidden [ossa] @$s26subscript_ownership_params9ConsumingVySiAA2NCVnciM
// CHECK-SAME:  $@yield_once @convention(method) (@owned NC, @inout Consuming) -> @yields @inout Int

// The index is forwarded into the coroutine, not copied for it.
// CHECK-LABEL: sil hidden [ossa] @$s26subscript_ownership_params14consumingIndex1c2ncSiAA9ConsumingV_AA2NCVntF
// CHECK-NOT:     copy_value {{.*}} : $NC
// CHECK:         [[READ:%[0-9]+]] = function_ref @$s26subscript_ownership_params9ConsumingVySiAA2NCVncir
// CHECK:         begin_apply [[READ]]({{%[0-9]+}}, {{%[0-9]+}})
// CHECK:       } // end sil function
func consumingIndex(c: Consuming, nc: consuming NC) -> Int {
  return c[nc]
}

// A read-modify-write runs only the `_modify` coroutine, so the index is still
// consumed once.
// CHECK-LABEL: sil hidden [ossa] @$s26subscript_ownership_params17consumingIndexRMW1c2ncyAA9ConsumingVz_AA2NCVntF
// CHECK-NOT:     copy_value {{.*}} : $NC
// CHECK:         [[MODIFY:%[0-9]+]] = function_ref @$s26subscript_ownership_params9ConsumingVySiAA2NCVnciM
// CHECK:         begin_apply [[MODIFY]]({{%[0-9]+}}, {{%[0-9]+}})
// CHECK:       } // end sil function
func consumingIndexRMW(c: inout Consuming, nc: consuming NC) {
  c[nc] += 1
}
