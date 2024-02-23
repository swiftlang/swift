// RUN: %target-swift-frontend -O -emit-sil -disable-availability-checking %s | %FileCheck %s

// REQUIRES: synchronization

import Synchronization

//===----------------------------------------------------------------------===//
// Ensure that we don't destroy the atomic before operations
//===----------------------------------------------------------------------===//

// CHECK-LABEL: @localLoad
// CHECK: [[ATOMIC:%.*]] = alloc_stack [lexical] $Atomic<Int>
// CHECK: [[ATOMIC_PTR:%.*]] = address_to_pointer [[ATOMIC]] : $*Atomic<Int> to $Builtin.RawPointer
// CHECK-NEXT: [[LOAD:%.*]] = builtin "atomicload_monotonic_Int64"([[ATOMIC_PTR]] : $Builtin.RawPointer) : $Builtin.Int64
// CHECK-NEXT: [[INT:%.*]] = struct $Int ([[LOAD]] : $Builtin.Int64)
// CHECK-NEXT: destroy_addr [[ATOMIC]] : $*Atomic<Int>
// CHECK-NEXT: dealloc_stack [[ATOMIC]] : $*Atomic<Int>
// CHECK-NEXT: return [[INT]] : $Int
@_silgen_name("localLoad")
func localLoad() -> Int {
  let x = Atomic(128)
  return x.load(ordering: .relaxed)
}

// CHECK-LABEL: @localStore
// CHECK: [[ATOMIC:%.*]] = alloc_stack [lexical] $Atomic<Int>
// CHECK: [[STORE_INT:%.*]] = integer_literal $Builtin.Int64, 0
// CHECK-NEXT: [[ATOMIC_PTR:%.*]] = address_to_pointer [[ATOMIC]] : $*Atomic<Int> to $Builtin.RawPointer
// CHECK-NEXT: {{%.*}} = builtin "atomicstore_release_Int64"([[ATOMIC_PTR]] : $Builtin.RawPointer, [[STORE_INT]] : $Builtin.Int64) : $()
// CHECK-NEXT: destroy_addr [[ATOMIC]] : $*Atomic<Int>
// CHECK-NEXT: dealloc_stack [[ATOMIC]] : $*Atomic<Int>
// CHECK: return {{%.*}} : $()
@_silgen_name("localStore")
func localStore() {
  let x = Atomic(128)
  x.store(0, ordering: .releasing)
}

// CHECK-LABEL: @localExchange
// CHECK: [[ATOMIC:%.*]] = alloc_stack [lexical] $Atomic<Int>
// CHECK: [[EXCHANGE_INT:%.*]] = integer_literal $Builtin.Int64, 0
// CHECK: [[ATOMIC_PTR:%.*]] = address_to_pointer [[ATOMIC]] : $*Atomic<Int> to $Builtin.RawPointer
// CHECK-NEXT: [[LOAD:%.*]] = builtin "atomicrmw_xchg_acquire_Int64"([[ATOMIC_PTR]] : $Builtin.RawPointer, [[EXCHANGE_INT]] : $Builtin.Int64) : $Builtin.Int64
// CHECK-NEXT: [[INT:%.*]] = struct $Int ([[LOAD]] : $Builtin.Int64)
// CHECK-NEXT: destroy_addr [[ATOMIC]] : $*Atomic<Int>
// CHECK-NEXT: dealloc_stack [[ATOMIC]] : $*Atomic<Int>
// CHECK-NEXT: return [[INT]] : $Int
@_silgen_name("localExchange")
func localExchange() -> Int {
  let x = Atomic(128)
  return x.exchange(0, ordering: .acquiring)
}

// CHECK-LABEL: @localCompareExchange
// CHECK: [[ATOMIC:%.*]] = alloc_stack [lexical] $Atomic<Int>
// CHECK-NEXT: [[EXPECTED_INT:%.*]] = integer_literal $Builtin.Int64, 128
// CHECK: [[DESIRED_INT:%.*]] = integer_literal $Builtin.Int64, 316
// CHECK-NEXT: [[ATOMIC_PTR:%.*]] = address_to_pointer [[ATOMIC]] : $*Atomic<Int> to $Builtin.RawPointer
// CHECK-NEXT: [[CMPXCHG:%.*]] = builtin "cmpxchg_seqcst_seqcst_Int64"([[ATOMIC_PTR]] : $Builtin.RawPointer, [[EXPECTED_INT]] : $Builtin.Int64, [[DESIRED_INT]] : $Builtin.Int64) : $(Builtin.Int64, Builtin.Int1)
// CHECK-NEXT: [[LOADED:%.*]] = tuple_extract [[CMPXCHG]] : $(Builtin.Int64, Builtin.Int1), 0
// CHECK-NEXT: [[EXCHANGED:%.*]] = tuple_extract [[CMPXCHG]] : $(Builtin.Int64, Builtin.Int1), 1
// CHECK-NEXT: [[BOOL:%.*]] = struct $Bool ([[EXCHANGED]] : $Builtin.Int1)
// CHECK-NEXT: [[INT:%.*]] = struct $Int ([[LOADED]] : $Builtin.Int64)
// CHECK-NEXT: destroy_addr [[ATOMIC]] : $*Atomic<Int>
// CHECK-NEXT: dealloc_stack [[ATOMIC]] : $*Atomic<Int>
// CHECK-NEXT: [[RETURN:%.*]] = tuple ([[BOOL]] : $Bool, [[INT]] : $Int)
// CHECK-NEXT: return [[RETURN]] : $(Bool, Int)
@_silgen_name("localCompareExchange")
func localCompareExchange() -> (exchanged: Bool, original: Int) {
  let x = Atomic(128)
  return x.compareExchange(
    expected: 128,
    desired: 316,
    ordering: .sequentiallyConsistent
  )
}
