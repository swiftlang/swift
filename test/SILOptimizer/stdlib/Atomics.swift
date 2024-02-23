// RUN: %target-swift-frontend -O -emit-sil -disable-availability-checking %s | %FileCheck %s

// REQUIRES: synchronization

import Synchronization

//===----------------------------------------------------------------------===//
// Ensure that we don't destroy the atomic before operations
//===----------------------------------------------------------------------===//

// CHECK-LABEL: sil {{.*}} @localLoad {{.*}} {
// CHECK:         [[ATOMIC:%.*]] = alloc_stack [lexical] $Atomic<Int>
// CHECK:         [[ATOMIC_PTR:%.*]] = address_to_pointer [[ATOMIC]]
// CHECK:         builtin "atomicload_monotonic_Int64"([[ATOMIC_PTR]] : $Builtin.RawPointer)
// CHECK:         destroy_addr [[ATOMIC]] : $*Atomic<Int>
// CHECK-NEXT:    dealloc_stack [[ATOMIC]] : $*Atomic<Int>
// CHECK-LABEL: } // end sil function 'localLoad'
@_silgen_name("localLoad")
func localLoad() -> Int {
  let x = Atomic(128)
  return x.load(ordering: .relaxed)
}

// CHECK-LABEL: sil {{.*}} @localStore {{.*}} {
// CHECK:         [[ATOMIC:%.*]] = alloc_stack [lexical] $Atomic<Int>
// CHECK:         [[ATOMIC_PTR:%.*]] = address_to_pointer [[ATOMIC]]
// CHECK:         builtin "atomicstore_release_Int64"([[ATOMIC_PTR]] : $Builtin.RawPointer
// CHECK:         destroy_addr [[ATOMIC]] : $*Atomic<Int>
// CHECK-NEXT:    dealloc_stack [[ATOMIC]] : $*Atomic<Int>
// CHECK-LABEL: } // end sil function 'localStore'
@_silgen_name("localStore")
func localStore() {
  let x = Atomic(128)
  x.store(0, ordering: .releasing)
}

// CHECK-LABEL: sil {{.*}} @localExchange {{.*}} {
// CHECK:         [[ATOMIC:%.*]] = alloc_stack [lexical] $Atomic<Int>
// CHECK:         [[ATOMIC_PTR:%.*]] = address_to_pointer [[ATOMIC]]
// CHECK:         builtin "atomicrmw_xchg_acquire_Int64"([[ATOMIC_PTR]] : $Builtin.RawPointer
// CHECK:         destroy_addr [[ATOMIC]] : $*Atomic<Int>
// CHECK-NEXT:    dealloc_stack [[ATOMIC]] : $*Atomic<Int>
// CHECK-LABEL: } // end sil function 'localExchange'
@_silgen_name("localExchange")
func localExchange() -> Int {
  let x = Atomic(128)
  return x.exchange(0, ordering: .acquiring)
}

// CHECK-LABEL: sil {{.*}} @localCompareExchange {{.*}} {
// CHECK:         [[ATOMIC:%.*]] = alloc_stack [lexical] $Atomic<Int>
// CHECK:         [[ATOMIC_PTR:%.*]] = address_to_pointer [[ATOMIC]]
// CHECK:         builtin "cmpxchg_seqcst_seqcst_Int64"([[ATOMIC_PTR]] : $Builtin.RawPointer
// CHECK:         destroy_addr [[ATOMIC]] : $*Atomic<Int>
// CHECK-NEXT:    dealloc_stack [[ATOMIC]] : $*Atomic<Int>
// CHECK-LABEL: } // end sil function 'localCompareExchange'
@_silgen_name("localCompareExchange")
func localCompareExchange() -> (exchanged: Bool, original: Int) {
  let x = Atomic(128)
  return x.compareExchange(
    expected: 128,
    desired: 316,
    ordering: .sequentiallyConsistent
  )
}
