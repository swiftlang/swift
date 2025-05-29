// RUN: %target-swift-frontend -parse-as-library -O -Xllvm -sil-print-types -emit-sil %s | %FileCheck %s

@_transparent func makeTuple<each T>(_ t: repeat each T) -> (repeat each T) {
  return (repeat each t)
}

// FIXME: Useless alloc_pack/dealloc_pack

// CHECK-LABEL: sil @$s17tuples_from_packs14makeEmptyTupleyyF : $@convention(thin) () -> () {
// CHECK: bb0:
// CHECK-NEXT: %0 = alloc_pack $Pack{}
// CHECK-NEXT: %1 = alloc_pack $Pack{}
// CHECK-NEXT: dealloc_pack %1 : $*Pack{}
// CHECK-NEXT: dealloc_pack %0 : $*Pack{}
// CHECK-NEXT: [[RET:%.*]] = tuple ()
// CHECK-NEXT: return [[RET]] : $()
public func makeEmptyTuple() {
  return makeTuple()
}

// FIXME: Useless pack_element_set/pack_element_get

// CHECK-LABEL: sil @$s17tuples_from_packs7makeOneyxxlF : $@convention(thin) <T> (@in_guaranteed T) -> @out T {
// CHECK: bb0(%0 : $*T, %1 : $*T):
// CHECK: [[PACK:%.*]] = alloc_pack $Pack{T}
// CHECK-NEXT: [[IDX:%.*]] = scalar_pack_index 0 of $Pack{T}
// CHECK-NEXT: pack_element_set %0 : $*T into [[IDX]] of [[PACK]] : $*Pack{T}
// CHECK-NEXT: [[PACK2:%.*]] = alloc_pack $Pack{T}
// CHECK-NEXT: [[BOX:%.*]] = alloc_stack $T
// CHECK-NEXT: copy_addr %1 to [init] [[BOX]] : $*T
// CHECK-NEXT: pack_element_set [[BOX]] : $*T into [[IDX]] of [[PACK2]] : $*Pack{T}
// CHECK-NEXT: [[ELT:%.*]] = pack_element_get [[IDX]] of [[PACK]] : $*Pack{T} as $*T
// CHECK-NEXT: [[ELT2:%.*]] = pack_element_get [[IDX]] of [[PACK2]] : $*Pack{T} as $*T
// CHECK-NEXT: copy_addr [[ELT2]] to [init] [[ELT]] : $*T
// CHECK-NEXT: destroy_addr [[BOX]] : $*T
// CHECK-NEXT: dealloc_stack [[BOX]] : $*T
// CHECK-NEXT: dealloc_pack [[PACK2]] : $*Pack{T}
// CHECK-NEXT: dealloc_pack [[PACK]] : $*Pack{T}
// CHECK-NEXT: [[RET:%.*]] = tuple ()
// CHECK-NEXT: return [[RET]] : $()
public func makeOne<T>(_ t: T) -> T {
  return makeTuple(t)
}

// FIXME: Useless pack_element_set/pack_element_get

// CHECK-LABEL: sil @$s17tuples_from_packs8makePairyx_q_tx_q_tr0_lF : $@convention(thin) <T, U> (@in_guaranteed T, @in_guaranteed U) -> (@out T, @out U) {
// CHECK: bb0(%0 : $*T, %1 : $*U, %2 : $*T, %3 : $*U):
// CHECK:      [[PACK:%.*]] = alloc_pack $Pack{T, U}
// CHECK-NEXT: [[IDX0:%.*]] = scalar_pack_index 0 of $Pack{T, U}
// CHECK-NEXT: pack_element_set %0 : $*T into [[IDX0]] of [[PACK]] : $*Pack{T, U}
// CHECK-NEXT: [[IDX1:%.*]] = scalar_pack_index 1 of $Pack{T, U}
// CHECK-NEXT: pack_element_set %1 : $*U into [[IDX1]] of [[PACK]] : $*Pack{T, U}
// CHECK-NEXT: [[PACK2:%.*]] = alloc_pack $Pack{T, U}
// CHECK-NEXT: [[T:%.*]] = alloc_stack $T
// CHECK-NEXT: copy_addr %2 to [init] [[T]] : $*T
// CHECK-NEXT: pack_element_set [[T]] : $*T into [[IDX0]] of [[PACK2]] : $*Pack{T, U}
// CHECK-NEXT: [[U:%.*]] = alloc_stack $U
// CHECK-NEXT: copy_addr %3 to [init] [[U]] : $*U
// CHECK-NEXT: pack_element_set [[U]] : $*U into [[IDX1]] of [[PACK2]] : $*Pack{T, U}
// CHECK-NEXT: [[ELT0:%.*]] = pack_element_get [[IDX0]] of [[PACK]] : $*Pack{T, U} as $*T
// CHECK-NEXT: [[ELT02:%.*]] = pack_element_get [[IDX0]] of [[PACK2]] : $*Pack{T, U} as $*T
// CHECK-NEXT: copy_addr [[ELT02]] to [init] [[ELT0]] : $*T
// CHECK-NEXT: [[ELT1:%.*]] = pack_element_get [[IDX1]] of [[PACK]] : $*Pack{T, U} as $*U
// CHECK-NEXT: [[ELT12:%.*]] = pack_element_get [[IDX1]] of [[PACK2]] : $*Pack{T, U} as $*U
// CHECK-NEXT: copy_addr [[ELT12]] to [init] [[ELT1]] : $*U
// CHECK-NEXT: destroy_addr [[U]] : $*U
// CHECK-NEXT: dealloc_stack [[U]] : $*U
// CHECK-NEXT: destroy_addr [[T]] : $*T
// CHECK-NEXT: dealloc_stack [[T]] : $*T
// CHECK-NEXT: dealloc_pack [[PACK2]] : $*Pack{T, U}
// CHECK-NEXT: dealloc_pack [[PACK]] : $*Pack{T, U}
// CHECK-NEXT: %30 = tuple ()
// CHECK-NEXT: return %30 : $()

public func makePair<T, U>(_ t: T, _ u: U) -> (T, U) {
  return makeTuple(t, u)
}
