// RUN: %target-swift-frontend -parse-as-library -O -Xllvm -sil-print-types -emit-sil %s | %FileCheck %s

@_transparent func makeTuple<each T>(_ t: repeat each T) -> (repeat each T) {
  return (repeat each t)
}

// Eliminate useless alloc_pack/dealloc_pack

// CHECK-LABEL: sil @$s17tuples_from_packs14makeEmptyTupleyyF : $@convention(thin) () -> () {
// CHECK: bb0:
// CHECK-NEXT: [[RET:%.*]] = tuple ()
// CHECK-NEXT: return [[RET]] : $()
// CHECK-NEXT: } // end sil function '$s17tuples_from_packs14makeEmptyTupleyyF'
public func makeEmptyTuple() {
  return makeTuple()
}

// Eliminate useless pack_element_set/pack_element_get

// CHECK-LABEL: sil @$s17tuples_from_packs7makeOneyxxlF : $@convention(thin) <T> (@in_guaranteed T) -> @out T {
// CHECK:       bb0(%0 : $*T, %1 : $*T):
// CHECK-NEXT:    debug_value %1 : $*T, let, name "t", argno 1, expr op_deref
// CHECK-NEXT:    copy_addr %1 to [init] %0 : $*T
// CHECK-NEXT:    [[RET:%[0-9]+]] = tuple ()
// CHECK-NEXT:    return [[RET]] : $()
// CHECK-NEXT:  } // end sil function '$s17tuples_from_packs7makeOneyxxlF'
public func makeOne<T>(_ t:   T) -> T {
  return makeTuple(t)
}

// Eliminate useless pack_element_set/pack_element_get

// CHECK-LABEL: sil @$s17tuples_from_packs8makePairyx_q_tx_q_tr0_lF : $@convention(thin) <T, U> (@in_guaranteed T, @in_guaranteed U) -> (@out T, @out U) {
// CHECK:       bb0(%0 : $*T, %1 : $*U, %2 : $*T, %3 : $*U):
// CHECK-NEXT:    debug_value %2 : $*T, let, name "t", argno 1, expr op_deref
// CHECK-NEXT:    debug_value %3 : $*U, let, name "u", argno 2, expr op_deref
// CHECK-NEXT:    copy_addr %3 to [init] %1
// CHECK-NEXT:    copy_addr %2 to [init] %0
// CHECK-NEXT:    %8 = tuple ()
// CHECK-NEXT:    return %8
// CHECK-NEXT:  } // end sil function '$s17tuples_from_packs8makePairyx_q_tx_q_tr0_lF'
public func makePair<T, U>(_ t: T, _ u: U) -> (T, U) {
  return makeTuple(t, u)
}
