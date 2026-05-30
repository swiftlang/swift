// RUN: %target-swift-frontend -parse-as-library -O -Xllvm -sil-print-types -emit-sil %s | %FileCheck %s

@_transparent func makeTuple<each T>(_ t: repeat each T) -> (repeat each T) {
  return (repeat each t)
}

// CHECK-LABEL: sil @$s17tuples_from_packs14makeEmptyTupleyyF : $@convention(thin) () -> () {
// CHECK: bb0:
// CHECK-NEXT: [[RET:%.*]] = tuple ()
// CHECK-NEXT: return [[RET]] : $()
public func makeEmptyTuple() {
  return makeTuple()
}

// CHECK-LABEL: sil @$s17tuples_from_packs7makeOneyxxlF : $@convention(thin) <T> (@in_guaranteed T) -> @out T {
// CHECK: bb0(%0 : $*T, %1 : $*T):
// CHECK: copy_addr %1 to [init] %0 : $*T
// CHECK-NEXT: [[RET:%.*]] = tuple ()
// CHECK-NEXT: return [[RET]] : $()
public func makeOne<T>(_ t: T) -> T {
  return makeTuple(t)
}

// CHECK-LABEL: sil @$s17tuples_from_packs8makePairyx_q_tx_q_tr0_lF : $@convention(thin) <T, U> (@in_guaranteed T, @in_guaranteed U) -> (@out T, @out U) {
// CHECK: bb0(%0 : $*T, %1 : $*U, %2 : $*T, %3 : $*U):
// CHECK: copy_addr %2 to [init] %0 : $*T
// CHECK-NEXT: copy_addr %3 to [init] %1 : $*U
// CHECK-NEXT: [[RET:%.*]] = tuple ()
// CHECK-NEXT: return [[RET]] : $()

public func makePair<T, U>(_ t: T, _ u: U) -> (T, U) {
  return makeTuple(t, u)
}
