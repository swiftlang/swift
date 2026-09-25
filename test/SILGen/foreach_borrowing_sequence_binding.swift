// RUN: %target-swift-emit-silgen %s \
// RUN:   -enable-experimental-feature Lifetimes \
// RUN:   -disable-availability-checking \
// RUN:   -module-name test | %FileCheck %s

// REQUIRES: swift_feature_Lifetimes

// A borrowing for-each loop borrows its sequence in an implicit local
// ('$<pattern>$sequence') whose scope encloses the loop. The binding is a
// borrow whether or not the sequence is copyable, so it must be initialized
// without copying the initializer.

struct NoncopyableSequence: ~Copyable, Iterable {
  struct BorrowingIterator: ~Copyable, ~Escapable, BorrowingIteratorProtocol {
    @_lifetime(&self)
    mutating func nextSpan(maxCount: Int) throws(Never) -> Span<Int> { Span() }
  }

  @_lifetime(borrow self)
  func makeBorrowingIterator() -> BorrowingIterator { BorrowingIterator() }
}

func makeSequence() -> NoncopyableSequence { NoncopyableSequence() }

// The sequence expression is a call, so there is no storage to borrow in place.
// The result is materialized and borrowed, and the temporary is destroyed along
// with the enclosing scope rather than at the end of the statement that formed
// the iterator. The borrow is marked as the binding's variable scope, since
// there is no access to storage for the iterator to depend on.

// CHECK-LABEL: sil hidden [ossa] @$s4test17temporarySequenceyyF : $@convention(thin) () -> () {
// CHECK:         [[MAKE:%.*]] = function_ref @$s4test12makeSequenceAA011NoncopyableC0VyF
// CHECK:         [[SEQ:%.*]] = apply [[MAKE]]()
// CHECK-NOT:     copy_value [[SEQ]]
// CHECK:         [[BORROW:%.*]] = begin_borrow [var_decl] [[SEQ]]
// CHECK:         debug_value [[BORROW]], let, name "$element$sequence"
// CHECK-NOT:     mark_unresolved_non_copyable_value {{.*}}[[BORROW]]
// CHECK:         [[MAKE_ITER:%.*]] = function_ref @$s4test19NoncopyableSequenceV21makeBorrowingIteratorAC0eF0VyF
// CHECK:         apply [[MAKE_ITER]]([[BORROW]])
// CHECK:         debug_value {{%.*}}, let, name "element"
// CHECK:         end_borrow [[BORROW]]
// CHECK:         destroy_value [[SEQ]]
// CHECK:         [[VOID:%.*]] = tuple ()
// CHECK:         return [[VOID]]
// CHECK:       } // end sil function '$s4test17temporarySequenceyyF'
func temporarySequence() {
  for element in makeSequence() { _ = element }
}

// The sequence expression refers to storage, so the binding borrows it in place
// instead of materializing a second copy of the value.

// CHECK-LABEL: sil hidden [ossa] @$s4test13localSequenceyyF : $@convention(thin) () -> () {
// CHECK:         [[BOX:%.*]] = alloc_box ${ let NoncopyableSequence }, let, name "seq"
// CHECK:         [[LIFETIME:%.*]] = begin_borrow [lexical] [var_decl] [[BOX]]
// CHECK:         [[ADDR:%.*]] = project_box [[LIFETIME]], 0
// CHECK:         store {{%.*}} to [init] [[ADDR]]
// CHECK-NOT:     copy_value
// CHECK:         [[CHECKED:%.*]] = mark_unresolved_non_copyable_value [strict] [no_consume_or_assign]
// CHECK:         debug_value [[CHECKED]], let, name "$element$sequence", expr op_deref
// CHECK:         [[LOADED:%.*]] = load_borrow [[CHECKED]]
// CHECK:         [[MAKE_ITER:%.*]] = function_ref @$s4test19NoncopyableSequenceV21makeBorrowingIteratorAC0eF0VyF
// CHECK:         apply [[MAKE_ITER]]([[LOADED]])
// CHECK:       } // end sil function '$s4test13localSequenceyyF'
func localSequence() {
  let seq = NoncopyableSequence()
  for element in seq { _ = element }
}

// A sequence read from a parameter is borrowed without copying it.

// CHECK-LABEL: sil hidden [ossa] @$s4test26borrowingParameterSequenceyyAA011NoncopyableD0VF : $@convention(thin) (@guaranteed NoncopyableSequence) -> () {
// CHECK:         [[PARAM:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign]
// CHECK:         debug_value [[PARAM]], let, name "seq"
// CHECK:         [[BORROW:%.*]] = begin_borrow [[PARAM]]
// CHECK:         debug_value [[BORROW]], let, name "$element$sequence"
// CHECK-NOT:     copy_value
// CHECK:         [[MAKE_ITER:%.*]] = function_ref @$s4test19NoncopyableSequenceV21makeBorrowingIteratorAC0eF0VyF
// CHECK:         apply [[MAKE_ITER]]([[BORROW]])
// CHECK:       } // end sil function '$s4test26borrowingParameterSequenceyyAA011NoncopyableD0VF'
func borrowingParameterSequence(_ seq: borrowing NoncopyableSequence) {
  for element in seq { _ = element }
}

// A copyable sequence is borrowed in place too, rather than copied out of its
// storage: the read access on the variable is what the iterator depends on.

struct CopyableSequence: Iterable {
  struct BorrowingIterator: ~Copyable, ~Escapable, BorrowingIteratorProtocol {
    @_lifetime(&self)
    mutating func nextSpan(maxCount: Int) throws(Never) -> Span<Int> { Span() }
  }

  @_lifetime(borrow self)
  func makeBorrowingIterator() -> BorrowingIterator { BorrowingIterator() }

  var payload: AnyObject? = nil
}

// CHECK-LABEL: sil hidden [ossa] @$s4test20mutableCopyableLocalyyF : $@convention(thin) () -> () {
// CHECK:         [[BOX:%.*]] = alloc_box ${ var CopyableSequence }, var, name "seq"
// CHECK:         [[LIFETIME:%.*]] = begin_borrow [lexical] [var_decl] [[BOX]]
// CHECK:         [[ADDR:%.*]] = project_box [[LIFETIME]], 0
// CHECK:         [[ACCESS:%.*]] = begin_access [read] [unknown] [[ADDR]]
// CHECK:         debug_value [[ACCESS]], let, name "$element$sequence", expr op_deref
// CHECK-NOT:     copy
// CHECK:         [[LOADED:%.*]] = load_borrow [[ACCESS]]
// CHECK-NOT:     copy
// CHECK:         [[MAKE_ITER:%.*]] = function_ref @$s4test16CopyableSequenceV21makeBorrowingIterator{{[_0-9a-zA-Z]*}}F
// CHECK:         apply [[MAKE_ITER]]([[LOADED]])
// CHECK:         end_borrow [[LOADED]]
// CHECK:       } // end sil function '$s4test20mutableCopyableLocalyyF'
func mutableCopyableLocal() {
  var seq = CopyableSequence()
  seq = CopyableSequence()
  for element in seq { _ = element }
}
