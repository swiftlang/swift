// RUN: %target-swift-emit-silgen %s \
// RUN:   -enable-experimental-feature Lifetimes \
// RUN:   -disable-availability-checking \
// RUN:   -module-name test | %FileCheck %s

// REQUIRES: swift_feature_Lifetimes

// A borrowing for-each loop binds its sequence to an implicit local
// ('$<pattern>$sequence') whose scope encloses the loop. When the sequence is
// noncopyable that binding is a borrow, so SILGenFunction::emitExprInto must
// initialize it without copying the initializer.

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
// the iterator.

// CHECK-LABEL: sil hidden [ossa] @$s4test17temporarySequenceyyF : $@convention(thin) () -> () {
// CHECK:         [[MAKE:%.*]] = function_ref @$s4test12makeSequenceAA011NoncopyableC0VyF
// CHECK:         [[SEQ:%.*]] = apply [[MAKE]]()
// CHECK-NOT:     copy_value [[SEQ]]
// CHECK:         [[BORROW:%.*]] = begin_borrow [[SEQ]]
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

// A sequence read directly from a parameter is not rebound at all: the
// parameter is live for the whole body, so its scope already covers the loop.

// CHECK-LABEL: sil hidden [ossa] @$s4test26borrowingParameterSequenceyyAA011NoncopyableD0VF : $@convention(thin) (@guaranteed NoncopyableSequence) -> () {
// CHECK-NOT:     name "$element$sequence"
// CHECK:       } // end sil function '$s4test26borrowingParameterSequenceyyAA011NoncopyableD0VF'
func borrowingParameterSequence(_ seq: borrowing NoncopyableSequence) {
  for element in seq { _ = element }
}
