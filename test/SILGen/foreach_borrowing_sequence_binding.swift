// RUN: %target-swift-emit-silgen %s \
// RUN:   -enable-experimental-feature Lifetimes \
// RUN:   -disable-availability-checking \
// RUN:   -module-name test | %FileCheck %s

// RUN: not --crash %target-swift-emit-silgen %s \
// RUN:   -enable-experimental-feature Lifetimes \
// RUN:   -enable-sil-opaque-values \
// RUN:   -disable-availability-checking \
// RUN:   -module-name test -o /dev/null

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
// CHECK:         [[PARAM_BORROW:%.*]] = begin_borrow [[PARAM]]
// CHECK:         [[BORROW:%.*]] = begin_borrow [var_decl] [[PARAM_BORROW]]
// CHECK:         debug_value [[BORROW]], let, name "$element$sequence"
// CHECK-NOT:     copy_value
// CHECK:         [[MAKE_ITER:%.*]] = function_ref @$s4test19NoncopyableSequenceV21makeBorrowingIteratorAC0eF0VyF
// CHECK:         apply [[MAKE_ITER]]([[BORROW]])
// CHECK:       } // end sil function '$s4test26borrowingParameterSequenceyyAA011NoncopyableD0VF'
func borrowingParameterSequence(_ seq: borrowing NoncopyableSequence) {
  for element in seq { _ = element }
}

// Iterating an Iterable never copies it. When the sequence refers to storage,
// the binding is the read access on that storage, and the access stays live
// for the whole loop, so it overlaps any modification of the storage in the
// loop body.

// CHECK-LABEL: sil hidden [ossa] @$s4test18reassignDuringLoopyySaySiGF :
// CHECK:         [[BOX:%.*]] = alloc_box ${ var Span<Int> }, var, name "span"
// CHECK:         [[LIFETIME:%.*]] = begin_borrow [var_decl] [[BOX]]
// CHECK:         [[ADDR:%.*]] = project_box [[LIFETIME]], 0
// CHECK:         [[ACCESS:%.*]] = begin_access [read] [unknown] [[ADDR]]
// CHECK-NEXT:    debug_value [[ACCESS]], let, name "$sequence", expr op_deref
// CHECK-NOT:     {{copy_value|load \[copy\]|copy_addr}}
// CHECK:         [[SEQ:%.*]] = load_borrow [[ACCESS]]
// CHECK-NOT:     {{copy_value|load \[copy\]|copy_addr}}
// CHECK:         [[MAKE_ITER:%.*]] = function_ref @$ss4SpanVsRi_zrlE21makeBorrowingIteratorABsRi_zrlE0cD0Vyx_GyF
// CHECK-NEXT:    apply [[MAKE_ITER]]<Int>([[SEQ]])
// CHECK:         begin_access [modify] [unknown] [[ADDR]]
// CHECK:         end_access [[ACCESS]]
// CHECK:       } // end sil function '$s4test18reassignDuringLoopyySaySiGF'
func reassignDuringLoop(_ array: [Int]) {
  var span = array.span
  for _ in span {
    span = array.span
  }
}

struct SpanHolder: ~Escapable {
  var span: Span<Int>

  @_lifetime(copy span)
  init(_ span: Span<Int>) { self.span = span }
}

// CHECK-LABEL: sil hidden [ossa] @$s4test32reassignStoredPropertyDuringLoopyySaySiGF :
// CHECK:         [[BOX:%.*]] = alloc_box ${ var SpanHolder }, var, name "holder"
// CHECK:         [[LIFETIME:%.*]] = begin_borrow [var_decl] [[BOX]]
// CHECK:         [[ADDR:%.*]] = project_box [[LIFETIME]], 0
// CHECK:         [[ACCESS:%.*]] = begin_access [read] [unknown] [[ADDR]]
// CHECK-NEXT:    [[PROJ:%.*]] = struct_element_addr [[ACCESS]], #SpanHolder.span
// CHECK-NEXT:    debug_value [[PROJ]], let, name "$sequence", expr op_deref
// CHECK-NOT:     {{copy_value|load \[copy\]|copy_addr}}
// CHECK:         [[SEQ:%.*]] = load_borrow [[PROJ]]
// CHECK-NOT:     {{copy_value|load \[copy\]|copy_addr}}
// CHECK:         [[MAKE_ITER:%.*]] = function_ref @$ss4SpanVsRi_zrlE21makeBorrowingIteratorABsRi_zrlE0cD0Vyx_GyF
// CHECK-NEXT:    apply [[MAKE_ITER]]<Int>([[SEQ]])
// CHECK:         begin_access [modify] [unknown] [[ADDR]]
// CHECK:         end_access [[ACCESS]]
// CHECK:       } // end sil function '$s4test32reassignStoredPropertyDuringLoopyySaySiGF'
func reassignStoredPropertyDuringLoop(_ array: [Int]) {
  var holder = SpanHolder(array.span)
  for _ in holder.span {
    holder.span = array.span
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s4test32reassignForceUnwrappedDuringLoopyySaySiGF :
// CHECK:         [[BOX:%.*]] = alloc_box ${ var Optional<Span<Int>> }, var, name "span"
// CHECK:         [[LIFETIME:%.*]] = begin_borrow [var_decl] [[BOX]]
// CHECK:         [[ADDR:%.*]] = project_box [[LIFETIME]], 0
// CHECK:         [[ACCESS:%.*]] = begin_access [read] [unknown] [[ADDR]]
// CHECK-NOT:     {{copy_value|load \[copy\]|copy_addr|end_access}}
// CHECK:         debug_value {{.*}}, let, name "$sequence"
// CHECK:         begin_access [modify] [unknown] [[ADDR]]
// CHECK:         end_access [[ACCESS]]
// CHECK:       } // end sil function '$s4test32reassignForceUnwrappedDuringLoopyySaySiGF'
func reassignForceUnwrappedDuringLoop(_ array: [Int]) {
  var span: Span<Int>? = array.span
  for _ in span! {
    span = array.span
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s4test30reassignTupleElementDuringLoopyySaySiGF :
// CHECK:         [[BOX:%.*]] = alloc_box ${ var (Span<Int>, Span<Int>) }, var, name "pair"
// CHECK:         [[LIFETIME:%.*]] = begin_borrow [var_decl] [[BOX]]
// CHECK:         [[ADDR:%.*]] = project_box [[LIFETIME]], 0
// CHECK:         [[ACCESS:%.*]] = begin_access [read] [unknown] [[ADDR]]
// CHECK-NEXT:    [[PROJ:%.*]] = tuple_element_addr [[ACCESS]], 0
// CHECK-NEXT:    debug_value [[PROJ]], let, name "$sequence", expr op_deref
// CHECK-NOT:     {{copy_value|load \[copy\]|copy_addr}}
// CHECK:         [[SEQ:%.*]] = load_borrow [[PROJ]]
// CHECK-NOT:     {{copy_value|load \[copy\]|copy_addr}}
// CHECK:         [[MAKE_ITER:%.*]] = function_ref @$ss4SpanVsRi_zrlE21makeBorrowingIteratorABsRi_zrlE0cD0Vyx_GyF
// CHECK-NEXT:    apply [[MAKE_ITER]]<Int>([[SEQ]])
// CHECK:         begin_access [modify] [unknown] [[ADDR]]
// CHECK:         end_access [[ACCESS]]
// CHECK:       } // end sil function '$s4test30reassignTupleElementDuringLoopyySaySiGF'
func reassignTupleElementDuringLoop(_ array: [Int]) {
  var pair = (array.span, array.span)
  for _ in pair.0 {
    pair.0 = array.span
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s4test29reassignInlineArrayDuringLoopyyF :
// CHECK:         [[BOX:%.*]] = alloc_box ${ var InlineArray<3, Int> }, var, name "array"
// CHECK:         [[LIFETIME:%.*]] = begin_borrow [var_decl] [[BOX]]
// CHECK:         [[ADDR:%.*]] = project_box [[LIFETIME]], 0
// CHECK:         [[ACCESS:%.*]] = begin_access [read] [unknown] [[ADDR]]
// CHECK-NEXT:    debug_value [[ACCESS]], let, name "$x$sequence", expr op_deref
// CHECK-NOT:     {{copy_value|load \[copy\]|copy_addr}}
// CHECK:         [[MAKE_ITER:%.*]] = function_ref @$ss11InlineArrayVsRi__rlE21makeBorrowingIterators4SpanVsRi_zrlE0dE0Vyq__GyF
// CHECK-NEXT:    apply [[MAKE_ITER]]<3, Int>([[ACCESS]])
// CHECK:         begin_access [modify] [unknown] [[ADDR]]
// CHECK:         end_access [[ACCESS]]
// CHECK:       } // end sil function '$s4test29reassignInlineArrayDuringLoopyyF'
func reassignInlineArrayDuringLoop() {
  var array: [3 of Int] = [1, 2, 3]
  for x in array {
    array = [x, x, x]
  }
}

// InlineArray's borrowing iterator depends on the array's address. A sequence
// bound as a value is stored to memory within the binding's scope, so that the
// in-memory borrow ends before the binding's borrow does.

// CHECK-LABEL: sil hidden [ossa] @$s4test28fromNonTrivialInlineArrayLetyyF :
// CHECK:         [[ARRAY:%.*]] = move_value [var_decl] {{%.*}}
// CHECK-NEXT:    debug_value [[ARRAY]], let, name "array"
// CHECK-NEXT:    [[ARRAY_BORROW:%.*]] = begin_borrow [[ARRAY]]
// CHECK-NEXT:    [[BORROW:%.*]] = begin_borrow [var_decl] [[ARRAY_BORROW]]
// CHECK-NEXT:    [[STACK:%.*]] = alloc_stack $InlineArray<3, String>
// CHECK-NEXT:    [[IN_MEMORY:%.*]] = store_borrow [[BORROW]] to [[STACK]]
// CHECK-NEXT:    debug_value [[IN_MEMORY]], let, name "$s$sequence", expr op_deref
// CHECK-NOT:     copy
// CHECK:         [[MAKE_ITER:%.*]] = function_ref @$ss11InlineArrayVsRi__rlE21makeBorrowingIterators4SpanVsRi_zrlE0dE0Vyq__GyF
// CHECK-NEXT:    apply [[MAKE_ITER]]<3, String>([[IN_MEMORY]])
// CHECK:         end_borrow [[IN_MEMORY]]{{ }}
// CHECK-NEXT:    end_borrow [[BORROW]]{{ }}
// CHECK-NEXT:    end_borrow [[ARRAY_BORROW]]{{ }}
// CHECK:         dealloc_stack [[STACK]]{{ }}
// CHECK:         destroy_value [[ARRAY]]{{ }}
// CHECK:       } // end sil function '$s4test28fromNonTrivialInlineArrayLetyyF'
func fromNonTrivialInlineArrayLet() {
  let array: [3 of String] = ["a", "b", "c"]
  for s in array { _ = s }
}

func makeNonTrivialInlineArray() -> [3 of String] { ["a", "b", "c"] }

// CHECK-LABEL: sil hidden [ossa] @$s4test34fromNonTrivialInlineArrayTemporaryyyF :
// CHECK:         [[TEMPORARY:%.*]] = apply {{%.*}}()
// CHECK-NEXT:    [[BORROW:%.*]] = begin_borrow [var_decl] [[TEMPORARY]]
// CHECK-NEXT:    [[STACK:%.*]] = alloc_stack $InlineArray<3, String>
// CHECK-NEXT:    [[IN_MEMORY:%.*]] = store_borrow [[BORROW]] to [[STACK]]
// CHECK-NEXT:    debug_value [[IN_MEMORY]], let, name "$s$sequence", expr op_deref
// CHECK:         [[MAKE_ITER:%.*]] = function_ref @$ss11InlineArrayVsRi__rlE21makeBorrowingIterators4SpanVsRi_zrlE0dE0Vyq__GyF
// CHECK-NEXT:    apply [[MAKE_ITER]]<3, String>([[IN_MEMORY]])
// CHECK:         end_borrow [[IN_MEMORY]]{{ }}
// CHECK-NEXT:    end_borrow [[BORROW]]{{ }}
// CHECK:         dealloc_stack [[STACK]]{{ }}
// CHECK-NEXT:    destroy_value [[TEMPORARY]]{{ }}
// CHECK:       } // end sil function '$s4test34fromNonTrivialInlineArrayTemporaryyyF'
func fromNonTrivialInlineArrayTemporary() {
  for s in makeNonTrivialInlineArray() { _ = s }
}

// A trivial sequence is stored to memory the same way.

// CHECK-LABEL: sil hidden [ossa] @$s4test24fromInlineArrayParameteryys0cD0Vy$2_SiGF :
// CHECK:       bb0([[ARRAY:%.*]] : $InlineArray<3, Int>):
// CHECK:         [[STACK:%.*]] = alloc_stack $InlineArray<3, Int>
// CHECK-NEXT:    store [[ARRAY]] to [trivial] [[STACK]]
// CHECK-NEXT:    debug_value [[STACK]], let, name "$x$sequence", expr op_deref
// CHECK:         [[MAKE_ITER:%.*]] = function_ref @$ss11InlineArrayVsRi__rlE21makeBorrowingIterators4SpanVsRi_zrlE0dE0Vyq__GyF
// CHECK-NEXT:    apply [[MAKE_ITER]]<3, Int>([[STACK]])
// CHECK:         dealloc_stack [[STACK]]{{ }}
// CHECK:       } // end sil function '$s4test24fromInlineArrayParameteryys0cD0Vy$2_SiGF'
func fromInlineArrayParameter(_ array: [3 of Int]) {
  for x in array { _ = x }
}

// So is a `borrowing` parameter, though it is wrapped as move-only.

// CHECK-LABEL: sil hidden [ossa] @$s4test33fromBorrowingInlineArrayParameteryys0dE0Vy$2_SiGF :
// CHECK:         [[ARRAY:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign]
// CHECK:         [[ARRAY_BORROW:%.*]] = begin_borrow [[ARRAY]]
// CHECK-NEXT:    [[BORROW:%.*]] = begin_borrow [var_decl] [[ARRAY_BORROW]]
// CHECK-NEXT:    [[STACK:%.*]] = alloc_stack $@moveOnly InlineArray<3, Int>
// CHECK-NEXT:    [[IN_MEMORY:%.*]] = store_borrow [[BORROW]] to [[STACK]]
// CHECK-NEXT:    [[MARKED:%.*]] = mark_unresolved_non_copyable_value [strict] [no_consume_or_assign] [[IN_MEMORY]]
// CHECK-NEXT:    debug_value [[MARKED]], let, name "$sequence", expr op_deref
// CHECK-NOT:     copy
// CHECK:         [[MAKE_ITER:%.*]] = function_ref @$ss11InlineArrayVsRi__rlE21makeBorrowingIterators4SpanVsRi_zrlE0dE0Vyq__GyF
// CHECK-NEXT:    [[UNWRAPPED:%.*]] = moveonlywrapper_to_copyable_addr [[MARKED]]
// CHECK-NEXT:    apply [[MAKE_ITER]]<3, Int>([[UNWRAPPED]])
// CHECK:         end_borrow [[IN_MEMORY]]{{ }}
// CHECK-NEXT:    end_borrow [[BORROW]]{{ }}
// CHECK-NEXT:    end_borrow [[ARRAY_BORROW]]{{ }}
// CHECK:         dealloc_stack [[STACK]]{{ }}
// CHECK:       } // end sil function '$s4test33fromBorrowingInlineArrayParameteryys0dE0Vy$2_SiGF'
func fromBorrowingInlineArrayParameter(_ array: borrowing [3 of Int]) {
  for _ in array { }
}

// CHECK-LABEL: sil hidden [ossa] @$s4test43fromBorrowingNonTrivialInlineArrayParameteryys0fG0Vy$2_SSGF :
// CHECK:         [[ARRAY:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign]
// CHECK:         [[ARRAY_BORROW:%.*]] = begin_borrow [[ARRAY]]
// CHECK-NEXT:    [[BORROW:%.*]] = begin_borrow [var_decl] [[ARRAY_BORROW]]
// CHECK-NEXT:    [[STACK:%.*]] = alloc_stack $@moveOnly InlineArray<3, String>
// CHECK-NEXT:    [[IN_MEMORY:%.*]] = store_borrow [[BORROW]] to [[STACK]]
// CHECK-NEXT:    [[MARKED:%.*]] = mark_unresolved_non_copyable_value [strict] [no_consume_or_assign] [[IN_MEMORY]]
// CHECK-NEXT:    debug_value [[MARKED]], let, name "$s$sequence", expr op_deref
// CHECK-NOT:     copy
// CHECK:         [[MAKE_ITER:%.*]] = function_ref @$ss11InlineArrayVsRi__rlE21makeBorrowingIterators4SpanVsRi_zrlE0dE0Vyq__GyF
// CHECK-NEXT:    [[UNWRAPPED:%.*]] = moveonlywrapper_to_copyable_addr [[MARKED]]
// CHECK-NEXT:    apply [[MAKE_ITER]]<3, String>([[UNWRAPPED]])
// CHECK:         end_borrow [[IN_MEMORY]]{{ }}
// CHECK-NEXT:    end_borrow [[BORROW]]{{ }}
// CHECK-NEXT:    end_borrow [[ARRAY_BORROW]]{{ }}
// CHECK:         dealloc_stack [[STACK]]{{ }}
// CHECK:         destroy_value [[ARRAY]]{{ }}
// CHECK:       } // end sil function '$s4test43fromBorrowingNonTrivialInlineArrayParameteryys0fG0Vy$2_SSGF'
func fromBorrowingNonTrivialInlineArrayParameter(_ array: borrowing [3 of String]) {
  for s in array { _ = s }
}

// A stored property projected from a `borrowing` self is wrapped as well.

// CHECK-LABEL: sil hidden [ossa] @$s4test27NonTrivialInlineArrayHolderV22fromBorrowingSelfFieldyyF :
// CHECK:         [[SELF:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign]
// CHECK:         [[SELF_BORROW:%.*]] = begin_borrow [[SELF]]
// CHECK-NEXT:    [[ARRAY:%.*]] = struct_extract [[SELF_BORROW]], #NonTrivialInlineArrayHolder.array
// CHECK-NEXT:    [[BORROW:%.*]] = begin_borrow [var_decl] [[ARRAY]]
// CHECK-NEXT:    [[STACK:%.*]] = alloc_stack $@moveOnly InlineArray<3, String>
// CHECK-NEXT:    [[IN_MEMORY:%.*]] = store_borrow [[BORROW]] to [[STACK]]
// CHECK-NEXT:    [[MARKED:%.*]] = mark_unresolved_non_copyable_value [strict] [no_consume_or_assign] [[IN_MEMORY]]
// CHECK-NEXT:    debug_value [[MARKED]], let, name "$s$sequence", expr op_deref
// CHECK-NOT:     copy
// CHECK:         [[MAKE_ITER:%.*]] = function_ref @$ss11InlineArrayVsRi__rlE21makeBorrowingIterators4SpanVsRi_zrlE0dE0Vyq__GyF
// CHECK-NEXT:    [[UNWRAPPED:%.*]] = moveonlywrapper_to_copyable_addr [[MARKED]]
// CHECK-NEXT:    apply [[MAKE_ITER]]<3, String>([[UNWRAPPED]])
// CHECK:         end_borrow [[IN_MEMORY]]{{ }}
// CHECK-NEXT:    end_borrow [[BORROW]]{{ }}
// CHECK:         dealloc_stack [[STACK]]{{ }}
// CHECK-NEXT:    end_borrow [[SELF_BORROW]]{{ }}
// CHECK:       } // end sil function '$s4test27NonTrivialInlineArrayHolderV22fromBorrowingSelfFieldyyF'
struct NonTrivialInlineArrayHolder {
  var array: [3 of String] = ["a", "b", "c"]

  borrowing func fromBorrowingSelfField() {
    for s in array { _ = s }
  }
}

// An escapable iterator does not depend on the sequence; the binding holds
// the access on its own.

struct EscapableIteratorSequence: Iterable {
  struct BorrowingIterator: BorrowingIteratorProtocol {
    @_lifetime(&self)
    mutating func nextSpan(maxCount: Int) throws(Never) -> Span<Int> { Span() }
  }

  func makeBorrowingIterator() -> BorrowingIterator { BorrowingIterator() }

  var payload: AnyObject? = nil
}

// CHECK-LABEL: sil hidden [ossa] @$s4test43reassignEscapableIteratorSequenceDuringLoopyyF :
// CHECK:         [[BOX:%.*]] = alloc_box ${ var EscapableIteratorSequence }, var, name "sequence"
// CHECK:         [[LIFETIME:%.*]] = begin_borrow [lexical] [var_decl] [[BOX]]
// CHECK:         [[ADDR:%.*]] = project_box [[LIFETIME]], 0
// CHECK:         [[ACCESS:%.*]] = begin_access [read] [unknown] [[ADDR]]
// CHECK-NEXT:    debug_value [[ACCESS]], let, name "$sequence", expr op_deref
// CHECK-NOT:     {{copy_value|load \[copy\]|copy_addr}}
// CHECK:         [[SEQ:%.*]] = load_borrow [[ACCESS]]
// CHECK-NOT:     {{copy_value|load \[copy\]|copy_addr}}
// CHECK:         [[MAKE_ITER:%.*]] = function_ref @$s4test25EscapableIteratorSequenceV013makeBorrowingC0AC0fC0VyF
// CHECK-NEXT:    apply [[MAKE_ITER]]([[SEQ]])
// CHECK:         begin_access [modify] [unknown] [[ADDR]]
// CHECK:         end_access [[ACCESS]]
// CHECK:       } // end sil function '$s4test43reassignEscapableIteratorSequenceDuringLoopyyF'
func reassignEscapableIteratorSequenceDuringLoop() {
  var sequence = EscapableIteratorSequence()
  for _ in sequence {
    sequence = EscapableIteratorSequence()
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s4test41reassignForceUnwrappedEscapableDuringLoopyyF :
// CHECK:         [[BOX:%.*]] = alloc_box ${ var Optional<EscapableIteratorSequence> }, var, name "sequence"
// CHECK:         [[LIFETIME:%.*]] = begin_borrow [lexical] [var_decl] [[BOX]]
// CHECK:         [[ADDR:%.*]] = project_box [[LIFETIME]], 0
// CHECK:         [[ACCESS:%.*]] = begin_access [read] [unknown] [[ADDR]]
// CHECK-NOT:     {{copy_value|load \[copy\]|copy_addr|end_access}}
// CHECK:         debug_value {{.*}}, let, name "$sequence"
// CHECK:         begin_access [modify] [unknown] [[ADDR]]
// CHECK:         end_access [[ACCESS]]
// CHECK:       } // end sil function '$s4test41reassignForceUnwrappedEscapableDuringLoopyyF'
func reassignForceUnwrappedEscapableDuringLoop() {
  var sequence: EscapableIteratorSequence? = EscapableIteratorSequence()
  for _ in sequence! {
    sequence = EscapableIteratorSequence()
  }
}

func makeEscapableIteratorSequence() -> EscapableIteratorSequence {
  EscapableIteratorSequence()
}

// CHECK-LABEL: sil hidden [ossa] @$s4test30fromEscapableIteratorTemporaryyyF :
// CHECK:         [[MAKE:%.*]] = function_ref @$s4test29makeEscapableIteratorSequenceAA0cdE0VyF
// CHECK:         [[SEQ:%.*]] = apply [[MAKE]]()
// CHECK-NOT:     {{copy_value|load \[copy\]|copy_addr}}
// CHECK:         [[BORROW:%.*]] = begin_borrow [var_decl] [[SEQ]]
// CHECK:         debug_value [[BORROW]], let, name "$sequence"
// CHECK:         [[MAKE_ITER:%.*]] = function_ref @$s4test25EscapableIteratorSequenceV013makeBorrowingC0AC0fC0VyF
// CHECK-NEXT:    apply [[MAKE_ITER]]([[BORROW]])
// CHECK:         end_borrow [[BORROW]]
// CHECK:         destroy_value [[SEQ]]
// CHECK:       } // end sil function '$s4test30fromEscapableIteratorTemporaryyyF'
func fromEscapableIteratorTemporary() {
  for _ in makeEscapableIteratorSequence() {}
}

// A trivial temporary has no ownership, so there is nothing to borrow.

struct Repeating: Iterable {
  struct BorrowingIterator: ~Escapable, BorrowingIteratorProtocol {
    @_lifetime(&self)
    mutating func nextSpan(maxCount: Int) throws(Never) -> Span<Int> { Span() }
  }

  var value: Int

  @_lifetime(borrow self)
  func makeBorrowingIterator() -> BorrowingIterator { BorrowingIterator() }
}

func makeRepeating() -> Repeating { Repeating(value: 1) }

var computedRepeating: Repeating { Repeating(value: 1) }

// CHECK-LABEL: sil hidden [ossa] @$s4test20fromTrivialTemporaryyyF :
// CHECK:         [[MAKE:%.*]] = function_ref @$s4test13makeRepeatingAA0C0VyF
// CHECK:         [[SEQ:%.*]] = apply [[MAKE]]()
// CHECK-NOT:     begin_borrow {{.*}}[[SEQ]]
// CHECK:         debug_value [[SEQ]], let, name "$sequence"
// CHECK:         [[MAKE_ITER:%.*]] = function_ref @$s4test9RepeatingV21makeBorrowingIteratorAC0dE0VyF
// CHECK-NEXT:    apply [[MAKE_ITER]]([[SEQ]])
// CHECK:       } // end sil function '$s4test20fromTrivialTemporaryyyF'
func fromTrivialTemporary() {
  for _ in makeRepeating() {}
}

// CHECK-LABEL: sil hidden [ossa] @$s4test27fromTrivialComputedPropertyyyF :
// CHECK:         [[MAKE:%.*]] = function_ref @$s4test17computedRepeatingAA0C0Vvg
// CHECK:         [[SEQ:%.*]] = apply [[MAKE]]()
// CHECK-NOT:     begin_borrow {{.*}}[[SEQ]]
// CHECK:         debug_value [[SEQ]], let, name "$sequence"
// CHECK:         [[MAKE_ITER:%.*]] = function_ref @$s4test9RepeatingV21makeBorrowingIteratorAC0dE0VyF
// CHECK-NEXT:    apply [[MAKE_ITER]]([[SEQ]])
// CHECK:       } // end sil function '$s4test27fromTrivialComputedPropertyyyF'
func fromTrivialComputedProperty() {
  for _ in computedRepeating {}
}

// CHECK-LABEL: sil hidden [ossa] @$s4test25reassignTrivialDuringLoopyyF :
// CHECK:         [[BOX:%.*]] = alloc_box ${ var Repeating }, var, name "repeating"
// CHECK:         [[LIFETIME:%.*]] = begin_borrow [var_decl] [[BOX]]
// CHECK:         [[ADDR:%.*]] = project_box [[LIFETIME]], 0
// CHECK:         [[ACCESS:%.*]] = begin_access [read] [unknown] [[ADDR]]
// CHECK-NEXT:    debug_value [[ACCESS]], let, name "$sequence", expr op_deref
// CHECK-NOT:     {{copy_value|load \[copy\]|copy_addr}}
// CHECK:         [[SEQ:%.*]] = load [trivial] [[ACCESS]]
// CHECK-NOT:     {{copy_value|load \[copy\]|copy_addr}}
// CHECK:         [[MAKE_ITER:%.*]] = function_ref @$s4test9RepeatingV21makeBorrowingIteratorAC0dE0VyF
// CHECK-NEXT:    apply [[MAKE_ITER]]([[SEQ]])
// CHECK:         begin_access [modify] [unknown] [[ADDR]]
// CHECK:         end_access [[ACCESS]]
// CHECK:       } // end sil function '$s4test25reassignTrivialDuringLoopyyF'
func reassignTrivialDuringLoop() {
  var repeating = Repeating(value: 1)
  for _ in repeating {
    repeating = Repeating(value: 2)
  }
}

// A trivial sequence stored in a property is borrowed in place too, just like
// the non-trivial `holder.span` in `reassignStoredPropertyDuringLoop`.

struct RepeatingHolder {
  var iterable = Repeating(value: 1)
}

// CHECK-LABEL: sil hidden [ossa] @$s4test39reassignTrivialStoredPropertyDuringLoopyyF :
// CHECK:         [[BOX:%.*]] = alloc_box ${ var RepeatingHolder }, var, name "holder"
// CHECK:         [[LIFETIME:%.*]] = begin_borrow {{.*}}[var_decl] [[BOX]]
// CHECK:         [[ADDR:%.*]] = project_box [[LIFETIME]], 0
// CHECK:         [[ACCESS:%.*]] = begin_access [read] [unknown] [[ADDR]]
// CHECK-NEXT:    [[FIELD:%.*]] = struct_element_addr [[ACCESS]], #RepeatingHolder.iterable
// CHECK-NEXT:    debug_value [[FIELD]], let, name "$sequence", expr op_deref
// CHECK-NOT:     copy
// CHECK:         [[SEQ:%.*]] = load [trivial] [[FIELD]]
// CHECK-NOT:     copy
// CHECK:         [[MAKE_ITER:%.*]] = function_ref @$s4test9RepeatingV21makeBorrowingIteratorAC0dE0VyF
// CHECK-NEXT:    apply [[MAKE_ITER]]([[SEQ]])
// CHECK:         [[MODIFY:%.*]] = begin_access [modify] [unknown] [[ADDR]]
// CHECK-NEXT:    struct_element_addr [[MODIFY]], #RepeatingHolder.iterable
// CHECK:         end_access [[ACCESS]]
// CHECK:       } // end sil function '$s4test39reassignTrivialStoredPropertyDuringLoopyyF'
func reassignTrivialStoredPropertyDuringLoop() {
  var holder = RepeatingHolder()
  for _ in holder.iterable {
    holder.iterable = Repeating(value: 2)
  }
}

