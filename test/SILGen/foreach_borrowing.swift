// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types \
// RUN:     -enable-experimental-feature BorrowingForLoop \
// RUN:     -disable-availability-checking \
// RUN:     %s | %FileCheck %s

// REQUIRES: swift_feature_BorrowingForLoop

struct NoncopyableInt: ~Copyable {
  var value: Int
}

extension NoncopyableInt: Equatable {
  static func ==(lhs: borrowing Self, rhs: borrowing Self) -> Bool {
    lhs.value == rhs.value
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s17foreach_borrowing32testNonCopyableBorrowingSequence3seqys4SpanVyAA14NoncopyableIntVG_tF : $@convention(thin) (@guaranteed Span<NoncopyableInt>) -> () {
func testNonCopyableBorrowingSequence(seq: borrowing Span<NoncopyableInt>) {
  // With borrowing feature enabled, we expect makeBorrowingIterator and nextSpan to be called
  // CHECK: = function_ref @$ss4SpanVsRi_zrlE21makeBorrowingIteratorAByxGyF : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (@guaranteed Span<τ_0_0>) -> @lifetime(borrow 0) @owned Span<τ_0_0>
  // CHECK: = function_ref @$ss4SpanVsRi_zrlE04nextA012maximumCountAByxGSi_tF : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (Int, @lifetime(copy 1) @inout Span<τ_0_0>) -> @lifetime(borrow 1) @owned Span<τ_0_0>
  // CHECK: [[IS_EMPTY_CHECK:%.*]] = function_ref @$ss4SpanVsRi_zrlE7isEmptySbvg : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (@guaranteed Span<τ_0_0>) -> Bool
  // CHECK: [[IS_EMPTY_CALL:%.*]] = apply [[IS_EMPTY_CHECK]]
  // CHECK: [[NOT_EMPTY:%.*]] = function_ref @$sSb1nopyS2bFZ : $@convention(method) (Bool, @thin Bool.Type) -> Bool
  // CHECK: [[NOT_EMPTY_CALL:%.*]] = apply [[NOT_EMPTY]]([[IS_EMPTY_CALL]]
  // CHECK: [[NOT_EMPTY_RES:%.*]] = struct_extract [[NOT_EMPTY_CALL]] : $Bool, #Bool._value
  // CHECK: cond_br [[NOT_EMPTY_RES]]
  // Inner While statement condition ($i < $count)
  // CHECK: function_ref @$sSi1loiySbSi_SitFZ : $@convention(method) (Int, Int, @thin Int.Type) -> Bool
  // TODO: add tests for accessing span elements

  for element in seq {
    print(element.value)
  }
}


// CHECK-LABEL: sil hidden [ossa] @$s17foreach_borrowing29testCopyableBorrowingSequence3seqys4SpanVySiG_tF : $@convention(thin) (@guaranteed Span<Int>) -> () {
func testCopyableBorrowingSequence(seq: borrowing Span<Int>) {
  // With borrowing feature enabled, we expect makeBorrowingIterator and nextSpan to be called
  // CHECK: = function_ref @$ss4SpanVsRi_zrlE21makeBorrowingIteratorAByxGyF : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (@guaranteed Span<τ_0_0>) -> @lifetime(borrow 0) @owned Span<τ_0_0>
  // CHECK: = function_ref @$ss4SpanVsRi_zrlE04nextA012maximumCountAByxGSi_tF : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (Int, @lifetime(copy 1) @inout Span<τ_0_0>) -> @lifetime(borrow 1) @owned Span<τ_0_0>
  // CHECK: [[IS_EMPTY_CHECK:%.*]] = function_ref @$ss4SpanVsRi_zrlE7isEmptySbvg : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (@guaranteed Span<τ_0_0>) -> Bool
  // CHECK: [[IS_EMPTY_CALL:%.*]] = apply [[IS_EMPTY_CHECK]]
  // CHECK: [[NOT_EMPTY:%.*]] = function_ref @$sSb1nopyS2bFZ : $@convention(method) (Bool, @thin Bool.Type) -> Bool
  // CHECK: [[NOT_EMPTY_CALL:%.*]] = apply [[NOT_EMPTY]]([[IS_EMPTY_CALL]]
  // CHECK: [[NOT_EMPTY_RES:%.*]] = struct_extract [[NOT_EMPTY_CALL]] : $Bool, #Bool._value
  // CHECK: cond_br [[NOT_EMPTY_RES]]
  // Inner While statement condition ($i < $count)
  // CHECK: function_ref @$sSi1loiySbSi_SitFZ : $@convention(method) (Int, Int, @thin Int.Type) -> Bool
  // TODO: add tests for accessing span elements

  for element in seq {
    print(element)
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s17foreach_borrowing35testContinueTargetBorrowingSequenceyyF : $@convention(thin) () -> () {
func testContinueTargetBorrowingSequence() {
  let arr = [0, 1, 2, 3]
  let seq = arr.span

  // CHECK: [[OUTER_LOOP_HEADER:bb1]]:
  // Is span empty?
  // CHECK: cond_br {{.*}}, [[OUTER_LOOP_BODY:bb2]]
  // CHECK: [[OUTER_LOOP_BODY]]:
  // CHECK: br [[INNER_LOOP_HEADER:bb3]]
  // CHECK: [[INNER_LOOP_HEADER]]:
  // CHECK: cond_br {{.*}}, [[INNER_LOOP_BODY:bb4]]
  // CHECK: [[INNER_LOOP_BODY]]:
  // Continue condition
  // CHECK: cond_br {{.*}}, [[CONTINUE_BB:bb5]], [[PRINT_BB:bb6]]
  // CHECK: [[CONTINUE_BB]]:
  // CHECK: br [[INNER_LOOP_HEADER]]
  // CHECK: [[PRINT_BB]]:
  // Make sure that we are printing on this branch.
  // CHECK: function_ref @$ss5print_9separator10terminatoryypd_S2StFfA0_ : $@convention(thin) () -> @owned String
  // CHECK: br [[INNER_LOOP_HEADER]]

  for element in seq {
    if (element == 2){
        continue
    }
    print(element)
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s17foreach_borrowing32testBreakTargetBorrowingSequenceyyF : $@convention(thin) () -> () {
func testBreakTargetBorrowingSequence() {
  let arr = [0, 1, 2, 3]
  let seq = arr.span

  // CHECK: [[OUTER_LOOP_HEADER:bb1]]:
  // Is span empty?
  // CHECK: cond_br {{.*}}, [[OUTER_LOOP_BODY:bb2]]
  // CHECK: [[OUTER_LOOP_BODY]]:
  // CHECK: br [[INNER_LOOP_HEADER:bb3]]
  // CHECK: [[INNER_LOOP_HEADER]]:
  // CHECK: cond_br {{.*}}, [[INNER_LOOP_BODY:bb4]]
  // CHECK: [[INNER_LOOP_BODY]]:
  // Break condition
  // CHECK: cond_br {{.*}}, [[BREAK_BB:bb5]], [[LOOP_BB:bb6]]
  // CHECK: [[BREAK_BB]]:
  // CHECK: br [[EXIT_OUTER_LOOP:bb9]]
  // CHECK: [[LOOP_BB]]:
  // CHECK: br [[INNER_LOOP_HEADER]]
  // CHECK: [[EXIT_OUTER_LOOP]]:
  // CHECK: return
  // CHECK: } // end sil function '$s17foreach_borrowing32testBreakTargetBorrowingSequenceyyF'

  for element in seq {
    if (element == 2){
        break
    }
  }
}
