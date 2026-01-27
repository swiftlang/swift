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

// CHECK-LABEL: sil hidden [ossa] @$s18borrowing_for_loop21testBorrowingSequenceyyF : $@convention(thin) () -> ()
func testBorrowingSequence() {
  let seq = UnsafeMutableBufferPointer<NoncopyableInt>.allocate(capacity: 4)
  // This loop iterates over a sequence of non copyable elements (Int).
  // It will resort to using the Sequence and Iterator protocols even if
  // borrowing is enabled.
  // CHECK: bb0:
  // CHECK: function_ref @$sSlss16IndexingIteratorVyxG0B0RtzrlE04makeB0ACyF : $@convention(method) <τ_0_0 where τ_0_0 : Collection, τ_0_0.Iterator == IndexingIterator<τ_0_0>> (@in τ_0_0) -> @out IndexingIterator<τ_0_0>
  // CHECK: bb1:
  // CHECK:  function_ref @$ss16IndexingIteratorV4next7ElementQzSgyF : $@convention(method) <τ_0_0 where τ_0_0 : Collection> (@inout IndexingIterator<τ_0_0>) -> @out Optional<τ_0_0.Element>
  for i in 0..<4 {
    seq.initializeElement(at: i, to: NoncopyableInt(value: i))
  }

  // With borrowing feature enabled, we expect makeBorrowingIterator and nextSpan to be called
  // CHECK: = function_ref @$sSrsRi_zrlE21makeBorrowingIterators4SpanVyxGyF : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (UnsafeMutableBufferPointer<τ_0_0>) -> @lifetime(borrow 0) @owned Span<τ_0_0>
  // CHECK: = function_ref @$ss4SpanVsRi_zrlE04nextA012maximumCountAByxGSi_tF : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (Int, @lifetime(copy 1) @inout Span<τ_0_0>) -> @lifetime(borrow 1) @owned Span<τ_0_0>
  // CHECK: [[IS_EMPTY_CHECK:%.*]] = function_ref @$ss4SpanVsRi_zrlE7isEmptySbvg : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (@guaranteed Span<τ_0_0>) -> Bool
  // CHECK: [[IS_EMPTY_CALL:%.*]] = apply [[IS_EMPTY_CHECK]]
  // CHECK: [[NOT_EMPTY:%.*]] = function_ref @$sSb1nopyS2bFZ : $@convention(method) (Bool, @thin Bool.Type) -> Bool
  // CHECK: [[NOT_EMPTY_CALL:%.*]] = apply [[NOT_EMPTY]]([[IS_EMPTY_CALL]]
  // CHECK: [[NOT_EMPTY_RES:%.*]] = struct_extract [[NOT_EMPTY_CALL]] : $Bool, #Bool._value
  // CHECK: cond_br [[NOT_EMPTY_RES]]
  // CHECK: function_ref @$sSlss16IndexingIteratorVyxG0B0RtzrlE04makeB0ACyF : $@convention(method) <τ_0_0 where τ_0_0 : Collection, τ_0_0.Iterator == IndexingIterator<τ_0_0>> (@in τ_0_0) -> @out IndexingIterator<τ_0_0>
  // CHECK: [[INNER_LOOP_NEXT_FN:%.*]] = function_ref @$ss16IndexingIteratorV4next7ElementQzSgyF : $@convention(method) <τ_0_0 where τ_0_0 : Collection> (@inout IndexingIterator<τ_0_0>) -> @out Optional<τ_0_0.Element>
  // CHECK: = apply [[INNER_LOOP_NEXT_FN]]
  // TODO: add tests for accessing span elements

  for element in seq {
    print(element.value)
  }
}

// CHECK: sil hidden [ossa] @$s18borrowing_for_loop35testContinueTargetBorrowingSequenceyyF : $@convention(thin) () -> ()
func testContinueTargetBorrowingSequence() {
  let seq = UnsafeMutableBufferPointer<NoncopyableInt>.allocate(capacity: 4)
  for i in 0..<4 {
    seq.initializeElement(at: i, to: NoncopyableInt(value: i))
  }

  // CHECK: [[OUTER_LOOP_HEADER:bb4]]:
  // CHECK: cond_br {{.*}}, [[OUTER_LOOP_BODY:bb5]], [[EXIT_OUTER_LOOP:bb11]]
  // CHECK: [[OUTER_LOOP_BODY]]:
  // CHECK: br [[INNER_LOOP_HEADER:bb6]]
  // CHECK: [[INNER_LOOP_HEADER]]:
  // CHECK: switch_enum {{.*}}: [[INNER_LOOP_BODY:%.*]]
  // Continue condition
  // CHECK: cond_br {{.*}}, [[CONTINUE_BB:bb8]], [[PRINT_BB:bb9]]
  // CHECK: [[CONTINUE_BB]]:
  // CHECK: br [[INNER_LOOP_HEADER]]
  // CHECK: [[PRINT_BB]]:
  // Make sure that we are printing on this branch.
  // CHECK: function_ref @$ss5print_9separator10terminatoryypd_S2StFfA0_ : $@convention(thin) () -> @owned String
  // CHECK: br [[INNER_LOOP_HEADER]]

  for element in seq {
    if (element.value == 2){
        continue
    }
    print(element.value)
  }
}

// CHECK: sil hidden [ossa] @$s18borrowing_for_loop32testBreakTargetBorrowingSequenceyyF : $@convention(thin) () -> ()
func testBreakTargetBorrowingSequence() {
  let seq = UnsafeMutableBufferPointer<NoncopyableInt>.allocate(capacity: 4)
  for i in 0..<4 {
    seq.initializeElement(at: i, to: NoncopyableInt(value: i))
  }

  // CHECK: [[OUTER_LOOP_HEADER:bb1]]:
  // CHECK: cond_br {{.*}}, [[OUTER_LOOP_BODY:bb5]]
  // CHECK: [[OUTER_LOOP_BODY]]:
  // CHECK: br [[INNER_LOOP_HEADER:bb6]]
  // CHECK: [[INNER_LOOP_HEADER]]:
  // CHECK: switch_enum {{.*}}: [[INNER_LOOP_BODY:%.*]]
  // Continue condition
  // CHECK: cond_br {{.*}}, [[BREAK_BB:bb8]], [[LOOP_BB:bb9]]
  // CHECK: [[BREAK_BB]]:
  // CHECK: br [[EXIT_OUTER_LOOP:bb12]]
  // CHECK: [[LOOP_BB]]:
  // CHECK: br [[INNER_LOOP_HEADER]]
  // CHECK: [[EXIT_OUTER_LOOP]]:
  // CHECK: return
  // CHECK: } // end sil function '$s18borrowing_for_loop32testBreakTargetBorrowingSequenceyyF'

  for element in seq {
    if (element.value == 2){
        break
    }
  }
}
