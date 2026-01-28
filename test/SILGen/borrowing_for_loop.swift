// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types \
// RUN:     -enable-experimental-feature BorrowingForLoop \
// RUN:     %s | %FileCheck %s --check-prefix=CHECK-BORROWING

// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types \
// RUN:     %s | %FileCheck %s --check-prefix=CHECK-STANDARD

// REQUIRES: swift_feature_BorrowingForLoop

// Protocol definitions
// FIXME: delete and replace with usage of stdlib protocol once available
public protocol BorrowingIteratorProtocol: IteratorProtocol {
  associatedtype Element
  associatedtype Span: Collection where Span.Element == Element

  mutating func nextSpan(maximumCount: Int) -> Span
}

public protocol
BorrowingSequence: Sequence where Iterator: BorrowingIteratorProtocol  {
  associatedtype Element

  __consuming func makeIterator() -> Iterator
}

// Mock implementation for testing
struct MockBorrowingIterator: BorrowingIteratorProtocol {
  typealias Element = Int
  typealias Span = ArraySlice<Int>

  private var data: [Int]
  private var position: Int = 0

  init(data: [Int]) {
    self.data = data
  }

  // This is only here for this to conform to the protocol.
  mutating func next() -> Int? {
    guard position < data.count else {
      return nil
    }
    let element = data[position]
    position += 1
    return element
  }

  mutating func nextSpan(maximumCount: Int) -> ArraySlice<Int> {
    guard position < data.count else {
      return []
    }

    let end = min(position + maximumCount, data.count)
    let result = data[position..<end]
    position = end
    return result
  }
}

struct MockBorrowingSequence: BorrowingSequence {
  typealias Element = Int
  typealias Iterator = MockBorrowingIterator

  private let data: [Int]

  init(_ data: [Int]) {
    self.data = data
  }

  func makeIterator() -> Iterator {
    return Iterator(data: data)
  }
}

// Test that the same borrowing sequence uses different iteration strategies
// based on whether the BorrowingForLoop feature is enabled

// CHECK-BORROWING-LABEL: sil hidden [ossa] @$s18borrowing_for_loop21testBorrowingSequenceyyF : $@convention(thin) () -> ()
// CHECK-STANDARD-LABEL: sil hidden [ossa] @$s18borrowing_for_loop21testBorrowingSequenceyyF : $@convention(thin) () -> ()
func testBorrowingSequence() {
  let seq = MockBorrowingSequence([1, 2, 3, 4, 5])

  // With borrowing feature enabled, we expect nextSpan to be called
  // CHECK-BORROWING: [[NEXT_SPAN_FN:%.*]] = function_ref @$s18borrowing_for_loop21MockBorrowingIteratorV8nextSpan12maximumCounts10ArraySliceVySiGSi_tF : $@convention(method) (Int, @inout MockBorrowingIterator) -> @owned ArraySlice<Int>
  // CHECK-BORROWING: = apply [[NEXT_SPAN_FN]]
  // CHECK-BORROWING: [[IS_EMPTY_CHECK:%.*]] = function_ref @$sSlsE7isEmptySbvg : $@convention(method) <τ_0_0 where τ_0_0 : Collection> (@in_guaranteed τ_0_0) -> Bool
  // CHECK-BORROWING: [[IS_EMPTY_CALL:%.*]] = apply [[IS_EMPTY_CHECK]]
  // CHECK-BORROWING: [[NOT_EMPTY:%.*]] = function_ref @$sSb1nopyS2bFZ : $@convention(method) (Bool, @thin Bool.Type) -> Bool
  // CHECK-BORROWING: [[NOT_EMPTY_CALL:%.*]] = apply [[NOT_EMPTY]]([[IS_EMPTY_CALL]]
  // CHECK-BORROWING: [[NOT_EMPTY_RES:%.*]] = struct_extract [[NOT_EMPTY_CALL]] : $Bool, #Bool._value
  // CHECK-BORROWING: cond_br [[NOT_EMPTY_RES]]
  // CHECK-BORROWING: function_ref @$sSlss16IndexingIteratorVyxG0B0RtzrlE04makeB0ACyF : $@convention(method) <τ_0_0 where τ_0_0 : Collection, τ_0_0.Iterator == IndexingIterator<τ_0_0>> (@in τ_0_0) -> @out IndexingIterator<τ_0_0>
  // CHECK-BORROWING: [[INNER_LOOP_NEXT_FN:%.*]] = function_ref @$ss16IndexingIteratorV4next7ElementQzSgyF : $@convention(method) <τ_0_0 where τ_0_0 : Collection> (@inout IndexingIterator<τ_0_0>) -> @out Optional<τ_0_0.Element>
  // CHECK-BORROWING: = apply [[INNER_LOOP_NEXT_FN]]
  // TODO: add tests for accessing span elements

  // Without borrowing feature, we expect standard next() to be called
  // CHECK-STANDARD: bb1:
  // CHECK-STANDARD: [[NEXT_FN:%.*]] = function_ref @$s18borrowing_for_loop21MockBorrowingIteratorV4nextSiSgyF : $@convention(method) (@inout MockBorrowingIterator) -> Optional<Int>
  // CHECK-STANDARD: = apply [[NEXT_FN]]
  // TODO: add tests for accessing iterator values

  for element in seq {
    print(element)
  }
}

// CHECK-BORROWING: sil hidden [ossa] @$s18borrowing_for_loop35testContinueTargetBorrowingSequenceyyF : $@convention(thin) () -> ()
func testContinueTargetBorrowingSequence() {
  let seq = MockBorrowingSequence([1, 2, 3, 4, 5])

  // CHECK-BORROWING: [[OUTER_LOOP_HEADER:bb1]]:
  // CHECK-BORROWING: cond_br {{.*}}, [[OUTER_LOOP_BODY:bb2]], [[EXIT_OUTER_LOOP:bb8]]
  // CHECK-BORROWING: [[OUTER_LOOP_BODY]]:
  // CHECK-BORROWING: br [[INNER_LOOP_HEADER:bb3]]
  // CHECK-BORROWING: [[INNER_LOOP_HEADER]]:
  // CHECK-BORROWING: switch_enum {{.*}}: [[INNER_LOOP_BODY:%.*]]
  // Continue condition
  // CHECK-BORROWING: cond_br {{.*}}, [[CONTINUE_BB:bb5]], [[PRINT_BB:bb6]]
  // CHECK-BORROWING: [[CONTINUE_BB]]:
  // CHECK-BORROWING: br [[INNER_LOOP_HEADER]]
  // CHECK-BORROWING: [[PRINT_BB]]:
  // Make sure that we are printing on this branch.
  // CHECK-BORROWING: function_ref @$ss5print_9separator10terminatoryypd_S2StFfA0_ : $@convention(thin) () -> @owned String
  // CHECK-BORROWING: br [[INNER_LOOP_HEADER]]

  for element in seq {
    if (element == 2){
        continue
    }
    print(element)
  }
}

// CHECK-BORROWING: sil hidden [ossa] @$s18borrowing_for_loop32testBreakTargetBorrowingSequenceyyF : $@convention(thin) () -> ()
func testBreakTargetBorrowingSequence() {
  let seq = MockBorrowingSequence([1, 2, 3, 4, 5])

  // CHECK-BORROWING: [[OUTER_LOOP_HEADER:bb1]]:
  // CHECK-BORROWING: cond_br {{.*}}, [[OUTER_LOOP_BODY:bb2]]
  // CHECK-BORROWING: [[OUTER_LOOP_BODY]]:
  // CHECK-BORROWING: br [[INNER_LOOP_HEADER:bb3]]
  // CHECK-BORROWING: [[INNER_LOOP_HEADER]]:
  // CHECK-BORROWING: switch_enum {{.*}}: [[INNER_LOOP_BODY:%.*]]
  // Continue condition
  // CHECK-BORROWING: cond_br {{.*}}, [[BREAK_BB:bb5]], [[LOOP_BB:bb6]]
  // CHECK-BORROWING: [[BREAK_BB]]:
  // CHECK-BORROWING: br [[EXIT_OUTER_LOOP:bb9]]
  // CHECK-BORROWING: [[LOOP_BB]]:
  // CHECK-BORROWING: br [[INNER_LOOP_HEADER]]
  // CHECK-BORROWING: [[EXIT_OUTER_LOOP]]:
  // CHECK-BORROWING: return
  // CHECK-BORROWING: } // end sil function '$s18borrowing_for_loop32testBreakTargetBorrowingSequenceyyF'

  for element in seq {
    if (element == 2){
        break
    }
  }
}
