// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types \
// RUN:     -g -Xllvm -sil-print-debuginfo-verbose \
// RUN:     -enable-experimental-feature BorrowingForLoop \
// RUN:     -enable-experimental-feature BorrowingSequence \
// RUN:     %s | %FileCheck %s

// REQUIRES: swift_feature_BorrowingForLoop
// REQUIRES: swift_feature_BorrowingSequence

struct NoncopyableInt: ~Copyable {
  var value: Int
}

extension NoncopyableInt: Equatable {
  static func ==(lhs: borrowing Self, rhs: borrowing Self) -> Bool {
    lhs.value == rhs.value
  }
}

// CHECK-LABEL: sil hidden {{.*}}[ossa] @$s17foreach_borrowing32testNonCopyableBorrowingSequence3seqys4SpanVyAA14NoncopyableIntVG_tF : $@convention(thin) (@guaranteed Span<NoncopyableInt>) -> () {
@available(SwiftStdlib 6.4, *)
func testNonCopyableBorrowingSequence(seq: borrowing Span<NoncopyableInt>) {
  // With borrowing feature enabled, we expect makeBorrowingIterator and nextSpan to be called
  // CHECK: = function_ref @$ss4SpanVsRi_zrlE21makeBorrowingIterators0aD0VyxGyF : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (@guaranteed Span<τ_0_0>) -> @lifetime(borrow 0) @out SpanIterator<τ_0_0>
  // CHECK: = function_ref @$ss12SpanIteratorVsRi_zrlE04nextA012maximumCounts0A0VyxGSi_tF : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (Int, @lifetime(copy 1) @inout SpanIterator<τ_0_0>) -> @lifetime(borrow address_for_deps 1) @owned Span<τ_0_0>
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


// CHECK-LABEL: sil hidden {{.*}}[ossa] @$s17foreach_borrowing29testCopyableBorrowingSequence3seqys4SpanVySiG_tF : $@convention(thin) (@guaranteed Span<Int>) -> () {
@available(SwiftStdlib 6.4, *)
func testCopyableBorrowingSequence(seq: borrowing Span<Int>) {
  // With borrowing feature enabled, we expect makeBorrowingIterator and nextSpan to be called
  // CHECK: = function_ref @$ss4SpanVsRi_zrlE21makeBorrowingIterators0aD0VyxGyF : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (@guaranteed Span<τ_0_0>) -> @lifetime(borrow 0) @out SpanIterator<τ_0_0>
  // CHECK: = function_ref @$ss12SpanIteratorVsRi_zrlE04nextA012maximumCounts0A0VyxGSi_tF : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (Int, @lifetime(copy 1) @inout SpanIterator<τ_0_0>) -> @lifetime(borrow address_for_deps 1) @owned Span<τ_0_0>
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

// CHECK-LABEL: sil hidden {{.*}}[ossa] @$s17foreach_borrowing35testContinueTargetBorrowingSequenceyyF : $@convention(thin) () -> () {
@available(SwiftStdlib 6.4, *)
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
  // CHECK: cond_br {{.*}}, [[CONTINUE_BB:bb5]], [[PRINT_BB:bb6]], {{.*}} isImplicit: false
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

// CHECK-LABEL: sil hidden {{.*}}[ossa] @$s17foreach_borrowing32testBreakTargetBorrowingSequenceyyF : $@convention(thin) () -> () {
@available(SwiftStdlib 6.4, *)
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


// CHECK-LABEL: sil hidden {{.*}}[ossa] @$s17foreach_borrowing20testForEachLocations3seq3valys4SpanVySiG_SitF : $@convention(thin) (@guaranteed Span<Int>, Int) -> () {
@available(SwiftStdlib 6.4, *)
func testForEachLocations(seq: borrowing Span<Int>, val: Int) {
  // Test that synthesized code has correct source locations for the borrowing foreach loop.
  // The borrowing foreach desugars to:
  //   let $x$generator = seq.makeBorrowingIterator()
  //   while case let $span = $x$generator.nextSpan(maximumCount: Int.max),
  //      !$span.isEmpty {
  //       let $i = 0
  //       let $count = $span.count
  //       while $i < $count, case let x = $span[$i] {
  //         $i = $i + 1
  //         if x == val {
  //           // body
  //         }
  //       }
  //     }
  //   }

  // makeBorrowingIterator() function_ref should be at "for" keyword location (172:3)
  // CHECK: [[MAKE_BORROWING_IT:%.*]] = function_ref @$ss4SpanVsRi_zrlE21makeBorrowingIterators0aD0VyxGyF {{.*}}, loc "{{.*}}":[[@LINE+31]]:3
  // CHECK: apply [[MAKE_BORROWING_IT]]{{.*}}, loc "{{.*}}":[[@LINE+30]]:18

  // nextSpan() function_ref should be at "for" keyword location
  // CHECK: [[NEXT_SPAN:%.*]] = function_ref @$ss12SpanIteratorVsRi_zrlE04nextA012maximumCounts0A0VyxGSi_tF {{.*}}, loc "{{.*}}":[[@LINE+27]]:3
  // CHECK: apply [[NEXT_SPAN]]{{.*}}, loc "{{.*}}":[[@LINE+26]]:3

  // $span debug_value should be at "for" keyword location
  // CHECK: debug_value {{.*}}, let, name "$span", loc "{{.*}}":[[@LINE+23]]:3

  // Outer loop condition (!$span.isEmpty)
  // CHECK: [[NOT_IS_EMPTY:%.*]] = function_ref @$sSb1nopyS2bFZ {{.*}}, loc "{{.*}}":[[@LINE+20]]:3
  // CHECK: apply [[NOT_IS_EMPTY]]{{.*}}, loc "{{.*}}":[[@LINE+19]]:3
  // CHECK: cond_br {{.*}}, loc "{{.*}}":[[@LINE+18]]:3

  // Inner loop condition ($i < $count)
  // CHECK: function_ref @$sSi1loiySbSi_SitFZ  {{.*}}, loc "{{.*}}":[[@LINE+15]]:3
  // CHECK: cond_br {{.*}}, loc "{{.*}}":[[@LINE+14]]:3

  // Pattern variable "x" should be at its declaration location
  // CHECK: debug_value {{.*}}, let, name "element", loc "{{.*}}":[[@LINE+11]]:7

  // Where clause "==" operator function_ref
  // CHECK: [[EQUALS:%.*]] = function_ref @$sSi2eeoiySbSi_SitFZ {{.*}}, loc "{{.*}}":[[@LINE+8]]:36
  // Where clause "==" operator apply
  // CHECK: apply [[EQUALS]]{{.*}}, loc "{{.*}}":[[@LINE+6]]:36
  // Where clause condition extraction
  // CHECK: struct_extract {{.*}} : $Bool, #Bool._value, loc "{{.*}}":[[@LINE+4]]:28
  // Where clause conditional branch
  // CHECK: cond_br {{.*}}, loc "{{.*}}":[[@LINE+2]]:28

  for element in seq where element == val{
  }
}

// CHECK-LABEL: sil hidden {{.*}}[ossa] @$s17foreach_borrowing34testForEachNonCopyableSILDebugInfo3seqys4SpanVyAA14NoncopyableIntVG_tF : $@convention(thin) (@guaranteed Span<NoncopyableInt>) -> () {
@available(SwiftStdlib 6.4, *)
func testForEachNonCopyableSILDebugInfo(seq: borrowing Span<NoncopyableInt>){
  // CHECK: debug_value {{.*}} : $*NoncopyableInt, let, name "element", expr op_deref, loc "{{.*}}":[[@LINE+1]]:7 isImplicit: false
  for element in seq {
      if (element.value == 0){
          continue
      }
  }
}

// A struct conforming to both Sequence and BorrowingSequence. The default
// makeBorrowingIterator() is provided by the Sequence where Self: BorrowingSequence
// extension in BorrowingSequence.swift.
@available(SwiftStdlib 6.4, *)
struct DualSeq: Sequence, BorrowingSequence {
  typealias Element = Int
  typealias BorrowingIterator = BorrowingIteratorAdapter<IndexingIterator<[Int]>>

  func makeIterator() -> IndexingIterator<[Int]> {
    return [1, 2, 3].makeIterator()
  }
}

// CHECK-LABEL: sil hidden {{.*}}[ossa] @$s17foreach_borrowing34testSequencePreferredOverBorrowing3seqyAA7DualSeqV_tF : $@convention(thin) (DualSeq) -> () {
@available(SwiftStdlib 6.4, *)
func testSequencePreferredOverBorrowing(seq: borrowing DualSeq) {
  // DualSeq conforms to Sequence, so shouldUseBorrowingSequence returns false.
  // CHECK: = function_ref @$s17foreach_borrowing7DualSeqV12makeIterators08IndexingF0VySaySiGGyF : $@convention(method) (DualSeq) -> @owned IndexingIterator<Array<Int>>
  // CHECK: } // end sil function '$s17foreach_borrowing34testSequencePreferredOverBorrowing3seqyAA7DualSeqV_tF'
  for element in seq {
    print(element)
  }
}

// A type that unconditionally conforms to Sequence but whose BorrowingSequence
// conformance is only available under SwiftStdlib 6.4. When a for-in loop over
// this type appears in a context without that availability,
// shouldUseBorrowingSequence returns false at the Sequence check, and the loop
// desugars via the Sequence path (makeIterator), not the BorrowingSequence path.
struct BorrowingFallbackWithSequence: Sequence {
  typealias Element = Int
  func makeIterator() -> IndexingIterator<[Int]> {
    return [1, 2, 3].makeIterator()
  }
}

@available(SwiftStdlib 6.4, *)
extension BorrowingFallbackWithSequence: BorrowingSequence {
  typealias BorrowingIterator = BorrowingIteratorAdapter<IndexingIterator<[Int]>>
}

// CHECK-LABEL: sil hidden {{.*}}[ossa] @$s17foreach_borrowing36testUnavailableBorrowingWithSequence3seqyAA0e8FallbackfG0V_tF : $@convention(thin) (BorrowingFallbackWithSequence) -> () {
func testUnavailableBorrowingWithSequence(seq: BorrowingFallbackWithSequence) {
  // BorrowingFallbackWithSequence conforms to Sequence, so
  // shouldUseBorrowingSequence returns false and makeIterator() is called.
  // CHECK: = function_ref @$s17foreach_borrowing29BorrowingFallbackWithSequenceV12makeIterators08IndexingH0VySaySiGGyF : $@convention(method) (BorrowingFallbackWithSequence) -> @owned IndexingIterator<Array<Int>>
  // CHECK: } // end sil function '$s17foreach_borrowing36testUnavailableBorrowingWithSequence3seqyAA0e8FallbackfG0V_tF'
  for element in seq {
    _ = element
  }
}

