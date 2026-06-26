// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types \
// RUN:     -enable-experimental-feature BorrowingForLoop \
// RUN:     -enable-experimental-feature BorrowingSequence \
// RUN:     -enable-experimental-feature Lifetimes \
// RUN:     -module-name test \
// RUN:     %s | %FileCheck %s

// REQUIRES: swift_feature_BorrowingForLoop
// REQUIRES: swift_feature_BorrowingSequence
// REQUIRES: swift_feature_Lifetimes

enum IterationError: Error {
  case exhausted
}

@available(SwiftStdlib 6.4, *)
struct ThrowingSpanIterator: BorrowingIteratorProtocol, ~Copyable, ~Escapable {
  typealias Element = Int
  typealias Failure = IterationError

  var _inner: Span<Int>.BorrowingIterator

  @_lifetime(&self)
  @_lifetime(self: copy self)
  mutating func nextSpan(maxCount: Int) throws(IterationError) -> Span<Int> {
    _inner.nextSpan(maxCount: maxCount)
  }
}

@available(SwiftStdlib 6.4, *)
struct ThrowingBorrowingSeq: Iterable, ~Copyable, ~Escapable {
  typealias Element = Int
  typealias Failure = IterationError
  typealias BorrowingIterator = ThrowingSpanIterator

  var _span: Span<Int>

  @_lifetime(borrow self)
  func makeBorrowingIterator() -> ThrowingSpanIterator {
    ThrowingSpanIterator(_inner: .init(_span))
  }
}

// CHECK-LABEL: sil hidden {{.*}}@$s4test{{.*}}ThrowingBorrowingLoop{{.*}} : $@convention(thin) (@guaranteed ThrowingBorrowingSeq) -> @error IterationError {
@available(SwiftStdlib 6.4, *)
func testThrowingBorrowingLoop(seq: borrowing ThrowingBorrowingSeq) throws(IterationError) {
  // CHECK: [[REF:%.*]] = function_ref @$s4test20ThrowingBorrowingSeqV04makeC8IteratorAA0b4SpanF0VyF
  // CHECK: apply [[REF]]

  // The nextSpan call should be a try_apply since it can throw
  // CHECK: [[NEXT_SPAN:%.*]] = function_ref @$s4test20ThrowingSpanIteratorV04nextC08maxCounts0C0VySiGSi_tAA14IterationErrorOYKF
  // CHECK: try_apply [[NEXT_SPAN]]{{.*}}, normal [[NORMAL_BB:bb.*]], error [[ERROR_BB:bb.*]] //

  // Normal path continues with isEmpty check
  // CHECK: [[NORMAL_BB]]({{.*}}):
  // CHECK: function_ref @$ss4SpanVsRi_zrlE7isEmptySbvg

  // Error path receives the typed IterationError
  // CHECK: [[ERROR_BB]]({{.*}} : $IterationError):
  for try element in seq {
    _ = element
  }
}

// CHECK-LABEL: sil hidden {{.*}}@$s4test{{.*}}DoCatch{{.*}} : $@convention(thin) (@guaranteed ThrowingBorrowingSeq) -> () {
@available(SwiftStdlib 6.4, *)
func testThrowingInDoCatch(seq: borrowing ThrowingBorrowingSeq) {
  // CHECK: [[NEXT_SPAN:%.*]] = function_ref @$s4test20ThrowingSpanIteratorV04nextC08maxCounts0C0VySiGSi_tAA14IterationErrorOYKF
  // CHECK: try_apply [[NEXT_SPAN]]{{.*}}, normal [[NORMAL_BB:bb.*]], error [[ERROR_BB:bb.*]] //
  // CHECK: [[NORMAL_BB]]({{.*}}):
  // CHECK: function_ref @$ss4SpanVsRi_zrlE7isEmptySbvg
  // CHECK: [[ERROR_BB]]({{.*}} : $IterationError):
  do {
    for try element in seq {
      _ = element
    }
  } catch {
    _ = error
  }
}

// CHECK-LABEL: sil hidden {{.*}}@$s4test{{.*}}NonThrowing{{.*}} : $@convention(thin) (@guaranteed Span<Int>) -> () {
@available(SwiftStdlib 6.4, *)
func testNonThrowingLoop(seq: borrowing Span<Int>) {
  // SpanIterator.nextSpan with Failure == Never uses apply
  // CHECK: [[NEXT_SPAN:%.*]] = function_ref @$ss4SpanVsRi_zrlE17BorrowingIteratorV04nextA08maxCountAByxGSi_tF
  // CHECK-NOT: try_apply
  // CHECK: apply [[NEXT_SPAN]]
  for element in seq {
    _ = element
  }
}

// CHECK-LABEL: sil hidden {{.*}}@$s4test{{.*}}TypedError{{.*}} : $@convention(thin) (@guaranteed ThrowingBorrowingSeq) -> @error IterationError {
@available(SwiftStdlib 6.4, *)
func testTypedErrorPropagation(seq: borrowing ThrowingBorrowingSeq) throws(IterationError) {
  // CHECK: [[NEXT_SPAN:%.*]] = function_ref @$s4test20ThrowingSpanIteratorV04nextC08maxCounts0C0VySiGSi_tAA14IterationErrorOYKF
  // CHECK: try_apply [[NEXT_SPAN]]{{.*}}, normal [[NORMAL_BB:bb.*]], error [[ERROR_BB:bb.*]] //
  // CHECK: [[NORMAL_BB]]({{.*}}):
  // CHECK: function_ref @$ss4SpanVsRi_zrlE7isEmptySbvg
  // CHECK: [[ERROR_BB]]({{.*}} : $IterationError):
  for try element in seq {
    _ = element
  }
}

// CHECK-LABEL: sil hidden {{.*}}@$s4test{{.*}}GenericThrowingLoop{{.*}} : $@convention(thin) <S where S : Iterable, S.Element == Int, S.Failure == IterationError>{{.*}} -> @error IterationError {
@available(SwiftStdlib 6.4, *)
func testGenericThrowingLoop<S: Iterable>(
  seq: borrowing S
) throws(S.Failure) where S.Element == Int, S.Failure == IterationError {
  // CHECK: [[NEXT_SPAN:%.*]] = witness_method $S.BorrowingIterator, #BorrowingIteratorProtocol.nextSpan
  // CHECK: [[ERROR:%.*]] = alloc_stack $IterationError
  // CHECK: try_apply [[NEXT_SPAN]]{{.*}}, normal [[NORMAL_BB:bb.*]], error [[ERROR_BB:bb.*]] //
  // CHECK: [[NORMAL_BB]]({{.*}}):
  // CHECK: function_ref @$ss4SpanVsRi_zrlE7isEmptySbvg
  // CHECK: [[ERROR_BB]]:
  // CHECK: load{{.*}} [[ERROR]] : $*IterationError
  for try element in seq {
    _ = element
  }
}

// CHECK-LABEL: sil hidden {{.*}}@$s4test{{.*}}GenericDoCatch{{.*}} : $@convention(thin) <S where S : Iterable, S.Element == Int, S.Failure == IterationError>{{.*}} -> () {
@available(SwiftStdlib 6.4, *)
func testGenericDoCatch<S: Iterable>(
  seq: borrowing S
) where S.Element == Int, S.Failure == IterationError {
  // CHECK: [[NEXT_SPAN:%.*]] = witness_method $S.BorrowingIterator, #BorrowingIteratorProtocol.nextSpan
  // CHECK: [[ERROR:%.*]] = alloc_stack $IterationError
  // CHECK: try_apply [[NEXT_SPAN]]{{.*}}, normal [[NORMAL_BB:bb.*]], error [[ERROR_BB:bb.*]] //
  // CHECK: [[NORMAL_BB]]({{.*}}):
  // CHECK: function_ref @$ss4SpanVsRi_zrlE7isEmptySbvg
  // CHECK: [[ERROR_BB]]:
  // CHECK: load{{.*}} [[ERROR]] : $*IterationError
  do {
    for try element in seq {
      _ = element
    }
  } catch {
    _ = error
  }
}

// CHECK-LABEL: sil hidden {{.*}}@$s4test{{.*}}GenericNeverFailure{{.*}} : $@convention(thin) <S where S : Iterable, S.Element == Int, S.Failure == Never>{{.*}} -> () {
@available(SwiftStdlib 6.4, *)
func testGenericNeverFailure<S: Iterable>(
  seq: borrowing S
) where S.Element == Int, S.Failure == Never {
  // When Failure == Never, no try_apply should be needed
  // CHECK: [[NEXT_SPAN:%.*]] = witness_method $S.BorrowingIterator, #BorrowingIteratorProtocol.nextSpan
  // CHECK-NOT: try_apply
  // CHECK: apply [[NEXT_SPAN]]
  for element in seq {
    _ = element
  }
}
