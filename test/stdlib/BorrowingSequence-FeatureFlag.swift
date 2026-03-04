// RUN: %empty-directory(%t)
// RUN: not %target-swift-emit-ir %s -module-name main -parse-as-library 2>&1 | %FileCheck %s
// RUN: %target-swift-emit-ir %s -module-name main -parse-as-library -enable-experimental-feature BorrowingSequence

// REQUIRES: swift_feature_BorrowingSequence

@available(SwiftStdlib 6.4, *)
extension BorrowingSequence {}

@available(SwiftStdlib 6.4, *)
extension BorrowingIteratorProtocol {}

@available(SwiftStdlib 6.4, *)
extension SpanIterator {}

@available(SwiftStdlib 6.4, *)
extension BorrowingIteratorAdapter {}

// CHECK: 'BorrowingSequence' requires -enable-experimental-feature BorrowingSequence
// CHECK: 'BorrowingIteratorProtocol' requires -enable-experimental-feature BorrowingSequence
// CHECK: 'SpanIterator' requires -enable-experimental-feature BorrowingSequence
// CHECK: 'BorrowingIteratorAdapter' requires -enable-experimental-feature BorrowingSequence
