// RUN: %empty-directory(%t)
// RUN: not %target-swift-emit-ir %s -module-name main -parse-as-library 2>&1 | %FileCheck %s
// RUN: %target-swift-emit-ir %s -module-name main -parse-as-library -enable-experimental-feature BorrowingSequence

// REQUIRES: swift_feature_BorrowingSequence

@available(SwiftStdlib 6.4, *)
extension Iterable {}

@available(SwiftStdlib 6.4, *)
extension IterableIteratorProtocol {}

@available(SwiftStdlib 6.4, *)
extension SpanIterator {}

@available(SwiftStdlib 6.4, *)
extension IterableIteratorAdapter {}

// CHECK: 'Iterable' requires -enable-experimental-feature BorrowingSequence
// CHECK: 'IterableIteratorProtocol' requires -enable-experimental-feature BorrowingSequence
// CHECK: 'SpanIterator' requires -enable-experimental-feature BorrowingSequence
// CHECK: 'IterableIteratorAdapter' requires -enable-experimental-feature BorrowingSequence
