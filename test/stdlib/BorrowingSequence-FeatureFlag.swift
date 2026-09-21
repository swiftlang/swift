// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-ir %s -module-name main -parse-as-library -enable-experimental-feature BorrowingSequence
// RUN: %target-swift-emit-ir %s -module-name main -parse-as-library

// REQUIRES: swift_feature_BorrowingSequence

@available(SwiftStdlib 6.4, *)
extension Iterable {}

@available(SwiftStdlib 6.4, *)
extension BorrowingIteratorProtocol {}

@available(SwiftStdlib 6.4, *)
extension BorrowingIteratorAdapter {}
