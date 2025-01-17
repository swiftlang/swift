// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -module-name conformances
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -module-name conformances
// RUN: %FileCheck %s < %t.swiftinterface

// REQUIRES: concurrency, OS=macosx

// CHECK: public struct SequenceAdapte
@available(SwiftStdlib 5.1, *)
public struct SequenceAdapter<Base: AsyncSequence>: AsyncSequence {
  // CHECK-LABEL: public struct AsyncIterator
  // CHECK: @available{{.*}}macOS 10.15
  // CHECK-NEXT: public typealias Element = Base.Element

  // CHECK: @available(
  // CHECK: @_implements(_Concurrency.AsyncIteratorProtocol, Failure)
  // CHECK-SAME: public typealias __AsyncIteratorProtocol_Failure = Base.Failure
  public typealias Element = Base.Element

  public struct AsyncIterator: AsyncIteratorProtocol {
    public mutating func next() async rethrows -> Base.Element? { nil }
  }

  // CHECK-LABEL: public func makeAsyncIterator
  public func makeAsyncIterator() -> AsyncIterator { AsyncIterator() }

  // CHECK: @available(
  // CHECK: @_implements(_Concurrency.AsyncSequence, Failure)
  // CHECK-SAME: public typealias __AsyncSequence_Failure = Base.Failure
}

// CHECK: public struct OtherSequenceAdapte
@available(SwiftStdlib 5.1, *)
public struct OtherSequenceAdapter<Base: AsyncSequence>: AsyncSequence {
  // CHECK: public typealias Element = Base.Element
  // CHECK-NOT: public typealias Failure
  // CHECK: public struct Failure

  // CHECK-LABEL: public struct AsyncIterator
  // CHECK: @available{{.*}}macOS 10.15
  // CHECK: @available(
  // CHECK: @_implements(_Concurrency.AsyncIteratorProtocol, Failure)
  // CHECK-SAME: public typealias __AsyncIteratorProtocol_Failure = Base.Failure
  public typealias Element = Base.Element

  public struct Failure: Error { }

  // CHECK-NOT: public typealias Failure
  public struct AsyncIterator: AsyncIteratorProtocol {
    public mutating func next() async rethrows -> Base.Element? { nil }
  }

  // CHECK: public func makeAsyncIterator
  public func makeAsyncIterator() -> AsyncIterator { AsyncIterator() }

  // CHECK-NOT: public typealias Failure
}

// CHECK: public struct MineOwnIterator
@available(SwiftStdlib 5.1, *)
public struct MineOwnIterator<Element>: AsyncSequence, AsyncIteratorProtocol {
  public mutating func next() async -> Element? { nil }
  public func makeAsyncIterator() -> Self { self }

  // CHECK:      @_implements(_Concurrency.AsyncIteratorProtocol, Failure)
  // CHECK-SAME: public typealias __AsyncIteratorProtocol_Failure = Swift.Never

  // CHECK:      @_implements(_Concurrency.AsyncSequence, Failure)
  // CHECK-SAME: public typealias __AsyncSequence_Failure = Swift.Never
}
