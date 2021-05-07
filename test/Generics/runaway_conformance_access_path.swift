// RUN: %target-swift-frontend -typecheck -debug-generic-signatures %s 2>&1 | %FileCheck %s
// RUN: %target-swift-frontend -emit-ir -verify %s

// Reduced from swift-futures project in the source compatibility suite.

public protocol FutureProtocol: FutureConvertible where FutureType == Self {
  associatedtype Output
}

public protocol FutureConvertible {
  associatedtype FutureType: FutureProtocol
}

func takesFuture<T : FutureProtocol>(_: T.Type) {}

public struct FutureHolder<T : FutureProtocol> {
  // CHECK-LABEL: Generic signature: <T, U where T == U.FutureType, U : FutureConvertible>
  init<U : FutureConvertible>(_: U) where U.FutureType == T {
    takesFuture(T.self)
  }
}
