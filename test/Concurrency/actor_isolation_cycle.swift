// RUN: %target-swift-frontend %s -emit-sil -o /dev/null -verify
// RUN: %target-swift-frontend %s -emit-sil -o /dev/null -verify -strict-concurrency=targeted
// RUN: %target-swift-frontend %s -emit-sil -o /dev/null -verify -strict-concurrency=complete

// REQUIRES: concurrency

public protocol P {
  associatedtype T
  @MainActor func f(_: T)
  @MainActor func g(_: T)
}
public struct S : P {
  public func g(_: Int) {}
  public func f(_: T) {}
}

// https://github.com/apple/swift/issues/61602
@available(SwiftStdlib 5.1, *)
@MainActor protocol ProtocolWithAssociatedTypes {
    associatedtype ID: Hashable
    associatedtype Value

    subscript(_ id: ID) -> Value { get set }
}

@available(SwiftStdlib 5.1, *)
final class ClassConforming<ID: Hashable,Value>: ProtocolWithAssociatedTypes {
    subscript(id: ID) -> Value {
        get { fatalError() }
        set { fatalError() }
    }
}
