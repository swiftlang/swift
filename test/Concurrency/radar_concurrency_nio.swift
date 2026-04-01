// RUN: %target-swift-frontend -enable-experimental-move-only -target %target-swift-5.1-abi-triple %s -emit-sil -o /dev/null -verify
// RUN: %target-swift-frontend -enable-experimental-move-only -target %target-swift-5.1-abi-triple %s -emit-sil -o /dev/null -verify -strict-concurrency=targeted
// RUN: %target-swift-frontend -enable-experimental-move-only -target %target-swift-5.1-abi-triple %s -emit-sil -o /dev/null -verify -strict-concurrency=complete


// REQUIRES: concurrency

public protocol EventLoop: Sendable {}

#if compiler(>=5.9)
/// A helper protocol that can be mixed in to a NIO ``EventLoop`` to provide an
/// automatic conformance to `SerialExecutor`.
///
/// Implementers of `EventLoop` should consider conforming to this protocol as
/// well on Swift 5.9 and later.
@available(macOS 14.0, iOS 17.0, watchOS 10.0, tvOS 17.0, *)
public protocol NIOSerialEventLoopExecutor: EventLoop, SerialExecutor { }

@available(macOS 14.0, iOS 17.0, watchOS 10.0, tvOS 17.0, *)
extension NIOSerialEventLoopExecutor {
  @inlinable
  public func enqueue(_ job: consuming ExecutorJob) {
    fatalError("mock impl")
  }

  @inlinable
  public func asUnownedSerialExecutor() -> UnownedSerialExecutor {
    UnownedSerialExecutor(ordinary: self)
  }

  @inlinable
  public var executor: any SerialExecutor {
    self
  }
}

// EARLIER AVAILABILITY
final class NIODefaultSerialEventLoopExecutor {
  @usableFromInline
  let loop: EventLoop

  @inlinable
  init(_ loop: EventLoop) {
    self.loop = loop
  }
}

@available(macOS 14.0, iOS 17.0, watchOS 10.0, tvOS 17.0, *)
extension NIODefaultSerialEventLoopExecutor: SerialExecutor {
  @inlinable
  public func enqueue(_ job: consuming ExecutorJob) { // do NOT issue a warning here
    fatalError("mock impl")
  }

  @inlinable
  public func asUnownedSerialExecutor() -> UnownedSerialExecutor {
    UnownedSerialExecutor(complexEquality: self)
  }

  @inlinable
  public func isSameExclusiveExecutionContext(other: NIODefaultSerialEventLoopExecutor) -> Bool {
    false
  }
}
#endif
