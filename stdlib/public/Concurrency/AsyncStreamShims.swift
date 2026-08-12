//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Swift

#if !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY

/// Holds the termination handler an adopter installed through the public
/// `Continuation.onTermination` setter.
@safe
internal final class _AsyncStreamTerminationHandlerBox<
  Element, Failure: Error, TerminationType
>: Sendable {
  typealias StorageTermination =
    _AsyncStreamStorage<Element, Failure, TerminationType>.Continuation.Termination

  let handler: @Sendable (TerminationType) -> Void
  private let invokeHandler: @Sendable (StorageTermination) -> Void

  /// `map` translates the storage's `Termination` and is installed once, when
  /// the box is created, so no wrapper is ever appended to `handler` itself
  init(
    handler: @escaping @Sendable (TerminationType) -> Void,
    map: @escaping @Sendable (StorageTermination) -> TerminationType
  ) {
    self.handler = handler
    self.invokeHandler = { termination in handler(map(termination)) }
  }

  func invoke(_ termination: StorageTermination) {
    self.invokeHandler(termination)
  }
}

// MARK: - AsyncStream Shims

// BufferingPolicy

extension AsyncStream.Continuation.BufferingPolicy {
  func asStorageBufferingPolicy()
  -> AsyncStream<Element>.Continuation.Storage.Continuation.BufferingPolicy {
    switch self {
    case .unbounded:
      return .unbounded
    case let .bufferingOldest(limit):
      return .bufferingOldest(limit)
    case let .bufferingNewest(limit):
      return .bufferingNewest(limit)
    }
  }
}

// Termination

extension _AsyncStreamStorage.Continuation.Termination {
  func asStreamTermination()
  -> AsyncStream<Element>.Continuation.Termination {
    switch self {
    case .finished:
      return .finished
    case .cancelled:
      return .cancelled
    }
  }
}

// TerminationHandler

extension AsyncStream.Continuation {
  internal typealias TerminationHandlerBox =
    _AsyncStreamTerminationHandlerBox<Element, Never, Termination>

  func box(_ onTermination: (@Sendable (Termination) -> Void)?) -> TerminationHandlerBox? {
    guard let onTermination else { return nil }

    return TerminationHandlerBox(handler: onTermination) { termination in
      termination.asStreamTermination()
    }
  }

  func unbox(_ box: TerminationHandlerBox?) -> (@Sendable (Termination) -> Void)? {
    return box?.handler
  }
}

// YieldResult

extension _AsyncStreamStorage.Continuation.YieldResult {
  func asStreamYieldResult()
  -> AsyncStream<Element>.Continuation.YieldResult {
    switch self {
    case let .enqueued(remaining):
      return .enqueued(remaining: remaining)
    case let .dropped(element):
      return .dropped(element)
    case .terminated:
      return .terminated
    }
  }
}

// MARK: - AsyncThrowingStream Shims

// BufferingPolicy

extension AsyncThrowingStream.Continuation.BufferingPolicy {
  func asStorageBufferingPolicy()
  -> AsyncThrowingStream<Element, Failure>.Continuation.Storage.Continuation.BufferingPolicy {
    switch self {
    case .unbounded:
      return .unbounded
    case let .bufferingOldest(limit):
      return .bufferingOldest(limit)
    case let .bufferingNewest(limit):
      return .bufferingNewest(limit)
    }
  }
}

// Termination

extension _AsyncStreamStorage.Continuation.Termination {
  func asStreamTermination()
  -> AsyncThrowingStream<Element, Failure>.Continuation.Termination {
    switch self {
    case let .finished(failure):
      return .finished(failure)
    case .cancelled:
      return .cancelled
    }
  }
}

// TerminationHandler

extension AsyncThrowingStream.Continuation {
  internal typealias TerminationHandlerBox =
    _AsyncStreamTerminationHandlerBox<Element, Failure, Termination>

  func box(_ onTermination: (@Sendable (Termination) -> Void)?) -> TerminationHandlerBox? {
    guard let onTermination else { return nil }

    return TerminationHandlerBox(handler: onTermination) { termination in
      termination.asStreamTermination()
    }
  }

  func unbox(_ box: TerminationHandlerBox?) -> (@Sendable (Termination) -> Void)? {
    return box?.handler
  }
}

// YieldResult

extension _AsyncStreamStorage.Continuation.YieldResult {
  func asStreamYieldResult()
  -> AsyncThrowingStream<Element, Failure>.Continuation.YieldResult {
    switch self {
    case let .enqueued(remaining):
      return .enqueued(remaining: remaining)
    case let .dropped(element):
      return .dropped(element)
    case .terminated:
      return .terminated
    }
  }
}
#endif
