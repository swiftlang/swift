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

// MARK: - AsyncStream Shims

// BufferingPolicy

extension AsyncStream.Continuation.BufferingPolicy {
  func asStorageBufferingPolicy()
  -> _AsyncStreamStorage<Element, Never>.Continuation.BufferingPolicy {
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

extension AsyncStream.Continuation.Termination {
  func asStorageTermination()
  -> _AsyncStreamStorage<Element, Never>.Continuation.Termination {
    switch self {
    case .finished:
      return .finished(nil)
    case .cancelled:
      return .cancelled
    }
  }
}

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
  internal typealias StorageTerminationHandler =
  @Sendable (_AsyncStreamStorage<Element, Never>.Continuation.Termination) -> Void

  internal typealias StreamTerminationHandler =
  @Sendable (Termination) -> Void

  func adaptToStreamTerminationHandler(
    _ onTermination: StorageTerminationHandler?
  ) -> StreamTerminationHandler? {
    guard
      let onTermination
    else { return nil }

    return { @Sendable termination in
      onTermination(termination.asStorageTermination())
    }
  }

  func adaptToStorageTerminationHandler(
    _ onTermination: StreamTerminationHandler?
  ) -> StorageTerminationHandler? {
    guard
      let onTermination
    else { return nil }

    return { @Sendable termination in
      onTermination(termination.asStreamTermination())
    }
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
  -> _AsyncStreamStorage<Element, Failure>.Continuation.BufferingPolicy {
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

extension AsyncThrowingStream.Continuation.Termination {
  func asStorageTermination()
  -> _AsyncStreamStorage<Element, Failure>.Continuation.Termination {
    switch self {
    case .finished:
      return .finished(nil)
    case .cancelled:
      return .cancelled
    }
  }
}

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
  internal typealias StorageTerminationHandler =
  @Sendable (_AsyncStreamStorage<Element, Failure>.Continuation.Termination) -> Void

  internal typealias StreamTerminationHandler =
  @Sendable (Termination) -> Void

  func adaptToStreamTerminationHandler(
    _ onTermination: StorageTerminationHandler?
  ) -> StreamTerminationHandler? {
    guard
      let onTermination
    else { return nil }

    return { @Sendable termination in
      onTermination(termination.asStorageTermination())
    }
  }

  func adaptToStorageTerminationHandler(
    _ onTermination: StreamTerminationHandler?
  ) -> StorageTerminationHandler? {
    guard
      let onTermination
    else { return nil }

    return { @Sendable termination in
      onTermination(termination.asStreamTermination())
    }
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
