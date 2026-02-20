//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020-2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Swift

#if !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
#if ASYNC_STREAM_STANDALONE
@_exported import _Concurrency
import Darwin

func _lockWordCount() -> Int {
  let sz =
    MemoryLayout<os_unfair_lock>.size / MemoryLayout<UnsafeRawPointer>.size
  return max(sz, 1)
}

func _lockInit(_ ptr: UnsafeRawPointer) {
  UnsafeMutableRawPointer(mutating: ptr)
    .assumingMemoryBound(to: os_unfair_lock.self)
    .initialize(to: os_unfair_lock())
}

func _lock(_ ptr: UnsafeRawPointer) {
  os_unfair_lock_lock(UnsafeMutableRawPointer(mutating: ptr)
    .assumingMemoryBound(to: os_unfair_lock.self))
}

func _unlock(_ ptr: UnsafeRawPointer) {
  os_unfair_lock_unlock(UnsafeMutableRawPointer(mutating: ptr)
    .assumingMemoryBound(to: os_unfair_lock.self))
}
#else
@_silgen_name("_swift_async_stream_lock_size")
func _lockWordCount() -> Int

@_silgen_name("_swift_async_stream_lock_init")
func _lockInit(_ ptr: UnsafeRawPointer)

@_silgen_name("_swift_async_stream_lock_lock")
func _lock(_ ptr: UnsafeRawPointer)

@_silgen_name("_swift_async_stream_lock_unlock")
func _unlock(_ ptr: UnsafeRawPointer)
#endif

@available(SwiftStdlib 5.1, *)
extension AsyncStream {
  typealias _Storage = AsyncThrowingStream<Element, Never>._Storage
}

@available(SwiftStdlib 5.1, *)
extension AsyncThrowingStream {
  @safe
  internal final class _Storage: @unchecked Sendable {
    typealias TerminationHandler = @Sendable (Continuation.Termination) -> Void
    enum Terminal {
      case finished
      case failed(Failure)
    }

    @unsafe struct State {
      var continuations = unsafe [UnsafeContinuation<Result<Element?, Failure>, Never>]()
      var pending = _Deque<Element>()
      let limit: Continuation.BufferingPolicy
      var onTermination: TerminationHandler?
      var terminal: Terminal?

      init(limit: Continuation.BufferingPolicy) {
        unsafe self.limit = limit
      }
    }
    // Stored as a singular structured assignment for initialization
    var state: State

    private init(_doNotCallMe: ()) {
      fatalError("Storage must be initialized by create")
    }

    deinit {
      unsafe state.onTermination?(.cancelled)
    }

    private func lock() {
      let ptr =
        unsafe UnsafeRawPointer(Builtin.projectTailElems(self, UnsafeRawPointer.self))
      unsafe _lock(ptr)
    }

    private func unlock() {
      let ptr =
        unsafe UnsafeRawPointer(Builtin.projectTailElems(self, UnsafeRawPointer.self))
      unsafe _unlock(ptr)
    }

    func getOnTermination() -> TerminationHandler? {
      lock()
      let handler = unsafe state.onTermination
      unlock()
      return handler
    }

    func setOnTermination(_ newValue: TerminationHandler?) {
      lock()
      unsafe withExtendedLifetime(state.onTermination) {
        unsafe state.onTermination = newValue
        unlock()
      }
    }

    @Sendable func cancel() {
      lock()
      // swap out the handler before we invoke it to prevent double cancel
      let handler = unsafe state.onTermination
      unsafe state.onTermination = nil
      unlock()

      // handler must be invoked before yielding nil for termination
      handler?(.cancelled)

      finish()
    }

    func yield(_ value: __owned Element) -> Continuation.YieldResult {
      var result: Continuation.YieldResult
      lock()
      let limit = unsafe state.limit
      let count = unsafe state.pending.count

      if unsafe !state.continuations.isEmpty {
        let continuation = unsafe state.continuations.removeFirst()
        if count > 0 {
          if unsafe state.terminal == nil {
            switch limit {
            case .unbounded:
              result = .enqueued(remaining: .max)
              unsafe state.pending.append(value)
            case .bufferingOldest(let limit):
              if count < limit {
                result = .enqueued(remaining: limit - (count + 1))
                unsafe state.pending.append(value)
              } else {
                result = .dropped(value)
              }
            case .bufferingNewest(let limit):
              if count < limit {
                unsafe state.pending.append(value)
                result = .enqueued(remaining: limit - (count + 1))
              } else if count > 0 {
                result = unsafe .dropped(state.pending.removeFirst())
                unsafe state.pending.append(value)
              } else {
                result = .dropped(value)
              }
            }
          } else {
            result = .terminated
          }
          let toSend = unsafe state.pending.removeFirst()
          unlock()
          unsafe continuation.resume(returning: .success(toSend))
        } else if let terminal = unsafe state.terminal {
          // FIXME: this case is presumably unreachable
          // We should never be in a terminal state and have pending continuations
          result = .terminated
          unsafe state.terminal = .finished
          unlock()
          switch terminal {
          case .finished:
            unsafe continuation.resume(returning: .success(nil))
          case .failed(let error):
            unsafe continuation.resume(returning: .failure(error))
          }
        } else {
          switch limit {
          case .unbounded:
            result = .enqueued(remaining: .max)
          case .bufferingOldest(let limit):
            result = .enqueued(remaining: limit)
          case .bufferingNewest(let limit):
            result = .enqueued(remaining: limit)
          }

          unlock()
          unsafe continuation.resume(returning: .success(value))
        }
      } else {
        if unsafe state.terminal == nil {
          switch limit {
          case .unbounded:
            result = .enqueued(remaining: .max)
            unsafe state.pending.append(value)
          case .bufferingOldest(let limit):
            if count < limit {
              result = .enqueued(remaining: limit - (count + 1))
              unsafe state.pending.append(value)
            } else {
              result = .dropped(value)
            }
          case .bufferingNewest(let limit):
            if count < limit {
              unsafe state.pending.append(value)
              result = .enqueued(remaining: limit - (count + 1))
            } else if count > 0 {
              result = unsafe .dropped(state.pending.removeFirst())
              unsafe state.pending.append(value)
            } else {
              result = .dropped(value)
            }
          }
        } else {
          result = .terminated
        }
        unlock()
      }
      return result
    }

    func finish(throwing error: __owned Failure? = nil) {
      lock()
      let handler = unsafe state.onTermination
      unsafe state.onTermination = nil
      let terminal = unsafe state.terminal ?? {
        let terminal  = error.map { Terminal.failed($0) } ?? .finished
        unsafe state.terminal = terminal
        return terminal
      }()

      guard unsafe !state.continuations.isEmpty else {
        unlock()
        handler?(.finished(error))
        return
      }

      // Hold on to the continuations to resume outside the lock.
      let continuations = unsafe state.continuations
      unsafe state.continuations.removeAll()

      unlock()
      handler?(.finished(error))

      let result: Result<Element?, Failure> = switch terminal {
      case .finished: .success(nil)
      case .failed(let error): .failure(error)
      }

      for unsafe continuation in unsafe continuations {
        unsafe continuation.resume(returning: result)
      }
    }

    func next(_ continuation: UnsafeContinuation<Result<Element?, Failure>, Never>) {
      lock()
      unsafe state.continuations.append(continuation)
      if unsafe state.pending.count > 0 {
        let cont = unsafe state.continuations.removeFirst()
        let toSend = unsafe state.pending.removeFirst()
        unlock()
        unsafe cont.resume(returning: .success(toSend))
      } else if let terminal = unsafe state.terminal {
        let cont = unsafe state.continuations.removeFirst()
        unsafe state.terminal = .finished
        unlock()
        switch terminal {
        case .finished:
          unsafe cont.resume(returning: .success(nil))
        case .failed(let error):
          unsafe cont.resume(returning: .failure(error))
        }
      } else {
        unlock()
      }
    }

    func next() async throws(Failure) -> Element? {
      try await withTaskCancellationHandler {
        unsafe await withUnsafeContinuation {
          unsafe next($0)
        }
      } onCancel: { [cancel] in
        cancel()
      }.get()
    }

    static func create(limit: Continuation.BufferingPolicy) -> _Storage {
      let minimumCapacity = _lockWordCount()
      let storage = unsafe Builtin.allocWithTailElems_1(
        _Storage.self,
          minimumCapacity._builtinWordValue,
          UnsafeRawPointer.self
      )

      let state =
        unsafe UnsafeMutablePointer<State>(Builtin.addressof(&storage.state))
      unsafe state.initialize(to: State(limit: limit))
      let ptr = unsafe UnsafeRawPointer(
        Builtin.projectTailElems(storage, UnsafeRawPointer.self))
      unsafe _lockInit(ptr)
      return storage
    }
  }
}

// this is used to store closures; which are two words
final class _AsyncStreamCriticalStorage<Contents>: @unchecked Sendable {
  var _value: Contents
  private init(_doNotCallMe: ()) {
    fatalError("_AsyncStreamCriticalStorage must be initialized by create")
  }

  private func lock() {
    let ptr =
      unsafe UnsafeRawPointer(Builtin.projectTailElems(self, UnsafeRawPointer.self))
    unsafe _lock(ptr)
  }

  private func unlock() {
    let ptr =
      unsafe UnsafeRawPointer(Builtin.projectTailElems(self, UnsafeRawPointer.self))
    unsafe _unlock(ptr)
  }

  var value: Contents {
    get {
      lock()
      let contents = _value
      unlock()
      return contents
    }

    set {
      lock()
      withExtendedLifetime(_value) {
        _value = newValue
        unlock()
      }
    }
  }

  static func create(_ initial: Contents) -> _AsyncStreamCriticalStorage {
    let minimumCapacity = _lockWordCount()
    let storage = unsafe Builtin.allocWithTailElems_1(
      _AsyncStreamCriticalStorage.self,
      minimumCapacity._builtinWordValue,
      UnsafeRawPointer.self
    )

    let state =
      unsafe UnsafeMutablePointer<Contents>(Builtin.addressof(&storage._value))
    unsafe state.initialize(to: initial)
    let ptr = unsafe UnsafeRawPointer(
      Builtin.projectTailElems(storage, UnsafeRawPointer.self))
    unsafe _lockInit(ptr)
    return storage
  }
}

// MARK: Conversion Shims

extension AsyncThrowingStream.Continuation.YieldResult {

  internal var nonThrowingRepresentation: AsyncStream<Element>.Continuation.YieldResult {
    switch self {
    case .dropped(let element): .dropped(element)
    case .enqueued(let remaining): .enqueued(remaining: remaining)
    case .terminated: .terminated
    }
  }
}

extension AsyncStream.Continuation.BufferingPolicy {
  internal var throwingRepresentation: AsyncThrowingStream<Element, Never>.Continuation.BufferingPolicy {
    switch self {
    case .bufferingNewest(let limit): .bufferingNewest(limit)
    case .bufferingOldest(let limit): .bufferingOldest(limit)
    case .unbounded: .unbounded
    }
  }
}

extension AsyncThrowingStream.Continuation.BufferingPolicy {

  internal var nonThrowingRepresentation: AsyncStream<Element>.Continuation.BufferingPolicy {
    switch self {
    case .bufferingNewest(let limit): .bufferingNewest(limit)
    case .bufferingOldest(let limit): .bufferingOldest(limit)
    case .unbounded: .unbounded
    }
  }
}

extension AsyncStream.Continuation.Termination {
  internal var throwingRepresentation: AsyncThrowingStream<Element, Never>.Continuation.Termination {
    switch self {
    case .finished: .finished(nil)
    case .cancelled: .cancelled
    }
  }
}

extension AsyncThrowingStream.Continuation.Termination {

  internal var nonThrowingRepresentation: AsyncStream<Element>.Continuation.Termination {
    switch self {
    case .finished: .finished
    case .cancelled: .cancelled
    }
  }
}

#endif
