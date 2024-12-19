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
  internal final class _Storage: @unchecked Sendable {
    typealias TerminationHandler = @Sendable (Continuation.Termination) -> Void

    struct State {
      var continuations = [UnsafeContinuation<Element?, Never>]()
      var pending = _Deque<Element>()
      let limit: Continuation.BufferingPolicy
      var onTermination: TerminationHandler?
      var terminal: Bool = false

      init(limit: Continuation.BufferingPolicy) {
        self.limit = limit
      }
    }
    // Stored as a singular structured assignment for initialization
    var state: State

    private init(_doNotCallMe: ()) {
      fatalError("Storage must be initialized by create")
    }

    deinit {
      state.onTermination?(.cancelled)
    }

    private func lock() {
      let ptr =
        UnsafeRawPointer(Builtin.projectTailElems(self, UnsafeRawPointer.self))
      _lock(ptr)
    }

    private func unlock() {
      let ptr =
        UnsafeRawPointer(Builtin.projectTailElems(self, UnsafeRawPointer.self))
      _unlock(ptr)
    }

    func getOnTermination() -> TerminationHandler? {
      lock()
      let handler = state.onTermination
      unlock()
      return handler
    }

    func setOnTermination(_ newValue: TerminationHandler?) {
      lock()
      withExtendedLifetime(state.onTermination) {
        state.onTermination = newValue
        unlock()
      }
    }

    @Sendable func cancel() {
      lock()
      // swap out the handler before we invoke it to prevent double cancel
      let handler = state.onTermination
      state.onTermination = nil
      unlock()

      // handler must be invoked before yielding nil for termination
      handler?(.cancelled)

      finish()
    }

    func yield(_ value: __owned Element) -> Continuation.YieldResult {
      let result: Continuation.YieldResult
      lock()
      let limit = state.limit
      let count = state.pending.count

      if !state.continuations.isEmpty {
        // Presence of continuations implies no pending elements.
        // TODO: which assertion flavor should be used?
        assert(
          state.pending.isEmpty,
          "Continuations should imply no pending elements."
        )
        let continuation = state.continuations.removeFirst()
        if state.terminal {
          result = .terminated
          unlock()
          continuation.resume(returning: nil)
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
          continuation.resume(returning: value)
        }
      } else {
        if !state.terminal {
          switch limit {
          case .unbounded:
            result = .enqueued(remaining: .max)
            state.pending.append(value)
          case .bufferingOldest(let limit):
            if count < limit {
              result = .enqueued(remaining: limit - (count + 1))
              state.pending.append(value)
            } else {
              result = .dropped(value)
            }
          case .bufferingNewest(let limit):
            if count < limit {
              state.pending.append(value)
              result = .enqueued(remaining: limit - (count + 1))
            } else if count > 0 {
              result = .dropped(state.pending.removeFirst())
              state.pending.append(value)
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

    func finish() {
      lock()
      let handler = state.onTermination
      state.onTermination = nil
      state.terminal = true

      guard !state.continuations.isEmpty else {
        unlock()
        handler?(.finished)
        return
      }

      assert(
        state.pending.isEmpty,
        "Continuations should imply no pending elements."
      )

      // Hold on to the continuations to resume outside the lock.
      let continuations = state.continuations
      state.continuations.removeAll()

      unlock()
      handler?(.finished)

      for continuation in continuations {
        continuation.resume(returning: nil)
      }
    }

    func next(_ continuation: UnsafeContinuation<Element?, Never>) {
      lock()
      state.continuations.append(continuation)
      if state.pending.count > 0 {
        let cont = state.continuations.removeFirst()
        let toSend = state.pending.removeFirst()
        unlock()
        cont.resume(returning: toSend)
      } else if state.terminal {
        let cont = state.continuations.removeFirst()
        unlock()
        cont.resume(returning: nil)
      } else {
        unlock()
      }

    }

    func next() async -> Element? {
      await withTaskCancellationHandler {
        await withUnsafeContinuation {
          next($0)
        }
      } onCancel: { [cancel] in
        cancel()
      }
    }

    static func create(limit: Continuation.BufferingPolicy) -> _Storage {
      let minimumCapacity = _lockWordCount()
      let storage = Builtin.allocWithTailElems_1(
        _Storage.self,
          minimumCapacity._builtinWordValue,
          UnsafeRawPointer.self
      )

      let state =
        UnsafeMutablePointer<State>(Builtin.addressof(&storage.state))
      state.initialize(to: State(limit: limit))
      let ptr = UnsafeRawPointer(
        Builtin.projectTailElems(storage, UnsafeRawPointer.self))
      _lockInit(ptr)
      return storage
    }
  }
}

@available(SwiftStdlib 5.1, *)
extension AsyncThrowingStream {
  internal final class _Storage: @unchecked Sendable {
    typealias TerminationHandler = @Sendable (Continuation.Termination) -> Void
    enum Terminal {
      case finished
      case failed(Failure)
    }

    struct State {
      var continuations = [UnsafeContinuation<Element?, Error>]()
      var pending = _Deque<Element>()
      let limit: Continuation.BufferingPolicy
      var onTermination: TerminationHandler?
      var terminal: Terminal?

      init(limit: Continuation.BufferingPolicy) {
        self.limit = limit
      }
    }
    // Stored as a singular structured assignment for initialization
    var state: State

    private init(_doNotCallMe: ()) {
      fatalError("Storage must be initialized by create")
    }

    deinit {
      state.onTermination?(.cancelled)
    }

    private func lock() {
      let ptr =
        UnsafeRawPointer(Builtin.projectTailElems(self, UnsafeRawPointer.self))
      _lock(ptr)
    }

    private func unlock() {
      let ptr =
        UnsafeRawPointer(Builtin.projectTailElems(self, UnsafeRawPointer.self))
      _unlock(ptr)
    }

    func getOnTermination() -> TerminationHandler? {
      lock()
      let handler = state.onTermination
      unlock()
      return handler
    }

    func setOnTermination(_ newValue: TerminationHandler?) {
      lock()
      withExtendedLifetime(state.onTermination) {
        state.onTermination = newValue
        unlock()
      }
    }

    @Sendable func cancel() {
      lock()
      // swap out the handler before we invoke it to prevent double cancel
      let handler = state.onTermination
      state.onTermination = nil
      unlock()

      // handler must be invoked before yielding nil for termination
      handler?(.cancelled)

      finish()
    }

    func yield(_ value: __owned Element) -> Continuation.YieldResult {
      let result: Continuation.YieldResult
      lock()
      let limit = state.limit
      let count = state.pending.count

      if !state.continuations.isEmpty {
        assert(
          state.pending.isEmpty,
          "Continuations should imply no pending elements."
        )
        let continuation = state.continuations.removeFirst()
        if let terminal = state.terminal {
          result = .terminated
          state.terminal = .finished
          unlock()
          switch terminal {
          case .finished:
            continuation.resume(returning: nil)
          case .failed(let error):
            continuation.resume(throwing: error)
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
          continuation.resume(returning: value)
        }
      } else {
        if state.terminal == nil {
          switch limit {
          case .unbounded:
            result = .enqueued(remaining: .max)
            state.pending.append(value)
          case .bufferingOldest(let limit):
            if count < limit {
              result = .enqueued(remaining: limit - (count + 1))
              state.pending.append(value)
            } else {
              result = .dropped(value)
            }
          case .bufferingNewest(let limit):
            if count < limit {
              state.pending.append(value)
              result = .enqueued(remaining: limit - (count + 1))
            } else if count > 0 {
              result = .dropped(state.pending.removeFirst())
              state.pending.append(value)
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
      let handler = state.onTermination
      state.onTermination = nil

      let terminal: Terminal
      if let failure = error {
        terminal = .failed(failure)
      } else {
        terminal = .finished
      }

      // Update terminal state if needed
      if state.terminal == nil {
        state.terminal = terminal
      }

      guard !state.continuations.isEmpty else {
          unlock()
          handler?(.finished(error))
          return
      }

      assert(
        state.pending.isEmpty,
        "Continuations should imply no pending elements."
      )

      // Hold on to the continuations to resume outside the lock.
      let continuations = state.continuations
      state.continuations.removeAll()

      unlock()
      handler?(.finished(error))

      for continuation in continuations {
          switch terminal {
          case .finished:
            continuation.resume(returning: nil)
          case .failed(let error):
            continuation.resume(throwing: error)
          }
      }
    }

    func next(_ continuation: UnsafeContinuation<Element?, Error>) {
      lock()
      state.continuations.append(continuation)
      if state.pending.count > 0 {
        let cont = state.continuations.removeFirst()
        let toSend = state.pending.removeFirst()
        unlock()
        cont.resume(returning: toSend)
      } else if let terminal = state.terminal {
        state.terminal = .finished
        let cont = state.continuations.removeFirst()
        unlock()
        switch terminal {
        case .finished:
          cont.resume(returning: nil)
        case .failed(let error):
          cont.resume(throwing: error)
        }
      } else {
        unlock()
      }
    }

    func next() async throws -> Element? {
      try await withTaskCancellationHandler {
        try await withUnsafeThrowingContinuation {
          next($0)
        }
      } onCancel: { [cancel] in
        cancel()
      }
    }

    static func create(limit: Continuation.BufferingPolicy) -> _Storage {
      let minimumCapacity = _lockWordCount()
      let storage = Builtin.allocWithTailElems_1(
        _Storage.self,
          minimumCapacity._builtinWordValue,
          UnsafeRawPointer.self
      )

      let state =
        UnsafeMutablePointer<State>(Builtin.addressof(&storage.state))
      state.initialize(to: State(limit: limit))
      let ptr = UnsafeRawPointer(
        Builtin.projectTailElems(storage, UnsafeRawPointer.self))
      _lockInit(ptr)
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
      UnsafeRawPointer(Builtin.projectTailElems(self, UnsafeRawPointer.self))
    _lock(ptr)
  }

  private func unlock() {
    let ptr =
      UnsafeRawPointer(Builtin.projectTailElems(self, UnsafeRawPointer.self))
    _unlock(ptr)
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
    let storage = Builtin.allocWithTailElems_1(
      _AsyncStreamCriticalStorage.self,
      minimumCapacity._builtinWordValue,
      UnsafeRawPointer.self
    )

    let state =
      UnsafeMutablePointer<Contents>(Builtin.addressof(&storage._value))
    state.initialize(to: initial)
    let ptr = UnsafeRawPointer(
      Builtin.projectTailElems(storage, UnsafeRawPointer.self))
    _lockInit(ptr)
    return storage
  }
}
#endif
