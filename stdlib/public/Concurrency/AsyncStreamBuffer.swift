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
  @safe
  internal final class _Storage: @unchecked Sendable {
    typealias TerminationHandler = @Sendable (Continuation.Termination) -> Void

    @unsafe struct State {
      var continuations = unsafe [UnsafeContinuation<Element?, Never>]()
      var pending = _Deque<Element>()
      let limit: Continuation.BufferingPolicy
      var onTermination: TerminationHandler?
      var terminal: Bool = false

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
          if unsafe !state.terminal {
            switch limit {
            case .unbounded:
              unsafe state.pending.append(value)
              result = .enqueued(remaining: .max)
            case .bufferingOldest(let limit):
              if count < limit {
                unsafe state.pending.append(value)
                result = .enqueued(remaining: limit - (count + 1))
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
          unsafe continuation.resume(returning: toSend)
        } else if unsafe state.terminal {
          result = .terminated
          unlock()
          unsafe continuation.resume(returning: nil)
        } else {
          switch limit {
          case .unbounded:
            result = .enqueued(remaining: .max)
          case .bufferingNewest(let limit):
            result = .enqueued(remaining: limit)
          case .bufferingOldest(let limit):
            result = .enqueued(remaining: limit)
          }

          unlock()
          unsafe continuation.resume(returning: value)
        }
      } else {
        if unsafe !state.terminal {
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

    func finish() {
      lock()
      let handler = unsafe state.onTermination
      unsafe state.onTermination = nil
      unsafe state.terminal = true

      guard unsafe !state.continuations.isEmpty else {
        unlock()
        handler?(.finished)
        return
      }

      // Hold on to the continuations to resume outside the lock.
      let continuations = unsafe state.continuations
      unsafe state.continuations.removeAll()

      unlock()
      handler?(.finished)

      for unsafe continuation in unsafe continuations {
        unsafe continuation.resume(returning: nil)
      }
    }

    func next(_ continuation: UnsafeContinuation<Element?, Never>) {
      lock()
      unsafe state.continuations.append(continuation)
      if unsafe state.pending.count > 0 {
        let cont = unsafe state.continuations.removeFirst()
        let toSend = unsafe state.pending.removeFirst()
        unlock()
        unsafe cont.resume(returning: toSend)
      } else if unsafe state.terminal {
        let cont = unsafe state.continuations.removeFirst()
        unlock()
        unsafe cont.resume(returning: nil)
      } else {
        unlock()
      }

    }

    func next() async -> Element? {
      await withTaskCancellationHandler {
        unsafe await withUnsafeContinuation {
          unsafe next($0)
        }
      } onCancel: { [cancel] in
        cancel()
      }
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
      var continuation: UnsafeContinuation<Element?, Error>?
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
      if let continuation = unsafe state.continuation {
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
          unsafe state.continuation = nil
          let toSend = unsafe state.pending.removeFirst()
          unlock()
          unsafe continuation.resume(returning: toSend)
        } else if let terminal = unsafe state.terminal {
          result = .terminated
          unsafe state.continuation = nil
          unsafe state.terminal = .finished
          unlock()
          switch terminal {
          case .finished:
            unsafe continuation.resume(returning: nil)
          case .failed(let error):
            unsafe continuation.resume(throwing: error)
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

          unsafe state.continuation = nil
          unlock()
          unsafe continuation.resume(returning: value)
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
      if unsafe state.terminal == nil {
        if let failure = error {
          unsafe state.terminal = .failed(failure)
        } else {
          unsafe state.terminal = .finished
        }
      }

      if let continuation = unsafe state.continuation {
        if unsafe state.pending.count > 0 {
          unsafe state.continuation = nil
          let toSend = unsafe state.pending.removeFirst()
          unlock()
          handler?(.finished(error))
          unsafe continuation.resume(returning: toSend)
        } else if let terminal = unsafe state.terminal {
          unsafe state.continuation = nil
          unlock()
          handler?(.finished(error))
          switch terminal {
          case .finished:
            unsafe continuation.resume(returning: nil)
          case .failed(let error):
            unsafe continuation.resume(throwing: error)
          }
        } else {
          unlock()
          handler?(.finished(error))
        }
      } else {
        unlock()
        handler?(.finished(error))
      }
    }

    func next(_ continuation: UnsafeContinuation<Element?, Error>) {
      lock()
      if unsafe state.continuation == nil {
        if unsafe state.pending.count > 0 {
          let toSend = unsafe state.pending.removeFirst()
          unlock()
          unsafe continuation.resume(returning: toSend)
        } else if let terminal = unsafe state.terminal {
          unsafe state.terminal = .finished
          unlock()
          switch terminal {
          case .finished:
            unsafe continuation.resume(returning: nil)
          case .failed(let error):
            unsafe continuation.resume(throwing: error)
          }
        } else {
          unsafe state.continuation = unsafe continuation
          unlock()
        }
      } else {
        unlock()
        fatalError("attempt to await next() on more than one task")
      }
    }

    func next() async throws -> Element? {
      try await withTaskCancellationHandler {
        try unsafe await withUnsafeThrowingContinuation {
          unsafe next($0)
        }
      } onCancel: { [cancel] in
        cancel()
      }
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
#endif
