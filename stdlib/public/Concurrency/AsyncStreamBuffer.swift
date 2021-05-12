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

@_silgen_name("_swift_async_stream_lock_size")
func _lockWordCount() -> Int

@_silgen_name("_swift_async_stream_lock_init")
func _lockInit(_ ptr: UnsafeRawPointer)

@_silgen_name("_swift_async_stream_lock_lock")
func _lock(_ ptr: UnsafeRawPointer)

@_silgen_name("_swift_async_stream_lock_unlock")
func _unlock(_ ptr: UnsafeRawPointer)

@available(SwiftStdlib 5.5, *)
extension AsyncStream {
  internal final class _Storage: UnsafeSendable {
    typealias TerminationHandler = @Sendable (Continuation.Termination) -> Void

    struct State {
      var continuation: UnsafeContinuation<Element?, Never>?
      var pending = _Deque<Element>()
      let limit: Int
      var onTermination: TerminationHandler?
      var terminal: Bool = false

      init(limit: Int) {
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

    func cancel() {
      lock()
      // swap out the handler before we invoke it to prevent double cancel
      let handler = state.onTermination
      state.onTermination = nil
      unlock()
      
      handler?(.cancelled) // handler must be invoked before yielding nil for termination

      finish()
    }

    func yield(_ value: __owned Element) {
      lock()
      let limit = state.limit
      if let continuation = state.continuation {
        let count = state.pending.count
        if count > 0 {
          if !state.terminal && count < limit {
            state.pending.append(value)
          }
          state.continuation = nil
          let toSend = state.pending.removeFirst()
          unlock()
          continuation.resume(returning: toSend)
        } else if state.terminal {
          state.continuation = nil
          unlock()
          continuation.resume(returning: nil)
        } else {
          state.continuation = nil
          unlock()
          continuation.resume(returning: value)
        }
      } else {
        if !state.terminal && ((limit == .max) || (state.pending.count < limit)) {
          state.pending.append(value)
        }
        unlock()
      }
    }
    
    func finish() {
      lock()
      let handler = state.onTermination
      state.onTermination = nil
      state.terminal = true

      if let continuation = state.continuation {
        if state.pending.count > 0 {
          state.continuation = nil
          let toSend = state.pending.removeFirst()
          unlock()
          handler?(.finished)
          continuation.resume(returning: toSend)
        } else if state.terminal {
          state.continuation = nil
          unlock()
          handler?(.finished)
          continuation.resume(returning: nil)
        } else {
          unlock()
          handler?(.finished)
        }
      } else {
        if state.limit == 0 {
          state.pending.removeFirst()
        }
        unlock()
        handler?(.finished)
      }
    }

    func next(_ continuation: UnsafeContinuation<Element?, Never>) {
      lock()
      if state.continuation == nil {
        if state.pending.count > 0 {
          let toSend = state.pending.removeFirst()
          unlock()
          continuation.resume(returning: toSend)
        } else if state.terminal {
          unlock()
          continuation.resume(returning: nil)
        } else {
          state.continuation = continuation
          unlock()
        }
      } else {
        unlock()
        fatalError("attempt to await next() on more than one task")
      }
    }
    
    func next() async -> Element? {
      await withTaskCancellationHandler { [cancel] in
        cancel()
      } operation: {
        await withUnsafeContinuation {
          next($0)
        }
      }
    }

    static func create(limit: Int) -> _Storage {
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

@available(SwiftStdlib 5.5, *)
extension AsyncThrowingStream {
  internal final class _Storage: UnsafeSendable {
    typealias TerminationHandler = @Sendable (Continuation.Termination) -> Void
    enum Terminal {
      case finished
      case failed(Error)
    }
    
    struct State {
      var continuation: UnsafeContinuation<Element?, Error>?
      var pending = _Deque<Element>()
      let limit: Int
      var onTermination: TerminationHandler?
      var terminal: Terminal?

      init(limit: Int) {
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

    func cancel() {
      lock()
      // swap out the handler before we invoke it to prevent double cancel
      let handler = state.onTermination
      state.onTermination = nil
      unlock()
      
      handler?(.cancelled) // handler must be invoked before yielding nil for termination

      finish()
    }

    func yield(_ value: __owned Element) {
      lock()
      let limit = state.limit
      if let continuation = state.continuation {
        let count = state.pending.count
        if count > 0 {
          if state.terminal == nil && count < limit {
            state.pending.append(value)
          }
          state.continuation = nil
          let toSend = state.pending.removeFirst()
          unlock()
          continuation.resume(returning: toSend)
        } else if let terminal = state.terminal {
          state.continuation = nil
          state.terminal = .finished
          unlock()
          switch terminal {
          case .finished:
            continuation.resume(returning: nil)
          case .failed(let error):
            continuation.resume(throwing: error)
          }
        } else {
          state.continuation = nil
          unlock()
          continuation.resume(returning: value)
        }
      } else {
        if state.terminal == nil && ((limit == .max) || (state.pending.count < limit)) {
          state.pending.append(value)
        }
        unlock()
      }
    }
    
    func finish(throwing error: __owned Error? = nil) {
      lock()
      let handler = state.onTermination
      state.onTermination = nil
      if state.terminal == nil {
        if let failure = error {
          state.terminal = .failed(failure)
        } else {
          state.terminal = .finished
        }
      }

      if let continuation = state.continuation {
        if state.pending.count > 0 {
          state.continuation = nil
          let toSend = state.pending.removeFirst()
          unlock()
          handler?(.finished(error))
          continuation.resume(returning: toSend)
        } else if let terminal = state.terminal {
          state.continuation = nil
          unlock()
          handler?(.finished(error))
          switch terminal {
          case .finished:
            continuation.resume(returning: nil)
          case .failed(let error):
            continuation.resume(throwing: error)
          }
        } else {
          unlock()
          handler?(.finished(error))
        }
      } else {
        if state.limit == 0 {
          state.pending.removeFirst()
        }
        unlock()
        handler?(.finished(error))
      }
    }

    func next(_ continuation: UnsafeContinuation<Element?, Error>) {
      lock()
      if state.continuation == nil {
        if state.pending.count > 0 {
          let toSend = state.pending.removeFirst()
          unlock()
          continuation.resume(returning: toSend)
        } else if let terminal = state.terminal {
          state.terminal = .finished
          unlock()
          switch terminal {
          case .finished:
            continuation.resume(returning: nil)
          case .failed(let error):
            continuation.resume(throwing: error)
          }
        } else {
          state.continuation = continuation
          unlock()
        }
      } else {
        unlock()
        fatalError("attempt to await next() on more than one task")
      }
    }
    
    func next() async throws -> Element? {
      try await withTaskCancellationHandler { [cancel] in
        cancel()
      } operation: {
        try await withUnsafeThrowingContinuation {
          next($0)
        }
      }
    }

    static func create(limit: Int) -> _Storage {
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

