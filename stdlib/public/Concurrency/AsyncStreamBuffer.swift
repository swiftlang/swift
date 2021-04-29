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

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
internal final class _AsyncStreamBufferedStorage<Element, Failure: Error>: UnsafeSendable {
  enum Terminal {
    case finished
    case failed(Failure)
  }

  struct State {
    // continuation can be either of two types:
    // UnsafeContinuation<Element?, Error>
    // UnsafeContinuation<Element?, Never>
    var continuation: Builtin.RawUnsafeContinuation?
    var pending = [Element]()
    var terminal: Terminal?
    var onTermination: (@Sendable () -> Void)?
    let limit: Int

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
    state.onTermination?()
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

  var onTermination: (@Sendable () -> Void)? {
    get {
      lock()
      let handler = state.onTermination
      unlock()
      return handler
    }
    set {
      lock()
      withExtendedLifetime(state.onTermination) {
        state.onTermination = newValue
        unlock()
      }
    }
  }

  func cancel() {
    lock()
    // swap out the handler before we invoke it to prevent double cancel
    let handler = state.onTermination
    state.onTermination = nil
    unlock()
    
    handler?() // handler must be invoked before yielding nil for termination

    yield(nil)
  }

  private func enqueue(_ value: __owned Element?) {
    if let value = value {
      if state.terminal == nil {
        state.pending.append(value)
        if state.limit < .max && state.pending.count > state.limit + 1 {
          state.pending.remove(at: 0)
        }
      }
    } else {
      terminate()
    }
  }

  private func terminate(error: __owned Failure? = nil) {
    state.onTermination = nil
    if state.terminal == nil {
      if let failure = error {
        state.terminal = .failed(failure)
      } else {
        state.terminal = .finished
      }
    }
  }

  func yield(_ value: __owned Element?) {
    lock()
    enqueue(value)

    if let raw = state.continuation {
      if state.pending.count > 0 {
        state.continuation = nil
        let continuation =
          unsafeBitCast(raw, to: UnsafeContinuation<Element?, Error>.self)
        let toSend = state.pending.first
        state.pending.remove(at: 0)
        unlock()
        continuation.resume(returning: toSend)
      } else if let terminal = state.terminal {
        state.continuation = nil
        let continuation =
          unsafeBitCast(raw, to: UnsafeContinuation<Element?, Error>.self)
        withExtendedLifetime((state.onTermination, state.terminal)) {
          terminate()
          unlock()
        }
        switch terminal {
        case .finished:
          continuation.resume(returning: nil)
        case .failed(let error):
          continuation.resume(throwing: error)
        }
      } else {
        unlock()
      }
    } else {
      if state.limit == 0 {
        state.pending.remove(at: 0)
      }
      unlock()
    }
  }

  func yield(_ value: __owned Element?) where Failure == Never {
    lock()
    enqueue(value)

    if let raw = state.continuation {
      if state.pending.count > 0 {
        state.continuation = nil
        let continuation =
          unsafeBitCast(raw, to: UnsafeContinuation<Element?, Never>.self)
        let toSend = state.pending.first
        state.pending.remove(at: 0)
        unlock()
        continuation.resume(returning: toSend)
      } else if let terminal = state.terminal {
        state.continuation = nil
        let continuation =
          unsafeBitCast(raw, to: UnsafeContinuation<Element?, Never>.self)
        withExtendedLifetime((state.onTermination, state.terminal)) {
          terminate()
          unlock()
        }
        switch terminal {
        case .finished:
          continuation.resume(returning: nil)
        case .failed(let error):
          continuation.resume(throwing: error)
        }
      } else {
        unlock()
      }
    } else {
      if state.limit == 0 {
        state.pending.remove(at: 0)
      }
      unlock()
    }
  }

  func yield(throwing error: __owned Failure) {
    lock()
    if let raw = state.continuation {
      state.continuation = nil
      let terminal = state.terminal
      let continuation =
        unsafeBitCast(raw, to: UnsafeContinuation<Element?, Error>.self)
      withExtendedLifetime((state.onTermination, state.terminal)) {
        terminate()
        unlock()
      }
      switch terminal {
      case .finished:
        continuation.resume(returning: nil)
      case .failed(let error):
        continuation.resume(throwing: error)
      case .none:
        continuation.resume(throwing: error)
      }
      
    } else {
      withExtendedLifetime((state.onTermination, state.terminal)) {
        terminate(error: error)
        unlock()
      }
    }
  }

  func next(_ continuation: UnsafeContinuation<Element?, Never>) {
    let raw =
      unsafeBitCast(continuation, to: Builtin.RawUnsafeContinuation.self)

    lock()
    if state.continuation == nil {
      if state.pending.count > 0 {
        let toSend = state.pending.first
        state.pending.remove(at: 0)
        unlock()
        continuation.resume(returning: toSend)
      } else if let terminal = state.terminal {
        withExtendedLifetime((state.onTermination, state.terminal)) {
          terminate()
          unlock()
        }
        switch terminal {
        case .finished:
          continuation.resume(returning: nil)
        default:
          fatalError("Internal inconsistency, expecting non throwing continuation but found an error")
        }
      } else {
        state.continuation = raw
        unlock()
      }
    } else {
      unlock()
      fatalError("attempt to await next() on more than one task")
    }
  }
  
  func next() async -> Element? where Failure == Never {
    await withTaskCancellationHandler { [cancel] in
      cancel()
    } operation: {
      await withUnsafeContinuation { 
        next($0)
      }
    }
  }

  func next(_ continuation: UnsafeContinuation<Element?, Error>) {
    let raw =
      unsafeBitCast(continuation, to: Builtin.RawUnsafeContinuation.self)

    lock()
    if state.continuation == nil {
      if state.pending.count > 0 {
        let toSend = state.pending.first
        state.pending.remove(at: 0)
        unlock()
        continuation.resume(returning: toSend)
      } else if let terminal = state.terminal {
        withExtendedLifetime((state.onTermination, state.terminal)) {
          terminate()
          unlock()
        }
        switch terminal {
        case .finished:
          continuation.resume(returning: nil)
        case .failed(let error):
          continuation.resume(throwing: error)
        }
      } else {
        state.continuation = raw
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

  static func create(limit: Int) -> _AsyncStreamBufferedStorage<Element, Failure> {
    let minimumCapacity = _lockWordCount()
    let storage = Builtin.allocWithTailElems_1(
        _AsyncStreamBufferedStorage<Element, Failure>.self,
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
