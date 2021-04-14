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

@_silgen_name("swift_yielding_continuation_lock_size")
func _lockWordCount() -> Int

@_silgen_name("swift_yielding_continuation_lock_init")
func _lockInit(_ ptr: UnsafeRawPointer)

@_silgen_name("swift_yielding_continuation_lock_lock")
func _lock(_ ptr: UnsafeRawPointer)

@_silgen_name("swift_yielding_continuation_lock_unlock")
func _unlock(_ ptr: UnsafeRawPointer)

@available(SwiftStdlib 5.5, *)
public struct YieldingContinuation<Element, Failure: Error>: Sendable {
  internal final class Storage: UnsafeSendable {
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
    }
    // Stored as a singular structured assignment for initialization
    var state: State
    
    private init(_doNotCallMe: ()) {
      fatalError()
    }

    func lock() {
      let ptr = 
        UnsafeRawPointer(Builtin.projectTailElems(self, UnsafeRawPointer.self))
      _lock(ptr)
    }

    func unlock() {
      let ptr = 
        UnsafeRawPointer(Builtin.projectTailElems(self, UnsafeRawPointer.self))
      _unlock(ptr)
    }

    func yield(_ value: __owned Element?) {
      lock()
      if let value = value {
        if state.terminal == nil {
          state.pending.append(value)
        }
      } else {
        state.terminal = .finished
      }

      if let raw = state.continuation {
        if state.pending.count > 0 {
          state.continuation = nil
          let continuation =
            unsafeBitCast(raw, to: UnsafeContinuation<Element?, Error>.self)
          let toSend = state.pending.removeFirst()
          unlock()
          continuation.resume(returning: toSend)
        } else if let terminal = state.terminal {
          state.continuation = nil
          let continuation =
            unsafeBitCast(raw, to: UnsafeContinuation<Element?, Error>.self)
          state .terminal = .finished
          unlock()
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
        unlock()
      }
    }
    
    func yield(_ value: __owned Element?) where Failure == Never {
      lock()
      if let value = value {
        if state.terminal == nil {
          state.pending.append(value)
        }
      } else {
        state.terminal = .finished
      }

      if let raw = state.continuation {
        if state.pending.count > 0 {
          state.continuation = nil
          let continuation =
            unsafeBitCast(raw, to: UnsafeContinuation<Element?, Never>.self)
          let toSend = state.pending.removeFirst()
          unlock()
          continuation.resume(returning: toSend)
        } else if let terminal = state.terminal {
          state.continuation = nil
          let continuation =
            unsafeBitCast(raw, to: UnsafeContinuation<Element?, Never>.self)
          state .terminal = .finished
          unlock()
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
        unlock()
      }
    }
    
    func yield(throwing error: __owned Failure) {
      lock()
      if let raw = state.continuation {
        state.terminal = .finished
        state.continuation = nil
        let continuation =
          unsafeBitCast(raw, to: UnsafeContinuation<Element?, Error>.self)
        unlock()
        continuation.resume(throwing: error)
      } else {
        state.terminal = .failed(error)
        unlock()
      }
    }
    
    func next(_ continuation: UnsafeContinuation<Element?, Never>) {
      lock()
      let raw = 
        unsafeBitCast(continuation, to: Builtin.RawUnsafeContinuation.self)
      if state.continuation == nil {
        if state.pending.count > 0 {
          let toSend = state.pending.removeFirst()
          unlock()
          continuation.resume(returning: toSend)
        } else if let terminal = state.terminal {
          state .terminal = .finished
          unlock()
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
    
    func next(_ continuation: UnsafeContinuation<Element?, Error>) {
      lock()
      let raw = 
        unsafeBitCast(continuation, to: Builtin.RawUnsafeContinuation.self)
      if state.continuation == nil {
        if state.pending.count > 0 {
          let toSend = state.pending.removeFirst()
          unlock()
          continuation.resume(returning: toSend)
        } else if let terminal = state.terminal {
          state .terminal = .finished
          unlock()
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
    
    static func create() -> Storage {
      let minimumCapacity = _lockWordCount()
      let storage = Builtin.allocWithTailElems_1(
          Storage.self,
          minimumCapacity._builtinWordValue,
          UnsafeRawPointer.self
      )
      
      let state = 
        UnsafeMutablePointer<Storage.State>(Builtin.addressof(&storage.state))
      state.initialize(to: State())
      let ptr = UnsafeRawPointer(
        Builtin.projectTailElems(storage, UnsafeRawPointer.self))
      _lockInit(ptr)
      return storage
    }
  }
  
  let storage = Storage.create()

  /// Construct a YieldingContinuation.
  ///
  /// This continuation type can be called more than once, unlike the unsafe and
  /// checked counterparts. Each call to the yielding functions will resume any
  /// awaiter on the next function. This type is inherently sendable and can
  /// safely be used and stored in multiple task contexts.
  public init() { }

  /// Construct a YieldingContinuation with specific types including a failure.
  ///
  /// This continuation type can be called more than once, unlike the unsafe and
  /// checked counterparts. Each call to the yielding functions will resume any
  /// awaiter on the next function. This type is inherently sendable and can
  /// safely be used and stored in multiple task contexts.
  public init(yielding: Element.Type, throwing: Failure.Type) { }
  
  /// Resume the task awaiting next by having it return normally from its
  /// suspension point.
  ///
  /// - Parameter value: The value to return from an awaiting call to next.
  ///
  /// Unlike other continuations `YieldingContinuation` may resume more than
  /// once. However if there are no potential awaiting calls to `next` this
  /// function will return false, indicating that the caller needs to decide how
  /// the behavior should be handled.
  public func yield(_ value: __owned Element?) {
    storage.yield(value)
  }
  
  public func yield(_ value: __owned Element?) where Failure == Never {
    storage.yield(value)
  }
  
  /// Resume the task awaiting the continuation by having it throw an error
  /// from its suspension point.
  ///
  /// - Parameter error: The error to throw from an awaiting call to next.
  ///
  /// Unlike other continuations `YieldingContinuation` may resume more than
  /// once. However if there are no potential awaiting calls to `next` this
  /// function will return false, indicating that the caller needs to decide how
  /// the behavior should be handled.
  public func yield(throwing error: __owned Failure) {
    storage.yield(throwing: error)
  }
}

@available(SwiftStdlib 5.5, *)
extension YieldingContinuation where Failure == Error {
  /// Await a resume from a call to a yielding function.
  ///
  /// - Return: The element that was yielded or a error that was thrown.
  ///
  /// When multiple calls are awaiting a produced value from next any call to
  /// yield will resume all awaiting calls to next with that value.
  public func next() async throws -> Element? {
    return try await withUnsafeThrowingContinuation {
      (continuation: UnsafeContinuation<Element?, Error>) in
      storage.next(continuation)
    }
  }
}

@available(SwiftStdlib 5.5, *)
extension YieldingContinuation where Failure == Never {
  /// Construct a YieldingContinuation with a specific Element type.
  ///
  /// This continuation type can be called more than once, unlike the unsafe and
  /// checked counterparts. Each call to the yielding functions will resume any
  /// awaiter on the next function. This type is inherently sendable and can
  /// safely be used and stored in multiple task contexts.
  public init(yielding: Element.Type) { }

  /// Await a resume from a call to a yielding function.
  ///
  /// - Return: The element that was yielded.
  public func next() async -> Element? {
    return await withUnsafeContinuation {
      (continuation: UnsafeContinuation<Element?, Never>) in
      storage.next(continuation)
    }
  }
}

@available(SwiftStdlib 5.5, *)
extension YieldingContinuation {
  /// Resume the task awaiting the continuation by having it either
  /// return normally or throw an error based on the state of the given
  /// `Result` value.
  ///
  /// - Parameter result: A value to either return or throw from the
  ///   continuation.
  ///
  /// Unlike other continuations `YieldingContinuation` may resume more than
  /// once. However if there are no potential awaiting calls to `next` this
  /// function will return false, indicating that the caller needs to decide how
  /// the behavior should be handled.
  public func yield<Er: Error>(
    with result: Result<Element, Er>
  ) where Failure == Error {
    switch result {
      case .success(let val):
        self.yield(val)
      case .failure(let err):
        self.yield(throwing: err)
    }
  }
  
  /// Resume the task awaiting the continuation by having it either
  /// return normally or throw an error based on the state of the given
  /// `Result` value.
  ///
  /// - Parameter result: A value to either return or throw from the
  ///   continuation.
  ///
  /// Unlike other continuations `YieldingContinuation` may resume more than
  /// once. However if there are no potential awaiting calls to `next` this
  /// function will return false, indicating that the caller needs to decide how
  /// the behavior should be handled.
  public func yield(with result: Result<Element, Failure>) {
    switch result {
      case .success(let val):
        self.yield(val)
      case .failure(let err):
        self.yield(throwing: err)
    }
  }
  
  /// Resume the task awaiting the continuation by having it return normally
  /// from its suspension point.
  ///
  /// Unlike other continuations `YieldingContinuation` may resume more than
  /// once. However if there are no potential awaiting calls to `next` this
  /// function will return false, indicating that the caller needs to decide how
  /// the behavior should be handled.
  public func yield() where Element == Void {
    self.yield(())
  }
}

