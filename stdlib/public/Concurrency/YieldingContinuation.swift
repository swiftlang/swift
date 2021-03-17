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

public struct YieldingContinuation<T, E: Error>: ConcurrentValue {
  @_fixed_layout
  @usableFromInline
  internal final class Storage: UnsafeConcurrentValue {
    @usableFromInline
    var continuation: UnsafeContinuation<T, Error>?
  }
  
  @usableFromInline
  let storage = Storage()

  public init(producing: T.Type, throwing: E.Type) { }

  @inlinable
  @inline(__always)
  internal func _extract() -> UnsafeContinuation<T, Error>? {
    let raw = Builtin.atomicrmw_xchg_seqcst_Word(
      Builtin.addressof(&storage.continuation), 
      UInt(bitPattern: 0)._builtinWordValue)
    return unsafeBitCast(raw, to: UnsafeContinuation<T, Error>?.self)
  }
  
  @inlinable
  @inline(__always)
  internal func _inject(
    _ continuation: UnsafeContinuation<T, Error>
  ) -> UnsafeContinuation<T, Error>? {
    let rawContinuation = unsafeBitCast(continuation, to: Builtin.Word.self)
    let raw = Builtin.atomicrmw_xchg_seqcst_Word(
      Builtin.addressof(&storage.continuation), rawContinuation)
    return unsafeBitCast(raw, to: UnsafeContinuation<T, Error>?.self)
  }
  
  /// Resume the task awaiting the continuation by having it return normally
  /// from its suspension point.
  ///
  /// - Parameter value: The value to return from the continuation.
  ///
  /// Unlike other continuations `YieldingContinuation` may resume more than 
  /// once. However if there are no potential awaiting calls to `next` this 
  /// function will return false, indicating that the caller needs to decide how
  /// the behavior should be handled.
  ///
  /// After `resume` enqueues the task, control is immediately returned to
  /// the caller. The task will continue executing when its executor is
  /// able to reschedule it.
  public func resume(yielding value: __owned T) -> Bool {
    if let continuation = _extract() {
      continuation.resume(returning: value)
      _fixLifetime(storage)
      return true
    }
    return false
  }
  
  /// Resume the task awaiting the continuation by having it throw an error
  /// from its suspension point.
  ///
  /// - Parameter error: The error to throw from the continuation.
  ///
  /// Unlike other continuations `YieldingContinuation` may resume more than 
  /// once. However if there are no potential awaiting calls to `next` this 
  /// function will return false, indicating that the caller needs to decide how
  /// the behavior should be handled.
  ///
  /// After `resume` enqueues the task, control is immediately returned to
  /// the caller. The task will continue executing when its executor is
  /// able to reschedule it.
  public func resume(throwing error: __owned E) -> Bool {
    if let continuation = _extract() {
      continuation.resume(throwing: error)
      _fixLifetime(storage)
      return true
    }
    return false
  }

  public func next() async throws -> T {
    var existing: UnsafeContinuation<T, Error>?
    do {
      let result = try await withUnsafeThrowingContinuation { 
        (continuation: UnsafeContinuation<T, Error>) in
        existing = _inject(continuation)
      }
      existing?.resume(returning: result)
      _fixLifetime(storage)
      return result
    } catch {
      existing?.resume(throwing: error)
      _fixLifetime(storage)
      throw error
    }
  }
}

extension YieldingContinuation where E == Never {
  public init(producing: T.Type) { }

  public func next() async -> T {
    var existing: UnsafeContinuation<T, Error>?
    let result = try! await withUnsafeThrowingContinuation { 
      (continuation: UnsafeContinuation<T, Error>) in
      existing = _inject(continuation)
    }
    existing?.resume(returning: result)
    _fixLifetime(storage)
    return result
  }
}

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
  ///
  /// After `resume` enqueues the task, control is immediately returned to
  /// the caller. The task will continue executing when its executor is
  /// able to reschedule it.
  public func resume<Er: Error>(
    with result: Result<T, Er>
  ) -> Bool where E == Error {
    switch result {
      case .success(let val):
        return self.resume(yielding: val)
      case .failure(let err):
        return self.resume(throwing: err)
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
  ///
  /// After `resume` enqueues the task, control is immediately returned to
  /// the caller. The task will continue executing when its executor is
  /// able to reschedule it.
  public func resume(with result: Result<T, E>) -> Bool {
    switch result {
      case .success(let val):
        return self.resume(yielding: val)
      case .failure(let err):
        return self.resume(throwing: err)
    }
  }
  
  /// Resume the task awaiting the continuation by having it return normally
  /// from its suspension point.
  ///
  /// Unlike other continuations `YieldingContinuation` may resume more than 
  /// once. However if there are no potential awaiting calls to `next` this 
  /// function will return false, indicating that the caller needs to decide how
  /// the behavior should be handled.
  ///
  /// After `resume` enqueues the task, control is immediately returned to
  /// the caller. The task will continue executing when its executor is
  /// able to reschedule it.
  public func resume() -> Bool where T == Void {
    return self.resume(yielding: ())
  }
}
