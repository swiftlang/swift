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

internal final class _YieldingContinuationStorage: UnsafeSendable {
  var continuation: Builtin.RawUnsafeContinuation?
}

@available(SwiftStdlib 5.5, *)
public struct YieldingContinuation<Element, Failure: Error>: Sendable {
  let storage = _YieldingContinuationStorage()

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

  internal func _extract() -> UnsafeContinuation<Element, Error>? {
    let raw = Builtin.atomicrmw_xchg_acqrel_Word(
      Builtin.addressof(&storage.continuation),
      UInt(bitPattern: 0)._builtinWordValue)
    return unsafeBitCast(raw, to: UnsafeContinuation<Element, Error>?.self)
  }

  internal func _inject(
    _ continuation: UnsafeContinuation<Element, Error>
  ) -> UnsafeContinuation<Element, Error>? {
    let rawContinuation = unsafeBitCast(continuation, to: Builtin.Word.self)
    let raw = Builtin.atomicrmw_xchg_acqrel_Word(
      Builtin.addressof(&storage.continuation), rawContinuation)
    return unsafeBitCast(raw, to: UnsafeContinuation<Element, Error>?.self)
  }
  
  /// Resume the task awaiting next by having it return normally from its 
  /// suspension point.
  ///
  /// - Parameter value: The value to return from an awaiting call to next.
  ///
  /// Unlike other continuations `YieldingContinuation` may resume more than
  /// once. However if there are no potential awaiting calls to `next` this
  /// function will return false, indicating that the caller needs to decide how
  /// the behavior should be handled.
  public func yield(_ value: __owned Element) -> Bool {
    if let continuation = _extract() {
      continuation.resume(returning: value)
      return true
    }
    return false
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
  public func yield(throwing error: __owned Failure) -> Bool {
    if let continuation = _extract() {
      continuation.resume(throwing: error)
      return true
    }
    return false
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
  public func next() async throws -> Element {
    var existing: UnsafeContinuation<Element, Error>?
    do {
      let result = try await withUnsafeThrowingContinuation {
        (continuation: UnsafeContinuation<Element, Error>) in
        existing = _inject(continuation)
      }
      existing?.resume(returning: result)
      return result
    } catch {
      existing?.resume(throwing: error)
      throw error
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
  public func next() async -> Element {
    var existing: UnsafeContinuation<Element, Error>?
    let result = try! await withUnsafeThrowingContinuation {
      (continuation: UnsafeContinuation<Element, Error>) in
      existing = _inject(continuation)
    }
    existing?.resume(returning: result)
    return result
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
  ) -> Bool where Failure == Error {
    switch result {
      case .success(let val):
        return self.yield(val)
      case .failure(let err):
        return self.yield(throwing: err)
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
  public func yield(with result: Result<Element, Failure>) -> Bool {
    switch result {
      case .success(let val):
        return self.yield(val)
      case .failure(let err):
        return self.yield(throwing: err)
    }
  }
  
  /// Resume the task awaiting the continuation by having it return normally
  /// from its suspension point.
  ///
  /// Unlike other continuations `YieldingContinuation` may resume more than
  /// once. However if there are no potential awaiting calls to `next` this
  /// function will return false, indicating that the caller needs to decide how
  /// the behavior should be handled.
  public func yield() -> Bool where Element == Void {
    return self.yield(())
  }
}

