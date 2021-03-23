
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

@_silgen_name("swift_continuation_exchange")
func exchangeContinuation(_ task: Builtin.NativeObject, _ continuation: Builtin.RawUnsafeContinuation?) -> Builtin.RawUnsafeContinuation?

public func withYieldingContinuation<T>(
    _ body: (YieldingContinuation<T, Never>) -> Void
) async -> YieldingContinuation<T, Never>.Handle {
  let task = Builtin.getCurrentAsyncTask()
  let continuation =
    YieldingContinuation(producing: T.self, throwing: Never.self, task: task)
  body(continuation)
  return YieldingContinuation<T, Never>.Handle(task: task)
}

public func withYieldingThrowingContinuation<T>(
    _ body: (YieldingContinuation<T, Error>) -> Void
) async -> YieldingContinuation<T, Error>.Handle {
  let task = Builtin.getCurrentAsyncTask()
  let continuation =
    YieldingContinuation(producing: T.self, throwing: Error.self, task: task)
  body(continuation)
  return YieldingContinuation<T, Error>.Handle(task: task)
}

public struct YieldingContinuation<T, E: Error>: Sendable {
  typealias Continuation = UnsafeContinuation<T, Never>
  typealias ThrowingContinuation = UnsafeContinuation<T, Error>
  
  public struct Handle {
    let task: Builtin.NativeObject
    
    public mutating func get() async throws -> T {
      var other: ThrowingContinuation?
      do {
        let result: T = try await withUnsafeThrowingContinuation {
          let continuation = unsafeBitCast($0, to: Builtin.RawUnsafeContinuation.self)
          let existing = exchangeContinuation(task, continuation)
          other = unsafeBitCast(existing, to: ThrowingContinuation?.self)
        }
        other?.resume(returning: result)
        return result
      } catch {
        other?.resume(throwing: error)
        throw error
      }
    }
    
    public mutating func get() async -> T where E == Never {
      var other: Continuation?
      let result: T = await withUnsafeContinuation {
        let continuation = unsafeBitCast($0, to: Builtin.RawUnsafeContinuation.self)
        let existing = exchangeContinuation(task, continuation)
        other = unsafeBitCast(existing, to: Continuation?.self)
      }
      other?.resume(returning: result)
      return result
    }
  }
  
  let task: Builtin.NativeObject
  
  init(producing: T.Type, throwing: E.Type, task: Builtin.NativeObject) {
    self.task = task
  }

  public func resume(yielding value: __owned T) -> Bool where E == Never {
    if let continuation = unsafeBitCast(
      exchangeContinuation(task, nil),
      to: Continuation?.self) {
      continuation.resume(returning: value)
      return true
    }
    return false
  }
  
  public func resume(yielding value: __owned T) -> Bool {
    if let continuation = unsafeBitCast(
      exchangeContinuation(task, nil),
      to: ThrowingContinuation?.self) {
      continuation.resume(returning: value)
      return true
    }
    return false
  }
  
  public func resume(throwing error: __owned E) -> Bool {
    if let continuation = unsafeBitCast(
      exchangeContinuation(task, nil),
      to: ThrowingContinuation?.self) {
      continuation.resume(throwing: error)
      return true
    }
    return false
  }
}

extension YieldingContinuation {
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

  public func resume(with result: Result<T, E>) -> Bool {
    switch result {
      case .success(let val):
        return self.resume(yielding: val)
      case .failure(let err):
        return self.resume(throwing: err)
    }
  }

  public func resume() -> Bool where T == Void {
    return self.resume(yielding: ())
  }
}