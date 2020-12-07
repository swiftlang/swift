//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Swift
@_implementationOnly import _SwiftConcurrencyShims

/// A partial task is a unit of scheduleable work.
@frozen
public struct PartialAsyncTask {
  private var context: Builtin.Job

  public func run() { }
}

@frozen
public struct UnsafeContinuation<T> {
  @usableFromInline internal var context: Builtin.RawUnsafeContinuation

  public func resume(returning: __owned T) { }

  @_alwaysEmitIntoClient
  internal init(_ context: Builtin.RawUnsafeContinuation) {
    self.context = context
  }
}

@frozen
public struct UnsafeThrowingContinuation<T> {
  @usableFromInline internal var context: Builtin.RawUnsafeContinuation

  public func resume(returning: __owned T) { }
  public func resume(throwing: __owned Error) { }

  @_alwaysEmitIntoClient
  internal init(_ context: Builtin.RawUnsafeContinuation) {
    self.context = context
  }
}

#if _runtime(_ObjC)

// Intrinsics used by SILGen to resume or fail continuations
// for
@_alwaysEmitIntoClient
internal func _resumeUnsafeContinuation<T>(
  _ continuation: UnsafeContinuation<T>,
  _ value: __owned T
) {
  continuation.resume(returning: value)
}

@_alwaysEmitIntoClient
internal func _resumeUnsafeThrowingContinuation<T>(
  _ continuation: UnsafeThrowingContinuation<T>,
  _ value: __owned T
) {
  continuation.resume(returning: value)
}

@_alwaysEmitIntoClient
internal func _resumeUnsafeThrowingContinuationWithError<T>(
  _ continuation: UnsafeThrowingContinuation<T>,
  _ error: __owned Error
) {
  continuation.resume(throwing: error)
}

#endif

// Wrappers around unsafe continuation builtins
@_alwaysEmitIntoClient
public func withUnsafeContinuation<T>(
  _ fn: (UnsafeContinuation<T>) -> Void
) async -> T {
  return await Builtin.withUnsafeContinuation {
    fn(UnsafeContinuation<T>($0))
  }
}

@_alwaysEmitIntoClient
public func withUnsafeThrowingContinuation<T>(
  _ fn: (UnsafeThrowingContinuation<T>) -> Void
) async throws -> T {
  return await try Builtin.withUnsafeThrowingContinuation {
    fn(UnsafeThrowingContinuation<T>($0))
  }
}
