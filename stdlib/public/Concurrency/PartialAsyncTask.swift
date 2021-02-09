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
public struct UnsafeContinuation<T, E: Error>: _Continuation {
  @usableFromInline internal var context: Builtin.RawUnsafeContinuation

  @_alwaysEmitIntoClient
  internal init(_ context: Builtin.RawUnsafeContinuation) {
    self.context = context
  }

  @usableFromInline
  @_silgen_name("swift_continuation_resume")
  internal func _resume(returning value: __owned T)

  @_alwaysEmitIntoClient
  public func resume(returning value: __owned T) where E == Never {
    self._resume(returning: value)
  }

  @usableFromInline
  @_silgen_name("swift_continuation_throwingResume")
  internal func _resume(returningToThrowingFunction: __owned T)

  @_alwaysEmitIntoClient
  public func resume(returning value: __owned T) {
    self._resume(returningToThrowingFunction: value)
  }

  @usableFromInline
  @_silgen_name("swift_continuation_throwingResumeWithError")
  internal func _resume(throwing: __owned Error)

  @_alwaysEmitIntoClient
  public func resume(throwing error: __owned E) {
    self._resume(throwing: error)
  }
}

#if _runtime(_ObjC)

// Intrinsics used by SILGen to resume or fail continuations.
@_alwaysEmitIntoClient
internal func _resumeUnsafeContinuation<T>(
  _ continuation: UnsafeContinuation<T, Never>,
  _ value: __owned T
) {
  continuation.resume(returning: value)
}

@_alwaysEmitIntoClient
internal func _resumeUnsafeThrowingContinuation<T>(
  _ continuation: UnsafeContinuation<T, Error>,
  _ value: __owned T
) {
  continuation.resume(returning: value)
}

@_alwaysEmitIntoClient
internal func _resumeUnsafeThrowingContinuationWithError<T>(
  _ continuation: UnsafeContinuation<T, Error>,
  _ error: __owned Error
) {
  continuation.resume(throwing: error)
}

#endif

/// The operation functions must resume the continuation *exactly once*.
///
/// The continuation will not begin executing until the operation function returns.
@_alwaysEmitIntoClient
public func withUnsafeContinuation<T>(
  _ fn: (UnsafeContinuation<T, Never>) -> Void
) async -> T {
  return await Builtin.withUnsafeContinuation {
    fn(UnsafeContinuation<T, Never>($0))
  }
}

/// The operation functions must resume the continuation *exactly once*.
///
/// The continuation will not begin executing until the operation function returns.
@_alwaysEmitIntoClient
public func withUnsafeThrowingContinuation<T>(
  _ fn: (UnsafeContinuation<T, Error>) -> Void
) async throws -> T {
  return try await Builtin.withUnsafeThrowingContinuation {
    fn(UnsafeContinuation<T, Error>($0))
  }
}
