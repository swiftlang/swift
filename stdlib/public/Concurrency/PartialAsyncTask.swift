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
public struct PartialAsyncTask {
  private var context: UnsafeMutablePointer<_SwiftContext>

  public func run() { }
}

@frozen
public struct UnsafeContinuation<T> {
  private var context: UnsafeRawPointer

  public func resume(returning: __owned T) { }
}

@frozen
public struct UnsafeThrowingContinuation<T> {
  private var context: UnsafeRawPointer

  public func resume(returning: __owned T) { }
  public func resume(throwing: __owned Error) { }
}

#if _runtime(_ObjC)

// Intrinsics used by SILGen to resume or fail continuations
// for
@_alwaysEmitIntoClient
@usableFromInline
internal func _resumeUnsafeContinuation<T>(
  _ continuation: UnsafeContinuation<T>,
  _ value: __owned T
) {
  continuation.resume(returning: value)
}

@_alwaysEmitIntoClient
@usableFromInline
internal func _resumeUnsafeThrowingContinuation<T>(
  _ continuation: UnsafeThrowingContinuation<T>,
  _ value: __owned T
) {
  continuation.resume(returning: value)
}

@_alwaysEmitIntoClient
@usableFromInline
internal func _resumeUnsafeThrowingContinuationWithError<T>(
  _ continuation: UnsafeThrowingContinuation<T>,
  _ error: __owned Error
) {
  continuation.resume(throwing: error)
}

#endif
