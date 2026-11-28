//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Swift

@available(SwiftStdlib 5.1, *)
extension Span where Element: ~Copyable {
  /// Calls the given closure with a pointer to the underlying elements of the
  /// viewed contiguous storage.
  ///
  /// The buffer pointer passed as an argument to `body` is valid until
  /// `body` returns, including across the suspension points of `body`. Do
  /// not escape it from the closure for later use.
  ///
  /// - Parameter body: A closure with an `UnsafeBufferPointer` parameter
  ///   that points to the viewed contiguous storage. The closure runs in the
  ///   isolation of the caller.
  /// - Returns: The return value of the `body` closure parameter.
  @export(implementation)
  @safe
  public nonisolated(nonsending) func withUnsafeBufferPointer<
    Return: ~Copyable, Failure: Error
  >(
    _ body: nonisolated(nonsending) (
      _ buffer: UnsafeBufferPointer<Element>
    ) async throws(Failure) -> Return
  ) async throws(Failure) -> Return {
    let bytes = unsafe UnsafeRawBufferPointer(
      start: _unsafePointer, count: count &* MemoryLayout<Element>.stride
    )
    return try await unsafe bytes._withMemoryRebound(to: Element.self) {
      buffer async throws(Failure) -> Return in
      try await unsafe body(buffer)
    }
  }
}

@available(SwiftStdlib 5.1, *)
extension Span where Element: BitwiseCopyable {
  /// Calls the given closure with a pointer to the underlying bytes of the
  /// viewed contiguous storage.
  ///
  /// The buffer pointer passed as an argument to `body` is valid until
  /// `body` returns, including across the suspension points of `body`. Do
  /// not escape it from the closure for later use.
  ///
  /// - Parameter body: A closure with an `UnsafeRawBufferPointer` parameter
  ///   that points to the viewed contiguous storage. The closure runs in the
  ///   isolation of the caller.
  /// - Returns: The return value of the `body` closure parameter.
  @export(implementation)
  @safe
  public nonisolated(nonsending) func withUnsafeBytes<
    Return: ~Copyable, Failure: Error
  >(
    _ body: nonisolated(nonsending) (
      _ buffer: UnsafeRawBufferPointer
    ) async throws(Failure) -> Return
  ) async throws(Failure) -> Return {
    let bytes = unsafe UnsafeRawBufferPointer(
      start: _unsafePointer, count: count &* MemoryLayout<Element>.stride
    )
    return try await unsafe body(bytes)
  }
}

@available(SwiftStdlib 5.1, *)
extension MutableSpan where Element: ~Copyable {
  /// Calls the given closure with a pointer to the underlying elements of the
  /// viewed contiguous storage.
  ///
  /// The buffer pointer passed as an argument to `body` is valid until
  /// `body` returns, including across the suspension points of `body`. Do
  /// not escape it from the closure for later use.
  ///
  /// - Parameter body: A closure with an `UnsafeBufferPointer` parameter
  ///   that points to the viewed contiguous storage. The closure runs in the
  ///   isolation of the caller.
  /// - Returns: The return value of the `body` closure parameter.
  @export(implementation)
  @safe
  public nonisolated(nonsending) func withUnsafeBufferPointer<
    Return: ~Copyable, Failure: Error
  >(
    _ body: nonisolated(nonsending) (
      _ buffer: UnsafeBufferPointer<Element>
    ) async throws(Failure) -> Return
  ) async throws(Failure) -> Return {
    try await Span(_mutableSpan: self).withUnsafeBufferPointer(body)
  }

  /// Calls the given closure with a pointer to the underlying elements of the
  /// viewed contiguous storage.
  ///
  /// The buffer pointer passed as an argument to `body` is valid until
  /// `body` returns, including across the suspension points of `body`. Do
  /// not escape it from the closure for later use.
  ///
  /// - Parameter body: A closure with an `UnsafeMutableBufferPointer`
  ///   parameter that points to the viewed contiguous storage. The closure
  ///   runs in the isolation of the caller.
  /// - Returns: The return value of the `body` closure parameter.
  @export(implementation)
  @_lifetime(self: copy self)
  @safe
  public nonisolated(nonsending) mutating func withUnsafeMutableBufferPointer<
    Return: ~Copyable, Failure: Error
  >(
    _ body: nonisolated(nonsending) (
      _ buffer: UnsafeMutableBufferPointer<Element>
    ) async throws(Failure) -> Return
  ) async throws(Failure) -> Return {
    let bytes = unsafe UnsafeMutableRawBufferPointer(
      start: _unsafePointer, count: count &* MemoryLayout<Element>.stride
    )
    return try await unsafe bytes._withMemoryRebound(to: Element.self) {
      buffer async throws(Failure) -> Return in
      try await unsafe body(buffer)
    }
  }
}

@available(SwiftStdlib 5.1, *)
extension MutableSpan where Element: BitwiseCopyable {
  /// Calls the given closure with a pointer to the underlying bytes of the
  /// viewed contiguous storage.
  ///
  /// The buffer pointer passed as an argument to `body` is valid until
  /// `body` returns, including across the suspension points of `body`. Do
  /// not escape it from the closure for later use.
  ///
  /// - Parameter body: A closure with an `UnsafeRawBufferPointer` parameter
  ///   that points to the viewed contiguous storage. The closure runs in the
  ///   isolation of the caller.
  /// - Returns: The return value of the `body` closure parameter.
  @export(implementation)
  @safe
  public nonisolated(nonsending) func withUnsafeBytes<
    Return: ~Copyable, Failure: Error
  >(
    _ body: nonisolated(nonsending) (
      _ buffer: UnsafeRawBufferPointer
    ) async throws(Failure) -> Return
  ) async throws(Failure) -> Return {
    try await unsafe RawSpan(_mutableSpan: self).withUnsafeBytes(body)
  }

  /// Calls the given closure with a pointer to the underlying bytes of the
  /// viewed contiguous storage.
  ///
  /// The buffer pointer passed as an argument to `body` is valid until
  /// `body` returns, including across the suspension points of `body`. Do
  /// not escape it from the closure for later use.
  ///
  /// - Parameter body: A closure with an `UnsafeMutableRawBufferPointer`
  ///   parameter that points to the viewed contiguous storage. The closure
  ///   runs in the isolation of the caller.
  /// - Returns: The return value of the `body` closure parameter.
  @export(implementation)
  @_lifetime(self: copy self)
  @safe
  public nonisolated(nonsending) mutating func withUnsafeMutableBytes<
    Return: ~Copyable, Failure: Error
  >(
    _ body: nonisolated(nonsending) (
      _ buffer: UnsafeMutableRawBufferPointer
    ) async throws(Failure) -> Return
  ) async throws(Failure) -> Return {
    let bytes = unsafe UnsafeMutableRawBufferPointer(
      start: _unsafePointer, count: count &* MemoryLayout<Element>.stride
    )
    return try await unsafe body(bytes)
  }
}

@available(SwiftStdlib 5.1, *)
extension OutputSpan where Element: ~Copyable {
  /// Calls the given closure with a pointer to the memory of the span and a
  /// reference to its initialized count.
  ///
  /// The buffer pointer passed as an argument to `body` is valid until
  /// `body` returns, including across the suspension points of `body`. Do
  /// not escape it from the closure for later use.
  ///
  /// The `body` closure must update the initialized count to reflect the
  /// elements that it initialized or deinitialized. The elements up to the
  /// initialized count must be initialized when `body` returns.
  ///
  /// - Parameter body: A closure that can read from and write to the
  ///   buffer and update the initialized count. The closure runs in the
  ///   isolation of the caller.
  /// - Returns: The return value of the `body` closure parameter.
  @export(implementation)
  @_lifetime(self: copy self)
  @unsafe
  public nonisolated(nonsending) mutating func withUnsafeMutableBufferPointer<
    Return: ~Copyable, Failure: Error
  >(
    _ body: nonisolated(nonsending) (
      UnsafeMutableBufferPointer<Element>,
      _ initializedCount: inout Int
    ) async throws(Failure) -> Return
  ) async throws(Failure) -> Return {
    let bytes = unsafe UnsafeMutableRawBufferPointer(
      start: _unsafePointer, count: capacity &* MemoryLayout<Element>.stride
    )
    var initializedCount = count
    defer {
      unsafe _unsafeSetInitializedCount(initializedCount)
    }
    return try await unsafe bytes._withMemoryRebound(to: Element.self) {
      buffer async throws(Failure) -> Return in
      try await unsafe body(buffer, &initializedCount)
    }
  }
}

@available(SwiftStdlib 5.1, *)
extension RawSpan {
  /// Calls the given closure with a pointer to the underlying bytes of the
  /// viewed contiguous storage.
  ///
  /// The buffer pointer passed as an argument to `body` is valid until
  /// `body` returns, including across the suspension points of `body`. Do
  /// not escape it from the closure for later use.
  ///
  /// - Parameter body: A closure with an `UnsafeRawBufferPointer` parameter
  ///   that points to the viewed contiguous storage. The closure runs in the
  ///   isolation of the caller.
  /// - Returns: The return value of the `body` closure parameter.
  @export(implementation)
  @safe
  public nonisolated(nonsending) func withUnsafeBytes<
    Return: ~Copyable, Failure: Error
  >(
    _ body: nonisolated(nonsending) (
      _ buffer: UnsafeRawBufferPointer
    ) async throws(Failure) -> Return
  ) async throws(Failure) -> Return {
    try await unsafe body(.init(start: _unsafePointer, count: byteCount))
  }
}

@available(SwiftStdlib 5.1, *)
extension MutableRawSpan {
  /// Calls the given closure with a pointer to the underlying bytes of the
  /// viewed contiguous storage.
  ///
  /// The buffer pointer passed as an argument to `body` is valid until
  /// `body` returns, including across the suspension points of `body`. Do
  /// not escape it from the closure for later use.
  ///
  /// - Parameter body: A closure with an `UnsafeRawBufferPointer` parameter
  ///   that points to the viewed contiguous storage. The closure runs in the
  ///   isolation of the caller.
  /// - Returns: The return value of the `body` closure parameter.
  @export(implementation)
  @safe
  public nonisolated(nonsending) func withUnsafeBytes<
    Return: ~Copyable, Failure: Error
  >(
    _ body: nonisolated(nonsending) (
      _ buffer: UnsafeRawBufferPointer
    ) async throws(Failure) -> Return
  ) async throws(Failure) -> Return {
    try await unsafe body(.init(start: _unsafePointer, count: byteCount))
  }

  /// Calls the given closure with a pointer to the underlying bytes of the
  /// viewed contiguous storage.
  ///
  /// The buffer pointer passed as an argument to `body` is valid until
  /// `body` returns, including across the suspension points of `body`. Do
  /// not escape it from the closure for later use.
  ///
  /// - Parameter body: A closure with an `UnsafeMutableRawBufferPointer`
  ///   parameter that points to the viewed contiguous storage. The closure
  ///   runs in the isolation of the caller.
  /// - Returns: The return value of the `body` closure parameter.
  @export(implementation)
  @_lifetime(self: copy self)
  @safe
  public nonisolated(nonsending) mutating func withUnsafeMutableBytes<
    Return: ~Copyable, Failure: Error
  >(
    _ body: nonisolated(nonsending) (
      _ buffer: UnsafeMutableRawBufferPointer
    ) async throws(Failure) -> Return
  ) async throws(Failure) -> Return {
    try await unsafe body(.init(start: _unsafePointer, count: byteCount))
  }
}

@available(SwiftStdlib 5.1, *)
extension OutputRawSpan {
  /// Calls the given closure with a pointer to the memory of the span and a
  /// reference to its initialized count.
  ///
  /// The buffer pointer passed as an argument to `body` is valid until
  /// `body` returns, including across the suspension points of `body`. Do
  /// not escape it from the closure for later use.
  ///
  /// The `body` closure must update the initialized count to reflect the
  /// bytes that it initialized or deinitialized. The bytes up to the
  /// initialized count must be initialized when `body` returns.
  ///
  /// - Parameter body: A closure that can read from and write to the
  ///   buffer and update the initialized count. The closure runs in the
  ///   isolation of the caller.
  /// - Returns: The return value of the `body` closure parameter.
  @export(implementation)
  @_lifetime(self: copy self)
  @unsafe
  public nonisolated(nonsending) mutating func withUnsafeMutableBytes<
    Return: ~Copyable, Failure: Error
  >(
    _ body: nonisolated(nonsending) (
      UnsafeMutableRawBufferPointer,
      _ initializedCount: inout Int
    ) async throws(Failure) -> Return
  ) async throws(Failure) -> Return {
    let bytes = unsafe UnsafeMutableRawBufferPointer(
      start: _unsafePointer, count: capacity
    )
    var initializedCount = byteCount
    defer {
      unsafe _unsafeSetInitializedCount(initializedCount)
    }
    return try await unsafe body(bytes, &initializedCount)
  }
}

@available(SwiftStdlib 5.1, *)
extension UnsafeRawBufferPointer {
  @export(implementation)
  internal nonisolated(nonsending) func _withMemoryRebound<
    T: ~Copyable, Return: ~Copyable, Failure: Error
  >(
    to type: T.Type,
    _ body: nonisolated(nonsending) (
      _ buffer: UnsafeBufferPointer<T>
    ) async throws(Failure) -> Return
  ) async throws(Failure) -> Return {
    // Unlike the synchronous variant, this one calls `body` in a single place.
    // Each `await` is a separate suspension point in the generated code.
    let s = baseAddress
    let n = count / MemoryLayout<T>.stride
    var binding = 0._builtinWordValue
    if let s = unsafe s {
      binding = Builtin.bindMemory(s._rawValue, n._builtinWordValue, T.self)
    }
    defer {
      if let s = unsafe s { Builtin.rebindMemory(s._rawValue, binding) }
    }
    return try await unsafe body(
      .init(start: unsafe s.map { unsafe .init($0._rawValue) }, count: n)
    )
  }
}

@available(SwiftStdlib 5.1, *)
extension UnsafeMutableRawBufferPointer {
  @export(implementation)
  internal nonisolated(nonsending) func _withMemoryRebound<
    T: ~Copyable, Return: ~Copyable, Failure: Error
  >(
    to type: T.Type,
    _ body: nonisolated(nonsending) (
      _ buffer: UnsafeMutableBufferPointer<T>
    ) async throws(Failure) -> Return
  ) async throws(Failure) -> Return {
    // Unlike the synchronous variant, this one calls `body` in a single place.
    // Each `await` is a separate suspension point in the generated code.
    let s = baseAddress
    let n = count / MemoryLayout<T>.stride
    var binding = 0._builtinWordValue
    if let s = unsafe s {
      binding = Builtin.bindMemory(s._rawValue, n._builtinWordValue, T.self)
    }
    defer {
      if let s = unsafe s { Builtin.rebindMemory(s._rawValue, binding) }
    }
    return try await unsafe body(
      .init(start: unsafe s.map { unsafe .init($0._rawValue) }, count: n)
    )
  }
}
