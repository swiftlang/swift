//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// Evaluates a closure while ensuring that the given instance is not destroyed
/// before the closure returns.
///
/// - Parameters:
///   - x: An instance to preserve until the execution of `body` is completed.
///   - body: A closure to execute that depends on the lifetime of `x` being
///     extended. If `body` has a return value, that value is also used as the
///     return value for the `withExtendedLifetime(_:_:)` method.
/// - Returns: The return value, if any, of the `body` closure parameter.
@_inlineable
public func withExtendedLifetime<T, Result>(
  _ x: T, _ body: () throws -> Result
) rethrows -> Result {
  defer { _fixLifetime(x) }
  return try body()
}

/// Evaluates a closure while ensuring that the given instance is not destroyed
/// before the closure returns.
///
/// - Parameters:
///   - x: An instance to preserve until the execution of `body` is completed.
///   - body: A closure to execute that depends on the lifetime of `x` being
///     extended. If `body` has a return value, that value is also used as the
///     return value for the `withExtendedLifetime(_:_:)` method.
/// - Returns: The return value, if any, of the `body` closure parameter.
@_inlineable
public func withExtendedLifetime<T, Result>(
  _ x: T, _ body: (T) throws -> Result
) rethrows -> Result {
  defer { _fixLifetime(x) }
  return try body(x)
}

extension String {

  /// Calls the given closure with a pointer to the contents of the string,
  /// represented as a null-terminated sequence of UTF-8 code units.
  ///
  /// The pointer passed as an argument to `body` is valid only during the
  /// execution of `withCString(_:)`. Do not store or return the pointer for
  /// later use.
  ///
  /// - Parameter body: A closure with a pointer parameter that points to a
  ///   null-terminated sequence of UTF-8 code units. If `body` has a return
  ///   value, that value is also used as the return value for the
  ///   `withCString(_:)` method. The pointer argument is valid only for the
  ///   duration of the method's execution.
  /// - Returns: The return value, if any, of the `body` closure parameter.
  @_inlineable
  public func withCString<Result>(
    _ body: (UnsafePointer<Int8>) throws -> Result
  ) rethrows -> Result {
    return try self.utf8CString.withUnsafeBufferPointer {
      try body($0.baseAddress!)
    }
  }
}

// Fix the lifetime of the given instruction so that the ARC optimizer does not
// shorten the lifetime of x to be before this point.
@_transparent
public func _fixLifetime<T>(_ x: T) {
  Builtin.fixLifetime(x)
}

/// Calls the given closure with a mutable pointer to the given argument.
///
/// The `withUnsafeMutablePointer(to:_:)` function is useful for calling
/// Objective-C APIs that take in/out parameters (and default-constructible
/// out parameters) by pointer.
///
/// The pointer argument to `body` is valid only during the execution of
/// `withUnsafeMutablePointer(to:_:)`. Do not store or return the pointer for
/// later use.
///
/// - Parameters:
///   - arg: An instance to temporarily use via pointer.
///   - body: A closure that takes a mutable pointer to `arg` as its sole
///     argument. If the closure has a return value, that value is also used
///     as the return value of the `withUnsafeMutablePointer(to:_:)` function.
///     The pointer argument is valid only for the duration of the function's
///     execution.
/// - Returns: The return value, if any, of the `body` closure.
@_inlineable
public func withUnsafeMutablePointer<T, Result>(
  to arg: inout T,
  _ body: (UnsafeMutablePointer<T>) throws -> Result
) rethrows -> Result
{
  return try body(UnsafeMutablePointer<T>(Builtin.addressof(&arg)))
}

/// Invokes the given closure with a pointer to the given argument.
///
/// The `withUnsafePointer(to:_:)` function is useful for calling Objective-C
/// APIs that take in/out parameters (and default-constructible out
/// parameters) by pointer.
///
/// The pointer argument to `body` is valid only during the execution of
/// `withUnsafePointer(to:_:)`. Do not store or return the pointer for later
/// use.
///
/// - Parameters:
///   - arg: An instance to temporarily use via pointer.
///   - body: A closure that takes a pointer to `arg` as its sole argument. If
///     the closure has a return value, that value is also used as the return
///     value of the `withUnsafePointer(to:_:)` function. The pointer argument
///     is valid only for the duration of the function's execution.
/// - Returns: The return value, if any, of the `body` closure.
@_inlineable
public func withUnsafePointer<T, Result>(
  to arg: inout T,
  _ body: (UnsafePointer<T>) throws -> Result
) rethrows -> Result
{
  return try body(UnsafePointer<T>(Builtin.addressof(&arg)))
}

@available(*, unavailable, renamed: "withUnsafeMutablePointer(to:_:)")
public func withUnsafeMutablePointer<T, Result>(
  _ arg: inout T,
  _ body: (UnsafeMutablePointer<T>) throws -> Result
) rethrows -> Result
{
  Builtin.unreachable()
}

@available(*, unavailable, renamed: "withUnsafePointer(to:_:)")
public func withUnsafePointer<T, Result>(
  _ arg: inout T,
  _ body: (UnsafePointer<T>) throws -> Result
) rethrows -> Result
{
  Builtin.unreachable()
}

@available(*, unavailable, message:"use nested withUnsafeMutablePointer(to:_:) instead")
public func withUnsafeMutablePointers<A0, A1, Result>(
  _ arg0: inout A0,
  _ arg1: inout A1,
  _ body: (
    UnsafeMutablePointer<A0>, UnsafeMutablePointer<A1>) throws -> Result
) rethrows -> Result {
  Builtin.unreachable()
}

@available(*, unavailable, message:"use nested withUnsafeMutablePointer(to:_:) instead")
public func withUnsafeMutablePointers<A0, A1, A2, Result>(
  _ arg0: inout A0,
  _ arg1: inout A1,
  _ arg2: inout A2,
  _ body: (
    UnsafeMutablePointer<A0>,
    UnsafeMutablePointer<A1>,
    UnsafeMutablePointer<A2>
  ) throws -> Result
) rethrows -> Result {
  Builtin.unreachable()
}

@available(*, unavailable, message:"use nested withUnsafePointer(to:_:) instead")
public func withUnsafePointers<A0, A1, Result>(
  _ arg0: inout A0,
  _ arg1: inout A1,
  _ body: (UnsafePointer<A0>, UnsafePointer<A1>) throws -> Result
) rethrows -> Result {
  Builtin.unreachable()
}

@available(*, unavailable, message:"use nested withUnsafePointer(to:_:) instead")
public func withUnsafePointers<A0, A1, A2, Result>(
  _ arg0: inout A0,
  _ arg1: inout A1,
  _ arg2: inout A2,
  _ body: (
    UnsafePointer<A0>,
    UnsafePointer<A1>,
    UnsafePointer<A2>
  ) throws -> Result
) rethrows -> Result {
  Builtin.unreachable()
}
