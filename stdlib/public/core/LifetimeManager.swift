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

/// Evaluate `f()` and return its result, ensuring that `x` is not
/// destroyed before f returns.
@_inlineable
public func withExtendedLifetime<T, Result>(
  _ x: T, _ body: () throws -> Result
) rethrows -> Result {
  defer { _fixLifetime(x) }
  return try body()
}

/// Evaluate `f(x)` and return its result, ensuring that `x` is not
/// destroyed before f returns.
@_inlineable
public func withExtendedLifetime<T, Result>(
  _ x: T, _ body: (T) throws -> Result
) rethrows -> Result {
  defer { _fixLifetime(x) }
  return try body(x)
}

// Fix the lifetime of the given instruction so that the ARC optimizer does not
// shorten the lifetime of x to be before this point.
@_transparent
public func _fixLifetime<T>(_ x: T) {
  Builtin.fixLifetime(x)
}

/// Invokes the given closure with a mutable pointer to the given argument.
///
/// The `withUnsafeMutablePointer(to:_:)` function is useful for calling
/// Objective-C APIs that take in/out parameters (and default-constructible
/// out parameters) by pointer.
///
/// The pointer argument to `body` is valid only for the lifetime of the
/// closure. Do not escape it from the closure for later use.
///
/// - Parameters:
///   - arg: An instance to temporarily use via pointer.
///   - body: A closure that takes a mutable pointer to `arg` as its sole
///     argument. If the closure has a return value, it is used as the return
///     value of the `withUnsafeMutablePointer(to:_:)` function. The pointer
///     argument is valid only for the duration of the closure's execution.
/// - Returns: The return value of the `body` closure, if any.
///
/// - SeeAlso: `withUnsafePointer(to:_:)`
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
/// The pointer argument to `body` is valid only for the lifetime of the
/// closure. Do not escape it from the closure for later use.
///
/// - Parameters:
///   - arg: An instance to temporarily use via pointer.
///   - body: A closure that takes a pointer to `arg` as its sole argument. If
///     the closure has a return value, it is used as the return value of the
///     `withUnsafePointer(to:_:)` function. The pointer argument is valid
///     only for the duration of the closure's execution.
/// - Returns: The return value of the `body` closure, if any.
///
/// - SeeAlso: `withUnsafeMutablePointer(to:_:)`
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
