//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// Evaluate `f()` and return its result, ensuring that `x` is not
/// destroyed before f returns.
public func withExtendedLifetime<T, Result>(
  _ x: T, _ body: () throws -> Result
) rethrows -> Result {
  defer { _fixLifetime(x) }
  return try body()
}

/// Evaluate `f(x)` and return its result, ensuring that `x` is not
/// destroyed before f returns.
public func withExtendedLifetime<T, Result>(
  _ x: T, _ body: (T) throws -> Result
) rethrows -> Result {
  defer { _fixLifetime(x) }
  return try body(x)
}

extension String {

  /// Invokes the given closure on the contents of the string, represented as a
  /// pointer to a null-terminated sequence of UTF-8 code units.
  ///
  /// The `withCString(_:)` method ensures that the sequence's lifetime extends
  /// through the execution of `f`.
  ///
  /// - Parameter f: A closure that takes a pointer to the string's UTF-8 code
  ///   unit sequence as its sole argument. If the closure has a return value,
  ///   it is used as the return value of the `withCString(_:)` method.
  /// - Returns: The return value of the `f` closure, if any.
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

/// Invokes `body` with an `UnsafeMutablePointer` to `arg` and returns the
/// result. Useful for calling Objective-C APIs that take "in/out"
/// parameters (and default-constructible "out" parameters) by pointer.
public func withUnsafeMutablePointer<T, Result>(
  to arg: inout T,
  _ body: (UnsafeMutablePointer<T>) throws -> Result
) rethrows -> Result
{
  return try body(UnsafeMutablePointer<T>(Builtin.addressof(&arg)))
}

/// Invokes `body` with an `UnsafePointer` to `arg` and returns the
/// result. Useful for calling Objective-C APIs that take "in/out"
/// parameters (and default-constructible "out" parameters) by pointer.
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
