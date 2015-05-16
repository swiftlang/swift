//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// Evaluate `f()` and return its result, ensuring that `x` is not
/// destroyed before f returns.
public func withExtendedLifetime<T, Result>(
  x: T, @noescape _ f: () -> Result
) -> Result {
  let result = f()
  _fixLifetime(x)
  return result
}

/// Evaluate `f(x)` and return its result, ensuring that `x` is not
/// destroyed before f returns.
public func withExtendedLifetime<T, Result>(
  x: T, @noescape _ f: T -> Result
) -> Result {
  let result = f(x)
  _fixLifetime(x)
  return result
}

extension String {

  /// Invoke `f` on the contents of this string, represented as
  /// a nul-terminated array of char, ensuring that the array's
  /// lifetime extends through the execution of `f`.
  public func withCString<Result>(
    @noescape f: UnsafePointer<Int8> -> Result
  ) -> Result {
    return self.nulTerminatedUTF8.withUnsafeBufferPointer {
      f(UnsafePointer($0.baseAddress))
    }
  }
}

// Fix the lifetime of the given instruction so that the ARC optimizer does not
// shorten the lifetime of x to be before this point.
@transparent
public func _fixLifetime<T>(x: T) {
  Builtin.fixLifetime(x)
}

/// Invokes `body` with an `UnsafeMutablePointer` to `arg` and returns the
/// result. Useful for calling Objective-C APIs that take "in/out"
/// parameters (and default-constructible "out" parameters) by pointer.
public func withUnsafeMutablePointer<T, Result>(
  inout arg: T,
  @noescape _ body: UnsafeMutablePointer<T> -> Result
) -> Result
{
  return body(UnsafeMutablePointer<T>(Builtin.addressof(&arg)))
}

/// Like `withUnsafeMutablePointer`, but passes pointers to `arg0` and `arg1`.
public func withUnsafeMutablePointers<A0, A1, Result>(
  inout arg0: A0,
  inout _ arg1: A1,
  @noescape _ body: (UnsafeMutablePointer<A0>, UnsafeMutablePointer<A1>) -> Result
) -> Result {
  return body(
    UnsafeMutablePointer<A0>(Builtin.addressof(&arg0)),
    UnsafeMutablePointer<A1>(Builtin.addressof(&arg1)))
}

/// Like `withUnsafeMutablePointer`, but passes pointers to `arg0`, `arg1`,
/// and `arg2`.
public func withUnsafeMutablePointers<A0, A1, A2, Result>(
  inout arg0: A0,
  inout _ arg1: A1,
  inout _ arg2: A2,
  @noescape _ body: (
    UnsafeMutablePointer<A0>,
    UnsafeMutablePointer<A1>,
    UnsafeMutablePointer<A2>
  ) -> Result
) -> Result {
  return body(
    UnsafeMutablePointer<A0>(Builtin.addressof(&arg0)),
    UnsafeMutablePointer<A1>(Builtin.addressof(&arg1)),
    UnsafeMutablePointer<A2>(Builtin.addressof(&arg2)))
}

/// Invokes `body` with an `UnsafePointer` to `arg` and returns the
/// result. Useful for calling Objective-C APIs that take "in/out"
/// parameters (and default-constructible "out" parameters) by pointer.
public func withUnsafePointer<T, Result>(
  inout arg: T,
  @noescape _ body: UnsafePointer<T> -> Result
) -> Result
{
  return body(UnsafePointer<T>(Builtin.addressof(&arg)))
}

/// Like `withUnsafePointer`, but passes pointers to `arg0` and `arg1`.
public func withUnsafePointers<A0, A1, Result>(
  inout arg0: A0,
  inout _ arg1: A1,
  @noescape _ body: (UnsafePointer<A0>, UnsafePointer<A1>) -> Result
) -> Result {
  return body(
    UnsafePointer<A0>(Builtin.addressof(&arg0)),
    UnsafePointer<A1>(Builtin.addressof(&arg1)))
}

/// Like `withUnsafePointer`, but passes pointers to `arg0`, `arg1`,
/// and `arg2`.
public func withUnsafePointers<A0, A1, A2, Result>(
  inout arg0: A0,
  inout _ arg1: A1,
  inout _ arg2: A2,
  @noescape _ body: (
    UnsafePointer<A0>,
    UnsafePointer<A1>,
    UnsafePointer<A2>
  ) -> Result
) -> Result {
  return body(
    UnsafePointer<A0>(Builtin.addressof(&arg0)),
    UnsafePointer<A1>(Builtin.addressof(&arg1)),
    UnsafePointer<A2>(Builtin.addressof(&arg2)))
}
