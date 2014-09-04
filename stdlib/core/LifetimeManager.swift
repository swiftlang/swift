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
  x: T, f: ()->Result
) -> Result {
  let result = f()
  _fixLifetime(x)
  return result
}

/// Evaluate `f(x)` and return its result, ensuring that `x` is not
/// destroyed before f returns.
public func withExtendedLifetime<T, Result>(
  x: T, f: (T)->Result
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
    f: (UnsafePointer<Int8>)->Result
  ) -> Result {
    return self.nulTerminatedUTF8.withUnsafeBufferPointer {
      f(UnsafePointer($0.baseAddress))
    }
  }
}

// Fix the lifetime of the given instruction so that the ARC optimizer does not
// shorten the lifetime of x to be before this point.
@transparent public
func _fixLifetime<T>(var x: T) {
  Builtin.fixLifetime(x)
}

