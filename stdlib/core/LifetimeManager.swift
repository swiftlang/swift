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
    f: (ConstUnsafePointer<Int8>)->Result
  ) -> Result {
    return self.nulTerminatedUTF8.withUnsafePointerToElements {
      f(ConstUnsafePointer($0))
    }
  }

  /// Invoke `f` on the contents of this string, represented as
  /// a nul-terminated array of char, ensuring that the array's
  /// lifetime extends through the execution of `f`.
  public func withCString<Result>(
    f: (UnsafePointer<CChar>)->Result
  ) -> Result {
    // FIXME: This interface isn't const-correct; only the UnsafePointer variant
    // above should be available.
    return self.nulTerminatedUTF8.withUnsafePointerToElements {
      f(UnsafePointer($0))
    }
  }
}

// This function should be opaque to the optimizer.
// BLOCKED: <rdar://problem/16464507> This function will be unnecessary when
// fix_lifetime is honored by the ARC optimizer.
@asmname("swift_keepAlive") internal
func _swift_keepAlive<T>(inout _: T)

@transparent public
func _fixLifetime<T>(var x: T) {
  _swift_keepAlive(&x)
}

