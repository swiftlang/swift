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

// This function should be opaque to the optimizer.
// BLOCKED: <rdar://problem/16464507> This function will be unnecessary when
// fix_lifetime is honored by the ARC optimizer.
@asmname("swift_keepAlive")
func swift_keepAlive<T>(inout _: T)

/// \brief An instance of this struct keeps the references registered with it
/// at +1 reference count until the call to \c release().
///
/// It is absolutely necessary to call \c release().  Forgetting to call
/// \c release() will not cause a memory leak.  Instead, the managed objects will be
/// released earlier than expected.
///
/// This class can be used to extend lifetime of objects to pass UnsafePointers
/// to them to C APIs.
class LifetimeManager {
  var _managedRefs : Builtin.NativeObject[]
  var _releaseCalled : Bool

  init() {
    _managedRefs = Array<Builtin.NativeObject>()
    _releaseCalled = false
  }

  deinit {
    if !_releaseCalled {
      _fatalError("release() should have been called")
    }
  }

  func put(objPtr: Builtin.NativeObject) {
    _managedRefs.append(objPtr)
  }

  // FIXME: Need class constraints for this to work properly.
  // func put<T>(obj: T) {
  //   put(Builtin.castToNativeObject(obj))
  // }

  /// \brief Call this function to end the forced lifetime extension.
  func release() {
    _fixLifetime(_managedRefs.owner)
    _releaseCalled = true
  }
}

/// \brief Evaluate f() and return its result, ensuring that x is not
/// destroyed before f returns.
func withExtendedLifetime<T, Result>(
  x: T, f: ()->Result
) -> Result {
  let result = f()
  _fixLifetime(x)
  return result
}

/// \brief Evaluate f(x) and return its result, ensuring that x is not
/// destroyed before f returns.
func withExtendedLifetime<T, Result>(
  x: T, f: (T)->Result
) -> Result {
  let result = f(x)
  _fixLifetime(x)
  return result
}

// FIXME: this function can die once <rdar://problem/14497260> (need
// support for CF bridging) is solved.

/// \brief Pass a given object as a COpaquePointer at +0 to the given
/// function, returning its result.  This function is useful for
/// calling CoreFoundation functions on NS types that are toll-free
/// bridged; you have to declare these functions as taking
/// COpaquePointer, obviously.
func withObjectAtPlusZero<Result>(x: AnyObject, f: (COpaquePointer)->Result) -> Result {
  return withExtendedLifetime(x) {
    return f(
      COpaquePointer(UnsafePointer<Void>(Builtin.bridgeToRawPointer(Builtin.castToNativeObject(x)))))
  }
}

extension String {

  /// \brief Invoke f on the contents of this string, represented as
  /// a nul-terminated array of char, ensuring that the array's
  /// lifetime extends through the execution of f
  func withCString<Result>(
    f: (CString)->Result
  ) -> Result {
    return self.nulTerminatedUTF8.withUnsafePointerToElements {
      f(CString($0))
    }
  }

  /// \brief Invoke f on the contents of this string, represented as
  /// a nul-terminated array of char, ensuring that the array's
  /// lifetime extends through the execution of f
  func withCString<Result>(
    f: (UnsafePointer<CChar>)->Result
  ) -> Result {
    return self.nulTerminatedUTF8.withUnsafePointerToElements {
      f(UnsafePointer($0))
    }
  }
}

@transparent
func _fixLifetime<T>(var x: T) {
  swift_keepAlive(&x)
}
