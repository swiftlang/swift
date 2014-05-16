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

/// A type for propagating an unmanaged object reference.
///
/// When you use this type, you become partially responsible for
/// keeping the object alive.
struct Unmanaged<T: AnyObject> {
  unowned(unsafe) var _value: T

  @transparent
  init(_private: T) { _value = _private }

  /// Unsafely turn an opaque C pointer into an unmanaged
  /// class reference.
  ///
  /// This operation does not change reference counts.
  ///
  /// \c let str: CFString = Unmanaged.fromOpaque(ptr).takeUnretainedValue()
  @transparent
  static func fromOpaque(value: COpaquePointer) -> Unmanaged {
    return Unmanaged(_private: reinterpretCast(value))
  }

  /// Unsafely turn an unmanaged class reference into an opaque
  /// C pointer.
  ///
  /// This operation does not change reference counts.
  ///
  /// \c let str: CFString = Unmanaged.fromOpaque(ptr).takeUnretainedValue()
  @transparent
  func toOpaque() -> COpaquePointer {
    return reinterpretCast(_value)
  }

  /// Create an unmanaged reference with an unbalanced retain.
  /// The object will leak if nothing eventually balances the retain.
  ///
  /// This is useful when passing an object to an API which Swift
  /// does not know the ownership rules for, but you know that the
  /// API expects you to pass the object at +1.
  ///
  /// \c CFAutorelease(.passRetained(object))
  @transparent
  static func passRetained(value: T) -> Unmanaged {
    return Unmanaged(_private: value).retain()
  }

  /// Create an unmanaged reference without performing an unbalanced
  /// retain.
  ///
  /// This is useful when passing a reference to an API which Swift
  /// does not know the ownership rules for, but you know that the
  /// API expects you to pass the object at +0.
  ///
  /// \c CFArraySetValueAtIndex(.passUnretained(array), i,
  ///                           .passUnretained(object))
  @transparent
  static func passUnretained(value: T) -> Unmanaged {
    return Unmanaged(_private: value)
  }

  /// Get the value of this unmanaged reference as a managed
  /// reference without consuming an unbalanced retain of it.
  ///
  /// This is useful when a function returns an unmanaged reference
  /// and you know that you're not responsible for releasing the result.
  func takeUnretainedValue() -> T {
    return _value
  }

  /// Get the value of this unmanaged reference as a managed
  /// reference and consume an unbalanced retain of it.
  ///
  /// This is useful when a function returns an unmanaged reference
  /// and you know that you're responsible for releasing the result.
  func takeRetainedValue() -> T {
    let result = _value
    release()
    return result
  }

  /// Perform an unbalanced retain of the object.
  @transparent
  func retain() -> Unmanaged {
    Builtin.retain(_value)
    return self
  }

  /// Perform an unbalanced release of the object.
  @transparent
  func release() {
    Builtin.release(_value)
  }

  /// Perform an unbalanced autorelease of the object.
  @transparent
  func autorelease() -> Unmanaged {
    Builtin.autorelease(_value)
    return self
  }
}
