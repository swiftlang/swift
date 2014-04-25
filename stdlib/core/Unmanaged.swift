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
  @unowned(unsafe) var _value: T

  @transparent
  init(_private: T) { _value = _private }

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

  /// Create an unmanaged reference with an unbalanced retain,
  /// starting from a value of a different class type.  You
  /// take responsibility for ensuring that the types are actually
  /// compatible at runtime.
  ///
  /// This is useful when passing an object to an API which Swift
  /// does not know the ownership rules for, but you know that the
  /// API expects you to pass the object at +1, and additionally
  /// the object has a static type which you know is
  /// toll-free-bridged to the type you need it to have.
  ///
  /// \c labels[i] = .bridgeToRetained<CFString>(someNSString)
  @transparent
  static func bridgeToRetained<U: AnyObject>(value: U) -> Unmanaged {
    return passRetained(reinterpretCast(value))
  }

  /// Create an unmanaged reference without performing an unbalanced
  /// retain, starting with a value of a different class type.
  /// You take responsibility for ensuring that the types are
  /// actually compatible at runtime.
  ///
  /// This is useful when passing an object to an API which Swift
  /// does not know the ownership rules for, but you know that the
  /// API expects you to pass the object at +0, and additionally
  /// the object has a static type which you know is
  /// toll-free-bridged to the type you need it to have.
  ///
  /// \c CGGradientCreateWithColors(colorSpace,
  ///                               .bridgeToUnretained(someNSArray),
  ///                               locations)
  @transparent
  static func bridgeToUnretained<U: AnyObject>(value: U) -> Unmanaged {
    return passUnretained(reinterpretCast(value))
  }

  /// Get the value of this unmanaged reference as a managed
  /// reference without consuming an unbalanced retain of it.
  var unretainedValue: T {
    return _value
  }

  /// Get the value of this unmanaged reference as a managed
  /// reference and consume an unbalanced retain of it.
  var retainedValue: T {
    let result = _value
    release()
    return result
  }

  /// Get the value of this unmanaged reference as an managed
  /// reference of a different class type without consuming
  /// an unbalanced retain.  You take responsibility
  /// for ensuring that the types are actually compatible at
  /// runtime.
  func bridgeUnretainedValueTo<U: AnyObject>() -> U {
    return reinterpretCast(unretainedValue)
  }

  /// Get the value of this unmanaged reference as an managed
  /// reference of a different class type and consume
  /// an unbalanced retain of it.  You take responsibility
  /// for ensuring that the types are actually compatible at
  /// runtime.
  func bridgeRetainedValueTo<U: AnyObject>() -> U {
    return reinterpretCast(retainedValue)
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

/// Bridge a value of one type to another type.
func bridgeTo<T: AnyObject, U: AnyObject>(value: U) -> T {
  return reinterpretCast(value)
}
