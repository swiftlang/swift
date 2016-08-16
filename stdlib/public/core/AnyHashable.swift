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

/// A value that has a custom representation in `AnyHashable`.
///
/// `Self` should also conform to `Hashable`.
public protocol _HasCustomAnyHashableRepresentation {
  /// Returns a custom representation of `self` as `AnyHashable`.
  /// If returns nil, the default representation is used.
  ///
  /// If your custom representation is a class instance, it
  /// needs to be boxed into `AnyHashable` using the static
  /// type that introduces the `Hashable` conformance.
  ///
  ///     class Base : Hashable {}
  ///     class Derived1 : Base {}
  ///     class Derived2 : Base, _HasCustomAnyHashableRepresentation {
  ///       func _toCustomAnyHashable() -> AnyHashable? {
  ///         // `Derived2` is canonicalized to `Devired1`.
  ///         let customRepresentation = Derived1()
  ///
  ///         // Wrong:
  ///         // return AnyHashable(customRepresentation)
  ///
  ///         // Correct:
  ///         return AnyHashable(customRepresentation as Base)
  ///       }
  func _toCustomAnyHashable() -> AnyHashable?
}

internal protocol _AnyHashableBox {
  var _typeID: ObjectIdentifier { get }
  func _unbox<T : Hashable>() -> T?

  func _isEqual(to: _AnyHashableBox) -> Bool
  var _hashValue: Int { get }

  var _base: Any { get }
  func _downCastConditional<T>(into result: UnsafeMutablePointer<T>) -> Bool
}

internal struct _ConcreteHashableBox<Base : Hashable> : _AnyHashableBox {
  internal var _baseHashable: Base
  internal init(_ base: Base) {
    self._baseHashable = base
  }

  internal var _typeID: ObjectIdentifier {
    return ObjectIdentifier(type(of: self))
  }

  internal func _unbox<T : Hashable>() -> T? {
    return (self as _AnyHashableBox as? _ConcreteHashableBox<T>)?._baseHashable
  }

  internal func _isEqual(to rhs: _AnyHashableBox) -> Bool {
    if let rhs: Base = rhs._unbox() {
      return _baseHashable == rhs
    }
    return false
  }

  internal var _hashValue: Int {
    return _baseHashable.hashValue
  }

  internal var _base: Any {
    return _baseHashable
  }

  internal
  func _downCastConditional<T>(into result: UnsafeMutablePointer<T>) -> Bool {
    guard let value = _baseHashable as? T else { return false }
    result.initialize(to: value)
    return true;
  }
}

/// A type-erased hashable value.
///
/// The `AnyHashable` type forwards equality comparisons and hashing operations
/// to an underlying hashable value, hiding its specific underlying type.
///
/// You can store mixed-type keys in dictionaries and other collections that
/// require `Hashable` conformance by wrapping mixed-type keys in
/// `AnyHashable` instances:
///
///     let descriptions: [AnyHashable: Any] = [
///         AnyHashable("😄"): "emoji",
///         AnyHashable(42): "an Int",
///         AnyHashable(Int8(43)): "an Int8",
///         AnyHashable(Set(["a", "b"])): "a set of strings"
///     ]
///     print(descriptions[AnyHashable(42)]!)      // prints "an Int"
///     print(descriptions[AnyHashable(43)])       // prints "nil"
///     print(descriptions[AnyHashable(Int8(43))]!) // prints "an Int8"
///     print(descriptions[AnyHashable(Set(["a", "b"]))]!) // prints "a set of strings"
public struct AnyHashable {
  internal var _box: _AnyHashableBox

  /// Creates a type-erased hashable value that wraps the given instance.
  ///
  /// The following example creates two type-erased hashable values: `x` wraps
  /// an `Int` with the value 42, while `y` wraps a `UInt8` with the same
  /// numeric value. Because the underlying types of `x` and `y` are
  /// different, the two variables do not compare as equal despite having
  /// equal underlying values.
  ///
  ///     let x = AnyHashable(Int(42))
  ///     let y = AnyHashable(UInt8(42))
  ///
  ///     print(x == y)
  ///     // Prints "false" because `Int` and `UInt8` are different types
  ///
  ///     print(x == AnyHashable(Int(42)))
  ///     // Prints "true"
  ///
  /// - Parameter base: A hashable value to wrap.
  public init<H : Hashable>(_ base: H) {
    if let customRepresentation =
      (base as? _HasCustomAnyHashableRepresentation)?._toCustomAnyHashable() {
      self = customRepresentation
      return
    }

    self._box = _ConcreteHashableBox(0 as Int)
    _stdlib_makeAnyHashableUpcastingToHashableBaseType(
      base,
      storingResultInto: &self)
  }

  internal init<H : Hashable>(_usingDefaultRepresentationOf base: H) {
    self._box = _ConcreteHashableBox(base)
  }

  /// The value wrapped by this instance.
  ///
  /// The `base` property can be cast back to its original type using one of
  /// the casting operators (`as?`, `as!`, or `as`).
  ///
  ///     let anyMessage = AnyHashable("Hello world!")
  ///     if let unwrappedMessage = anyMessage.base as? String {
  ///         print(unwrappedMessage)
  ///     }
  ///     // Prints "Hello world!"
  public var base: Any {
    return _box._base
  }

  /// Perform a downcast directly on the internal boxed representation.
  ///
  /// This avoids the intermediate re-boxing we would get if we just did
  /// a downcast on `base`.
  internal
  func _downCastConditional<T>(into result: UnsafeMutablePointer<T>) -> Bool {
    return _box._downCastConditional(into: result)
  }
}

extension AnyHashable : Equatable {
  /// Returns a Boolean value indicating whether two type-erased hashable
  /// instances wrap the same type and value.
  ///
  /// Two instances of `AnyHashable` compare as equal if and only if the
  /// underlying types have the same conformance to the `Equatable` protocol
  /// and the underlying values compare as equal.
  ///
  /// The following example creates two type-erased hashable values: `x` wraps
  /// an `Int` with the value 42, while `y` wraps a `UInt8` with the same
  /// numeric value. Because the underlying types of `x` and `y` are
  /// different, the two variables do not compare as equal despite having
  /// equal underlying values.
  ///
  ///     let x = AnyHashable(Int(42))
  ///     let y = AnyHashable(UInt8(42))
  ///
  ///     print(x == y)
  ///     // Prints "false" because `Int` and `UInt8` are different types
  ///
  ///     print(x == AnyHashable(Int(42)))
  ///     // Prints "true"
  ///
  /// - Parameters:
  ///   - lhs: A type-erased hashable value.
  ///   - rhs: Another type-erased hashable value.
  public static func == (lhs: AnyHashable, rhs: AnyHashable) -> Bool {
    return lhs._box._isEqual(to: rhs._box)
  }
}

extension AnyHashable : Hashable {
  public var hashValue: Int {
    return _box._hashValue
  }
}

extension AnyHashable : CustomStringConvertible {
  public var description: String {
    return String(describing: base)
  }
}

extension AnyHashable : CustomDebugStringConvertible {
  public var debugDescription: String {
    return "AnyHashable(" + String(reflecting: base) + ")"
  }
}

extension AnyHashable : CustomReflectable {
  public var customMirror: Mirror {
    return Mirror(
      self,
      children: ["value": base])
  }
}

extension Hashable {
  public func _toAnyHashable() -> AnyHashable {
    return AnyHashable(self)
  }
}

/// Returns a default (non-custom) representation of `self`
/// as `AnyHashable`.
///
/// Completely ignores the `_HasCustomAnyHashableRepresentation`
/// conformance, if it exstis.
@_silgen_name("_swift_stdlib_makeAnyHashableUsingDefaultRepresentation")
public // COMPILER_INTRINSIC (actually, called from the runtime)
func _stdlib_makeAnyHashableUsingDefaultRepresentation<H : Hashable>(
  of value: H,
  storingResultInto result: UnsafeMutablePointer<AnyHashable>
) {
  result.pointee = AnyHashable(_usingDefaultRepresentationOf: value)
}

@_silgen_name("_swift_stdlib_makeAnyHashableUpcastingToHashableBaseType")
func _stdlib_makeAnyHashableUpcastingToHashableBaseType<H : Hashable>(
  _ value: H,
  storingResultInto result: UnsafeMutablePointer<AnyHashable>
)

@_silgen_name("_swift_convertToAnyHashable")
public // COMPILER_INTRINSIC
func _convertToAnyHashable<H : Hashable>(_ value: H) -> AnyHashable {
  return AnyHashable(value)
}

@_silgen_name("_swift_convertToAnyHashableIndirect")
public // COMPILER_INTRINSIC (actually, called from the runtime)
func _convertToAnyHashableIndirect<H : Hashable>(
  _ value: H,
  _ target: UnsafeMutablePointer<AnyHashable>
) {
  target.initialize(to: AnyHashable(value))
}

@_silgen_name("_swift_anyHashableDownCastConditionalIndirect")
public // COMPILER_INTRINSIC (actually, called from the runtime)
func _anyHashableDownCastConditionalIndirect<T>(
  _ value: UnsafePointer<AnyHashable>,
  _ target: UnsafeMutablePointer<T>
) -> Bool {
  return value.pointee._downCastConditional(into: target)
}
