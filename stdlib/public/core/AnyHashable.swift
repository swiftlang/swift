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
  ///         // `Derived2` is canonicalized to `Derived1`.
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

  /// Determine whether values in the boxes are equivalent.
  ///
  /// - Returns: `nil` to indicate that the boxes store different types, so
  ///   no comparison is possible. Otherwise, contains the result of `==`.
  func _isEqual(to: _AnyHashableBox) -> Bool?
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

  internal func _isEqual(to rhs: _AnyHashableBox) -> Bool? {
    if let rhs: Base = rhs._unbox() {
      return _baseHashable == rhs
    }
    return nil
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
    return true
  }
}

#if _runtime(_ObjC)
// Retrieve the custom AnyHashable representation of the value after it
// has been bridged to Objective-C. This mapping to Objective-C and back
// turns a non-custom representation into a custom one, which is used as
// the lowest-common-denominator for comparisons.
func _getBridgedCustomAnyHashable<T>(_ value: T) -> AnyHashable? {
  let bridgedValue = _bridgeAnythingToObjectiveC(value)
  return (bridgedValue as?
    _HasCustomAnyHashableRepresentation)?._toCustomAnyHashable()
}
#endif

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
  internal var _usedCustomRepresentation: Bool

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
      self._usedCustomRepresentation = true
      return
    }

    self._box = _ConcreteHashableBox(0 as Int)
    self._usedCustomRepresentation = false
    _stdlib_makeAnyHashableUpcastingToHashableBaseType(
      base,
      storingResultInto: &self)
  }

  internal init<H : Hashable>(_usingDefaultRepresentationOf base: H) {
    self._box = _ConcreteHashableBox(base)
    self._usedCustomRepresentation = false
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
    // Attempt the downcast.
    if _box._downCastConditional(into: result) { return true }

    #if _runtime(_ObjC)
    // If we used a custom representation, bridge to Objective-C and then
    // attempt the cast from there.
    if _usedCustomRepresentation {
      if let value = _bridgeAnythingToObjectiveC(_box._base) as? T {
        result.initialize(to: value)
        return true
      }
    }
    #endif

    return false
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
    // If they're equal, we're done.
    if let result = lhs._box._isEqual(to: rhs._box) { return result }

    #if _runtime(_ObjC)
    // If one used a custom representation but the other did not, bridge
    // the one that did *not* use the custom representation to Objective-C:
    // if the bridged result has a custom representation, compare those custom
    // custom representations.
    if lhs._usedCustomRepresentation != rhs._usedCustomRepresentation {
      // If the lhs used a custom representation, try comparing against the
      // custom representation of the bridged rhs (if there is one).
      if lhs._usedCustomRepresentation {
        if let customRHS = _getBridgedCustomAnyHashable(rhs._box._base) {
          return lhs._box._isEqual(to: customRHS._box) ?? false
        }
        return false
      }

      // Otherwise, try comparing the rhs against the custom representation of
      // the bridged lhs (if there is one).
      if let customLHS = _getBridgedCustomAnyHashable(lhs._box._base) {
        return customLHS._box._isEqual(to: rhs._box) ?? false
      }
      return false
    }
    #endif

    return false
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
/// conformance, if it exists.
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
