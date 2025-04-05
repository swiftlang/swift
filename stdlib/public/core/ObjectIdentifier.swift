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

#if !$Embedded

/// A unique identifier for a class instance or metatype.
///
/// This unique identifier is only valid for comparisons during the lifetime
/// of the instance.
///
/// In Swift, only class instances and metatypes have unique identities. There
/// is no notion of identity for structs, enums, functions, or tuples.
@frozen // trivial-implementation
public struct ObjectIdentifier: Sendable {
  @usableFromInline // trivial-implementation
  internal let _value: Builtin.RawPointer

  /// Creates an instance that uniquely identifies the given class instance.
  ///
  /// The following example creates an example class `IntegerRef` and compares
  /// instances of the class using their object identifiers and the identical-to
  /// operator (`===`):
  ///
  ///     class IntegerRef {
  ///         let value: Int
  ///         init(_ value: Int) {
  ///             self.value = value
  ///         }
  ///     }
  ///
  ///     let x = IntegerRef(10)
  ///     let y = x
  ///
  ///     print(ObjectIdentifier(x) == ObjectIdentifier(y))
  ///     // Prints "true"
  ///     print(x === y)
  ///     // Prints "true"
  ///
  ///     let z = IntegerRef(10)
  ///     print(ObjectIdentifier(x) == ObjectIdentifier(z))
  ///     // Prints "false"
  ///     print(x === z)
  ///     // Prints "false"
  ///
  /// - Parameter x: An instance of a class.
  @inlinable // trivial-implementation
  public init(_ x: AnyObject) {
    self._value = Builtin.bridgeToRawPointer(x)
  }

  /// Creates an instance that uniquely identifies the given metatype.
  ///
  /// - Parameters:
  ///   - x: A metatype.
  @_alwaysEmitIntoClient
  public init(_ x: any (~Copyable & ~Escapable).Type) {
    self._value = unsafe unsafeBitCast(x, to: Builtin.RawPointer.self)
  }

  @inlinable
  public init(_ x: Any.Type) {
    // FIXME: This ought to be obsoleted in favor of the generalized overload
    // above. Unfortunately, that one sometimes causes a runtime hang.
    self._value = unsafe unsafeBitCast(x, to: Builtin.RawPointer.self)
  }
}

#else

@frozen // trivial-implementation
public struct ObjectIdentifier: Sendable {
  @usableFromInline // trivial-implementation
  internal let _value: Builtin.RawPointer

  @inlinable // trivial-implementation
  public init<Object: AnyObject>(_ x: Object) {
    self._value = Builtin.bridgeToRawPointer(x)
  }

  @inlinable // trivial-implementation
  public init<T: ~Copyable & ~Escapable>(_ x: T.Type) {
    self._value = unsafe unsafeBitCast(x, to: Builtin.RawPointer.self)
  }
}

#endif

@_unavailableInEmbedded
extension ObjectIdentifier: CustomDebugStringConvertible {
  /// A textual representation of the identifier, suitable for debugging.
  public var debugDescription: String {
    return "ObjectIdentifier(\(_rawPointerToString(_value)))"
  }
}

extension ObjectIdentifier: Equatable {
  @inlinable // trivial-implementation
  public static func == (x: ObjectIdentifier, y: ObjectIdentifier) -> Bool {
    return Bool(Builtin.cmp_eq_RawPointer(x._value, y._value))
  }
}

extension ObjectIdentifier: Comparable {
  @inlinable // trivial-implementation
  public static func < (lhs: ObjectIdentifier, rhs: ObjectIdentifier) -> Bool {
    return UInt(bitPattern: lhs) < UInt(bitPattern: rhs)
  }
}

extension ObjectIdentifier: Hashable {
  /// Hashes the essential components of this value by feeding them into the
  /// given hasher.
  ///
  /// - Parameter hasher: The hasher to use when combining the components
  ///   of this instance.
  @inlinable
  public func hash(into hasher: inout Hasher) {
    hasher.combine(Int(Builtin.ptrtoint_Word(_value)))
  }

  @_alwaysEmitIntoClient // For back deployment
  public func _rawHashValue(seed: Int) -> Int {
    Int(Builtin.ptrtoint_Word(_value))._rawHashValue(seed: seed)
  }
}

extension UInt {
  /// Creates an integer that captures the full value of the given object
  /// identifier.
  @inlinable // trivial-implementation
  public init(bitPattern objectID: ObjectIdentifier) {
    self.init(Builtin.ptrtoint_Word(objectID._value))
  }
}

extension Int {
  /// Creates an integer that captures the full value of the given object
  /// identifier.
  @inlinable // trivial-implementation
  public init(bitPattern objectID: ObjectIdentifier) {
    self.init(bitPattern: UInt(bitPattern: objectID))
  }
}
