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

/// A unique identifier for a class instance or metatype.
///
/// In Swift, only class instances and metatypes have unique identities. There
/// is no notion of identity for structs, enums, functions, or tuples.
public struct ObjectIdentifier : Hashable {
  internal let _value: Builtin.RawPointer

  // FIXME: Better hashing algorithm
  /// The identifier's hash value.
  ///
  /// The hash value is not guaranteed to be stable across different
  /// invocations of the same program.  Do not persist the hash value across
  /// program runs.
  ///
  /// - SeeAlso: `Hashable`
  public var hashValue: Int {
    return Int(Builtin.ptrtoint_Word(_value))
  }

  /// Creates an instance that uniquely identifies the given class instance.
  ///
  /// The following example creates an example class `A` and compares instances
  /// of the class using their object identifiers and the identical-to
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
  public init(_ x: AnyObject) {
    self._value = Builtin.bridgeToRawPointer(x)
  }

  /// Creates an instance that uniquely identifies the given metatype.
  ///
  /// - Parameter: A metatype.
  public init(_ x: Any.Type) {
    self._value = unsafeBitCast(x, to: Builtin.RawPointer.self)
  }
}

extension ObjectIdentifier : CustomDebugStringConvertible {
  /// A textual representation of the identifier, suitable for debugging.
  public var debugDescription: String {
    return "ObjectIdentifier(\(_rawPointerToString(_value)))"
  }
}

extension ObjectIdentifier : Comparable {
  public static func < (lhs: ObjectIdentifier, rhs: ObjectIdentifier) -> Bool {
    return UInt(bitPattern: lhs) < UInt(bitPattern: rhs)
  }

  public static func == (x: ObjectIdentifier, y: ObjectIdentifier) -> Bool {
    return Bool(Builtin.cmp_eq_RawPointer(x._value, y._value))
  }
}

extension UInt {
  /// Creates an integer that captures the full value of the given object
  /// identifier.
  public init(bitPattern objectID: ObjectIdentifier) {
    self.init(Builtin.ptrtoint_Word(objectID._value))
  }
}

extension Int {
  /// Creates an integer that captures the full value of the given object
  /// identifier.
  public init(bitPattern objectID: ObjectIdentifier) {
    self.init(bitPattern: UInt(bitPattern: objectID))
  }
}

extension ObjectIdentifier {
  @available(*, unavailable, message: "use the 'UInt(_:)' initializer")
  public var uintValue: UInt {
    Builtin.unreachable()
  }
}

extension UInt {
  @available(*, unavailable, renamed: "init(bitPattern:)")
  public init(_ objectID: ObjectIdentifier) {
    Builtin.unreachable()
  }
}

extension Int {
  @available(*, unavailable, renamed: "init(bitPattern:)")
  public init(_ objectID: ObjectIdentifier) {
    Builtin.unreachable()
  }
}

