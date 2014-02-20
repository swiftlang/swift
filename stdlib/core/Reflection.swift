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

/// A protocol that produces a reflection interface for a value.
protocol Reflectable {
  // The runtime has inappropriate knowledge of this protocol and how its
  // witness tables are laid out. Changing this protocol requires a
  // corresponding change to Reflection.cpp.
    
  /// Get the mirror that reflects this object.
  func getMirror() -> Mirror
}

/// A unique identifier for a class instance. This can be used by reflection
/// clients to avoid visiting cycles in the object graph.
///
/// In Swift, only class instances have unique identities. There is no notion
/// of identity for structs, enums, or tuples.
struct ObjectIdentifier : Hashable {
  val value: Builtin.RawPointer

  // FIXME: Better hashing algorithm
  func hashValue() -> Int {
    return Int(Builtin.ptrtoint_Word(value))
  }
}
func ==(x: ObjectIdentifier, y: ObjectIdentifier) -> Bool {
  return Bool(Builtin.cmp_eq_RawPointer(x.value, y.value))
}

/// A protocol that provides a reflection interface to an underlying value.
protocol Mirror {
  // The runtime has inappropriate knowledge of this protocol and how its
  // witness tables are laid out. Changing this protocol requires a
  // corresponding change to Reflection.cpp.
    
  // FIXME: Most or all of these should be properties and subscripts when we
  // can have those in protocols.

  /// Copy the value out as an Any.
  func getValue() -> Any

  /// Get the type of the value.
  func getType() -> Any.metatype

  /// Get the unique identifier for this value, if it has one.
  /// Always returns Some value for class instances, and always returns None
  /// for value types.
  func getObjectIdentifier() -> ObjectIdentifier?

  /// Get the number of logical children this value has.
  func getCount() -> Int

  /// Get a mirror for one of this value's children.
  ///
  /// Returns a pair of the child's name and its mirror.
  func getChild(i: Int) -> (String, Mirror)

  /// Get a string description of this value.
  func getString() -> String

  /// Get a rich representation of this value for the IDE, if it has one.
  func getIDERepresentation() -> IDERepresentable?
}

/// A protocol for objects that can be used as rich representations by the IDE.
/// These are serialized into a data type identifier and an array of bytes
/// for the IDE's consumption.
protocol IDERepresentable {
  /// Get a string that describes the type of data.
  func getTypeString() -> String

  /// Get the raw data.
  func getData() -> UInt8[]
}

/// Produce a mirror for any value. If the value's type conforms to Reflectable,
/// invoke its getMirror() method; otherwise, fall back to an implementation
/// in the runtime that structurally reflects values of any type.
@asmname="swift_reflectAny" func reflect<T>(x: T) -> Mirror

/// Implementation detail of the runtime mirror implementation.
/// If this is changed, Reflection.cpp in the runtime must be changed to
/// correspond.
struct _MagicMirrorData {
  val owner: Builtin.ObjectPointer
  val ptr: Builtin.RawPointer
  val metadata: Any.metatype
}

/// A thunk that returns a String by value, for C entry points.
func _returnString(inout s: String) -> String {
  return s
}
