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

/// Customizes the result of `_reflect(x)`, where `x` is a conforming
/// type.
public protocol _Reflectable {
  // The runtime has inappropriate knowledge of this protocol and how its
  // witness tables are laid out. Changing this protocol requires a
  // corresponding change to Reflection.cpp.

  /// Returns a mirror that reflects `self`.
  @warn_unused_result
  func _getMirror() -> _MirrorType
}

/// A unique identifier for a class instance or metatype. This can be used by
/// reflection clients to recognize cycles in the object graph.
///
/// In Swift, only class instances and metatypes have unique identities. There
/// is no notion of identity for structs, enums, functions, or tuples.
public struct ObjectIdentifier : Hashable, Comparable {
  let value: Builtin.RawPointer

  /// Convert to a `UInt` that captures the full value of `self`.
  ///
  /// Axiom: `a.uintValue == b.uintValue` iff `a == b`.
  public var uintValue: UInt {
    return UInt(Builtin.ptrtoint_Word(value))
  }

  // FIXME: Better hashing algorithm
  /// The hash value.
  ///
  /// **Axiom:** `x == y` implies `x.hashValue == y.hashValue`.
  ///
  /// - Note: The hash value is not guaranteed to be stable across
  ///   different invocations of the same program.  Do not persist the
  ///   hash value across program runs.
  public var hashValue: Int {
    return Int(Builtin.ptrtoint_Word(value))
  }

  /// Construct an instance that uniquely identifies the class instance `x`.
  public init(_ x: AnyObject) {
    self.value = Builtin.bridgeToRawPointer(x)
  }

  /// Construct an instance that uniquely identifies the metatype `x`.
  public init(_ x: Any.Type) {
    self.value = unsafeBitCast(x, Builtin.RawPointer.self)
  }
}

@warn_unused_result
public func <(lhs: ObjectIdentifier, rhs: ObjectIdentifier) -> Bool {
  return lhs.uintValue < rhs.uintValue
}

@warn_unused_result
public func ==(x: ObjectIdentifier, y: ObjectIdentifier) -> Bool {
  return Bool(Builtin.cmp_eq_RawPointer(x.value, y.value))
}


/// How children of this value should be presented in the IDE.
public enum _MirrorDisposition {
  /// As a struct.
  case Struct
  /// As a class.
  case Class
  /// As an enum.
  case Enum
  /// As a tuple.
  case Tuple
  /// As a miscellaneous aggregate with a fixed set of children.
  case Aggregate
  /// As a container that is accessed by index.
  case IndexContainer
  /// As a container that is accessed by key.
  case KeyContainer
  /// As a container that represents membership of its values.
  case MembershipContainer
  /// As a miscellaneous container with a variable number of children.
  case Container
  /// An Optional which can have either zero or one children.
  case Optional
  /// An Objective-C object imported in Swift.
  case ObjCObject
}

/// The type returned by `_reflect(x)`; supplies an API for runtime
/// reflection on `x`.
public protocol _MirrorType {
  /// The instance being reflected.
  var value: Any { get }

  /// Identical to `value.dynamicType`.
  var valueType: Any.Type { get }

  /// A unique identifier for `value` if it is a class instance; `nil`
  /// otherwise.
  var objectIdentifier: ObjectIdentifier? { get }

  /// The count of `value`'s logical children.
  var count: Int { get }

  /// Get a name and mirror for the `i`th logical child.
  subscript(i: Int) -> (String, _MirrorType) { get }

  /// A string description of `value`.
  var summary: String { get }

  /// A rich representation of `value` for an IDE, or `nil` if none is supplied.
  var quickLookObject: PlaygroundQuickLook? { get }

  /// How `value` should be presented in an IDE.
  var disposition: _MirrorDisposition { get }
}

/// An entry point that can be called from C++ code to get the summary string
/// for an arbitrary object. The memory pointed to by "out" is initialized with
/// the summary string.
@warn_unused_result
@_silgen_name("swift_getSummary")
public // COMPILER_INTRINSIC
func _getSummary<T>(out: UnsafeMutablePointer<String>, x: T) {
  out.initialize(_reflect(x).summary)
}

/// Produce a mirror for any value. If the value's type conforms to
/// `_Reflectable`, invoke its `_getMirror()` method; otherwise, fall back
/// to an implementation in the runtime that structurally reflects values
/// of any type.
@warn_unused_result
@_silgen_name("swift_reflectAny")
public func _reflect<T>(x: T) -> _MirrorType

/// Dump an object's contents using its mirror to the specified output stream.
public func dump<T, TargetStream : OutputStreamType>(
    x: T, inout _ targetStream: TargetStream,
    name: String? = nil, indent: Int = 0,
    maxDepth: Int = .max, maxItems: Int = .max
) -> T {
  var maxItemCounter = maxItems
  var visitedItems = [ObjectIdentifier : Int]()
  _dumpWithMirror(
    _reflect(x), name, indent, maxDepth, &maxItemCounter, &visitedItems,
    &targetStream)
  return x
}

/// Dump an object's contents using its mirror to standard output.
public func dump<T>(x: T, name: String? = nil, indent: Int = 0,
             maxDepth: Int = .max, maxItems: Int = .max) -> T {
  var stdoutStream = _Stdout()
  return dump(
    x, &stdoutStream, name: name, indent: indent, maxDepth: maxDepth,
    maxItems: maxItems)
}

/// Dump an object's contents using a mirror. User code should use dump().
func _dumpWithMirror<TargetStream : OutputStreamType>(
    mirror: _MirrorType, _ name: String?, _ indent: Int, _ maxDepth: Int,
    inout _ maxItemCounter: Int,
    inout _ visitedItems: [ObjectIdentifier : Int],
    inout _ targetStream: TargetStream
) {
  if maxItemCounter <= 0 { return }
  maxItemCounter -= 1

  for _ in 0..<indent { print(" ", terminator: "", toStream: &targetStream) }

  let count = mirror.count
  let bullet = count == 0    ? "-"
             : maxDepth <= 0 ? "▹" : "▿"
  print("\(bullet) ", terminator: "", toStream: &targetStream)

  if let nam = name {
    print("\(nam): ", terminator: "", toStream: &targetStream)
  }
  print(mirror.summary, terminator: "", toStream: &targetStream)

  if let id = mirror.objectIdentifier {
    if let previous = visitedItems[id] {
      print(" #\(previous)", toStream: &targetStream)
      return
    }
    let identifier = visitedItems.count
    visitedItems[id] = identifier
    print(" #\(identifier)", terminator: "", toStream: &targetStream)
  }

  print("", toStream: &targetStream)

  if maxDepth <= 0 { return }

  for i in 0..<count {
    if maxItemCounter <= 0 {
      for _ in 0..<(indent+4) {
        print(" ", terminator: "", toStream: &targetStream)
      }
      let remainder = count - i
      print("(\(remainder)", terminator: "", toStream: &targetStream)
      if i > 0 { print(" more", terminator: "", toStream: &targetStream) }
      if remainder == 1 {
        print(" child)", toStream: &targetStream)
      } else {
        print(" children)", toStream: &targetStream)
      }
      return
    }

    let (name, child) = mirror[i]
    _dumpWithMirror(child, name, indent + 2, maxDepth - 1,
                    &maxItemCounter, &visitedItems, &targetStream)
  }
}

// -- _MirrorType implementations for basic data types

/// A mirror for a value that is represented as a simple value with no
/// children.
internal struct _LeafMirror<T>: _MirrorType {
  let _value: T
  let summaryFunction: T -> String
  let quickLookFunction: T -> PlaygroundQuickLook?

  init(_ value: T, _ summaryFunction: T -> String,
       _ quickLookFunction: T -> PlaygroundQuickLook?) {
    self._value = value
    self.summaryFunction = summaryFunction
    self.quickLookFunction = quickLookFunction
  }

  var value: Any { return _value }
  var valueType: Any.Type { return value.dynamicType }
  var objectIdentifier: ObjectIdentifier? { return nil }
  var count: Int { return 0 }
  subscript(i: Int) -> (String, _MirrorType) {
    _preconditionFailure("no children")
  }
  var summary: String { return summaryFunction(_value) }
  var quickLookObject: PlaygroundQuickLook? { return quickLookFunction(_value) }
  var disposition: _MirrorDisposition { return .Aggregate }
}

// -- Implementation details for the runtime's _MirrorType implementation

@_silgen_name("swift_MagicMirrorData_summary")
func _swift_MagicMirrorData_summaryImpl(
  metadata: Any.Type, _ result: UnsafeMutablePointer<String>
)

public struct _MagicMirrorData {
  let owner: Builtin.NativeObject
  let ptr: Builtin.RawPointer
  let metadata: Any.Type

  var value: Any {
    @_silgen_name("swift_MagicMirrorData_value")get
  }
  var valueType: Any.Type {
    @_silgen_name("swift_MagicMirrorData_valueType")get
  }

  public var objcValue: Any {
    @_silgen_name("swift_MagicMirrorData_objcValue")get
  }
  public var objcValueType: Any.Type {
    @_silgen_name("swift_MagicMirrorData_objcValueType")get
  }

  var summary: String {
    let (_, result) = _withUninitializedString {
      _swift_MagicMirrorData_summaryImpl(self.metadata, $0)
    }
    return result
  }

  public func _loadValue<T>() -> T {
    return Builtin.load(ptr) as T
  }
}

struct _OpaqueMirror : _MirrorType {
  let data: _MagicMirrorData

  var value: Any { return data.value }
  var valueType: Any.Type { return data.valueType }
  var objectIdentifier: ObjectIdentifier? { return nil }
  var count: Int { return 0 }
  subscript(i: Int) -> (String, _MirrorType) {
    _preconditionFailure("no children")
  }
  var summary: String { return data.summary }
  var quickLookObject: PlaygroundQuickLook? { return nil }
  var disposition: _MirrorDisposition { return .Aggregate }
}

internal struct _TupleMirror : _MirrorType {
  let data: _MagicMirrorData

  var value: Any { return data.value }
  var valueType: Any.Type { return data.valueType }
  var objectIdentifier: ObjectIdentifier? { return nil }
  var count: Int {
    @_silgen_name("swift_TupleMirror_count")get
  }
  subscript(i: Int) -> (String, _MirrorType) {
    @_silgen_name("swift_TupleMirror_subscript")get
  }
  var summary: String { return "(\(count) elements)" }
  var quickLookObject: PlaygroundQuickLook? { return nil }
  var disposition: _MirrorDisposition { return .Tuple }
}

struct _StructMirror : _MirrorType {
  let data: _MagicMirrorData

  var value: Any { return data.value }
  var valueType: Any.Type { return data.valueType }
  var objectIdentifier: ObjectIdentifier? { return nil }
  var count: Int {
    @_silgen_name("swift_StructMirror_count")get
  }
  subscript(i: Int) -> (String, _MirrorType) {
    @_silgen_name("swift_StructMirror_subscript")get
  }

  var summary: String {
    return _typeName(valueType)
  }
  var quickLookObject: PlaygroundQuickLook? { return nil }
  var disposition: _MirrorDisposition { return .Struct }
}

struct _EnumMirror : _MirrorType {
  let data: _MagicMirrorData

  var value: Any { return data.value }
  var valueType: Any.Type { return data.valueType }
  var objectIdentifier: ObjectIdentifier? { return nil }
  var count: Int {
    @_silgen_name("swift_EnumMirror_count")get
  }
  var caseName: UnsafePointer<CChar> {
    @_silgen_name("swift_EnumMirror_caseName")get
  }
  subscript(i: Int) -> (String, _MirrorType) {
    @_silgen_name("swift_EnumMirror_subscript")get
  }
  var summary: String {
    let maybeCaseName = String.fromCString(self.caseName)
    let typeName = _typeName(valueType)
    if let caseName = maybeCaseName {
      return typeName + "." + caseName
    }
    return typeName
  }
  var quickLookObject: PlaygroundQuickLook? { return nil }
  var disposition: _MirrorDisposition { return .Enum }
}

@warn_unused_result
@_silgen_name("swift_ClassMirror_count")
func _getClassCount(_: _MagicMirrorData) -> Int

@warn_unused_result
@_silgen_name("swift_ClassMirror_subscript")
func _getClassChild(_: Int, _: _MagicMirrorData) -> (String, _MirrorType)

#if _runtime(_ObjC)
@warn_unused_result
@_silgen_name("swift_ClassMirror_quickLookObject")
public func _getClassPlaygroundQuickLook(
  data: _MagicMirrorData
) -> PlaygroundQuickLook?
#endif

struct _ClassMirror : _MirrorType {
  let data: _MagicMirrorData

  var value: Any { return data.value }
  var valueType: Any.Type { return data.valueType }
  var objectIdentifier: ObjectIdentifier? {
    return data._loadValue() as ObjectIdentifier
  }
  var count: Int {
    return _getClassCount(data)
  }
  subscript(i: Int) -> (String, _MirrorType) {
    return _getClassChild(i, data)
  }
  var summary: String {
    return _typeName(valueType)
  }
  var quickLookObject: PlaygroundQuickLook? {
#if _runtime(_ObjC)
    return _getClassPlaygroundQuickLook(data)
#else
    return nil
#endif
  }
  var disposition: _MirrorDisposition { return .Class }
}

struct _ClassSuperMirror : _MirrorType {
  let data: _MagicMirrorData

  var value: Any { return data.value }
  var valueType: Any.Type { return data.valueType }

  // Suppress the value identifier for super mirrors.
  var objectIdentifier: ObjectIdentifier? {
    return nil
  }
  var count: Int {
    return _getClassCount(data)
  }
  subscript(i: Int) -> (String, _MirrorType) {
    return _getClassChild(i, data)
  }
  var summary: String {
    return _typeName(data.metadata)
  }
  var quickLookObject: PlaygroundQuickLook? { return nil }
  var disposition: _MirrorDisposition { return .Class }
}

struct _MetatypeMirror : _MirrorType {
  let data: _MagicMirrorData

  var value: Any { return data.value }
  var valueType: Any.Type { return data.valueType }

  var objectIdentifier: ObjectIdentifier? {
    return data._loadValue() as ObjectIdentifier
  }

  var count: Int {
    return 0
  }
  subscript(i: Int) -> (String, _MirrorType) {
    _preconditionFailure("no children")
  }
  var summary: String {
    return _typeName(data._loadValue() as Any.Type)
  }
  var quickLookObject: PlaygroundQuickLook? { return nil }

  // Special disposition for types?
  var disposition: _MirrorDisposition { return .Aggregate }
}

@available(*, unavailable, message="call the 'Mirror(reflecting:)' initializer")
public func reflect<T>(x: T) -> _MirrorType {
  fatalError("unavailable function can't be called")
}

