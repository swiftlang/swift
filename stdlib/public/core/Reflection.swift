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
  out.initialize(String(reflecting: x))
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
  targetStream._lock()
  defer { targetStream._unlock() }
  _dumpObject_unlocked(
    x, name, indent, maxDepth, &maxItemCounter, &visitedItems,
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

/// Dump an object's contents. User code should use dump().
internal func _dumpObject_unlocked<TargetStream : OutputStreamType>(
    object: Any, _ name: String?, _ indent: Int, _ maxDepth: Int,
    inout _ maxItemCounter: Int,
    inout _ visitedItems: [ObjectIdentifier : Int],
    inout _ targetStream: TargetStream
) {
  guard maxItemCounter > 0 else { return }
  maxItemCounter -= 1

  for _ in 0..<indent { targetStream.write(" ") }

  let mirror = Mirror(reflecting: object)
  let count = mirror.children.count
  let bullet = count == 0    ? "-"
             : maxDepth <= 0 ? "▹" : "▿"
  targetStream.write(bullet)
  targetStream.write(" ")

  if let nam = name {
    targetStream.write(nam)
    targetStream.write(": ")
  }
  // This takes the place of the old mirror API's 'summary' property
  _dumpPrint_unlocked(object, mirror, &targetStream)

  let id: ObjectIdentifier?
  if let classInstance = object as? AnyObject where object.dynamicType is AnyObject.Type {
    // Object is a class (but not an ObjC-bridged struct)
    id = ObjectIdentifier(classInstance)
  } else if let metatypeInstance = object as? Any.Type {
    // Object is a metatype
    id = ObjectIdentifier(metatypeInstance)
  } else {
    id = nil
  }
  if let theId = id {
    if let previous = visitedItems[theId] {
      targetStream.write(" #")
      _print_unlocked(previous, &targetStream)
      targetStream.write("\n")
      return
    }
    let identifier = visitedItems.count
    visitedItems[theId] = identifier
    targetStream.write(" #")
    _print_unlocked(identifier, &targetStream)
  }

  targetStream.write("\n")

  guard maxDepth > 0 else { return }

  if let superclassMirror = mirror.superclassMirror() {
    _dumpSuperclass_unlocked(superclassMirror, indent + 2, maxDepth - 1, &maxItemCounter, &visitedItems, &targetStream)
  }

  var currentIndex = mirror.children.startIndex
  for i in 0..<count {
    if maxItemCounter <= 0 {
      for _ in 0..<(indent+4) {
        _print_unlocked(" ", &targetStream)
      }
      let remainder = count - i
      targetStream.write("(")
      _print_unlocked(remainder, &targetStream)
      if i > 0 { targetStream.write(" more") }
      if remainder == 1 {
        targetStream.write(" child)\n")
      } else {
        targetStream.write(" children)\n")
      }
      return
    }

    let (name, child) = mirror.children[currentIndex]
    currentIndex = currentIndex.successor()
    _dumpObject_unlocked(child, name, indent + 2, maxDepth - 1,
                         &maxItemCounter, &visitedItems, &targetStream)
  }
}

/// Dump information about an object's superclass, given a mirror reflecting
/// that superclass.
internal func _dumpSuperclass_unlocked<TargetStream : OutputStreamType>(
    mirror: Mirror, _ indent: Int, _ maxDepth: Int,
    inout _ maxItemCounter: Int,
    inout _ visitedItems: [ObjectIdentifier : Int],
    inout _ targetStream: TargetStream
) {
  guard maxItemCounter > 0 else { return }
  maxItemCounter -= 1

  for _ in 0..<indent { targetStream.write(" ") }

  let count = mirror.children.count
  let bullet = count == 0    ? "-"
             : maxDepth <= 0 ? "▹" : "▿"
  targetStream.write(bullet)
  targetStream.write(" super: ")
  _debugPrint_unlocked(mirror.subjectType, &targetStream)
  targetStream.write("\n")

  guard maxDepth > 0 else { return }

  if let superclassMirror = mirror.superclassMirror() {
    _dumpSuperclass_unlocked(superclassMirror, indent + 2, maxDepth - 1,
                             &maxItemCounter, &visitedItems, &targetStream)
  }

  var currentIndex = mirror.children.startIndex
  for i in 0..<count {
    if maxItemCounter <= 0 {
      for _ in 0..<(indent+4) {
        targetStream.write(" ")
      }
      let remainder = count - i
      targetStream.write("(")
      _print_unlocked(remainder, &targetStream)
      if i > 0 { targetStream.write(" more") }
      if remainder == 1 {
        targetStream.write(" child)\n")
      } else {
        targetStream.write(" children)\n")
      }
      return
    }

    let (name, child) = mirror.children[currentIndex]
    currentIndex = currentIndex.successor()
    _dumpObject_unlocked(child, name, indent + 2, maxDepth - 1,
                         &maxItemCounter, &visitedItems, &targetStream)
  }
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
    return _subscript_get(i)
  }
  @_silgen_name("swift_TupleMirror_subscript")
  func _subscript_get<T>(i: Int) -> (T, _MirrorType)

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
    return _subscript_get(i)
  }
  @_silgen_name("swift_StructMirror_subscript")
  func _subscript_get<T>(i: Int) -> (T, _MirrorType)

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
    return _subscript_get(i)
  }
  @_silgen_name("swift_EnumMirror_subscript")
  func _subscript_get<T>(i: Int) -> (T, _MirrorType)

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

// Like the other swift_*Mirror_subscript functions declared here and
// elsewhere, this is implemented in the runtime.  The Swift CC would
// normally require the String to be returned directly and the _MirrorType
// indirectly.  However, Clang isn't currently capable of doing that
// reliably because the size of String exceeds the normal direct-return
// ABI rules on most platforms.  Therefore, we make this function generic,
// which has the disadvantage of passing the String type metadata as an
// extra argument, but does force the string to be returned indirectly.
@warn_unused_result
@_silgen_name("swift_ClassMirror_subscript")
func _getClassChild<T>(_: Int, _: _MagicMirrorData) -> (T, _MirrorType)

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

