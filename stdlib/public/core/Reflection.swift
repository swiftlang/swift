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
  func _getMirror() -> _Mirror
}

/// A unique identifier for a class instance or metatype.
///
/// In Swift, only class instances and metatypes have unique identities. There
/// is no notion of identity for structs, enums, functions, or tuples.
public struct ObjectIdentifier : Hashable, Comparable {
  internal let _value: Builtin.RawPointer

  // FIXME: Better hashing algorithm
  /// The hash value.
  ///
  /// **Axiom:** `x == y` implies `x.hashValue == y.hashValue`.
  ///
  /// - Note: The hash value is not guaranteed to be stable across
  ///   different invocations of the same program.  Do not persist the
  ///   hash value across program runs.
  public var hashValue: Int {
    return Int(Builtin.ptrtoint_Word(_value))
  }

  /// Construct an instance that uniquely identifies the class instance `x`.
  public init(_ x: AnyObject) {
    self._value = Builtin.bridgeToRawPointer(x)
  }

  /// Construct an instance that uniquely identifies the metatype `x`.
  public init(_ x: Any.Type) {
    self._value = unsafeBitCast(x, to: Builtin.RawPointer.self)
  }
}

@warn_unused_result
public func <(lhs: ObjectIdentifier, rhs: ObjectIdentifier) -> Bool {
  return UInt(lhs) < UInt(rhs)
}

@warn_unused_result
public func ==(x: ObjectIdentifier, y: ObjectIdentifier) -> Bool {
  return Bool(Builtin.cmp_eq_RawPointer(x._value, y._value))
}

extension UInt {
  /// Create a `UInt` that captures the full value of `objectID`.
  public init(_ objectID: ObjectIdentifier) {
    self.init(Builtin.ptrtoint_Word(objectID._value))
  }
}

extension Int {
  /// Create an `Int` that captures the full value of `objectID`.
  public init(_ objectID: ObjectIdentifier) {
    self.init(bitPattern: UInt(objectID))
  }
}

/// How children of this value should be presented in the IDE.
public enum _MirrorDisposition {
  /// As a struct.
  case `struct`
  /// As a class.
  case `class`
  /// As an enum.
  case `enum`
  /// As a tuple.
  case tuple
  /// As a miscellaneous aggregate with a fixed set of children.
  case aggregate
  /// As a container that is accessed by index.
  case indexContainer
  /// As a container that is accessed by key.
  case keyContainer
  /// As a container that represents membership of its values.
  case membershipContainer
  /// As a miscellaneous container with a variable number of children.
  case container
  /// An Optional which can have either zero or one children.
  case optional
  /// An Objective-C object imported in Swift.
  case objCObject
}

/// The type returned by `_reflect(x)`; supplies an API for runtime
/// reflection on `x`.
public protocol _Mirror {
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
  subscript(i: Int) -> (String, _Mirror) { get }

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
func _getSummary<T>(_ out: UnsafeMutablePointer<String>, x: T) {
  out.initialize(with: String(reflecting: x))
}

/// Produce a mirror for any value. If the value's type conforms to
/// `_Reflectable`, invoke its `_getMirror()` method; otherwise, fall back
/// to an implementation in the runtime that structurally reflects values
/// of any type.
@warn_unused_result
@_silgen_name("swift_reflectAny")
internal func _reflect<T>(_ x: T) -> _Mirror

/// Dump an object's contents using its mirror to the specified output stream.
@discardableResult
public func dump<T, TargetStream : OutputStream>(
  _ value: T,
  to target: inout TargetStream,
  name: String? = nil,
  indent: Int = 0,
  maxDepth: Int = .max,
  maxItems: Int = .max
) -> T {
  var maxItemCounter = maxItems
  var visitedItems = [ObjectIdentifier : Int]()
  target._lock()
  defer { target._unlock() }
  _dump_unlocked(
    value,
    to: &target,
    name: name,
    indent: indent,
    maxDepth: maxDepth,
    maxItemCounter: &maxItemCounter,
    visitedItems: &visitedItems)
  return value
}

/// Dump an object's contents using its mirror to standard output.
@discardableResult
public func dump<T>(
  _ value: T,
  name: String? = nil,
  indent: Int = 0,
  maxDepth: Int = .max,
  maxItems: Int = .max
) -> T {
  var stdoutStream = _Stdout()
  return dump(
    value,
    to: &stdoutStream,
    name: name,
    indent: indent,
    maxDepth: maxDepth,
    maxItems: maxItems)
}

/// Dump an object's contents. User code should use dump().
internal func _dump_unlocked<TargetStream : OutputStream>(
  _ value: Any,
  to target: inout TargetStream,
  name: String?,
  indent: Int,
  maxDepth: Int,
  maxItemCounter: inout Int,
  visitedItems: inout [ObjectIdentifier : Int]
) {
  guard maxItemCounter > 0 else { return }
  maxItemCounter -= 1

  for _ in 0..<indent { target.write(" ") }

  let mirror = Mirror(reflecting: value)
  let count = mirror.children.count
  let bullet = count == 0    ? "-"
             : maxDepth <= 0 ? "▹" : "▿"
  target.write(bullet)
  target.write(" ")

  if let nam = name {
    target.write(nam)
    target.write(": ")
  }
  // This takes the place of the old mirror API's 'summary' property
  _dumpPrint_unlocked(value, mirror, &target)

  let id: ObjectIdentifier?
  if let classInstance = value as? AnyObject where value.dynamicType is AnyObject.Type {
    // Object is a class (but not an ObjC-bridged struct)
    id = ObjectIdentifier(classInstance)
  } else if let metatypeInstance = value as? Any.Type {
    // Object is a metatype
    id = ObjectIdentifier(metatypeInstance)
  } else {
    id = nil
  }
  if let theId = id {
    if let previous = visitedItems[theId] {
      target.write(" #")
      _print_unlocked(previous, &target)
      target.write("\n")
      return
    }
    let identifier = visitedItems.count
    visitedItems[theId] = identifier
    target.write(" #")
    _print_unlocked(identifier, &target)
  }

  target.write("\n")

  guard maxDepth > 0 else { return }

  if let superclassMirror = mirror.superclassMirror {
    _dumpSuperclass_unlocked(
      mirror: superclassMirror,
      to: &target,
      indent: indent + 2,
      maxDepth: maxDepth - 1,
      maxItemCounter: &maxItemCounter,
      visitedItems: &visitedItems)
  }

  var currentIndex = mirror.children.startIndex
  for i in 0..<count {
    if maxItemCounter <= 0 {
      for _ in 0..<(indent+4) {
        _print_unlocked(" ", &target)
      }
      let remainder = count - i
      target.write("(")
      _print_unlocked(remainder, &target)
      if i > 0 { target.write(" more") }
      if remainder == 1 {
        target.write(" child)\n")
      } else {
        target.write(" children)\n")
      }
      return
    }

    let (name, child) = mirror.children[currentIndex]
    mirror.children.formIndex(after: &currentIndex)
    _dump_unlocked(
      child,
      to: &target,
      name: name,
      indent: indent + 2,
      maxDepth: maxDepth - 1,
      maxItemCounter: &maxItemCounter,
      visitedItems: &visitedItems)
  }
}

/// Dump information about an object's superclass, given a mirror reflecting
/// that superclass.
internal func _dumpSuperclass_unlocked<TargetStream : OutputStream>(
  mirror: Mirror,
  to target: inout TargetStream,
  indent: Int,
  maxDepth: Int,
  maxItemCounter: inout Int,
  visitedItems: inout [ObjectIdentifier : Int]
) {
  guard maxItemCounter > 0 else { return }
  maxItemCounter -= 1

  for _ in 0..<indent { target.write(" ") }

  let count = mirror.children.count
  let bullet = count == 0    ? "-"
             : maxDepth <= 0 ? "▹" : "▿"
  target.write(bullet)
  target.write(" super: ")
  _debugPrint_unlocked(mirror.subjectType, &target)
  target.write("\n")

  guard maxDepth > 0 else { return }

  if let superclassMirror = mirror.superclassMirror {
    _dumpSuperclass_unlocked(
      mirror: superclassMirror,
      to: &target,
      indent: indent + 2,
      maxDepth: maxDepth - 1,
      maxItemCounter: &maxItemCounter,
      visitedItems: &visitedItems)
  }

  var currentIndex = mirror.children.startIndex
  for i in 0..<count {
    if maxItemCounter <= 0 {
      for _ in 0..<(indent+4) {
        target.write(" ")
      }
      let remainder = count - i
      target.write("(")
      _print_unlocked(remainder, &target)
      if i > 0 { target.write(" more") }
      if remainder == 1 {
        target.write(" child)\n")
      } else {
        target.write(" children)\n")
      }
      return
    }

    let (name, child) = mirror.children[currentIndex]
    mirror.children.formIndex(after: &currentIndex)
    _dump_unlocked(
      child,
      to: &target,
      name: name,
      indent: indent + 2,
      maxDepth: maxDepth - 1,
      maxItemCounter: &maxItemCounter,
      visitedItems: &visitedItems)
  }
}

// -- Implementation details for the runtime's _Mirror implementation

@_silgen_name("swift_MagicMirrorData_summary")
func _swift_MagicMirrorData_summaryImpl(
  _ metadata: Any.Type, _ result: UnsafeMutablePointer<String>
)

@_fixed_layout
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

struct _OpaqueMirror : _Mirror {
  let data: _MagicMirrorData

  var value: Any { return data.value }
  var valueType: Any.Type { return data.valueType }
  var objectIdentifier: ObjectIdentifier? { return nil }
  var count: Int { return 0 }
  subscript(i: Int) -> (String, _Mirror) {
    _preconditionFailure("no children")
  }
  var summary: String { return data.summary }
  var quickLookObject: PlaygroundQuickLook? { return nil }
  var disposition: _MirrorDisposition { return .aggregate }
}

@warn_unused_result
@_silgen_name("swift_TupleMirror_count")
func _getTupleCount(_: _MagicMirrorData) -> Int

// Like the other swift_*Mirror_subscript functions declared here and
// elsewhere, this is implemented in the runtime.  The Swift CC would
// normally require the String to be returned directly and the _Mirror
// indirectly.  However, Clang isn't currently capable of doing that
// reliably because the size of String exceeds the normal direct-return
// ABI rules on most platforms.  Therefore, we make this function generic,
// which has the disadvantage of passing the String type metadata as an
// extra argument, but does force the string to be returned indirectly.
@warn_unused_result
@_silgen_name("swift_TupleMirror_subscript")
func _getTupleChild<T>(_: Int, _: _MagicMirrorData) -> (T, _Mirror)

internal struct _TupleMirror : _Mirror {
  let data: _MagicMirrorData

  var value: Any { return data.value }
  var valueType: Any.Type { return data.valueType }
  var objectIdentifier: ObjectIdentifier? { return nil }
  var count: Int {
    return _getTupleCount(data)
  }
  subscript(i: Int) -> (String, _Mirror) {
    return _getTupleChild(i, data)
  }
  var summary: String { return "(\(count) elements)" }
  var quickLookObject: PlaygroundQuickLook? { return nil }
  var disposition: _MirrorDisposition { return .tuple }
}

@warn_unused_result
@_silgen_name("swift_StructMirror_count")
func _getStructCount(_: _MagicMirrorData) -> Int

@warn_unused_result
@_silgen_name("swift_StructMirror_subscript")
func _getStructChild<T>(_: Int, _: _MagicMirrorData) -> (T, _Mirror)

struct _StructMirror : _Mirror {
  let data: _MagicMirrorData

  var value: Any { return data.value }
  var valueType: Any.Type { return data.valueType }
  var objectIdentifier: ObjectIdentifier? { return nil }
  var count: Int {
    return _getStructCount(data)
  }
  subscript(i: Int) -> (String, _Mirror) {
    return _getStructChild(i, data)
  }

  var summary: String {
    return _typeName(valueType)
  }
  var quickLookObject: PlaygroundQuickLook? { return nil }
  var disposition: _MirrorDisposition { return .`struct` }
}

@warn_unused_result
@_silgen_name("swift_EnumMirror_count")
func _getEnumCount(_: _MagicMirrorData) -> Int

@warn_unused_result
@_silgen_name("swift_EnumMirror_subscript")
func _getEnumChild<T>(_: Int, _: _MagicMirrorData) -> (T, _Mirror)

@warn_unused_result
@_silgen_name("swift_EnumMirror_caseName")
func _swift_EnumMirror_caseName(
    _ data: _MagicMirrorData) -> UnsafePointer<CChar>

struct _EnumMirror : _Mirror {
  let data: _MagicMirrorData

  var value: Any { return data.value }
  var valueType: Any.Type { return data.valueType }
  var objectIdentifier: ObjectIdentifier? { return nil }
  var count: Int {
    return _getEnumCount(data)
  }
  var caseName: UnsafePointer<CChar> {
    return _swift_EnumMirror_caseName(data)
  }
  subscript(i: Int) -> (String, _Mirror) {
    return _getEnumChild(i, data)
  }

  var summary: String {
    let maybeCaseName = String(validatingUTF8: self.caseName)
    let typeName = _typeName(valueType)
    if let caseName = maybeCaseName {
      return typeName + "." + caseName
    }
    return typeName
  }
  var quickLookObject: PlaygroundQuickLook? { return nil }
  var disposition: _MirrorDisposition { return .`enum` }
}

@warn_unused_result
@_silgen_name("swift_ClassMirror_count")
func _getClassCount(_: _MagicMirrorData) -> Int

@warn_unused_result
@_silgen_name("swift_ClassMirror_subscript")
func _getClassChild<T>(_: Int, _: _MagicMirrorData) -> (T, _Mirror)

#if _runtime(_ObjC)
@_silgen_name("swift_ClassMirror_quickLookObject")
public func _swift_ClassMirror_quickLookObject(_: _MagicMirrorData) -> AnyObject

@_silgen_name("swift_isKind")
func _swift_isKind(_ object: AnyObject, of: AnyObject) -> Bool

func _isKind(_ object: AnyObject, of: String) -> Bool {
  return _swift_isKind(object, of: _bridgeToObjectiveC(of)!)
}

func _getClassPlaygroundQuickLook(_ object: AnyObject) -> PlaygroundQuickLook? {
  if _isKind(object, of: "NSNumber") {
    let number: _NSNumber = unsafeBitCast(object, to: _NSNumber.self)
    switch UInt8(number.objCType[0]) {
    case UInt8(ascii: "d"):
      return .double(number.doubleValue)
    case UInt8(ascii: "f"):
      return .float(number.floatValue)
    case UInt8(ascii: "Q"):
      return .uInt(number.unsignedLongLongValue)
    default:
      return .int(number.longLongValue)
    }
  } else if _isKind(object, of: "NSAttributedString") {
    return .attributedString(object)
  } else if _isKind(object, of: "NSImage") ||
            _isKind(object, of: "UIImage") ||
            _isKind(object, of: "NSImageView") ||
            _isKind(object, of: "UIImageView") ||
            _isKind(object, of: "CIImage") ||
            _isKind(object, of: "NSBitmapImageRep") {
    return .image(object)
  } else if _isKind(object, of: "NSColor") ||
            _isKind(object, of: "UIColor") {
    return .color(object)
  } else if _isKind(object, of: "NSBezierPath") ||
            _isKind(object, of: "UIBezierPath") {
    return .bezierPath(object)
  } else if _isKind(object, of: "NSString") {
    return .text(_forceBridgeFromObjectiveC(object, String.self))
  }

  return .none
}

#endif

struct _ClassMirror : _Mirror {
  let data: _MagicMirrorData

  var value: Any { return data.value }
  var valueType: Any.Type { return data.valueType }
  var objectIdentifier: ObjectIdentifier? {
    return data._loadValue() as ObjectIdentifier
  }
  var count: Int {
    return _getClassCount(data)
  }
  subscript(i: Int) -> (String, _Mirror) {
    return _getClassChild(i, data)
  }
  var summary: String {
    return _typeName(valueType)
  }
  var quickLookObject: PlaygroundQuickLook? {
#if _runtime(_ObjC)
    let object = _swift_ClassMirror_quickLookObject(data)
    return _getClassPlaygroundQuickLook(object)
#else
    return nil
#endif
  }
  var disposition: _MirrorDisposition { return .`class` }
}

struct _ClassSuperMirror : _Mirror {
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
  subscript(i: Int) -> (String, _Mirror) {
    return _getClassChild(i, data)
  }
  var summary: String {
    return _typeName(data.metadata)
  }
  var quickLookObject: PlaygroundQuickLook? { return nil }
  var disposition: _MirrorDisposition { return .`class` }
}

struct _MetatypeMirror : _Mirror {
  let data: _MagicMirrorData

  var value: Any { return data.value }
  var valueType: Any.Type { return data.valueType }

  var objectIdentifier: ObjectIdentifier? {
    return data._loadValue() as ObjectIdentifier
  }

  var count: Int {
    return 0
  }
  subscript(i: Int) -> (String, _Mirror) {
    _preconditionFailure("no children")
  }
  var summary: String {
    return _typeName(data._loadValue() as Any.Type)
  }
  var quickLookObject: PlaygroundQuickLook? { return nil }

  // Special disposition for types?
  var disposition: _MirrorDisposition { return .aggregate }
}

extension ObjectIdentifier {
  @available(*, unavailable, message: "use the 'UInt(_:)' initializer")
  public var uintValue: UInt {
    Builtin.unreachable()
  }
}
