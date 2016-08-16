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

  /// Identical to `type(of: value)`.
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
@_silgen_name("swift_getSummary")
public // COMPILER_INTRINSIC
func _getSummary<T>(_ out: UnsafeMutablePointer<String>, x: T) {
  out.initialize(to: String(reflecting: x))
}

/// Produce a mirror for any value.  The runtime produces a mirror that
/// structurally reflects values of any type.
@_silgen_name("swift_reflectAny")
internal func _reflect<T>(_ x: T) -> _Mirror

/// Dumps an object's contents using its mirror to the specified output stream.
@discardableResult
public func dump<T, TargetStream : TextOutputStream>(
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

/// Dumps an object's contents using its mirror to standard output.
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
internal func _dump_unlocked<TargetStream : TextOutputStream>(
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
  if type(of: value) is AnyObject.Type {
    // Object is a class (but not an ObjC-bridged struct)
    id = ObjectIdentifier(_unsafeDowncastToAnyObject(fromAny: value))
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
internal func _dumpSuperclass_unlocked<TargetStream : TextOutputStream>(
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

  public func _loadValue<T>(ofType _: T.Type) -> T {
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

@_silgen_name("swift_StructMirror_count")
func _getStructCount(_: _MagicMirrorData) -> Int

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

@_silgen_name("swift_EnumMirror_count")
func _getEnumCount(_: _MagicMirrorData) -> Int

@_silgen_name("swift_EnumMirror_subscript")
func _getEnumChild<T>(_: Int, _: _MagicMirrorData) -> (T, _Mirror)

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

@_silgen_name("swift_ClassMirror_count")
func _getClassCount(_: _MagicMirrorData) -> Int

@_silgen_name("swift_ClassMirror_subscript")
func _getClassChild<T>(_: Int, _: _MagicMirrorData) -> (T, _Mirror)

#if _runtime(_ObjC)
@_silgen_name("swift_ClassMirror_quickLookObject")
public func _swift_ClassMirror_quickLookObject(_: _MagicMirrorData) -> AnyObject

@_silgen_name("_swift_stdlib_NSObject_isKindOfClass")
internal func _swift_NSObject_isImpl(_ object: AnyObject, kindOf: AnyObject) -> Bool

internal func _is(_ object: AnyObject, kindOf `class`: String) -> Bool {
  return _swift_NSObject_isImpl(object, kindOf: `class` as AnyObject)
}

func _getClassPlaygroundQuickLook(_ object: AnyObject) -> PlaygroundQuickLook? {
  if _is(object, kindOf: "NSNumber") {
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
  } else if _is(object, kindOf: "NSAttributedString") {
    return .attributedString(object)
  } else if _is(object, kindOf: "NSImage") ||
            _is(object, kindOf: "UIImage") ||
            _is(object, kindOf: "NSImageView") ||
            _is(object, kindOf: "UIImageView") ||
            _is(object, kindOf: "CIImage") ||
            _is(object, kindOf: "NSBitmapImageRep") {
    return .image(object)
  } else if _is(object, kindOf: "NSColor") ||
            _is(object, kindOf: "UIColor") {
    return .color(object)
  } else if _is(object, kindOf: "NSBezierPath") ||
            _is(object, kindOf: "UIBezierPath") {
    return .bezierPath(object)
  } else if _is(object, kindOf: "NSString") {
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
    return data._loadValue(ofType: ObjectIdentifier.self)
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
    return data._loadValue(ofType: ObjectIdentifier.self)
  }

  var count: Int {
    return 0
  }
  subscript(i: Int) -> (String, _Mirror) {
    _preconditionFailure("no children")
  }
  var summary: String {
    return _typeName(data._loadValue(ofType: Any.Type.self))
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
