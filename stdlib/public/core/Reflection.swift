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

/// Customizes the result of `reflect(x)`, where `x` is a conforming
/// type.
public protocol Reflectable {
  // The runtime has inappropriate knowledge of this protocol and how its
  // witness tables are laid out. Changing this protocol requires a
  // corresponding change to Reflection.cpp.

  /// Returns a mirror that reflects `self`.
  func getMirror() -> MirrorType
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
  /// Axiom: `a.uintValue == b.uintValue` iff `a == b`
  public var uintValue: UInt {
    return UInt(Builtin.ptrtoint_Word(value))
  }

  // FIXME: Better hashing algorithm
  /// The hash value.
  ///
  /// **Axiom:** `x == y` implies `x.hashValue == y.hashValue`
  ///
  /// - note: The hash value is not guaranteed to be stable across
  /// different invocations of the same program.  Do not persist the
  /// hash value across program runs.
  public var hashValue: Int {
    return Int(Builtin.ptrtoint_Word(value))
  }

  /// Construct an instance that uniquely identifies the class instance `x`.
  public init(_ x: AnyObject) {
    self.value = unsafeBitCast(x, Builtin.RawPointer.self)
  }

  /// Construct an instance that uniquely identifies the metatype `x`.
  public init(_ x: Any.Type) {
    self.value = unsafeBitCast(x, Builtin.RawPointer.self)
  }
}

public func <(lhs: ObjectIdentifier, rhs: ObjectIdentifier) -> Bool {
  return lhs.uintValue < rhs.uintValue
}

public func ==(x: ObjectIdentifier, y: ObjectIdentifier) -> Bool {
  return Bool(Builtin.cmp_eq_RawPointer(x.value, y.value))
}

/// The sum of types that can be used as a quick look representation.
public enum QuickLookObject {
  //
  // This type must be binary-compatible with the 'QuickLookObject' struct in
  // stdlib/Runtime/Reflection.mm, and 'QuickLookObject?' must be binary
  // compatible with 'OptionalQuickLookObject' from the same.
  //
  // NB: This type is somewhat carefully laid out to *suppress* enum layout
  // optimization so that it is easier to manufacture in the C++ runtime
  // implementation.

  /// Plain text.
  case Text(String)

  /// An integer numeric value.
  case Int(Int64)

  /// An unsigned integer numeric value.
  case UInt(UInt64)

  /// A single precision floating-point numeric value.
  case Float(Float32)

  /// A double precision floating-point numeric value.
  case Double(Float64)

  // FIXME: Uses an Any to avoid coupling a particular Cocoa type.
  /// An image.
  case Image(Any)

  // FIXME: Uses an Any to avoid coupling a particular Cocoa type.
  /// A sound.
  case Sound(Any)

  // FIXME: Uses an Any to avoid coupling a particular Cocoa type.
  /// A color.
  case Color(Any)

  // FIXME: Uses an Any to avoid coupling a particular Cocoa type.
  /// A bezier path.
  case BezierPath(Any)

  // FIXME: Uses an Any to avoid coupling a particular Cocoa type.
  /// An attributed string.
  case AttributedString(Any)

  /// A rectangle
  /// Uses explicit coordinates to avoid coupling a particular Cocoa type.
  case Rectangle(Float64,Float64,Float64,Float64)

  /// A point
  /// Uses explicit coordinates to avoid coupling a particular Cocoa type.
  case Point(Float64,Float64)

  /// A size
  /// Uses explicit coordinates to avoid coupling a particular Cocoa type.
  case Size(Float64,Float64)

  /// A logical value
  case Logical(Bool)

  /// A range
  /// Uses explicit values to avoid coupling a particular Cocoa type.
  case Range(UInt64, UInt64)

  /// A GUI view
  /// Uses an Any to avoid coupling a particular Cocoa type.
  case View(Any)

  /// A graphical sprite
  /// Uses an Any to avoid coupling a particular Cocoa type.
  case Sprite(Any)

  /// A Uniform Resource Locator
  case URL(String)

  /// Raw data that has already been encoded in a format the IDE understands.
  case _Raw([UInt8], String)
}

/// How children of this value should be presented in the IDE.
public enum MirrorDisposition {
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
  /// An Objective-C object imported in Swift
  case ObjCObject
}

/// The type returned by `reflect(x)`; supplies an API for runtime
/// reflection on `x`
public protocol MirrorType {
  /// The instance being reflected
  var value: Any { get }

  /// Identical to `value.dynamicType`
  var valueType: Any.Type { get }

  /// A unique identifier for `value` if it is a class instance; `nil`
  /// otherwise.
  var objectIdentifier: ObjectIdentifier? { get }

  /// The count of `value`'s logical children
  var count: Int { get }

  /// Get a name and mirror for the `i`th logical child.
  subscript(i: Int) -> (String, MirrorType) { get }

  /// A string description of `value`.
  var summary: String { get }

  /// A rich representation of `value` for an IDE, or `nil` if none is supplied.
  var quickLookObject: QuickLookObject? { get }

  /// How `value` should be presented in an IDE.
  var disposition: MirrorDisposition { get }
}

/// An entry point that can be called from C++ code to get the summary string
/// for an arbitrary object. The memory pointed to by "out" is initialized with
/// the summary string.
@asmname("swift_getSummary")
public // COMPILER_INTRINSIC
func _getSummary<T>(out: UnsafeMutablePointer<String>, x: T) {
  out.initialize(reflect(x).summary)
}

/// Produce a mirror for any value. If the value's type conforms to Reflectable,
/// invoke its getMirror() method; otherwise, fall back to an implementation
/// in the runtime that structurally reflects values of any type.
@asmname("swift_reflectAny")public func reflect<T>(x: T) -> MirrorType

/// Unsafely produce a mirror for a value in memory whose lifetime is
/// guaranteed by holding a strong reference to a heap object.
/// This lets containers with heap storage vend mirrors for their elements
/// without unnecessary copying of the underlying value.
@asmname("swift_unsafeReflectAny")func unsafeReflect<T>(
  owner: Builtin.NativeObject,
  ptr: UnsafeMutablePointer<T>
) -> MirrorType


/// Dump an object's contents using its mirror to the specified output stream.
public func dump<T, TargetStream : OutputStreamType>(
    x: T, inout _ targetStream: TargetStream,
    name: String? = nil, indent: Int = 0,
    maxDepth: Int = .max, maxItems: Int = .max
) -> T {
  var maxItemCounter = maxItems
  var visitedItems = [ObjectIdentifier : Int]()
  _dumpWithMirror(
    reflect(x), name, indent, maxDepth, &maxItemCounter, &visitedItems,
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
    mirror: MirrorType, _ name: String?, _ indent: Int, _ maxDepth: Int,
    inout _ maxItemCounter: Int,
    inout _ visitedItems: [ObjectIdentifier : Int],
    inout _ targetStream: TargetStream
) {
  if maxItemCounter <= 0 { return }
  --maxItemCounter

  for _ in 0..<indent { print(" ", &targetStream, appendNewline: false) }

  let count = mirror.count
  let bullet = count == 0    ? "-"
             : maxDepth <= 0 ? "▹" : "▿"
  print("\(bullet) ", &targetStream, appendNewline: false)

  if let nam = name {
    print("\(nam): ", &targetStream, appendNewline: false)
  }
  print(mirror.summary, &targetStream, appendNewline: false)

  if let id = mirror.objectIdentifier {
    if let previous = visitedItems[id] {
      print(" #\(previous)", &targetStream)
      return
    }
    let identifier = visitedItems.count
    visitedItems[id] = identifier
    print(" #\(identifier)", &targetStream, appendNewline: false)
  }

  print("", &targetStream)

  if maxDepth <= 0 { return }

  for i in 0..<count {
    if maxItemCounter <= 0 {
      for _ in 0..<(indent+4) { print(" ", &targetStream, appendNewline: false) }
      let remainder = count - i
      print("(\(remainder)", &targetStream, appendNewline: false)
      if i > 0 { print(" more", &targetStream, appendNewline: false) }
      if remainder == 1 {
        print(" child)", &targetStream)
      } else {
        print(" children)", &targetStream)
      }
      return
    }

    let (name, child) = mirror[i]
    _dumpWithMirror(child, name, indent + 2, maxDepth - 1,
                    &maxItemCounter, &visitedItems, &targetStream)
  }
}

// -- MirrorType implementations for basic data types

/// A mirror for a value that is represented as a simple value with no
/// children.
internal struct _LeafMirror<T>: MirrorType {
  let _value: T
  let summaryFunction: T -> String
  let quickLookFunction: T -> QuickLookObject?

  init(_ value: T, _ summaryFunction: T -> String,
       _ quickLookFunction: T -> QuickLookObject?) {
    self._value = value
    self.summaryFunction = summaryFunction
    self.quickLookFunction = quickLookFunction
  }

  var value: Any { return _value }
  var valueType: Any.Type { return value.dynamicType }
  var objectIdentifier: ObjectIdentifier? { return nil }
  var count: Int { return 0 }
  subscript(i: Int) -> (String, MirrorType) {
    _preconditionFailure("no children")
  }
  var summary: String { return summaryFunction(_value) }
  var quickLookObject: QuickLookObject? { return quickLookFunction(_value) }
  var disposition: MirrorDisposition { return .Aggregate }
}

// -- Implementation details for the runtime's MirrorType implementation

@asmname("swift_MagicMirrorData_summary")
func _swift_MagicMirrorData_summaryImpl(
  metadata: Any.Type, _ result: UnsafeMutablePointer<String>
)

public struct _MagicMirrorData {
  let owner: Builtin.NativeObject
  let ptr: Builtin.RawPointer
  let metadata: Any.Type

  var value: Any {
    @asmname("swift_MagicMirrorData_value")get
  }
  var valueType: Any.Type {
    @asmname("swift_MagicMirrorData_valueType")get
  }

  public var objcValue: Any {
    @asmname("swift_MagicMirrorData_objcValue")get
  }
  public var objcValueType: Any.Type {
    @asmname("swift_MagicMirrorData_objcValueType")get
  }

  var summary: String {
    var (_, result) = _withUninitializedString {
      _swift_MagicMirrorData_summaryImpl(self.metadata, $0)
    }
    return result
  }

  public func _loadValue<T>() -> T {
    return Builtin.load(ptr) as T
  }
}

struct _OpaqueMirror : MirrorType {
  let data: _MagicMirrorData

  var value: Any { return data.value }
  var valueType: Any.Type { return data.valueType }
  var objectIdentifier: ObjectIdentifier? { return nil }
  var count: Int { return 0 }
  subscript(i: Int) -> (String, MirrorType) {
    _preconditionFailure("no children")
  }
  var summary: String { return data.summary }
  var quickLookObject: QuickLookObject? { return nil }
  var disposition: MirrorDisposition { return .Aggregate }
}

internal struct _TupleMirror : MirrorType {
  let data: _MagicMirrorData

  var value: Any { return data.value }
  var valueType: Any.Type { return data.valueType }
  var objectIdentifier: ObjectIdentifier? { return nil }
  var count: Int {
    @asmname("swift_TupleMirror_count")get
  }
  subscript(i: Int) -> (String, MirrorType) {
    @asmname("swift_TupleMirror_subscript")get
  }
  var summary: String { return "(\(count) elements)" }
  var quickLookObject: QuickLookObject? { return nil }
  var disposition: MirrorDisposition { return .Tuple }
}

struct _StructMirror : MirrorType {
  let data: _MagicMirrorData

  var value: Any { return data.value }
  var valueType: Any.Type { return data.valueType }
  var objectIdentifier: ObjectIdentifier? { return nil }
  var count: Int {
    @asmname("swift_StructMirror_count")get
  }
  subscript(i: Int) -> (String, MirrorType) {
    @asmname("swift_StructMirror_subscript")get
  }

  var summary: String {
    return _stdlib_getDemangledTypeName(value)
  }
  var quickLookObject: QuickLookObject? { return nil }
  var disposition: MirrorDisposition { return .Struct }
}

@asmname("swift_ClassMirror_count")
func _getClassCount(_MagicMirrorData) -> Int
@asmname("swift_ClassMirror_subscript")
func _getClassChild(Int, _MagicMirrorData) -> (String, MirrorType)

#if _runtime(_ObjC)
@asmname("swift_ClassMirror_quickLookObject")public
func _getClassQuickLookObject(data: _MagicMirrorData) -> QuickLookObject?
#endif

struct _ClassMirror : MirrorType {
  let data: _MagicMirrorData

  var value: Any { return data.value }
  var valueType: Any.Type { return data.valueType }
  var objectIdentifier: ObjectIdentifier? {
    return data._loadValue() as ObjectIdentifier
  }
  var count: Int {
    return _getClassCount(data)
  }
  subscript(i: Int) -> (String, MirrorType) {
    return _getClassChild(i, data)
  }
  var summary: String {
    return _stdlib_getDemangledTypeName(value)
  }
  var quickLookObject: QuickLookObject? {
#if _runtime(_ObjC)
    return _getClassQuickLookObject(data)
#else
    return .None
#endif
  }
  var disposition: MirrorDisposition { return .Class }
}

struct _ClassSuperMirror : MirrorType {
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
  subscript(i: Int) -> (String, MirrorType) {
    return _getClassChild(i, data)
  }
  var summary: String {
    return _stdlib_getDemangledTypeName(value)
  }
  var quickLookObject: QuickLookObject? { return nil }
  var disposition: MirrorDisposition { return .Class }
}

struct _MetatypeMirror : MirrorType {
  let data: _MagicMirrorData

  var value: Any { return data.value }
  var valueType: Any.Type { return data.valueType }

  var objectIdentifier: ObjectIdentifier? {
    return data._loadValue() as ObjectIdentifier
  }

  var count: Int {
    return 0
  }
  subscript(i: Int) -> (String, MirrorType) {
    _preconditionFailure("no children")
  }
  var summary: String {
    return _typeName(data._loadValue() as Any.Type)
  }
  var quickLookObject: QuickLookObject? { return nil }

  // Special disposition for types?
  var disposition: MirrorDisposition { return .Aggregate }
}
