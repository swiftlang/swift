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
/// clients to recognize cycles in the object graph.
///
/// In Swift, only class instances have unique identities. There is no notion
/// of identity for structs, enums, or tuples.
struct ObjectIdentifier : Hashable {
  let value: Builtin.RawPointer

  func uintValue() -> UInt {
    return UInt(Builtin.ptrtoint_Word(value))
  }

  // FIXME: Better hashing algorithm
  var hashValue: Int {
    return Int(Builtin.ptrtoint_Word(value))
  }

  init(_ x: AnyObject) {
    self.value = reinterpretCast(x)
  }
}
func ==(x: ObjectIdentifier, y: ObjectIdentifier) -> Bool {
  return Bool(Builtin.cmp_eq_RawPointer(x.value, y.value))
}

/// The sum of types that can be used as a quick look representation.
///
/// This type must be binary-compatible with the 'QuickLookObject' struct in
/// stdlib/Runtime/Reflection.mm, and 'QuickLookObject?' must be binary
/// compatible with 'OptionalQuickLookObject' from the same.
///
/// NB: This type is somewhat carefully laid out to *suppress* enum layout
/// optimization so that it is easier to manufacture in the C++ runtime
/// implementation.
enum QuickLookObject {
  /// Plain text.
  case Text(String)

  /// An integer numeric value.
  case Int(Int64)

  /// An unsigned integer numeric value.
  case UInt(UInt64)

  /// A floating-point numeric value.
  case Float(Double)

  /// An image.
  /// FIXME: Uses an Any to avoid coupling a particular Cocoa type.
  case Image(Any)

  /// A sound.
  /// FIXME: Uses an Any to avoid coupling a particular Cocoa type.
  case Sound(Any)

  /// A color.
  /// FIXME: Uses an Any to avoid coupling a particular Cocoa type.
  case Color(Any)

  /// A bezier path.
  /// FIXME: Uses an Any to avoid coupling a particular Cocoa type.
  case BezierPath(Any)
  
  /// An attributed string.
  /// FIXME: Uses an Any to avoid coupling a particular Cocoa type.
  case AttributedString(Any)
  
  /// A rectangle
  /// Uses explicit coordinates to avoid coupling a particular Cocoa type.
  case Rectangle(Double,Double,Double,Double)
  
  /// A point
  /// Uses explicit coordinates to avoid coupling a particular Cocoa type.
  case Point(Double,Double)
  
  /// A size
  /// Uses explicit coordinates to avoid coupling a particular Cocoa type.
  case Size(Double,Double)
  
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
  case _Raw(UInt8[], String)
}

/// How children of this value should be presented in the IDE.
enum MirrorDisposition {
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
}

/// A protocol that provides a reflection interface to an underlying value.
protocol Mirror {
  /// Copy the value out as an Any.
  var value: Any { get }

  /// Get the type of the value.
  var valueType: Any.Type { get }

  /// Get the unique identifier for this value, if it has one.
  /// Always returns Some value for class instances, and always returns None
  /// for value types.
  var objectIdentifier: ObjectIdentifier? { get }

  /// Get the number of logical children this value has.
  var count: Int { get }

  /// Get a mirror for one of this value's children.
  ///
  /// Returns a pair of the child's name and its mirror.
  subscript(i: Int) -> (String, Mirror) { get }

  /// Get a string description of this value.
  var summary: String { get }

  /// Get a rich representation of this value for the IDE, if it has one.
  var quickLookObject: QuickLookObject? { get }

  /// Get the disposition of the value.
  var disposition: MirrorDisposition { get }
}

/// An entry point that can be called from C++ code to get the summary string
/// for an arbitrary object. The memory pointed to by "out" is initialized with
/// the summary string.
@asmname("swift_getSummary") func _getSummary<T>(out: UnsafePointer<String>,
                                                 x: T) {
  out.initialize(reflect(x).summary)
}

/// Produce a mirror for any value. If the value's type conforms to Reflectable,
/// invoke its getMirror() method; otherwise, fall back to an implementation
/// in the runtime that structurally reflects values of any type.
@asmname("swift_reflectAny") func reflect<T>(x: T) -> Mirror

/// Unsafely produce a mirror for a value in memory whose lifetime is
/// guaranteed by holding a strong reference to a heap object.
/// This lets containers with heap storage vend mirrors for their elements
/// without unnecessary copying of the underlying value.
@asmname("swift_unsafeReflectAny") func unsafeReflect<T>(
  owner: Builtin.NativeObject,
  ptr: UnsafePointer<T>
) -> Mirror


/// Dump an object's contents using its mirror to the specified output stream.
func dump<T, TargetStream : OutputStream>(
    x: T, name: String? = nil, indent: Int = 0,
    maxDepth: Int = .max, maxItems: Int = .max,
    inout targetStream: TargetStream
) -> T {
  var maxItemCounter = maxItems
  var visitedItems = Dictionary<ObjectIdentifier, Int>()
  _dumpWithMirror(reflect(x), name, indent, maxDepth,
                  &maxItemCounter, &visitedItems, &targetStream)
  return x
}

/// Dump an object's contents using its mirror to standard output.
func dump<T>(x: T, name: String? = nil, indent: Int = 0,
             maxDepth: Int = .max, maxItems: Int = .max) -> T {
  var stdoutStream = _Stdout()
  return dump(x, name: name, indent: indent, maxDepth: maxDepth,
              maxItems: maxItems, &stdoutStream)
}

/// Dump an object's contents using a mirror. User code should use dump().
func _dumpWithMirror<TargetStream : OutputStream>(
    mirror: Mirror, name: String?, indent: Int, maxDepth: Int,
    inout maxItemCounter: Int,
    inout visitedItems: Dictionary<ObjectIdentifier, Int>,
    inout targetStream: TargetStream
) {
  if maxItemCounter <= 0 { return }
  --maxItemCounter

  for _ in 0..indent { print(" ") }

  let count = mirror.count
  let bullet = count == 0    ? "-"
             : maxDepth <= 0 ? "▹" : "▿"
  print("\(bullet) ", &targetStream)

  if let nam = name {
    print("\(nam): ", &targetStream)
  }
  print(mirror.summary)

  if let id = mirror.objectIdentifier {
    if let previous = visitedItems.find(id) {
      println(" #\(previous)", &targetStream)
      return
    }
    let identifier = visitedItems.count
    visitedItems[id] = identifier
    print(" #\(identifier)", &targetStream)
  }

  println("", &targetStream)

  if maxDepth <= 0 { return }

  for i in 0..count {
    if maxItemCounter <= 0 {
      for _ in 0..(indent+4) { print(" ") }
      let remainder = count - i
      print("(\(remainder)", &targetStream)
      if i > 0 { print(" more", &targetStream) }
      if remainder == 1 {
        println(" child)", &targetStream)
      } else {
        println(" children)", &targetStream)
      }
      return
    }

    let (name, child) = mirror[i]
    _dumpWithMirror(child, name, indent + 2, maxDepth - 1,
                    &maxItemCounter, &visitedItems, &targetStream)
  }
}

// -- Mirror implementations for basic data types

func _formatNumChildren(count: Int) -> String {
  if count == 1 {
    return " (has 1 child)"
  } else {
    return " (has \(count) children)"
  }
}

/// A mirror for a value that is represented as a simple value with no
/// children.
struct _LeafMirror<T>: Mirror {
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
  subscript(i: Int) -> (String, Mirror) { fatal("no children") }
  var summary: String { return summaryFunction(_value) }
  var quickLookObject: QuickLookObject? { return quickLookFunction(_value) }
  var disposition: MirrorDisposition { return .Aggregate }
}

// -- Implementation details for the runtime's Mirror implementation

struct _MagicMirrorData {
  let owner: Builtin.NativeObject
  let ptr: Builtin.RawPointer
  let metadata: Any.Type

  var value: Any {
    @asmname("swift_MagicMirrorData_value") get
  }
  var valueType: Any.Type {
    @asmname("swift_MagicMirrorData_valueType") get
  }

  var objcValue: Any {
    @asmname("swift_MagicMirrorData_objcValue") get
  }
  var objcValueType: Any.Type {
    @asmname("swift_MagicMirrorData_objcValueType") get
  }
  
  func _loadValue<T>() -> T {
    return Builtin.load(ptr) as T
  }
}

struct _OpaqueMirror: Mirror {
  let data: _MagicMirrorData

  var value: Any { return data.value }
  var valueType: Any.Type { return data.valueType }
  var objectIdentifier: ObjectIdentifier? { return nil }
  var count: Int { return 0 }
  subscript(i: Int) -> (String, Mirror) { fatal("no children") }
  var summary: String { return "<opaque>" }
  var quickLookObject: QuickLookObject? { return nil }
  var disposition: MirrorDisposition { return .Aggregate }
}

struct _TupleMirror: Mirror {
  let data: _MagicMirrorData

  var value: Any { return data.value }
  var valueType: Any.Type { return data.valueType }
  var objectIdentifier: ObjectIdentifier? { return nil }
  var count: Int {
    @asmname("swift_TupleMirror_count") get
  }
  subscript(i: Int) -> (String, Mirror) {
    @asmname("swift_TupleMirror_subscript") get
  }
  var summary: String { return "(\(count) elements)" }
  var quickLookObject: QuickLookObject? { return nil }
  var disposition: MirrorDisposition { return .Tuple }
}

struct _StructMirror: Mirror {
  let data: _MagicMirrorData

  var value: Any { return data.value }
  var valueType: Any.Type { return data.valueType }
  var objectIdentifier: ObjectIdentifier? { return nil }
  var count: Int {
    @asmname("swift_StructMirror_count") get
  }
  subscript(i: Int) -> (String, Mirror) {
    @asmname("swift_StructMirror_subscript") get
  }

  var summary: String {
    return "\(_stdlib_getTypeName(value))\(_formatNumChildren(count))"
  }
  var quickLookObject: QuickLookObject? { return nil }
  var disposition: MirrorDisposition { return .Struct }
}

@asmname("swift_ClassMirror_count")
func _getClassCount(_MagicMirrorData) -> Int
@asmname("swift_ClassMirror_subscript")
func _getClassChild(Int, _MagicMirrorData) -> (String, Mirror)

struct _ClassMirror: Mirror {
  let data: _MagicMirrorData
  
  var value: Any { return data.value }
  var valueType: Any.Type { return data.valueType }
  var objectIdentifier: ObjectIdentifier? {
    return data._loadValue() as ObjectIdentifier
  }
  var count: Int {
    return _getClassCount(data)
  }
  subscript(i: Int) -> (String, Mirror) {
    return _getClassChild(i, data)
  }
  var summary: String {
    return "\(_stdlib_getTypeName(value))\(_formatNumChildren(count))"
  }
  var quickLookObject: QuickLookObject? { return nil }
  var disposition: MirrorDisposition { return .Class }
}

struct _ClassSuperMirror: Mirror {
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
  subscript(i: Int) -> (String, Mirror) {
    return _getClassChild(i, data)
  }
  var summary: String {
    return "\(_stdlib_getTypeName(value))\(_formatNumChildren(count))"
  }
  var quickLookObject: QuickLookObject? { return nil }
  var disposition: MirrorDisposition { return .Class }
}

