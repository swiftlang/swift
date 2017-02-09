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

@_versioned
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

@_versioned
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

@_versioned
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

@_versioned
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

@_versioned
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

@_versioned
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

@_versioned
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

