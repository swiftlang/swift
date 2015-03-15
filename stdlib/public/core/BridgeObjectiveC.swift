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

#if _runtime(_ObjC)
/// A Swift Array or Dictionary of types conforming to
/// `_ObjectiveCBridgeable` can be passed to Objective-C as an NSArray or
/// NSDictionary, respectively.  The elements of the resulting NSArray
/// or NSDictionary will be the result of calling `_bridgeToObjectiveC`
/// on each elmeent of the source container.
public protocol _ObjectiveCBridgeable {
  typealias _ObjectiveCType: AnyObject

  /// Return true iff instances of `Self` can be converted to
  /// Objective-C.  Even if this method returns `true`, A given
  /// instance of `Self._ObjectiveCType` may, or may not, convert
  /// successfully to `Self`; for example, an `NSArray` will only
  /// convert successfully to `[String]` if it contains only
  /// `NSString`\ s.
  static func _isBridgedToObjectiveC() -> Bool
  
  // _getObjectiveCType is a workaround: right now protocol witness
  // tables don't include associated types, so we can not find
  // '_ObjectiveCType.self' from them.
  
  /// Must return `_ObjectiveCType.self`.
  static func _getObjectiveCType() -> Any.Type

  /// Convert `self` to Objective-C
  func _bridgeToObjectiveC() -> _ObjectiveCType

  /// Bridge from an Objective-C object of the bridged class type to a
  /// value of the Self type.
  ///
  /// This bridging operation is used for forced downcasting (e.g.,
  /// via as), and may defer complete checking until later. For
  /// example, when bridging from NSArray to Array<T>, we can defer
  /// the checking for the individual elements of the array.
  ///
  /// :param: result The location where the result is written. The optional
  /// will always contain a value.
  static func _forceBridgeFromObjectiveC(
    source: _ObjectiveCType,
    inout result: Self?
  )

  /// Try to bridge from an Objective-C object of the bridged class
  /// type to a value of the Self type.
  ///
  /// This conditional bridging operation is used for conditional
  /// downcasting (e.g., via as?) and therefore must perform a
  /// complete conversion to the value type; it cannot defer checking
  /// to a later time.
  ///
  /// :param: result The location where the result is written.
  ///
  /// :returns: true if bridging succeeded, false otherwise. This redundant
  /// information is provided for the convenience of the runtime's dynamic_cast
  /// implementation, so that it need not look into the optional representation
  /// to determine success.
  static func _conditionallyBridgeFromObjectiveC(
    source: _ObjectiveCType,
    inout result: Self?
  ) -> Bool
}

//===--- Bridging facilities written in Objective-C -----------------------===//
// Functions that must discover and possibly use an arbitrary type's
// conformance to a given protocol.  See ../runtime/Metadata.cpp for
// implementations.
//===----------------------------------------------------------------------===//

/// Attempt to convert `x` to its Objective-C representation.
///
/// - If `T` is a class type, it is alaways bridged verbatim, the function
///   returns `x`;
///
/// - otherwise, `T` conforms to `_ObjectiveCBridgeable`:
///   + if `T._isBridgedToObjectiveC()` returns `false`, then the
///     result is empty;
///   + otherwise, returns the result of `x._bridgeToObjectiveC()`;
///  
/// - otherwise, the result is empty.
public func _bridgeToObjectiveC<T>(x: T) -> AnyObject? {
  if _fastPath(_isClassOrObjCExistential(T.self)) {
    return unsafeBitCast(x, AnyObject.self)
  }
  return _bridgeNonVerbatimToObjectiveC(x)
}

public func _bridgeToObjectiveCUnconditional<T>(x: T) -> AnyObject {
  let optResult: AnyObject? = _bridgeToObjectiveC(x)
  _precondition(optResult != nil,
      "value failed to bridge from Swift type to a Objective-C type")
  return optResult!
}

/// Same as `_bridgeToObjectiveCUnconditional`, but autoreleases the
/// return value if `T` is bridged non-verbatim.
func _bridgeToObjectiveCUnconditionalAutorelease<T>(x: T) -> AnyObject
{
  if _fastPath(_isClassOrObjCExistential(T.self)) {
    return unsafeBitCast(x, AnyObject.self)
  }
  if let bridged? = _bridgeNonVerbatimToObjectiveC(x) {
    _autorelease(bridged)
    return bridged
  } else {
    _preconditionFailure(
      "Dictionary key failed to bridge from Swift type to a Objective-C type")
  }
}

@asmname("swift_bridgeNonVerbatimToObjectiveC")
func _bridgeNonVerbatimToObjectiveC<T>(x: T) -> AnyObject?

/// Convert `x` from its Objective-C representation to its Swift
/// representation.
///
/// - If `T` is a class type:
///   - if the dynamic type of `x` is `T` or a subclass of it, it is bridged
///     verbatim, the function returns `x`;
/// - otherwise, if `T` conforms to `_ObjectiveCBridgeable`:
///   + if the dynamic type of `x` is not `T._getObjectiveCType()`
///     or a subclass of it, trap
///   + otherwise, returns the result of `T._forceBridgeFromObjectiveC(x)`;
/// - otherwise, trap
public func _forceBridgeFromObjectiveC<T>(x: AnyObject, _: T.Type) -> T {
  if _fastPath(_isClassOrObjCExistential(T.self)) {
    return x as! T
  }

  var result: T?
  _bridgeNonVerbatimFromObjectiveC(x, T.self, &result)
  return result!
}

/// Attempt to convert `x` from its Objective-C representation to its Swift
/// representation.
///
/// - If `T` is a class type:
///   - if the dynamic type of `x` is `T` or a subclass of it, it is bridged
///     verbatim, the function returns `x`;
/// - otherwise, if `T` conforms to `_ObjectiveCBridgeable`:
///   + if `T._isBridgedToObjectiveC()` returns `false`, then the result is
///     empty;
///   + otherwise, if the dynamic type of `x` is not `T._getObjectiveCType()`
///     or a subclass of it, the result is empty;
///   + otherwise, returns the result of
///     `T._conditionallyBridgeFromObjectiveC(x)`;
/// - otherwise, the result is empty.
public func _conditionallyBridgeFromObjectiveC<T>(
  x: AnyObject, 
  _: T.Type
) -> T? {
  if _fastPath(_isClassOrObjCExistential(T.self)) {
    return x as? T
  }

  var result: T?
  _bridgeNonVerbatimFromObjectiveCConditional(x, T.self, &result)
  return result
}

@asmname("swift_bridgeNonVerbatimFromObjectiveC")
func _bridgeNonVerbatimFromObjectiveC<T>(
  x: AnyObject, 
  nativeType: T.Type,
  inout result: T?
)

/// Runtime optional to conditionall perform a bridge from an object to a value
/// type.
///
/// :param: result Will be set to the resulting value if bridging succeeds, and
/// unchanged otherwise.
///
/// :returns: true to indicate success, false to indicate failure
@asmname("swift_bridgeNonVerbatimFromObjectiveCConditional")
func _bridgeNonVerbatimFromObjectiveCConditional<T>(
  x: AnyObject, 
  nativeType: T.Type,
  inout result: T?
) -> Bool

/// Determines if values of a given type can be converted to an Objective-C
/// representation.
///
/// - If `T` is a class type, returns `true`;
/// - otherwise, if `T` conforms to `_ObjectiveCBridgeable`, returns
///   `T._isBridgedToObjectiveC()`;
public func _isBridgedToObjectiveC<T>(_: T.Type) -> Bool {
  if _fastPath(_isClassOrObjCExistential(T.self)) {
    return true
  }
  return _isBridgedNonVerbatimToObjectiveC(T.self)
}

@asmname("swift_isBridgedNonVerbatimToObjectiveC")
func _isBridgedNonVerbatimToObjectiveC<T>(_: T.Type) -> Bool

/// A type that's bridged "verbatim" does not conform to
/// `_ObjectiveCBridgeable`, and can have its bits reinterpreted as an
/// `AnyObject`.  When this function returns true, the storage of an
/// `Array<T>` can be `unsafeBitCast` as an array of `AnyObject`.
public func _isBridgedVerbatimToObjectiveC<T>(_: T.Type) -> Bool {
  return _isClassOrObjCExistential(T.self)
}

/// Retrieve the Objective-C type to which the given type is bridged.
public func _getBridgedObjectiveCType<T>(_: T.Type) -> Any.Type?  {
  if _fastPath(_isClassOrObjCExistential(T.self)) {
    return T.self
  }
  return _getBridgedNonVerbatimObjectiveCType(T.self)
}

@asmname("swift_getBridgedNonVerbatimObjectiveCType")
func _getBridgedNonVerbatimObjectiveCType<T>(_: T.Type) -> Any.Type?

// -- Pointer argument bridging

@transparent
internal var _nilNativeObject: AnyObject? {
  return nil
}

/// A mutable pointer-to-ObjC-pointer argument.
///
/// This type has implicit conversions to allow passing any of the following
/// to a C or ObjC API:
///
/// - 'nil', which gets passed as a null pointer,
/// - an inout argument of the referenced type, which gets passed as a pointer
///   to a writeback temporary with autoreleasing ownership semantics,
/// - an UnsafeMutablePointer<T>, which is passed as-is.
///
/// Passing pointers to mutable arrays of ObjC class pointers is not
/// directly supported. Unlike UnsafeMutablePointer<T>,
/// AutoreleasingUnsafeMutablePointer must reference storage that does
/// not own a reference count to the referenced
/// value. UnsafeMutablePointer's operations, by contrast, assume that
/// the referenced storage owns values loaded from or stored to it.
///
/// This type does not carry an owner pointer unlike the other C*Pointer types
/// because it only needs to reference the results of inout conversions, which
/// already have writeback-scoped lifetime.
public struct AutoreleasingUnsafeMutablePointer<T /* TODO : class */>
  : Equatable, NilLiteralConvertible, _PointerType {
  public let _rawValue: Builtin.RawPointer

  @transparent
  public // COMPILER_INTRINSIC
  init(_ _rawValue: Builtin.RawPointer) {
    self._rawValue = _rawValue
  }

  @transparent
  var _isNull : Bool {
    return UnsafeMutablePointer<T>(self)._isNull
  }
  
  /// Access the underlying raw memory, getting and
  /// setting values.
  public var memory : T {
    /// Retrieve the value the pointer points to.
    @transparent get {
      _debugPrecondition(!_isNull)
      // We can do a strong load normally.
      return UnsafeMutablePointer<T>(self).memory
    }
    /// Set the value the pointer points to, copying over the previous value.
    ///
    /// AutoreleasingUnsafeMutablePointers are assumed to reference a
    /// value with __autoreleasing ownership semantics, like 'NSFoo**'
    /// in ARC. This autoreleases the argument before trivially
    /// storing it to the referenced memory.
    @transparent nonmutating set {
      _debugPrecondition(!_isNull)
      // Autorelease the object reference.
      typealias OptionalAnyObject = AnyObject?
      Builtin.retain(unsafeBitCast(newValue, OptionalAnyObject.self))
      Builtin.autorelease(unsafeBitCast(newValue, OptionalAnyObject.self))
      // Trivially assign it as a COpaquePointer; the pointer references an
      // autoreleasing slot, so retains/releases of the original value are
      // unneeded.
      let p = UnsafeMutablePointer<COpaquePointer>(
        UnsafeMutablePointer<T>(self))
        p.memory = unsafeBitCast(newValue, COpaquePointer.self)
    }
  }

  /// Access the `i`\ th element of the raw array pointed to by
  /// `self`.
  ///
  /// Requires: `self != nil`
  public subscript(i: Int) -> T {
    @transparent
    get {
      _debugPrecondition(!_isNull)
      // We can do a strong load normally.
      return (UnsafePointer<T>(self) + i).memory
    }
  }
  
  /// Create an instance initialized with `nil`.
  @transparent public
  init(nilLiteral: ()) {
    _rawValue = _nilRawPointer
  }

  /// Returns `nil`
  @availability(*, unavailable, message="use 'nil' literal instead")
  public static func null() -> AutoreleasingUnsafeMutablePointer {
    _preconditionFailure("unavailable function can not be called")
  }

  /// Initialize to a null pointer.
  @transparent public
  init() {
    self._rawValue = _nilRawPointer
  }
  
  /// Explicit construction from an UnsafeMutablePointer.
  ///
  /// This is inherently unsafe; UnsafeMutablePointer assumes the
  /// referenced memory has +1 strong ownership semantics, whereas
  /// AutoreleasingUnsafeMutablePointer implies +0 semantics.
  @transparent public
  init<U>(_ ptr: UnsafeMutablePointer<U>) {
    self._rawValue = ptr._rawValue
  }

  /// Explicit construction from a UnsafePointer.
  ///
  /// This is inherently unsafe because UnsafePointers do not imply
  /// mutability.
  @transparent
  init<U>(_ ptr: UnsafePointer<U>) {
    self._rawValue = ptr._rawValue
  }
}

extension AutoreleasingUnsafeMutablePointer : DebugPrintable {
  /// A textual representation of `self`, suitable for debugging.
  public var debugDescription: String {
    return _rawPointerToString(_rawValue)
  }
}

@transparent
public func == <T> (
  lhs: AutoreleasingUnsafeMutablePointer<T>, 
  rhs: AutoreleasingUnsafeMutablePointer<T>
) -> Bool {
  return Bool(Builtin.cmp_eq_RawPointer(lhs._rawValue, rhs._rawValue))
}

internal struct _CocoaFastEnumerationStackBuf {
  // Clang uses 16 pointers.  So do we.
  var item0: Builtin.RawPointer
  var item1: Builtin.RawPointer
  var item2: Builtin.RawPointer
  var item3: Builtin.RawPointer
  var item4: Builtin.RawPointer
  var item5: Builtin.RawPointer
  var item6: Builtin.RawPointer
  var item7: Builtin.RawPointer
  var item8: Builtin.RawPointer
  var item9: Builtin.RawPointer
  var item10: Builtin.RawPointer
  var item11: Builtin.RawPointer
  var item12: Builtin.RawPointer
  var item13: Builtin.RawPointer
  var item14: Builtin.RawPointer
  var item15: Builtin.RawPointer

  @transparent
  var length: Int {
    return 16
  }

  init() {
    item0 = _nilRawPointer
    item1 = item0
    item2 = item0
    item3 = item0
    item4 = item0
    item5 = item0
    item6 = item0
    item7 = item0
    item8 = item0
    item9 = item0
    item10 = item0
    item11 = item0
    item12 = item0
    item13 = item0
    item14 = item0
    item15 = item0

    _sanityCheck(sizeofValue(self) >= sizeof(Builtin.RawPointer.self) * length)
  }
}

#endif
