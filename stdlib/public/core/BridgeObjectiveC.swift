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

#if _runtime(_ObjC)
/// A Swift Array or Dictionary of types conforming to
/// `_ObjectiveCBridgeable` can be passed to Objective-C as an NSArray or
/// NSDictionary, respectively.  The elements of the resulting NSArray
/// or NSDictionary will be the result of calling `_bridgeToObjectiveC`
/// on each element of the source container.
public protocol _ObjectiveCBridgeable {
  associatedtype _ObjectiveCType : AnyObject

  /// Returns `true` iff instances of `Self` can be converted to
  /// Objective-C.  Even if this method returns `true`, a given
  /// instance of `Self._ObjectiveCType` may, or may not, convert
  /// successfully to `Self`; for example, an `NSArray` will only
  /// convert successfully to `[String]` if it contains only
  /// `NSString`s.
  @warn_unused_result
  static func _isBridgedToObjectiveC() -> Bool

  /// Convert `self` to Objective-C.
  @warn_unused_result
  func _bridgeToObjectiveC() -> _ObjectiveCType

  /// Bridge from an Objective-C object of the bridged class type to a
  /// value of the Self type.
  ///
  /// This bridging operation is used for forced downcasting (e.g.,
  /// via as), and may defer complete checking until later. For
  /// example, when bridging from `NSArray` to `Array<Element>`, we can defer
  /// the checking for the individual elements of the array.
  ///
  /// - parameter result: The location where the result is written. The optional
  ///   will always contain a value.
  static func _forceBridgeFromObjectiveC(
    _ source: _ObjectiveCType,
    result: inout Self?
  )

  /// Try to bridge from an Objective-C object of the bridged class
  /// type to a value of the Self type.
  ///
  /// This conditional bridging operation is used for conditional
  /// downcasting (e.g., via as?) and therefore must perform a
  /// complete conversion to the value type; it cannot defer checking
  /// to a later time.
  ///
  /// - parameter result: The location where the result is written.
  ///
  /// - Returns: `true` if bridging succeeded, `false` otherwise. This redundant
  ///   information is provided for the convenience of the runtime's `dynamic_cast`
  ///   implementation, so that it need not look into the optional representation
  ///   to determine success.
  @discardableResult
  static func _conditionallyBridgeFromObjectiveC(
    _ source: _ObjectiveCType,
    result: inout Self?
  ) -> Bool

  /// Bridge from an Objective-C object of the bridged class type to a
  /// value of the Self type.
  ///
  /// This bridging operation is used for unconditional bridging when
  /// interoperating with Objective-C code, either in the body of an
  /// Objective-C thunk or when calling Objective-C code, and may
  /// defer complete checking until later. For example, when bridging
  /// from `NSArray` to `Array<Element>`, we can defer the checking
  /// for the individual elements of the array.
  ///
  /// \param source The Objective-C object from which we are
  /// bridging. This optional value will only be `nil` in cases where
  /// an Objective-C method has returned a `nil` despite being marked
  /// as `_Nonnull`/`nonnull`. In most such cases, bridging will
  /// generally force the value immediately. However, this gives
  /// bridging the flexibility to substitute a default value to cope
  /// with historical decisions, e.g., an existing Objective-C method
  /// that returns `nil` to for "empty result" rather than (say) an
  /// empty array. In such cases, when `nil` does occur, the
  /// implementation of `Swift.Array`'s conformance to
  /// `_ObjectiveCBridgeable` will produce an empty array rather than
  /// dynamically failing.
  static func _unconditionallyBridgeFromObjectiveC(_ source: _ObjectiveCType?)
      -> Self
}

//===--- Bridging for metatypes -------------------------------------------===//

/// A stand-in for a value of metatype type.
///
/// The language and runtime do not yet support protocol conformances for
/// structural types like metatypes. However, we can use a struct that contains
/// a metatype, make it conform to _ObjectiveCBridgeable, and its witness table
/// will be ABI-compatible with one that directly provided conformance to the
/// metatype type itself.
@_fixed_layout
public struct _BridgeableMetatype: _ObjectiveCBridgeable {
  internal var value: AnyObject.Type

  public typealias _ObjectiveCType = AnyObject

  public static func _isBridgedToObjectiveC() -> Bool {
    return true
  }

  public func _bridgeToObjectiveC() -> AnyObject {
    return value
  }

  public static func _forceBridgeFromObjectiveC(
    _ source: AnyObject,
    result: inout _BridgeableMetatype?
  ) {
    result = _BridgeableMetatype(value: source as! AnyObject.Type)
  }

  public static func _conditionallyBridgeFromObjectiveC(
    _ source: AnyObject,
    result: inout _BridgeableMetatype?
  ) -> Bool {
    if let type = source as? AnyObject.Type {
      result = _BridgeableMetatype(value: type)
      return true
    }

    result = nil
    return false
  }

  public static func _unconditionallyBridgeFromObjectiveC(_ source: AnyObject?)
      -> _BridgeableMetatype {
    var result: _BridgeableMetatype?
    _forceBridgeFromObjectiveC(source!, result: &result)
    return result!
  }
}


//===--- Bridging facilities written in Objective-C -----------------------===//
// Functions that must discover and possibly use an arbitrary type's
// conformance to a given protocol.  See ../runtime/Metadata.cpp for
// implementations.
//===----------------------------------------------------------------------===//

/// Attempt to convert `x` to its Objective-C representation.
///
/// - If `T` is a class type, it is always bridged verbatim, the function
///   returns `x`;
///
/// - otherwise, `T` conforms to `_ObjectiveCBridgeable`:
///   + if `T._isBridgedToObjectiveC()` returns `false`, then the
///     result is empty;
///   + otherwise, returns the result of `x._bridgeToObjectiveC()`;
///
/// - otherwise, the result is empty.
@warn_unused_result
public func _bridgeToObjectiveC<T>(_ x: T) -> AnyObject? {
  if _fastPath(_isClassOrObjCExistential(T.self)) {
    return unsafeBitCast(x, to: AnyObject.self)
  }
  return _bridgeNonVerbatimToObjectiveC(x)
}

@warn_unused_result
public func _bridgeToObjectiveCUnconditional<T>(_ x: T) -> AnyObject {
  let optResult: AnyObject? = _bridgeToObjectiveC(x)
  _precondition(optResult != nil,
      "value failed to bridge from Swift type to a Objective-C type")
  return optResult!
}

/// Same as `_bridgeToObjectiveCUnconditional`, but autoreleases the
/// return value if `T` is bridged non-verbatim.
@warn_unused_result
func _bridgeToObjectiveCUnconditionalAutorelease<T>(_ x: T) -> AnyObject
{
  if _fastPath(_isClassOrObjCExistential(T.self)) {
    return unsafeBitCast(x, to: AnyObject.self)
  }
  guard let bridged = _bridgeNonVerbatimToObjectiveC(x) else {
    _preconditionFailure(
      "Dictionary key failed to bridge from Swift type to a Objective-C type")
  }

  _autorelease(bridged)
  return bridged
}

@warn_unused_result
@_silgen_name("_swift_bridgeNonVerbatimToObjectiveC")
func _bridgeNonVerbatimToObjectiveC<T>(_ x: T) -> AnyObject?

/// Convert `x` from its Objective-C representation to its Swift
/// representation.
///
/// - If `T` is a class type:
///   - if the dynamic type of `x` is `T` or a subclass of it, it is bridged
///     verbatim, the function returns `x`;
/// - otherwise, if `T` conforms to `_ObjectiveCBridgeable`:
///   + if the dynamic type of `x` is not `T._ObjectiveCType`
///     or a subclass of it, trap;
///   + otherwise, returns the result of `T._forceBridgeFromObjectiveC(x)`;
/// - otherwise, trap.
@warn_unused_result
public func _forceBridgeFromObjectiveC<T>(_ x: AnyObject, _: T.Type) -> T {
  if _fastPath(_isClassOrObjCExistential(T.self)) {
    return x as! T
  }

  var result: T?
  _bridgeNonVerbatimFromObjectiveC(x, T.self, &result)
  return result!
}

/// Convert `x` from its Objective-C representation to its Swift
/// representation.
@warn_unused_result
@_silgen_name("_forceBridgeFromObjectiveC_bridgeable")
public func _forceBridgeFromObjectiveC_bridgeable<T:_ObjectiveCBridgeable>
  (_ x: T._ObjectiveCType, _: T.Type) -> T {
  var result: T?
  T._forceBridgeFromObjectiveC(x, result: &result)
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
///   + otherwise, if the dynamic type of `x` is not `T._ObjectiveCType`
///     or a subclass of it, the result is empty;
///   + otherwise, returns the result of
///     `T._conditionallyBridgeFromObjectiveC(x)`;
/// - otherwise, the result is empty.
@warn_unused_result
public func _conditionallyBridgeFromObjectiveC<T>(
  _ x: AnyObject,
  _: T.Type
) -> T? {
  if _fastPath(_isClassOrObjCExistential(T.self)) {
    return x as? T
  }

  var result: T?
  _ = _bridgeNonVerbatimFromObjectiveCConditional(x, T.self, &result)
  return result
}

/// Attempt to convert `x` from its Objective-C representation to its Swift
/// representation.
@warn_unused_result
@_silgen_name("_conditionallyBridgeFromObjectiveC_bridgeable")
public func _conditionallyBridgeFromObjectiveC_bridgeable<T:_ObjectiveCBridgeable>(
  _ x: T._ObjectiveCType,
  _: T.Type
) -> T? {
  var result: T?
  T._conditionallyBridgeFromObjectiveC (x, result: &result)
  return result
}

@_silgen_name("_swift_bridgeNonVerbatimFromObjectiveC")
func _bridgeNonVerbatimFromObjectiveC<T>(
  _ x: AnyObject,
  _ nativeType: T.Type,
  _ result: inout T?
)

/// Runtime optional to conditionally perform a bridge from an object to a value
/// type.
///
/// - parameter result: Will be set to the resulting value if bridging succeeds, and
///   unchanged otherwise.
///
/// - Returns: `true` to indicate success, `false` to indicate failure.
@_silgen_name("_swift_bridgeNonVerbatimFromObjectiveCConditional")
func _bridgeNonVerbatimFromObjectiveCConditional<T>(
  _ x: AnyObject,
  _ nativeType: T.Type,
  _ result: inout T?
) -> Bool

/// Determines if values of a given type can be converted to an Objective-C
/// representation.
///
/// - If `T` is a class type, returns `true`;
/// - otherwise, if `T` conforms to `_ObjectiveCBridgeable`, returns
///   `T._isBridgedToObjectiveC()`.
@warn_unused_result
public func _isBridgedToObjectiveC<T>(_: T.Type) -> Bool {
  if _fastPath(_isClassOrObjCExistential(T.self)) {
    return true
  }
  return _isBridgedNonVerbatimToObjectiveC(T.self)
}

@warn_unused_result
@_silgen_name("_swift_isBridgedNonVerbatimToObjectiveC")
func _isBridgedNonVerbatimToObjectiveC<T>(_: T.Type) -> Bool

/// A type that's bridged "verbatim" does not conform to
/// `_ObjectiveCBridgeable`, and can have its bits reinterpreted as an
/// `AnyObject`.  When this function returns true, the storage of an
/// `Array<T>` can be `unsafeBitCast` as an array of `AnyObject`.
@warn_unused_result
public func _isBridgedVerbatimToObjectiveC<T>(_: T.Type) -> Bool {
  return _isClassOrObjCExistential(T.self)
}

/// Retrieve the Objective-C type to which the given type is bridged.
@warn_unused_result
public func _getBridgedObjectiveCType<T>(_: T.Type) -> Any.Type? {
  if _fastPath(_isClassOrObjCExistential(T.self)) {
    return T.self
  }
  return _getBridgedNonVerbatimObjectiveCType(T.self)
}

@warn_unused_result
@_silgen_name("_swift_getBridgedNonVerbatimObjectiveCType")
func _getBridgedNonVerbatimObjectiveCType<T>(_: T.Type) -> Any.Type?

// -- Pointer argument bridging

@_transparent
internal var _nilNativeObject: AnyObject? {
  return nil
}

/// A mutable pointer-to-ObjC-pointer argument.
///
/// This type has implicit conversions to allow passing any of the following
/// to a C or ObjC API:
///
/// - `nil`, which gets passed as a null pointer,
/// - an inout argument of the referenced type, which gets passed as a pointer
///   to a writeback temporary with autoreleasing ownership semantics,
/// - an `UnsafeMutablePointer<Pointee>`, which is passed as-is.
///
/// Passing pointers to mutable arrays of ObjC class pointers is not
/// directly supported. Unlike `UnsafeMutablePointer<Pointee>`,
/// `AutoreleasingUnsafeMutablePointer<Pointee>` must reference storage that
/// does not own a reference count to the referenced
/// value. UnsafeMutablePointer's operations, by contrast, assume that
/// the referenced storage owns values loaded from or stored to it.
///
/// This type does not carry an owner pointer unlike the other C*Pointer types
/// because it only needs to reference the results of inout conversions, which
/// already have writeback-scoped lifetime.
@_fixed_layout
public struct AutoreleasingUnsafeMutablePointer<Pointee /* TODO : class */>
  : Equatable, _Pointer {

  public let _rawValue: Builtin.RawPointer

  @_transparent
  public // COMPILER_INTRINSIC
  init(_ _rawValue: Builtin.RawPointer) {
    self._rawValue = _rawValue
  }

  /// Access the `Pointee` instance referenced by `self`.
  ///
  /// - Precondition: the pointee has been initialized with an instance of type
  ///   `Pointee`.
  public var pointee: Pointee {
    /// Retrieve the value the pointer points to.
    @_transparent get {
      // We can do a strong load normally.
      return UnsafeMutablePointer<Pointee>(self).pointee
    }
    /// Set the value the pointer points to, copying over the previous value.
    ///
    /// AutoreleasingUnsafeMutablePointers are assumed to reference a
    /// value with __autoreleasing ownership semantics, like 'NSFoo**'
    /// in ARC. This autoreleases the argument before trivially
    /// storing it to the referenced memory.
    @_transparent nonmutating set {
      // Autorelease the object reference.
      typealias OptionalAnyObject = AnyObject?
      Builtin.retain(unsafeBitCast(newValue, to: OptionalAnyObject.self))
      Builtin.autorelease(unsafeBitCast(newValue, to: OptionalAnyObject.self))
      // Trivially assign it as an OpaquePointer; the pointer references an
      // autoreleasing slot, so retains/releases of the original value are
      // unneeded.
      typealias OptionalOpaquePointer = OpaquePointer?
      let p = UnsafeMutablePointer<OptionalOpaquePointer>(
        UnsafeMutablePointer<Pointee>(self))
      p.pointee = unsafeBitCast(newValue, to: OptionalOpaquePointer.self)
    }
  }

  /// Access the `i`th element of the raw array pointed to by
  /// `self`.
  ///
  /// - Precondition: `self != nil`.
  public subscript(i: Int) -> Pointee {
    @_transparent
    get {
      // We can do a strong load normally.
      return (UnsafePointer<Pointee>(self) + i).pointee
    }
  }

  /// Explicit construction from an UnsafeMutablePointer.
  ///
  /// This is inherently unsafe; UnsafeMutablePointer assumes the
  /// referenced memory has +1 strong ownership semantics, whereas
  /// AutoreleasingUnsafeMutablePointer implies +0 semantics.
  @_transparent public
  init<U>(_ from: UnsafeMutablePointer<U>) {
    self._rawValue = from._rawValue
  }

  /// Explicit construction from an UnsafeMutablePointer.
  ///
  /// Returns nil if `from` is nil.
  ///
  /// This is inherently unsafe; UnsafeMutablePointer assumes the
  /// referenced memory has +1 strong ownership semantics, whereas
  /// AutoreleasingUnsafeMutablePointer implies +0 semantics.
  @_transparent public
  init?<U>(_ from: UnsafeMutablePointer<U>?) {
    guard let unwrapped = from else { return nil }
    self.init(unwrapped)
  }

  /// Explicit construction from a UnsafePointer.
  ///
  /// This is inherently unsafe because UnsafePointers do not imply
  /// mutability.
  @_transparent
  init<U>(_ from: UnsafePointer<U>) {
    self._rawValue = from._rawValue
  }

  /// Explicit construction from a UnsafePointer.
  ///
  /// Returns nil if `from` is nil.
  ///
  /// This is inherently unsafe because UnsafePointers do not imply
  /// mutability.
  @_transparent
  init?<U>(_ from: UnsafePointer<U>?) {
    guard let unwrapped = from else { return nil }
    self.init(unwrapped)
  }
}

extension AutoreleasingUnsafeMutablePointer : CustomDebugStringConvertible {
  /// A textual representation of `self`, suitable for debugging.
  public var debugDescription: String {
    return _rawPointerToString(_rawValue)
  }
}

@_transparent
@warn_unused_result
public func == <Pointee> (
  lhs: AutoreleasingUnsafeMutablePointer<Pointee>,
  rhs: AutoreleasingUnsafeMutablePointer<Pointee>
) -> Bool {
  return Bool(Builtin.cmp_eq_RawPointer(lhs._rawValue, rhs._rawValue))
}

@_fixed_layout
internal struct _CocoaFastEnumerationStackBuf {
  // Clang uses 16 pointers.  So do we.
  internal var _item0: UnsafePointer<Void>?
  internal var _item1: UnsafePointer<Void>?
  internal var _item2: UnsafePointer<Void>?
  internal var _item3: UnsafePointer<Void>?
  internal var _item4: UnsafePointer<Void>?
  internal var _item5: UnsafePointer<Void>?
  internal var _item6: UnsafePointer<Void>?
  internal var _item7: UnsafePointer<Void>?
  internal var _item8: UnsafePointer<Void>?
  internal var _item9: UnsafePointer<Void>?
  internal var _item10: UnsafePointer<Void>?
  internal var _item11: UnsafePointer<Void>?
  internal var _item12: UnsafePointer<Void>?
  internal var _item13: UnsafePointer<Void>?
  internal var _item14: UnsafePointer<Void>?
  internal var _item15: UnsafePointer<Void>?

  @_transparent
  internal var count: Int {
    return 16
  }

  internal init() {
    _item0 = nil
    _item1 = _item0
    _item2 = _item0
    _item3 = _item0
    _item4 = _item0
    _item5 = _item0
    _item6 = _item0
    _item7 = _item0
    _item8 = _item0
    _item9 = _item0
    _item10 = _item0
    _item11 = _item0
    _item12 = _item0
    _item13 = _item0
    _item14 = _item0
    _item15 = _item0

    _sanityCheck(sizeofValue(self) >=
                   sizeof(Optional<UnsafePointer<Void>>.self) * count)
  }
}

extension AutoreleasingUnsafeMutablePointer {
  @available(*, unavailable, renamed: "Pointee")
  public typealias Memory = Pointee

  @available(*, unavailable, renamed: "pointee")
  public var memory: Pointee {
    Builtin.unreachable()
  }

  @available(*, unavailable, message: "Removed in Swift 3. Please use nil literal instead.")
  public init() {
    Builtin.unreachable()
  }
}
#endif
