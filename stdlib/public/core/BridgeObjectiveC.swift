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

#if _runtime(_ObjC)
/// A Swift Array or Dictionary of types conforming to
/// `_ObjectiveCBridgeable` can be passed to Objective-C as an NSArray or
/// NSDictionary, respectively.  The elements of the resulting NSArray
/// or NSDictionary will be the result of calling `_bridgeToObjectiveC`
/// on each element of the source container.
public protocol _ObjectiveCBridgeable {
  associatedtype _ObjectiveCType : AnyObject

  /// Convert `self` to Objective-C.
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
  @usableFromInline // FIXME(sil-serialize-all)
  internal var value: AnyObject.Type

  @inlinable // FIXME(sil-serialize-all)
  internal init(value: AnyObject.Type) {
    self.value = value
  }

  public typealias _ObjectiveCType = AnyObject

  @inlinable // FIXME(sil-serialize-all)
  public func _bridgeToObjectiveC() -> AnyObject {
    return value
  }

  @inlinable // FIXME(sil-serialize-all)
  public static func _forceBridgeFromObjectiveC(
    _ source: AnyObject,
    result: inout _BridgeableMetatype?
  ) {
    result = _BridgeableMetatype(value: source as! AnyObject.Type)
  }

  @inlinable // FIXME(sil-serialize-all)
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

  @inlinable // FIXME(sil-serialize-all)
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

/// Bridge an arbitrary value to an Objective-C object.
///
/// - If `T` is a class type, it is always bridged verbatim, the function
///   returns `x`;
///
/// - otherwise, if `T` conforms to `_ObjectiveCBridgeable`,
///   returns the result of `x._bridgeToObjectiveC()`;
///
/// - otherwise, we use **boxing** to bring the value into Objective-C.
///   The value is wrapped in an instance of a private Objective-C class
///   that is `id`-compatible and dynamically castable back to the type of
///   the boxed value, but is otherwise opaque.
///
/// COMPILER_INTRINSIC
@inlinable // FIXME(sil-serialize-all)
public func _bridgeAnythingToObjectiveC<T>(_ x: T) -> AnyObject {
  if _fastPath(_isClassOrObjCExistential(T.self)) {
    return unsafeBitCast(x, to: AnyObject.self)
  }
  return _bridgeAnythingNonVerbatimToObjectiveC(x)
}

@_silgen_name("")
public func _bridgeAnythingNonVerbatimToObjectiveC<T>(_ x: T) -> AnyObject

/// Convert a purportedly-nonnull `id` value from Objective-C into an Any.
///
/// Since Objective-C APIs sometimes get their nullability annotations wrong,
/// this includes a failsafe against nil `AnyObject`s, wrapping them up as
/// a nil `AnyObject?`-inside-an-`Any`.
///
/// COMPILER_INTRINSIC
@inlinable // FIXME(sil-serialize-all)
public func _bridgeAnyObjectToAny(_ possiblyNullObject: AnyObject?) -> Any {
  if let nonnullObject = possiblyNullObject {
    return nonnullObject // AnyObject-in-Any
  }
  return possiblyNullObject as Any
}

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
@inlinable // FIXME(sil-serialize-all)
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
/// COMPILER_INTRINSIC
@inlinable // FIXME(sil-serialize-all)
public func _forceBridgeFromObjectiveC_bridgeable<T:_ObjectiveCBridgeable> (
  _ x: T._ObjectiveCType,
  _: T.Type
) -> T {
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
///   + otherwise, if the dynamic type of `x` is not `T._ObjectiveCType`
///     or a subclass of it, the result is empty;
///   + otherwise, returns the result of
///     `T._conditionallyBridgeFromObjectiveC(x)`;
/// - otherwise, the result is empty.
@inlinable // FIXME(sil-serialize-all)
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
/// COMPILER_INTRINSIC
@inlinable // FIXME(sil-serialize-all)
public func _conditionallyBridgeFromObjectiveC_bridgeable<T:_ObjectiveCBridgeable>(
  _ x: T._ObjectiveCType,
  _: T.Type
) -> T? {
  var result: T?
  T._conditionallyBridgeFromObjectiveC (x, result: &result)
  return result
}

@_silgen_name("")
public func _bridgeNonVerbatimFromObjectiveC<T>(
  _ x: AnyObject,
  _ nativeType: T.Type,
  _ result: inout T?
)

/// Helper stub to upcast to Any and store the result to an inout Any?
/// on the C++ runtime's behalf.
@_silgen_name("_bridgeNonVerbatimFromObjectiveCToAny")
internal func _bridgeNonVerbatimFromObjectiveCToAny(
    _ x: AnyObject,
    _ result: inout Any?
) {
  result = x as Any
}

/// Helper stub to upcast to Optional on the C++ runtime's behalf.
@_silgen_name("_bridgeNonVerbatimBoxedValue")
internal func _bridgeNonVerbatimBoxedValue<NativeType>(
    _ x: UnsafePointer<NativeType>,
    _ result: inout NativeType?
) {
  result = x.pointee
}

/// Runtime optional to conditionally perform a bridge from an object to a value
/// type.
///
/// - parameter result: Will be set to the resulting value if bridging succeeds, and
///   unchanged otherwise.
///
/// - Returns: `true` to indicate success, `false` to indicate failure.
@_silgen_name("")
public func _bridgeNonVerbatimFromObjectiveCConditional<T>(
  _ x: AnyObject,
  _ nativeType: T.Type,
  _ result: inout T?
) -> Bool

/// Determines if values of a given type can be converted to an Objective-C
/// representation.
///
/// - If `T` is a class type, returns `true`;
/// - otherwise, returns whether `T` conforms to `_ObjectiveCBridgeable`.
@inlinable // FIXME(sil-serialize-all)
public func _isBridgedToObjectiveC<T>(_: T.Type) -> Bool {
  if _fastPath(_isClassOrObjCExistential(T.self)) {
    return true
  }
  return _isBridgedNonVerbatimToObjectiveC(T.self)
}

@_silgen_name("")
public func _isBridgedNonVerbatimToObjectiveC<T>(_: T.Type) -> Bool

/// A type that's bridged "verbatim" does not conform to
/// `_ObjectiveCBridgeable`, and can have its bits reinterpreted as an
/// `AnyObject`.  When this function returns true, the storage of an
/// `Array<T>` can be `unsafeBitCast` as an array of `AnyObject`.
@inlinable // FIXME(sil-serialize-all)
public func _isBridgedVerbatimToObjectiveC<T>(_: T.Type) -> Bool {
  return _isClassOrObjCExistential(T.self)
}

/// Retrieve the Objective-C type to which the given type is bridged.
@inlinable // FIXME(sil-serialize-all)
public func _getBridgedObjectiveCType<T>(_: T.Type) -> Any.Type? {
  if _fastPath(_isClassOrObjCExistential(T.self)) {
    return T.self
  }
  return _getBridgedNonVerbatimObjectiveCType(T.self)
}

@_silgen_name("")
public func _getBridgedNonVerbatimObjectiveCType<T>(_: T.Type) -> Any.Type?

// -- Pointer argument bridging

@inlinable // FIXME(sil-serialize-all)
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
  :  _Pointer {

  public let _rawValue: Builtin.RawPointer

  @inlinable // FIXME(sil-serialize-all)
  @_transparent
  public // COMPILER_INTRINSIC
  init(_ _rawValue: Builtin.RawPointer) {
    self._rawValue = _rawValue
  }

  /// Access the `Pointee` instance referenced by `self`.
  ///
  /// - Precondition: the pointee has been initialized with an instance of type
  ///   `Pointee`.
  @inlinable
  public var pointee: Pointee {
    /// Retrieve the value the pointer points to.
    @_transparent get {
      // We can do a strong load normally.
      return UnsafePointer(self).pointee
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
      let newAnyObject = unsafeBitCast(newValue, to: OptionalAnyObject.self)
      Builtin.retain(newAnyObject)
      Builtin.autorelease(newAnyObject)
      // Trivially assign it as an OpaquePointer; the pointer references an
      // autoreleasing slot, so retains/releases of the original value are
      // unneeded.
      typealias OptionalUnmanaged = Unmanaged<AnyObject>?
      UnsafeMutablePointer<Pointee>(_rawValue).withMemoryRebound(
        to: OptionalUnmanaged.self, capacity: 1) {
        if let newAnyObject = newAnyObject {
          $0.pointee = Unmanaged.passUnretained(newAnyObject)
        }
        else {
          $0.pointee = nil
        }
      }
    }
  }

  /// Access the `i`th element of the raw array pointed to by
  /// `self`.
  ///
  /// - Precondition: `self != nil`.
  @inlinable // FIXME(sil-serialize-all)
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
  ///
  /// - Warning: Accessing `pointee` as a type that is unrelated to
  ///   the underlying memory's bound type is undefined.
  @inlinable // FIXME(sil-serialize-all)
  @_transparent
  public init<U>(_ from: UnsafeMutablePointer<U>) {
    self._rawValue = from._rawValue
  }

  /// Explicit construction from an UnsafeMutablePointer.
  ///
  /// Returns nil if `from` is nil.
  ///
  /// This is inherently unsafe; UnsafeMutablePointer assumes the
  /// referenced memory has +1 strong ownership semantics, whereas
  /// AutoreleasingUnsafeMutablePointer implies +0 semantics.
  ///
  /// - Warning: Accessing `pointee` as a type that is unrelated to
  ///   the underlying memory's bound type is undefined.
  @inlinable // FIXME(sil-serialize-all)
  @_transparent
  public init?<U>(_ from: UnsafeMutablePointer<U>?) {
    guard let unwrapped = from else { return nil }
    self.init(unwrapped)
  }

  /// Explicit construction from a UnsafePointer.
  ///
  /// This is inherently unsafe because UnsafePointers do not imply
  /// mutability.
  ///
  /// - Warning: Accessing `pointee` as a type that is unrelated to
  ///   the underlying memory's bound type is undefined.
  @inlinable // FIXME(sil-serialize-all)
  @_transparent
  internal init<U>(_ from: UnsafePointer<U>) {
    self._rawValue = from._rawValue
  }

  /// Explicit construction from a UnsafePointer.
  ///
  /// Returns nil if `from` is nil.
  ///
  /// This is inherently unsafe because UnsafePointers do not imply
  /// mutability.
  ///
  /// - Warning: Accessing `pointee` as a type that is unrelated to
  ///   the underlying memory's bound type is undefined.
  @inlinable // FIXME(sil-serialize-all)
  @_transparent
  internal init?<U>(_ from: UnsafePointer<U>?) {
    guard let unwrapped = from else { return nil }
    self.init(unwrapped)
  }
}

extension AutoreleasingUnsafeMutablePointer: Equatable {
  @inlinable // FIXME(sil-serialize-all)
  @_transparent
  public static func == (
    lhs: AutoreleasingUnsafeMutablePointer,
    rhs: AutoreleasingUnsafeMutablePointer
  ) -> Bool {
    return Bool(Builtin.cmp_eq_RawPointer(lhs._rawValue, rhs._rawValue))
  }
}

extension UnsafeMutableRawPointer {
  /// Creates a new raw pointer from an `AutoreleasingUnsafeMutablePointer`
  /// instance.
  ///
  /// - Parameter other: The pointer to convert.
  @inlinable // FIXME(sil-serialize-all)
  @_transparent
  public init<T>(_ other: AutoreleasingUnsafeMutablePointer<T>) {
    _rawValue = other._rawValue
  }

  /// Creates a new raw pointer from an `AutoreleasingUnsafeMutablePointer`
  /// instance.
  ///
  /// - Parameter other: The pointer to convert. If `other` is `nil`, the
  ///   result is `nil`.
  @inlinable // FIXME(sil-serialize-all)
  @_transparent
  public init?<T>(_ other: AutoreleasingUnsafeMutablePointer<T>?) {
    guard let unwrapped = other else { return nil }
    self.init(unwrapped)
  }
}

extension UnsafeRawPointer {
  /// Creates a new raw pointer from an `AutoreleasingUnsafeMutablePointer`
  /// instance.
  ///
  /// - Parameter other: The pointer to convert.
  @inlinable // FIXME(sil-serialize-all)
  @_transparent
  public init<T>(_ other: AutoreleasingUnsafeMutablePointer<T>) {
    _rawValue = other._rawValue
  }

  /// Creates a new raw pointer from an `AutoreleasingUnsafeMutablePointer`
  /// instance.
  ///
  /// - Parameter other: The pointer to convert. If `other` is `nil`, the
  ///   result is `nil`.
  @inlinable // FIXME(sil-serialize-all)
  @_transparent
  public init?<T>(_ other: AutoreleasingUnsafeMutablePointer<T>?) {
    guard let unwrapped = other else { return nil }
    self.init(unwrapped)
  }
}

extension AutoreleasingUnsafeMutablePointer : CustomDebugStringConvertible {
  /// A textual representation of `self`, suitable for debugging.
  @inlinable
  public var debugDescription: String {
    return _rawPointerToString(_rawValue)
  }
}

@_fixed_layout
@usableFromInline
internal struct _CocoaFastEnumerationStackBuf {
  // Clang uses 16 pointers.  So do we.
  @usableFromInline // FIXME(sil-serialize-all)
  internal var _item0: UnsafeRawPointer?
  @usableFromInline // FIXME(sil-serialize-all)
  internal var _item1: UnsafeRawPointer?
  @usableFromInline // FIXME(sil-serialize-all)
  internal var _item2: UnsafeRawPointer?
  @usableFromInline // FIXME(sil-serialize-all)
  internal var _item3: UnsafeRawPointer?
  @usableFromInline // FIXME(sil-serialize-all)
  internal var _item4: UnsafeRawPointer?
  @usableFromInline // FIXME(sil-serialize-all)
  internal var _item5: UnsafeRawPointer?
  @usableFromInline // FIXME(sil-serialize-all)
  internal var _item6: UnsafeRawPointer?
  @usableFromInline // FIXME(sil-serialize-all)
  internal var _item7: UnsafeRawPointer?
  @usableFromInline // FIXME(sil-serialize-all)
  internal var _item8: UnsafeRawPointer?
  @usableFromInline // FIXME(sil-serialize-all)
  internal var _item9: UnsafeRawPointer?
  @usableFromInline // FIXME(sil-serialize-all)
  internal var _item10: UnsafeRawPointer?
  @usableFromInline // FIXME(sil-serialize-all)
  internal var _item11: UnsafeRawPointer?
  @usableFromInline // FIXME(sil-serialize-all)
  internal var _item12: UnsafeRawPointer?
  @usableFromInline // FIXME(sil-serialize-all)
  internal var _item13: UnsafeRawPointer?
  @usableFromInline // FIXME(sil-serialize-all)
  internal var _item14: UnsafeRawPointer?
  @usableFromInline // FIXME(sil-serialize-all)
  internal var _item15: UnsafeRawPointer?

  @inlinable // FIXME(sil-serialize-all)
  @_transparent
  internal var count: Int {
    return 16
  }

  @inlinable // FIXME(sil-serialize-all)
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

    _sanityCheck(MemoryLayout.size(ofValue: self) >=
                   MemoryLayout<Optional<UnsafeRawPointer>>.size * count)
  }
}

/// Get the ObjC type encoding for a type as a pointer to a C string.
///
/// This is used by the Foundation overlays. The compiler will error if the
/// passed-in type is generic or not representable in Objective-C
@inlinable // FIXME(sil-serialize-all)
@_transparent
public func _getObjCTypeEncoding<T>(_ type: T.Type) -> UnsafePointer<Int8> {
  // This must be `@_transparent` because `Builtin.getObjCTypeEncoding` is
  // only supported by the compiler for concrete types that are representable
  // in ObjC.
  return UnsafePointer(Builtin.getObjCTypeEncoding(type))
}

#endif
