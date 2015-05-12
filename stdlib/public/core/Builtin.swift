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

import SwiftShims

// Definitions that make elements of Builtin usable in real code
// without gobs of boilerplate.

/// An initialized raw pointer to use as a NULL value.
@transparent
internal var _nilRawPointer: Builtin.RawPointer {
  let zero: Int8 = 0
  return Builtin.inttoptr_Int8(zero.value)
}

/// Returns the contiguous memory footprint of `T`.
///
/// Does not include any dynamically-allocated or "remote" storage.
/// In particular, `sizeof(X.self)`, when `X` is a class type, is the
/// same regardless of how many stored properties `X` has.
@transparent
public func sizeof<T>(_:T.Type) -> Int {
  return Int(Builtin.sizeof(T.self))
}

/// Returns the contiguous memory footprint of  `T`.
///
/// Does not include any dynamically-allocated or "remote" storage.
/// In particular, `sizeof(a)`, when `a` is a class instance, is the
/// same regardless of how many stored properties `a` has.
@transparent
public func sizeofValue<T>(_:T) -> Int {
  return sizeof(T.self)
}

/// Returns the minimum memory alignment of `T`.
@transparent
public func alignof<T>(_:T.Type) -> Int {
  return Int(Builtin.alignof(T.self))
}

/// Returns the minimum memory alignment of `T`.
@transparent
public func alignofValue<T>(_:T) -> Int {
  return alignof(T.self)
}

/// Returns the least possible interval between distinct instances of
/// `T` in memory.  The result is always positive.
@transparent
public func strideof<T>(_:T.Type) -> Int {
  return Int(Builtin.strideof_nonzero(T.self))
}

/// Returns the least possible interval between distinct instances of
/// `T` in memory.  The result is always positive.
@transparent
public func strideofValue<T>(_:T) -> Int {
  return strideof(T.self)
}

func _roundUpToAlignment(offset: Int, _ alignment: Int) -> Int {
  _sanityCheck(offset >= 0)
  _sanityCheck(alignment > 0)
  _sanityCheck(_isPowerOf2(alignment))
  // Note, given that offset is >= 0, and alignment > 0, we don't
  // need to underflow check the -1, as it can never underflow.
  let x = (offset + alignment &- 1)
  // Note, as alignment is a power of 2, we'll use masking to efficiently
  // get the aligned value
  return x & ~(alignment &- 1)
}

/// Returns a tri-state of 0 = no, 1 = yes, 2 = maybe.
@transparent
public // @testable
func _canBeClass<T>(_: T.Type) -> Int8 {
  return Int8(Builtin.canBeClass(T.self))
}

/// Returns the the bits of `x`, interpreted as having type `U`.
///
/// Caution: Breaks the guarantees of Swift's type system; use
/// with extreme care.  There's almost always a better way to do
/// anything.
///
@transparent
public func unsafeBitCast<T, U>(var x: T, _: U.Type) -> U {
  _precondition(sizeof(T.self) == sizeof(U.self),
    "can't unsafeBitCast between types of different sizes")
  // FIXME: <rdar://problem/18574301> Builtin.reinterpretCast is broken
  // return Builtin.reinterpretCast(x)
  return UnsafeMutablePointer<U>(Builtin.addressof(&x)).memory
}

/// `unsafeBitCast` something to `AnyObject`
@transparent
public func _reinterpretCastToAnyObject<T>(x: T) -> AnyObject {
  return unsafeBitCast(x, AnyObject.self)
}

@transparent
func ==(lhs: Builtin.NativeObject, rhs: Builtin.NativeObject) -> Bool {
  return unsafeBitCast(lhs, Int.self) == unsafeBitCast(rhs, Int.self)
}

@transparent
func !=(lhs: Builtin.NativeObject, rhs: Builtin.NativeObject) -> Bool {
  return !(lhs == rhs)
}

@transparent
func ==(lhs: Builtin.RawPointer, rhs: Builtin.RawPointer) -> Bool {
  return unsafeBitCast(lhs, Int.self) == unsafeBitCast(rhs, Int.self)
}

@transparent
func !=(lhs: Builtin.RawPointer, rhs: Builtin.RawPointer) -> Bool {
  return !(lhs == rhs)
}

/// Return `true` iff `t0` is identical to `t1`; i.e. if they are both
/// `nil` or they both represent the same type.
public func == (t0: Any.Type?, t1: Any.Type?) -> Bool {
  return unsafeBitCast(t0, Int.self) == unsafeBitCast(t1, Int.self)
}

/// Return `false` iff `t0` is identical to `t1`; i.e. if they are both
/// `nil` or they both represent the same type.
public func != (t0: Any.Type?, t1: Any.Type?) -> Bool {
  return !(t0 == t1)
}


/// Tell the optimizer that this code is unreachable if condition is
/// known at compile-time to be true.  If condition is false, or true
/// but not a compile-time constant, this call has no effect.
@transparent
internal func _unreachable(condition: Bool = true) {
  if condition {
    // FIXME: use a parameterized version of Builtin.unreachable when
    // <rdar://problem/16806232> is closed.
    Builtin.unreachable()
  }
}

/// Tell the optimizer that this code is unreachable if this builtin is
/// reachable after constant folding build configuration builtins.
@transparent @noreturn internal
func _conditionallyUnreachable() {
  Builtin.conditionallyUnreachable()
}

@asmname("swift_isClassOrObjCExistential")
func _swift_isClassOrObjCExistential<T>(x: T.Type) -> Bool

/// Returns true iff T is a class type or an @objc existential such as
/// AnyObject
@inline(__always)
internal func _isClassOrObjCExistential<T>(x: T.Type) -> Bool {
  let tmp = _canBeClass(x)

  // Is not a class.
  if tmp == 0 {
    return false
  // Is a class.
  } else if tmp == 1 {
    return true
  }

  // Maybe a class.
  return _swift_isClassOrObjCExistential(x)
}

/// Return an UnsafePointer to the storage used for `object`.  There's
/// not much you can do with this other than use it to identify the
/// object
@transparent
public func unsafeAddressOf(object: AnyObject) -> UnsafePointer<Void> {
  return UnsafePointer(Builtin.bridgeToRawPointer(object))
}

/// - returns: `x as T`.
///
/// - Requires: `x is T`.  In particular, in -O builds, no test is
///   performed to ensure that `x` actually has dynamic type `T`.
///
/// - warning: Trades safety for performance.  Use `unsafeDowncast`
///   only when `x as T` has proven to be a performance problem and you
///   are confident that, always, `x is T`.  It is better than an
///   `unsafeBitCast` because it's more restrictive, and because
///   checking is still performed in debug builds.
@transparent
public func unsafeDowncast<T : AnyObject>(x: AnyObject) -> T {
  _debugPrecondition(x is T, "invalid unsafeDowncast")
  return Builtin.bridgeFromRawPointer(Builtin.bridgeToRawPointer(x))
}

/// - Returns: `nonEmpty!`
///
/// - Requires: `nonEmpty != nil`.  In particular, in -O builds, no test
///   is performed to ensure that `nonEmpty` actually is non-nil.
///
/// - warning: Trades safety for performance.  Use `unsafeUnwrap`
///   only when `nonEmpty!` has proven to be a performance problem and
///   you are confident that, always, `nonEmpty != nil`.  It is better
///   than an `unsafeBitCast` because it's more restrictive, and
///   because checking is still performed in debug builds.
@inline(__always)
public func unsafeUnwrap<T>(nonEmpty: T?) -> T {
  if let x = nonEmpty {
    return x
  }
  _debugPreconditionFailure("unsafeUnwrap of nil optional")
}

/// - Returns: `unsafeUnwrap(nonEmpty)`
///
/// This version is for internal stdlib use; it avoids any checking
/// overhead for users, even in Debug builds.
@inline(__always)
public // SPI(SwiftExperimental)
func _unsafeUnwrap<T>(nonEmpty: T?) -> T {
  if let x = nonEmpty {
    return x
  }
  _sanityCheckFailure("_unsafeUnwrap of nil optional")
}

@inline(__always)
public func _getUnsafePointerToStoredProperties(x: AnyObject)
  -> UnsafeMutablePointer<UInt8> {
  let storedPropertyOffset = _roundUpToAlignment(
    sizeof(_HeapObject.self), alignof(Optional<AnyObject>.self))
  return UnsafeMutablePointer<UInt8>(Builtin.bridgeToRawPointer(x)) +
    storedPropertyOffset
}

//===----------------------------------------------------------------------===//
// Branch hints
//===----------------------------------------------------------------------===//

// Use @_semantics to indicate that the optimizer recognizes the
// semantics of these function calls. This won't be necessary with
// mandatory generic inlining.

@transparent @_semantics("branchhint") internal
func _branchHint<C: BooleanType>(actual: C, _ expected: Bool) -> Bool {
  return Bool(Builtin.int_expect_Int1(actual.boolValue.value, expected.value))
}

/// Optimizer hint that `x` is expected to be `true`
@transparent @_semantics("fastpath") public
func _fastPath<C: BooleanType>(x: C) -> Bool {
  return _branchHint(x.boolValue, true)
}

/// Optimizer hint that `x` is expected to be `false`
@transparent @_semantics("slowpath") public
func _slowPath<C: BooleanType>(x: C) -> Bool {
  return _branchHint(x.boolValue, false)
}

//===--- Runtime shim wrappers --------------------------------------------===//

/// Returns `true` iff the class indicated by `theClass` uses native
/// Swift reference-counting
@inline(__always)
internal func _usesNativeSwiftReferenceCounting(theClass: AnyClass) -> Bool {
#if _runtime(_ObjC)
  return _swift_usesNativeSwiftReferenceCounting_class(
    unsafeAddressOf(theClass)
  )
#else
  return true
#endif
}

@asmname("_swift_class_getInstancePositiveExtentSize_native")
func _swift_class_getInstancePositiveExtentSize_native(theClass: AnyClass) -> UInt

/// - Returns: `class_getInstanceSize(theClass)`
@inline(__always)
internal func _class_getInstancePositiveExtentSize(theClass: AnyClass) -> Int {
#if _runtime(_ObjC)
  return Int(_swift_class_getInstancePositiveExtentSize(
      unsafeAddressOf(theClass)))
#else
  return Int(_swift_class_getInstancePositiveExtentSize_native(theClass))
#endif
}

@asmname("_swift_isClass")
public func _swift_isClass(x: Any) -> Bool

//===--- Builtin.BridgeObject ---------------------------------------------===//

#if arch(i386) || arch(arm)
internal var _objectPointerSpareBits: UInt {
    @inline(__always) get { return 0x0000_0003 }
}
internal var _objectPointerIsObjCBit: UInt {
    @inline(__always) get { return 0x0000_0002 }
}
internal var _objectPointerLowSpareBitShift: UInt {
    @inline(__always) get { return 0 }
}
internal var _objCTaggedPointerBits: UInt {
  @inline(__always) get { return 0 }
}
#elseif arch(x86_64)
internal var _objectPointerSpareBits: UInt {
  @inline(__always) get { return 0x7F00_0000_0000_0006 }
}
internal var _objectPointerIsObjCBit: UInt {
  @inline(__always) get { return 0x4000_0000_0000_0000 }
}
internal var _objectPointerLowSpareBitShift: UInt {
  @inline(__always) get { return 1 }
}
internal var _objCTaggedPointerBits: UInt {
  @inline(__always) get { return 0x8000_0000_0000_0001 }
}
#elseif arch(arm64)
internal var _objectPointerSpareBits: UInt {
  @inline(__always) get { return 0x7F00_0000_0000_0007 }
}
internal var _objectPointerIsObjCBit: UInt {
  @inline(__always) get { return 0x4000_0000_0000_0000 }
}
internal var _objectPointerLowSpareBitShift: UInt {
    @inline(__always) get { return 0 }
}
internal var _objCTaggedPointerBits: UInt {
    @inline(__always) get { return 0x8000_0000_0000_0000 }
}
#endif

/// Extract the raw bits of `x`
@inline(__always)
internal func _bitPattern(x: Builtin.BridgeObject) -> UInt {
  return UInt(Builtin.castBitPatternFromBridgeObject(x))
}

/// Extract the raw spare bits of `x`
@inline(__always)
internal func _nonPointerBits(x: Builtin.BridgeObject) -> UInt {
  return _bitPattern(x) & _objectPointerSpareBits
}

@inline(__always)
internal func _isObjCTaggedPointer(x: AnyObject) -> Bool {
  return (Builtin.reinterpretCast(x) & _objCTaggedPointerBits) != 0
}

/// Create a `BridgeObject` around the given `nativeObject` with the
/// given spare bits.
///
/// Reference-counting and other operations on this
/// object will have access to the knowledge that it is native.
///
/// - Requires: `bits & _objectPointerIsObjCBit == 0`,
///   `bits & _objectPointerSpareBits == bits`.
@inline(__always)
internal func _makeNativeBridgeObject(
  nativeObject: AnyObject, _ bits: UInt
) -> Builtin.BridgeObject {
  _sanityCheck(
    (bits & _objectPointerIsObjCBit) == 0,
    "BridgeObject is treated as non-native when ObjC bit is set"
  )
  return _makeBridgeObject(nativeObject, bits)
}

/// Create a `BridgeObject` around the given `objCObject`.
@inline(__always)
internal func _makeObjCBridgeObject(
  objCObject: AnyObject
) -> Builtin.BridgeObject {
  return _makeBridgeObject(
    objCObject,
    _isObjCTaggedPointer(objCObject) ? 0 : _objectPointerIsObjCBit)
}

/// Create a `BridgeObject` around the given `object` with the
/// given spare bits.
///
/// - Requires:
///
///   1. `bits & _objectPointerSpareBits == bits`
///   2. if `object` is a tagged pointer, `bits == 0`.  Otherwise,
///      `object` is either a native object, or `bits ==
///      _objectPointerIsObjCBit`.
@inline(__always)
internal func _makeBridgeObject(
  object: AnyObject, _ bits: UInt
) -> Builtin.BridgeObject {
  _sanityCheck(!_isObjCTaggedPointer(object) || bits == 0,
    "Tagged pointers cannot be combined with bits")

  _sanityCheck(
    _isObjCTaggedPointer(object)
    || _usesNativeSwiftReferenceCounting(object.dynamicType)
    || bits == _objectPointerIsObjCBit,
    "All spare bits must be set in non-native, non-tagged bridge objects"
  )

  _sanityCheck(
    bits & _objectPointerSpareBits == bits,
    "Can't store non-spare bits into Builtin.BridgeObject")

  return Builtin.castToBridgeObject(
    object, bits._builtinWordValue
  )
}

/// Return the superclass of `t`, if any.  The result is nil if `t` is
/// a root class or class protocol.
@inline(__always)
public // @testable
func _getSuperclass(t: AnyClass) -> AnyClass? {
  return unsafeBitCast(
    _swift_getSuperclass_nonNull(unsafeBitCast(t, COpaquePointer.self)),
    AnyClass.self)
}

/// Return the superclass of `t`, if any.  The result is nil if `t` is
/// not a class, is a root class, or is a class protocol.
@inline(__always)
public // @testable
func _getSuperclass(t: Any.Type) -> AnyClass? {
  return (t as? AnyClass).flatMap { _getSuperclass($0) }
}

//===--- Builtin.IsUnique -------------------------------------------------===//
// _isUnique functions must take an inout object because they rely on
// Builtin.isUnique which requires an inout reference to preserve
// source-level copies in the presence of ARC optimization.
//
// Taking an inout object makes sense for two additional reasons:
//
// 1. You should only call it when about to mutate the object.
//    Doing so otherwise implies a race condition if the buffer is
//    shared across threads.
//
// 2. When it is not an inout function, self is passed by
//    value... thus bumping the reference count and disturbing the
//    result we are trying to observe, Dr. Heisenberg!
//
// _isUnique and _isUniquePinned cannot be made public or the compiler
// will attempt to generate generic code for the transparent function
// and type checking will fail.

/// Return true if `object` is uniquely referenced.
@transparent
internal func _isUnique<T>(inout object: T) -> Bool {
  return Bool(Builtin.isUnique(&object))
}

/// Return true if `object` is uniquely referenced or pinned.
@transparent
internal func _isUniqueOrPinned<T>(inout object: T) -> Bool {
  return Bool(Builtin.isUniqueOrPinned(&object))
}

/// Return true if `object` is uniquely referenced.
/// This provides sanity checks on top of the Builtin.
@transparent
public // @testable
func _isUnique_native<T>(inout object: T) -> Bool {
  // This could be a bridge object, single payload enum, or plain old
  // reference. Any any case it's non pointer bits must be zero.
  //
  // FIXME: We should be able to do:
  // _sanityCheck((_bitPattern(object) & _objectPointerSpareBits) == 0)
  // _sanityCheck(_usesNativeSwiftReferenceCounting(
  //   (Builtin.convertFromRawPointer(
  //      Builtin.reinterpretCastToRawPointer(object))
  //    as AnyObject).dynamicType))
  return Bool(Builtin.isUnique_native(&object))
}

/// Return true if `object` is uniquely referenced or pinned.
/// This provides sanity checks on top of the Builtin.
@transparent
public // @testable
func _isUniqueOrPinned_native<T>(inout object: T) -> Bool {
  // This could be a bridge object, single payload enum, or plain old
  // reference. Any any case it's non pointer bits must be zero.
  //
  // FIXME: We should be able to sanityCheck as shown in isUnique_native.
  return Bool(Builtin.isUniqueOrPinned_native(&object))
}
