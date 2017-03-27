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

import SwiftShims

// Definitions that make elements of Builtin usable in real code
// without gobs of boilerplate.

@available(*, unavailable, message: "use MemoryLayout<T>.size instead.")
public func sizeof<T>(_:T.Type) -> Int {
  Builtin.unreachable()
}

@available(*, unavailable, renamed: "MemoryLayout.size(ofValue:)")
public func sizeofValue<T>(_:T) -> Int {
  Builtin.unreachable()
}

@available(*, unavailable, message: "use MemoryLayout<T>.alignment instead.")
public func alignof<T>(_:T.Type) -> Int {
  Builtin.unreachable()
}

@available(*, unavailable, renamed: "MemoryLayout.alignment(ofValue:)")
public func alignofValue<T>(_:T) -> Int {
  Builtin.unreachable()
}

@available(*, unavailable, message: "use MemoryLayout<T>.stride instead.")
public func strideof<T>(_:T.Type) -> Int {
  Builtin.unreachable()
}

@available(*, unavailable, renamed: "MemoryLayout.stride(ofValue:)")
public func strideofValue<T>(_:T) -> Int {
  Builtin.unreachable()
}

// This function is the implementation of the `_roundUp` overload set.  It is
// marked `@inline(__always)` to make primary `_roundUp` entry points seem
// cheap enough for the inliner.
@_versioned
@inline(__always)
internal func _roundUpImpl(_ offset: UInt, toAlignment alignment: Int) -> UInt {
  _sanityCheck(alignment > 0)
  _sanityCheck(_isPowerOf2(alignment))
  // Note, given that offset is >= 0, and alignment > 0, we don't
  // need to underflow check the -1, as it can never underflow.
  let x = offset + UInt(bitPattern: alignment) &- 1
  // Note, as alignment is a power of 2, we'll use masking to efficiently
  // get the aligned value
  return x & ~(UInt(bitPattern: alignment) &- 1)
}

@_versioned
internal func _roundUp(_ offset: UInt, toAlignment alignment: Int) -> UInt {
  return _roundUpImpl(offset, toAlignment: alignment)
}

@_versioned
internal func _roundUp(_ offset: Int, toAlignment alignment: Int) -> Int {
  _sanityCheck(offset >= 0)
  return Int(_roundUpImpl(UInt(bitPattern: offset), toAlignment: alignment))
}

/// Returns a tri-state of 0 = no, 1 = yes, 2 = maybe.
@_transparent
public // @testable
func _canBeClass<T>(_: T.Type) -> Int8 {
  return Int8(Builtin.canBeClass(T.self))
}

/// Returns the bits of the given instance, interpreted as having the specified
/// type.
///
/// Only use this function to convert the instance passed as `x` to a
/// layout-compatible type when the conversion is not possible through other
/// means. Common conversions that are supported by the standard library
/// include the following:
///
/// - To convert an integer value from one type to another, use an initializer
///   or the `numericCast(_:)` function.
/// - To perform a bitwise conversion of an integer value to a different type,
///   use an `init(bitPattern:)` or `init(truncatingBitPattern:)` initializer.
/// - To convert between a pointer and an integer value with that bit pattern,
///   or vice versa, use the `init(bitPattern:)` initializer for the
///   destination type.
/// - To perform a reference cast, use the casting operators (`as`, `as!`, or
///   `as?`) or the `unsafeDowncast(_:to:)` function. Do not use
///   `unsafeBitCast(_:to:)` with class or pointer types; doing so may
///   introduce undefined behavior.
///
/// - Warning: Calling this function breaks the guarantees of Swift's type
///   system; use with extreme care.
///
/// - Parameters:
///   - x: The instance to cast to `type`.
///   - type: The type to cast `x` to. `type` and the type of `x` must have the
///     same size of memory representation and compatible memory layout.
/// - Returns: A new instance of type `U`, cast from `x`.
@_transparent
public func unsafeBitCast<T, U>(_ x: T, to type: U.Type) -> U {
  _precondition(MemoryLayout<T>.size == MemoryLayout<U>.size,
    "can't unsafeBitCast between types of different sizes")
  return Builtin.reinterpretCast(x)
}

/// `unsafeBitCast` something to `AnyObject`.
@_transparent
internal func _reinterpretCastToAnyObject<T>(_ x: T) -> AnyObject {
  return unsafeBitCast(x, to: AnyObject.self)
}

@_inlineable
@_versioned
@_transparent
func == (lhs: Builtin.NativeObject, rhs: Builtin.NativeObject) -> Bool {
  return unsafeBitCast(lhs, to: Int.self) == unsafeBitCast(rhs, to: Int.self)
}

@_inlineable
@_versioned
@_transparent
func != (lhs: Builtin.NativeObject, rhs: Builtin.NativeObject) -> Bool {
  return !(lhs == rhs)
}

@_inlineable
@_versioned
@_transparent
func == (lhs: Builtin.RawPointer, rhs: Builtin.RawPointer) -> Bool {
  return unsafeBitCast(lhs, to: Int.self) == unsafeBitCast(rhs, to: Int.self)
}

@_inlineable
@_versioned
@_transparent
func != (lhs: Builtin.RawPointer, rhs: Builtin.RawPointer) -> Bool {
  return !(lhs == rhs)
}

/// Returns `true` iff `t0` is identical to `t1`; i.e. if they are both
/// `nil` or they both represent the same type.
@_inlineable
public func == (t0: Any.Type?, t1: Any.Type?) -> Bool {
  return unsafeBitCast(t0, to: Int.self) == unsafeBitCast(t1, to: Int.self)
}

/// Returns `false` iff `t0` is identical to `t1`; i.e. if they are both
/// `nil` or they both represent the same type.
@_inlineable
public func != (t0: Any.Type?, t1: Any.Type?) -> Bool {
  return !(t0 == t1)
}


/// Tell the optimizer that this code is unreachable if condition is
/// known at compile-time to be true.  If condition is false, or true
/// but not a compile-time constant, this call has no effect.
@_transparent
internal func _unreachable(_ condition: Bool = true) {
  if condition {
    // FIXME: use a parameterized version of Builtin.unreachable when
    // <rdar://problem/16806232> is closed.
    Builtin.unreachable()
  }
}

/// Tell the optimizer that this code is unreachable if this builtin is
/// reachable after constant folding build configuration builtins.
@_versioned @_transparent internal
func _conditionallyUnreachable() -> Never {
  Builtin.conditionallyUnreachable()
}

@_versioned
@_silgen_name("_swift_isClassOrObjCExistentialType")
func _swift_isClassOrObjCExistentialType<T>(_ x: T.Type) -> Bool

/// Returns `true` iff `T` is a class type or an `@objc` existential such as
/// `AnyObject`.
@_versioned
@inline(__always)
internal func _isClassOrObjCExistential<T>(_ x: T.Type) -> Bool {

  switch _canBeClass(x) {
  // Is not a class.
  case 0:
    return false
  // Is a class.
  case 1:
    return true
  // Maybe a class.
  default:
    return _swift_isClassOrObjCExistentialType(x)
  }
}

/// Returns an `UnsafePointer` to the storage used for `object`.  There's
/// not much you can do with this other than use it to identify the
/// object.
@available(*, unavailable, message: "Removed in Swift 3. Use Unmanaged.passUnretained(x).toOpaque() instead.")
public func unsafeAddress(of object: AnyObject) -> UnsafeRawPointer {
  Builtin.unreachable()
}

@available(*, unavailable, message: "Removed in Swift 3. Use Unmanaged.passUnretained(x).toOpaque() instead.")
public func unsafeAddressOf(_ object: AnyObject) -> UnsafeRawPointer {
  Builtin.unreachable()
}

/// Converts a reference of type `T` to a reference of type `U` after
/// unwrapping one level of Optional.
///
/// Unwrapped `T` and `U` must be convertible to AnyObject. They may
/// be either a class or a class protocol. Either T, U, or both may be
/// optional references.
@_transparent
public func _unsafeReferenceCast<T, U>(_ x: T, to: U.Type) -> U {
  return Builtin.castReference(x)
}

/// - returns: `x as T`.
///
/// - Precondition: `x is T`.  In particular, in -O builds, no test is
///   performed to ensure that `x` actually has dynamic type `T`.
///
/// - Warning: Trades safety for performance.  Use `unsafeDowncast`
///   only when `x as! T` has proven to be a performance problem and you
///   are confident that, always, `x is T`.  It is better than an
///   `unsafeBitCast` because it's more restrictive, and because
///   checking is still performed in debug builds.
@_transparent
public func unsafeDowncast<T : AnyObject>(_ x: AnyObject, to: T.Type) -> T {
  _debugPrecondition(x is T, "invalid unsafeDowncast")
  return Builtin.castReference(x)
}

@inline(__always)
public func _getUnsafePointerToStoredProperties(_ x: AnyObject)
  -> UnsafeMutableRawPointer {
  let storedPropertyOffset = _roundUp(
    MemoryLayout<_HeapObject>.size,
    toAlignment: MemoryLayout<Optional<AnyObject>>.alignment)
  return UnsafeMutableRawPointer(Builtin.bridgeToRawPointer(x)) +
    storedPropertyOffset
}

//===----------------------------------------------------------------------===//
// Branch hints
//===----------------------------------------------------------------------===//

// Use @_semantics to indicate that the optimizer recognizes the
// semantics of these function calls. This won't be necessary with
// mandatory generic inlining.

@_versioned
@_transparent
@_semantics("branchhint")
internal func _branchHint(_ actual: Bool, expected: Bool) -> Bool {
  return Bool(Builtin.int_expect_Int1(actual._value, expected._value))
}

/// Optimizer hint that `x` is expected to be `true`.
@_transparent
@_semantics("fastpath")
public func _fastPath(_ x: Bool) -> Bool {
  return _branchHint(x, expected: true)
}

/// Optimizer hint that `x` is expected to be `false`.
@_transparent
@_semantics("slowpath")
public func _slowPath(_ x: Bool) -> Bool {
  return _branchHint(x, expected: false)
}

/// Optimizer hint that the code where this function is called is on the fast
/// path.
@_transparent
public func _onFastPath() {
  Builtin.onFastPath()
}

//===--- Runtime shim wrappers --------------------------------------------===//

/// Returns `true` iff the class indicated by `theClass` uses native
/// Swift reference-counting.
#if _runtime(_ObjC)
// Declare it here instead of RuntimeShims.h, because we need to specify
// the type of argument to be AnyClass. This is currently not possible
// when using RuntimeShims.h
@_versioned
@_silgen_name("swift_objc_class_usesNativeSwiftReferenceCounting")
func _usesNativeSwiftReferenceCounting(_ theClass: AnyClass) -> Bool
#else
@_versioned
@inline(__always)
func _usesNativeSwiftReferenceCounting(_ theClass: AnyClass) -> Bool {
  return true
}
#endif

@_silgen_name("swift_class_getInstanceExtents")
func swift_class_getInstanceExtents(_ theClass: AnyClass)
  -> (negative: UInt, positive: UInt)

@_silgen_name("swift_objc_class_unknownGetInstanceExtents")
func swift_objc_class_unknownGetInstanceExtents(_ theClass: AnyClass)
  -> (negative: UInt, positive: UInt)

/// - Returns: 
@inline(__always)
internal func _class_getInstancePositiveExtentSize(_ theClass: AnyClass) -> Int {
#if _runtime(_ObjC)
  return Int(swift_objc_class_unknownGetInstanceExtents(theClass).positive)
#else
  return Int(swift_class_getInstanceExtents(theClass).positive)
#endif
}

//===--- Builtin.BridgeObject ---------------------------------------------===//

#if arch(i386) || arch(arm)
@_versioned
internal var _objectPointerSpareBits: UInt {
    @inline(__always) get { return 0x0000_0003 }
}
@_versioned
internal var _objectPointerIsObjCBit: UInt {
    @inline(__always) get { return 0x0000_0002 }
}
@_versioned
internal var _objectPointerLowSpareBitShift: UInt {
    @inline(__always) get { return 0 }
}
@_versioned
internal var _objCTaggedPointerBits: UInt {
  @inline(__always) get { return 0 }
}
#elseif arch(x86_64)
@_versioned
internal var _objectPointerSpareBits: UInt {
  @inline(__always) get { return 0x7F00_0000_0000_0006 }
}
@_versioned
internal var _objectPointerIsObjCBit: UInt {
  @inline(__always) get { return 0x4000_0000_0000_0000 }
}
@_versioned
internal var _objectPointerLowSpareBitShift: UInt {
  @inline(__always) get { return 1 }
}
@_versioned
internal var _objCTaggedPointerBits: UInt {
  @inline(__always) get { return 0x8000_0000_0000_0001 }
}
#elseif arch(arm64)
@_versioned
internal var _objectPointerSpareBits: UInt {
  @inline(__always) get { return 0x7F00_0000_0000_0007 }
}
@_versioned
internal var _objectPointerIsObjCBit: UInt {
  @inline(__always) get { return 0x4000_0000_0000_0000 }
}
@_versioned
internal var _objectPointerLowSpareBitShift: UInt {
    @inline(__always) get { return 0 }
}
@_versioned
internal var _objCTaggedPointerBits: UInt {
    @inline(__always) get { return 0x8000_0000_0000_0000 }
}
#elseif arch(powerpc64) || arch(powerpc64le)
@_versioned
internal var _objectPointerSpareBits: UInt {
  @inline(__always) get { return 0x0000_0000_0000_0007 }
}
@_versioned
internal var _objectPointerIsObjCBit: UInt {
  @inline(__always) get { return 0x0000_0000_0000_0002 }
}
@_versioned
internal var _objectPointerLowSpareBitShift: UInt {
    @inline(__always) get { return 0 }
}
@_versioned
internal var _objCTaggedPointerBits: UInt {
    @inline(__always) get { return 0 }
}
#elseif arch(s390x)
@_versioned
internal var _objectPointerSpareBits: UInt {
  @inline(__always) get { return 0x0000_0000_0000_0007 }
}
@_versioned
internal var _objectPointerIsObjCBit: UInt {
  @inline(__always) get { return 0x0000_0000_0000_0002 }
}
@_versioned
internal var _objectPointerLowSpareBitShift: UInt {
  @inline(__always) get { return 0 }
}
@_versioned
internal var _objCTaggedPointerBits: UInt {
  @inline(__always) get { return 0 }
}
#endif

/// Extract the raw bits of `x`.
@_versioned
@inline(__always)
internal func _bitPattern(_ x: Builtin.BridgeObject) -> UInt {
  return UInt(Builtin.castBitPatternFromBridgeObject(x))
}

/// Extract the raw spare bits of `x`.
@_versioned
@inline(__always)
internal func _nonPointerBits(_ x: Builtin.BridgeObject) -> UInt {
  return _bitPattern(x) & _objectPointerSpareBits
}

@_versioned
@inline(__always)
internal func _isObjCTaggedPointer(_ x: AnyObject) -> Bool {
  return (Builtin.reinterpretCast(x) & _objCTaggedPointerBits) != 0
}

/// Create a `BridgeObject` around the given `nativeObject` with the
/// given spare bits.
///
/// Reference-counting and other operations on this
/// object will have access to the knowledge that it is native.
///
/// - Precondition: `bits & _objectPointerIsObjCBit == 0`,
///   `bits & _objectPointerSpareBits == bits`.
@_versioned
@inline(__always)
internal func _makeNativeBridgeObject(
  _ nativeObject: AnyObject, _ bits: UInt
) -> Builtin.BridgeObject {
  _sanityCheck(
    (bits & _objectPointerIsObjCBit) == 0,
    "BridgeObject is treated as non-native when ObjC bit is set"
  )
  return _makeBridgeObject(nativeObject, bits)
}

/// Create a `BridgeObject` around the given `objCObject`.
@inline(__always)
public // @testable
func _makeObjCBridgeObject(
  _ objCObject: AnyObject
) -> Builtin.BridgeObject {
  return _makeBridgeObject(
    objCObject,
    _isObjCTaggedPointer(objCObject) ? 0 : _objectPointerIsObjCBit)
}

/// Create a `BridgeObject` around the given `object` with the
/// given spare bits.
///
/// - Precondition:
///
///   1. `bits & _objectPointerSpareBits == bits`
///   2. if `object` is a tagged pointer, `bits == 0`.  Otherwise,
///      `object` is either a native object, or `bits ==
///      _objectPointerIsObjCBit`.
@_versioned
@inline(__always)
internal func _makeBridgeObject(
  _ object: AnyObject, _ bits: UInt
) -> Builtin.BridgeObject {
  _sanityCheck(!_isObjCTaggedPointer(object) || bits == 0,
    "Tagged pointers cannot be combined with bits")

  _sanityCheck(
    _isObjCTaggedPointer(object)
    || _usesNativeSwiftReferenceCounting(type(of: object))
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

@_versioned
@_silgen_name("_swift_class_getSuperclass")
internal func _swift_class_getSuperclass(_ t: AnyClass) -> AnyClass?

/// Returns the superclass of `t`, if any.  The result is `nil` if `t` is
/// a root class or class protocol.
@inline(__always)
public // @testable
func _getSuperclass(_ t: AnyClass) -> AnyClass? {
  return _swift_class_getSuperclass(t)
}

/// Returns the superclass of `t`, if any.  The result is `nil` if `t` is
/// not a class, is a root class, or is a class protocol.
@inline(__always)
public // @testable
func _getSuperclass(_ t: Any.Type) -> AnyClass? {
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

/// Returns `true` if `object` is uniquely referenced.
@_inlineable
@_transparent  // WARNING: do not remove this or uniqueness testing will break!
public // testable
func _isUnique<T>(_ object: inout T) -> Bool {
  return Bool(Builtin.isUnique(&object))
}

/// Returns `true` if `object` is uniquely referenced or pinned.
@_versioned
@_transparent
internal func _isUniqueOrPinned<T>(_ object: inout T) -> Bool {
  return Bool(Builtin.isUniqueOrPinned(&object))
}

/// Returns `true` if `object` is uniquely referenced.
/// This provides sanity checks on top of the Builtin.
@_transparent
public // @testable
func _isUnique_native<T>(_ object: inout T) -> Bool {
  // This could be a bridge object, single payload enum, or plain old
  // reference. Any case it's non pointer bits must be zero, so
  // force cast it to BridgeObject and check the spare bits.
  _sanityCheck(
    (_bitPattern(Builtin.reinterpretCast(object)) & _objectPointerSpareBits)
    == 0)
  _sanityCheck(_usesNativeSwiftReferenceCounting(
      type(of: Builtin.reinterpretCast(object) as AnyObject)))
  return Bool(Builtin.isUnique_native(&object))
}

/// Returns `true` if `object` is uniquely referenced or pinned.
/// This provides sanity checks on top of the Builtin.
@_transparent
public // @testable
func _isUniqueOrPinned_native<T>(_ object: inout T) -> Bool {
  // This could be a bridge object, single payload enum, or plain old
  // reference. Any case it's non pointer bits must be zero.
  _sanityCheck(
    (_bitPattern(Builtin.reinterpretCast(object)) & _objectPointerSpareBits)
    == 0)
  _sanityCheck(_usesNativeSwiftReferenceCounting(
      type(of: Builtin.reinterpretCast(object) as AnyObject)))
  return Bool(Builtin.isUniqueOrPinned_native(&object))
}

/// Returns `true` if type is a POD type. A POD type is a type that does not
/// require any special handling on copying or destruction.
@_transparent
public // @testable
func _isPOD<T>(_ type: T.Type) -> Bool {
  return Bool(Builtin.ispod(type))
}

/// Returns `true` if type is nominally an Optional type.
@_transparent
public // @testable
func _isOptional<T>(_ type: T.Type) -> Bool {
  return Bool(Builtin.isOptional(type))
}

@available(*, unavailable, message: "Removed in Swift 3. Please use Optional.unsafelyUnwrapped instead.")
public func unsafeUnwrap<T>(_ nonEmpty: T?) -> T {
  Builtin.unreachable()
}

/// Extract an object reference from an Any known to contain an object.
internal func _unsafeDowncastToAnyObject(fromAny any: Any) -> AnyObject {
  _sanityCheck(type(of: any) is AnyObject.Type
               || type(of: any) is AnyObject.Protocol,
               "Any expected to contain object reference")
  // With a SIL instruction, we could more efficiently grab the object reference
  // out of the Any's inline storage.

  // On Linux, bridging isn't supported, so this is a force cast.
#if _runtime(_ObjC)
  return any as AnyObject
#else
  return any as! AnyObject
#endif
}

// Game the SIL diagnostic pipeline by inlining this into the transparent
// definitions below after the stdlib's diagnostic passes run, so that the
// `staticReport`s don't fire while building the standard library, but do
// fire if they ever show up in code that uses the standard library.
@inline(__always)
public // internal with availability
func _trueAfterDiagnostics() -> Builtin.Int1 {
  return true._value
}

/// Returns the dynamic type of a value.
///
/// You can use the `type(of:)` function to find the dynamic type of a value,
/// particularly when the dynamic type is different from the static type. The
/// *static type* of a value is the known, compile-time type of the value. The
/// *dynamic type* of a value is the value's actual type at run-time, which
/// may be nested inside its concrete type.
///
/// In the following code, the `count` variable has the same static and dynamic
/// type: `Int`. When `count` is passed to the `printInfo(_:)` function,
/// however, the `value` parameter has a static type of `Any`, the type
/// declared for the parameter, and a dynamic type of `Int`.
///
///     func printInfo(_ value: Any) {
///         let type = type(of: value)
///         print("'\(value)' of type '\(type)'")
///     }
///
///     let count: Int = 5
///     printInfo(count)
///     // '5' of type 'Int'
///
/// The dynamic type returned from `type(of:)` is a *concrete metatype*
/// (`T.Type`) for a class, structure, enumeration, or other non-protocol type
/// `T`, or an *existential metatype* (`P.Type`) for a protocol or protocol
/// composition `P`. When the static type of the value passed to `type(of:)`
/// is constrained to a class or protocol, you can use that metatype to access
/// initializers or other static members of the class or protocol.
///
/// For example, the parameter passed as `value` to the `printSmileyInfo(_:)`
/// function in the example below is an instance of the `Smiley` class or one
/// of its subclasses. The function uses `type(of:)` to find the dynamic type
/// of `value`, which itself is an instance of the `Smiley.Type` metatype.
///
///     class Smiley {
///         class var text: String {
///             return ":)"
///         }
///     }
///
///     class EmojiSmiley : Smiley {
///          override class var text: String {
///             return "😀"
///         }
///     }
///
///     func printSmileyInfo(_ value: Smiley) {
///         let smileyType = type(of: value)
///         print("Smile!", smileyType.text)
///     }
///
///     let emojiSmiley = EmojiSmiley()
///     printSmileyInfo(emojiSmiley)
///     // Smile! 😀
///
/// In this example, accessing the `text` property of the `smileyType` metatype
/// retrieves the overridden value from the `EmojiSmiley` subclass, instead of
/// the `Smiley` class's original definition.
///
/// Normally, you don't need to be aware of the difference between concrete and
/// existential metatypes, but calling `type(of:)` can yield unexpected
/// results in a generic context with a type parameter bound to a protocol. In
/// a case like this, where a generic parameter `T` is bound to a protocol
/// `P`, the type parameter is not statically known to be a protocol type in
/// the body of the generic function, so `type(of:)` can only produce the
/// concrete metatype `P.Protocol`.
///
/// The following example defines a `printGenericInfo(_:)` function that takes
/// a generic parameter and declares the `String` type's conformance to a new
/// protocol `P`. When `printGenericInfo(_:)` is called with a string that has
/// `P` as its static type, the call to `type(of:)` returns `P.self` instead
/// of the dynamic type inside the parameter, `String.self`.
///
///     func printGenericInfo<T>(_ value: T) {
///         let type = type(of: value)
///         print("'\(value)' of type '\(type)'")
///     }
///
///     protocol P {}
///     extension String: P {}
///
///     let stringAsP: P = "Hello!"
///     printGenericInfo(stringAsP)
///     // 'Hello!' of type 'P'
///
/// This unexpected result occurs because the call to `type(of: value)` inside
/// `printGenericInfo(_:)` must return a metatype that is an instance of
/// `T.Type`, but `String.self` (the expected dynamic type) is not an instance
/// of `P.Type` (the concrete metatype of `value`). To get the dynamic type
/// inside `value` in this generic context, cast the parameter to `Any` when
/// calling `type(of:)`.
///
///     func betterPrintGenericInfo<T>(_ value: T) {
///         let type = type(of: value as Any)
///         print("'\(value)' of type '\(type)'")
///     }
///
///     betterPrintGenericInfo(stringAsP)
///     // 'Hello!' of type 'String'
///
/// - Parameter value: The value to find the dynamic type of.
/// - Returns: The dynamic type, which is a value of metatype type.
@_transparent
@_semantics("typechecker.type(of:)")
public func type<T, Metatype>(of value: T) -> Metatype {
  // This implementation is never used, since calls to `Swift.type(of:)` are
  // resolved as a special case by the type checker.
  Builtin.staticReport(_trueAfterDiagnostics(), true._value,
    ("internal consistency error: 'type(of:)' operation failed to resolve"
     as StaticString).utf8Start._rawValue)
  Builtin.unreachable()
}

/// Allows a nonescaping closure to temporarily be used as if it were allowed
/// to escape.
///
/// You can use this function to call an API that takes an escaping closure in
/// a way that doesn't allow the closure to escape in practice. The examples
/// below demonstrate how to use `withoutActuallyEscaping(_:do:)` in
/// conjunction with two common APIs that use escaping closures: lazy
/// collection views and asynchronous operations.
///
/// The following code declares an `allValues(in:match:)` function that checks
/// whether all the elements in an array match a predicate. The function won't
/// compile as written, because a lazy collection's `filter(_:)` method
/// requires an escaping closure. The lazy collection isn't persisted, so the
/// `predicate` closure won't actually escape the body of the function, but
/// even so it can't be used in this way.
///
///     func allValues(in array: [Int], match predicate: (Int) -> Bool) -> Bool {
///         return array.lazy.filter { !predicate($0) }.isEmpty
///     }
///     // error: closure use of non-escaping parameter 'predicate'...
///
/// `withoutActuallyEscaping(_:do:)` provides a temporarily-escapable copy of
/// `predicate` that _can_ be used in a call to the lazy view's `filter(_:)`
/// method. The second version of `allValues(in:match:)` compiles without
/// error, with the compiler guaranteeing that the `escapablePredicate`
/// closure doesn't last beyond the call to `withoutActuallyEscaping(_:do:)`.
///
///     func allValues(in array: [Int], match predicate: (Int) -> Bool) -> Bool {
///         return withoutActuallyEscaping(predicate) { escapablePredicate in
///             array.lazy.filter { !escapablePredicate($0) }.isEmpty
///         }
///     }
///
/// Asynchronous calls are another type of API that typically escape their
/// closure arguments. The following code declares a
/// `perform(_:simultaneouslyWith:)` function that uses a dispatch queue to
/// execute two closures concurrently.
///
///     func perform(_ f: () -> Void, simultaneouslyWith g: () -> Void) {
///         let queue = DispatchQueue(label: "perform", attributes: .concurrent)
///         queue.async(execute: f)
///         queue.async(execute: g)
///         queue.sync(flags: .barrier) {}
///     }
///     // error: passing non-escaping parameter 'f'...
///     // error: passing non-escaping parameter 'g'...
///
/// The `perform(_:simultaneouslyWith:)` function ends with a call to the
/// `sync(flags:execute:)` method using the `.barrier` flag, which forces the
/// function to wait until both closures have completed running before
/// returning. Even though the barrier guarantees that neither closure will
/// escape the function, the `async(execute:)` method still requires that the
/// closures passed be marked as `@escaping`, so the first version of the
/// function does not compile. To resolve this, you can use
/// `withoutActuallyEscaping(_:do:)` to get copies of `f` and `g` that can be
/// passed to `async(execute:)`.
///
///     func perform(_ f: () -> Void, simultaneouslyWith g: () -> Void) {
///         withoutActuallyEscaping(f) { escapableF in
///             withoutActuallyEscaping(g) { escapableG in
///                 let queue = DispatchQueue(label: "perform", attributes: .concurrent)
///                 queue.async(execute: escapableF)
///                 queue.async(execute: escapableG)
///                 queue.sync(flags: .barrier) {}
///             }
///         }
///     }
///
/// - Important: The escapable copy of `closure` passed as `body` is only valid
///   during the call to `withoutActuallyEscaping(_:do:)`. It is undefined
///   behavior for the escapable closure to be stored, referenced, or executed
///   after the function returns.
///
/// - Parameter closure: A non-escaping closure value that will be made
///   escapable for the duration of the execution of the `body` closure. If
///   `body` has a return value, it is used as the return value for the
///   `withoutActuallyEscaping(_:do:)` function.
/// - Parameter body: A closure that will be immediately executed, receiving an
///   escapable copy of `closure` as an argument.
/// - Returns: The return value of the `body` closure, if any.
@_transparent
@_semantics("typechecker.withoutActuallyEscaping(_:do:)")
public func withoutActuallyEscaping<ClosureType, ResultType>(
  _ closure: ClosureType,
  do body: (_ escapingClosure: ClosureType) throws -> ResultType
) rethrows -> ResultType {
  // This implementation is never used, since calls to
  // `Swift.withoutActuallyEscaping(_:do:)` are resolved as a special case by
  // the type checker.
  Builtin.staticReport(_trueAfterDiagnostics(), true._value,
    ("internal consistency error: 'withoutActuallyEscaping(_:do:)' operation failed to resolve"
     as StaticString).utf8Start._rawValue)
  Builtin.unreachable()
}

@_transparent
@_semantics("typechecker._openExistential(_:do:)")
public func _openExistential<ExistentialType, ContainedType, ResultType>(
  _ existential: ExistentialType,
  do body: (_ escapingClosure: ContainedType) throws -> ResultType
) rethrows -> ResultType {
  // This implementation is never used, since calls to
  // `Swift._openExistential(_:do:)` are resolved as a special case by
  // the type checker.
  Builtin.staticReport(_trueAfterDiagnostics(), true._value,
    ("internal consistency error: '_openExistential(_:do:)' operation failed to resolve"
     as StaticString).utf8Start._rawValue)
  Builtin.unreachable()
}

/// A wrapper that can hold any object type with optimal layout properties
/// (especially good for enum payloads). See
/// https://bugs.swift.org/browse/SR-4365, for which this is a workaround.
public struct _UnknownObject<T: AnyObject> {
  public init(_ x: T) { _base = Builtin.castReference(x) }
  public var object : T { return Builtin.castReference(_base) }
  internal let _base: Builtin.UnknownObject
}
