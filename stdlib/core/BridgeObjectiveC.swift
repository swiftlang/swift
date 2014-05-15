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

/// \brief Invokes body with an UnsafePointer to arg and returns the
/// result. Useful for calling Objective-C APIs that take "in/out"
/// parameters (and default-constructible "out" parameters) by pointer
func withUnsafePointer<T, Result>(
  inout arg: T,
  body: (UnsafePointer<T>)->Result
) -> Result
{
  return body(UnsafePointer<T>(Builtin.addressof(&arg)))
}

/// \brief Like withUnsafePointer, but passes pointers to arg0 and
/// arg1
func withUnsafePointers<A0, A1, Result>(
  inout arg0: A0,
  inout arg1: A1,
  body: (UnsafePointer<A0>, UnsafePointer<A1>)->Result
) -> Result {
  return withUnsafePointer(&arg0) {
    arg0 in withUnsafePointer(&arg1) {
      arg1 in body(arg0, arg1)
    }
  }
}

/// \brief Like withUnsafePointer, but passes pointers to arg0, arg1,
/// and arg2
func withUnsafePointers<A0, A1, A2, Result>(
  inout arg0: A0,
  inout arg1: A1,
  inout arg2: A2,
  body: (UnsafePointer<A0>, UnsafePointer<A1>, UnsafePointer<A2>)->Result
) -> Result {
  return withUnsafePointer(&arg0) {
    arg0 in withUnsafePointer(&arg1) {
      arg1 in withUnsafePointer(&arg2) {
        arg2 in body(arg0, arg1, arg2)
      }
    }
  }
}

/// \brief Invokes body with an UnsafePointer to a nil T, sets arg to
/// the value of that T (or .None if the T is still nil), and returns
/// the result of the invocation
///
/// Useful for calling Objective-C APIs that take class instances by
/// pointer as @autorelease "out" parameters.
func withUnsafePointerToObject<T: AnyObject, Result>(
  inout arg: T?,
  body: (UnsafePointer<ImplicitlyUnwrappedOptional<T>>)->Result
) -> Result {
  var buffer: Builtin.RawPointer = Builtin.inttoptr_Word(0.value)
  var address = UnsafePointer<ImplicitlyUnwrappedOptional<T>>(Builtin.addressof(&buffer))
  var result = body(address)
  arg = address.get()
  return result
}

/// A Swift Array or Dictionary of types conforming to
/// _BridgedToObjectiveC can be passed to ObjectiveC as an NSArray or
/// NSDictionary, respectively.  The elements of the resulting NSArray
/// or NSDictionary will be the result of calling bridgeToObjectiveC
/// on each elmeent of the source container.
protocol _BridgedToObjectiveC {
  typealias ObjectiveCType: AnyObject

  // Workaround: right now protocol witness tables don't include associated
  // types, so we can not find 'ObjectiveCType' from them.
  class func getObjectiveCType() -> Any.Type

  func bridgeToObjectiveC() -> ObjectiveCType
  class func bridgeFromObjectiveC(x: ObjectiveCType) -> Self?
}

/// Whether a given type conforming to this protocol bridges to
/// ObjectiveC is only knowable at runtime.  Array<T> is an example;
/// it is bridges to ObjectiveC iff T does, too.
protocol _ConditionallyBridgedToObjectiveC : _BridgedToObjectiveC {
  class func isBridgedToObjectiveC() -> Bool
}

/// Attempt to convert `x` to its Objective-C representation.
///
/// - If `T` conforms to `_BridgedToObjectiveC`:
///   + if `T` conforms to `_ConditionallyBridgedToObjectiveC` and
///     `T.isBridgedToObjectiveC()` returns `false`, then the result is empty;
///   + otherwise, returns the result of `x.bridgeToObjectiveC()`;
/// - otherwise, if `T` is a class type (bridged verbatim), returns `x`;
/// - otherwise, the result is empty.
@asmname("swift_bridgeToObjectiveC")
func bridgeToObjectiveC<T>(x: T) -> AnyObject?

/// Attempt to convert `x` from its Objective-C representation to its Swift
/// representation.
///
/// - If `T` conforms to `_BridgedToObjectiveC`:
///   + if `T` conforms to `_ConditionallyBridgedToObjectiveC` and
///     `T.isBridgedToObjectiveC()` returns `false`, then the result is empty;
///   + otherwise, if the dynamic type of `x` is not `T.getObjectiveCType()`
///     or a subclass of it, the result is empty;
///   + otherwise, returns the result of `T.bridgeFromObjectiveC(x)`;
/// - otherwise, if `T` is a class type (bridged verbatim), and the dynamic
///   type of `x` is `T` or a subclass of it, returns `x`;
/// - otherwise, the result is empty.
@asmname("swift_bridgeFromObjectiveC")
func bridgeFromObjectiveC<T>(x: AnyObject, nativeType: T.Type) -> T?

/// Determines if values of a given type can be converted to an Objective-C
/// representation.
///
/// - If `T` conforms to `_ConditionallyBridgedToObjectiveC`, returns
///   `T.isBridgedToObjectiveC()`;
/// - otherwise, if `T` conforms to `_BridgedToObjectiveC` or `T` is a class
///   type, returns `true`.
@asmname("swift_isBridgedToObjectiveC")
func isBridgedToObjectiveC<T>(_: T.Type) -> Bool

/// A type that's bridged "verbatim" does not conform to
/// _BridgedToObjectiveC, and can have its bits reinterpreted as an
/// AnyObject.  This function does not necessarily detect all such
/// types (there may be false negatives) but when it returns true, the
/// storage Array<T> can be reinterpretCast as an array of AnyObject
@asmname("swift_isBridgedVerbatimToObjectiveC")
func isBridgedVerbatimToObjectiveC<T>(_: T.Type) -> Bool

/// Retrieve the Objective-C type to which the given type is bridged.
@asmname("swift_getBridgedObjectiveCType")
func getBridgedObjectiveCType<T>(_: T.Type) -> Any.Type?

// -- Pointer argument bridging

@transparent
var _nilNativeObject: AnyObject? {
  return nil
}
@transparent
var _nilRawPointer: Builtin.RawPointer {
  return Builtin.inttoptr_Word(0.value)
}

/// A mutable C pointer argument.
///
/// This type has no operations of its own, but has implicit conversions
/// to allow passing any of the following to a C or ObjC API:
///
/// - 'nil', which gets passed as a null pointer,
/// - an inout argument of the referenced type, which gets passed as a pointer
///   to the inout-ed lvalue (or its writeback temporary, if it is a computed
///   lvalue),
/// - an inout argument of the Array<T> type, which gets passed as a pointer
///   to the beginning of the array,
/// - an UnsafePointer<T>, which is passed as-is.
///
/// The value consists of an owner-value pair. During bridging, a strong
/// reference to the owner is held for the duration of the call, and the pointer
/// value is passed down to the C or Objective-C entry point. This allows
/// types that own heap storage, such as Array, to convert themselves to
/// a pointer and still guarantee that their storage will be held for the
/// duration of the call.
///
/// Pointers to ObjC object pointer type ``NSFoo**`` are not mapped to this
/// type; they instead get mapped to ObjCMutablePointer<T>. ``void*`` pointers
/// are mapped to CMutableVoidPointer.
struct CMutablePointer<T> : Equatable {
  let owner: AnyObject?
  let value: Builtin.RawPointer

  /// Conversion from an inout scalar.
  @transparent
  static func __inout_conversion(inout scalar: T) -> CMutablePointer {
    // No owner pointer for an inout scalar; the lifetime guarantee of writeback
    // is sufficient.
    return CMutablePointer(owner: _nilNativeObject,
                           value: Builtin.addressof(&scalar))
  }

  /// Conversion from an inout array.
  @transparent
  static func __inout_conversion(inout a: Array<T>) -> CMutablePointer {
    assert(a.elementStorage != nil || a.count == 0)

    // TODO: Putting a canary at the end of the array in checked builds might
    // be a good idea

    // The callee that receives the pointer may mutate through it, so
    // force uniqueness by calling reserve(0).
    a.reserve(0)
    return CMutablePointer(owner: a.owner, value: a.elementStorage.value)
  }

  /// True if this is a scoped pointer, meaning it has a owner reference
  /// that guarantees the lifetime of the referenced memory.
  @transparent
  var scoped: Bool {
    return owner.getLogicValue()
  }

  /// Make the pointer available as an UnsafePointer within a closure.
  @transparent
  func withUnsafePointer<U>(f: UnsafePointer<T> -> U) -> U {
    let result = f(UnsafePointer<T>(value))
    // Ensure the owner pointer stays alive for the duration of the closure.
    _fixLifetime(owner)
    return result
  }  
}

@transparent
func == <T> (lhs: CMutablePointer<T>, rhs: CMutablePointer<T>) -> Bool {
  return Bool(Builtin.cmp_eq_RawPointer(lhs.value, rhs.value))
}

// Also make CMutablePointer comparable to CConstPointer.
@transparent
func == <T> (lhs: CMutablePointer<T>, rhs: CConstPointer<T>) -> Bool {
  return Bool(Builtin.cmp_eq_RawPointer(lhs.value, rhs.value))
}

@transparent
func == <T> (lhs: CConstPointer<T>, rhs: CMutablePointer<T>) -> Bool {
  return Bool(Builtin.cmp_eq_RawPointer(lhs.value, rhs.value))
}

/// A mutable C void pointer argument.
///
/// This type has no operations of its own, but has implicit conversions
/// to allow passing any of the following to a C or ObjC API:
///
/// - 'nil', which gets passed as a null pointer,
/// - an inout argument of any type, which gets passed as a pointer
///   to the inout-ed lvalue (or its writeback temporary, if it is a computed
///   lvalue),
/// - an inout argument of Array<T> type for any T, which gets passed as a
///   pointer to the beginning of the array,
/// - an UnsafePointer<T> for any T or COpaquePointer, which is passed as-is.
///
/// The value consists of an owner-value pair. During bridging, a strong
/// reference to the owner is held for the duration of the call, and the pointer
/// value is passed down to the C or Objective-C entry point. This allows
/// types that own heap storage, such as Array, to convert themselves to
/// a pointer and still guarantee that their storage will be held for the
/// duration of the call.
struct CMutableVoidPointer : Equatable {
  let owner: AnyObject?
  let value: Builtin.RawPointer

  /// Conversion from an inout scalar.
  @transparent
  static func __inout_conversion<T>(inout scalar: T) -> CMutableVoidPointer {
    // No owner pointer for an inout scalar; the lifetime guarantee of writeback
    // is sufficient.
    return CMutableVoidPointer(owner: _nilNativeObject,
                               value: Builtin.addressof(&scalar))
  }

  /// Conversion from an inout array.
  @transparent
  static func __inout_conversion<T>(inout a: Array<T>)
  -> CMutableVoidPointer {
    assert(a.elementStorage != nil || a.count == 0)

    // TODO: Putting a canary at the end of the array in checked builds might
    // be a good idea.

    // The callee that receives the pointer may mutate through it, so
    // force uniqueness by calling reserve(0).
    a.reserve(0)
    return CMutableVoidPointer(owner: a.owner,
                               value: a.elementStorage.value)
  }

  /// True if this is a scoped pointer, meaning it has a owner reference
  /// that guarantees the lifetime of the referenced memory.
  @transparent
  var scoped: Bool {
    return owner.getLogicValue()
  }

  /// Make the pointer available as an UnsafePointer within a closure.
  @transparent
  func withUnsafePointer<T, U>(f: UnsafePointer<T> -> U) -> U {
    let result = f(UnsafePointer(value))
    // Ensure the owner pointer stays alive for the duration of the closure.
    _fixLifetime(owner)
    return result
  }
}

@transparent
func == (lhs: CMutableVoidPointer, rhs: CMutableVoidPointer) -> Bool {
  return Bool(Builtin.cmp_eq_RawPointer(lhs.value, rhs.value))
}

/// A mutable pointer-to-ObjC-pointer argument.
///
/// This type has implicit conversions to allow passing any of the following
/// to a C or ObjC API:
///
/// - 'nil', which gets passed as a null pointer,
/// - an inout argument of the referenced type, which gets passed as a pointer
///   to a writeback temporary with autoreleasing ownership semantics,
/// - an UnsafePointer<T>, which is passed as-is.
///
/// Unlike CMutablePointer, passing pointers to mutable arrays of ObjC class
/// pointers is not directly supported. Unlike UnsafePointer<T>,
/// ObjCMutablePointer must reference storage that does not own a reference
/// count to the referenced value. UnsafePointer's operations, by contrast,
/// assume that the referenced storage owns values loaded from or stored to it.
///
/// This type does not carry an owner pointer unlike the other C*Pointer types
/// because it only needs to reference the results of inout conversions, which
/// already have writeback-scoped lifetime.
struct ObjCMutablePointer<T /* TODO : class */> : Equatable {
  let value: Builtin.RawPointer

  @transparent
  init(_ value: Builtin.RawPointer) {
    self.value = value
  }

  /// Create the writeback temporary for inout conversion.
  @transparent
  static func __writeback_conversion_get(x: T) -> Builtin.RawPointer {
    // Reinterpreting the object reference as a RawPointer gives us a
    // nonowning reference to the original value.
    return reinterpretCast(x)
  }

  /// Commit the writeback temporary back to the original inout parameter.
  @transparent
  static func __writeback_conversion_set(x: Builtin.RawPointer) -> T {
    // Reinterpreting the RawPointer back to an object reference gives us a +1
    // reference to the object again.
    return reinterpretCast(x)
  }

  /// Conversion from an inout scalar.
  ///
  /// Variables always have strong ownership semantics in Swift, but "NSFoo**"
  /// pointers in ARC have autoreleasing ownership by default, so doing this
  /// properly requires a writeback temporary.
  @transparent
  static func __writeback_conversion(
    inout autoreleasingTemp: Builtin.RawPointer
  ) -> ObjCMutablePointer {
    return ObjCMutablePointer(Builtin.addressof(&autoreleasingTemp))
  }

  @transparent
  func isNull() -> Bool {
    return UnsafePointer<T>(self).isNull()
  }
  
  /// Retrieve the value the pointer points to.
  @transparent
  func get() -> T {
    assert(!isNull())
    // We can do a strong load normally.
    return UnsafePointer<T>(self).get()
  }

  /// Set the value the pointer points to, copying over the previous value.
  ///
  /// ObjCMutablePointers are assumed to reference a value with __autoreleasing
  /// ownership semantics, like 'NSFoo**' in ARC. This autoreleases the
  /// argument before trivially storing it to the referenced memory.
  @transparent
  func set(newValue: T) {
    assert(!isNull())
    // Autorelease the object reference.
    Builtin.retain(reinterpretCast(newValue) as AnyObject?)
    Builtin.autorelease(reinterpretCast(newValue) as AnyObject?)
    // Trivially assign it as a COpaquePointer; the pointer references an
    // autoreleasing slot, so retains/releases of the original value are
    // unneeded.
    UnsafePointer<COpaquePointer>(UnsafePointer<T>(self))
      .set(reinterpretCast(newValue))
  }
}

@transparent
func == <T> (lhs: ObjCMutablePointer<T>, rhs: ObjCMutablePointer<T>) -> Bool {
  return Bool(Builtin.cmp_eq_RawPointer(lhs.value, rhs.value))
}

/// A const C pointer argument.
///
/// This type has no operations of its own, but has implicit conversions
/// to allow passing any of the following to a C or ObjC API:
///
/// - 'nil', which gets passed as a null pointer,
/// - an inout argument of the referenced type, which gets passed as a pointer
///   to the inout-ed lvalue (or its writeback temporary, if it is a computed
///   lvalue),
/// - a value argument of the Array<T> type, which gets passed as a pointer
///   to the beginning of the array.
///
/// The value consists of an owner-value pair. During bridging, a strong
/// reference to the owner is held for the duration of the call, and the pointer
/// value is passed down to the C or Objective-C entry point. This allows
/// types that own heap storage, such as Array, to convert themselves to
/// a pointer and still guarantee that their storage will be held for the
/// duration of the call.
struct CConstPointer<T> : Equatable {
  // TODO: Owner should become AnyObject? when the new Array implementation
  // comes online.
  let owner: AnyObject?
  let value: Builtin.RawPointer

  @transparent
  init(_ owner: AnyObject?, _ value: Builtin.RawPointer) {
    self.owner = owner
    self.value = value
  }

  // Conversion from an inout scalar.
  @transparent
  static func __inout_conversion(inout scalar: T) -> CConstPointer {
    // No owner pointer for an inout scalar; the lifetime guarantee of writeback
    // is sufficient.
    return CConstPointer(_nilNativeObject, Builtin.addressof(&scalar))
  }

  /// True if this is a scoped pointer, meaning it has a owner reference
  /// that guarantees the lifetime of the referenced memory.
  @transparent
  var scoped: Bool {
    return owner.getLogicValue()
  }

  /// Make the pointer available as an UnsafePointer within a closure.
  @transparent
  func withUnsafePointer<U>(f: UnsafePointer<T> -> U) -> U {
    let result = f(UnsafePointer<T>(value))
    // Ensure the owner pointer stays alive for the duration of the closure.
    _fixLifetime(owner)
    return result
  }
}

@transparent
func == <T> (lhs: CConstPointer<T>, rhs: CConstPointer<T>) -> Bool {
  return Bool(Builtin.cmp_eq_RawPointer(lhs.value, rhs.value))
}

struct CConstVoidPointer : Equatable {
  // TODO: Owner should become AnyObject? when the new Array implementation
  // comes online.
  let owner: AnyObject?
  let value: Builtin.RawPointer

  @transparent
  init(_ owner: AnyObject?, _ value: Builtin.RawPointer) {
    self.owner = owner
    self.value = value
  }

  // Conversion from an inout scalar.
  @transparent
  static func __inout_conversion<T>(inout scalar: T) -> CConstVoidPointer {
    // No owner pointer for an inout scalar; the lifetime guarantee of writeback
    // is sufficient.
    return CConstVoidPointer(_nilNativeObject, Builtin.addressof(&scalar))
  }

  /// True if this is a scoped pointer, meaning it has a owner reference
  /// that guarantees the lifetime of the referenced memory.
  @transparent
  var scoped: Bool {
    return owner.getLogicValue()
  }

  /// Make the pointer available as an UnsafePointer within a closure.
  @transparent
  func withUnsafePointer<T, U>(f: UnsafePointer<T> -> U) -> U {
    let result = f(UnsafePointer(value))
    // Ensure the owner pointer stays alive for the duration of the closure.
    _fixLifetime(owner)
    return result
  }
}

@transparent
func ==(lhs: CConstVoidPointer, rhs: CConstVoidPointer) -> Bool {
  return Bool(Builtin.cmp_eq_RawPointer(lhs.value, rhs.value))
}

//
// Conversions from nil to bridging pointer types.
//

extension _Nil {
  @transparent @conversion
  func __conversion<T>() -> CMutablePointer<T> {
    return CMutablePointer(owner: _nilNativeObject, value: _nilRawPointer)
  }
  @transparent @conversion
  func __conversion() -> CMutableVoidPointer {
    return CMutableVoidPointer(owner: _nilNativeObject, value: _nilRawPointer)
  }
  @transparent @conversion
  func __conversion<T>() -> CConstPointer<T> {
    return CConstPointer(_nilNativeObject, _nilRawPointer)
  }
  @transparent @conversion
  func __conversion() -> CConstVoidPointer {
    return CConstVoidPointer(_nilNativeObject, _nilRawPointer)
  }
  @transparent @conversion
  func __conversion<T>() -> ObjCMutablePointer<T> {
    return ObjCMutablePointer(_nilRawPointer)
  }
}

//
// Conversions from COpaquePointer to C*VoidPointer
//

extension COpaquePointer {
  @transparent @conversion
  func __conversion() -> CMutableVoidPointer {
    return CMutableVoidPointer(owner: _nilNativeObject, value: value)
  }

  @transparent @conversion
  func __conversion() -> CConstVoidPointer {
    return CConstVoidPointer(_nilNativeObject, value)
  }
}

//
// Native-to-bridged conversion functions from C*Pointer to UnsafePointer
//

@transparent
func _convertCConstPointerToUnsafePointer<T>(p: CConstPointer<T>)
-> UnsafePointer<T> {
  return UnsafePointer(p.value)
}

@transparent
func _convertCConstVoidPointerToCOpaquePointer(p: CConstVoidPointer)
-> COpaquePointer {
  return COpaquePointer(p.value)
}

@transparent
func _convertCMutablePointerToUnsafePointer<T>(p: CMutablePointer<T>)
-> UnsafePointer<T> {
  return UnsafePointer(p.value)
}

@transparent
func _convertCMutableVoidPointerToCOpaquePointer(p: CMutableVoidPointer)
-> COpaquePointer {
  return COpaquePointer(p.value)
}

@transparent
func _convertObjCMutablePointerToUnsafePointer<T>(p: ObjCMutablePointer<T>)
-> UnsafePointer<T> {
  return UnsafePointer(p.value)
}

//
// Bridged-to-native conversion functions from UnsafePointer to C*Pointer
//

// UnsafePointers will be bridged back into C*Pointer types in argument
// position, where they should reference storage lifetime-guaranteed for
// the duration of the function being called. We can thus safely bridge
// the arguments to C*Pointer types with nil owner references.

@transparent
func _convertUnsafePointerToCConstPointer<T>(p: UnsafePointer<T>)
-> CConstPointer<T> {
  return CConstPointer(_nilNativeObject, p.value)
}

@transparent
func _convertCOpaquePointerToCConstVoidPointer(p: COpaquePointer)
-> CConstVoidPointer {
  return CConstVoidPointer(_nilNativeObject, p.value)
}

@transparent
func _convertUnsafePointerToCMutablePointer<T>(p: UnsafePointer<T>)
-> CMutablePointer<T> {
  return CMutablePointer(owner: _nilNativeObject, value: p.value)
}

@transparent
func _convertCOpaquePointerToCMutableVoidPointer(p: COpaquePointer)
-> CMutableVoidPointer {
  return CMutableVoidPointer(owner: _nilNativeObject, value: p.value)
}

@transparent
func _convertUnsafePointerToObjCMutablePointer<T>(p: UnsafePointer<T>)
-> ObjCMutablePointer<T> {
  return ObjCMutablePointer(p.value)
}
