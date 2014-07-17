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

/// Invokes `body` with an `UnsafePointer` to `arg` and returns the
/// result. Useful for calling Objective-C APIs that take "in/out"
/// parameters (and default-constructible "out" parameters) by pointer
public func withUnsafePointer<T, Result>(
  inout arg: T,
  body: (UnsafePointer<T>)->Result
) -> Result
{
  return body(UnsafePointer<T>(Builtin.addressof(&arg)))
}

/// Like `withUnsafePointer`, but passes pointers to `arg0` and `arg1`.
public func withUnsafePointers<A0, A1, Result>(
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

/// Like `withUnsafePointer`, but passes pointers to `arg0`, `arg1`,
/// and `arg2`.
public func withUnsafePointers<A0, A1, A2, Result>(
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

/// A Swift Array or Dictionary of types conforming to
/// `_BridgedToObjectiveCType` can be passed to ObjectiveC as an NSArray or
/// NSDictionary, respectively.  The elements of the resulting NSArray
/// or NSDictionary will be the result of calling `_bridgeToObjectiveC`
/// on each elmeent of the source container.
public protocol _BridgedToObjectiveCType {
  typealias _ObjectiveCType: AnyObject

  // Workaround: right now protocol witness tables don't include associated
  // types, so we can not find '_ObjectiveCType' from them.
  class func _getObjectiveCType() -> Any.Type

  func _bridgeToObjectiveC() -> _ObjectiveCType

  /// Bridge from an Objective-C object of the bridged class type to a
  /// value of the Self type.
  ///
  /// This bridging operation is used for forced downcasting (e.g.,
  /// via as), and may defer complete checking until later. For
  /// example, when bridging from NSArray to Array<T>, we can defer
  /// the checking for the individual elements of the array.
  class func _bridgeFromObjectiveC(source: _ObjectiveCType) -> Self
}

/// Whether a given type conforming to this protocol bridges to
/// ObjectiveC is only knowable at runtime.  Array<T> is an example;
/// it bridges to ObjectiveC iff T does.
public protocol _ConditionallyBridgedToObjectiveCType :
    _BridgedToObjectiveCType {
  class func _isBridgedToObjectiveC() -> Bool

  /// Try to bridge from an Objective-C object of the bridged class
  /// type to a value of the Self type.
  ///
  /// This conditional bridging operation is used for conditional
  /// downcasting (e.g., via as?) and therefore must perform a
  /// complete conversion to the value type; it cannot defer checking
  /// to a later time.
  ///
  /// Returns the bridged value if bridging succeeded, nil if bridging
  /// did not succeed.
  class func _bridgeFromObjectiveCConditional(source: _ObjectiveCType) -> Self?
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
/// - otherwise, `T` conforms to `_BridgedToObjectiveCType`:
///   + if `T` conforms to `_ConditionallyBridgedToObjectiveCType` and
///     `T._isBridgedToObjectiveC()` returns `false`, then the result is empty;
///   + otherwise, returns the result of `x._bridgeToObjectiveC()`;
/// - otherwise, the result is empty.
public func _bridgeToObjectiveC<T>(x: T) -> AnyObject? {
  if _fastPath(_isClassOrObjCExistential(T.self)) {
    return reinterpretCast(x) as AnyObject
  }
  return _bridgeNonVerbatimToObjectiveC(x)
}

public func _bridgeToObjectiveCUnconditional<T>(x: T) -> AnyObject {
  let optResult: AnyObject? = _bridgeToObjectiveC(x)
  _precondition(optResult,
      "value failed to bridge from Swift type to a Objective-C type")
  return optResult!
}

@asmname("swift_bridgeNonVerbatimToObjectiveC")
func _bridgeNonVerbatimToObjectiveC<T>(x: T) -> AnyObject?

/// Convert `x` from its Objective-C representation to its Swift
/// representation.
///
/// - If `T` is a class type:
///   - if the dynamic type of `x` is `T` or a subclass of it, it is bridged
///     verbatim, the function returns `x`;
/// - otherwise, if `T` conforms to `_BridgedToObjectiveCType`:
///   + if the dynamic type of `x` is not `T._getObjectiveCType()`
///     or a subclass of it, trap
///   + otherwise, returns the result of `T._bridgeFromObjectiveC(x)`;
/// - otherwise, trap
public func _bridgeFromObjectiveC<T>(x: AnyObject, _: T.Type) -> T {
  if _fastPath(_isClassOrObjCExistential(T.self)) {
    return x as T
  }
  return _bridgeNonVerbatimFromObjectiveC(x, T.self)
}

/// Attempt to convert `x` from its Objective-C representation to its Swift
/// representation.
///
/// - If `T` is a class type:
///   - if the dynamic type of `x` is `T` or a subclass of it, it is bridged
///     verbatim, the function returns `x`;
/// - otherwise, if `T` conforms to `_BridgedToObjectiveCType`:
///   + if `T` conforms to `_ConditionallyBridgedToObjectiveCType` and
///     `T._isBridgedToObjectiveC()` returns `false`, then the result is empty;
///   + otherwise, if the dynamic type of `x` is not `T._getObjectiveCType()`
///     or a subclass of it, the result is empty;
///   + otherwise, returns the result of `T._bridgeFromObjectiveCConditional(x)`;
/// - otherwise, the result is empty.
public func _bridgeFromObjectiveCConditional<T>(x: AnyObject, _: T.Type) -> T? {
  if _fastPath(_isClassOrObjCExistential(T.self)) {
    return x as? T
  }
  return _bridgeNonVerbatimFromObjectiveCConditional(x, T.self)
}

@asmname("swift_bridgeNonVerbatimFromObjectiveC")
func _bridgeNonVerbatimFromObjectiveC<T>(x: AnyObject, nativeType: T.Type) -> T

@asmname("swift_bridgeNonVerbatimFromObjectiveCConditional")
func _bridgeNonVerbatimFromObjectiveCConditional<T>(x: AnyObject, 
                                                    nativeType: T.Type) -> T?

/// Determines if values of a given type can be converted to an Objective-C
/// representation.
///
/// - If `T` is a class type, returns `true`;
/// - otherwise, `T` conforms to
///   `_ConditionallyBridgedToObjectiveCType`, returns
///   `T._isBridgedToObjectiveC()`;
/// - otherwise, if `T` conforms to `_BridgedToObjectiveCType`, returns `true`.
public func _isBridgedToObjectiveC<T>(_: T.Type) -> Bool {
  if _fastPath(_isClassOrObjCExistential(T.self)) {
    return true
  }
  return _isBridgedNonVerbatimToObjectiveC(T.self)
}

@asmname("swift_isBridgedNonVerbatimToObjectiveC")
func _isBridgedNonVerbatimToObjectiveC<T>(_: T.Type) -> Bool

/// A type that's bridged "verbatim" does not conform to
/// `_BridgedToObjectiveCType`, and can have its bits reinterpreted as an
/// `AnyObject`.  When this function returns true, the storage of an
/// `Array<T>` can be `reinterpretCast` as an array of `AnyObject`.
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

@transparent internal
var _nilNativeObject: AnyObject? {
  return nil
}
@transparent internal
var _nilRawPointer: Builtin.RawPointer {
  return Builtin.inttoptr_Word(0.value)
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
/// Passing pointers to mutable arrays of ObjC class
/// pointers is not directly supported. Unlike UnsafePointer<T>,
/// AutoreleasingUnsafePointer must reference storage that does not own a reference
/// count to the referenced value. UnsafePointer's operations, by contrast,
/// assume that the referenced storage owns values loaded from or stored to it.
///
/// This type does not carry an owner pointer unlike the other C*Pointer types
/// because it only needs to reference the results of inout conversions, which
/// already have writeback-scoped lifetime.
public struct AutoreleasingUnsafePointer<T /* TODO : class */>
  : Equatable, BooleanType, NilLiteralConvertible, _PointerType {
  let value: Builtin.RawPointer

  @transparent
  init(_ value: Builtin.RawPointer) {
    self.value = value
  }

  @transparent
  var _isNull : Bool {
    return UnsafePointer<T>(self)._isNull
  }
  
  @transparent public
  func getLogicValue() -> Bool {
    return !_isNull
  }

  /// Access the underlying raw memory, getting and
  /// setting values.
  public var memory : T {
    /// Retrieve the value the pointer points to.
    @transparent get {
      _debugPrecondition(!_isNull)
      // We can do a strong load normally.
      return UnsafePointer<T>(self).memory
    }
    /// Set the value the pointer points to, copying over the previous value.
    ///
    /// AutoreleasingUnsafePointers are assumed to reference a value with __autoreleasing
    /// ownership semantics, like 'NSFoo**' in ARC. This autoreleases the
    /// argument before trivially storing it to the referenced memory.    
    @transparent nonmutating set {
      _debugPrecondition(!_isNull)
      // Autorelease the object reference.
      Builtin.retain(reinterpretCast(newValue) as AnyObject?)
      Builtin.autorelease(reinterpretCast(newValue) as AnyObject?)
      // Trivially assign it as a COpaquePointer; the pointer references an
      // autoreleasing slot, so retains/releases of the original value are
      // unneeded.
      let p = UnsafePointer<COpaquePointer>(UnsafePointer<T>(self))
      p.memory = reinterpretCast(newValue)
    }
  }

  // Allow read-only subscripting through an AutoreleasingUnsafePointer.t
  public subscript(i: Int) -> T {
    @transparent
    get {
      _debugPrecondition(!_isNull)
      // We can do a strong load normally.
      return (ConstUnsafePointer<T>(self) + i).memory
    }
  }
  
  @transparent public
  static func convertFromNilLiteral() -> AutoreleasingUnsafePointer {
    return AutoreleasingUnsafePointer(_nilRawPointer)
  }
  
  @transparent public
  static func null() -> AutoreleasingUnsafePointer {
    return AutoreleasingUnsafePointer(_nilRawPointer)
  }

  /// Initialize to a null pointer.
  @transparent public
  init() {
    self.value = _nilRawPointer
  }
  
  /// Explicit construction from an UnsafePointer.
  ///
  /// This is inherently unsafe; UnsafePointer assumes the referenced memory
  /// has +1 strong ownership semantics, whereas AutoreleasingUnsafePointer
  /// implies +0 semantics.
  @transparent public
  init<U>(_ ptr: UnsafePointer<U>) {
    self.value = ptr.value
  }

  /// Explicit construction from a ConstUnsafePointer.
  ///
  /// This is inherently unsafe because ConstUnsafePointers do not imply
  /// mutability.
  @transparent
  init<U>(_ ptr: ConstUnsafePointer<U>) {
    self.value = ptr.value
  }
}

@transparent public
func == <T> (lhs: AutoreleasingUnsafePointer<T>, rhs: AutoreleasingUnsafePointer<T>) -> Bool {
  return Bool(Builtin.cmp_eq_RawPointer(lhs.value, rhs.value))
}

