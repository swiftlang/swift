//===--- ArrayCast.swift - Casts and conversions for Array ----------------===//
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
//
//  Because NSArray is effectively an [AnyObject], casting [T] -> [U]
//  is an integral part of the bridging process and these two issues
//  are handled together.
//
//===----------------------------------------------------------------------===//

//===--- Forced casts: [T] as [U] -----------------------------------------===//

/// Implements the semantics of `x as [Derived]` where `x` has type
/// `[Base]` and `Derived` is a verbatim-bridged trivial subtype of
/// `Base`.
public func _arrayDownCast<Base, Derived>(a: Array<Base>) -> [Derived] {
  _sanityCheck(_isBridgedVerbatimToObjectiveC(Base.self))
  _sanityCheck(_isBridgedVerbatimToObjectiveC(Derived.self))

  let native = a._buffer.requestNativeBuffer()
    
  // Fast path: a native buffer that already stores elements of the
  // Derived type.
  if _fastPath(native != nil) {
    if _fastPath(native!.storesOnlyElementsOfType(Derived.self)) {
      return Array(a._buffer.castToBufferOf(Derived.self))
    }
  }

  // FIXME: Make these checks deferred.
  let result: [Derived]? = _arrayDownCastConditional(a)
  _precondition(result != nil, "array cannot be downcast to array of derived")
  return result!
}

/// Convert a to its corresponding bridged array type.
/// Precondition: T is bridged non-verbatim to objective C
/// O(N), because each element must be bridged separately.
public func _arrayBridgeToObjectiveC<BridgesToDerived, Base>(
  source: Array<BridgesToDerived>
) -> Array<Base> {
  _sanityCheck(_isBridgedVerbatimToObjectiveC(Base.self))
  _sanityCheck(!_isBridgedVerbatimToObjectiveC(BridgesToDerived.self))
  var buf = _ContiguousArrayBuffer<Base>(count: source.count, minimumCapacity: 0)
  var p = buf._unsafeElementStorage
  for value in source {
    let bridged: AnyObject? = _bridgeToObjectiveC(value)
    _precondition(bridged != nil, "array element cannot be bridged to Objective-C")
    p++.initialize(unsafeBitCast(bridged!, Base.self))
  }
  return Array(_ArrayBuffer(buf))
}

/// Try to convert the source array of objects to an array of values
/// produced by bridging the objects from Objective-C to \c
/// BridgesToDerived.
///
/// Precondition: Base is a class type.
/// Precondition: BridgesToDerived is bridged non-verbatim to Objective-C.
/// O(n), because each element must be bridged separately.
public func _arrayBridgeFromObjectiveC<Base, BridgesToDerived>(
  source: Array<Base>
) -> Array<BridgesToDerived> {
  let result: Array<BridgesToDerived>?
    = _arrayBridgeFromObjectiveCConditional(source);
  _precondition(result != nil, "array cannot be bridged from Objective-C")
  return result!
}

/// Implements `source as [TargetElement]`.
///
/// Requires: At least one of `SourceElement` and `TargetElement` is a
/// class type or ObjC existential.  May trap for other "valid" inputs
/// when `TargetElement` is not bridged verbatim, if an element can't
/// be converted.
public func _arrayForceCast<SourceElement, TargetElement>(
  source: Array<SourceElement>
) -> Array<TargetElement> {
  if _isClassOrObjCExistential(SourceElement.self) {
    if _isBridgedVerbatimToObjectiveC(TargetElement.self) {
      return _arrayDownCast(source)
    }
    else {
      return _arrayBridgeFromObjectiveC(source)
    }
  }
  else if _isClassOrObjCExistential(TargetElement.self) {
    return _arrayBridgeToObjectiveC(source)
  }
  _fatalError(
    "Force-casting between Arrays of value types not prevented at compile-time"
  )
}

//===--- Conditional casts: [T] as? [U] -----------------------------------===//

/// Implements the semantics of `x as? [Derived]` where `x` has type
/// `[Base]` and `Derived` is a verbatim-bridged trivial subtype of
/// `Base`.
///
/// Returns an Array<Derived> containing the same elements as a in
/// O(1) iff a's buffer elements are dynamically known to have
/// type Derived or a type derived from Derived.
public func _arrayDownCastConditional<Base, Derived>(
  a: Array<Base>
) -> [Derived]? {
  _sanityCheck(_isBridgedVerbatimToObjectiveC(Base.self))
  _sanityCheck(_isBridgedVerbatimToObjectiveC(Derived.self))
  
  if _fastPath(!a.isEmpty) {
    let native = a._buffer.requestNativeBuffer()
    
    if _fastPath(native != nil) {
      if native!.storesOnlyElementsOfType(Derived.self) {
        return Array(a._buffer.castToBufferOf(Derived.self))
      }
      return nil
    }
    
    // slow path: we store an NSArray
    
    // We can skip the check if Derived happens to be AnyObject
    if !(AnyObject.self is Derived.Type) {
      for element in a {
        // FIXME: unsafeBitCast works around <rdar://problem/16953026>
        if !(unsafeBitCast(element, AnyObject.self) is Derived) {
          return nil
        }
      }
    }
    return Array(a._buffer.castToBufferOf(Derived.self))
  }
  return []
}

/// Try to convert the source array of objects to an array of values
/// produced by bridging the objects from Objective-C to \c
/// BridgesToDerived.
///
/// Precondition: Base is a class type.
/// Precondition: BridgesToDerived is bridged non-verbatim to Objective-C.
/// O(n), because each element must be bridged separately.
public func _arrayBridgeFromObjectiveCConditional<Base, BridgesToDerived>(
       source: Array<Base>
     ) -> Array<BridgesToDerived>? {
  _sanityCheck(_isBridgedVerbatimToObjectiveC(Base.self))
  _sanityCheck(!_isBridgedVerbatimToObjectiveC(BridgesToDerived.self))
  var buf = _ContiguousArrayBuffer<BridgesToDerived>(count: source.count, 
                                                    minimumCapacity: 0)
  var p = buf._unsafeElementStorage
  
ElementwiseBridging:
  do {
    for object: Base in source {
      let value = Swift._conditionallyBridgeFromObjectiveC(
        unsafeBitCast(object, AnyObject.self), BridgesToDerived.self)
      if _slowPath(value == nil) {
        break ElementwiseBridging
      }
      p++.initialize(value!)
    }
    return Array(_ArrayBuffer(buf))
  }
  while false
  
  // Don't destroy anything we never created.
  buf.count = p - buf._unsafeElementStorage
  
  // Report failure
  return nil
}

/// Implements `source as? [TargetElement]`: convert each element of
/// `source` to a `TargetElement` and return the resulting array, or
/// return `nil` if any element fails to convert.
///
/// Requires: `SourceElement` is a class or ObjC existential type
public func _arrayConditionalCast<SourceElement, TargetElement>(
  source: [SourceElement]
) -> [TargetElement]? {
  _sanityCheck(
    _isClassOrObjCExistential(SourceElement.self),
    "Conditional cast from array of value types not prevented at compile-time"
  )
  if _isBridgedVerbatimToObjectiveC(TargetElement.self) {
    return _arrayDownCastConditional(source)
  }
  else {
    return _arrayBridgeFromObjectiveCConditional(source)
  }
}
