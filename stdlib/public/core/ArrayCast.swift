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

#if _runtime(_ObjC)
// FIXME: These need to be implemented even for non-objc:
// rdar://problem/18881196

enum _ValueOrReference {
case Reference, Value
  init<T>(_: T.Type) {
    self = _isClassOrObjCExistential(T.self) ? .Reference : .Value
  }
}

enum _BridgeStyle {
case Verbatim, Explicit
  init<T>(_: T.Type) {
   self = _isBridgedVerbatimToObjectiveC(T.self) ? .Verbatim : .Explicit
  }
}


//===--- Forced casts: [T] as [U] -----------------------------------------===//

/// Implements `source as [TargetElement]`.
///
/// Requires: At least one of `SourceElement` and `TargetElement` is a
/// class type or ObjC existential.  May trap for other "valid" inputs
/// when `TargetElement` is not bridged verbatim, if an element can't
/// be converted.
public func _arrayForceCast<SourceElement, TargetElement>(
  source: Array<SourceElement>
) -> Array<TargetElement> {
  switch (
    _ValueOrReference(SourceElement.self), _BridgeStyle(TargetElement.self)
  ) {
  case (.Reference, .Verbatim):
    let native = source._buffer.requestNativeBuffer()
    
    if _fastPath(native != nil) {
      if _fastPath(native!.storesOnlyElementsOfType(TargetElement.self)) {
        // A native buffer that is known to store only elements of the
        // TargetElement can be used directly
        return Array(source._buffer.castToBufferOf(TargetElement.self))
      }
      // Other native buffers must use deferred element type checking
      return Array(
        source._buffer.downcastToBufferWithDeferredTypeCheckOf(
          TargetElement.self))
    }
    // All non-native buffers use deferred element typechecking
    return Array(_fromCocoaArray: source._buffer._asCocoaArray())
    
  case (.Reference, .Explicit):
    let result: [TargetElement]? = _arrayConditionalBridgeElements(source)
    _precondition(result != nil, "array cannot be bridged from Objective-C")
    return result!
    
  case (.Value, .Verbatim):
    var buf = _ContiguousArrayBuffer<TargetElement>(
      count: source.count(), minimumCapacity: 0)
    
    let _: Void = buf.withUnsafeMutableBufferPointer {
      var p = $0.baseAddress
      for value in source {
        let bridged: AnyObject? = _bridgeToObjectiveC(value)
        _precondition(
          bridged != nil, "array element cannot be bridged to Objective-C")
        // FIXME: should be an unsafeDowncast, but for <rdar://problem/18638230>
        p++.initialize(unsafeBitCast(bridged!, TargetElement.self))
      }
    }
    return Array(_ArrayBuffer(buf))
    
  case (.Value, .Explicit):
    _sanityCheckFailure(
      "Force-casting between Arrays of value types not prevented at compile-time"
    )
  }
}

//===--- Conditional casts: [T] as? [U] -----------------------------------===//

/// Implements the semantics of `x as? [TargetElement]` where `x` has type
/// `[SourceElement]` and `TargetElement` is a verbatim-bridged trivial subtype of
/// `SourceElement`.
///
/// Returns an Array<TargetElement> containing the same elements as a
///
/// O(1) if a's buffer elements are dynamically known to have type
/// TargetElement or a type derived from TargetElement.  O(N)
/// otherwise.
internal func _arrayConditionalDownCastElements<SourceElement, TargetElement>(
  a: Array<SourceElement>
) -> [TargetElement]? {
  _sanityCheck(_isBridgedVerbatimToObjectiveC(SourceElement.self))
  _sanityCheck(_isBridgedVerbatimToObjectiveC(TargetElement.self))
  
  if _fastPath(!a.isEmpty) {
    let native = a._buffer.requestNativeBuffer()
    
    if _fastPath(native != nil) {
      if native!.storesOnlyElementsOfType(TargetElement.self) {
        return Array(a._buffer.castToBufferOf(TargetElement.self))
      }
      return nil
    }
    
    // slow path: we store an NSArray
    
    // We can skip the check if TargetElement happens to be AnyObject
    if !(AnyObject.self is TargetElement.Type) {
      for element in a {
        if !(element is TargetElement) {
          return nil
        }
      }
    }
    return Array(a._buffer.castToBufferOf(TargetElement.self))
  }
  return []
}

/// Try to convert the source array of objects to an array of values
/// produced by bridging the objects from Objective-C to `TargetElement`.
///
/// - precondition: SourceElement is a class type.
/// - precondition: TargetElement is bridged non-verbatim to Objective-C.
/// O(n), because each element must be bridged separately.
internal func _arrayConditionalBridgeElements<SourceElement, TargetElement>(
       source: Array<SourceElement>
     ) -> Array<TargetElement>? {
  _sanityCheck(_isBridgedVerbatimToObjectiveC(SourceElement.self))
  _sanityCheck(!_isBridgedVerbatimToObjectiveC(TargetElement.self))
  
  var buf = _ContiguousArrayBuffer<TargetElement>(
    count: source.count(), minimumCapacity: 0)
  
  var p = buf.baseAddress
  
ElementwiseBridging:
  repeat {
    for object: SourceElement in source {
      let value = Swift._conditionallyBridgeFromObjectiveC(
        unsafeBitCast(object, AnyObject.self), TargetElement.self)
      if _slowPath(value == nil) {
        break ElementwiseBridging
      }
      p++.initialize(value!)
    }
    return Array(_ArrayBuffer(buf))
  }
  while false
  
  // Don't destroy anything we never created.
  buf.count = p - buf.baseAddress
  
  // Report failure
  return nil
}

/// Implements `source as? [TargetElement]`: convert each element of
/// `source` to a `TargetElement` and return the resulting array, or
/// return `nil` if any element fails to convert.
///
/// Requires: `SourceElement` is a class or ObjC existential type
/// O(n), because each element must be checked.
public func _arrayConditionalCast<SourceElement, TargetElement>(
  source: [SourceElement]
) -> [TargetElement]? {
  switch (_ValueOrReference(SourceElement.self), _BridgeStyle(TargetElement.self)) {
  case (.Value, _): 
    _sanityCheckFailure(
      "Conditional cast from array of value types not prevented at compile-time")
  case (.Reference, .Verbatim):
    return _arrayConditionalDownCastElements(source)
  case (.Reference, .Explicit):
    return _arrayConditionalBridgeElements(source)
  }
}
#endif
