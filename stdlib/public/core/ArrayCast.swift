//===--- ArrayCast.swift - Casts and conversions for Array ----------------===//
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
//
//  Because NSArray is effectively an [AnyObject], casting [T] -> [U]
//  is an integral part of the bridging process and these two issues
//  are handled together.
//
//===----------------------------------------------------------------------===//

#if _runtime(_ObjC)
// FIXME: These need to be implemented even for non-objc:
// rdar://problem/18881196

internal enum _ValueOrReference {
  case reference, value

  internal init<T>(_: T.Type) {
    self = _isClassOrObjCExistential(T.self) ? .reference : .value
  }
}

internal enum _BridgeStyle {
  case verbatim, explicit

  internal init<T>(_: T.Type) {
   self = _isBridgedVerbatimToObjectiveC(T.self) ? .verbatim : .explicit
  }
}

//===--- Forced casts: [T] as! [U] ----------------------------------------===//

/// Implements `source as! [TargetElement]`.
///
/// - Precondition: At least one of `SourceElement` and `TargetElement` is a
/// class type or ObjC existential.  May trap for other "valid" inputs when
/// `TargetElement` is not bridged verbatim, if an element can't be converted.
public func _arrayForceCast<SourceElement, TargetElement>(
  _ source: Array<SourceElement>
) -> Array<TargetElement> {
  switch (
    _ValueOrReference(SourceElement.self), _BridgeStyle(TargetElement.self)
  ) {
  case (.reference, .verbatim):
    let native = source._buffer.requestNativeBuffer()
    
    if _fastPath(native != nil) {
      if _fastPath(native!.storesOnlyElementsOfType(TargetElement.self)) {
        // A native buffer that is known to store only elements of the
        // TargetElement can be used directly
        return Array(source._buffer.cast(toBufferOf: TargetElement.self))
      }
      // Other native buffers must use deferred element type checking
      return Array(
        source._buffer.downcast(
          toBufferWithDeferredTypeCheckOf: TargetElement.self))
    }
    // All non-native buffers use deferred element typechecking
    return Array(_immutableCocoaArray: source._buffer._asCocoaArray())
    
  case (.reference, .explicit):
    let result: [TargetElement]? = _arrayConditionalBridgeElements(source)
    _precondition(result != nil, "array cannot be bridged from Objective-C")
    return result!
    
  case (.value, .verbatim):
    if source.isEmpty {
      return Array()
    }

    var buf = _ContiguousArrayBuffer<TargetElement>(
      uninitializedCount: source.count, minimumCapacity: 0)
    
    let _: Void = buf.withUnsafeMutableBufferPointer {
      var p = $0.baseAddress!
      for value in source {
        let bridged: AnyObject? = _bridgeToObjectiveC(value)
        _precondition(
          bridged != nil, "array element cannot be bridged to Objective-C")
        // FIXME: should be an unsafeDowncast.
        p.initialize(with: unsafeBitCast(bridged!, to: TargetElement.self))
        p += 1
      }
    }
    return Array(_ArrayBuffer(buf, shiftedToStartIndex: 0))
    
  case (.value, .explicit):
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
  _ a: Array<SourceElement>
) -> [TargetElement]? {
  _sanityCheck(_isBridgedVerbatimToObjectiveC(SourceElement.self))
  _sanityCheck(_isBridgedVerbatimToObjectiveC(TargetElement.self))
  
  if _fastPath(!a.isEmpty) {
    let native = a._buffer.requestNativeBuffer()
    
    if _fastPath(native != nil) {
      if native!.storesOnlyElementsOfType(TargetElement.self) {
        return Array(a._buffer.cast(toBufferOf: TargetElement.self))
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
    return Array(a._buffer.cast(toBufferOf: TargetElement.self))
  }
  return []
}

/// Try to convert the source array of objects to an array of values
/// produced by bridging the objects from Objective-C to `TargetElement`.
///
/// - Precondition: SourceElement is a class type.
/// - Precondition: TargetElement is bridged non-verbatim to Objective-C.
///   O(n), because each element must be bridged separately.
internal func _arrayConditionalBridgeElements<SourceElement, TargetElement>(
       _ source: Array<SourceElement>
     ) -> Array<TargetElement>? {
  _sanityCheck(_isBridgedVerbatimToObjectiveC(SourceElement.self))
  _sanityCheck(!_isBridgedVerbatimToObjectiveC(TargetElement.self))
  
  let buf = _ContiguousArrayBuffer<TargetElement>(
    uninitializedCount: source.count, minimumCapacity: 0)
  
  var p = buf.firstElementAddress
  
ElementwiseBridging:
  repeat {
    for object: SourceElement in source {
      let value = Swift._conditionallyBridgeFromObjectiveC(
        unsafeBitCast(object, to: AnyObject.self), TargetElement.self)
      if _slowPath(value == nil) {
        break ElementwiseBridging
      }
      p.initialize(with: value!)
      p += 1
    }
    return Array(_ArrayBuffer(buf, shiftedToStartIndex: 0))
  }
  while false
  
  // Don't destroy anything we never created.
  buf.count = p - buf.firstElementAddress
  
  // Report failure
  return nil
}

/// Implements `source as? [TargetElement]`: convert each element of
/// `source` to a `TargetElement` and return the resulting array, or
/// return `nil` if any element fails to convert.
///
/// - Precondition: `SourceElement` is a class or ObjC existential type.
/// O(n), because each element must be checked.
public func _arrayConditionalCast<SourceElement, TargetElement>(
  _ source: [SourceElement]
) -> [TargetElement]? {
  switch (_ValueOrReference(SourceElement.self), _BridgeStyle(TargetElement.self)) {
  case (.value, _):
    _sanityCheckFailure(
      "Conditional cast from array of value types not prevented at compile-time")
  case (.reference, .verbatim):
    return _arrayConditionalDownCastElements(source)
  case (.reference, .explicit):
    return _arrayConditionalBridgeElements(source)
  }
}
#endif
