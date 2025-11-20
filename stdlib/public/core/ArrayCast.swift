//===--- ArrayCast.swift - Casts and conversions for Array ----------------===//
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
//
//  Because NSArray is effectively an [AnyObject], casting [T] -> [U]
//  is an integral part of the bridging process and these two issues
//  are handled together.
//
//===----------------------------------------------------------------------===//

/// Called by the casting machinery.
@_silgen_name("_swift_arrayDownCastIndirect")
internal func _arrayDownCastIndirect<SourceValue, TargetValue>(
  _ source: UnsafePointer<Array<SourceValue>>,
  _ target: UnsafeMutablePointer<Array<TargetValue>>) {
  unsafe target.initialize(to: _arrayForceCast(source.pointee))
}

/// Implements `source as! [TargetElement]`.
///
/// - Note: When SourceElement and TargetElement are both bridged verbatim, type
///   checking is deferred until elements are actually accessed.
@inlinable //for performance reasons
public func _arrayForceCast<SourceElement, TargetElement>(
  _ source: Array<SourceElement>
) -> Array<TargetElement> {
#if _runtime(_ObjC)
  if _isClassOrObjCExistential(SourceElement.self)
  && _isClassOrObjCExistential(TargetElement.self) {
    let src = source._buffer
    if let native = src.requestNativeBuffer() {
      if native.storesOnlyElementsOfType(TargetElement.self) {
        // A native buffer that is known to store only elements of the
        // TargetElement can be used directly
        return Array(_buffer: src.cast(toBufferOf: TargetElement.self))
      }
      // Other native buffers must use deferred element type checking
      return Array(_buffer:
        src.downcast(toBufferWithDeferredTypeCheckOf: TargetElement.self))
    }
    return Array(_immutableCocoaArray: source._buffer._asCocoaArray())
  }
#endif
  return source.map { $0 as! TargetElement }
}

/// Called by the casting machinery.
@_silgen_name("_swift_arrayDownCastConditionalIndirect")
internal func _arrayDownCastConditionalIndirect<SourceValue, TargetValue>(
  _ source: UnsafePointer<Array<SourceValue>>,
  _ target: UnsafeMutablePointer<Array<TargetValue>>
) -> Bool {
  if let result: Array<TargetValue> = unsafe _arrayConditionalCast(source.pointee) {
    unsafe target.initialize(to: result)
    return true
  }
  return false
}

/// Implements `source as? [TargetElement]`: convert each element of
/// `source` to a `TargetElement` and return the resulting array, or
/// return `nil` if any element fails to convert.
///
/// - Complexity: O(n), because each element must be checked.
@inlinable //for performance reasons
@_semantics("array.conditional_cast")
public func _arrayConditionalCast<SourceElement, TargetElement>(
  _ source: [SourceElement]
) -> [TargetElement]? {
  var successfulCasts = ContiguousArray<TargetElement>()
  successfulCasts.reserveCapacity(source.count)
  for element in source {
    if let casted = element as? TargetElement {
      successfulCasts.append(casted)
    } else {
      return nil
    }
  }
  return Array(successfulCasts)
}
