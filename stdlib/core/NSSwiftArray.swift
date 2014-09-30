//===--- NSSwiftArray.swift - Links NSArray and _ContiguousArrayStorage ---===//
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
//  _ContiguousArrayStorageBase supplies the implementation of the
//  _SwiftNSArrayRequiredOverridesType API (and thus, NSArray the API) for our
//  _ContiguousArrayStorage<T>.  We can't put this implementation
//  directly on _ContiguousArrayStorage because generic classes can't
//  override Objective-C selectors.
//
//===----------------------------------------------------------------------===//

import SwiftShims

/// Return whether the given `index` is valid, i.e. `0 ≤ index ≤ count`
@transparent
internal func _isValidArrayIndex(index: Int, count: Int) -> Bool {
  return (index >= 0) & (index <= count)
}

/// Return whether the given `index` is valid for subscripting, i.e.
/// `0 ≤ index < count`
@transparent
internal func _isValidArraySubscript(index: Int, count: Int) -> Bool {
  return (index >= 0) & (index < count)
}

/// `Swift.Array` bridges to this class, which is a subclass of `NSArray`.
@objc internal final class _NSSwiftArrayImpl :
  _NSSwiftArray, _SwiftNSArrayRequiredOverridesType {

  // This stored property should be stored at offset zero.  We perform atomic
  // operations on it.
  //
  // Do not access this property directly.
  internal var _heapBufferBridged_DoNotUse: AnyObject? = nil

  internal let _nativeStorage: _ContiguousArrayStorageBase

  internal var _heapBufferBridgedPtr: UnsafeMutablePointer<AnyObject?> {
    return UnsafeMutablePointer(_getUnsafePointerToStoredProperties(self))
  }

  internal var _heapBufferBridged: HeapBufferStorage<Int, AnyObject>? {
    if let ref: AnyObject =
      _stdlib_atomicLoadARCRef(object: _heapBufferBridgedPtr) {
      return unsafeBitCast(ref, HeapBufferStorage<Int, AnyObject>.self)
    }
    return nil
  }

  internal init(_nativeStorage: _ContiguousArrayStorageBase) {
    self._nativeStorage = _nativeStorage
  }

  internal func _destroyBridgedStorage(hb: HeapBufferStorage<Int, AnyObject>?) {
    if let bridgedStorage = hb {
      let heapBuffer = HeapBuffer(bridgedStorage)
      let count = heapBuffer.value
      heapBuffer.baseAddress.destroy(count)
    }
  }

  deinit {
    _destroyBridgedStorage(_heapBufferBridged)
  }

  internal func _getBridgedUnsafeBuffer() -> UnsafeBufferPointer<AnyObject> {
    // If we've already got a buffer of bridged objects, just return it.
    if let bridgedStorage = _heapBufferBridged {
      let heapBuffer = HeapBuffer(bridgedStorage)
      return UnsafeBufferPointer(
        start: heapBuffer.baseAddress, count: heapBuffer.value)
    }

    // If elements are bridged verbatim, the native buffer is all we
    // need, so return that.
    let maybeNewDataUnsafeBuffer =
      _nativeStorage._tryGetVerbatimBridgedUnsafeBuffer()
    if maybeNewDataUnsafeBuffer.baseAddress != nil {
      return maybeNewDataUnsafeBuffer
    }

    // Create buffer of bridged objects.
    let bridgedHeapBuffer = _nativeStorage._getNonVerbatimBridgedHeapBuffer()
    
    // Atomically store a reference to that buffer in self.
    if !_stdlib_atomicInitializeARCRef(
      object: _heapBufferBridgedPtr, desired: bridgedHeapBuffer.storage!) {
      // Another thread won the race.  Throw out our buffer and try again.
      _destroyBridgedStorage(bridgedHeapBuffer.storage!)
      return _getBridgedUnsafeBuffer()
    }

    // Now that we're bridged, we don't need the native storage any
    // longer... but we can't let it go without causing a race against
    // another thread that may already be building its own buffer of
    // bridged objects.
    return UnsafeBufferPointer(
      start: bridgedHeapBuffer.baseAddress, count: bridgedHeapBuffer.value)
  }

  //
  // NSArray implementation.
  //
  // Do not call any of these methods from the standard library!
  //

  @objc internal var count: Int {
    // Note: calling this function should not trigger bridging of array
    // elements, there is no need in that.
    if let bridgedStorage = _heapBufferBridged {
      return HeapBuffer(bridgedStorage).value
    }

    // Check if elements are bridged verbatim.
    let maybeNewDataUnsafeBuffer =
      _nativeStorage._tryGetVerbatimBridgedUnsafeBuffer()
    if maybeNewDataUnsafeBuffer.baseAddress != nil {
      return maybeNewDataUnsafeBuffer.count
    }
    return _nativeStorage._getNonVerbatimBridgedCount()
  }

  @objc internal func objectAtIndex(index: Int) -> AnyObject {
    let bridgedUnsafeBuffer = _getBridgedUnsafeBuffer()
    _precondition(
      _isValidArraySubscript(index, bridgedUnsafeBuffer.count),
      "Array index out of range")
    return bridgedUnsafeBuffer[index]
  }

  @objc internal func getObjects(
    aBuffer: UnsafeMutablePointer<AnyObject>, range: _SwiftNSRange) {
    let bridgedUnsafeBuffer = _getBridgedUnsafeBuffer()
    _precondition(
      _isValidArrayIndex(range.location, bridgedUnsafeBuffer.count),
      "Array index out of range")
    _precondition(
      _isValidArrayIndex(
        range.location + range.length, bridgedUnsafeBuffer.count),
      "Array index out of range")

    // These objects are "returned" at +0, so treat them as values to
    // avoid retains.
    var dst = UnsafeMutablePointer<Word>(aBuffer)
    dst.initializeFrom(
      UnsafeMutablePointer(bridgedUnsafeBuffer.baseAddress + range.location),
      count: range.length)
  }

  @objc internal func countByEnumeratingWithState(
    state: UnsafeMutablePointer<_SwiftNSFastEnumerationState>,
    objects: UnsafeMutablePointer<AnyObject>, count: Int
  ) -> Int {
    var enumerationState = state.memory

    if enumerationState.state != 0 {
      return 0
    }

    let bridgedUnsafeBuffer = _getBridgedUnsafeBuffer()
    enumerationState.mutationsPtr = _fastEnumerationStorageMutationsPtr
    enumerationState.itemsPtr = unsafeBitCast(
      bridgedUnsafeBuffer.baseAddress,
      AutoreleasingUnsafeMutablePointer<AnyObject?>.self)
    enumerationState.state = 1
    state.memory = enumerationState
    return bridgedUnsafeBuffer.count
  }

  @objc internal func copyWithZone(_: _SwiftNSZone) -> AnyObject {
    return self
  }
}

/// Base class of the heap buffer backing arrays.
class _ContiguousArrayStorageBase {
  internal func _tryGetVerbatimBridgedUnsafeBuffer(
    dummy: Void
  ) -> UnsafeBufferPointer<AnyObject> {
    _sanityCheckFailure(
      "Concrete subclasses must implement _tryGetVerbatimBridgedUnsafeBuffer")
  }

  internal func _getNonVerbatimBridgedCount(dummy: Void) -> Int {
    _sanityCheckFailure(
      "Concrete subclasses must implement _getNonVerbatimBridgedCount")
  }

  internal func _getNonVerbatimBridgedHeapBuffer(dummy: Void) ->
    HeapBuffer<Int, AnyObject> {
    _sanityCheckFailure(
      "Concrete subclasses must implement _getNonVerbatimBridgedHeapBuffer")
  }

  func canStoreElementsOfDynamicType(_: Any.Type) -> Bool {
    _sanityCheckFailure(
      "Concrete subclasses must implement canStoreElementsOfDynamicType")
  }

  /// A type that every element in the array is.
  var staticElementType: Any.Type {
    _sanityCheckFailure(
      "Concrete subclasses must implement staticElementType")
  }
}

