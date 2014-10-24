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
//  _NSArrayCoreType API (and thus, NSArray the API) for our
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
@objc internal final class _SwiftNativeNSArray : _NSArrayCoreType {

  // This stored property should be stored at offset zero.  We perform atomic
  // operations on it.
  //
  // Do not access this property directly.
  internal var _heapBufferBridged_DoNotUse: AnyObject? = nil

  internal let _nativeStorage: _ContiguousArrayStorageBase

  internal var _heapBufferBridgedPtr: UnsafeMutablePointer<AnyObject?> {
    return UnsafeMutablePointer(_getUnsafePointerToStoredProperties(self))
  }

  internal typealias HeapBufferStorage = _HeapBufferStorage<Int, AnyObject>
  
  internal var _heapBufferBridged: HeapBufferStorage? {
    if let ref: AnyObject =
      _stdlib_atomicLoadARCRef(object: _heapBufferBridgedPtr) {
      return unsafeBitCast(ref, HeapBufferStorage.self)
    }
    return nil
  }

  internal init(_nativeStorage: _ContiguousArrayStorageBase) {
    self._nativeStorage = _nativeStorage
  }

  internal func _destroyBridgedStorage(hb: HeapBufferStorage?) {
    if let bridgedStorage = hb {
      let heapBuffer = _HeapBuffer(bridgedStorage)
      let count = heapBuffer.value
      heapBuffer.baseAddress.destroy(count)
    }
  }

  deinit {
    _destroyBridgedStorage(_heapBufferBridged)
  }

  internal func _withBridgedUnsafeBuffer<R>(
    body: (UnsafeBufferPointer<AnyObject>)->R
  ) -> R {
    do {
      var buffer: UnsafeBufferPointer<AnyObject>
      
      // If we've already got a buffer of bridged objects, just use it
      if let bridgedStorage = _heapBufferBridged {
        let heapBuffer = _HeapBuffer(bridgedStorage)
        buffer = UnsafeBufferPointer(
            start: heapBuffer.baseAddress, count: heapBuffer.value)
      }

      // If elements are bridged verbatim, the native buffer is all we
      // need, so return that.
      else if let buf = _nativeStorage._withVerbatimBridgedUnsafeBuffer({ $0 }) {
        buffer = buf
      }
      else {
        // Create buffer of bridged objects.
        let bridgedHeapBuffer = _nativeStorage._getNonVerbatimBridgedHeapBuffer()
        
        // Atomically store a reference to that buffer in self.
        if !_stdlib_atomicInitializeARCRef(
          object: _heapBufferBridgedPtr, desired: bridgedHeapBuffer.storage!) {
          // Another thread won the race.  Throw out our buffer
          let storage: HeapBufferStorage
            = unsafeDowncast(bridgedHeapBuffer.storage!)
          _destroyBridgedStorage(storage)
        }
        continue // try again
      }
      let result = body(buffer)
      _fixLifetime(self)
      return result
    }
    while true
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
      return _HeapBuffer(bridgedStorage).value
    }

    // Check if elements are bridged verbatim.
    return _nativeStorage._withVerbatimBridgedUnsafeBuffer { $0.count }
      ?? _nativeStorage._getNonVerbatimBridgedCount()
  }

  @objc internal func objectAtIndex(index: Int) -> AnyObject {
    return _withBridgedUnsafeBuffer {
      objects in
      _precondition(
        _isValidArraySubscript(index, objects.count),
        "Array index out of range")
      return objects[index]
    }
  }

  @objc internal func getObjects(
    aBuffer: UnsafeMutablePointer<AnyObject>, range: _SwiftNSRange
  ) {
    return _withBridgedUnsafeBuffer {
      objects in
      _precondition(
        _isValidArrayIndex(range.location, objects.count),
        "Array index out of range")

      _precondition(
        _isValidArrayIndex(
          range.location + range.length, objects.count),
        "Array index out of range")

      // These objects are "returned" at +0, so treat them as values to
      // avoid retains.
      UnsafeMutablePointer<Word>(aBuffer).initializeFrom(
        UnsafeMutablePointer(objects.baseAddress + range.location),
        count: range.length)
    }
  }

  @objc internal func countByEnumeratingWithState(
    state: UnsafeMutablePointer<_SwiftNSFastEnumerationState>,
    objects: UnsafeMutablePointer<AnyObject>, count: Int
  ) -> Int {
    var enumerationState = state.memory

    if enumerationState.state != 0 {
      return 0
    }

    return _withBridgedUnsafeBuffer {
      objects in
      enumerationState.mutationsPtr = _fastEnumerationStorageMutationsPtr
      enumerationState.itemsPtr = unsafeBitCast(
        objects.baseAddress,
        AutoreleasingUnsafeMutablePointer<AnyObject?>.self)
      enumerationState.state = 1
      state.memory = enumerationState
      return objects.count
    }
  }

  @objc internal func copyWithZone(_: _SwiftNSZone) -> AnyObject {
    return self
  }
}

/// Base class of the heap buffer backing arrays.  The only reason
/// this is not a class protocol instead, is that instances of class
/// protocols are fatter than plain object references.
class _ContiguousArrayStorageBase {
  /// If the stored type is bridged verbatim, invoke `body` on an
  /// `UnsafeBufferPointer` to the elements and return the result.
  /// Otherwise, return `nil`.
  internal func _withVerbatimBridgedUnsafeBuffer<R>(
    body: (UnsafeBufferPointer<AnyObject>)->R
  ) -> R? {
    _sanityCheckFailure(
      "Concrete subclasses must implement _withVerbatimBridgedUnsafeBuffer")
  }

  internal func _getNonVerbatimBridgedCount(dummy: Void) -> Int {
    _sanityCheckFailure(
      "Concrete subclasses must implement _getNonVerbatimBridgedCount")
  }

  internal func _getNonVerbatimBridgedHeapBuffer(dummy: Void) ->
    _HeapBuffer<Int, AnyObject> {
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

