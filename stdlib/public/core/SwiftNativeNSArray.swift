//===--- SwiftNativeNSArray.swift -----------------------------------------===//
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

#if _runtime(_ObjC)
import SwiftShims

/// Return true iff the given `index` is valid as a position, i.e. `0
/// ≤ index ≤ count`
@transparent
internal func _isValidArrayIndex(index: Int, count: Int) -> Bool {
  return (index >= 0) && (index <= count)
}

/// Return true iff the given `index` is valid for subscripting, i.e.
/// `0 ≤ index < count`
@transparent
internal func _isValidArraySubscript(index: Int, count: Int) -> Bool {
  return (index >= 0) && (index < count)
}

/// An `NSArray` with Swift-native reference counting and contiguous
/// storage.
class _SwiftNativeNSArrayWithContiguousStorage
  : _SwiftNativeNSArray { // provides NSArray inheritance and native refcounting

  // Operate on our contiguous storage
  internal func withUnsafeBufferOfObjects<R>(
    @noescape body: UnsafeBufferPointer<AnyObject> -> R
  ) -> R {
    _sanityCheckFailure(
      "Must override withUnsafeBufferOfObjects in derived classes")
  }
}

// Implement the APIs required by NSArray 
extension _SwiftNativeNSArrayWithContiguousStorage: _NSArrayCoreType {
  @objc internal var count: Int {
    return withUnsafeBufferOfObjects { $0.count }
  }

  @objc internal final func objectAtIndex(index: Int) -> AnyObject {
    return withUnsafeBufferOfObjects {
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
    return withUnsafeBufferOfObjects {
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

    return withUnsafeBufferOfObjects {
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

/// An `NSArray` whose contiguous storage is created and filled, upon
/// first access, by bridging the elements of a Swift `Array`.
///
/// Ideally instances of this class would be allocated in-line in the
/// buffers used for Array storage.
@objc internal final class _SwiftDeferredNSArray
  : _SwiftNativeNSArrayWithContiguousStorage {

  // This stored property should be stored at offset zero.  We perform atomic
  // operations on it.
  //
  // Do not access this property directly.
  internal var _heapBufferBridged_DoNotUse: AnyObject? = nil

  // When this class is allocated inline, this property can become a
  // computed one.
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

  internal override func withUnsafeBufferOfObjects<R>(
    @noescape body: UnsafeBufferPointer<AnyObject> -> R
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
      else if let buf = _nativeStorage._withVerbatimBridgedUnsafeBuffer(
        { $0 }
      ) {
        buffer = buf
      }
      else {
        // Create buffer of bridged objects.
        let objects = _nativeStorage._getNonVerbatimBridgedHeapBuffer()
        
        // Atomically store a reference to that buffer in self.
        if !_stdlib_atomicInitializeARCRef(
          object: _heapBufferBridgedPtr, desired: objects.storage!) {

          // Another thread won the race.  Throw out our buffer
          let storage: HeapBufferStorage = unsafeDowncast(objects.storage!)
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

  /// Return the number of elements in the array
  ///
  /// This override allows the count can be read without triggering
  /// bridging of array elements
  @objc
  internal override var count: Int {
    if let bridgedStorage = _heapBufferBridged {
      return _HeapBuffer(bridgedStorage).value
    }

    // Check if elements are bridged verbatim.
    return _nativeStorage._withVerbatimBridgedUnsafeBuffer { $0.count }
      ?? _nativeStorage._getNonVerbatimBridgedCount()
  }
}
#else
// Empty shim version for non-objc platforms.
class _SwiftNativeNSArrayWithContiguousStorage {}
#endif

/// Base class of the heap buffer backing arrays.  
internal class _ContiguousArrayStorageBase
  : _SwiftNativeNSArrayWithContiguousStorage {

#if _runtime(_ObjC)
  internal override func withUnsafeBufferOfObjects<R>(
    @noescape body: UnsafeBufferPointer<AnyObject> -> R
  ) -> R {
    if let result = _withVerbatimBridgedUnsafeBuffer(body) {
      return result
    }
    _sanityCheckFailure(
      "Can't use a buffer of non-verbatim-bridged elements as an NSArray")
  }

  /// If the stored type is bridged verbatim, invoke `body` on an
  /// `UnsafeBufferPointer` to the elements and return the result.
  /// Otherwise, return `nil`.
  internal func _withVerbatimBridgedUnsafeBuffer<R>(
    @noescape body: UnsafeBufferPointer<AnyObject> -> R
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
#endif

  func canStoreElementsOfDynamicType(_: Any.Type) -> Bool {
    _sanityCheckFailure(
      "Concrete subclasses must implement canStoreElementsOfDynamicType")
  }

  /// A type that every element in the array is.
  var staticElementType: Any.Type {
    _sanityCheckFailure(
      "Concrete subclasses must implement staticElementType")
  }

  deinit {
    _sanityCheck(
      self !== _emptyArrayStorage, "Deallocating empty array storage?!")
  }
}
