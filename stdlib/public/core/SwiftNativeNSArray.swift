//===--- SwiftNativeNSArray.swift -----------------------------------------===//
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
//  _ContiguousArrayStorageBase supplies the implementation of the
//  _NSArrayCore API (and thus, NSArray the API) for our
//  _ContiguousArrayStorage<T>.  We can't put this implementation
//  directly on _ContiguousArrayStorage because generic classes can't
//  override Objective-C selectors.
//
//===----------------------------------------------------------------------===//

#if _runtime(_ObjC)
import SwiftShims

/// Returns `true` iff the given `index` is valid as a position, i.e. `0
/// ≤ index ≤ count`.
@usableFromInline @_transparent
internal func _isValidArrayIndex(_ index: Int, count: Int) -> Bool {
  return (index >= 0) && (index <= count)
}

/// Returns `true` iff the given `index` is valid for subscripting, i.e.
/// `0 ≤ index < count`.
@usableFromInline @_transparent
internal func _isValidArraySubscript(_ index: Int, count: Int) -> Bool {
  return (index >= 0) && (index < count)
}

/// An `NSArray` with Swift-native reference counting and contiguous
/// storage.
@_fixed_layout // FIXME(sil-serialize-all)
@usableFromInline
internal class _SwiftNativeNSArrayWithContiguousStorage
  : _SwiftNativeNSArray { // Provides NSArray inheritance and native refcounting

  @inlinable // FIXME(sil-serialize-all)
  @nonobjc internal override init() {}

  @inlinable // FIXME(sil-serialize-all)
  deinit {}

  // Operate on our contiguous storage
  @inlinable
  internal func withUnsafeBufferOfObjects<R>(
    _ body: (UnsafeBufferPointer<AnyObject>) throws -> R
  ) rethrows -> R {
    _sanityCheckFailure(
      "Must override withUnsafeBufferOfObjects in derived classes")
  }
}

// Implement the APIs required by NSArray 
extension _SwiftNativeNSArrayWithContiguousStorage : _NSArrayCore {
  @objc internal var count: Int {
    return withUnsafeBufferOfObjects { $0.count }
  }

  @objc(objectAtIndex:)
  internal func objectAt(_ index: Int) -> AnyObject {
    return withUnsafeBufferOfObjects {
      objects in
      _precondition(
        _isValidArraySubscript(index, count: objects.count),
        "Array index out of range")
      return objects[index]
    }
  }

  @objc internal func getObjects(
    _ aBuffer: UnsafeMutablePointer<AnyObject>, range: _SwiftNSRange
  ) {
    return withUnsafeBufferOfObjects {
      objects in
      _precondition(
        _isValidArrayIndex(range.location, count: objects.count),
        "Array index out of range")

      _precondition(
        _isValidArrayIndex(
          range.location + range.length, count: objects.count),
        "Array index out of range")

      if objects.isEmpty { return }

      // These objects are "returned" at +0, so treat them as pointer values to
      // avoid retains. Copy bytes via a raw pointer to circumvent reference
      // counting while correctly aliasing with all other pointer types.
      UnsafeMutableRawPointer(aBuffer).copyMemory(
        from: objects.baseAddress! + range.location,
        byteCount: range.length * MemoryLayout<AnyObject>.stride)
    }
  }

  @objc(countByEnumeratingWithState:objects:count:)
  internal func countByEnumerating(
    with state: UnsafeMutablePointer<_SwiftNSFastEnumerationState>,
    objects: UnsafeMutablePointer<AnyObject>?, count: Int
  ) -> Int {
    var enumerationState = state.pointee

    if enumerationState.state != 0 {
      return 0
    }

    return withUnsafeBufferOfObjects {
      objects in
      enumerationState.mutationsPtr = _fastEnumerationStorageMutationsPtr
      enumerationState.itemsPtr =
        AutoreleasingUnsafeMutablePointer(objects.baseAddress)
      enumerationState.state = 1
      state.pointee = enumerationState
      return objects.count
    }
  }

  @objc(copyWithZone:)
  internal func copy(with _: _SwiftNSZone?) -> AnyObject {
    return self
  }
}

/// An `NSArray` whose contiguous storage is created and filled, upon
/// first access, by bridging the elements of a Swift `Array`.
///
/// Ideally instances of this class would be allocated in-line in the
/// buffers used for Array storage.
@_fixed_layout // FIXME(sil-serialize-all)
@usableFromInline
@objc internal final class _SwiftDeferredNSArray
  : _SwiftNativeNSArrayWithContiguousStorage {

  // This stored property should be stored at offset zero.  We perform atomic
  // operations on it.
  //
  // Do not access this property directly.
  @usableFromInline
  @nonobjc
  internal var _heapBufferBridged_DoNotUse: AnyObject?

  // When this class is allocated inline, this property can become a
  // computed one.
  @usableFromInline
  @nonobjc
  internal let _nativeStorage: _ContiguousArrayStorageBase

  @inlinable
  @nonobjc
  internal var _heapBufferBridgedPtr: UnsafeMutablePointer<AnyObject?> {
    return _getUnsafePointerToStoredProperties(self).assumingMemoryBound(
      to: Optional<AnyObject>.self)
  }

  @usableFromInline
  internal typealias HeapBufferStorage = _HeapBufferStorage<Int, AnyObject>

  @inlinable
  internal var _heapBufferBridged: HeapBufferStorage? {
    if let ref =
      _stdlib_atomicLoadARCRef(object: _heapBufferBridgedPtr) {
      return unsafeBitCast(ref, to: HeapBufferStorage.self)
    }
    return nil
  }

  @inlinable // FIXME(sil-serialize-all)
  @nonobjc
  internal init(_nativeStorage: _ContiguousArrayStorageBase) {
    self._nativeStorage = _nativeStorage
  }

  @inlinable
  internal func _destroyBridgedStorage(_ hb: HeapBufferStorage?) {
    if let bridgedStorage = hb {
      let heapBuffer = _HeapBuffer(bridgedStorage)
      let count = heapBuffer.value
      heapBuffer.baseAddress.deinitialize(count: count)
    }
  }

  @inlinable // FIXME(sil-serialize-all)
  deinit {
    _destroyBridgedStorage(_heapBufferBridged)
  }

  @inlinable
  internal override func withUnsafeBufferOfObjects<R>(
    _ body: (UnsafeBufferPointer<AnyObject>) throws -> R
  ) rethrows -> R {
    while true {
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

          // Another thread won the race.  Throw out our buffer.
          _destroyBridgedStorage(
            unsafeDowncast(objects.storage!, to: HeapBufferStorage.self))
        }
        continue // Try again
      }
      
      defer { _fixLifetime(self) }
      return try body(buffer)
    }
  }

  /// Returns the number of elements in the array.
  ///
  /// This override allows the count to be read without triggering
  /// bridging of array elements.
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
@usableFromInline
@_fixed_layout
internal class _SwiftNativeNSArrayWithContiguousStorage {
  @inlinable
  internal init() {}

  @inlinable
  deinit {}
}
#endif

/// Base class of the heap buffer backing arrays.  
@usableFromInline
@_fixed_layout
internal class _ContiguousArrayStorageBase
  : _SwiftNativeNSArrayWithContiguousStorage {

  @usableFromInline
  final var countAndCapacity: _ArrayBody

  @inlinable // FIXME(sil-serialize-all)
  @nonobjc
  internal init(_doNotCallMeBase: ()) {
    _sanityCheckFailure("creating instance of _ContiguousArrayStorageBase")
  }
  
#if _runtime(_ObjC)
  @inlinable
  internal override func withUnsafeBufferOfObjects<R>(
    _ body: (UnsafeBufferPointer<AnyObject>) throws -> R
  ) rethrows -> R {
    if let result = try _withVerbatimBridgedUnsafeBuffer(body) {
      return result
    }
    _sanityCheckFailure(
      "Can't use a buffer of non-verbatim-bridged elements as an NSArray")
  }

  /// If the stored type is bridged verbatim, invoke `body` on an
  /// `UnsafeBufferPointer` to the elements and return the result.
  /// Otherwise, return `nil`.
  @inlinable // FIXME(sil-serialize-all)
  internal func _withVerbatimBridgedUnsafeBuffer<R>(
    _ body: (UnsafeBufferPointer<AnyObject>) throws -> R
  ) rethrows -> R? {
    _sanityCheckFailure(
      "Concrete subclasses must implement _withVerbatimBridgedUnsafeBuffer")
  }

  @inlinable // FIXME(sil-serialize-all)
  @nonobjc
  internal func _getNonVerbatimBridgedCount() -> Int {
    _sanityCheckFailure(
      "Concrete subclasses must implement _getNonVerbatimBridgedCount")
  }

  @inlinable // FIXME(sil-serialize-all)
  internal func _getNonVerbatimBridgedHeapBuffer() ->
    _HeapBuffer<Int, AnyObject> {
    _sanityCheckFailure(
      "Concrete subclasses must implement _getNonVerbatimBridgedHeapBuffer")
  }
#endif

  @inlinable // FIXME(sil-serialize-all)
  internal func canStoreElements(ofDynamicType _: Any.Type) -> Bool {
    _sanityCheckFailure(
      "Concrete subclasses must implement canStoreElements(ofDynamicType:)")
  }

  /// A type that every element in the array is.
  @inlinable // FIXME(sil-serialize-all)
  internal var staticElementType: Any.Type {
    _sanityCheckFailure(
      "Concrete subclasses must implement staticElementType")
  }

  @inlinable // FIXME(sil-serialize-all)
  deinit {
    _sanityCheck(
      self !== _emptyArrayStorage, "Deallocating empty array storage?!")
  }
}
