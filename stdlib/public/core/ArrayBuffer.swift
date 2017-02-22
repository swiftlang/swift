//===--- ArrayBuffer.swift - Dynamic storage for Swift Array --------------===//
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
//  This is the class that implements the storage and object management for
//  Swift Array.
//
//===----------------------------------------------------------------------===//

#if _runtime(_ObjC)
import SwiftShims

internal typealias _ArrayBridgeStorage
  = _BridgeStorage<_ContiguousArrayStorageBase, _NSArrayCore>

@_versioned
@_fixed_layout
internal struct _ArrayBuffer<Element> : _ArrayBufferProtocol {

  /// Create an empty buffer.
  internal init() {
    _storage = _ArrayBridgeStorage(native: _emptyArrayStorage)
  }

  @_versioned  // FIXME(abi): Used from tests
  internal init(nsArray: _NSArrayCore) {
    _sanityCheck(_isClassOrObjCExistential(Element.self))
    _storage = _ArrayBridgeStorage(objC: nsArray)
  }

  /// Returns an `_ArrayBuffer<U>` containing the same elements.
  ///
  /// - Precondition: The elements actually have dynamic type `U`, and `U`
  ///   is a class or `@objc` existential.
  internal func cast<U>(toBufferOf _: U.Type) -> _ArrayBuffer<U> {
    _sanityCheck(_isClassOrObjCExistential(Element.self))
    _sanityCheck(_isClassOrObjCExistential(U.self))
    return _ArrayBuffer<U>(storage: _storage)
  }

  /// The spare bits that are set when a native array needs deferred
  /// element type checking.
  internal var deferredTypeCheckMask: Int { return 1 }
  
  /// Returns an `_ArrayBuffer<U>` containing the same elements,
  /// deferring checking each element's `U`-ness until it is accessed.
  ///
  /// - Precondition: `U` is a class or `@objc` existential derived from
  /// `Element`.
  internal func downcast<U>(
    toBufferWithDeferredTypeCheckOf _: U.Type
  ) -> _ArrayBuffer<U> {
    _sanityCheck(_isClassOrObjCExistential(Element.self))
    _sanityCheck(_isClassOrObjCExistential(U.self))
    
    // FIXME: can't check that U is derived from Element pending
    // <rdar://problem/20028320> generic metatype casting doesn't work
    // _sanityCheck(U.self is Element.Type)

    return _ArrayBuffer<U>(
      storage: _ArrayBridgeStorage(
        native: _native._storage, bits: deferredTypeCheckMask))
  }

  internal var needsElementTypeCheck: Bool {
    // NSArray's need an element typecheck when the element type isn't AnyObject
    return !_isNativeTypeChecked && !(AnyObject.self is Element.Type)
  }
  
  //===--- private --------------------------------------------------------===//
  internal init(storage: _ArrayBridgeStorage) {
    _storage = storage
  }

  @_versioned
  internal var _storage: _ArrayBridgeStorage
}

extension _ArrayBuffer {
  /// Adopt the storage of `source`.
  internal init(_buffer source: NativeBuffer, shiftedToStartIndex: Int) {
    _sanityCheck(shiftedToStartIndex == 0, "shiftedToStartIndex must be 0")
    _storage = _ArrayBridgeStorage(native: source._storage)
  }

  /// `true`, if the array is native and does not need a deferred type check.
  internal var arrayPropertyIsNativeTypeChecked: Bool {
    return _isNativeTypeChecked
  }

  /// Returns `true` iff this buffer's storage is uniquely-referenced.
  internal mutating func isUniquelyReferenced() -> Bool {
    if !_isClassOrObjCExistential(Element.self) {
      return _storage.isUniquelyReferenced_native_noSpareBits()
    }
    return _storage.isUniquelyReferencedNative() && _isNative
  }

  /// Returns `true` iff this buffer's storage is either
  /// uniquely-referenced or pinned.
  internal mutating func isUniquelyReferencedOrPinned() -> Bool {
    if !_isClassOrObjCExistential(Element.self) {
      return _storage.isUniquelyReferencedOrPinned_native_noSpareBits()
    }
    return _storage.isUniquelyReferencedOrPinnedNative() && _isNative
  }

  /// Convert to an NSArray.
  ///
  /// O(1) if the element type is bridged verbatim, O(*n*) otherwise.
  @_versioned  // FIXME(abi): Used from tests
  internal func _asCocoaArray() -> _NSArrayCore {
    return _fastPath(_isNative) ? _native._asCocoaArray() : _nonNative
  }

  /// If this buffer is backed by a uniquely-referenced mutable
  /// `_ContiguousArrayBuffer` that can be grown in-place to allow the self
  /// buffer store minimumCapacity elements, returns that buffer.
  /// Otherwise, returns `nil`.
  internal mutating func requestUniqueMutableBackingBuffer(minimumCapacity: Int)
  -> NativeBuffer? {
    if _fastPath(isUniquelyReferenced()) {
      let b = _native
      if _fastPath(b.capacity >= minimumCapacity) {
        return b
      }
    }
    return nil
  }

  internal mutating func isMutableAndUniquelyReferenced() -> Bool {
    return isUniquelyReferenced()
  }

  internal mutating func isMutableAndUniquelyReferencedOrPinned() -> Bool {
    return isUniquelyReferencedOrPinned()
  }

  /// If this buffer is backed by a `_ContiguousArrayBuffer`
  /// containing the same number of elements as `self`, return it.
  /// Otherwise, return `nil`.
  internal func requestNativeBuffer() -> NativeBuffer? {
    if !_isClassOrObjCExistential(Element.self) {
      return _native
    }
    return _fastPath(_storage.isNative) ? _native : nil
  }

  // We have two versions of type check: one that takes a range and the other
  // checks one element. The reason for this is that the ARC optimizer does not
  // handle loops atm. and so can get blocked by the presence of a loop (over
  // the range). This loop is not necessary for a single element access.
  @inline(never)
  internal func _typeCheckSlowPath(_ index: Int) {
    if _fastPath(_isNative) {
      let element: AnyObject = cast(toBufferOf: AnyObject.self)._native[index]
      _precondition(
        element is Element,
        "Down-casted Array element failed to match the target type")
    }
    else {
      let ns = _nonNative
      _precondition(
        ns.objectAt(index) is Element,
        "NSArray element failed to match the Swift Array Element type")
    }
  }

  internal func _typeCheck(_ subRange: Range<Int>) {
    if !_isClassOrObjCExistential(Element.self) {
      return
    }

    if _slowPath(needsElementTypeCheck) {
      // Could be sped up, e.g. by using
      // enumerateObjectsAtIndexes:options:usingBlock: in the
      // non-native case.
      for i in CountableRange(subRange) {
        _typeCheckSlowPath(i)
      }
    }
  }

  /// Copy the elements in `bounds` from this buffer into uninitialized
  /// memory starting at `target`.  Return a pointer "past the end" of the
  /// just-initialized memory.
  @discardableResult
  internal func _copyContents(
    subRange bounds: Range<Int>,
    initializing target: UnsafeMutablePointer<Element>
  ) -> UnsafeMutablePointer<Element> {
    _typeCheck(bounds)
    if _fastPath(_isNative) {
      return _native._copyContents(subRange: bounds, initializing: target)
    }

    let nonNative = _nonNative

    let nsSubRange = SwiftShims._SwiftNSRange(
      location: bounds.lowerBound,
      length: bounds.upperBound - bounds.lowerBound)

    let buffer = UnsafeMutableRawPointer(target).assumingMemoryBound(
      to: AnyObject.self)
    
    // Copies the references out of the NSArray without retaining them
    nonNative.getObjects(buffer, range: nsSubRange)
    
    // Make another pass to retain the copied objects
    var result = target
    for _ in CountableRange(bounds) {
      result.initialize(to: result.pointee)
      result += 1
    }
    return result
  }

  /// Returns a `_SliceBuffer` containing the given sub-range of elements in
  /// `bounds` from this buffer.
  internal subscript(bounds: Range<Int>) -> _SliceBuffer<Element> {
    get {
      _typeCheck(bounds)

      if _fastPath(_isNative) {
        return _native[bounds]
      }

      let boundsCount = bounds.count
      if boundsCount == 0 {
        return _SliceBuffer(
          _buffer: _ContiguousArrayBuffer<Element>(),
          shiftedToStartIndex: bounds.lowerBound)
      }

      // Look for contiguous storage in the NSArray
      let nonNative = self._nonNative
      let cocoa = _CocoaArrayWrapper(nonNative)
      let cocoaStorageBaseAddress =
        cocoa.contiguousStorage(Range(self.indices))

      if let cocoaStorageBaseAddress = cocoaStorageBaseAddress {
        let basePtr = UnsafeMutableRawPointer(cocoaStorageBaseAddress)
          .assumingMemoryBound(to: Element.self)
        return _SliceBuffer(
          owner: nonNative,
          subscriptBaseAddress: basePtr,
          indices: bounds,
          hasNativeBuffer: false)
      }

      // No contiguous storage found; we must allocate
      let result = _ContiguousArrayBuffer<Element>(
        _uninitializedCount: boundsCount, minimumCapacity: 0)

      // Tell Cocoa to copy the objects into our storage
      cocoa.buffer.getObjects(
        UnsafeMutableRawPointer(result.firstElementAddress)
          .assumingMemoryBound(to: AnyObject.self),
        range: _SwiftNSRange(location: bounds.lowerBound, length: boundsCount))

      return _SliceBuffer(
        _buffer: result, shiftedToStartIndex: bounds.lowerBound)
    }
    set {
      fatalError("not implemented")
    }
  }

  /// A pointer to the first element.
  ///
  /// - Precondition: The elements are known to be stored contiguously.
  @_versioned
  internal var firstElementAddress: UnsafeMutablePointer<Element> {
    _sanityCheck(_isNative, "must be a native buffer")
    return _native.firstElementAddress
  }

  internal var firstElementAddressIfContiguous: UnsafeMutablePointer<Element>? {
    return _fastPath(_isNative) ? firstElementAddress : nil
  }

  /// The number of elements the buffer stores.
  internal var count: Int {
    @inline(__always)
    get {
      return _fastPath(_isNative) ? _native.count : _nonNative.count
    }
    set {
      _sanityCheck(_isNative, "attempting to update count of Cocoa array")
      _native.count = newValue
    }
  }
  
  /// Traps if an inout violation is detected or if the buffer is
  /// native and the subscript is out of range.
  ///
  /// wasNative == _isNative in the absence of inout violations.
  /// Because the optimizer can hoist the original check it might have
  /// been invalidated by illegal user code.
  internal func _checkInoutAndNativeBounds(_ index: Int, wasNative: Bool) {
    _precondition(
      _isNative == wasNative,
      "inout rules were violated: the array was overwritten")

    if _fastPath(wasNative) {
      _native._checkValidSubscript(index)
    }
  }

  // TODO: gyb this
  
  /// Traps if an inout violation is detected or if the buffer is
  /// native and typechecked and the subscript is out of range.
  ///
  /// wasNativeTypeChecked == _isNativeTypeChecked in the absence of
  /// inout violations.  Because the optimizer can hoist the original
  /// check it might have been invalidated by illegal user code.
  internal func _checkInoutAndNativeTypeCheckedBounds(
    _ index: Int, wasNativeTypeChecked: Bool
  ) {
    _precondition(
      _isNativeTypeChecked == wasNativeTypeChecked,
      "inout rules were violated: the array was overwritten")

    if _fastPath(wasNativeTypeChecked) {
      _native._checkValidSubscript(index)
    }
  }

  /// The number of elements the buffer can store without reallocation.
  internal var capacity: Int {
    return _fastPath(_isNative) ? _native.capacity : _nonNative.count
  }

  @_versioned
  @inline(__always)
  internal func getElement(_ i: Int, wasNativeTypeChecked: Bool) -> Element {
    if _fastPath(wasNativeTypeChecked) {
      return _nativeTypeChecked[i]
    }
    return unsafeBitCast(_getElementSlowPath(i), to: Element.self)
  }

  @_versioned
  @inline(never)
  internal func _getElementSlowPath(_ i: Int) -> AnyObject {
    _sanityCheck(
      _isClassOrObjCExistential(Element.self),
      "Only single reference elements can be indexed here.")
    let element: AnyObject
    if _isNative {
      // _checkInoutAndNativeTypeCheckedBounds does no subscript
      // checking for the native un-typechecked case.  Therefore we
      // have to do it here.
      _native._checkValidSubscript(i)
      
      element = cast(toBufferOf: AnyObject.self)._native[i]
      _precondition(
        element is Element,
        "Down-casted Array element failed to match the target type")
    } else {
      // ObjC arrays do their own subscript checking.
      element = _nonNative.objectAt(i)
      _precondition(
        element is Element,
        "NSArray element failed to match the Swift Array Element type")
    }
    return element
  }

  /// Get or set the value of the ith element.
  internal subscript(i: Int) -> Element {
    get {
      return getElement(i, wasNativeTypeChecked: _isNativeTypeChecked)
    }
    
    nonmutating set {
      if _fastPath(_isNative) {
        _native[i] = newValue
      }
      else {
        var refCopy = self
        refCopy.replaceSubrange(
          i..<(i + 1),
          with: CollectionOfOne(newValue))
      }
    }
  }

  /// Call `body(p)`, where `p` is an `UnsafeBufferPointer` over the
  /// underlying contiguous storage.  If no such storage exists, it is
  /// created on-demand.
  internal func withUnsafeBufferPointer<R>(
    _ body: (UnsafeBufferPointer<Element>) throws -> R
  ) rethrows -> R {
    if _fastPath(_isNative) {
      defer { _fixLifetime(self) }
      return try body(
        UnsafeBufferPointer(start: firstElementAddress, count: count))
    }
    return try ContiguousArray(self).withUnsafeBufferPointer(body)
  }
  
  /// Call `body(p)`, where `p` is an `UnsafeMutableBufferPointer`
  /// over the underlying contiguous storage.
  ///
  /// - Precondition: Such contiguous storage exists or the buffer is empty.
  internal mutating func withUnsafeMutableBufferPointer<R>(
    _ body: (UnsafeMutableBufferPointer<Element>) throws -> R
  ) rethrows -> R {
    _sanityCheck(
      _isNative || count == 0,
      "Array is bridging an opaque NSArray; can't get a pointer to the elements"
    )
    defer { _fixLifetime(self) }
    return try body(UnsafeMutableBufferPointer(
      start: firstElementAddressIfContiguous, count: count))
  }
  
  /// An object that keeps the elements stored in this buffer alive.
  internal var owner: AnyObject {
    return _fastPath(_isNative) ? _native._storage : _nonNative
  }
  
  /// An object that keeps the elements stored in this buffer alive.
  ///
  /// - Precondition: This buffer is backed by a `_ContiguousArrayBuffer`.
  internal var nativeOwner: AnyObject {
    _sanityCheck(_isNative, "Expect a native array")
    return _native._storage
  }

  /// A value that identifies the storage used by the buffer.  Two
  /// buffers address the same elements when they have the same
  /// identity and count.
  internal var identity: UnsafeRawPointer {
    if _isNative {
      return _native.identity
    }
    else {
      return UnsafeRawPointer(Unmanaged.passUnretained(_nonNative).toOpaque())
    }
  }
  
  //===--- Collection conformance -------------------------------------===//
  /// The position of the first element in a non-empty collection.
  ///
  /// In an empty collection, `startIndex == endIndex`.
  internal var startIndex: Int {
    return 0
  }

  /// The collection's "past the end" position.
  ///
  /// `endIndex` is not a valid argument to `subscript`, and is always
  /// reachable from `startIndex` by zero or more applications of
  /// `index(after:)`.
  internal var endIndex: Int {
    return count
  }

  internal typealias Indices = CountableRange<Int>

  //===--- private --------------------------------------------------------===//
  internal typealias Storage = _ContiguousArrayStorage<Element>
  internal typealias NativeBuffer = _ContiguousArrayBuffer<Element>

  @_versioned
  internal var _isNative: Bool {
    if !_isClassOrObjCExistential(Element.self) {
      return true
    } else {
      return _storage.isNative
    }
  }

  /// `true`, if the array is native and does not need a deferred type check.
  internal var _isNativeTypeChecked: Bool {
    if !_isClassOrObjCExistential(Element.self) {
      return true
    } else {
      return _storage.isNativeWithClearedSpareBits(deferredTypeCheckMask)
    }
  }

  /// Our native representation.
  ///
  /// - Precondition: `_isNative`.
  @_versioned
  internal var _native: NativeBuffer {
    return NativeBuffer(
      _isClassOrObjCExistential(Element.self)
      ? _storage.nativeInstance : _storage.nativeInstance_noSpareBits)
  }

  /// Fast access to the native representation.
  ///
  /// - Precondition: `_isNativeTypeChecked`.
  @_versioned
  internal var _nativeTypeChecked: NativeBuffer {
    return NativeBuffer(_storage.nativeInstance_noSpareBits)
  }

  @_versioned
  internal var _nonNative: _NSArrayCore {
    @inline(__always)
    get {
      _sanityCheck(_isClassOrObjCExistential(Element.self))
      return _storage.objCInstance
    }
  }
}
#endif
