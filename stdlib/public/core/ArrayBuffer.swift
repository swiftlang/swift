//===--- ArrayBuffer.swift - Dynamic storage for Swift Array --------------===//
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
//  This is the class that implements the storage and object management for
//  Swift Array.
//
//===----------------------------------------------------------------------===//

#if _runtime(_ObjC)
import SwiftShims

internal typealias _ArrayBridgeStorage
  = _BridgeStorage<_ContiguousArrayStorageBase, _NSArrayCore>

@_fixed_layout
public struct _ArrayBuffer<Element> : _ArrayBufferProtocol {

  /// Create an empty buffer.
  public init() {
    _storage = _ArrayBridgeStorage(native: _emptyArrayStorage)
  }

  public init(nsArray: _NSArrayCore) {
    _sanityCheck(_isClassOrObjCExistential(Element.self))
    _storage = _ArrayBridgeStorage(objC: nsArray)
  }

  /// Returns an `_ArrayBuffer<U>` containing the same elements.
  ///
  /// - Precondition: The elements actually have dynamic type `U`, and `U`
  ///   is a class or `@objc` existential.
  @warn_unused_result
  internal func cast<U>(toBufferOf _: U.Type) -> _ArrayBuffer<U> {
    _sanityCheck(_isClassOrObjCExistential(Element.self))
    _sanityCheck(_isClassOrObjCExistential(U.self))
    return _ArrayBuffer<U>(storage: _storage)
  }

  /// The spare bits that are set when a native array needs deferred
  /// element type checking.
  var deferredTypeCheckMask : Int { return 1 }
  
  /// Returns an `_ArrayBuffer<U>` containing the same elements,
  /// deferring checking each element's `U`-ness until it is accessed.
  ///
  /// - Precondition: `U` is a class or `@objc` existential derived from
  /// `Element`.
  @warn_unused_result
  internal func downcast<U>(
    toBufferWithDeferredTypeCheckOf _: U.Type
  ) -> _ArrayBuffer<U> {
    _sanityCheck(_isClassOrObjCExistential(Element.self))
    _sanityCheck(_isClassOrObjCExistential(U.self))
    
    // FIXME: can't check that U is derived from Element pending
    // <rdar://problem/19915280> generic metatype casting doesn't work
    // _sanityCheck(U.self is Element.Type)

    return _ArrayBuffer<U>(
      storage: _ArrayBridgeStorage(
        native: _native._storage, bits: deferredTypeCheckMask))
  }

  var needsElementTypeCheck: Bool {
    // NSArray's need an element typecheck when the element type isn't AnyObject
    return !_isNativeTypeChecked && !(AnyObject.self is Element.Type)
  }
  
  //===--- private --------------------------------------------------------===//
  internal init(storage: _ArrayBridgeStorage) {
    _storage = storage
  }

  internal var _storage: _ArrayBridgeStorage
}

extension _ArrayBuffer {
  /// Adopt the storage of `source`.
  public init(_ source: NativeBuffer, shiftedToStartIndex: Int) {
    _sanityCheck(shiftedToStartIndex == 0, "shiftedToStartIndex must be 0")
    _storage = _ArrayBridgeStorage(native: source._storage)
  }

  /// `true`, if the array is native and does not need a deferred type check.
  var arrayPropertyIsNativeTypeChecked : Bool {
    return _isNativeTypeChecked
  }

  /// Returns `true` iff this buffer's storage is uniquely-referenced.
  @warn_unused_result
  mutating func isUniquelyReferenced() -> Bool {
    if !_isClassOrObjCExistential(Element.self) {
      return _storage.isUniquelyReferenced_native_noSpareBits()
    }
    return _storage.isUniquelyReferencedNative() && _isNative
  }

  /// Returns `true` iff this buffer's storage is either
  /// uniquely-referenced or pinned.
  @warn_unused_result
  mutating func isUniquelyReferencedOrPinned() -> Bool {
    if !_isClassOrObjCExistential(Element.self) {
      return _storage.isUniquelyReferencedOrPinned_native_noSpareBits()
    }
    return _storage.isUniquelyReferencedOrPinnedNative() && _isNative
  }

  /// Convert to an NSArray.
  ///
  /// - Precondition: `_isBridgedToObjectiveC(Element.self)`.
  ///   O(1) if the element type is bridged verbatim, O(N) otherwise.
  @warn_unused_result
  public func _asCocoaArray() -> _NSArrayCore {
    _sanityCheck(
      _isBridgedToObjectiveC(Element.self),
      "Array element type is not bridged to Objective-C")

    return _fastPath(_isNative) ? _native._asCocoaArray() : _nonNative
  }

  /// If this buffer is backed by a uniquely-referenced mutable
  /// `_ContiguousArrayBuffer` that can be grown in-place to allow the self
  /// buffer store minimumCapacity elements, returns that buffer.
  /// Otherwise, returns `nil`.
  @warn_unused_result
  public mutating func requestUniqueMutableBackingBuffer(minimumCapacity: Int)
  -> NativeBuffer? {
    if _fastPath(isUniquelyReferenced()) {
      let b = _native
      if _fastPath(b.capacity >= minimumCapacity) {
        return b
      }
    }
    return nil
  }

  @warn_unused_result
  public mutating func isMutableAndUniquelyReferenced() -> Bool {
    return isUniquelyReferenced()
  }

  @warn_unused_result
  public mutating func isMutableAndUniquelyReferencedOrPinned() -> Bool {
    return isUniquelyReferencedOrPinned()
  }

  /// If this buffer is backed by a `_ContiguousArrayBuffer`
  /// containing the same number of elements as `self`, return it.
  /// Otherwise, return `nil`.
  @warn_unused_result
  public func requestNativeBuffer() -> NativeBuffer? {
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

  func _typeCheck(_ subRange: Range<Int>) {
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
  /// memory starting at `target`.  Return a pointer past-the-end of the
  /// just-initialized memory.
  @discardableResult
  public func _copyContents(
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

    let buffer = UnsafeMutablePointer<AnyObject>(target)
    
    // Copies the references out of the NSArray without retaining them
    nonNative.getObjects(buffer, range: nsSubRange)
    
    // Make another pass to retain the copied objects
    var result = target
    for _ in CountableRange(bounds) {
      result.initialize(with: result.pointee)
      result += 1
    }
    return result
  }

  /// Returns a `_SliceBuffer` containing the given sub-range of elements in
  /// `bounds` from this buffer.
  public subscript(bounds: Range<Int>) -> _SliceBuffer<Element> {
    get {
      _typeCheck(bounds)

      if _fastPath(_isNative) {
        return _native[bounds]
      }

      let boundsCount = bounds.count
      if boundsCount == 0 {
        return _SliceBuffer(
          _ContiguousArrayBuffer<Element>(),
          shiftedToStartIndex: bounds.lowerBound)
      }

      // Look for contiguous storage in the NSArray
      let nonNative = self._nonNative
      let cocoa = _CocoaArrayWrapper(nonNative)
      let cocoaStorageBaseAddress =
        cocoa.contiguousStorage(Range(self.indices))

      if let cocoaStorageBaseAddress = cocoaStorageBaseAddress {
        return _SliceBuffer(
          owner: nonNative,
          subscriptBaseAddress: UnsafeMutablePointer(cocoaStorageBaseAddress),
          indices: bounds,
          hasNativeBuffer: false)
      }

      // No contiguous storage found; we must allocate
      let result = _ContiguousArrayBuffer<Element>(
        uninitializedCount: boundsCount, minimumCapacity: 0)

      // Tell Cocoa to copy the objects into our storage
      cocoa.buffer.getObjects(
        UnsafeMutablePointer(result.firstElementAddress),
        range: _SwiftNSRange(location: bounds.lowerBound, length: boundsCount))

      return _SliceBuffer(result, shiftedToStartIndex: bounds.lowerBound)
    }
    set {
      fatalError("not implemented")
    }
  }

  /// A pointer to the first element.
  ///
  /// - Precondition: The elements are known to be stored contiguously.
  public var firstElementAddress: UnsafeMutablePointer<Element> {
    _sanityCheck(_isNative, "must be a native buffer")
    return _native.firstElementAddress
  }

  public var firstElementAddressIfContiguous: UnsafeMutablePointer<Element>? {
    return _fastPath(_isNative) ? firstElementAddress : nil
  }

  /// The number of elements the buffer stores.
  public var count: Int {
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
  public var capacity: Int {
    return _fastPath(_isNative) ? _native.capacity : _nonNative.count
  }

  @_versioned
  @inline(__always)
  @warn_unused_result
  func getElement(_ i: Int, wasNativeTypeChecked: Bool) -> Element {
    if _fastPath(wasNativeTypeChecked) {
      return _nativeTypeChecked[i]
    }
    return unsafeBitCast(_getElementSlowPath(i), to: Element.self)
  }

  @_versioned
  @inline(never)
  @warn_unused_result
  func _getElementSlowPath(_ i: Int) -> AnyObject {
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
  public subscript(i: Int) -> Element {
    get {
      return getElement(i, wasNativeTypeChecked: _isNativeTypeChecked)
    }
    
    nonmutating set {
      if _fastPath(_isNative) {
        _native[i] = newValue
      }
      else {
        var refCopy = self
        refCopy.replace(
          subRange: i..<(i + 1),
          with: 1,
          elementsOf: CollectionOfOne(newValue))
      }
    }
  }

  /// Call `body(p)`, where `p` is an `UnsafeBufferPointer` over the
  /// underlying contiguous storage.  If no such storage exists, it is
  /// created on-demand.
  public func withUnsafeBufferPointer<R>(
    _ body: @noescape (UnsafeBufferPointer<Element>) throws -> R
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
  public mutating func withUnsafeMutableBufferPointer<R>(
    _ body: @noescape (UnsafeMutableBufferPointer<Element>) throws -> R
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
  public var owner: AnyObject {
    return _fastPath(_isNative) ? _native._storage : _nonNative
  }
  
  /// An object that keeps the elements stored in this buffer alive.
  ///
  /// - Precondition: This buffer is backed by a `_ContiguousArrayBuffer`.
  public var nativeOwner: AnyObject {
    _sanityCheck(_isNative, "Expect a native array")
    return _native._storage
  }

  /// A value that identifies the storage used by the buffer.  Two
  /// buffers address the same elements when they have the same
  /// identity and count.
  public var identity: UnsafePointer<Void> {
    if _isNative {
      return _native.identity
    }
    else {
      return unsafeAddress(of: _nonNative)
    }
  }
  
  //===--- Collection conformance -------------------------------------===//
  /// The position of the first element in a non-empty collection.
  ///
  /// In an empty collection, `startIndex == endIndex`.
  public var startIndex: Int {
    return 0
  }

  /// The collection's "past the end" position.
  ///
  /// `endIndex` is not a valid argument to `subscript`, and is always
  /// reachable from `startIndex` by zero or more applications of
  /// `index(after:)`.
  public var endIndex: Int {
    return count
  }

  public typealias Indices = CountableRange<Int>

  //===--- private --------------------------------------------------------===//
  typealias Storage = _ContiguousArrayStorage<Element>
  public typealias NativeBuffer = _ContiguousArrayBuffer<Element>

  @_versioned
  var _isNative: Bool {
    if !_isClassOrObjCExistential(Element.self) {
      return true
    } else {
      return _storage.isNative
    }
  }

  /// `true`, if the array is native and does not need a deferred type check.
  var _isNativeTypeChecked: Bool {
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
  var _native: NativeBuffer {
    return NativeBuffer(
      _isClassOrObjCExistential(Element.self)
      ? _storage.nativeInstance : _storage.nativeInstance_noSpareBits)
  }

  /// Fast access to the native representation.
  ///
  /// - Precondition: `_isNativeTypeChecked`.
  @_versioned
  var _nativeTypeChecked: NativeBuffer {
    return NativeBuffer(_storage.nativeInstance_noSpareBits)
  }

  @_versioned
  var _nonNative: _NSArrayCore {
    @inline(__always)
    get {
      _sanityCheck(_isClassOrObjCExistential(Element.self))
      return _storage.objCInstance
    }
  }
}
#endif
