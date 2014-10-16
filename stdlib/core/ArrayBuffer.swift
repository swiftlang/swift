//===--- ArrayBuffer.swift - Dynamic storage for Swift Array --------------===//
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
//  This is the class that implements the storage and object management for
//  Swift Array.
//
//===----------------------------------------------------------------------===//

import SwiftShims

enum _ArrayCastKind { case Up, Down, DeferredDown }

final internal
class _IndirectArrayBuffer {
  
  init<T>(
    nativeBuffer: _ContiguousArrayBuffer<T>,
    isMutable: Bool,
    needsElementTypeCheck: Bool
  ) {
    self.buffer = nativeBuffer._storage
    self.isMutable = isMutable
    self.isCocoa = false
    self.needsElementTypeCheck = needsElementTypeCheck
  }
    
  init(cocoa: _NSArrayCoreType, needsElementTypeCheck: Bool) {
    self.buffer = cocoa
    self.isMutable = false
    self.isCocoa = true
    self.needsElementTypeCheck = needsElementTypeCheck
  }

  init<Target>(
    castFrom source: _IndirectArrayBuffer,
    toElementType _: Target.Type
  ) {
    self.buffer = source.buffer
    self.isCocoa = source.isCocoa
    
    if source.isCocoa {
      self.isMutable = false
    }
    else {
      self.isMutable = source
        .getNativeBufferOf(AnyObject.self)
        .canStoreElementsOfDynamicType(Target.self)
    }
    
    self.needsElementTypeCheck = source.needsElementTypeCheck
      ? !(AnyObject.self is Target.Type)
      : false
  }
  
  // When this buffer has immutable storage and it is modified, the
  // storage is replaced with mutable storage.
  func replaceStorage<T>(newBuffer: _ContiguousArrayBuffer<T>) {
    self.buffer = newBuffer._storage
    self.isMutable = true
    self.isCocoa = false
    self.needsElementTypeCheck = false
  }

  var buffer: AnyObject?
  var isMutable: Bool
  var isCocoa: Bool
  var needsElementTypeCheck: Bool
  
  func getNativeBufferOf<T>(_: T.Type) -> _ContiguousArrayBuffer<T> {
    _sanityCheck(!isCocoa)
    return _ContiguousArrayBuffer(
      buffer != nil ? unsafeBitCast(buffer, _ContiguousArrayStorage<T>.self) : nil)
  }

  func getCocoa() -> _NSArrayCoreType {
    _sanityCheck(isCocoa)
    return unsafeBitCast(buffer!, _NSArrayCoreType.self)
  }
}

public struct _ArrayBuffer<T> : _ArrayBufferType {
  var storage: Builtin.NativeObject?

  var indirect: _IndirectArrayBuffer {
    _sanityCheck(_isClassOrObjCExistential(T.self))
    return Builtin.castFromNativeObject(storage!)
  }
  
  public typealias Element = T

  /// create an empty buffer
  public init() {
    storage = !_isClassOrObjCExistential(T.self)
      ? Builtin.castToNativeObject(_emptyArrayStorage) 
      : Builtin.castToNativeObject(
      _IndirectArrayBuffer(
        nativeBuffer: _ContiguousArrayBuffer<T>(),
        isMutable: false,
        needsElementTypeCheck: false
      ))
  }

  public init(_ cocoa: _NSArrayCoreType) {
    _sanityCheck(_isClassOrObjCExistential(T.self))
    storage = Builtin.castToNativeObject(
      _IndirectArrayBuffer(
        cocoa: cocoa,
        // FIXME: it may be possible to avoid a deferred check if we can
        // verify that source is backed by a ContiguousArray<T>.
        needsElementTypeCheck: !(AnyObject.self is T.Type)))
  }

  init(_ buffer: _IndirectArrayBuffer) {
    storage = Builtin.castToNativeObject(buffer)
  }
  
  /// Returns an `_ArrayBuffer<U>` containing the same elements.
  /// Requires: the elements actually have dynamic type `U`, and `U`
  /// is a class or `@objc` existential.
  func castToBufferOf<U>(_: U.Type) -> _ArrayBuffer<U> {
    _sanityCheck(_isClassOrObjCExistential(T.self))
    _sanityCheck(_isClassOrObjCExistential(U.self))
    return _ArrayBuffer<U>(
      _IndirectArrayBuffer(castFrom: self.indirect, toElementType: U.self))
  }
}

extension _ArrayBuffer {
  /// Adopt the storage of source
  public
  init(_ source: NativeBuffer) {
    if !_isClassOrObjCExistential(T.self) {
      self.storage
        = (source._storage != nil) ? Builtin.castToNativeObject(source._storage!) : nil
    }
    else {
      self.storage = Builtin.castToNativeObject(
        _IndirectArrayBuffer(
          nativeBuffer: source, isMutable: true, needsElementTypeCheck: false))
    }
  }
  
  /// Return true iff this buffer's storage is uniquely-referenced.
  mutating func isUniquelyReferenced() -> Bool {
    return _isUniquelyReferenced_native(&storage)
  }

  /// Convert to an NSArray.
  /// Precondition: _isBridgedToObjectiveC(Element.self)
  /// O(1) if the element type is bridged verbatim, O(N) otherwise
  public func _asCocoaArray() -> _NSArrayCoreType {
    _sanityCheck(
      _isBridgedToObjectiveC(T.self),
      "Array element type is not bridged to ObjectiveC")

    return _fastPath(_isNative) ? _native._asCocoaArray() : _nonNative!
  }

  var _hasMutableBuffer: Bool {
    if !_isClassOrObjCExistential(T.self) {
      return true
    }
    return indirect.isMutable && isUniquelyReferencedNonObjC(&indirect.buffer)
  }

  /// If this buffer is backed by a uniquely-referenced mutable
  /// _ContiguousArrayBuffer that can be grown in-place to allow the self
  /// buffer store minimumCapacity elements, returns that buffer.
  /// Otherwise, returns nil
  public
  mutating func requestUniqueMutableBackingBuffer(minimumCapacity: Int)
    -> NativeBuffer?
  {
    if _fastPath(isUniquelyReferenced() && _hasMutableBuffer) {
      let b = _native
      if _fastPath(b.capacity >= minimumCapacity) {
        return b
      }
    }
    return nil
  }

  public
  mutating func isMutableAndUniquelyReferenced() -> Bool {
    return isUniquelyReferenced() && _hasMutableBuffer
  }
  
  /// If this buffer is backed by a `_ContiguousArrayBuffer`
  /// containing the same number of elements as `self`, return it.
  /// Otherwise, return `nil`.
  public func requestNativeBuffer() -> NativeBuffer? {
    if !_isClassOrObjCExistential(T.self) {
      return _native
    }
    else {
      let i = indirect
      return _fastPath(!i.isCocoa)
        ? i.getNativeBufferOf(T.self) 
        : nil
    }
  }

  /// Replace the given subRange with the first newCount elements of
  /// the given collection.
  ///
  /// Requires: this buffer is backed by a uniquely-referenced
  /// _ContiguousArrayBuffer
  public
  mutating func replace<C: CollectionType where C.Generator.Element == Element>(
    #subRange: Range<Int>, with newCount: Int, elementsOf newValues: C
  ) {
    _arrayNonSliceInPlaceReplace(&self, subRange, newCount, newValues)
  }
  
  func _typeCheck(subRange: Range<Int>) {
    if !_isClassOrObjCExistential(T.self) {
      return
    }
    if _slowPath(indirect.needsElementTypeCheck) {
      if _fastPath(_isNative) {
        for x in _native[subRange] {
          _precondition(
            unsafeBitCast(x, AnyObject.self) is T,
            "NSArray element failed to match the Swift Array Element type")
        }
      }
      else if !subRange.isEmpty {
        let ns = _nonNative!
        // Could be sped up, e.g. by using
        // enumerateObjectsAtIndexes:options:usingBlock:
        for i in subRange {
          _precondition(ns.objectAtIndex(i) is T,
            "NSArray element failed to match the Swift Array Element type")
        }
      }
    }
  }
  
  /// Copy the given subRange of this buffer into uninitialized memory
  /// starting at target.  Return a pointer past-the-end of the
  /// just-initialized memory.
  public
  func _uninitializedCopy(subRange: Range<Int>, target: UnsafeMutablePointer<T>)
         -> UnsafeMutablePointer<T> {
    _typeCheck(subRange)
    if _fastPath(_isNative) {
      return _native._uninitializedCopy(subRange, target: target)
    }

    let nonNative = _nonNative!

    let nsSubRange = SwiftShims._SwiftNSRange(
      location:subRange.startIndex,
      length: subRange.endIndex - subRange.startIndex)

    let buffer = unsafeBitCast(target, UnsafeMutablePointer<AnyObject>.self)
    
    // Copies the references out of the NSArray without retaining them
    nonNative.getObjects(buffer, range: nsSubRange)
    
    // Make another pass to retain the copied objects
    var result = target
    for i in subRange {
      result.initialize(result.memory)
      ++result
    }
    return result
  }
  
  /// Return a _SliceBuffer containing the given subRange of values
  /// from this buffer.
  public
  subscript(subRange: Range<Int>) -> _SliceBuffer<T> {
    _typeCheck(subRange)
    
    if _fastPath(_isNative) {
      return _native[subRange]
    }

    let nonNative = self._nonNative

    let subRangeCount = Swift.count(subRange)
    
    // Look for contiguous storage in the NSArray
    let cocoa = _CocoaArrayWrapper(nonNative!)
    let start = cocoa.contiguousStorage(subRange)
    if start != nil {
      return _SliceBuffer(owner: nonNative, start: UnsafeMutablePointer(start),
          count: subRangeCount, hasNativeBuffer: false)
    }
    
    // No contiguous storage found; we must allocate
    var result = _ContiguousArrayBuffer<T>(
        count: subRangeCount, minimumCapacity: 0)

    // Tell Cocoa to copy the objects into our storage
    cocoa.buffer.getObjects(
      UnsafeMutablePointer(result.baseAddress),
      range: _SwiftNSRange(location: subRange.startIndex, length: subRangeCount)
    )

    return _SliceBuffer(result)
  }

  /// If the elements are stored contiguously, a pointer to the first
  /// element. Otherwise, nil.
  public
  var baseAddress: UnsafeMutablePointer<T> {
    if (_fastPath(_isNative)) {
      return _native.baseAddress
    }
    return nil
  }
  
  /// How many elements the buffer stores
  public
  var count: Int {
    get {
      return _fastPath(_isNative) ? _native.count : _nonNative!.count
    }
    set {
      _sanityCheck(_isNative, "attempting to update count of Cocoa array")
      _native.count = newValue
    }
  }
  
  /// Return whether the given `index` is valid for subscripting, i.e. `0
  /// ≤ index < count`
  internal func _isValidSubscript(index : Int) -> Bool {
    if _fastPath(_isNative) {
      /// Note we call through to the native buffer here as it has a more
      /// optimal implementation than just doing 'index < count'
      return _native._isValidSubscript(index)
    }
    return index >= 0 && index < count
  }

  /// How many elements the buffer can store without reallocation
  public
  var capacity: Int {
    return _fastPath(_isNative) ? _native.capacity : _nonNative!.count
  }

  /// Get/set the value of the ith element
  public
  subscript(i: Int) -> T {
    get {
      _typeCheck(i...i)
      if _fastPath(_isNative) {
        return _native[i]
      }
      return unsafeBitCast(_nonNative!.objectAtIndex(i), T.self)
    }
    
    nonmutating set {
      if _fastPath(_hasMutableBuffer) {
        _native[i] = newValue
      }
      else {
        indirect.replaceStorage(_copyCollectionToNativeArrayBuffer(self))
        _native[i] = newValue
      }
    }
  }

  /// Call `body(p)`, where `p` is an `UnsafeBufferPointer` over the
  /// underlying contiguous storage.  If no such storage exists, it is
  /// created on-demand.
  public
  func withUnsafeBufferPointer<R>(
    body: (UnsafeBufferPointer<Element>)->R
  ) -> R {
    if _isClassOrObjCExistential(T.self) {
      if _nonNative != nil {
        indirect.replaceStorage(_copyCollectionToNativeArrayBuffer(self))
      }
    }
    let ret = body(UnsafeBufferPointer(start: self.baseAddress, count: count))
    _fixLifetime(self)
    return ret
  }
  
  /// Call `body(p)`, where `p` is an `UnsafeMutableBufferPointer`
  /// over the underlying contiguous storage.  Requires: such
  /// contiguous storage exists or the buffer is empty
  public
  mutating func withUnsafeMutableBufferPointer<R>(
    body: (UnsafeMutableBufferPointer<T>)->R
  ) -> R {
    _sanityCheck(
      baseAddress != nil || count == 0,
      "Array is bridging an opaque NSArray; can't get a pointer to the elements"
    )
    let ret = body(
      UnsafeMutableBufferPointer(start: baseAddress, count: count))
    _fixLifetime(self)
    return ret
  }
  
  /// An object that keeps the elements stored in this buffer alive
  public
  var owner: AnyObject? {
    return _fastPath(_isNative) ? _native._storage : _nonNative!
  }
  
  /// A value that identifies the storage used by the buffer.  Two
  /// buffers address the same elements when they have the same
  /// identity and count.
  public var identity: UnsafePointer<Void> {
    if _isNative {
      return _native.identity
    }
    else if let cocoa = _nonNative {
      return unsafeAddressOf(cocoa)
    }
    else {
      return nil
    }
  }
  
  //===--- CollectionType conformance -------------------------------------===//
  /// The position of the first element in a non-empty collection.
  ///
  /// Identical to `endIndex` in an empty collection.
  public
  var startIndex: Int {
    return 0
  }

  /// The collection's "past the end" position.
  ///
  /// `endIndex` is not a valid argument to `subscript`, and is always
  /// reachable from `startIndex` by zero or more applications of
  /// `successor()`.
  public var endIndex: Int {
    return count
  }

  /// Return a *generator* over the elements of this *sequence*.
  ///
  /// Complexity: O(1)
  public func generate() -> IndexingGenerator<_ArrayBuffer> {
    return IndexingGenerator(self)
  }
  
  //===--- private --------------------------------------------------------===//
  typealias Storage = _ContiguousArrayStorage<T>
  typealias NativeBuffer = _ContiguousArrayBuffer<T>

  func _invariantCheck() -> Bool {
    return true
  }

  var _isNative: Bool {
    if !_isClassOrObjCExistential(T.self) {
      return true
    }
    else {
      return !indirect.isCocoa
    }
  }

  /// Our native representation, if any.  If there's no native
  /// representation, the result is an empty buffer.
  typealias _OptStorage = _ContiguousArrayStorage<T>?
  var _native: NativeBuffer {
    if !_isClassOrObjCExistential(T.self) {
      return NativeBuffer(
        unsafeBitCast(storage, _OptStorage.self))
    }
    else {
      let i = indirect
      return _fastPath(!i.isCocoa)
        ? i.getNativeBufferOf(T.self) 
        : NativeBuffer()
    }
  }

  var _nonNative: _NSArrayCoreType? {
    if !_isClassOrObjCExistential(T.self) {
      return nil
    }
    else {
      let i = indirect
      return _fastPath(!i.isCocoa) ? nil : i.getCocoa()
    }
  }
}

