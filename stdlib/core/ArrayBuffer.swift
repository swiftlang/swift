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

@final
class IndirectArrayBuffer {
  
  init<T>(nativeBuffer: ContiguousArrayBuffer<T>, isMutable: Bool) {
    self.buffer = nativeBuffer.storage
    self.isMutable = isMutable
    self.isCocoa = false
  }
    
  init(cocoa: CocoaArray) {
    self.buffer = cocoa
    self.isMutable = false
    self.isCocoa = true
  }

  init(castFrom: IndirectArrayBuffer, castKind: _ArrayCastKind) {
    self.buffer = castFrom.buffer
    self.isMutable = castKind == .Down
    self.isCocoa = castFrom.isCocoa
  }
  
  // When this buffer has immutable storage and it is modified, the
  // storage is replaced with mutable storage.
  func replaceStorage<T>(buffer: ContiguousArrayBuffer<T>) {
    self.buffer = buffer.storage
    self.isMutable = true
    self.isCocoa = false
  }

  var buffer: AnyObject?
  var isMutable: Bool
  var isCocoa: Bool
  
  func getNative<T>() -> ContiguousArrayBuffer<T> {
    _sanityCheck(!isCocoa)
    return ContiguousArrayBuffer(
      buffer ? reinterpretCast(buffer) as ContiguousArrayStorage<T> : nil)
  }

  func getCocoa() -> CocoaArray {
    _sanityCheck(isCocoa)
    return reinterpretCast(buffer!) as CocoaArray
  }
}

struct ArrayBuffer<T> : ArrayBufferType {
  var storage: Builtin.NativeObject?

  var indirect: IndirectArrayBuffer {
    _sanityCheck(_isClassOrObjCExistential(T.self))
    return Builtin.castFromNativeObject(storage!)
  }
  
  typealias Element = T

  /// create an empty buffer
  init() {
    storage = !_isClassOrObjCExistential(T.self)
      ? nil : Builtin.castToNativeObject(
      IndirectArrayBuffer(
        nativeBuffer: ContiguousArrayBuffer<T>(),
        isMutable: false))
  }

  init(_ cocoa: CocoaArray) {
    _sanityCheck(_isClassOrObjCExistential(T.self))
    storage = Builtin.castToNativeObject(IndirectArrayBuffer(cocoa: cocoa))
  }

  /// An ArrayBuffer<T> containing the same elements.
  /// Requires: the elements actually have dynamic type T.
  init<U>(castFrom: ArrayBuffer<U>, castKind: _ArrayCastKind) {
    _sanityCheck(_isClassOrObjCExistential(T.self))
    _sanityCheck(_isClassOrObjCExistential(U.self))
    storage = Builtin.castToNativeObject(
      IndirectArrayBuffer(castFrom: castFrom.indirect, castKind: castKind))
  }
  
  var dynamicElementType: Any.Type {
    return _isNative ? _native.dynamicElementType : AnyObject.self
  }
}

extension ArrayBuffer {
  /// Adopt the storage of x
  init(_ buffer: NativeBuffer) {
    if !_isClassOrObjCExistential(T.self) {
      self.storage
        = buffer.storage ? Builtin.castToNativeObject(buffer.storage!) : nil
    }
    else {
      self.storage = Builtin.castToNativeObject(
        IndirectArrayBuffer(nativeBuffer: buffer, isMutable: true))
    }
  }
  
  /// Return true iff this buffer's storage is uniquely-referenced.
  mutating func isUniquelyReferenced() -> Bool {
    return Swift.isUniquelyReferenced(&storage)
  }

  /// Convert to an NSArray.
  /// Precondition: isBridgedToObjectiveC(Element.self)
  /// O(1) if the element type is bridged verbatim, O(N) otherwise
  func asCocoaArray() -> CocoaArray {
    _sanityCheck(
      isBridgedToObjectiveC(T.self),
      "Array element type is not bridged to ObjectiveC")

    return _fastPath(_isNative) ? _native.asCocoaArray() : _nonNative!
  }

  var _hasMutableBuffer: Bool {
    if !_isClassOrObjCExistential(T.self) {
      return true
    }
    return indirect.isMutable && Swift.isUniquelyReferenced(&indirect.buffer)
  }
  
  /// If this buffer is backed by a uniquely-referenced mutable
  /// ContiguousArrayBuffer that can be grown in-place to allow the self
  /// buffer store minimumCapacity elements, returns that buffer.
  /// Otherwise, returns nil
  mutating func requestUniqueMutableBuffer(minimumCapacity: Int)
    -> NativeBuffer?
  {
    if _fastPath(Swift.isUniquelyReferenced(&storage) && _hasMutableBuffer) {
      let b = _native
      return b.capacity >= minimumCapacity ? b : nil
    }
    return nil
  }
  
  /// If this buffer is backed by a ContiguousArrayBuffer, return it.
  /// Otherwise, return nil.  Note: the result's elementStorage may
  /// not match ours, if we are a SliceBuffer.
  func requestNativeBuffer() -> NativeBuffer? {
    let result = self._native
    if result { return result }
    return nil
  }
  
  /// Copy the given subRange of this buffer into uninitialized memory
  /// starting at target.  Return a pointer past-the-end of the
  /// just-initialized memory.
  func _uninitializedCopy(
    subRange: Range<Int>, target: UnsafePointer<T>
  ) -> UnsafePointer<T> {
    if _fastPath(_isNative) {
      return _native._uninitializedCopy(subRange, target: target)
    }

    let nonNative = _nonNative!

    let nsSubRange = SwiftShims._SwiftNSRange(
      location:subRange.startIndex,
      length: subRange.endIndex - subRange.startIndex)

    let buffer = reinterpretCast(target) as UnsafePointer<AnyObject>
    
    // Copies the references out of the NSArray without retaining them
    nonNative.getObjects(buffer, range: nsSubRange)
    
    // Make another pass to retain the copied objects
    var result = target
    for i in subRange {
      result.initialize(result.pointee)
      ++result
    }
    return result
  }
  
  /// Return a SliceBuffer containing the given subRange of values
  /// from this buffer.
  subscript(subRange: Range<Int>) -> SliceBuffer<T> {
    
    if _fastPath(_isNative) {
      return _native[subRange]
    }

    let nonNative = self._nonNative

    let subRangeCount = countElements(subRange)
    
    // Look for contiguous storage in the NSArray
    let cocoa = CocoaArrayWrapper(nonNative!)
    let start = cocoa.contiguousStorage(subRange)
    if start != nil {
      return SliceBuffer(owner: nonNative, start: UnsafePointer(start),
                         count: subRangeCount, hasNativeBuffer: false)
    }
    
    // No contiguous storage found; we must allocate
    var result = ContiguousArrayBuffer<T>(count: subRangeCount, minimumCapacity: 0)

    // Tell Cocoa to copy the objects into our storage
    cocoa.buffer.getObjects(
      UnsafePointer(result.elementStorage),
      range: _SwiftNSRange(location: subRange.startIndex, length: subRangeCount)
    )
    
    return SliceBuffer(result)
  }

  /// If the elements are stored contiguously, a pointer to the first
  /// element. Otherwise, nil.
  var elementStorage: UnsafePointer<T> {
    if (_fastPath(_isNative)) {
      return _native.elementStorage
    }
    return nil
  }
  
  /// How many elements the buffer stores
  var count: Int {
    get {
      return _fastPath(_isNative) ? _native.count : _nonNative!.count
    }
    set {
      // Allow zero here for the case where elements have been moved
      // out of the buffer during reallocation
      _sanityCheck(
        newValue == 0 || newValue >= count,
        "We don't yet know how to shrink an array")
      
      _sanityCheck(_isNative, "attempting to update count of Cocoa array")
      _native.count = newValue
    }
  }
  
  /// How many elements the buffer can store without reallocation
  var capacity: Int {
    return _fastPath(_isNative) ? _native.capacity : _nonNative!.count
  }

  /// Get/set the value of the ith element
  subscript(i: Int) -> T {
    get {
      if _fastPath(_isNative) {
        return _native[i]
      }
      // FIXME: Should be possible to use "as" + "!" instead of
      // reinterpretCast; <rdar://problem/16898943>
      return reinterpretCast(_nonNative!.objectAtIndex(i))
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

  /// An object that keeps the elements stored in this buffer alive
  var owner: AnyObject? {
    return _fastPath(_isNative) ? _native.storage : _nonNative!
  }
  
  /// A value that identifies first mutable element, if any.  Two
  /// arrays compare === iff they are both empty or if their buffers
  /// have the same identity and count.
  var identity: Word {
    let p = elementStorage
    return p != nil ? reinterpretCast(p) : reinterpretCast(owner)
  }
  
  //===--- Collection conformance -----------------------------------------===//
  var startIndex: Int {
    return 0
  }
  
  var endIndex: Int {
    return count
  }

  func generate() -> IndexingGenerator<ArrayBuffer> {
    return IndexingGenerator(self)
  }
  
  //===--- private --------------------------------------------------------===//
  typealias Storage = ContiguousArrayStorage<T>
  typealias NativeBuffer = ContiguousArrayBuffer<T>

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
  var _native: NativeBuffer {
    if !_isClassOrObjCExistential(T.self) {
      return NativeBuffer(
        reinterpretCast(storage) as ContiguousArrayStorage<T>?)
    }
    else {
      let i = indirect
      return _fastPath(!i.isCocoa) ? i.getNative() : NativeBuffer()
    }
  }

  var _nonNative: CocoaArray? {
    if !_isClassOrObjCExistential(T.self) {
      return nil
    }
    else {
      let i = indirect
      return _fastPath(!i.isCocoa) ? nil : i.getCocoa()
    }
  }
}

