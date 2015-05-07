//===--- ArrayBridge.swift - Array<T> <=> NSArray bridging ----------------===//
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

import SwiftShims

/// Class used whose sole instance is used as storage for empty
/// arrays.  The instance is defined in the runtime and statically
/// initialized.  See stdlib/runtime/GlobalObjects.cpp for details.
/// Because it's statically referenced, it requires nonlazy realization
/// by the Objective-C runtime.
@objc_non_lazy_realization
internal final class _EmptyArrayStorage
  : _ContiguousArrayStorageBase {

  init(_doNotCallMe: ()) {
    _sanityCheckFailure("creating instance of _EmptyArrayStorage")
  }
  
  var countAndCapacity: _ArrayBody

#if _runtime(_ObjC)
  override func _withVerbatimBridgedUnsafeBuffer<R>(
    @noescape body: (UnsafeBufferPointer<AnyObject>) -> R
  ) -> R? {
    return body(UnsafeBufferPointer(start: nil, count: 0))
  }

  override func _getNonVerbatimBridgedCount(dummy: Void) -> Int {
    return 0
  }

  override func _getNonVerbatimBridgedHeapBuffer(
    dummy: Void
  ) -> _HeapBuffer<Int, AnyObject> {
    return _HeapBuffer<Int, AnyObject>(
      _HeapBufferStorage<Int, AnyObject>.self, 0, 0)
  }
#endif

  override func canStoreElementsOfDynamicType(_: Any.Type) -> Bool {
    return false
  }

  /// A type that every element in the array is.
  override var staticElementType: Any.Type {
    return Void.self
  }
}

/// The empty array prototype.  We use the same object for all empty
/// [Native]Array<T>s.
internal var _emptyArrayStorage : _EmptyArrayStorage {
  return Builtin.bridgeFromRawPointer(
    Builtin.addressof(&_swiftEmptyArrayStorage))
}

// FIXME: This whole class is a workaround for
// <rdar://problem/18560464> Can't override generic method in generic
// subclass.  If it weren't for that bug, we'd override
// _withVerbatimBridgedUnsafeBuffer directly in
// _ContiguousArrayStorage<T>.
class _ContiguousArrayStorage1 : _ContiguousArrayStorageBase {
#if _runtime(_ObjC)
  /// If the `T` is bridged verbatim, invoke `body` on an
  /// `UnsafeBufferPointer` to the elements and return the result.
  /// Otherwise, return `nil`.
  final override func _withVerbatimBridgedUnsafeBuffer<R>(
    @noescape body: (UnsafeBufferPointer<AnyObject>) -> R
  ) -> R? {
    var result: R? = nil
    self._withVerbatimBridgedUnsafeBufferImpl {
      result = body($0)
    }
    return result
  }

  /// If `T` is bridged verbatim, invoke `body` on an
  /// `UnsafeBufferPointer` to the elements.
  internal func _withVerbatimBridgedUnsafeBufferImpl(
    @noescape body: (UnsafeBufferPointer<AnyObject>) -> Void
  ) {
    _sanityCheckFailure(
      "Must override _withVerbatimBridgedUnsafeBufferImpl in derived classes")
  }
#endif
}

// The class that implements the storage for a ContiguousArray<T>
final class _ContiguousArrayStorage<T> : _ContiguousArrayStorage1 {

  deinit {
    __manager._elementPointer.destroy(__manager._valuePointer.memory.count)
    __manager._valuePointer.destroy()
    _fixLifetime(__manager)
  }

#if _runtime(_ObjC)
  /// If `T` is bridged verbatim, invoke `body` on an
  /// `UnsafeBufferPointer` to the elements.
  internal final override func _withVerbatimBridgedUnsafeBufferImpl(
    @noescape body: (UnsafeBufferPointer<AnyObject>) -> Void
  ) {
    if _isBridgedVerbatimToObjectiveC(T.self) {
      let count = __manager.value.count
      let elements = UnsafePointer<AnyObject>(__manager._elementPointer)
      body(UnsafeBufferPointer(start: elements, count: count))
      _fixLifetime(__manager)
    }
  }

  /// Returns the number of elements in the array.
  ///
  /// - precondition: `T` is bridged non-verbatim.
  override internal func _getNonVerbatimBridgedCount(dummy: Void) -> Int {
    _sanityCheck(
      !_isBridgedVerbatimToObjectiveC(T.self),
      "Verbatim bridging should be handled separately")
    return __manager.value.count
  }

  /// Bridge array elements and return a new buffer that owns them.
  ///
  /// - precondition: `T` is bridged non-verbatim.
  override internal func _getNonVerbatimBridgedHeapBuffer(dummy: Void) ->
    _HeapBuffer<Int, AnyObject> {
    _sanityCheck(
      !_isBridgedVerbatimToObjectiveC(T.self),
      "Verbatim bridging should be handled separately")
    let count = __manager.value.count
    let result = _HeapBuffer<Int, AnyObject>(
      _HeapBufferStorage<Int, AnyObject>.self, count, count)
    let resultPtr = result.baseAddress
    let p = __manager._elementPointer
    for i in 0..<count {
      (resultPtr + i).initialize(_bridgeToObjectiveCUnconditional(p[i]))
    }
    _fixLifetime(__manager)
    return result
  }
#endif

  /// Return true if the `proposedElementType` is `T` or a subclass of
  /// `T`.  We can't store anything else without violating type
  /// safety; for example, the destructor has static knowledge that
  /// all of the elements can be destroyed as `T`
  override func canStoreElementsOfDynamicType(
    proposedElementType: Any.Type
  ) -> Bool {
#if _runtime(_ObjC)
    return proposedElementType is T.Type
#else
    // FIXME: Dynamic casts don't currently work without objc. 
    // rdar://problem/18801510
    return false
#endif
  }

  /// A type that every element in the array is.
  override var staticElementType: Any.Type {
    return T.self
  }

  internal // private
  typealias Manager = ManagedBufferPointer<_ArrayBody, T>

  internal // private
  var __manager : Manager {
    return Manager(_uncheckedUnsafeBufferObject: self)
  }
}

public struct _ContiguousArrayBuffer<T> : _ArrayBufferType {

  /// Make a buffer with uninitialized elements.  After using this
  /// method, you must either initialize the count elements at the
  /// result's .baseAddress or set the result's .count to zero.
  public init(count: Int, minimumCapacity: Int)
  {
    let realMinimumCapacity = max(count, minimumCapacity)
    if realMinimumCapacity == 0 {
      self = _ContiguousArrayBuffer<T>()
    }
    else {
      __bufferPointer = ManagedBufferPointer(
        _uncheckedBufferClass: _ContiguousArrayStorage<T>.self,
        minimumCapacity: realMinimumCapacity)

#if _runtime(_ObjC)
      let verbatim = _isBridgedVerbatimToObjectiveC(T.self)
#else
      let verbatim = false
#endif

      __bufferPointer._valuePointer.initialize(
        _ArrayBody(
          count: count,
          capacity: __bufferPointer.allocatedElementCount,
          elementTypeIsBridgedVerbatim: verbatim))
      
      _fixLifetime(__bufferPointer)
    }
  }

  init(_ storage: _ContiguousArrayStorageBase) {
    __bufferPointer = ManagedBufferPointer(
      _uncheckedUnsafeBufferObject: storage)
  }

  var arrayPropertyIsNative : Bool {
    return true
  }

  /// True, if the array is native and does not need a deferred type check.
  var arrayPropertyIsNativeNoTypeCheck : Bool {
    return true
  }

  /// If the elements are stored contiguously, a pointer to the first
  /// element. Otherwise, nil.
  public var baseAddress: UnsafeMutablePointer<T> {
    return __bufferPointer._elementPointer
  }

  /// Call `body(p)`, where `p` is an `UnsafeBufferPointer` over the
  /// underlying contiguous storage.
  public func withUnsafeBufferPointer<R>(
    @noescape body: UnsafeBufferPointer<Element> -> R
  ) -> R {
    let ret = body(UnsafeBufferPointer(start: self.baseAddress, count: count))
    _fixLifetime(self)
    return ret
  }

  /// Call `body(p)`, where `p` is an `UnsafeMutableBufferPointer`
  /// over the underlying contiguous storage.
  public mutating func withUnsafeMutableBufferPointer<R>(
    @noescape body: UnsafeMutableBufferPointer<T> -> R
  ) -> R {
    let ret = body(
      UnsafeMutableBufferPointer(start: baseAddress, count: count))
    _fixLifetime(self)
    return ret
  }

  internal func _getBaseAddress() -> UnsafeMutablePointer<T> {
    return baseAddress
  }

  //===--- _ArrayBufferType conformance -----------------------------------===//
  /// The type of elements stored in the buffer
  public typealias Element = T

  /// create an empty buffer
  public init() {
    __bufferPointer = ManagedBufferPointer(
      _uncheckedUnsafeBufferObject: _emptyArrayStorage)
  }

  /// Adopt the storage of x
  public init(_ buffer: _ContiguousArrayBuffer) {
    self = buffer
  }

  public mutating func requestUniqueMutableBackingBuffer(minimumCapacity: Int)
    -> _ContiguousArrayBuffer<Element>?
  {
    if _fastPath(isUniquelyReferenced() && capacity >= minimumCapacity) {
      return self
    }
    return nil
  }

  public mutating func isMutableAndUniquelyReferenced() -> Bool {
    return isUniquelyReferenced()
  }

  public mutating func isMutableAndUniquelyReferencedOrPinned() -> Bool {
    return isUniquelyReferencedOrPinned()
  }

  /// If this buffer is backed by a `_ContiguousArrayBuffer`
  /// containing the same number of elements as `self`, return it.
  /// Otherwise, return `nil`.
  public func requestNativeBuffer() -> _ContiguousArrayBuffer<Element>? {
    return self
  }

  /// Replace the given subRange with the first newCount elements of
  /// the given collection.
  ///
  /// Requires: this buffer is backed by a uniquely-referenced
  /// _ContiguousArrayBuffer
  public mutating func replace<
    C: CollectionType where C.Generator.Element == Element
  >(
    subRange subRange: Range<Int>, with newCount: Int, elementsOf newValues: C
  ) {
    _arrayNonSliceInPlaceReplace(&self, subRange, newCount, newValues)
  }

  func getElement(i: Int, hoistedIsNativeNoTypeCheckBuffer: Bool) -> T {
    _sanityCheck(
      _isValidSubscript(i,
          hoistedIsNativeBuffer: hoistedIsNativeNoTypeCheckBuffer),
      "Array index out of range")
    // If the index is in bounds, we can assume we have storage.
    return baseAddress[i]
  }

  /// Get/set the value of the ith element
  public subscript(i: Int) -> T {
    get {
      return getElement(i, hoistedIsNativeNoTypeCheckBuffer: true)
    }
    nonmutating set {
      _sanityCheck(i >= 0 && i < count, "Array index out of range")
      // If the index is in bounds, we can assume we have storage.

      // FIXME: Manually swap because it makes the ARC optimizer happy.  See
      // <rdar://problem/16831852> check retain/release order
      // baseAddress[i] = newValue
      var nv = newValue
      let tmp = nv
      nv = baseAddress[i]
      baseAddress[i] = tmp
    }
  }

  /// How many elements the buffer stores
  public var count: Int {
    get {
      return __bufferPointer.value.count
    }
    nonmutating set {
      _sanityCheck(newValue >= 0)

      _sanityCheck(
        newValue <= capacity,
        "Can't grow an array buffer past its capacity")

      __bufferPointer._valuePointer.memory.count = newValue
    }
  }

  /// Return whether the given `index` is valid for subscripting, i.e. `0
  /// ≤ index < count`
  func _isValidSubscript(index : Int, hoistedIsNativeBuffer : Bool) -> Bool {
    /// Instead of returning 0 for no storage, we explicitly check
    /// for the existance of storage.
    /// Note that this is better than folding hasStorage in to
    /// the return from this function, as this implementation generates
    /// no shortcircuiting blocks.
    return (index >= 0) && (index < __bufferPointer.value.count)
  }

  /// Return whether the given `index` is valid for subscripting, i.e. `0
  /// ≤ index < count`
  ///
  /// For ContiguousArrayBuffer, this is equivalent to the
  /// `_isValidSubscript(_:hoistedIsNativeBuffer:)` form, but is necessary
  /// for interface parity with `ArrayBuffer`.
  @inline(__always)
  func _isValidSubscript(index : Int, hoistedIsNativeNoTypeCheckBuffer : Bool)
      -> Bool {
    return _isValidSubscript(index,
      hoistedIsNativeNoTypeCheckBuffer : hoistedIsNativeNoTypeCheckBuffer)
  }

  /// How many elements the buffer can store without reallocation
  public var capacity: Int {
    return __bufferPointer.value.capacity
  }

  /// Copy the given subRange of this buffer into uninitialized memory
  /// starting at target.  Return a pointer past-the-end of the
  /// just-initialized memory.
  public func _uninitializedCopy(
    subRange: Range<Int>, target: UnsafeMutablePointer<T>
  ) -> UnsafeMutablePointer<T> {
    _sanityCheck(subRange.startIndex >= 0)
    _sanityCheck(subRange.endIndex >= subRange.startIndex)
    _sanityCheck(subRange.endIndex <= count)

    let c = subRange.endIndex - subRange.startIndex
    target.initializeFrom(baseAddress + subRange.startIndex, count: c)
    _fixLifetime(owner)
    return target + c
  }

  /// Return a _SliceBuffer containing the given subRange of values
  /// from this buffer.
  public subscript(subRange: Range<Int>) -> _SliceBuffer<T>
  {
    return _SliceBuffer(
      owner: __bufferPointer.buffer,
      start: baseAddress + subRange.startIndex,
      count: subRange.endIndex - subRange.startIndex,
      hasNativeBuffer: true)
  }

  /// Return true iff this buffer's storage is uniquely-referenced.
  /// NOTE: this does not mean the buffer is mutable.  Other factors
  /// may need to be considered, such as whether the buffer could be
  /// some immutable Cocoa container.
  public mutating func isUniquelyReferenced() -> Bool {
    return __bufferPointer.holdsUniqueReference()
  }

  /// Return true iff this buffer's storage is either
  /// uniquely-referenced or pinned.  NOTE: this does not mean
  /// the buffer is mutable; see the comment on isUniquelyReferenced.
  public mutating func isUniquelyReferencedOrPinned() -> Bool {
    return __bufferPointer.holdsUniqueOrPinnedReference()
  }

#if _runtime(_ObjC)
  /// Convert to an NSArray.
  /// - precondition: T is bridged to Objective-C
  /// O(1).
  public func _asCocoaArray() -> _NSArrayCoreType {
    _sanityCheck(
        _isBridgedToObjectiveC(T.self),
        "Array element type is not bridged to Objective-C")
    if count == 0 {
      return _SwiftDeferredNSArray(
        _nativeStorage: _emptyArrayStorage)
    }
    return _SwiftDeferredNSArray(_nativeStorage: _storage)
  }
#endif

  /// An object that keeps the elements stored in this buffer alive
  public var owner: AnyObject {
    return _storage
  }

  /// An object that keeps the elements stored in this buffer alive
  public var nativeOwner: AnyObject {
    return _storage
  }

  /// A value that identifies the storage used by the buffer.  Two
  /// buffers address the same elements when they have the same
  /// identity and count.
  public var identity: UnsafePointer<Void> {
    return withUnsafeBufferPointer { UnsafePointer($0.baseAddress) }
  }
  
  /// Return true iff we have storage for elements of the given
  /// `proposedElementType`.  If not, we'll be treated as immutable.
  func canStoreElementsOfDynamicType(proposedElementType: Any.Type) -> Bool {
    return _storage.canStoreElementsOfDynamicType(proposedElementType)
  }

  /// Return true if the buffer stores only elements of type `U`.
  /// Requires: `U` is a class or `@objc` existential. O(N)
  func storesOnlyElementsOfType<U>(
    _: U.Type
  ) -> Bool {
    _sanityCheck(_isClassOrObjCExistential(U.self))
    
    // Start with the base class so that optimizations based on
    // 'final' don't bypass dynamic type check.
    let s: _ContiguousArrayStorageBase = _storage
    
    if _fastPath(s.staticElementType is U.Type) {
      // Done in O(1)
      return true
    }

    // Check the elements
    for x in self {
      if !(x is U) {
        return false
      }
    }
    return true
  }

  internal var _storage: _ContiguousArrayStorageBase {
    return Builtin.castFromNativeObject(__bufferPointer._nativeBuffer)
  }

  var __bufferPointer: ManagedBufferPointer<_ArrayBody, T>
}

/// Append the elements of rhs to lhs
public func += <
  T, C: CollectionType where C.Generator.Element == T
> (inout lhs: _ContiguousArrayBuffer<T>, rhs: C) {
  let oldCount = lhs.count
  let newCount = oldCount + numericCast(rhs.count())

  if _fastPath(newCount <= lhs.capacity) {
    lhs.count = newCount
    (lhs.baseAddress + oldCount).initializeFrom(rhs)
  }
  else {
    var newLHS = _ContiguousArrayBuffer<T>(
      count: newCount,
      minimumCapacity: _growArrayCapacity(lhs.capacity))

    newLHS.baseAddress.moveInitializeFrom(lhs.baseAddress, count: oldCount)
    lhs.count = 0
    swap(&lhs, &newLHS)
    (lhs.baseAddress + oldCount).initializeFrom(rhs)
  }
}

extension _ContiguousArrayBuffer : CollectionType {
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
  /// `successor()`.
  public var endIndex: Int {
    return count
  }
  
  /// Return a *generator* over the elements of this *sequence*.
  ///
  /// - complexity: O(1)
  public func generate() -> IndexingGenerator<_ContiguousArrayBuffer> {
    return IndexingGenerator(self)
  }
}

public func ~> <
  S : _Sequence_Type
>(
  source: S, _: (_CopyToNativeArrayBuffer,())
) -> _ContiguousArrayBuffer<S.Generator.Element>
{
  let initialCapacity = source.underestimateCount()
  var result = _ContiguousArrayBuffer<S.Generator.Element>(
    count: initialCapacity, minimumCapacity: initialCapacity)

  var generator = source.generate()

  var p = result.baseAddress
  for _ in 0..<initialCapacity {
    (p++).initialize(generator.next()!)
  }

  while let element = generator.next() {
    result += CollectionOfOne(element)
  }

  return result
}

public func ~> <
  C : CollectionType
>(
  source: C, _:(_CopyToNativeArrayBuffer, ())
) -> _ContiguousArrayBuffer<C.Generator.Element>
{
  return _copyCollectionToNativeArrayBuffer(source)
}

func _copyCollectionToNativeArrayBuffer<
  C : CollectionType
>(source: C) -> _ContiguousArrayBuffer<C.Generator.Element>
{
  let count: Int = numericCast(source.count())
  if count == 0 {
    return _ContiguousArrayBuffer()
  }

  var result = _ContiguousArrayBuffer<C.Generator.Element>(
    count: numericCast(count),
    minimumCapacity: 0
  )

  var p = result.baseAddress
  var i = source.startIndex
  for _ in 0..<count {
    (p++).initialize(source[i++])
  }
  _expectEnd(i, source)
  return result
}
