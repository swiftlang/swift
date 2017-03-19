//===--- ArrayBufferProtocol.swift ----------------------------------------===//
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


// FIXME: these should all be internal, not public. Needs to be public to use in String prototype.

public protocol _PointerFunction {
  associatedtype Element
  func call(_: UnsafeMutablePointer<Element>, count: Int)
}

public struct _IgnorePointer<T> : _PointerFunction {
  @_versioned
  @_inlineable
  init() {}

  public func call(_: UnsafeMutablePointer<T>, count: Int) {
    _sanityCheck(count == 0)
  }
}

public struct _InitializeMemoryFromCollection<
  C: Collection
> : _PointerFunction {
  public func call(_ rawMemory: UnsafeMutablePointer<C.Iterator.Element>, count: Int) {
    var p = rawMemory
    var q = newValues.startIndex
    for _ in 0..<count {
      p.initialize(to: newValues[q])
      newValues.formIndex(after: &q)
      p += 1
    }
    _expectEnd(of: newValues, is: q)
  }

  init(_ newValues: C) {
    self.newValues = newValues
  }

  var newValues: C
}

// Just a quick little thing to abstract over _ContiguousArrayBuffer 
// and String's native buffer
// @_versioned
public protocol _ContiguousBufferProtocol {
  associatedtype Element

  var count: Int { get nonmutating set }
  var firstElementAddress: UnsafeMutablePointer<Element> { get }

  init(_uninitializedCount: Int, minimumCapacity: Int)
}

extension _ContiguousArrayBuffer: _ContiguousBufferProtocol { }

/// The underlying buffer for an ArrayType conforms to
/// `_ArrayBufferProtocol`.  This buffer does not provide value semantics.
// @_versioned
public protocol _ArrayBufferProtocol
  : MutableCollection, RandomAccessCollection {

  associatedtype Indices : RandomAccessCollection = CountableRange<Int>

  /// The type of elements stored in the buffer.
  associatedtype Element
  associatedtype Buffer: _ContiguousBufferProtocol
  associatedtype SliceBuffer

  /// Create an empty buffer.
  init()

  /// Adopt the entire buffer, presenting it at the provided `startIndex`.
  init(_buffer: Buffer, shiftedToStartIndex: Int)

  /// Copy the elements in `bounds` from this buffer into uninitialized
  /// memory starting at `target`.  Return a pointer "past the end" of the
  /// just-initialized memory.
  @discardableResult
  func _copyContents(
    subRange bounds: Range<Int>,
    initializing target: UnsafeMutablePointer<Element>
  ) -> UnsafeMutablePointer<Element>

  /// Get or set the index'th element.
  subscript(index: Int) -> Element { get nonmutating set }

  /// If this buffer is backed by a uniquely-referenced mutable
  /// `_ContiguousArrayBuffer` that can be grown in-place to allow the `self`
  /// buffer store `minimumCapacity` elements, returns that buffer.
  /// Otherwise, returns `nil`.
  ///
  /// - Note: The result's firstElementAddress may not match ours, if we are a
  ///   _SliceBuffer.
  ///
  /// - Note: This function must remain mutating; otherwise the buffer
  ///   may acquire spurious extra references, which will cause
  ///   unnecessary reallocation.
  mutating func requestUniqueMutableBackingBuffer(
    minimumCapacity: Int
  ) -> Buffer?

  /// Returns `true` iff this buffer is backed by a uniquely-referenced mutable
  /// _ContiguousArrayBuffer.
  ///
  /// - Note: This function must remain mutating; otherwise the buffer
  ///   may acquire spurious extra references, which will cause
  ///   unnecessary reallocation.
  mutating func isMutableAndUniquelyReferenced() -> Bool

  /// If this buffer is backed by a `_ContiguousArrayBuffer`
  /// containing the same number of elements as `self`, return it.
  /// Otherwise, return `nil`.
  func requestNativeBuffer() -> Buffer?

  /// Subroutine of replaceSubrange that assumes everything is inbounds
  /// and that `isMutableAndUniquelyReferenced()` returned true.
  ///
  /// Slices should override this to do subrange adjustments for their owner
  /// and post-processing of their own range.
  mutating func replaceSubrangeInPlace<C>(
    _ subrange: Range<Int>,
    with newValues: C,
    insertCount: Int
  ) where 
    C : Collection, 
    C.Iterator.Element == Element

  /// Returns a `_SliceBuffer` containing the elements in `bounds`.
  subscript(bounds: Range<Int>) -> SliceBuffer { get }

  /// Call `body(p)`, where `p` is an `UnsafeBufferPointer` over the
  /// underlying contiguous storage.  If no such storage exists, it is
  /// created on-demand.
  func withUnsafeBufferPointer<R>(
    _ body: (UnsafeBufferPointer<Element>) throws -> R
  ) rethrows -> R

  /// Call `body(p)`, where `p` is an `UnsafeMutableBufferPointer`
  /// over the underlying contiguous storage.
  ///
  /// - Precondition: Such contiguous storage exists or the buffer is empty.
  mutating func withUnsafeMutableBufferPointer<R>(
    _ body: (UnsafeMutableBufferPointer<Element>) throws -> R
  ) rethrows -> R

  /// The number of elements the buffer stores.
  var count: Int { get set }

  /// The number of elements the buffer can store without reallocation.
  var capacity: Int { get }

  /// An object that keeps the elements stored in this buffer alive.
  var owner: AnyObject { get }

  /// A pointer to the first element.
  ///
  /// - Precondition: The elements are known to be stored contiguously.
  var firstElementAddress: UnsafeMutablePointer<Element> { get }

  /// If the elements are stored contiguously, a pointer to the first
  /// element. Otherwise, `nil`.
  var firstElementAddressIfContiguous: UnsafeMutablePointer<Element>? { get }

  /// Returns a base address to which you can add an index `i` to get the
  /// address of the corresponding element at `i`.
  var subscriptBaseAddress: UnsafeMutablePointer<Element> { get }

  /// A value that identifies the storage used by the buffer.  Two
  /// buffers address the same elements when they have the same
  /// identity and count.
  var identity: UnsafeRawPointer { get }

  var startIndex: Int { get }
  var endIndex: Int { get }
}

extension _ArrayBufferProtocol 
  where Buffer.Element == Element
{
  @_inlineable
  public var subscriptBaseAddress: UnsafeMutablePointer<Element> {
    return firstElementAddress
  }

  @_inlineable
  public mutating func replaceSubrange<C>(
    _ subrange: Range<Int>,
    with newValues: C
  ) where C : Collection, C.Iterator.Element == Element {

    let oldCount = self.count
    let eraseCount = subrange.count
    let insertCount = numericCast(newValues.count) as Int
    let growth = insertCount - eraseCount
    let newCount = oldCount + growth

    if self.requestUniqueMutableBackingBuffer(
          minimumCapacity: newCount) == nil {
      self._arrayOutOfPlaceReplace(subrange, with: newValues, 
          insertCount: insertCount)
    } else {
      replaceSubrangeInPlace(subrange, 
        with: newValues, insertCount: insertCount)
    }
  }

  @inline(never)
  public mutating func replaceSubrangeInPlace<C>(
    _ subrange: Range<Int>,
    with newValues: C,
    insertCount: Int
  ) where 
    C : Collection, 
    C.Iterator.Element == Element {
    
    _sanityCheck(startIndex == 0, "Slices should override this function.")

    let growth = insertCount - subrange.count
    let elements = self.subscriptBaseAddress
    let oldTailIndex = subrange.upperBound
    let oldTailStart = elements + oldTailIndex
    let newTailIndex = oldTailIndex + growth
    let newTailStart = oldTailStart + growth
    let tailCount = self.count - subrange.upperBound
    self.count += growth

    if growth > 0 {
      // Slide the tail part of the buffer forwards, in reverse order
      // so as not to self-clobber.
      newTailStart.moveInitialize(from: oldTailStart, count: tailCount)

      // Assign over the original subrange
      var i = newValues.startIndex
      for j in CountableRange(subrange) {
        elements[j] = newValues[i]
        newValues.formIndex(after: &i)
      }
      // Initialize the hole left by sliding the tail forward
      for j in oldTailIndex..<newTailIndex {
        (elements + j).initialize(to: newValues[i])
        newValues.formIndex(after: &i)
      }
      _expectEnd(of: newValues, is: i)
    }
    else { // We're not growing the buffer
      // Assign all the new elements into the start of the subrange
      var i = subrange.lowerBound
      var j = newValues.startIndex
      for _ in 0..<insertCount {
        elements[i] = newValues[j]
        i += 1
        newValues.formIndex(after: &j)
      }
      _expectEnd(of: newValues, is: j)

      // If the size didn't change, we're done.
      if growth == 0 {
        return
      }

      // Move the tail backward to cover the shrinkage.
      let shrinkage = -growth
      if tailCount > shrinkage {   // If the tail length exceeds the shrinkage

        // Assign over the rest of the replaced range with the first
        // part of the tail.
        newTailStart.moveAssign(from: oldTailStart, count: shrinkage)

        // Slide the rest of the tail back
        oldTailStart.moveInitialize(
          from: oldTailStart + shrinkage, count: tailCount - shrinkage)
      }
      else {                      // Tail fits within erased elements
        // Assign over the start of the replaced range with the tail
        newTailStart.moveAssign(from: oldTailStart, count: tailCount)

        // Destroy elements remaining after the tail in subrange
        (newTailStart + tailCount).deinitialize(
          count: shrinkage - tailCount)
      }
    }
  }

  @inline(never)
  @_versioned
  internal mutating func _arrayOutOfPlaceReplace<C : Collection>(
    _ bounds: Range<Int>,
    with newValues: C,
    insertCount: Int
  ) where 
    C.Iterator.Element == Element,
    Buffer.Element == Element {

    let growth = insertCount - bounds.count
    let newCount = self.count + growth
    var newBuffer = _forceCreateUniqueMutableBuffer(
      newCount: newCount, requiredCapacity: newCount)

    _arrayOutOfPlaceUpdate(
      &newBuffer,
      bounds.lowerBound - startIndex, insertCount,
      _InitializeMemoryFromCollection(newValues)
    )
  }

  /// Initialize the elements of dest by copying the first headCount
  /// items from source, calling initializeNewElements on the next
  /// uninitialized element, and finally by copying the last N items
  /// from source into the N remaining uninitialized elements of dest.
  ///
  /// As an optimization, may move elements out of source rather than
  /// copying when it isUniquelyReferenced.
  @inline(never)
  @_versioned
  internal mutating func _arrayOutOfPlaceUpdate<Initializer>(
    _ dest: inout Buffer,
    _ headCount: Int, // Count of initial source elements to copy/move
    _ newCount: Int,  // Number of new elements to insert
    _ initializeNewElements: Initializer
  ) where
    Initializer : _PointerFunction,
    Initializer.Element == Element {

    _sanityCheck(headCount >= 0)
    _sanityCheck(newCount >= 0)

    // Count of trailing source elements to copy/move
    let sourceCount = self.count
    let tailCount = dest.count - headCount - newCount
    _sanityCheck(headCount + tailCount <= sourceCount)

    let oldCount = sourceCount - headCount - tailCount
    let destStart = dest.firstElementAddress
    let newStart = destStart + headCount
    let newEnd = newStart + newCount

    // Check to see if we have storage we can move from
    if let backing = requestUniqueMutableBackingBuffer(
      minimumCapacity: sourceCount) {

      let sourceStart = firstElementAddress
      let oldStart = sourceStart + headCount

      // Destroy any items that may be lurking in a _SliceBuffer before
      // its real first element
      let backingStart = backing.firstElementAddress
      let sourceOffset = sourceStart - backingStart
      backingStart.deinitialize(count: sourceOffset)

      // Move the head items
      destStart.moveInitialize(from: sourceStart, count: headCount)

      // Destroy unused source items
      oldStart.deinitialize(count: oldCount)

      initializeNewElements.call(newStart, count: newCount)

      // Move the tail items
      newEnd.moveInitialize(from: oldStart + oldCount, count: tailCount)

      // Destroy any items that may be lurking in a _SliceBuffer after
      // its real last element
      let backingEnd = backingStart + backing.count
      let sourceEnd = sourceStart + sourceCount
      sourceEnd.deinitialize(count: backingEnd - sourceEnd)
      backing.count = 0
    }
    else {
      let headStart = startIndex
      let headEnd = headStart + headCount
      let newStart = _copyContents(
        subRange: headStart..<headEnd,
        initializing: destStart)
      initializeNewElements.call(newStart, count: newCount)
      let tailStart = headEnd + oldCount
      let tailEnd = endIndex
      _copyContents(subRange: tailStart..<tailEnd, initializing: newEnd)
    }
    self = Self(_buffer: dest, shiftedToStartIndex: startIndex)
  }

  /// Create a unique mutable buffer that has enough capacity to hold 'newCount'
  /// elements and at least 'requiredCapacity' elements. Set the count of the new
  /// buffer to 'newCount'. The content of the buffer is uninitialized.
  /// The formula used to compute the new buffers capacity is:
  ///   max(requiredCapacity, source.capacity)  if newCount <= source.capacity
  ///   max(requiredCapacity, _growArrayCapacity(source.capacity)) otherwise
  @inline(never)
  internal func _forceCreateUniqueMutableBuffer(
    newCount: Int, requiredCapacity: Int
  ) -> Buffer {
    return _forceCreateUniqueMutableBufferImpl(
      countForBuffer: newCount, minNewCapacity: newCount,
      requiredCapacity: requiredCapacity)
  }

  /// Create a unique mutable buffer that has enough capacity to hold
  /// 'minNewCapacity' elements and set the count of the new buffer to
  /// 'countForNewBuffer'. The content of the buffer uninitialized.
  /// The formula used to compute the new buffers capacity is:
  ///   max(minNewCapacity, source.capacity) if minNewCapacity <= source.capacity
  ///   max(minNewCapacity, _growArrayCapacity(source.capacity)) otherwise
  @inline(never)
  @_versioned
  internal func _forceCreateUniqueMutableBuffer(
    countForNewBuffer: Int, minNewCapacity: Int
  ) -> Buffer {
    return _forceCreateUniqueMutableBufferImpl(
      countForBuffer: countForNewBuffer, minNewCapacity: minNewCapacity,
      requiredCapacity: minNewCapacity)
  }

  /// Create a unique mutable buffer that has enough capacity to hold
  /// 'minNewCapacity' elements and at least 'requiredCapacity' elements and set
  /// the count of the new buffer to 'countForBuffer'. The content of the buffer
  /// uninitialized.
  /// The formula used to compute the new capacity is:
  ///  max(requiredCapacity, source.capacity) if minNewCapacity <= source.capacity
  ///  max(requiredCapacity, _growArrayCapacity(source.capacity))  otherwise
  internal func _forceCreateUniqueMutableBufferImpl(
    countForBuffer: Int, minNewCapacity: Int,
    requiredCapacity: Int
  ) -> Buffer {
    _sanityCheck(countForBuffer >= 0)
    _sanityCheck(requiredCapacity >= countForBuffer)
    _sanityCheck(minNewCapacity >= countForBuffer)

    let minimumCapacity = Swift.max(requiredCapacity,
      minNewCapacity > capacity
         ? _growArrayCapacity(capacity) : capacity)

    return Buffer(
      _uninitializedCount: countForBuffer, minimumCapacity: minimumCapacity)
  }

  @_versioned
  @inline(never)
  internal mutating func _outlinedMakeUniqueBuffer(bufferCount: Int) {

    if _fastPath(
        requestUniqueMutableBackingBuffer(minimumCapacity: bufferCount) != nil) {
      return
    }

    var newBuffer = _forceCreateUniqueMutableBuffer(
      newCount: bufferCount, requiredCapacity: bufferCount)
    _arrayOutOfPlaceUpdate(&newBuffer, bufferCount, 0, _IgnorePointer<Buffer.Element>())
  }

  /// Append items from `newItems` to a buffer.
  @_versioned
  internal mutating func _arrayAppendSequence<S : Sequence>(
    _ newItems: S
  ) where S.Iterator.Element == Element {
    
    // this function is only ever called from append(contentsOf:)
    // which should always have exhausted its capacity before calling
    _sanityCheck(count == capacity)
    var newCount = self.count

    // there might not be any elements to append remaining,
    // so check for nil element first, then increase capacity,
    // then inner-loop to fill that capacity with elements
    var stream = newItems.makeIterator()
    var nextItem = stream.next()
    while nextItem != nil {

      // grow capacity, first time around and when filled
      var newBuffer = _forceCreateUniqueMutableBuffer(
        countForNewBuffer: newCount, 
        // minNewCapacity handles the exponential growth, just
        // need to request 1 more than current count/capacity
        minNewCapacity: newCount + 1)

      _arrayOutOfPlaceUpdate(&newBuffer, newCount, 0, _IgnorePointer<Buffer.Element>())

      let currentCapacity = self.capacity
      let base = self.firstElementAddress

      // fill while there is another item and spare capacity
      while let next = nextItem, newCount < currentCapacity {
        (base + newCount).initialize(to: next)
        newCount += 1
        nextItem = stream.next()
      }
      self.count = newCount
    }
  }
}
