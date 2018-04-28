// ###sourceLocation(file: "/Users/lanceparker/Development/swift-source/swift/stdlib/public/core/Arrays.swift.gyb", line: 1)
//===--- Arrays.swift.gyb -------------------------------------*- swift -*-===//
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
//  Three generic, mutable array-like types with value semantics.
//
//  - `ContiguousArray<Element>` is a fast, contiguous array of `Element` with
//    a known backing store.
//
//  - `ArraySlice<Element>` presents an arbitrary subsequence of some
//    contiguous sequence of `Element`s.
//
//  - `Array<Element>` is like `ContiguousArray<Element>` when `Element` is not
//    a reference type or an Objective-C existential.  Otherwise, it may use
//    an `NSArray` bridged from Cocoa for storage.
//
//===----------------------------------------------------------------------===//

/// This type is used as a result of the _checkSubscript call to associate the
/// call with the array access call it guards.
@_fixed_layout
public struct _DependenceToken {
  @inlinable
  public init() {
  }
}

// ###sourceLocation(file: "/Users/lanceparker/Development/swift-source/swift/stdlib/public/core/Arrays.swift.gyb", line: 41)

// ###sourceLocation(file: "/Users/lanceparker/Development/swift-source/swift/stdlib/public/core/Arrays.swift.gyb", line: 1823)

// Utility method for collections that wish to implement CustomStringConvertible
// and CustomDebugStringConvertible using a bracketed list of elements,
// like an array.
@inlinable // FIXME(sil-serialize-all)
internal func _makeCollectionDescription<C : Collection>
  (for items: C, withTypeName type: String?) -> String {
  var result = ""
  if let type = type {
    result += "\(type)(["
  } else {
    result += "["
  }
  var first = true
  for item in items {
    if first {
      first = false
    } else {
      result += ", "
    }
    debugPrint(item, terminator: "", to: &result)
  }
  result += type != nil ? "])" : "]"
  return result
}

@usableFromInline
@_fixed_layout
internal struct _InitializeMemoryFromCollection<
  C: Collection
> : _PointerFunction {
  @inlinable
  internal func call(_ rawMemory: UnsafeMutablePointer<C.Element>, count: Int) {
    var p = rawMemory
    var q = newValues.startIndex
    for _ in 0..<count {
      p.initialize(to: newValues[q])
      newValues.formIndex(after: &q)
      p += 1
    }
    _expectEnd(of: newValues, is: q)
  }

  @inlinable
  internal init(_ newValues: C) {
    self.newValues = newValues
  }

  @usableFromInline
  internal var newValues: C
}

extension _ArrayBufferProtocol {
  @inlinable
  @inline(never)
  internal mutating func _arrayOutOfPlaceReplace<C : Collection>(
    _ bounds: Range<Int>,
    with newValues: C,
    count insertCount: Int
  ) where C.Element == Element {

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
}

/// A _debugPrecondition check that `i` has exactly reached the end of
/// `s`.  This test is never used to ensure memory safety; that is
/// always guaranteed by measuring `s` once and re-using that value.
@inlinable
internal func _expectEnd<C : Collection>(of s: C, is i: C.Index) {
  _debugPrecondition(
    i == s.endIndex,
    "invalid Collection: count differed in successive traversals")
}

@inlinable
internal func _growArrayCapacity(_ capacity: Int) -> Int {
  return capacity * 2
}

// ###sourceLocation(file: "/Users/lanceparker/Development/swift-source/swift/stdlib/public/core/Arrays.swift.gyb", line: 1980)

//===--- generic helpers --------------------------------------------------===//

extension _ArrayBufferProtocol {
  /// Create a unique mutable buffer that has enough capacity to hold 'newCount'
  /// elements and at least 'requiredCapacity' elements. Set the count of the new
  /// buffer to 'newCount'. The content of the buffer is uninitialized.
  /// The formula used to compute the new buffers capacity is:
  ///   max(requiredCapacity, source.capacity)  if newCount <= source.capacity
  ///   max(requiredCapacity, _growArrayCapacity(source.capacity)) otherwise
  @inlinable
  @inline(never)
  internal func _forceCreateUniqueMutableBuffer(
    newCount: Int, requiredCapacity: Int
  ) -> _ContiguousArrayBuffer<Element> {
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
  @inlinable
  @inline(never)
  internal func _forceCreateUniqueMutableBuffer(
    countForNewBuffer: Int, minNewCapacity: Int
  ) -> _ContiguousArrayBuffer<Element> {
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
  @inlinable
  internal func _forceCreateUniqueMutableBufferImpl(
    countForBuffer: Int, minNewCapacity: Int,
    requiredCapacity: Int
  ) -> _ContiguousArrayBuffer<Element> {
    _sanityCheck(countForBuffer >= 0)
    _sanityCheck(requiredCapacity >= countForBuffer)
    _sanityCheck(minNewCapacity >= countForBuffer)

    let minimumCapacity = Swift.max(requiredCapacity,
      minNewCapacity > capacity
         ? _growArrayCapacity(capacity) : capacity)

    return _ContiguousArrayBuffer(
      _uninitializedCount: countForBuffer, minimumCapacity: minimumCapacity)
  }
}

@usableFromInline
internal protocol _PointerFunction {
  associatedtype Element
  func call(_: UnsafeMutablePointer<Element>, count: Int)
}

extension _ArrayBufferProtocol {
  /// Initialize the elements of dest by copying the first headCount
  /// items from source, calling initializeNewElements on the next
  /// uninitialized element, and finally by copying the last N items
  /// from source into the N remaining uninitialized elements of dest.
  ///
  /// As an optimization, may move elements out of source rather than
  /// copying when it isUniquelyReferenced.
  @inlinable
  @inline(never)
  internal mutating func _arrayOutOfPlaceUpdate<Initializer>(
    _ dest: inout _ContiguousArrayBuffer<Element>,
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
}

@usableFromInline
@_fixed_layout
internal struct _IgnorePointer<T> : _PointerFunction {
  @inlinable
  internal func call(_: UnsafeMutablePointer<T>, count: Int) {
    _sanityCheck(count == 0)
  }

  @inlinable
  internal init() {
  }
}

extension _ArrayBufferProtocol {
  @inlinable
  @inline(never)
  internal mutating func _outlinedMakeUniqueBuffer(bufferCount: Int) {

    if _fastPath(
        requestUniqueMutableBackingBuffer(minimumCapacity: bufferCount) != nil) {
      return
    }

    var newBuffer = _forceCreateUniqueMutableBuffer(
      newCount: bufferCount, requiredCapacity: bufferCount)
    _arrayOutOfPlaceUpdate(&newBuffer, bufferCount, 0, _IgnorePointer())
  }

  /// Append items from `newItems` to a buffer.
  @inlinable
  internal mutating func _arrayAppendSequence<S : Sequence>(
    _ newItems: S
  ) where S.Element == Element {
    
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

      _arrayOutOfPlaceUpdate(&newBuffer, newCount, 0, _IgnorePointer())

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

// ###sourceLocation(file: "/Users/lanceparker/Development/swift-source/swift/stdlib/public/core/Arrays.swift.gyb", line: 2361)

#if _runtime(_ObjC)
// We isolate the bridging of the Cocoa Array -> Swift Array here so that
// in the future, we can eagerly bridge the Cocoa array. We need this function
// to do the bridging in an ABI safe way. Even though this looks useless,
// DO NOT DELETE!
@usableFromInline internal
func _bridgeCocoaArray<T>(_ _immutableCocoaArray: _NSArrayCore) -> Array<T> {
  return Array(_buffer: _ArrayBuffer(nsArray: _immutableCocoaArray))
}

extension Array {
  @inlinable
  public // @SPI(Foundation)
  func _bridgeToObjectiveCImpl() -> AnyObject {
    return _buffer._asCocoaArray()
  }

  /// Tries to downcast the source `NSArray` as our native buffer type.
  /// If it succeeds, creates a new `Array` around it and returns that.
  /// Returns `nil` otherwise.
  // Note: this function exists here so that Foundation doesn't have
  // to know Array's implementation details.
  @inlinable
  public static func _bridgeFromObjectiveCAdoptingNativeStorageOf(
    _ source: AnyObject
  ) -> Array? {
    // If source is deferred, we indirect to get its native storage
    let maybeNative = (source as? _SwiftDeferredNSArray)?._nativeStorage ?? source

    return (maybeNative as? _ContiguousArrayStorage<Element>).map {
      Array(_ContiguousArrayBuffer($0))
    }
  }

  /// Private initializer used for bridging.
  ///
  /// Only use this initializer when both conditions are true:
  ///
  /// * it is statically known that the given `NSArray` is immutable;
  /// * `Element` is bridged verbatim to Objective-C (i.e.,
  ///   is a reference type).
  @inlinable
  public init(_immutableCocoaArray: _NSArrayCore) {
    self = _bridgeCocoaArray(_immutableCocoaArray)
  }
}
#endif

extension ArraySlice {
  @inlinable
  public // @testable
  init(_startIndex: Int) {
    self.init(
      _buffer: _Buffer(
        _buffer: ContiguousArray()._buffer,
        shiftedToStartIndex: _startIndex))
  }
}

// Local Variables:
// eval: (read-only-mode 1)
// End:
