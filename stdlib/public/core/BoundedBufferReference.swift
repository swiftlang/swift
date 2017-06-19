//===--- BoundedBufferReference.swift -------------------------------------===//
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
// Growable buffer classes with bounded capacity
//===----------------------------------------------------------------------===//
import SwiftShims // for _swift_stdlib_malloc_size

/// Stores (at least) the count and capacity of a growable buffer class
public // @testable
protocol _BoundedBufferHeader {
  init(count: Int, capacity: Int)
  associatedtype Size : UnsignedInteger
  var count: Size { get set }
  var capacity: Size { get }
}

/// A class that presents its contiguous array of Elements as a
/// random-access, range-replaceable collection
public // @testable
protocol _BoundedBufferReference
: class, _ContiguouslyStoredMutableCollection,
    RangeReplaceableCollection {

  associatedtype Header : _BoundedBufferHeader
  
  @nonobjc
  var _header: Header { get set }
  
  // WARNING: don't use this property without a fixLifetime call
  // protecting the use; ARC may end the lifetime of self before you
  // get a chance to use the result.
  @nonobjc
  var _baseAddress: UnsafeMutablePointer<Iterator.Element> { get }

  static func make(uninitializedWithMinimumCapacity: Int) -> Self

  /// A number of extra elements to allocate for
  ///
  /// Used for NUL-termination in String
  @nonobjc
  static var extraCapacity : Int { get }

  /// Returns an instance with count == 0.
  ///
  /// This is a separate entry point to allow us to return a
  /// statically-allocated instance
  static func _emptyInstance() -> Self
}

extension _BoundedBufferReference {
  public typealias Element = Iterator.Element
  
  public init() {
    self.init(Self._emptyInstance())
  }

  public static var extraCapacity: Int { return 0 }
  
  public static func make(
    minimumCapacity: Int = 0,
    makeInitialHeader: (_ allocatedCapacity: Int)->Header
  ) -> Self {
    let extra = Self.extraCapacity
    let r = make(uninitializedWithMinimumCapacity: minimumCapacity + extra)
    withUnsafeMutablePointer(to: &r._header) {
      $0.initialize(to: makeInitialHeader(r.allocatedCapacity() - extra))
    }
    return r
  }

  public static func make(
    _uninitializedCount: Int, minimumCapacity: Int = 0
  ) -> Self {
    return make(
      minimumCapacity: Swift.max(_uninitializedCount, minimumCapacity)
    ) {
      Header(count: _uninitializedCount, capacity: $0)
    }
  }
  
  public var capacity: Int {
    return numericCast(_header.capacity)
  }
  
  internal func allocatedCapacity() -> Int {
    let selfAddr = UnsafeRawPointer(Builtin.bridgeToRawPointer(self))
    let baseByteOffset = UnsafeRawPointer(_baseAddress) - selfAddr
    let bytesAllocated = _swift_stdlib_malloc_size(selfAddr)
    let elementBytes = bytesAllocated - baseByteOffset
    return elementBytes / MemoryLayout<Element>.stride
  }  
}

/// Fulfills the RandomAccessCollection and MutableCollection requirements
extension _BoundedBufferReference {
  @nonobjc
  public var startIndex: Int { return 0 }
  @nonobjc
  public var endIndex: Int { return count }

  @nonobjc
  public subscript(i: Int) -> Element {
    // FIXME: Add addressors
    @inline(__always)
    get {
      return withUnsafeBufferPointer { $0[i] }
    }
    @inline(__always)
    set {
      return withUnsafeMutableBufferPointer { $0[i] = newValue }
    }
  }
  @nonobjc
  public var count: Int {
    @inline(__always)
    get { return numericCast(_header.count) }
    @inline(__always)
    set { _header.count = numericCast(newValue) }
  }
}

/// Fulfills the RangeReplaceableCollection requirements
extension _BoundedBufferReference {
  public static func make<S : Sequence>(_ elements: S) -> Self
    where S.Iterator.Element == Iterator.Element {
    return make(Array(elements))
  }
  
  public static func make<C : Collection>(_ elements: C) -> Self
    where C.Iterator.Element == Iterator.Element {
    let r = make(_uninitializedCount: numericCast(elements.count))
    _ = r.withUnsafeMutableBufferPointer {
      elements._copyCompleteContents(initializing: $0)
    }
    return r
  }
  
  public func replaceSubrange<C>(
    _ target: Range<Int>,
    with newValues: C
  ) where C : Collection, 
  C.Iterator.Element == Iterator.Element {
    self.replaceSubrange(
      target,
      with: numericCast(newValues.count),
      elementsOf: newValues)
  }
  
  public func replaceSubrange<C>(
    _ target: Range<Int>,
    with newCount: Int,
    elementsOf newValues: C
  ) where C : Collection, 
  C.Iterator.Element == Iterator.Element {
    defer { _fixLifetime(self) }
    let oldCount: Int = numericCast(self.count)
    let eraseCount: Int = numericCast(target.count)

    let growth = newCount - eraseCount
    _sanityCheck(oldCount + growth <= capacity)
    self._header.count = numericCast(oldCount + growth)

    let elements = self._baseAddress
    let targetStart = elements + target.lowerBound
    let oldTailIndex = target.upperBound
    let oldTailStart = elements + oldTailIndex
    let newTailStart = oldTailStart + growth
    let tailCount = oldCount - target.upperBound

    if growth > 0 {
      // Slide the tail part of the buffer down to make space
      newTailStart.moveInitialize(from: oldTailStart, count: tailCount)

      // Assign over the original target elements
      let (i, _) = newValues._copyContents(
        assigning: UnsafeMutableBufferPointer(
          start: targetStart, count: eraseCount))
      
      // Initialize the hole left by sliding the tail forward
      IteratorSequence(i)._copyCompleteContents(
        initializing: UnsafeMutableBufferPointer(
          start: oldTailStart, count: growth))
    }
    else { // We're not growing the buffer
      // Assign all the new elements into the start of the target
      newValues._copyCompleteContents(
        assigning: UnsafeMutableBufferPointer(
          start: targetStart, count: newCount))
      
      // If the size didn't change, we're done.
      if _slowPath(growth == 0) { return }

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

        // Destroy elements remaining after the tail in target
        (newTailStart + tailCount).deinitialize(
          count: shrinkage - tailCount)
      }
    }
  }
}

/// Fulfills the _ContiguouslyStoredMutableCollection requirements
extension _BoundedBufferReference {
  @inline(__always)
  public func withUnsafeBufferPointer<R>(
    _ body: (UnsafeBufferPointer<Iterator.Element>) throws -> R
  ) rethrows -> R {
    defer { _fixLifetime(self) }
    return try body(UnsafeBufferPointer(start: _baseAddress, count: count))
  }
  
  @inline(__always)
  public func withUnsafeMutableBufferPointer<R>(
    _ body: (inout UnsafeMutableBufferPointer<Iterator.Element>) throws->R
  ) rethrows -> R {
    defer { _fixLifetime(self) }
    var buffer = UnsafeMutableBufferPointer(start: _baseAddress, count: count)
    return try body(&buffer)
  }
}

extension _BoundedBufferReference {
  /// Construct the concatenation of head, middle, and tail
  public static func make<
    Head : Collection, Middle : Collection, Tail : Collection
  >(
    joining head: Head, _ middle: Middle, _ tail: Tail, minimumCapacity: Int = 0
  ) -> Self
  where
    Head.Iterator.Element == Iterator.Element,
    Middle.Iterator.Element == Iterator.Element,
    Tail.Iterator.Element == Iterator.Element
  {
    let newCount = numericCast(head.count) as IndexDistance
      + numericCast(middle.count) as IndexDistance
      + numericCast(tail.count) as IndexDistance

    let r = self.make(
      _uninitializedCount: numericCast(newCount),
      minimumCapacity: minimumCapacity)
    
    r.withUnsafeMutableBufferPointer { b0 in

      let (_, i0) = head._copyContents(initializing: b0)
      
      let b1 = UnsafeMutableBufferPointer(
        start: b0.baseAddress! + i0, count: b0.count - b0[..<i0].count)
      let (_, i1) = middle._copyContents(initializing: b1)

      let b2 = UnsafeMutableBufferPointer(
        start: b1.baseAddress! + i1, count: b1.count - b1[..<i1].count)
      let (_, i2) = tail._copyContents(initializing: b2)
      
      assert(i2 == b2.endIndex, "Failed to consume input")
    }
    return r
  }
}

extension _BoundedBufferReference where Index == Int, IndexDistance == Int {
  /// If there is sufficient capacity, replaces the elements bounded by `target`
  /// with the contents of `replacement` and returns true.
  ///
  /// Returns `false` otherwise.
  public func _tryToReplaceSubrange<C: Collection>(
    _ target: Range<Index>, with replacement: C
  ) -> Bool
  where C.Iterator.Element == Iterator.Element {
    let r = _Counted(replacement)
    let targetCount = self.distance(
      from: target.lowerBound, to: target.upperBound)
    let delta = numericCast(r.count) - targetCount
    let newCount = self.count + delta
    
    if _fastPath(capacity >= newCount) {
      replaceSubrange(target, with: r)
      return true
    }
    return false
  }
}

