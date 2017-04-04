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

// This is a hack to work around the inability to assign to self in a class init
public protocol FactoryInitializable {}
extension FactoryInitializable {
  @nonobjc
  public init(_ me: Self) {
    self = me
  }
}

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
: class, ContiguouslyStoredMutableCollection,
    FactoryInitializable,  // Allows us to code init in terms of Builtin.allocWithTailElems_1
    RangeReplaceableCollection {

  associatedtype Header : _BoundedBufferHeader
  
  @nonobjc var _header: Header { get set }
  // WARNING: don't use this property without a fixLifetime call
  // protecting the use; ARC may end the lifetime of self before you
  // get a chance to use the result.
  @nonobjc var _baseAddress: UnsafeMutablePointer<Iterator.Element> { get }

  init(uninitializedWithMinimumCapacity: Int)

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
  
  public init(
    minimumCapacity: Int = 0,
    makeInitialHeader: (_ allocatedCapacity: Int)->Header) {
    self.init(uninitializedWithMinimumCapacity: minimumCapacity)
    withUnsafeMutablePointer(to: &_header) {
      $0.initialize(to: makeInitialHeader(allocatedCapacity()))
    }
  }

  public init(_uninitializedCount: Int, minimumCapacity: Int = 0) {
    self.init(minimumCapacity: Swift.max(_uninitializedCount, minimumCapacity)) {
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
    get {
      return withUnsafeBufferPointer { $0[i] }
    }
    set {
      return withUnsafeMutableBufferPointer { $0[i] = newValue }
    }
  }
  @nonobjc
  public var count: Int {
    get { return numericCast(_header.count) }
    set { _header.count = numericCast(newValue) }
  }
}

/// Fulfills the RangeReplaceableCollection requirements
extension _BoundedBufferReference {
  public init<S : Sequence>(_ elements: S)
    where S.Iterator.Element == Iterator.Element {
    self.init(Array(elements))
  }
  
  public init<C : Collection>(_ elements: C)
    where C.Iterator.Element == Iterator.Element {
    self.init(_uninitializedCount: numericCast(elements.count))
    withUnsafeMutableBufferPointer {
      elements._copyCompleteContents(initializing: $0)
    }
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
    let oldTailIndex = target.upperBound
    let oldTailStart = elements + oldTailIndex
    let newTailIndex = oldTailIndex + growth
    let newTailStart = oldTailStart + growth
    let tailCount = oldCount - target.upperBound

    if growth > 0 {
      // Slide the tail part of the buffer forwards, in reverse order
      // so as not to self-clobber.
      newTailStart.moveInitialize(from: oldTailStart, count: tailCount)

      // Assign over the original target
      var i = newValues.startIndex
      for j in CountableRange(target) {
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
      // Assign all the new elements into the start of the target
      var i = target.lowerBound
      var j = newValues.startIndex
      for _ in 0..<newCount {
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

        // Destroy elements remaining after the tail in target
        (newTailStart + tailCount).deinitialize(
          count: shrinkage - tailCount)
      }
    }
  }
}

/// Fulfills the ContiguouslyStoredMutableCollection requirements
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
  public init<Head : Collection, Middle : Collection, Tail : Collection>(
    joining head: Head, _ middle: Middle, _ tail: Tail, minimumCapacity: Int = 0
  )
  where
    Head.Iterator.Element == Iterator.Element,
    Middle.Iterator.Element == Iterator.Element,
    Tail.Iterator.Element == Iterator.Element
  {
    let newCount = numericCast(head.count) as IndexDistance
      + numericCast(middle.count) as IndexDistance
      + numericCast(tail.count) as IndexDistance

    self.init(
      _uninitializedCount: numericCast(newCount),
      minimumCapacity: minimumCapacity)
    
    self.withUnsafeMutableBufferPointer { b0 in

      let (_, i0) = head._copyContents(initializing: b0)
      
      let b1 = UnsafeMutableBufferPointer(
        start: b0.baseAddress! + i0, count: b0.count - b0[..<i0].count)
      let (_, i1) = middle._copyContents(initializing: b1)

      let b2 = UnsafeMutableBufferPointer(
        start: b1.baseAddress! + i1, count: b1.count - b1[..<i1].count)
      let (_, i2) = tail._copyContents(initializing: b2)
      
      assert(i2 == b2.endIndex, "Failed to consume input")
    }
  }

  /// If there is sufficient capacity, replaces the elements bounded by `target`
  /// with the contents of `replacement` and returns true.
  ///
  /// Returns `false` otherwise.
  public func _tryToReplaceSubrange<C: Collection>(
    from targetStart: Index, to targetEnd: Index, with replacement: C
  ) -> Bool
  where C.Iterator.Element == Iterator.Element {
    typealias D = IndexDistance
    let r = Counted(replacement)
    let delta = numericCast(r.count)
      - numericCast(self[targetStart..<targetEnd].count) as D

    if _fastPath(numericCast(capacity) as D >= self.count + delta) {
      let start: Int = numericCast(offset(of: targetStart))
      let end: Int = numericCast(offset(of: targetEnd))
      replaceSubrange(start..<end, with: r)
      return true
    }
    return false
  }
}

