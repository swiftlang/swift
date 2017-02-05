//===----------------------------------------------------------------------===//
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

internal struct RingBuffer<Element>:
  RangeReplaceableCollection, MutableCollection, RandomAccessCollection,
  MutableCollectionAlgorithms
{
  // Stores up to _bufferCapacity elements into _buffer the indices start at
  // _indexOffset and wrap around.
  //
  // The notation [0,1][2,3,4] indicates an internal state of:
  //     _buffer:         [2,3,4,0,1]
  //     _indexOffset:    3      ^
  //     _bufferCapacity: 5
  //
  // If _buffer.count < _bufferCapacity then this has a few implications:
  //  * the buffer is not full
  //  * new elements will be appended to the end of the buffer (for speed)
  //  * _indexOffset must be zero (0)
  //
  // Algorithms used in this implementation aim to be O(1) in additional
  // memory usage, even at the expense of performance.
  
  private var _bufferCapacity: Int
  fileprivate var _buffer: ContiguousArray<Element>
  fileprivate var _indexOffset: Int

  public typealias Indices = CountableRange<Int>
  public typealias Iterator = IndexingIterator<RingBuffer>
  public typealias SubSequence = ArraySlice<Element>

  private func _checkIndex(_ position: Int) {
    _precondition(position <= _buffer.count + _bufferCapacity,
      "RingBuffer index is out of range")
  }
  
  public init() {
    self.init(capacity: 1)
  }
  
  public init(capacity: Int) {
    var buffer = ContiguousArray<Element>()
    buffer.reserveCapacity(capacity)
    self.init(buffer, capacity: capacity, offset: 0)
  }
  init(_ buffer: ContiguousArray<Element>, capacity: Int, offset: Int) {
    _bufferCapacity = capacity
    _buffer = buffer
    _indexOffset = offset
  }
  
  public var startIndex: Int {
    return 0
  }
  public var endIndex: Int {
    return _buffer.count
  }
  public var underestimatedCount: Int {
    return _buffer.count
  }
  public var isFull: Bool {
    return _buffer.count == _bufferCapacity
  }
  public var count: Int {
    return _buffer.count
  }
  public var capacity: Int {
    return _bufferCapacity
  }
  
  public mutating func reserveCapacity(_ n: Int) {
    rotate(shiftingToStart: _indexOffset)
    _bufferCapacity = Swift.max(n, _buffer.count)
    _buffer.reserveCapacity(_bufferCapacity)
  }
  
  public subscript(bounds: Range<Int>) -> SubSequence {
    get {
      let count = _buffer.count
      _precondition(bounds.count <= count)
      _checkIndex(bounds.lowerBound)
      _checkIndex(bounds.upperBound)
      let lowerBound = _indexOffset + bounds.lowerBound
      let upperBound = _indexOffset + bounds.upperBound
      guard lowerBound < count else {
        return _buffer[(lowerBound - count) ..< (upperBound - count)]
      }
      guard upperBound > count else {
        return _buffer[lowerBound ..< upperBound]
      }
      let lhs = _buffer[lowerBound ..< count]
      let rhs = _buffer[0 ..< (upperBound - count)]
      return SubSequence([lhs, rhs].joined())
    }
    set {
      replaceSubrange(bounds, with: newValue)
    }
  }
  public subscript(position: Int) -> Element {
    get {
      _checkIndex(position)
      let index = (_indexOffset + position) % _buffer.count
      return _buffer[index]
    }
    set {
      _checkIndex(position)
      let index = (_indexOffset + position) % _buffer.count
      _buffer[index] = newValue
    }
  }
  
  public func index(after i: Int) -> Int {
    return i + 1
  }
  public func index(before i: Int) -> Int {
    return i - 1
  }
  
  public mutating func replaceSubrange<C>(
    _ subrange: Range<Index>, with newElements: C) where
    C : Collection, Element == C.Iterator.Element
  {
    guard !newElements.isEmpty else {
      removeSubrange(subrange)
      return
    }
    guard !isEmpty else {
      _precondition(subrange.lowerBound == 0)
      append(contentsOf: newElements)
      return
    }
    
    let count = _buffer.count
    
    // FIXME: Is there a better way to do this
    // it's potentially O(n) in newElements, and has an unsafe cast
    let newCount = Int(newElements.count.toIntMax())
    // the change in self.count after inserting the elements
    let offsMin = -subrange.count, offsMax = _bufferCapacity - count
    let offset = Swift.max(offsMin, Swift.min(newCount-subrange.count, offsMax))
    var suffix = newCount.makeIterator()
    
    // equivalent of suffix(suffixCount), which can't be used as it uses a
    // ring buffer, this is also O(1) in memory.
    let suffixCount = Swift.max(0, offset) + subrange.count
    for _ in 0 ..< Swift.max(0, elementsCount-suffixCount) {
      _ = suffix.next()
    }
    
    // If the total number of elements doesn't increase only elements in
    // subrange need to be modified.
    // We can replace the elements of subrange, then remove excess subrange
    // elements.
    if offset <= 0 {
      if !subrange.isEmpty {
        var index = subrange.lowerBound
        while let element = suffix.next() {
          self[index] = element
          index += 1
          // FIXME: This should never happen, due to suffix above
          if index == subrange.upperBound {
            index = subrange.lowerBound
          }
        }
      }
      removeSubrange((subrange.upperBound+offset) ..< subrange.upperBound)
    }
    // If the total number of elements increases:
    //  1. move elements to the end as needed, or until the buffer is big enough
    //  2. insert the new elements into subrange and the new buffer
    else {
      _precondition(count < _bufferCapacity)
      _precondition(_indexOffset == 0)
      // FIXME: No need for `uninitializedElement` if elements can be
      // uninitialized in a lower-level container.
      // Copy elements to the end, until there's room for newElements
      let uninitializedElement = _buffer[0]
      for index in (count-offset) ..< count {
        _buffer.append(index < 0 ? uninitializedElement : _buffer[index])
      }
      let range = subrange.lowerBound ..< (subrange.upperBound + offset)
      var index = range.lowerBound
      while let element = suffix.next() {
        _buffer[index] = element
        index += 1
        // FIXME: This should never happen, due to suffix above
        if index == range.upperBound {
          index = range.lowerBound
        }
      }
    }
  }
  
  public mutating func removeSubrange(_ bounds: Range<Int>) {
    let count = _buffer.count
    _precondition(bounds.count <= count)
    _checkIndex(bounds.lowerBound)
    _checkIndex(bounds.upperBound)
    guard bounds.lowerBound < bounds.upperBound else {
      return
    }
    guard bounds.count < count else {
      removeAll()
      return
    }
    
    let newCount = count - bounds.count
    
    var lowerBound = _indexOffset + bounds.lowerBound
    var upperBound = _indexOffset + bounds.upperBound
    if lowerBound >= _bufferCapacity {
      lowerBound -= _bufferCapacity
      upperBound -= _bufferCapacity
    }
    
    if _indexOffset == 0 {
      for i in 0 ..< (newCount - bounds.lowerBound) {
        _buffer[bounds.lowerBound + i] = _buffer[bounds.upperBound + i]
      }
    }
    else {
      for i in bounds.lowerBound ..< newCount {
        let from = (_indexOffset + i + bounds.count) % _bufferCapacity
        let to = (_indexOffset + i) % _bufferCapacity
        _buffer[to] = _buffer[from]
      }
      rotate(shiftingToStart: _indexOffset)
    }
    
    _buffer.removeSubrange(newCount ..< count)
    _indexOffset = 0
  }
  
  public mutating func removeAll() {
    _indexOffset = 0
    _buffer.removeAll(keepingCapacity: true)
  }
  
  public mutating func append(_ newElement: Element) {
    if _buffer.count < _bufferCapacity {
      _buffer.append(newElement)
    }
    else {
      _buffer[_indexOffset] = newElement
      _indexOffset = (_indexOffset + 1) % _bufferCapacity
    }
  }
}

extension RingBuffer: ExpressibleByArrayLiteral {
  public init(arrayLiteral elements: Element...) {
    let buffer = ContiguousArray(elements)
    self.init(buffer, capacity: buffer.count, offset: 0)
  }
}

extension RingBuffer: CustomStringConvertible, CustomDebugStringConvertible {
  public var description: String {
    var output = "["
    if !isEmpty {
      output.reserveCapacity(2 + count * 3)
      output.append(self.lazy
        .map(String.init(describing:))
        .joined(separator: ", "))
    }
    output.append("]")
    return output
  }
  
  public var debugDescription: String {
    var output = "RingBuffer<"
    output.append(String(describing: Element.self))
    output.append(",\(capacity)>([")
    if !_buffer.isEmpty {
      output.reserveCapacity(2 + count * 3)
      if _buffer.count < _bufferCapacity {
        output.append(self.lazy
          .map(String.init(describing:))
          .joined(separator: ", "))
      }
      else {
        output.append(_buffer[_indexOffset ..< _buffer.count].lazy
          .map(String.init(describing:))
          .joined(separator: ", "))
        output.append("][")
        output.append(_buffer[0 ..< _indexOffset].lazy
          .map(String.init(describing:))
          .joined(separator: ", "))
      }
    }
    output.append("])")
    
    return output
  }
}
