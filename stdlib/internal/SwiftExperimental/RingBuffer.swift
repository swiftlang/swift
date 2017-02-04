//
//  RingBuffer.swift
//  RingBuffer
//
//  Created by Andrew Bennett on 29/1/17.
//  Copyright © 2017 Andrew Bennett. All rights reserved.
//

internal struct RingBuffer<Element>:
  RangeReplaceableCollection, MutableCollection, RandomAccessCollection,
  MutableCollectionAlgorithms
{
  private var _bufferCapacity: Int
  fileprivate var _buffer: ContiguousArray<Element>
  fileprivate var _indexOffset: Int

  public typealias Indices = CountableRange<Int>
  public typealias Iterator = IndexingIterator<RingBuffer>
  public typealias SubSequence = ArraySlice<Element>

  private func _checkIndex(_ position: Int) {
    precondition(position <= _buffer.count + _bufferCapacity,
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
    _buffer.reserveCapacity(Swift.max(n, _bufferCapacity))
  }

  public subscript(bounds: Range<Int>) -> SubSequence {
    get {
      let count = _buffer.count
      precondition(bounds.count <= count)
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
        precondition(subrange.lowerBound == 0)
        append(contentsOf: newElements)
        return
      }

      let count = _buffer.count
      let elementsCount = Int(newElements.count.toIntMax())
      let offset = Swift.max(-subrange.count,
        Swift.min(elementsCount - subrange.count,
          _bufferCapacity - count))
      var elements = newElements.makeIterator()
      let insertCount = Swift.max(0, offset) + subrange.count
      for _ in 0 ..< Swift.max(0, elementsCount-insertCount) {
        _ = elements.next()
      }

      if offset <= 0 {
        if !subrange.isEmpty {
          var index = subrange.lowerBound
          while let element = elements.next() {
            self[index] = element
            index += 1
            if index == subrange.upperBound {
              index = subrange.lowerBound
            }
          }
        }
        removeSubrange((subrange.upperBound+offset) ..< subrange.upperBound)
      }
      else {
        precondition(count < _bufferCapacity)
        precondition(_indexOffset == 0)
        for index in (count-offset) ..< count {
          _buffer.append(_buffer[Swift.max(index, 0)])
        }
        let range = subrange.lowerBound ..< (subrange.upperBound + offset)
        var index = range.lowerBound
        while let element = elements.next() {
          _buffer[index] = element
          index += 1
          if index == range.upperBound {
            index = range.lowerBound
          }
        }
      }
  }

  public mutating func removeSubrange(_ bounds: Range<Int>) {
      let count = _buffer.count
      precondition(bounds.count <= count)
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
      output.append(_buffer[_indexOffset ..< _buffer.count].lazy
        .map(String.init(describing:))
        .joined(separator: ", "))
      output.append("][")
      output.append(_buffer[0 ..< _indexOffset].lazy
        .map(String.init(describing:))
        .joined(separator: ", "))
    }
    output.append("])")

    return output
  }
}
