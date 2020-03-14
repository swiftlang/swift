//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//


/**
 An ordered, random-access collection.

 You can use circular buffer instead of an array when you need fast
 front and back insertions and deletion together with fast subsript
 element access.

 When CircularBuffer is full, new data will be written to the beginning
 and old will be overwritten.

 Example:
 ~~~
 var circularBuffer = CircularBuffer<Int>(capacity: 2)
 circularBuffer.pushBack(1)
 circularBuffer.pushBack(2)

 print(circularBuffer)
 // Prints "[1, 2]"
 circularBuffer.pushBack(3)

 print(circularBuffer)
 // Prints "[2, 3]"
 ~~~

 You can manually increase CircularBuffer size using resize(newCapacity: ) method

 Example:
 ~~~
 var circularBuffer = CircularBuffer<Int>(capacity: 2)
 circularBuffer.pushBack(1)
 circularBuffer.pushBack(2)
 circularBuffer.pushBack(3)
 print(circularBuffer)
 // Prints "[2, 3]"

 circularBuffer.resize(newCapacity: 3)
 circularBuffer.pushBack(4)
 print(circularBuffer)
 // Prints "[2, 3, 4]"
 ~~~

 CircularBuffer supports both front and back insertion and deletion.
 ~~~
 var circularBuffer = CircularBuffer<Int>(capacity: 2)
 circularBuffer.pushBack(1)
 circularBuffer.pushFront(2)
 print(circularBuffer)
 // Prints "[2, 1]"
 // Now buffer isFull so next writes will overwrite data at beggining

 circularBuffer.pushFront(4)
 print(circularBuffer)
 // Prints "[4, 2]"

 circularBuffer.pushBack(3)
 print(circularBuffer)
 // Prints "[2, 3]"

 print(circularBuffer.popFront())
 // Prints "2"

 print(circularBuffer)
 // Prints "[3]"

 circularBuffer.popBack()
 print(circularBuffer)
 // Prints "[]"
 ~~~
 */
public struct CircularBuffer<Element>: RandomAccessCollection, RangeReplaceableCollection, MutableCollection {

  public typealias Element = Element

  public typealias Index = Int

  public typealias Indices = Range<Int>

  public typealias Iterator = IndexingIterator<Self>

  public typealias SubSequence = Slice<CircularBuffer<Element>>

  private var _buffer: CircularBufferBuffer<Element>

  public init(capacity: Int) {
    _buffer = CircularBufferBuffer<Element>(capacity: capacity)
  }

  /// A boolean value indicating that CircularBuffer is full.
  public var isFull: Bool {
    return _buffer.isFull
  }

  /**
   A total number of elements that CircularBuffer can contain
   without allocating new storage.
 */
  public var capacity: Int {
    return _buffer.capacity
  }

  /**
  CircularBuffer does not increase capacity automatically when it is full
   except for RandomAccessCollection conformance functions.
  You can manually increase capacity if you need to store more elements.

   New capacity must be greater of equal zero.
  Example:
  ~~~
  var circularBuffer = CircularBuffer<Int>(capacity: 2)
  circularBuffer.pushBack(1)
  circularBuffer.pushBack(2)
  circularBuffer.pushBack(3)
  print(circularBuffer)
  // Prints "[2, 3]"

  circularBuffer.resize(newCapacity: 3)
  circularBuffer.pushBack(4)
  print(circularBuffer)
  // Prints "[2, 3, 4]"
  ~~~

  - Parameter newCapacity: The element to append to the CircularBuffer
  */
  public mutating func resize(newCapacity: Int) {
    precondition(newCapacity >= 0)

    if isUnique {
      _buffer.resize(newCapacity: newCapacity)
    } else {
      let bufferCopy = _buffer.copy(newCapacity: newCapacity)
      self._buffer = bufferCopy
    }
  }
}

public extension CircularBuffer {

  /**
   Pushes element to the back of the CircularBuffer without increasing its capacity.
   If the CircularBuffer does not have sufficient capacity for another element,
   additional storage will not be allocated. An oldest element will be removed.

   Example:
   ~~~
   var circularBuffer = CircularBuffer<Int>(capacity: 2)
   circularBuffer.pushBack(1)
   circularBuffer.pushBack(2)

   print(circularBuffer)
   // Prints "[1, 2]"
   circularBuffer.pushBack(3)

   print(circularBuffer)
   // Prints "[2, 3]"
   ~~~

   - Parameter newElement: The element to append to the CircularBuffer

   Complexity: O(1).
   */
  mutating func pushBack(_ newElement: Element) {
    makeUnique()

    _buffer.pushBack(newElement)
  }

  /**
   Pushes element to the back of the CircularBuffer without increasing its capacity.
   If the CircularBuffer does not have sufficient capacity for another element,
   additional storage will not be allocated. An oldest elements will be removed.
   
   Example:
   ~~~
   var circularBuffer = CircularBuffer<Int>(capacity: 2)
   circularBuffer.pushBack(contentsOf: [2, 3])
   print(circularBuffer)
   // Prints "[1, 2]"

   circularBuffer.pushBack(contentsOf: [3, 4])
   print(circularBuffer)
   // Prints "[3, 4]"
   ~~~

   Complexity: O(m), where m is the length of the elements.

   - Parameter newElements: The elements to append to the CircularBuffer
   */
  mutating func pushBack<S>(contentsOf newElements: S) where S: Sequence, Element == S.Element {
    makeUnique()

    for newElement in newElements {
      _buffer.pushBack(newElement)
    }
  }

  /**
  Pushes element to the front of the CircularBuffer without increasing its capacity.
  If the CircularBuffer does not have sufficient capacity for another element,
  additional storage will not be allocated. An oldest element will be removed.

  Example:
  ~~~
  var circularBuffer = CircularBuffer<Int>(capacity: 2)
  circularBuffer.pushFront(1)
  circularBuffer.pushFront(2)
  print(circularBuffer)
  // Prints "[2, 1]"

  circularBuffer.pushFront(3)
  print(circularBuffer)
  // Prints "[3, 2]"
  ~~~

  - Parameter newElement: The element to append to the CircularBuffer

  Complexity: O(1).
  */
  mutating func pushFront(_ newElement: Element) {
    makeUnique()

    _buffer.pushFront(newElement)
  }

  /**
  Pushes elements to the front of the CircularBuffer without increasing its capacity.
  If the CircularBuffer does not have sufficient capacity for another element,
  additional storage will not be allocated. An oldest element will be removed.

  Example:
  ~~~
  var circularBuffer = CircularBuffer<Int>(capacity: 2)
  circularBuffer.pushFront(contentsOf: [1, 2])
  print(circularBuffer)
  // Prints "[2, 1]"

  circularBuffer.pushFront(contentsOf: [3, 4])
  print(circularBuffer)
  // Prints "[4, 3]"
  ~~~

  Complexity: O(m), where m is the length of the elements.

   - Parameter newElements: The elements to append to the CircularBuffer
  */
  mutating func pushFront<S>(contentsOf newElements: S) where S: Sequence, Element == S.Element {
    makeUnique()

    for newElement in newElements {
      _buffer.pushFront(newElement)
    }
  }

  /**
  Removes and returns element from the back of the CircularBuffer.

   The CircularBuffer must not be empty.

  Example:
  ~~~
  var circularBuffer = CircularBuffer<Int>(capacity: 2)
  circularBuffer.pushBack(1)
  circularBuffer.pushBack(2)
  print(circularBuffer.popBack())
  // Prints "2"

  circularBuffer.pushBack(3)
  print(circularBuffer.popBack())
  // Prints "3"
  ~~~

  Complexity: O(1).
  */
  @discardableResult
  mutating func popBack() -> Element {
    precondition(count > 0)
    makeUnique()

    return _buffer.popBack()
  }

  /**
  Removes and returns element from the front of the CircularBuffer.

   The CircularBuffer must not be empty.

  Example:
  ~~~
  var circularBuffer = CircularBuffer<Int>(capacity: 2)
  circularBuffer.pushFront(1)
  circularBuffer.pushFront(2)
  print(circularBuffer.popFront())
  // Prints "2"

  circularBuffer.pushFront(3)
  print(circularBuffer.popFront())
  // Prints "3"
  ~~~

  Complexity: O(1).
  */
  @discardableResult
  mutating func popFront() -> Element {
    precondition(count > 0)
    makeUnique()

    return _buffer.popFront()
  }
}

public extension CircularBuffer {

  subscript(position: Int) -> Element {
    get {
      checkSubscript(index: position)

      return _buffer[position]
    }
    set {
      checkSubscript(index: position)
      makeUnique()

      _buffer[position] = newValue
    }
  }

  subscript(range: Range<Int>) -> SubSequence {
    get {
      precondition(range.count <= _buffer.count)
      return SubSequence(base: self, bounds: range)
    }
    set {
      precondition(range.count <= _buffer.count)
      replaceSubrange(range, with: newValue)
    }
  }

  var count: Int {
    return _buffer.count
  }

  var underestimatedCount: Int {
    return _buffer.count
  }

  var isEmpty: Bool {
    return _buffer.count == 0
  }

  var startIndex: Int {
    return 0
  }

  var endIndex: Int {
    return _buffer.count
  }

  func index(after i: Int) -> Int {
    return i + 1
  }

  func index(before i: Int) -> Int {
    return i - 1
  }

  func index(_ i: Int, offsetBy distance: Int) -> Int {
    return i + distance
  }

  func formIndex(after i: inout Int) {
    i += 1
  }

  func formIndex(before i: inout Int) {
    i -= 1
  }

  func formIndex(_ i: inout Int, offsetBy distance: Int) {
    i += distance
  }
}

extension CircularBuffer: ExpressibleByArrayLiteral {

  public init(arrayLiteral elements: Element...) {
    _buffer = CircularBufferBuffer(elements: elements)
  }
}

public extension CircularBuffer {

  init() {
    self._buffer = CircularBufferBuffer(capacity: 0)
  }

  init(repeating repeatedValue: Element, count: Int) {
    self._buffer = CircularBufferBuffer(repeating: repeatedValue, count: count)
  }

  init<S>(_ elements: S) where S : Sequence, Element == S.Element {
    self._buffer = CircularBufferBuffer(capacity: elements.underestimatedCount)
    self._buffer.append(contentsOf: elements)
  }

  mutating func replaceSubrange<C>(_ subrange: Range<Int>, with newElements: C) where C : Collection, Element == C.Element {
    precondition(subrange.lowerBound >= startIndex, "CircularBuffer replaceSubrange: subrange start is negative")
    precondition(subrange.upperBound <= endIndex, "CircularBuffer replaceSubrange: subrange extends past the end")
    makeUnique()

    _buffer.replaceSubrange(subrange, with: newElements)
  }

  mutating func removeSubrange(_ bounds: Range<Int>) {
    precondition(bounds.lowerBound >= startIndex, "CircularBuffer removeSubrange: subrange start is negative")
    precondition(bounds.upperBound <= endIndex, "CircularBuffer removeSubrange: subrange extends past the end")
    makeUnique()

    _buffer.removeSubrange(bounds)
  }

  mutating func insert(_ newElement: Element, at i: Int) {
    checkSubscriptIncludingCount(index: i)
    makeUnique()

    _buffer.replaceSubrange(i..<i, with: [newElement])
  }

  mutating func insert<C>(contentsOf newElements: C, at i: Int) where C : Collection, Element == C.Element {
    checkSubscriptIncludingCount(index: i)
    makeUnique()

    _buffer.replaceSubrange(i..<i, with: newElements)
  }

  @discardableResult
  mutating func remove(at position: Int) -> Element {
    checkSubscript(index: position)
    makeUnique()

    let element = _buffer[position]
    removeSubrange(position..<position+1)
    return element
  }

  mutating func reserveCapacity(_ n: Int) {
    precondition(n > 0)
    if isUnique {
      _buffer.reserveCapacity(n)
    } else {
      let bufferCopy = _buffer.copy(additionalCapacity: n)
      self._buffer = bufferCopy
    }
  }

  mutating func append(_ newElement: Element) {
    makeUnique()

    _buffer.append(newElement)
  }

  mutating func append<S>(contentsOf newElements: S) where S : Sequence, Element == S.Element {
    makeUnique()

    _buffer.append(contentsOf: newElements)
  }

  @discardableResult
  mutating func removeLast() -> Element {
    precondition(count > 0)
    makeUnique()

    return _buffer.removeLast()
  }

  @discardableResult
  mutating func popLast() -> Element? {
    makeUnique()

    return _buffer.popLast()
  }

  mutating func removeLast(_ k: Int) {
    precondition(count >= k)
    makeUnique()

    _buffer.removeLast(k)
  }

  @discardableResult
  mutating func popFirst() -> Element? {
    makeUnique()

    return _buffer.popFirst()
  }

  @discardableResult
  mutating func removeFirst() -> Element {
    precondition(count > 0)
    makeUnique()

    return _buffer.removeFirst()
  }

  mutating func removeFirst(_ k: Int) {
    precondition(count >= k)
    makeUnique()

    _buffer.removeFirst(k)
  }

  mutating func removeAll(keepingCapacity keepCapacity: Bool = false) {
    makeUnique()

    _buffer.removeAll(keepingCapacity: keepCapacity)
  }

  mutating func removeAll(where shouldBeRemoved: (Element) throws -> Bool) rethrows {
    makeUnique()

    try _buffer.removeAll(where: shouldBeRemoved)
  }
}

extension CircularBuffer: CustomStringConvertible, CustomDebugStringConvertible {

  public var description: String {
    var output = "["

    var first = true
    for item in self {
      if first {
        first = false
      } else {
        output += ", "
      }
      debugPrint(item, terminator: "", to: &output)
    }

    output += "]"

    return output
  }

  public var debugDescription: String {
    let typeName = "CircularBuffer"
    return typeName + "(" + description + ")"
  }
}

extension CircularBuffer: Equatable where Element: Equatable {

  public static func == (lhs: CircularBuffer<Element>, rhs: CircularBuffer<Element>) -> Bool {
    let lhsCount = lhs.count
    let rhsCount = rhs.count

    if lhsCount != rhsCount { return false }
    if lhsCount == 0 || lhs._buffer === rhs._buffer { return true }

    for index in 0..<lhsCount {
      if lhs[index] != rhs[index] {
        return false
      }
    }

    return true
  }

  public static func != (lhs: CircularBuffer<Element>, rhs: CircularBuffer<Element>) -> Bool {
    return !(lhs == rhs)
  }
}

extension CircularBuffer: Hashable where Element: Hashable {

  public func hash(into hasher: inout Hasher) {
    hasher.combine(count)

    for element in self {
      hasher.combine(element)
    }
  }
}

private extension CircularBuffer {

  mutating func makeUnique(additionalCapacity: Int = 0) {
    guard !isUnique else {
      return
    }

    let bufferCopy = _buffer.copy(additionalCapacity: additionalCapacity)
    self._buffer = bufferCopy
  }

  @inline(__always)
  private var isUnique: Bool {
    mutating get {
      isKnownUniquelyReferenced(&_buffer)
    }
  }

  @inline(__always)
  func checkSubscript(index: Int) {
    precondition(index >= 0 && index < count)
  }

  @inline(__always)
  func checkSubscriptIncludingCount(index: Int) {
    precondition(index >= 0 && index <= count)
  }
}

private final class CircularBufferBuffer<Element> {

  private var _elements: UnsafeMutablePointer<Element>

  private(set) var capacity: Int

  private var _head: Int

  private var _tail: Int

  private var _elementsCount: Int

  @inline(__always)
  init(capacity: Int) {
    self._elements = UnsafeMutablePointer<Element>.allocate(capacity: capacity)
    self.capacity = capacity
    self._elementsCount = 0
    self._tail = 0
    self._head = 0
  }

  @inline(__always)
  init(repeating repeatedValue: Element, count: Int) {
    self._elements = UnsafeMutablePointer<Element>.allocate(capacity: count)
    self._elements.initialize(repeating: repeatedValue, count: count)
    self.capacity = count
    self._head = 0
    self._tail = 0
    self._elementsCount = count
  }

  @inline(__always)
  init(elements: [Element]) {
    let elementsCount = elements.count
    self._elements = UnsafeMutablePointer<Element>.allocate(capacity: elementsCount)

    for index in 0..<elementsCount {
      self._elements.advanced(by: index).initialize(to: elements[index])
    }

    self.capacity = elementsCount
    self._head = 0
    self._tail = 0
    self._elementsCount = elementsCount
  }

  @inline(__always)
  var isFull: Bool {
    return _elementsCount == capacity
  }

  @inline(__always)
  var isEmpty: Bool {
    return _elementsCount == 0
  }

  @inline(__always)
  var count: Int {
    return _elementsCount
  }

  @inline(__always)
  func resize(newCapacity: Int) {
    if newCapacity > capacity {
      let newElements = UnsafeMutablePointer<Element>.allocate(capacity: newCapacity)

      if _head + _elementsCount > capacity {
        let rightCount = capacity - _head
        newElements.moveInitialize(from: _elements.advanced(by: _head), count: rightCount)
        newElements.advanced(by: rightCount).moveInitialize(from: _elements, count: _elementsCount - rightCount)
      } else {
        newElements.moveInitialize(from: _elements.advanced(by: _head), count: _elementsCount)
      }

      _elements.deallocate()
      _elements = newElements
    } else if newCapacity < capacity {
      let newElements = UnsafeMutablePointer<Element>.allocate(capacity: newCapacity)
      let elementsToMoveCount = newCapacity < _elementsCount ? newCapacity : _elementsCount

      if _head + _elementsCount > capacity {
        let rightCount = capacity - _head

        if rightCount >= elementsToMoveCount {
          newElements.moveInitialize(from: _elements.advanced(by: _head), count: elementsToMoveCount)

          let rightElementsToDeinitializeCount = rightCount - elementsToMoveCount
          _elements.advanced(by: _head + elementsToMoveCount).deinitialize(count: rightElementsToDeinitializeCount)

          let leftElementsToDeinitializCount = _elementsCount - rightCount
          _elements.deinitialize(count: leftElementsToDeinitializCount)
        } else {
          let rightElementsToMoveCount = elementsToMoveCount - rightCount
          newElements.moveInitialize(from: _elements.advanced(by: _head), count: rightElementsToMoveCount)

          let leftElementsToMoveCount = elementsToMoveCount - rightElementsToMoveCount
          newElements.advanced(by: rightElementsToMoveCount).moveInitialize(from: _elements, count: leftElementsToMoveCount)

          let elementsToDeinitializeCount = _elementsCount - elementsToMoveCount
          _elements.advanced(by: leftElementsToMoveCount).deinitialize(count: elementsToDeinitializeCount)
        }
      } else {
        let elementsToDeinitializeCount = _elementsCount - elementsToMoveCount
        newElements.moveInitialize(from: _elements.advanced(by: _head), count: elementsToMoveCount)
        _elements.advanced(by: _head + elementsToMoveCount).deinitialize(count: elementsToDeinitializeCount)
      }
      _elements.deallocate()

      _elements = newElements
      _elementsCount = elementsToMoveCount
    } else if newCapacity == capacity {
      return
    }

    self.capacity = newCapacity
    self._head = 0
    self._tail = incrementIndex(index: self._elementsCount - 1)
  }

  @inline(__always)
  func copy(newCapacity: Int) -> CircularBufferBuffer {
    let copyBuffer = CircularBufferBuffer(capacity: newCapacity)
    let elementsToCopyCount = newCapacity < _elementsCount ? newCapacity : _elementsCount

    for index in 0..<elementsToCopyCount {
      let mappedIndex = mapIndexToBuffer(index: index)
      let element = _elements.advanced(by: mappedIndex).pointee
      copyBuffer._elements.advanced(by: index).initialize(to: element)
    }
    copyBuffer._elementsCount = elementsToCopyCount
    copyBuffer._tail = incrementIndex(index: elementsToCopyCount-1)
    copyBuffer._head = 0

    return copyBuffer
  }

  @inline(__always)
  func copy(additionalCapacity: Int) -> CircularBufferBuffer {
    return copy(newCapacity: capacity + additionalCapacity)
  }

  deinit {
    if _head + _elementsCount > capacity {
      let rightCount = capacity - _head
      _elements.advanced(by: _head).deinitialize(count: rightCount)
      _elements.deinitialize(count: _elementsCount - rightCount)
    } else {
      _elements.advanced(by: _head).deinitialize(count: _elementsCount)
    }
    _elements.deallocate()
  }
}

extension CircularBufferBuffer {

  @inline(__always)
  func pushBack(_ newElement: Element) {
    if isFull {
      if isEmpty {
        return
      }
      _elements.advanced(by: _tail).pointee = newElement
      _tail = incrementIndex(index: _tail)
      _head = _tail
    } else {
      _elements.advanced(by: _tail).initialize(to: newElement)
      _tail = incrementIndex(index: _tail)
      _elementsCount += 1
    }
  }

  @inline(__always)
  func pushFront(_ newElement: Element) {
    if isFull {
      if isEmpty {
        return
      }
      _head = decrementIndex(index: _head)
      _elements.advanced(by: _head).pointee = newElement
      _tail = _head
    } else {
      _head = decrementIndex(index: _head)
      _elements.advanced(by: _head).initialize(to: newElement)
      _elementsCount += 1
    }
  }

  @discardableResult
  @inline(__always)
  func popBack() -> Element {
    _tail = decrementIndex(index: _tail)
    let element = _elements.advanced(by: _tail).move()
    _elementsCount -= 1

    return element
  }

  @discardableResult
  @inline(__always)
  func popFront() -> Element {
    let element = _elements.advanced(by: _head).move()
    _head = incrementIndex(index: _head)
    _elementsCount -= 1

    return element
  }
}

extension CircularBufferBuffer {

  @inline(__always)
  subscript(index: Int) -> Element {
    get {
      let mappedIndex = mapIndexToBuffer(index: index)
      return _elements.advanced(by: mappedIndex).pointee
    }
    set {
      let mappedIndex = mapIndexToBuffer(index: index)
      _elements.advanced(by: mappedIndex).pointee = newValue
    }
  }
}

extension CircularBufferBuffer {

  @inline(__always)
  func replaceSubrange<C>(_ subrange: Range<Int>, with newElements: C) where C : Collection, Element == C.Element {
    let newElementsCount = newElements.count
    let subrangeCount = subrange.count

    if newElementsCount == 0 {
      removeSubrange(subrange)
      return
    }

    /// FIXME: DoubleCheck implementation using reference type tests
    if subrangeCount == newElementsCount {
      for (index, element) in newElements.enumerated() {
        let mappedIndex = mapIndexToBuffer(index: index + subrange.lowerBound)
        _elements.advanced(by: mappedIndex).pointee = element
      }
    } else if subrangeCount > newElementsCount {
      rotateBuffer()
      let elementsToRemoveCount = subrangeCount - newElementsCount

      for (index, element) in newElements.enumerated() {
        _elements.advanced(by: index + subrange.lowerBound).pointee = element
      }

      for index in subrange.lowerBound + newElementsCount..<subrange.upperBound {
        _elements.advanced(by: index).deinitialize(count: 1)
      }

      let startOffset = subrange.upperBound - elementsToRemoveCount
      let elementsToMoveCount = _elementsCount - subrange.upperBound

      let moveToPointer = _elements.advanced(by: startOffset)
      let moveFromPointer = _elements.advanced(by: subrange.upperBound)
      moveToPointer.moveInitialize(from: moveFromPointer, count: elementsToMoveCount)

      _elementsCount -= elementsToRemoveCount
      _head = 0
      _tail = incrementIndex(index: _elementsCount - 1)
    } else if subrangeCount < newElementsCount {
      let elementsInsertCount = newElementsCount - subrangeCount
      let leftCount = capacity - _elementsCount
      let additionalCapacityCount = elementsInsertCount - leftCount
      if additionalCapacityCount > 0 {
        resize(newCapacity: grow(minCapacity: capacity + additionalCapacityCount))
      } else {
        rotateBuffer()
      }

      let moveFromOffset = subrange.upperBound
      let moveFromPointer = _elements.advanced(by: moveFromOffset)

      let moveToOffset = subrange.upperBound + elementsInsertCount
      let moveToPointer = _elements.advanced(by: moveToOffset)
      let moveCount = _elementsCount - moveFromOffset

      moveToPointer.moveInitialize(from: moveFromPointer, count: moveCount)

      for (index, element) in newElements.enumerated() {
        let indexToInsert = index + subrange.lowerBound

        if subrange.contains(indexToInsert) {
          _elements.advanced(by: indexToInsert).pointee = element
        } else {
          _elements.advanced(by: indexToInsert).initialize(to: element)
        }
      }

      _elementsCount += elementsInsertCount
      _head = 0
      _tail = incrementIndex(index: _elementsCount - 1)
    }
  }

  @inline(__always)
  func removeSubrange(_ bounds: Range<Int>) {
    /// FIXME: DoubleCheck implementation using reference type tests
    if bounds.count == _elementsCount {
      removeAll(keepingCapacity: true)
    } else if bounds.lowerBound == 0 {
      removeFirst(bounds.count)
    } else if bounds.upperBound == _elementsCount {
      removeLast(bounds.count)
    } else {
      if _head + bounds.lowerBound > capacity {
        let elementsToRemoveStartOffset = _head + bounds.lowerBound - capacity
        let elementsToRemoveStartPointer = _elements.advanced(by: elementsToRemoveStartOffset)
        elementsToRemoveStartPointer.deinitialize(count: bounds.count)

        let rightElementsCount = capacity - _head
        let leftElementsCount = elementsToRemoveStartOffset

        let elementsToMoveCount = _elementsCount - rightElementsCount - leftElementsCount - bounds.count
        let elementsToMoveStartOffset = elementsToRemoveStartOffset + bounds.count
        let elementsToMoveStartPointer = _elements.advanced(by: elementsToMoveStartOffset)

        elementsToRemoveStartPointer.moveInitialize(from: elementsToMoveStartPointer, count: elementsToMoveCount)

        _tail = elementsToRemoveStartOffset + bounds.count
        _elementsCount -= bounds.count
      } else if _head + bounds.upperBound > capacity {
        let rightElementsToRemoveOffset = _head + bounds.lowerBound
        let rightElementsToRemoveCount = capacity - rightElementsToRemoveOffset
        _elements.advanced(by: rightElementsToRemoveOffset).deinitialize(count: rightElementsToRemoveCount)

        let leftElementsToRemoveCount = bounds.count - rightElementsToRemoveCount
        _elements.deinitialize(count: leftElementsToRemoveCount)

        let rightElementsToMoveCount = bounds.lowerBound
        let rightElementsToMoveStart = _elements.advanced(by: _head)
        _elements.moveInitialize(from: rightElementsToMoveStart, count: rightElementsToMoveCount)

        let leftElementsToMoveCount = _elementsCount - bounds.count - rightElementsToMoveCount
        let leftElementsToMoveStart = _elements.advanced(by: rightElementsToMoveCount)

        leftElementsToMoveStart.moveInitialize(from: _elements.advanced(by: leftElementsToRemoveCount), count: leftElementsToMoveCount)

        _head = 0
        _elementsCount -= bounds.count
        _tail = _elementsCount
      } else {
        let elementsToRemoveStart = _elements.advanced(by: _head + bounds.lowerBound)
        elementsToRemoveStart.deinitialize(count: bounds.count)

        let leftElementsCount = _head + bounds.lowerBound + bounds.count
        let rightElementsCount = _elementsCount - bounds.count - (bounds.lowerBound - _head)

        let moveElementsStart = _elements.advanced(by: leftElementsCount)
        elementsToRemoveStart.moveInitialize(from: moveElementsStart, count: rightElementsCount)

        _elementsCount -= bounds.count
        _tail = leftElementsCount + rightElementsCount
      }
    }
  }

  @inline(__always)
  func reserveCapacity(_ n: Int) {
    self.resize(newCapacity: grow(minCapacity: capacity + n))
  }

  @inline(__always)
  func append(_ newElement: Element) {
    if capacity - _elementsCount > 0 {
      pushBack(newElement)
    } else {
      resize(newCapacity: grow(minCapacity: capacity + 1))
      pushBack(newElement)
    }
  }

  @inline(__always)
  func append<S>(contentsOf newElements: S) where S : Sequence, Element == S.Element {
    let newElementsCount = newElements.underestimatedCount
    let leftCount = capacity - _elementsCount
    if leftCount < newElementsCount {
      let additionalCapacityRequired = newElementsCount - leftCount
      self.resize(newCapacity: grow(minCapacity: capacity + additionalCapacityRequired))
    }

    for newElement in newElements {
      append(newElement)
    }
  }

  @inline(__always)
  func removeLast() -> Element {
    return popBack()
  }

  @inline(__always)
  func popLast() -> Element? {
    if isEmpty {
      return nil
    }

    return popBack()
  }

  @inline(__always)
  func removeLast(_ k: Int) {
    for _ in 0..<k {
      popBack()
    }
  }

  @inline(__always)
  func popFirst() -> Element? {
    if isEmpty {
      return nil
    }

    return popFront()
  }

  @inline(__always)
  func removeFirst() -> Element {
    return popFront()
  }

  @inline(__always)
  func removeFirst(_ k: Int) {
    for _ in 0..<k {
      popFront()
    }
  }

  @inline(__always)
  func removeAll(keepingCapacity keepCapacity: Bool = false) {
    if _head + _elementsCount > capacity {
      let rightCount = capacity - _head
      _elements.advanced(by: _head).deinitialize(count: rightCount)
      _elements.deinitialize(count: _elementsCount - rightCount)
    } else {
      _elements.advanced(by: _head).deinitialize(count: _elementsCount)
    }

    _head = 0
    _elementsCount = 0
    _tail = 0
    if !keepCapacity {
      capacity = 0
      _elements.deallocate()
      _elements = UnsafeMutablePointer<Element>.allocate(capacity: 0)
    }
  }

  @inline(__always)
  func removeAll(where shouldBeRemoved: (Element) throws -> Bool) rethrows {
    var indexesToRemove: [Int] = []

    for index in 0..<_elementsCount {
      let element = self[index]
      if try shouldBeRemoved(element) {
        indexesToRemove.append(index)
      }
    }

    var removedCount: Int = 0
    for index in indexesToRemove {
      let adjustedIndex = index-removedCount
      self.removeSubrange(adjustedIndex..<adjustedIndex+1)
      removedCount += 1
    }
  }
}

private extension CircularBufferBuffer {

  @inline(__always)
  func mapIndexToBuffer(index: Int) -> Int {
    let advanceHeadIndex = _head + index
    return advanceHeadIndex < capacity ? advanceHeadIndex : advanceHeadIndex - capacity
  }

  @inline(__always)
  func incrementIndex(index: Int) -> Int {
    if index == capacity-1 {
      return 0
    }

    return index + 1
  }

  @inline(__always)
  func decrementIndex(index: Int) -> Int {
    if index == 0 {
      return capacity-1
    }

    return index-1
  }

  @inline(__always)
  func grow(minCapacity: Int) -> Int {
    let newCapacity = self.capacity + self.capacity >> 1
    return Swift.max(minCapacity, newCapacity)
  }

  func reverseBuffer(from: Int, to: Int) {
    var startIndex = from
    var endIndex = to

    while (startIndex < endIndex) {
      let startPointer = self._elements.advanced(by: startIndex)
      let endPointer = self._elements.advanced(by: endIndex)

      let temp = startPointer.pointee
      startPointer.pointee = endPointer.pointee
      endPointer.pointee = temp

      startIndex += 1
      endIndex -= 1
    }
  }

  func rotateBuffer() {
    if _head + _elementsCount > capacity {
      reverseBuffer(from: _head, to: capacity-1)
      let rightElementsCount = capacity-_head
      let leftElementsCount = _elementsCount-rightElementsCount

      reverseBuffer(from: 0, to: leftElementsCount-1)
      if !isFull {
        _elements.advanced(by: _head - leftElementsCount).moveInitialize(from: _elements, count: leftElementsCount)
        _elements.moveInitialize(from: _elements.advanced(by: _head - leftElementsCount), count: _elementsCount)
      }

      reverseBuffer(from: 0, to: _elementsCount-1)
      _head = 0
      _tail = incrementIndex(index: _head + _elementsCount - 1)
    } else {
      _elements.moveInitialize(from: _elements.advanced(by: _head), count: _elementsCount)
      _head = 0
      _tail = incrementIndex(index: _head + _elementsCount - 1)
    }
  }
}
