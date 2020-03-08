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


public struct CircularArray<Element>: RandomAccessCollection, RangeReplaceableCollection {

    public typealias Element = Element

    public typealias Index = Int

    public typealias Indices = Range<Int>

    public typealias Iterator = IndexingIterator<Self>

    public typealias SubSequence = Slice<CircularArray<Element>>

    private var _buffer: CircularArrayBuffer<Element>

    public init(capacity: Int) {
        _buffer = CircularArrayBuffer<Element>(capacity: capacity)
    }

    public var isFull: Bool {
        return _buffer.isFull
    }

    public var capacity: Int {
        return _buffer.capacity
    }

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

public extension CircularArray {

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

public extension CircularArray {

    mutating func pushBack(_ newElement: Element) {
        makeUnique()

        _buffer.pushBack(newElement)
    }

    mutating func pushBack<S>(contentsOf newElements: S) where S: Sequence, Element == S.Element {
        makeUnique()
        
        for newElement in newElements {
            _buffer.pushBack(newElement)
        }
    }

    mutating func pushFront(_ newElement: Element) {
        makeUnique()

        _buffer.pushFront(newElement)
    }

    mutating func pushFront<S>(contentsOf newElements: S) where S: Sequence, Element == S.Element {
        makeUnique()

        for newElement in newElements {
            _buffer.pushFront(newElement)
        }
    }

    mutating func popBack() -> Element {
        makeUnique()

        return _buffer.popBack()
    }

    mutating func popFront() -> Element {
        makeUnique()

        return _buffer.popFront()
    }
}

extension CircularArray: ExpressibleByArrayLiteral {

    public init(arrayLiteral elements: Element...) {
        _buffer = CircularArrayBuffer(elements: elements)
    }
}

public extension CircularArray {

    init() {
        self._buffer = CircularArrayBuffer(capacity: 0)
    }

    init(repeating repeatedValue: Element, count: Int) {
        self._buffer = CircularArrayBuffer(repeating: repeatedValue, count: count)
    }

    init<S>(_ elements: S) where S : Sequence, Element == S.Element {
        self._buffer = CircularArrayBuffer(capacity: elements.underestimatedCount)
        self._buffer.append(contentsOf: elements)
    }

    mutating func replaceSubrange<C>(_ subrange: Range<Int>, with newElements: C) where C : Collection, Element == C.Element {
        precondition(subrange.lowerBound >= startIndex, "CircularArray replace: subrange start is negative")
        precondition(subrange.upperBound <= endIndex, "CircularArray replace: subrange extends past the end")
        makeUnique()

        _buffer.replaceSubrange(subrange, with: newElements)
    }

    mutating func removeSubrange(_ bounds: Range<Int>) {
        precondition(bounds.lowerBound >= startIndex, "CircularArray removeSubrange: subrange start is negative")
        precondition(bounds.upperBound <= endIndex, "CircularArray removeSubrange: subrange extends past the end")
        makeUnique()

        _buffer.removeSubrange(bounds)
    }

    mutating func insert(_ newElement: Element, at i: Int) {
        checkSubscript(index: i)
        makeUnique()

        _buffer.replaceSubrange(i..<i+1, with: [newElement])
    }

    mutating func insert<C>(contentsOf newElements: C, at i: Int) where C : Collection, Element == C.Element {
        checkSubscript(index: i)
        makeUnique()

        _buffer.replaceSubrange(i..<i+1, with: newElements)
    }

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

        return popFront()
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

extension CircularArray: CustomStringConvertible, CustomDebugStringConvertible {

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
        /// FIXME: DebugDescription
        return description
    }
}

internal extension CircularArray {

    @inline(__always)
    var isUnique: Bool {
        mutating get {
            isKnownUniquelyReferenced(&_buffer)
        }
    }
}

private extension CircularArray {

    mutating func makeUnique(additionalCapacity: Int = 0) {
        guard !isUnique else {
            return
        }

        let bufferCopy = _buffer.copy(additionalCapacity: additionalCapacity)
        self._buffer = bufferCopy
    }

    @inline(__always)
    func checkSubscript(index: Int) {
        precondition(index >= 0 && index < count)
    }
}

extension CircularArray: Equatable where Element: Equatable {

    public static func == (lhs: CircularArray<Element>, rhs: CircularArray<Element>) -> Bool {
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

    public static func != (lhs: CircularArray<Element>, rhs: CircularArray<Element>) -> Bool {
        return !(lhs == rhs)
    }
}

private final class CircularArrayBuffer<Element> {

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
                    _elements.advanced(by: _head + leftElementsToMoveCount).deinitialize(count: elementsToDeinitializeCount)
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
    func copy(newCapacity: Int) -> CircularArrayBuffer {
        let copyBuffer = CircularArrayBuffer(capacity: newCapacity)
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
    func copy(additionalCapacity: Int) -> CircularArrayBuffer {
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

extension CircularArrayBuffer {

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
    func pushBack<S>(contentsOf newElements: S) where S: Sequence, Element == S.Element {
        for newElement in newElements {
            pushBack(newElement)
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

    @inline(__always)
    func pushFront<S>(contentsOf newElements: S) where S: Sequence, Element == S.Element {
        for newElement in newElements {
            pushFront(newElement)
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

extension CircularArrayBuffer {

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

extension CircularArrayBuffer {

    @inline(__always)
    func replaceSubrange<C>(_ subrange: Range<Int>, with newElements: C) where C : Collection, Element == C.Element {
        let newElementsCount = newElements.count
        let subrangeCount = subrange.count

        if newElementsCount == 0 {
            removeSubrange(subrange)
            return
        }

        if subrangeCount == newElementsCount {
            for (index, element) in newElements.enumerated() {
                let mappedIndex = mapIndexToBuffer(index: index + subrange.lowerBound)
                self[mappedIndex] = element
            }
        } else if subrangeCount > newElementsCount {
            /// FIXME: TODO
        } else if subrangeCount < newElementsCount {
            /// FIXME: TODO
        }
    }

    @inline(__always)
    func removeSubrange(_ bounds: Range<Int>) {
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
        self.resize(newCapacity: capacity + n)
    }

    @inline(__always)
    func append(_ newElement: Element) {
        if capacity - _elementsCount > 0 {
            pushBack(newElement)
        } else {
            resize(newCapacity: capacity + 1)
            pushBack(newElement)
        }
    }

    @inline(__always)
    func append<S>(contentsOf newElements: S) where S : Sequence, Element == S.Element {
        let newElementsCount = newElements.underestimatedCount
        let leftCount = capacity - _elementsCount
        if leftCount < newElementsCount {
            let additionalCapacityRequired = newElementsCount - leftCount
            self.resize(newCapacity: capacity + additionalCapacityRequired)
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
        /// FIXME: Could be potentically optimized
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

private extension CircularArrayBuffer {

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
}
