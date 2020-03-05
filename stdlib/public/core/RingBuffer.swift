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

public struct RingBuffer<Element>: RangeReplaceableCollection, RandomAccessCollection, MutableCollection {

    public typealias Element = Element

    public typealias Index = Int

    public typealias Indices = Range<Int>

    public typealias Iterator = IndexingIterator<Self>

    public typealias SubSequence = ArraySlice<Element>

    public private(set) var capacity: Int

    private var _elementsCount: Int = 0

    private var _head: Int = 0

    private var _tail: Int = 0

    private var _continiousBuffer: ContiguousArray<Element?>

    public init(capacity: Int) {
        self.capacity = capacity
        self._continiousBuffer = ContiguousArray<Element?>(repeating: nil, count: capacity)
    }

    public var isFull: Bool {
        return _elementsCount == capacity
    }

    mutating public func resize(newCapacity: Int) {
        precondition(newCapacity >= 0)
        rotateBuffer()

        if newCapacity > capacity {
            let elementsToAllocate = newCapacity-capacity
            _continiousBuffer.reserveCapacity(elementsToAllocate)
            for _ in 0..<elementsToAllocate {
                _continiousBuffer.append(nil)
            }
        } else {
            let elementsToRemove = capacity - newCapacity
            _continiousBuffer.removeLast(elementsToRemove)

            if _elementsCount > newCapacity {
                _elementsCount = newCapacity
            }
        }

        self.capacity = newCapacity
        self._head = 0
        self._tail = incrementIndex(index: self._elementsCount - 1)
    }

}

public extension RingBuffer {

    mutating func pushBack(_ newElement: Element) {
        if isFull {
            if isEmpty {
                return
            }
            _continiousBuffer[_tail] = newElement
            _tail = incrementIndex(index: _tail)
            _head = _tail
        } else {
            _continiousBuffer[_tail] = newElement
            _tail = incrementIndex(index: _tail)
            _elementsCount += 1
        }
    }

    mutating func pushBack<S>(contentsOf newElements: S) where S: Sequence, Element == S.Element {
        for newElement in newElements {
            pushBack(newElement)
        }
    }

    mutating func pushFront(_ newElement: Element) {
        if isFull {
            if isEmpty {
                return
            }
            _head = decrementIndex(index: _head)
            _continiousBuffer[_head] = newElement
            _tail = _head
        } else {
            _head = decrementIndex(index: _head)
            _continiousBuffer[_head] = newElement
            _elementsCount += 1
        }
    }

    mutating func pushFront<S>(contentsOf newElements: S) where S: Sequence, Element == S.Element {
        for newElement in newElements {
            pushFront(newElement)
        }
    }

    @discardableResult
    mutating func popBack() -> Element {
        _tail = decrementIndex(index: _tail)
        let element = _continiousBuffer[_tail]
        _continiousBuffer[_tail] = nil
        _elementsCount -= 1

        return element!
    }

    @discardableResult
    mutating func popFront() -> Element {
        let element = _continiousBuffer[_head]
        _continiousBuffer[_head] = nil
        _head = incrementIndex(index: _head)
        _elementsCount -= 1

        return element!
    }
}

// MARK: RandomAccessCollection protocol

public extension RingBuffer {

    subscript(position: Int) -> Element {
        get {
            precondition(position >= 0 && position < _elementsCount)
            let mappedIndex = mapIndexToBuffer(index: position)
            return _continiousBuffer[mappedIndex]!
        }
        set(newValue) {
            precondition(position >= 0 && position < _elementsCount)
            let mappedIndex = mapIndexToBuffer(index: position)
            _continiousBuffer[mappedIndex]! = newValue
        }
    }

    subscript(range: Range<Int>) -> SubSequence {
        get {
            precondition(range.count <= _elementsCount)
            let upperBound = range.upperBound

            /// FIXME: Rewriting using internal API if possible without compactMap slices
            if _head + upperBound < capacity {
                let optionalSlice = _continiousBuffer[range]

                return SubSequence(optionalSlice.compactMap { $0 })
            } else {
                let rightBound = _head..<capacity
                let rightSlice = _continiousBuffer[rightBound].compactMap { $0 }

                let leftBound = 0..<upperBound-rightBound.count
                let leftSlice = _continiousBuffer[leftBound].compactMap { $0 }

                return SubSequence([rightSlice, leftSlice].joined())
            }
        }
        set {
            replaceSubrange(range, with: newValue)
        }
    }

    @inlinable
    var startIndex: Int {
        return 0
    }

    var endIndex: Int {
        return _elementsCount
    }

    @inlinable
    func index(after i: Int) -> Int {
        return i+1
    }

    @inlinable
    func index(before i: Int) -> Int {
        return i-1
    }

    var count: Int {
        return _elementsCount
    }

    var underestimatedCount: Int {
        return _elementsCount
    }
}

// MARK: RangeReplaceableCollection protocol

public extension RingBuffer {

    init() {
        self.capacity = 0
        self._continiousBuffer = []
    }

    init(repeating repeatedValue: Element, count: Int) {
        self.capacity = count
        self._elementsCount = count
        self._continiousBuffer = ContiguousArray<Element?>(repeating: repeatedValue, count: count)
    }

    init<S>(_ elements: S) where S : Sequence, Element == S.Element {
        self._continiousBuffer = []
        for element in elements {
            self._continiousBuffer.append(element)
        }
        self.capacity = _continiousBuffer.count
        self._elementsCount = _continiousBuffer.count
    }

    mutating func replaceSubrange<C>(_ subrange: Range<Int>, with newElements: C) where C : Collection, Element == C.Element {
        precondition(subrange.lowerBound >= startIndex, "Ring Buffer replace: subrange start is negative")
        precondition(subrange.upperBound <= endIndex, "Ring Buffer replace: subrange extends past the end")
        if newElements.isEmpty {
            removeSubrange(subrange)
            return
        }

        let newElementsCount = newElements.count
        let subrangeCount = subrange.count
        
        if subrangeCount == newElementsCount {
            for (index, element) in newElements.enumerated() {
                let mappedIndex = mapIndexToBuffer(index: index + subrange.lowerBound)
                _continiousBuffer[mappedIndex] = element
            }
        } else if subrangeCount < newElementsCount {
            let elementsToAddCount = newElementsCount - subrangeCount
            let leftCount = capacity - _elementsCount
            if leftCount < elementsToAddCount {
                let additionalCapacityRequired = elementsToAddCount - leftCount
                self.resize(newCapacity: capacity + additionalCapacityRequired)
            }

            let shiftElementsCount = _elementsCount - subrange.upperBound
            shiftRightBuffer(from: subrange.upperBound, to: subrange.upperBound+shiftElementsCount, count: shiftElementsCount)

            for (index, newElement) in newElements.enumerated() {
                _continiousBuffer[index+subrange.lowerBound] = newElement
            }

            self._elementsCount += elementsToAddCount
            self._tail = incrementIndex(index: _elementsCount - 1)
        } else if subrangeCount > newElementsCount {
            rotateBuffer()
            for (index, newElement) in newElements.enumerated() {
                _continiousBuffer[index+subrange.lowerBound] = newElement
            }

            let removeRange = subrange.lowerBound + newElementsCount..<subrange.upperBound

            for index in removeRange {
                self._continiousBuffer[index] = nil
            }

            let elementsToShift = _elementsCount - removeRange.lowerBound - removeRange.count
            shiftLeftBuffer(from: removeRange.upperBound, to: removeRange.lowerBound, count: elementsToShift)
            self._elementsCount -= removeRange.count
            self._head = 0
            self._tail = incrementIndex(index: _elementsCount - 1)
        }
    }

    mutating func removeSubrange(_ bounds: Range<Int>) {
        precondition(bounds.lowerBound >= startIndex, "Ring Buffer removeSubrange: subrange start is negative")
        precondition(bounds.upperBound <= endIndex, "Ring Buffer removeSubrange: subrange extends past the end")

        rotateBuffer()

        for index in bounds {
            self._continiousBuffer[index] = nil
        }

        let elementsToShift = _elementsCount - bounds.lowerBound - bounds.count
        shiftLeftBuffer(from: bounds.upperBound, to: bounds.lowerBound, count: elementsToShift)
        self._elementsCount -= bounds.count
        self._head = 0
        self._tail = incrementIndex(index: _elementsCount - 1)
    }

    mutating func append(_ newElement: Element) {
        if capacity - _elementsCount > 0 {
            pushBack(newElement)
        } else {
            resize(newCapacity: capacity + 1)
            pushBack(newElement)
        }
    }

    mutating func append<S>(contentsOf newElements: S) where S : Sequence, Element == S.Element {
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

    @inlinable
    mutating func reserveCapacity(_ n: Int) {
        self.resize(newCapacity: capacity + n)
    }

    @inlinable
    @discardableResult
    mutating func removeLast() -> Element {
        return popBack()
    }

    @inlinable
    @discardableResult
    mutating func popLast() -> Element? {
        if isEmpty {
            return nil
        }

        return popBack()
    }

    mutating func removeLast(_ k: Int) {
        for _ in 0..<k {
            popBack()
        }
    }

    @inlinable
    @discardableResult
    mutating func popFirst() -> Element? {
        if isEmpty {
            return nil
        }

        return popFront()
    }

    @discardableResult
    mutating func removeFirst() -> Element {
        return popFront()
    }

    mutating func removeFirst(_ k: Int) {
        for _ in 0..<k {
            popFront()
        }
    }

    mutating func removeAll(keepingCapacity keepCapacity: Bool = false) {
        for i in 0..<_elementsCount {
            _continiousBuffer[i] = nil
        }
        _head = 0
        _elementsCount = 0
        _tail = 0

        if !keepCapacity {
            capacity = 0
            _continiousBuffer = []
        }
    }
}

extension RingBuffer: ExpressibleByArrayLiteral {

    public init(arrayLiteral elements: Element...) {
        self.capacity = elements.count
        self._continiousBuffer = ContiguousArray<Element?>(elements)
        self._elementsCount = elements.count
    }
}

extension RingBuffer: CustomStringConvertible, CustomDebugStringConvertible {

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
        return description
    }
}

private extension RingBuffer {

    mutating func reverseBuffer(from: Int, to: Int) {
        var startIndex = from
        var endIndex = to

        while (startIndex < endIndex) {
            let temp = self._continiousBuffer[startIndex]
            self._continiousBuffer[startIndex] = self._continiousBuffer[endIndex]
            self._continiousBuffer[endIndex] = temp

            startIndex += 1
            endIndex -= 1
        }
    }

    mutating func rotateBuffer() {
        if _head + _elementsCount > capacity {
            reverseBuffer(from: _head, to: _elementsCount-1)
            reverseBuffer(from: 0, to: _head-1)
            reverseBuffer(from: 0, to: _elementsCount-1)
        } else {
            shiftLeftBuffer(from: _head, to: 0, count: _elementsCount)
        }
    }

    mutating func shiftLeftBuffer(from: Int, to: Int, count: Int) {
        var offset = to
        for index in from..<from+count {
            _continiousBuffer[offset] = _continiousBuffer[index]
            offset += 1
        }

        let start = Swift.max(to+count, from)
        for index in start..<from+count {
            _continiousBuffer[index] = nil
        }
    }

    mutating func shiftRightBuffer(from: Int, to: Int, count: Int) {
        var offsetTo = to + count-1
        var offsetFrom = from + count-1
        for _ in from..<from+count {
            _continiousBuffer[offsetTo] = _continiousBuffer[offsetFrom]
            offsetTo -= 1
            offsetFrom -= 1
        }

        let start = from
        let end = Swift.min(from+count, to)
        for index in start..<end {
            _continiousBuffer[index] = nil
        }
    }

    @inline(__always)
    func mapIndexToBuffer(index: Int) -> Int {
        /// FIXME: Rewrite if possible without if
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
