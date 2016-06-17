//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@_exported import Foundation // Clang module

public func ==(lhs: IndexSet.Index, rhs: IndexSet.Index) -> Bool {
    return lhs.value == rhs.value && rhs.rangeIndex == rhs.rangeIndex
}

public func <(lhs: IndexSet.Index, rhs: IndexSet.Index) -> Bool {
    return lhs.value < rhs.value && rhs.rangeIndex <= rhs.rangeIndex
}

public func <=(lhs: IndexSet.Index, rhs: IndexSet.Index) -> Bool {
    return lhs.value <= rhs.value && rhs.rangeIndex <= rhs.rangeIndex
}

public func >(lhs: IndexSet.Index, rhs: IndexSet.Index) -> Bool {
    return lhs.value > rhs.value && rhs.rangeIndex >= rhs.rangeIndex
}

public func >=(lhs: IndexSet.Index, rhs: IndexSet.Index) -> Bool {
    return lhs.value >= rhs.value && rhs.rangeIndex >= rhs.rangeIndex
}

public func ==(lhs: IndexSet.RangeView, rhs: IndexSet.RangeView) -> Bool {
    return lhs.startIndex == rhs.startIndex && lhs.endIndex == rhs.endIndex && lhs.indexSet == rhs.indexSet
}

// We currently cannot use this mechanism because NSIndexSet is not abstract; it has its own ivars and therefore subclassing it using the same trick as NSData, etc. does not work.

/*
private final class _SwiftNSIndexSet : _SwiftNativeNSIndexSet, _SwiftNativeFoundationType {
    public typealias ImmutableType = NSIndexSet
    public typealias MutableType = NSMutableIndexSet

    var __wrapped : _MutableUnmanagedWrapper<ImmutableType, MutableType>

    init(immutableObject: AnyObject) {
      // Take ownership.
      __wrapped = .Immutable(
        Unmanaged.passRetained(
          _unsafeReferenceCast(immutableObject, to: ImmutableType.self)))

      super.init()
    }

    init(mutableObject: AnyObject) {
      // Take ownership.
      __wrapped = .Mutable(
       Unmanaged.passRetained(
         _unsafeReferenceCast(mutableObject, to: MutableType.self)))
      super.init()
    }

    public required init(unmanagedImmutableObject: Unmanaged<ImmutableType>) {
      // Take ownership.
      __wrapped = .Immutable(unmanagedImmutableObject)

      super.init()
    }

    public required init(unmanagedMutableObject: Unmanaged<MutableType>) {
      // Take ownership.
      __wrapped = .Mutable(unmanagedMutableObject)

      super.init()
    }

    deinit {
      releaseWrappedObject()
    }
}
*/

/// Manages a `Set` of integer values, which are commonly used as an index type in Cocoa API.
///
/// The range of valid integer values is 0..<INT_MAX-1. Anything outside this range is an error.
public struct IndexSet : ReferenceConvertible, Equatable, BidirectionalCollection, SetAlgebra {
    
    /// An view of the contents of an IndexSet, organized by range.
    ///
    /// For example, if an IndexSet is composed of:
    ///  `[1..<5]` and `[7..<10]` and `[13]`
    /// then calling `next()` on this view's iterator will produce 3 ranges before returning nil.
    public struct RangeView : Equatable, BidirectionalCollection {
        public typealias Index = Int
        public let startIndex : Index
        public let endIndex : Index
        
        private var indexSet : IndexSet
        
        // Range of element values
        private var intersectingRange : Range<IndexSet.Element>?
        
        private init(indexSet : IndexSet, intersecting range : Range<IndexSet.Element>?) {
            self.indexSet = indexSet
            self.intersectingRange = range
            
            if let r = range {
                if r.lowerBound == r.upperBound {
                    startIndex = 0
                    endIndex = 0
                } else {
                    let minIndex = indexSet._indexOfRange(containing: r.lowerBound)
                    let maxIndex = indexSet._indexOfRange(containing: r.upperBound)
                    
                    switch (minIndex, maxIndex) {
                    case (nil, nil):
                        startIndex = 0
                        endIndex = 0
                    case (nil, .some(let max)):
                        // Start is before our first range
                        startIndex = 0
                        endIndex = max + 1
                    case (.some(let min), nil):
                        // End is after our last range
                        startIndex = min
                        endIndex = indexSet._rangeCount
                    case (.some(let min), .some(let max)):
                        startIndex = min
                        endIndex = max + 1
                    }
                }
            } else {
                startIndex = 0
                endIndex = indexSet._rangeCount
            }
        }
        
        public func makeIterator() -> IndexingIterator<RangeView> {
            return IndexingIterator(_elements: self)
        }
        
        public subscript(index : Index) -> CountableRange<IndexSet.Element> {
            let indexSetRange = indexSet._range(at: index)
            if let intersectingRange = intersectingRange {
                return Swift.max(intersectingRange.lowerBound, indexSetRange.lowerBound)..<Swift.min(intersectingRange.upperBound, indexSetRange.upperBound)
            } else {
                return indexSetRange.lowerBound..<indexSetRange.upperBound
            }
        }
        
        public subscript(bounds: Range<Index>) -> BidirectionalSlice<RangeView> {
            return BidirectionalSlice(base: self, bounds: bounds)
        }

        public func index(after i: Index) -> Index {
            return i + 1
        }
        
        public func index(before i: Index) -> Index {
            return i - 1
        }
        
    }
    
    /// The mechanism for getting to the integers stored in an IndexSet.
    public struct Index : CustomStringConvertible, Comparable {
        private let indexSet : IndexSet
        private var value : IndexSet.Element
        private var extent : Range<IndexSet.Element>
        private var rangeIndex : Int
        private let rangeCount : Int
        
        private init(firstIn indexSet : IndexSet) {
            self.indexSet = indexSet
            self.rangeCount = indexSet._rangeCount
            self.rangeIndex = 0
            self.extent =  indexSet._range(at: 0)
            self.value = extent.lowerBound
        }
        
        private init(lastIn indexSet : IndexSet) {
            self.indexSet = indexSet
            let rangeCount = indexSet._rangeCount
            self.rangeIndex = rangeCount - 1
            if rangeCount > 0 {
                self.extent = indexSet._range(at: rangeCount - 1)
                self.value = extent.upperBound // "1 past the end" position is the last range, 1 + the end of that range's extent
            } else {
                self.extent = 0..<0
                self.value = 0
            }
            self.rangeCount = rangeCount
        }
        
        private init(indexSet: IndexSet, index: Int) {
            self.indexSet = indexSet
            self.rangeCount = self.indexSet._rangeCount
            self.value = index
            if let rangeIndex = self.indexSet._indexOfRange(containing: index) {
                self.extent = self.indexSet._range(at: rangeIndex)
                self.rangeIndex = rangeIndex
            } else {
                self.extent = 0..<0
                self.rangeIndex = 0
            }
        }
        
        // First or last value in a specified range
        private init(indexSet: IndexSet, rangeIndex: Int, rangeCount: Int, first : Bool) {
            self.indexSet = indexSet
            let extent = indexSet._range(at: rangeIndex)
            if first {
                self.value = extent.lowerBound
            } else {
                self.value = extent.upperBound-1
            }
            self.extent = extent
            self.rangeCount = rangeCount
            self.rangeIndex = rangeIndex
        }
        
        private init(indexSet: IndexSet, value: Int, extent: Range<Int>, rangeIndex: Int, rangeCount: Int) {
            self.indexSet = indexSet
            self.value = value
            self.extent = extent
            self.rangeCount = rangeCount
            self.rangeIndex = rangeIndex
        }
        
        private func successor() -> Index {
            if value + 1 == extent.upperBound {
                // Move to the next range
                if rangeIndex + 1 == rangeCount {
                    // We have no more to go; return a 'past the end' index
                    return Index(indexSet: indexSet, value: value + 1, extent: extent, rangeIndex: rangeIndex, rangeCount: rangeCount)
                } else {
                    return Index(indexSet: indexSet, rangeIndex: rangeIndex + 1, rangeCount: rangeCount, first: true)
                }
            } else {
                // Move to the next value in this range
                return Index(indexSet: indexSet, value: value + 1, extent: extent, rangeIndex: rangeIndex, rangeCount: rangeCount)
            }
        }
        
        private mutating func _successorInPlace() {
            if value + 1 == extent.upperBound {
                // Move to the next range
                if rangeIndex + 1 == rangeCount {
                    // We have no more to go; return a 'past the end' index
                    value += 1
                } else {
                    rangeIndex += 1
                    extent = indexSet._range(at: rangeIndex)
                    value = extent.lowerBound
                }
            } else {
                // Move to the next value in this range
                value += 1
            }
        }
        
        private func predecessor() -> Index {
            if value == extent.lowerBound {
                // Move to the next range
                if rangeIndex == 0 {
                    // We have no more to go
                    return Index(indexSet: indexSet, value: value, extent: extent, rangeIndex: rangeIndex, rangeCount: rangeCount)
                } else {
                    return Index(indexSet: indexSet, rangeIndex: rangeIndex - 1, rangeCount: rangeCount, first: false)
                }
            } else {
                // Move to the previous value in this range
                return Index(indexSet: indexSet, value: value - 1, extent: extent, rangeIndex: rangeIndex, rangeCount: rangeCount)
            }
        }
        
        public var description: String {
            return "index \(value) in a range of \(extent) [range #\(rangeIndex + 1)/\(rangeCount)]"
        }
        
        private mutating func _predecessorInPlace() {
            if value == extent.lowerBound {
                // Move to the next range
                if rangeIndex == 0 {
                    // We have no more to go
                } else {
                    rangeIndex -= 1
                    extent = indexSet._range(at: rangeIndex)
                    value = extent.upperBound - 1
                }
            } else {
                // Move to the previous value in this range
                value -= 1
            }
        }
    }

    public typealias ReferenceType = NSIndexSet
    public typealias Element = Int
    
    private var _handle: _MutablePairHandle<NSIndexSet, NSMutableIndexSet>
    
    /// Initialize an `IndexSet` with a range of integers.
    public init(integersIn range: Range<Element>) {
        _handle = _MutablePairHandle(NSIndexSet(indexesIn: _toNSRange(range)), copying: false)
    }
    
    /// Initialize an `IndexSet` with a single integer.
    public init(integer: Element) {
        _handle = _MutablePairHandle(NSIndexSet(index: integer), copying: false)
    }
    
    /// Initialize an empty `IndexSet`.
    public init() {
        _handle = _MutablePairHandle(NSIndexSet(), copying: false)
    }
    
    public var hashValue : Int {
        return _handle.map { $0.hash }
    }
    
    /// Returns the number of integers in `self`.
    public var count: Int {
        return _handle.map { $0.count }
    }
    
    public func makeIterator() -> IndexingIterator<IndexSet> {
        return IndexingIterator(_elements: self)
    }
    
    /// Returns a `Range`-based view of `self`.
    ///
    /// - parameter range: A subrange of `self` to view. The default value is `nil`, which means that the entire `IndexSet` is used.
    public func rangeView(of range : Range<Element>? = nil) -> RangeView {
        return RangeView(indexSet: self, intersecting: range)
    }
    
    private func _indexOfRange(containing integer : Element) -> RangeView.Index? {
        let result = _handle.map { 
            __NSIndexSetIndexOfRangeContainingIndex($0, UInt(integer))
        }
        if result == UInt(NSNotFound) {
            return nil
        } else {
            return Int(result)
        }
    }
    
    private func _range(at index: RangeView.Index) -> Range<Element> {
        return _handle.map {
            var location : UInt = 0
            var length : UInt = 0
            __NSIndexSetRangeAtIndex($0, UInt(index), &location, &length)
            return Int(location)..<Int(location)+Int(length)
        }
    }
    
    private var _rangeCount : Int {
        return _handle.map { 
            Int(__NSIndexSetRangeCount($0))
        }
    }
    
    public var startIndex: Index {
        // TODO: We should cache this result
        
        // If this winds up being NSNotFound, that's ok because then endIndex is also NSNotFound, and empty collections have startIndex == endIndex
        return Index(firstIn: self)
    }

    public var endIndex: Index {
        // TODO: We should cache this result
        
        return Index(lastIn: self)
    }
    
    public subscript(index : Index) -> Element {
        return index.value
    }

    public subscript(bounds: Range<Index>) -> BidirectionalSlice<IndexSet> {
        return BidirectionalSlice(base: self, bounds: bounds)
    }

    // We adopt the default implementation of subscript(range: Range<Index>) from MutableCollection
    
    private func _toOptional(_ x : Int) -> Int? {
        if x == NSNotFound { return nil } else { return x }
    }

    /// Returns the first integer in `self`, or nil if `self` is empty.
    public var first: Element? {
        return _handle.map { _toOptional($0.firstIndex) }
    }
    
    /// Returns the last integer in `self`, or nil if `self` is empty.
    public var last: Element? {
        return _handle.map { _toOptional($0.lastIndex) }
    }
    
    /// Returns an integer contained in `self` which is greater than `integer`.
    public func integerGreaterThan(_ integer: Element) -> Element {
        return _handle.map { $0.indexGreaterThanIndex(integer) }
    }
    
    /// Returns an integer contained in `self` which is less than `integer`.
    public func integerLessThan(_ integer: Element) -> Element {
        return _handle.map { $0.indexLessThanIndex(integer) }
    }
    
    /// Returns an integer contained in `self` which is greater than or equal to `integer`.
    public func integerGreaterThanOrEqualTo(_ integer: Element) -> Element {
        return _handle.map { $0.indexGreaterThanOrEqual(to: integer) }
    }
    
    /// Returns an integer contained in `self` which is less than or equal to `integer`.
    public func integerLessThanOrEqualTo(_ integer: Element) -> Element {
        return _handle.map { $0.indexLessThanOrEqual(to: integer) }
    }
    
    /// Return a `Range<IndexSet.Index>` which can be used to subscript the index set.
    ///
    /// The resulting range is the range of the intersection of the integers in `range` with the index set.
    ///
    /// - parameter range: The range of integers to include.
    public func indexRange(in range: Range<Element>) -> Range<Index> {
        if range.isEmpty {
            let i = Index(indexSet: self, index: 0)
            return i..<i
        }
        
        if range.lowerBound > last || (range.upperBound - 1) < first {
            let i = Index(indexSet: self, index: 0)
            return i..<i
        }
        
        let resultFirst = Index(indexSet: self, index: integerGreaterThanOrEqualTo(range.lowerBound))
        let resultLast = Index(indexSet: self, index: integerLessThanOrEqualTo(range.upperBound - 1))
        return resultFirst..<resultLast.successor()
    }

    /// Returns the count of integers in `self` that intersect `range`.
    public func count(in range: Range<Element>) -> Int {
        return _handle.map { $0.countOfIndexes(in: _toNSRange(range)) }
    }
    
    /// Returns `true` if `self` contains `integer`.
    public func contains(_ integer: Element) -> Bool {
        return _handle.map { $0.contains(integer) }
    }
    
    /// Returns `true` if `self` contains all of the integers in `range`.
    public func contains(integersIn range: Range<Element>) -> Bool {
        return _handle.map { $0.contains(in: _toNSRange(range)) }
    }
    
    /// Returns `true` if `self` contains any of the integers in `indexSet`.
    public func contains(integersIn indexSet: IndexSet) -> Bool {
        return _handle.map { $0.contains(indexSet) }
    }
    
    /// Returns `true` if `self` intersects any of the integers in `range`.
    public func intersects(integersIn range: Range<Element>) -> Bool {
        return _handle.map { $0.intersects(in: _toNSRange(range)) }
    }
    
    // MARK: -
    // Indexable
    
    public func index(after i: Index) -> Index {
        return i.successor()
    }
    
    public func formIndex(after i: inout Index) {
        i._successorInPlace()
    }
    
    public func index(before i: Index) -> Index {
        return i.predecessor()
    }
    
    public func formIndex(before i: inout Index) {
        i._predecessorInPlace()
    }
    
    // MARK: -
    // MARK: SetAlgebra
    
    /// Union the `IndexSet` with `other`.
    public mutating func formUnion(_ other: IndexSet) {
        self = self.union(other)
    }
    
    /// Union the `IndexSet` with `other`.
    public func union(_ other: IndexSet) -> IndexSet {
        // This algorithm is naïve but it works. We could avoid calling insert in some cases.
        
        var result = IndexSet()
        for r in self.rangeView() {
            result.insert(integersIn: Range(r))
        }
        
        for r in other.rangeView() {
            result.insert(integersIn: Range(r))
        }
        return result
    }
    
    /// Exclusive or the `IndexSet` with `other`.
    public func symmetricDifference(_ other: IndexSet) -> IndexSet {
        var result = IndexSet()
        var boundaryIterator = IndexSetBoundaryIterator(self, other)
        var flag = false
        var start = 0

        while let i = boundaryIterator.next() {
            if !flag {
                // Starting a range; if the edge is contained or not depends on the xor of this particular value.
                let startInclusive = self.contains(i) != other.contains(i)
                start = startInclusive ? i : i + 1
                flag = true
            } else {
                // Ending a range; if the edge is contained or not depends on the xor of this particular value.
                let endInclusive = self.contains(i) != other.contains(i)
                let end = endInclusive ? i + 1 : i
                if start < end {
                    // Otherwise, we had an empty range
                    result.insert(integersIn: start..<end)
                }
                flag = false
            }
            // We never have to worry about having flag set to false after exiting this loop because the iterator will always return an even number of results; ranges come in pairs, and we always alternate flag
        }
        
        return result
    }
    
    /// Exclusive or the `IndexSet` with `other`.
    public mutating func formSymmetricDifference(_ other: IndexSet) {
        self = self.symmetricDifference(other)
    }
    
    /// Intersect the `IndexSet` with `other`.
    public func intersection(_ other: IndexSet) -> IndexSet {
        var result = IndexSet()
        var boundaryIterator = IndexSetBoundaryIterator(self, other)
        var flag = false
        var start = 0
        
        while let i = boundaryIterator.next() {
            if !flag {
                // If both sets contain then start a range.
                if self.contains(i) && other.contains(i) {
                    flag = true
                    start = i
                }
            } else {
                // If both sets contain then end a range.
                if self.contains(i) && other.contains(i) {
                    flag = false
                    result.insert(integersIn: start..<(i + 1))
                }
            }
        }
        
        return result
    }

    /// Intersect the `IndexSet` with `other`.
    public mutating func formIntersection(_ other: IndexSet) {
        self = self.intersection(other)
    }

    /// Insert an integer into the `IndexSet`.
    @discardableResult
    public mutating func insert(_ integer: Element) -> (inserted: Bool, memberAfterInsert: Element) {
        _applyMutation { $0.add(integer) }
        // TODO: figure out how to return the truth here
        return (true, integer)
    }

    /// Insert an integer into the `IndexSet`.
    @discardableResult
    public mutating func update(with integer: Element) -> Element? {
        _applyMutation { $0.add(integer) }
        // TODO: figure out how to return the truth here
        return integer
    }


    /// Remove an integer from the `IndexSet`.
    @discardableResult
    public mutating func remove(_ integer: Element) -> Element? {
        // TODO: Add method to NSIndexSet to do this in one call
        let result : Element? = contains(integer) ? integer : nil
        _applyMutation { $0.remove(integer) }
        return result
    }

    // MARK: -
    
    /// Remove all values from the `IndexSet`.
    public mutating func removeAll() {
        _applyMutation { $0.removeAllIndexes() }
    }
    
    /// Insert a range of integers into the `IndexSet`.
    public mutating func insert(integersIn range: Range<Element>) {
        _applyMutation { $0.add(in: _toNSRange(range)) }
    }
    
    /// Remove a range of integers from the `IndexSet`.
    public mutating func remove(integersIn range: Range<Element>) {
        _applyMutation { $0.remove(in: _toNSRange(range)) }
    }
    
    /// Returns `true` if self contains no values.
    public var isEmpty : Bool {
        return self.count == 0
    }
    
    /// Returns an IndexSet filtered according to the result of `includeInteger`.
    ///
    /// - parameter range: A range of integers. For each integer in the range that intersects the integers in the IndexSet, then the `includeInteger predicate will be invoked. Pass `nil` (the default) to use the entire range.
    /// - parameter includeInteger: The predicate which decides if an integer will be included in the result or not.
    public func filteredIndexSet(in range : Range<Element>? = nil, includeInteger: @noescape (Element) throws -> Bool) rethrows -> IndexSet {
        let r : NSRange = range != nil ? _toNSRange(range!) : NSMakeRange(0, NSNotFound - 1) 
        return try _handle.map {
            var error : ErrorProtocol? = nil
            let result = $0.indexes(in: r, options: [], passingTest: { (i, stop) -> Bool in
                do {
                    let include = try includeInteger(i)
                    return include
                } catch let e {
                    error = e
                    stop.pointee = true
                    return false
                }
            }) as IndexSet
            if let e = error {
                throw e
            } else {
                return result
            }
        }
    }
    
    /// For a positive delta, shifts the indexes in [index, INT_MAX] to the right, thereby inserting an "empty space" [index, delta], for a negative delta, shifts the indexes in [index, INT_MAX] to the left, thereby deleting the indexes in the range [index - delta, delta].
    public mutating func shift(startingAt integer: Element, by delta: IndexSet.IndexDistance) {
        _applyMutation { $0.shiftIndexesStarting(at: integer, by: delta) }
    }

    public var description: String {
        return _handle.map { $0.description }
    }
    
    public var debugDescription: String {
        return _handle.map { $0.debugDescription }
    }
    
    // Temporary boxing function, until we can get a native Swift type for NSIndexSet
    @inline(__always)
    mutating func _applyMutation<ReturnType>(_ whatToDo : @noescape (NSMutableIndexSet) throws -> ReturnType) rethrows -> ReturnType {
        // This check is done twice because: <rdar://problem/24939065> Value kept live for too long causing uniqueness check to fail
        var unique = true
        switch _handle._pointer {
        case .Default(_):
            break
        case .Mutable(_):
            unique = isUniquelyReferencedNonObjC(&_handle)
        }
        
        switch _handle._pointer {
        case .Default(let i):
            // We need to become mutable; by creating a new box we also become unique
            let copy = i.mutableCopy() as! NSMutableIndexSet
            // Be sure to set the _handle before calling out; otherwise references to the struct in the closure may be looking at the old _handle
            _handle = _MutablePairHandle(copy, copying: false)
            let result = try whatToDo(copy)
            return result
        case .Mutable(let m):
            // Only create a new box if we are not uniquely referenced
            if !unique {
                let copy = m.mutableCopy() as! NSMutableIndexSet
                _handle = _MutablePairHandle(copy, copying: false)
                let result = try whatToDo(copy)
                return result
            } else {
                return try whatToDo(m)
            }
        }
    }
    
    // MARK: - Bridging Support
    
    private var reference: NSIndexSet {
        return _handle.reference
    }
    
    private init(reference: NSIndexSet) {
        _handle = _MutablePairHandle(reference)
    }
}

/// Iterate two index sets on the boundaries of their ranges. This is where all of the interesting stuff happens for exclusive or, intersect, etc.
private struct IndexSetBoundaryIterator : IteratorProtocol {
    private typealias Element = IndexSet.Element
    
    private var i1 : IndexSet.RangeView.Iterator
    private var i2 : IndexSet.RangeView.Iterator
    private var i1Range : CountableRange<Element>?
    private var i2Range : CountableRange<Element>?
    private var i1UsedFirst : Bool
    private var i2UsedFirst : Bool
    
    private init(_ is1 : IndexSet, _ is2 : IndexSet) {
        i1 = is1.rangeView().makeIterator()
        i2 = is2.rangeView().makeIterator()
        
        i1Range = i1.next()
        i2Range = i2.next()
        
        // A sort of cheap iterator on [i1Range.first, i1Range.last]
        i1UsedFirst = false
        i2UsedFirst = false
    }
    
    private mutating func next() -> Element? {
        if i1Range == nil && i2Range == nil {
            return nil
        }
        
        let nextIn1 : Element
        if let r = i1Range {
            nextIn1 = i1UsedFirst ? r.last! : r.first!
        } else {
            nextIn1 = Int.max
        }
        
        let nextIn2 : Element
        if let r = i2Range {
            nextIn2 = i2UsedFirst ? r.last! : r.first!
        } else {
            nextIn2 = Int.max
        }
        
        var result : Element
        if nextIn1 <= nextIn2 {
            // 1 has the next element, or they are the same. We need to iterate both the value from is1 and is2 in the == case.
            result = nextIn1
            if i1UsedFirst { i1Range = i1.next() }
            i1UsedFirst = !i1UsedFirst
        } else {
            // 2 has the next element
            result = nextIn2
            if i2UsedFirst { i2Range = i2.next() }
            i2UsedFirst = !i2UsedFirst
        }
        
        return result
    }
}

public func ==(lhs: IndexSet, rhs: IndexSet) -> Bool {
    return lhs._handle.map { $0.isEqual(to: rhs) }
}

private func _toNSRange(_ r : Range<IndexSet.Element>) -> NSRange {
    return NSMakeRange(r.lowerBound, r.upperBound - r.lowerBound)
}

extension IndexSet : _ObjectiveCBridgeable {
    public static func _isBridgedToObjectiveC() -> Bool {
        return true
    }
    
    public static func _getObjectiveCType() -> Any.Type {
        return NSIndexSet.self
    }
    
    @_semantics("convertToObjectiveC")
    public func _bridgeToObjectiveC() -> NSIndexSet {
        return reference
    }
    
    public static func _forceBridgeFromObjectiveC(_ x: NSIndexSet, result: inout IndexSet?) {
        result = IndexSet(reference: x)
    }
    
    public static func _conditionallyBridgeFromObjectiveC(_ x: NSIndexSet, result: inout IndexSet?) -> Bool {
        result = IndexSet(reference: x)
        return true
    }

    public static func _unconditionallyBridgeFromObjectiveC(_ source: NSIndexSet?) -> IndexSet {
        return IndexSet(reference: source!)
    }    
    
}

@_silgen_name("__NSIndexSetRangeCount")
internal func __NSIndexSetRangeCount(_ indexSet: NSIndexSet) -> UInt

@_silgen_name("__NSIndexSetRangeAtIndex")
internal func __NSIndexSetRangeAtIndex(_ indexSet: NSIndexSet, _ index: UInt, _ location : UnsafeMutablePointer<UInt>, _ length : UnsafeMutablePointer<UInt>)

@_silgen_name("__NSIndexSetIndexOfRangeContainingIndex")
internal func __NSIndexSetIndexOfRangeContainingIndex(_ indexSet: NSIndexSet, _ index: UInt) -> UInt

// MARK: Protocol

// TODO: This protocol should be replaced with a native Swift object like the other Foundation bridged types. However, NSIndexSet does not have an abstract zero-storage base class like NSCharacterSet, NSData, and NSAttributedString. Therefore the same trick of laying it out with Swift ref counting does not work.and
/// Holds either the immutable or mutable version of a Foundation type.
///
/// In many cases, the immutable type has optimizations which make it preferred when we know we do not need mutation.
private enum _MutablePair<ImmutableType, MutableType> {
    case Default(ImmutableType)
    case Mutable(MutableType)
}

/// A class type which acts as a handle (pointer-to-pointer) to a Foundation reference type which has both an immutable and mutable class (e.g., NSData, NSMutableData).
///
/// a.k.a. Box
private final class _MutablePairHandle<ImmutableType : NSObject, MutableType : NSObject where ImmutableType : NSMutableCopying, MutableType : NSMutableCopying> {
    private var _pointer: _MutablePair<ImmutableType, MutableType>
    
    /// Initialize with an immutable reference instance.
    ///
    /// - parameter immutable: The thing to stash.
    /// - parameter copying: Should be true unless you just created the instance (or called copy) and want to transfer ownership to this handle.
    init(_ immutable : ImmutableType, copying : Bool = true) {
        if copying {
            self._pointer = _MutablePair.Default(immutable.copy() as! ImmutableType)
        } else {
            self._pointer = _MutablePair.Default(immutable)
        }
    }
    
    /// Initialize with a mutable reference instance.
    ///
    /// - parameter mutable: The thing to stash.
    /// - parameter copying: Should be true unless you just created the instance (or called copy) and want to transfer ownership to this handle.
    init(_ mutable : MutableType, copying : Bool = true) {
        if copying {
            self._pointer = _MutablePair.Mutable(mutable.mutableCopy() as! MutableType)
        } else {
            self._pointer = _MutablePair.Mutable(mutable)
        }
    }
    
    /// Apply a closure to the reference type, regardless if it is mutable or immutable.
    @inline(__always)
    func map<ReturnType>(_ whatToDo : @noescape (ImmutableType) throws -> ReturnType) rethrows -> ReturnType {
        switch _pointer {
        case .Default(let i):
            return try whatToDo(i)
        case .Mutable(let m):
            // TODO: It should be possible to reflect the constraint that MutableType is a subtype of ImmutableType in the generics for the class, but I haven't figured out how yet. For now, cheat and unsafe bit cast.
            return try whatToDo(unsafeBitCast(m, to: ImmutableType.self))
        }
    }
    
    var reference : ImmutableType {
        switch _pointer {
        case .Default(let i):
            return i
        case .Mutable(let m):
            // TODO: It should be possible to reflect the constraint that MutableType is a subtype of ImmutableType in the generics for the class, but I haven't figured out how yet. For now, cheat and unsafe bit cast.
            return unsafeBitCast(m, to: ImmutableType.self)
        }
    }
}
