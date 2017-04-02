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

@_exported import Foundation // Clang module
import _SwiftFoundationOverlayShims

extension IndexSet.Index {
    public static func ==(lhs: IndexSet.Index, rhs: IndexSet.Index) -> Bool {
        return lhs.value == rhs.value && rhs.rangeIndex == rhs.rangeIndex
    }

    public static func <(lhs: IndexSet.Index, rhs: IndexSet.Index) -> Bool {
        return lhs.value < rhs.value && rhs.rangeIndex <= rhs.rangeIndex
    }

    public static func <=(lhs: IndexSet.Index, rhs: IndexSet.Index) -> Bool {
        return lhs.value <= rhs.value && rhs.rangeIndex <= rhs.rangeIndex
    }

    public static func >(lhs: IndexSet.Index, rhs: IndexSet.Index) -> Bool {
        return lhs.value > rhs.value && rhs.rangeIndex >= rhs.rangeIndex
    }

    public static func >=(lhs: IndexSet.Index, rhs: IndexSet.Index) -> Bool {
        return lhs.value >= rhs.value && rhs.rangeIndex >= rhs.rangeIndex
    }
}

extension IndexSet.RangeView {
    public static func ==(lhs: IndexSet.RangeView, rhs: IndexSet.RangeView) -> Bool {
        return lhs.startIndex == rhs.startIndex && lhs.endIndex == rhs.endIndex && lhs.indexSet == rhs.indexSet
    }
}

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
        public let startIndex: Index
        public let endIndex: Index
        
        fileprivate var indexSet: IndexSet
        
        // Range of element values
        private var intersectingRange : Range<IndexSet.Element>?
        
        fileprivate init(indexSet : IndexSet, intersecting range : Range<IndexSet.Element>?) {
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
    
    /// The mechanism for accessing the integers stored in an IndexSet.
    public struct Index : CustomStringConvertible, Comparable {
        fileprivate var value: IndexSet.Element
        fileprivate var extent: Range<IndexSet.Element>
        fileprivate var rangeIndex: Int
        fileprivate let rangeCount: Int
        
        fileprivate init(value: Int, extent: Range<Int>, rangeIndex: Int, rangeCount: Int) {
            self.value = value
            self.extent = extent
            self.rangeCount = rangeCount
            self.rangeIndex = rangeIndex
        }
        
        public var description: String {
            return "index \(value) in a range of \(extent) [range #\(rangeIndex + 1)/\(rangeCount)]"
        }
    }

    public typealias ReferenceType = NSIndexSet
    public typealias Element = Int
    
    fileprivate var _handle: _MutablePairHandle<NSIndexSet, NSMutableIndexSet>
    
    /// Initialize an `IndexSet` with a range of integers.
    public init(integersIn range: Range<Element>) {
        _handle = _MutablePairHandle(NSIndexSet(indexesIn: _toNSRange(range)), copying: false)
    }
    
    /// Initialize an `IndexSet` with a range of integers.
    public init(integersIn range: ClosedRange<Element>) { self.init(integersIn: Range(range)) }
    /// Initialize an `IndexSet` with a range of integers.
    public init(integersIn range: CountableClosedRange<Element>) { self.init(integersIn: Range(range)) }
    /// Initialize an `IndexSet` with a range of integers.
    public init(integersIn range: CountableRange<Element>) { self.init(integersIn: Range(range)) }
    
    /// Initialize an `IndexSet` with a single integer.
    public init(integer: Element) {
        _handle = _MutablePairHandle(NSIndexSet(index: integer), copying: false)
    }
    
    /// Initialize an empty `IndexSet`.
    public init() {
        _handle = _MutablePairHandle(NSIndexSet(), copying: false)
    }
    
    public var hashValue: Int {
        return _handle.map { $0.hash }
    }
    
    /// Returns the number of integers in `self`.
    public var count: Int {
        return _handle.map { $0.count }
    }
    
    public func makeIterator() -> IndexingIterator<IndexSet> {
        return IndexingIterator(_elements: self)
    }

    /// Returns a `Range`-based view of the entire contents of `self`.
    ///
    /// - seealso: rangeView(of:)
    public var rangeView: RangeView {
        return RangeView(indexSet: self, intersecting: nil)
    }

    /// Returns a `Range`-based view of `self`.
    ///
    /// - parameter range: A subrange of `self` to view.
    public func rangeView(of range : Range<Element>) -> RangeView {
        return RangeView(indexSet: self, intersecting: range)
    }
    
    /// Returns a `Range`-based view of `self`.
    ///
    /// - parameter range: A subrange of `self` to view.
    public func rangeView(of range : ClosedRange<Element>) -> RangeView { return self.rangeView(of: Range(range)) }
    /// Returns a `Range`-based view of `self`.
    ///
    /// - parameter range: A subrange of `self` to view.
    public func rangeView(of range : CountableClosedRange<Element>) -> RangeView { return self.rangeView(of: Range(range)) }
    /// Returns a `Range`-based view of `self`.
    ///
    /// - parameter range: A subrange of `self` to view.
    public func rangeView(of range : CountableRange<Element>) -> RangeView { return self.rangeView(of: Range(range)) }

    
    private func _indexOfRange(containing integer : Element) -> RangeView.Index? {
        let result = _handle.map { 
            __NSIndexSetIndexOfRangeContainingIndex($0, integer)
        }
        if result == NSNotFound {
            return nil
        } else {
            return Int(result)
        }
    }
    
    private func _range(at index: RangeView.Index) -> Range<Element> {
        return _handle.map {
            var location: Int = 0
            var length: Int = 0
            __NSIndexSetRangeAtIndex($0, index, &location, &length)
            return Int(location)..<Int(location)+Int(length)
        }
    }
    
    private var _rangeCount : Int {
        return _handle.map { 
            Int(__NSIndexSetRangeCount($0))
        }
    }
    
    public var startIndex: Index {
        // If this winds up being NSNotFound, that's ok because then endIndex is also NSNotFound, and empty collections have startIndex == endIndex
        let extent = _range(at: 0)
        return Index(value: extent.lowerBound, extent: extent, rangeIndex: 0, rangeCount: _rangeCount)
    }

    public var endIndex: Index {
        let rangeCount = _rangeCount
        let rangeIndex = rangeCount - 1
        let extent: Range<Int>
        let value: Int
        if rangeCount > 0 {
            extent = _range(at: rangeCount - 1)
            value = extent.upperBound // "1 past the end" position is the last range, 1 + the end of that range's extent
        } else {
            extent = 0..<0
            value = 0
        }
        
        return Index(value: value, extent: extent, rangeIndex: rangeIndex, rangeCount: rangeCount)
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
    
    /// Returns an integer contained in `self` which is greater than `integer`, or `nil` if a result could not be found.
    public func integerGreaterThan(_ integer: Element) -> Element? {
        return _handle.map { _toOptional($0.indexGreaterThanIndex(integer)) }
    }
    
    /// Returns an integer contained in `self` which is less than `integer`, or `nil` if a result could not be found.
    public func integerLessThan(_ integer: Element) -> Element? {
        return _handle.map { _toOptional($0.indexLessThanIndex(integer)) }
    }
    
    /// Returns an integer contained in `self` which is greater than or equal to `integer`, or `nil` if a result could not be found.
    public func integerGreaterThanOrEqualTo(_ integer: Element) -> Element? {
        return _handle.map { _toOptional($0.indexGreaterThanOrEqual(to: integer)) }
    }
    
    /// Returns an integer contained in `self` which is less than or equal to `integer`, or `nil` if a result could not be found.
    public func integerLessThanOrEqualTo(_ integer: Element) -> Element? {
        return _handle.map { _toOptional($0.indexLessThanOrEqual(to: integer)) }
    }
    
    /// Return a `Range<IndexSet.Index>` which can be used to subscript the index set.
    ///
    /// The resulting range is the range of the intersection of the integers in `range` with the index set. The resulting range will be `isEmpty` if the intersection is empty.
    ///
    /// - parameter range: The range of integers to include.
    public func indexRange(in range: Range<Element>) -> Range<Index> {
        guard !range.isEmpty, let first = first, let last = last else {
            let i = _index(ofInteger: 0)
            return i..<i
        }

        if range.lowerBound > last || (range.upperBound - 1) < first {
            let i = _index(ofInteger: 0)
            return i..<i
        }
        
        if let start = integerGreaterThanOrEqualTo(range.lowerBound), let end = integerLessThanOrEqualTo(range.upperBound - 1) {
            let resultFirst = _index(ofInteger: start)
            let resultLast = _index(ofInteger: end)
            return resultFirst..<index(after: resultLast)
        } else {
            let i = _index(ofInteger: 0)
            return i..<i
        }
    }
    
    /// Return a `Range<IndexSet.Index>` which can be used to subscript the index set.
    ///
    /// The resulting range is the range of the intersection of the integers in `range` with the index set.
    ///
    /// - parameter range: The range of integers to include.
    public func indexRange(in range: CountableRange<Element>) -> Range<Index> { return self.indexRange(in: Range(range)) }
    /// Return a `Range<IndexSet.Index>` which can be used to subscript the index set.
    ///
    /// The resulting range is the range of the intersection of the integers in `range` with the index set.
    ///
    /// - parameter range: The range of integers to include.
    public func indexRange(in range: ClosedRange<Element>) -> Range<Index> { return self.indexRange(in: Range(range)) }
    /// Return a `Range<IndexSet.Index>` which can be used to subscript the index set.
    ///
    /// The resulting range is the range of the intersection of the integers in `range` with the index set.
    ///
    /// - parameter range: The range of integers to include.
    public func indexRange(in range: CountableClosedRange<Element>) -> Range<Index> { return self.indexRange(in: Range(range)) }


    /// Returns the count of integers in `self` that intersect `range`.
    public func count(in range: Range<Element>) -> Int {
        return _handle.map { $0.countOfIndexes(in: _toNSRange(range)) }
    }

    /// Returns the count of integers in `self` that intersect `range`.
    public func count(in range: CountableRange<Element>) -> Int { return self.count(in: Range(range)) }
    /// Returns the count of integers in `self` that intersect `range`.
    public func count(in range: ClosedRange<Element>) -> Int { return self.count(in: Range(range)) }
    /// Returns the count of integers in `self` that intersect `range`.
    public func count(in range: CountableClosedRange<Element>) -> Int { return self.count(in: Range(range)) }

    /// Returns `true` if `self` contains `integer`.
    public func contains(_ integer: Element) -> Bool {
        return _handle.map { $0.contains(integer) }
    }
    
    /// Returns `true` if `self` contains all of the integers in `range`.
    public func contains(integersIn range: Range<Element>) -> Bool {
        return _handle.map { $0.contains(in: _toNSRange(range)) }
    }

    /// Returns `true` if `self` contains all of the integers in `range`.
    public func contains(integersIn range: CountableRange<Element>) -> Bool { return self.contains(integersIn: Range(range)) }
    /// Returns `true` if `self` contains all of the integers in `range`.
    public func contains(integersIn range: ClosedRange<Element>) -> Bool { return self.contains(integersIn: Range(range)) }
    /// Returns `true` if `self` contains all of the integers in `range`.
    public func contains(integersIn range: CountableClosedRange<Element>) -> Bool { return self.contains(integersIn: Range(range)) }

    
    /// Returns `true` if `self` contains all of the integers in `indexSet`.
    public func contains(integersIn indexSet: IndexSet) -> Bool {
        return _handle.map { $0.contains(indexSet) }
    }
    
    /// Returns `true` if `self` intersects any of the integers in `range`.
    public func intersects(integersIn range: Range<Element>) -> Bool {
        return _handle.map { $0.intersects(in: _toNSRange(range)) }
    }

    /// Returns `true` if `self` intersects any of the integers in `range`.
    public func intersects(integersIn range: CountableRange<Element>) -> Bool { return self.intersects(integersIn: Range(range)) }
    /// Returns `true` if `self` intersects any of the integers in `range`.
    public func intersects(integersIn range: ClosedRange<Element>) -> Bool { return self.intersects(integersIn: Range(range)) }
    /// Returns `true` if `self` intersects any of the integers in `range`.
    public func intersects(integersIn range: CountableClosedRange<Element>) -> Bool { return self.intersects(integersIn: Range(range)) }

    // MARK: -
    // Indexable
    
    public func index(after i: Index) -> Index {
        if i.value + 1 == i.extent.upperBound {
            // Move to the next range
            if i.rangeIndex + 1 == i.rangeCount {
                // We have no more to go; return a 'past the end' index
                return Index(value: i.value + 1, extent: i.extent, rangeIndex: i.rangeIndex, rangeCount: i.rangeCount)
            } else {
                let rangeIndex = i.rangeIndex + 1
                let rangeCount = i.rangeCount
                let extent = _range(at: rangeIndex)
                let value = extent.lowerBound
                return Index(value: value, extent: extent, rangeIndex: rangeIndex, rangeCount: rangeCount)
            }
        } else {
            // Move to the next value in this range
            return Index(value: i.value + 1, extent: i.extent, rangeIndex: i.rangeIndex, rangeCount: i.rangeCount)
        }
    }
    
    public func formIndex(after i: inout Index) {
        if i.value + 1 == i.extent.upperBound {
            // Move to the next range
            if i.rangeIndex + 1 == i.rangeCount {
                // We have no more to go; return a 'past the end' index
                i.value += 1
            } else {
                i.rangeIndex += 1
                i.extent = _range(at: i.rangeIndex)
                i.value = i.extent.lowerBound
            }
        } else {
            // Move to the next value in this range
            i.value += 1
        }
    }
    
    public func index(before i: Index) -> Index {
        if i.value == i.extent.lowerBound {
            // Move to the next range
            if i.rangeIndex == 0 {
                // We have no more to go
                return Index(value: i.value, extent: i.extent, rangeIndex: i.rangeIndex, rangeCount: i.rangeCount)
            } else {
                let rangeIndex = i.rangeIndex - 1
                let rangeCount = i.rangeCount
                let extent = _range(at: rangeIndex)
                let value = extent.upperBound - 1
                return Index(value: value, extent: extent, rangeIndex: rangeIndex, rangeCount: rangeCount)
            }
        } else {
            // Move to the previous value in this range
            return Index(value: i.value - 1, extent: i.extent, rangeIndex: i.rangeIndex, rangeCount: i.rangeCount)
        }
    }
    
    public func formIndex(before i: inout Index) {
        if i.value == i.extent.lowerBound {
            // Move to the next range
            if i.rangeIndex == 0 {
                // We have no more to go
            } else {
                i.rangeIndex -= 1
                i.extent = _range(at: i.rangeIndex)
                i.value = i.extent.upperBound - 1
            }
        } else {
            // Move to the previous value in this range
            i.value -= 1
        }
    }
    
    private func _index(ofInteger integer: Element) -> Index {
        let rangeCount = _rangeCount
        let value = integer
        if let rangeIndex = _indexOfRange(containing: integer) {
            let extent = _range(at: rangeIndex)
            let rangeIndex = rangeIndex
            return Index(value: value, extent: extent, rangeIndex: rangeIndex, rangeCount: rangeCount)
        } else {
            let extent = 0..<0
            let rangeIndex = 0
            return Index(value: value, extent: Range(extent), rangeIndex: rangeIndex, rangeCount: rangeCount)
        }
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
        for r in self.rangeView {
            result.insert(integersIn: Range(r))
        }
        
        for r in other.rangeView {
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
                // Start a range if one set contains but not the other.
                if self.contains(i) != other.contains(i) {
                    flag = true
                    start = i
                }
            } else {
                // End a range if both sets contain or both sets do not contain.
                if self.contains(i) == other.contains(i) {
                    flag = false
                    result.insert(integersIn: start..<i)
                }
            }
            // We never have to worry about having flag set to false after exiting this loop because the last boundary is guaranteed to be past the end of ranges in both index sets
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
                // If both sets do not contain then end a range.
                if !self.contains(i) || !other.contains(i) {
                    flag = false
                    result.insert(integersIn: start..<i)
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

    /// Insert a range of integers into the `IndexSet`.
    public mutating func insert(integersIn range: CountableRange<Element>) { self.insert(integersIn: Range(range)) }
    /// Insert a range of integers into the `IndexSet`.
    public mutating func insert(integersIn range: ClosedRange<Element>) { self.insert(integersIn: Range(range)) }
    /// Insert a range of integers into the `IndexSet`.
    public mutating func insert(integersIn range: CountableClosedRange<Element>) { self.insert(integersIn: Range(range)) }

    /// Remove a range of integers from the `IndexSet`.
    public mutating func remove(integersIn range: Range<Element>) {
        _applyMutation { $0.remove(in: _toNSRange(range)) }
    }
    
    /// Remove a range of integers from the `IndexSet`.
    public mutating func remove(integersIn range: CountableRange<Element>) { self.remove(integersIn: Range(range)) }
    /// Remove a range of integers from the `IndexSet`.
    public mutating func remove(integersIn range: ClosedRange<Element>) { self.remove(integersIn: Range(range)) }
    /// Remove a range of integers from the `IndexSet`.
    public mutating func remove(integersIn range: CountableClosedRange<Element>) { self.remove(integersIn: Range(range)) }

    /// Returns `true` if self contains no values.
    public var isEmpty : Bool {
        return self.count == 0
    }
    
    /// Returns an IndexSet filtered according to the result of `includeInteger`.
    ///
    /// - parameter range: A range of integers. For each integer in the range that intersects the integers in the IndexSet, then the `includeInteger` predicate will be invoked.
    /// - parameter includeInteger: The predicate which decides if an integer will be included in the result or not.
    public func filteredIndexSet(in range : Range<Element>, includeInteger: (Element) throws -> Bool) rethrows -> IndexSet {
        let r : NSRange = _toNSRange(range)
        return try _handle.map {
            var error: Error?
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
    
    /// Returns an IndexSet filtered according to the result of `includeInteger`.
    ///
    /// - parameter range: A range of integers. For each integer in the range that intersects the integers in the IndexSet, then the `includeInteger` predicate will be invoked.
    /// - parameter includeInteger: The predicate which decides if an integer will be included in the result or not.
    public func filteredIndexSet(in range : CountableRange<Element>, includeInteger: (Element) throws -> Bool) rethrows -> IndexSet { return try self.filteredIndexSet(in: Range(range), includeInteger: includeInteger) }
    /// Returns an IndexSet filtered according to the result of `includeInteger`.
    ///
    /// - parameter range: A range of integers. For each integer in the range that intersects the integers in the IndexSet, then the `includeInteger` predicate will be invoked.
    /// - parameter includeInteger: The predicate which decides if an integer will be included in the result or not.
    public func filteredIndexSet(in range : ClosedRange<Element>, includeInteger: (Element) throws -> Bool) rethrows -> IndexSet { return try self.filteredIndexSet(in: Range(range), includeInteger: includeInteger) }
    /// Returns an IndexSet filtered according to the result of `includeInteger`.
    ///
    /// - parameter range: A range of integers. For each integer in the range that intersects the integers in the IndexSet, then the `includeInteger` predicate will be invoked.
    /// - parameter includeInteger: The predicate which decides if an integer will be included in the result or not.
    public func filteredIndexSet(in range : CountableClosedRange<Element>, includeInteger: (Element) throws -> Bool) rethrows -> IndexSet { return try self.filteredIndexSet(in: Range(range), includeInteger: includeInteger) }
    
    /// Returns an IndexSet filtered according to the result of `includeInteger`.
    ///
    /// - parameter includeInteger: The predicate which decides if an integer will be included in the result or not.
    public func filteredIndexSet(includeInteger: (Element) throws -> Bool) rethrows -> IndexSet {
        return try self.filteredIndexSet(in: 0..<NSNotFound-1, includeInteger: includeInteger)
    }

    /// For a positive delta, shifts the indexes in [index, INT_MAX] to the right, thereby inserting an "empty space" [index, delta], for a negative delta, shifts the indexes in [index, INT_MAX] to the left, thereby deleting the indexes in the range [index - delta, delta].
    public mutating func shift(startingAt integer: Element, by delta: IndexSet.IndexDistance) {
        _applyMutation { $0.shiftIndexesStarting(at: integer, by: delta) }
    }
    
    // Temporary boxing function, until we can get a native Swift type for NSIndexSet
    @inline(__always)
    mutating func _applyMutation<ReturnType>(_ whatToDo : (NSMutableIndexSet) throws -> ReturnType) rethrows -> ReturnType {
        // This check is done twice because: <rdar://problem/24939065> Value kept live for too long causing uniqueness check to fail
        var unique = true
        switch _handle._pointer {
        case .Default(_):
            break
        case .Mutable(_):
            unique = isKnownUniquelyReferenced(&_handle)
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
    
    fileprivate var reference: NSIndexSet {
        return _handle.reference
    }
    
    fileprivate init(reference: NSIndexSet) {
        _handle = _MutablePairHandle(reference)
    }
}

extension IndexSet : CustomStringConvertible, CustomDebugStringConvertible, CustomReflectable {
    public var description: String {
        return "\(count) indexes"
    }
    
    public var debugDescription: String {
        return "\(count) indexes"
    }

    public var customMirror: Mirror {
        var c: [(label: String?, value: Any)] = []
        c.append((label: "ranges", value: Array(rangeView)))
        return Mirror(self, children: c, displayStyle: Mirror.DisplayStyle.struct)
    }
}

/// Iterate two index sets on the boundaries of their ranges. This is where all of the interesting stuff happens for exclusive or, intersect, etc.
private struct IndexSetBoundaryIterator : IteratorProtocol {
    typealias Element = IndexSet.Element
    
    private var i1: IndexSet.RangeView.Iterator
    private var i2: IndexSet.RangeView.Iterator
    private var i1Range: CountableRange<Element>?
    private var i2Range: CountableRange<Element>?
    private var i1UsedLower: Bool
    private var i2UsedLower: Bool
    
    fileprivate init(_ is1: IndexSet, _ is2: IndexSet) {
        i1 = is1.rangeView.makeIterator()
        i2 = is2.rangeView.makeIterator()
        
        i1Range = i1.next()
        i2Range = i2.next()
        
        // A sort of cheap iterator on [i1Range.lowerBound, i1Range.upperBound]
        i1UsedLower = false
        i2UsedLower = false
    }
    
    fileprivate mutating func next() -> Element? {
        if i1Range == nil && i2Range == nil {
            return nil
        }
        
        let nextIn1: Element
        if let r = i1Range {
            nextIn1 = i1UsedLower ? r.upperBound : r.lowerBound
        } else {
            nextIn1 = Int.max
        }
        
        let nextIn2: Element
        if let r = i2Range {
            nextIn2 = i2UsedLower ? r.upperBound : r.lowerBound
        } else {
            nextIn2 = Int.max
        }
        
        var result: Element
        if nextIn1 <= nextIn2 {
            // 1 has the next element, or they are the same.
            result = nextIn1
            if i1UsedLower { i1Range = i1.next() }
            // We need to iterate both the value from is1 and is2 in the == case.
            if result == nextIn2 {
                if i2UsedLower { i2Range = i2.next() }
                i2UsedLower = !i2UsedLower
            }
            i1UsedLower = !i1UsedLower
        } else {
            // 2 has the next element
            result = nextIn2
            if i2UsedLower { i2Range = i2.next() }
            i2UsedLower = !i2UsedLower
        }
        
        return result
    }
}

extension IndexSet {
    public static func ==(lhs: IndexSet, rhs: IndexSet) -> Bool {
        return lhs._handle.map { $0.isEqual(to: rhs) }
    }
}

private func _toNSRange(_ r: Range<IndexSet.Element>) -> NSRange {
    return NSMakeRange(r.lowerBound, r.upperBound - r.lowerBound)
}

extension IndexSet : _ObjectiveCBridgeable {
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

extension NSIndexSet : _HasCustomAnyHashableRepresentation {
    // Must be @nonobjc to avoid infinite recursion during bridging.
    @nonobjc
    public func _toCustomAnyHashable() -> AnyHashable? {
        return AnyHashable(self as IndexSet)
    }
}

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
private final class _MutablePairHandle<ImmutableType : NSObject, MutableType : NSObject>
  where ImmutableType : NSMutableCopying, MutableType : NSMutableCopying {
    fileprivate var _pointer: _MutablePair<ImmutableType, MutableType>
    
    /// Initialize with an immutable reference instance.
    ///
    /// - parameter immutable: The thing to stash.
    /// - parameter copying: Should be true unless you just created the instance (or called copy) and want to transfer ownership to this handle.
    init(_ immutable: ImmutableType, copying: Bool = true) {
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
    init(_ mutable: MutableType, copying: Bool = true) {
        if copying {
            self._pointer = _MutablePair.Mutable(mutable.mutableCopy() as! MutableType)
        } else {
            self._pointer = _MutablePair.Mutable(mutable)
        }
    }
    
    /// Apply a closure to the reference type, regardless if it is mutable or immutable.
    @inline(__always)
    func map<ReturnType>(_ whatToDo: (ImmutableType) throws -> ReturnType) rethrows -> ReturnType {
        switch _pointer {
        case .Default(let i):
            return try whatToDo(i)
        case .Mutable(let m):
            // TODO: It should be possible to reflect the constraint that MutableType is a subtype of ImmutableType in the generics for the class, but I haven't figured out how yet. For now, cheat and unsafe bit cast.
            return try whatToDo(unsafeDowncast(m, to: ImmutableType.self))
        }
    }
    
    var reference: ImmutableType {
        switch _pointer {
        case .Default(let i):
            return i
        case .Mutable(let m):
            // TODO: It should be possible to reflect the constraint that MutableType is a subtype of ImmutableType in the generics for the class, but I haven't figured out how yet. For now, cheat and unsafe bit cast.
            return unsafeDowncast(m, to: ImmutableType.self)
        }
    }
}
