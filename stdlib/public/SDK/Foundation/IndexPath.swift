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

// Implementation note: NSIndexPath is an efficient array of integers for Objective-C.
// For Swift, we bridge to this wrapper of Array<Int>. This gives us great Swift performance and interop with Objective-C.

/**
 `IndexPath` represents the path to a specific node in a tree of nested array collections.
 
 Each index in an index path represents the index into an array of children from one node in the tree to another, deeper, node.
*/
public struct IndexPath : ReferenceConvertible, Equatable, Hashable, MutableCollection, RandomAccessCollection, Comparable, ExpressibleByArrayLiteral {
    public typealias ReferenceType = NSIndexPath
    public typealias Element = Int
    public typealias Index = Array<Int>.Index
    public typealias Indices = DefaultRandomAccessIndices<IndexPath>
    
    fileprivate var _indexes : Array<Int>
    
    /// Initialize an empty index path.
    public init() {
        _indexes = []
    }
    
    /// Initialize with a sequence of integers.
    public init<ElementSequence : Sequence>(indexes: ElementSequence)
      where ElementSequence.Iterator.Element == Element {
        _indexes = Array(indexes)
    }
    
    /// Initialize with an array literal.
    public init(arrayLiteral indexes: Element...) {
        _indexes = indexes
    }

    /// Initialize with an array of elements.
    public init(indexes: Array<Element>) {
        _indexes = indexes
    }

    /// Initialize with a single element.
    public init(index: Element) {
        _indexes = [index]
    }
    
    /// Return a new `IndexPath` containing all but the last element.
    public func dropLast() -> IndexPath {
        return IndexPath(indexes: _indexes.dropLast())
    }
    
    /// Append an `IndexPath` to `self`.
    public mutating func append(_ other: IndexPath) {
        _indexes.append(contentsOf: other._indexes)
    }
    
    /// Append a single element to `self`.
    public mutating func append(_ other: Element) {
        _indexes.append(other)
    }

    /// Append an array of elements to `self`.
    public mutating func append(_ other: Array<Element>) {
        _indexes.append(contentsOf: other)
    }
    
    /// Return a new `IndexPath` containing the elements in self and the elements in `other`.
    public func appending(_ other: Element) -> IndexPath {
        var result = _indexes
        result.append(other)
        return IndexPath(indexes: result)
    }
    
    /// Return a new `IndexPath` containing the elements in self and the elements in `other`.
    public func appending(_ other: IndexPath) -> IndexPath {
        return IndexPath(indexes: _indexes + other._indexes)
    }
    
    /// Return a new `IndexPath` containing the elements in self and the elements in `other`.
    public func appending(_ other: Array<Element>) -> IndexPath {
        return IndexPath(indexes: _indexes + other)
    }
    
    public subscript(index: Index) -> Element {
        get {
            return _indexes[index]
        }
        set {
            _indexes[index] = newValue
        }
    }
    
    public subscript(range: Range<Index>) -> IndexPath {
        get {
            return IndexPath(indexes: _indexes[range])
        }
        set {
            _indexes.replaceSubrange(range, with: newValue._indexes)
        }
    }
    
    public func makeIterator() -> IndexingIterator<IndexPath> {
        return IndexingIterator(_elements: self)
    }
    
    public var count: Int {
        return _indexes.count
    }
    
    public var startIndex: Index {
        return _indexes.startIndex
    }
    
    public var endIndex: Index {
        return _indexes.endIndex
    }

    public func index(before i: Index) -> Index {
        return _indexes.index(before: i)
    }
    
    public func index(after i: Index) -> Index {
        return _indexes.index(after: i)
    }

    /// Sorting an array of `IndexPath` using this comparison results in an array representing nodes in depth-first traversal order.
    public func compare(_ other: IndexPath) -> ComparisonResult  {
        // This is temporary
        let me = self.makeReference()
        let other = other.makeReference()
        return me.compare(other as IndexPath)
    }

    public var hashValue: Int {
        // This is temporary
        let me = self.makeReference()
        return me.hash
    }
        
    // MARK: - Bridging Helpers
    
    fileprivate init(nsIndexPath: ReferenceType) {
        let count = nsIndexPath.length
        if count == 0 {
            _indexes = []
        } else {
            var ptr = malloc(count * MemoryLayout<Element>.size)
            defer { free(ptr) }

            let elementPtr = ptr!.bindMemory(to: Element.self, capacity: count)
            nsIndexPath.getIndexes(elementPtr, range: NSMakeRange(0, count))
            
            let buffer = UnsafeBufferPointer(start: elementPtr, count: count)
            _indexes = Array(buffer)
        }
    }
    
    fileprivate func makeReference() -> ReferenceType {
        return _indexes.withUnsafeBufferPointer {
            return ReferenceType(indexes: $0.baseAddress, length: $0.count)
        }
    }

    public static func ==(lhs: IndexPath, rhs: IndexPath) -> Bool {
        return lhs._indexes == rhs._indexes
    }

    public static func +(lhs: IndexPath, rhs: IndexPath) -> IndexPath {
        return lhs.appending(rhs)
    }

    public static func +=(lhs: inout IndexPath, rhs: IndexPath) {
        lhs.append(rhs)
    }

    public static func <(lhs: IndexPath, rhs: IndexPath) -> Bool {
        return lhs.compare(rhs) == ComparisonResult.orderedAscending
    }

    public static func <=(lhs: IndexPath, rhs: IndexPath) -> Bool {
        let order = lhs.compare(rhs)
        return order == ComparisonResult.orderedAscending || order == ComparisonResult.orderedSame
    }

    public static func >(lhs: IndexPath, rhs: IndexPath) -> Bool {
        return lhs.compare(rhs) == ComparisonResult.orderedDescending
    }

    public static func >=(lhs: IndexPath, rhs: IndexPath) -> Bool {
        let order = lhs.compare(rhs)
        return order == ComparisonResult.orderedDescending || order == ComparisonResult.orderedSame
    }
}

extension IndexPath : CustomStringConvertible, CustomDebugStringConvertible, CustomReflectable {
    public var description: String {
        return _indexes.description
    }
    
    public var debugDescription: String {
        return _indexes.debugDescription
    }

    public var customMirror: Mirror {
        return _indexes.customMirror
    }
}

extension IndexPath : _ObjectiveCBridgeable {
    public static func _getObjectiveCType() -> Any.Type {
        return NSIndexPath.self
    }
    
    @_semantics("convertToObjectiveC")
    public func _bridgeToObjectiveC() -> NSIndexPath {
        return makeReference()
    }
    
    public static func _forceBridgeFromObjectiveC(_ x: NSIndexPath, result: inout IndexPath?) {
        result = IndexPath(nsIndexPath: x)
    }
    
    public static func _conditionallyBridgeFromObjectiveC(_ x: NSIndexPath, result: inout IndexPath?) -> Bool {
        result = IndexPath(nsIndexPath: x)
        return true
    }

    public static func _unconditionallyBridgeFromObjectiveC(_ source: NSIndexPath?) -> IndexPath {
        return IndexPath(nsIndexPath: source!)
    }    
}

extension NSIndexPath : _HasCustomAnyHashableRepresentation {
    // Must be @nonobjc to avoid infinite recursion during bridging.
    @nonobjc
    public func _toCustomAnyHashable() -> AnyHashable? {
        return AnyHashable(self as IndexPath)
    }
}

