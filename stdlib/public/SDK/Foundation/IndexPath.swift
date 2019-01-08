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

/**
 `IndexPath` represents the path to a specific node in a tree of nested array collections.
 
 Each index in an index path represents the index into an array of children from one node in the tree to another, deeper, node.
 */
public struct IndexPath : ReferenceConvertible, Equatable, Hashable, MutableCollection, RandomAccessCollection, Comparable, ExpressibleByArrayLiteral {
    public typealias ReferenceType = NSIndexPath
    public typealias Element = Int
    public typealias Index = Array<Int>.Index
    public typealias Indices = DefaultIndices<IndexPath>
    
    fileprivate enum Storage : ExpressibleByArrayLiteral {
        typealias Element = Int
        case empty
        case single(Int)
        case pair(Int, Int)
        case array([Int])
        
        init(arrayLiteral elements: Int...) {
            self.init(elements)
        }
        
        init(_ elements: [Int]) {
            switch elements.count {
            case 0:
                self = .empty
                break
            case 1:
                self = .single(elements[0])
                break
            case 2:
                self = .pair(elements[0], elements[1])
                break
            default:
                self = .array(elements)
                break
            }
        }
        
        func dropLast() -> Storage {
            switch self {
            case .empty:
                return .empty
            case .single(_):
                return .empty
            case .pair(let first, _):
                return .single(first)
            case .array(let indexes):
                switch indexes.count {
                case 3:
                    return .pair(indexes[0], indexes[1])
                default:
                    return .array(Array<Int>(indexes.dropLast()))
                }
            }
        }
        
        mutating func append(_ other: Int) {
            switch self {
            case .empty:
                self = .single(other)
                break
            case .single(let first):
                self = .pair(first, other)
                break
            case .pair(let first, let second):
                self = .array([first, second, other])
                break
            case .array(let indexes):
                self = .array(indexes + [other])
                break
            }
        }
        
        mutating func append(contentsOf other: Storage) {
            switch self {
            case .empty:
                switch other {
                case .empty:
                    // DO NOTHING
                    break
                case .single(let rhsIndex):
                    self = .single(rhsIndex)
                    break
                case .pair(let rhsFirst, let rhsSecond):
                    self = .pair(rhsFirst, rhsSecond)
                    break
                case .array(let rhsIndexes):
                    self = .array(rhsIndexes)
                    break
                }
                break
            case .single(let lhsIndex):
                switch other {
                case .empty:
                    // DO NOTHING
                    break
                case .single(let rhsIndex):
                    self = .pair(lhsIndex, rhsIndex)
                    break
                case .pair(let rhsFirst, let rhsSecond):
                    self = .array([lhsIndex, rhsFirst, rhsSecond])
                    break
                case .array(let rhsIndexes):
                    self = .array([lhsIndex] + rhsIndexes)
                    break
                }
                break
            case .pair(let lhsFirst, let lhsSecond):
                switch other {
                case .empty:
                    // DO NOTHING
                    break
                case .single(let rhsIndex):
                    self = .array([lhsFirst, lhsSecond, rhsIndex])
                    break
                case .pair(let rhsFirst, let rhsSecond):
                    self = .array([lhsFirst, lhsSecond, rhsFirst, rhsSecond])
                    break
                case .array(let rhsIndexes):
                    self = .array([lhsFirst, lhsSecond] + rhsIndexes)
                    break
                }
                break
            case .array(let lhsIndexes):
                switch other {
                case .empty:
                    // DO NOTHING
                    break
                case .single(let rhsIndex):
                    self = .array(lhsIndexes + [rhsIndex])
                    break
                case .pair(let rhsFirst, let rhsSecond):
                    self = .array(lhsIndexes + [rhsFirst, rhsSecond])
                    break
                case .array(let rhsIndexes):
                    self = .array(lhsIndexes + rhsIndexes)
                    break
                }
                break
            }
        }
        
        mutating func append(contentsOf other: __owned [Int]) {
            switch self {
            case .empty:
                switch other.count {
                case 0:
                    // DO NOTHING
                    break
                case 1:
                    self = .single(other[0])
                    break
                case 2:
                    self = .pair(other[0], other[1])
                    break
                default:
                    self = .array(other)
                    break
                }
                break
            case .single(let first):
                switch other.count {
                case 0:
                    // DO NOTHING
                    break
                case 1:
                    self = .pair(first, other[0])
                    break
                default:
                    self = .array([first] + other)
                    break
                }
                break
            case .pair(let first, let second):
                switch other.count {
                case 0:
                    // DO NOTHING
                    break
                default:
                    self = .array([first, second] + other)
                    break
                }
                break
            case .array(let indexes):
                self = .array(indexes + other)
                break
            }
        }
        
        subscript(_ index: Int) -> Int {
            get {
                switch self {
                case .empty:
                    fatalError("Index \(index) out of bounds of count 0")
                    break
                case .single(let first):
                    precondition(index == 0, "Index \(index) out of bounds of count 1")
                    return first
                case .pair(let first, let second):
                    precondition(index >= 0 && index < 2, "Index \(index) out of bounds of count 2")
                    return index == 0 ? first : second
                case .array(let indexes):
                    return indexes[index]
                }
            }
            set {
                switch self {
                case .empty:
                    fatalError("Index \(index) out of bounds of count 0")
                    break
                case .single(_):
                    precondition(index == 0, "Index \(index) out of bounds of count 1")
                    self = .single(newValue)
                    break
                case .pair(let first, let second):
                    precondition(index >= 0 && index < 2, "Index \(index) out of bounds of count 2")
                    if index == 0 {
                        self = .pair(newValue, second)
                    } else {
                        self = .pair(first, newValue)
                    }
                    break
                case .array(let indexes_):
                    var indexes = indexes_
                    indexes[index] = newValue
                    self = .array(indexes)
                    break
                }
            }
        }
        
        subscript(range: Range<Index>) -> Storage {
            get {
                switch self {
                case .empty:
                    switch (range.lowerBound, range.upperBound) {
                    case (0, 0):
                        return .empty
                    default:
                        fatalError("Range \(range) is out of bounds of count 0")
                    }
                case .single(let index):
                    switch (range.lowerBound, range.upperBound) {
                    case (0, 0): fallthrough
                    case (1, 1):
                        return .empty
                    case (0, 1):
                        return .single(index)
                    default:
                        fatalError("Range \(range) is out of bounds of count 1")
                    }
                case .pair(let first, let second):
                    
                    switch (range.lowerBound, range.upperBound) {
                    case (0, 0):
                        fallthrough
                    case (1, 1):
                        fallthrough
                    case (2, 2):
                        return .empty
                    case (0, 1):
                        return .single(first)
                    case (1, 2):
                        return .single(second)
                    case (0, 2):
                        return self
                    default:
                        fatalError("Range \(range) is out of bounds of count 2")
                    }
                case .array(let indexes):
                    let slice = indexes[range]
                    switch slice.count {
                    case 0:
                        return .empty
                    case 1:
                        return .single(slice.first!)
                    case 2:
                        return .pair(slice.first!, slice.last!)
                    default:
                        return .array(Array<Int>(slice))
                    }
                }
            }
            set {
                switch self {
                case .empty:
                    precondition(range.lowerBound == 0 && range.upperBound == 0, "Range \(range) is out of bounds of count 0")
                    self = newValue
                    break
                case .single(let index):
                    switch (range.lowerBound, range.upperBound, newValue) {
                    case (0, 0, .empty):
                        fallthrough
                    case (1, 1, .empty):
                        break
                    case (0, 0, .single(let other)):
                        self = .pair(other, index)
                        break
                    case (0, 0, .pair(let first, let second)):
                        self = .array([first, second, index])
                        break
                    case (0, 0, .array(let other)):
                        self = .array(other + [index])
                        break
                    case (0, 1, .empty):
                        fallthrough
                    case (0, 1, .single):
                        fallthrough
                    case (0, 1, .pair):
                        fallthrough
                    case (0, 1, .array):
                        self = newValue
                    case (1, 1, .single(let other)):
                        self = .pair(index, other)
                        break
                    case (1, 1, .pair(let first, let second)):
                        self = .array([index, first, second])
                        break
                    case (1, 1, .array(let other)):
                        self = .array([index] + other)
                        break
                    default:
                        fatalError("Range \(range) is out of bounds of count 1")
                    }
                case .pair(let first, let second):
                    switch (range.lowerBound, range.upperBound) {
                    case (0, 0):
                        switch newValue {
                        case .empty:
                            break
                        case .single(let other):
                            self = .array([other, first, second])
                            break
                        case .pair(let otherFirst, let otherSecond):
                            self = .array([otherFirst, otherSecond, first, second])
                            break
                        case .array(let other):
                            self = .array(other + [first, second])
                            break
                        }
                        break
                    case (0, 1):
                        switch newValue {
                        case .empty:
                            self = .single(second)
                            break
                        case .single(let other):
                            self = .pair(other, second)
                            break
                        case .pair(let otherFirst, let otherSecond):
                            self = .array([otherFirst, otherSecond, second])
                            break
                        case .array(let other):
                            self = .array(other + [second])
                            break
                        }
                        break
                    case (0, 2):
                        self = newValue
                        break
                    case (1, 2):
                        switch newValue {
                        case .empty:
                            self = .single(first)
                            break
                        case .single(let other):
                            self = .pair(first, other)
                            break
                        case .pair(let otherFirst, let otherSecond):
                            self = .array([first, otherFirst, otherSecond])
                            break
                        case .array(let other):
                            self = .array([first] + other)
                        }
                        break
                    case (2, 2):
                        switch newValue {
                        case .empty:
                            break
                        case .single(let other):
                            self = .array([first, second, other])
                            break
                        case .pair(let otherFirst, let otherSecond):
                            self = .array([first, second, otherFirst, otherSecond])
                            break
                        case .array(let other):
                            self = .array([first, second] + other)
                        }
                        break
                    default:
                        fatalError("Range \(range) is out of bounds of count 2")
                    }
                case .array(let indexes):
                    var newIndexes = indexes
                    newIndexes.removeSubrange(range)
                    switch newValue {
                    case .empty:
                        break
                    case .single(let index):
                        newIndexes.insert(index, at: range.lowerBound)
                        break
                    case .pair(let first, let second):
                        newIndexes.insert(first, at: range.lowerBound)
                        newIndexes.insert(second, at: range.lowerBound + 1)
                        break
                    case .array(let other):
                        newIndexes.insert(contentsOf: other, at: range.lowerBound)
                        break
                    }
                    self = Storage(newIndexes)
                    break
                }
            }
        }
        
        var count: Int {
            switch self {
            case .empty:
                return 0
            case .single:
                return 1
            case .pair:
                return 2
            case .array(let indexes):
                return indexes.count
            }
        }
        
        var startIndex: Int {
            return 0
        }
        
        var endIndex: Int {
            return count
        }

        var allValues: [Int] {
            switch self {
            case .empty: return []
            case .single(let index): return [index]
            case .pair(let first, let second): return [first, second]
            case .array(let indexes): return indexes
            }
        }
        
        func index(before i: Int) -> Int {
            return i - 1
        }
        
        func index(after i: Int) -> Int {
            return i + 1
        }
        
        var description: String {
            switch self {
            case .empty:
                return "[]"
            case .single(let index):
                return "[\(index)]"
            case .pair(let first, let second):
                return "[\(first), \(second)]"
            case .array(let indexes):
                return indexes.description
            }
        }
        
        func withUnsafeBufferPointer<R>(_ body: (UnsafeBufferPointer<Int>) throws -> R) rethrows -> R {
            switch self {
            case .empty:
                return try body(UnsafeBufferPointer<Int>(start: nil, count: 0))
            case .single(let index_):
                var index = index_
                return try withUnsafePointer(to: &index) { (start) throws -> R in
                    return try body(UnsafeBufferPointer<Int>(start: start, count: 1))
                }
            case .pair(let first, let second):
                var pair = (first, second)
                return try withUnsafeBytes(of: &pair) { (rawBuffer: UnsafeRawBufferPointer) throws -> R in
                    return try body(UnsafeBufferPointer<Int>(start: rawBuffer.baseAddress?.assumingMemoryBound(to: Int.self), count: 2))
                }
            case .array(let indexes):
                return try indexes.withUnsafeBufferPointer(body)
            }
        }
        
        var debugDescription: String { return description }
        
        static func +(lhs: Storage, rhs: Storage) -> Storage {
            var res = lhs
            res.append(contentsOf: rhs)
            return res
        }
        
        static func +(lhs: Storage, rhs: [Int]) -> Storage {
            var res = lhs
            res.append(contentsOf: rhs)
            return res
        }
        
        static func ==(lhs: Storage, rhs: Storage) -> Bool {
            switch (lhs, rhs) {
            case (.empty, .empty):
                return true
            case (.single(let lhsIndex), .single(let rhsIndex)):
                return lhsIndex == rhsIndex
            case (.pair(let lhsFirst, let lhsSecond), .pair(let rhsFirst, let rhsSecond)):
                return lhsFirst == rhsFirst && lhsSecond == rhsSecond
            case (.array(let lhsIndexes), .array(let rhsIndexes)):
                return lhsIndexes == rhsIndexes
            default:
                return false
            }
        }
    }
    
    fileprivate var _indexes : Storage
    
    /// Initialize an empty index path.
    public init() {
        _indexes = []
    }
    
    /// Initialize with a sequence of integers.
    public init<ElementSequence : Sequence>(indexes: ElementSequence)
        where ElementSequence.Iterator.Element == Element {
            _indexes = Storage(indexes.map { $0 })
    }
    
    /// Initialize with an array literal.
    public init(arrayLiteral indexes: Element...) {
        _indexes = Storage(indexes)
    }
    
    /// Initialize with an array of elements.
    public init(indexes: Array<Element>) {
        _indexes = Storage(indexes)
    }
    
    fileprivate init(storage: Storage) {
        _indexes = storage
    }
    
    /// Initialize with a single element.
    public init(index: Element) {
        _indexes = [index]
    }
    
    /// Return a new `IndexPath` containing all but the last element.
    public func dropLast() -> IndexPath {
        return IndexPath(storage: _indexes.dropLast())
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
        return IndexPath(storage: result)
    }
    
    /// Return a new `IndexPath` containing the elements in self and the elements in `other`.
    public func appending(_ other: IndexPath) -> IndexPath {
        return IndexPath(storage: _indexes + other._indexes)
    }
    
    /// Return a new `IndexPath` containing the elements in self and the elements in `other`.
    public func appending(_ other: Array<Element>) -> IndexPath {
        return IndexPath(storage: _indexes + other)
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
            return IndexPath(storage: _indexes[range])
        }
        set {
            _indexes[range] = newValue._indexes
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
        let thisLength = count
        let otherLength = other.count
        let length = Swift.min(thisLength, otherLength)
        for idx in 0..<length {
            let otherValue = other[idx]
            let value = self[idx]
            if value < otherValue {
                return .orderedAscending
            } else if value > otherValue {
                return .orderedDescending
            }
        }
        if thisLength > otherLength {
            return .orderedDescending
        } else if thisLength < otherLength {
            return .orderedAscending
        }
        return .orderedSame
    }
    
    public var hashValue: Int {
        func hashIndexes(first: Int, last: Int, count: Int) -> Int {
            let totalBits = MemoryLayout<Int>.size * 8
            let lengthBits = 8
            let firstIndexBits = (totalBits - lengthBits) / 2
            return count &+ (first << lengthBits) &+ (last << (lengthBits + firstIndexBits))
        }

        switch _indexes {
            case .empty: return 0
            case .single(let index): return index.hashValue
            case .pair(let first, let second):
                return hashIndexes(first: first, last: second, count: 2)
            default:
                let cnt = _indexes.count
                return hashIndexes(first: _indexes[0], last: _indexes[cnt - 1], count: cnt)
        }
    }
    
    // MARK: - Bridging Helpers
    
    fileprivate init(nsIndexPath: __shared ReferenceType) {
        let count = nsIndexPath.length
        if count == 0 {
            _indexes = []
        } else if count == 1 {
            _indexes = .single(nsIndexPath.index(atPosition: 0))
        } else if count == 2 {
            _indexes = .pair(nsIndexPath.index(atPosition: 0), nsIndexPath.index(atPosition: 1))
        } else {
            var indexes = Array<Int>(repeating: 0, count: count)
            indexes.withUnsafeMutableBufferPointer { (buffer: inout UnsafeMutableBufferPointer<Int>) -> Void in
                nsIndexPath.getIndexes(buffer.baseAddress!, range: NSRange(location: 0, length: count))
            }
            _indexes = .array(indexes)
        }
    }
    
    fileprivate func makeReference() -> ReferenceType {
        switch _indexes {
        case .empty:
            return ReferenceType()
        case .single(let index):
            return ReferenceType(index: index)
        case .pair(let first, let second):
            return _NSIndexPathCreateFromIndexes(first, second) as! ReferenceType
        default:
            return _indexes.withUnsafeBufferPointer {
                return ReferenceType(indexes: $0.baseAddress, length: $0.count)
            }
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
        return Mirror(self, unlabeledChildren: self, displayStyle: .collection)
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
    
    @_effects(readonly)
    public static func _unconditionallyBridgeFromObjectiveC(_ source: NSIndexPath?) -> IndexPath {
        guard let src = source else { return IndexPath() }
        return IndexPath(nsIndexPath: src)
    }
}

extension NSIndexPath : _HasCustomAnyHashableRepresentation {
    // Must be @nonobjc to avoid infinite recursion during bridging.
    @nonobjc
    public func _toCustomAnyHashable() -> AnyHashable? {
        return AnyHashable(self as IndexPath)
    }
}

extension IndexPath : Codable {
    private enum CodingKeys : Int, CodingKey {
        case indexes
    }

    public init(from decoder: Decoder) throws {
        let container = try decoder.container(keyedBy: CodingKeys.self)
        var indexesContainer = try container.nestedUnkeyedContainer(forKey: .indexes)

        var indexes = [Int]()
        if let count = indexesContainer.count {
            indexes.reserveCapacity(count)
        }

        while !indexesContainer.isAtEnd {
            let index = try indexesContainer.decode(Int.self)
            indexes.append(index)
        }

        self.init(indexes: indexes)
    }

    public func encode(to encoder: Encoder) throws {
        var container = encoder.container(keyedBy: CodingKeys.self)
        var indexesContainer = container.nestedUnkeyedContainer(forKey: .indexes)
        switch self._indexes {
        case .empty:
            break
        case .single(let index):
            try indexesContainer.encode(index)
        case .pair(let first, let second):
            try indexesContainer.encode(first)
            try indexesContainer.encode(second)
        case .array(let indexes):
            try indexesContainer.encode(contentsOf: indexes)
        }
    }
}
