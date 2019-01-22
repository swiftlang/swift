//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2015 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// A type that represents the difference between two ordered collection states.
// @available(swift, introduced: 5.1)
public struct OrderedCollectionDifference<ChangeElement> {
    /// A type that represents a single change to an ordered collection.
    ///
    /// The `offset` of each `insert` refers to the offset of its `element` in
    /// the final state after the difference is fully applied. The `offset` of
    /// each `remove` refers to the offset of its `element` in the original
    /// state. Non-`nil` values of `associatedWith` refer to the offset of the
    /// complementary change.
    public enum Change {
        case insert(offset: Int, element: ChangeElement, associatedWith: Int?)
        case remove(offset: Int, element: ChangeElement, associatedWith: Int?)

        // Internal common field accessors
        var offset: Int {
            get {
                switch self {
                case .insert(offset: let o, element: _, associatedWith: _):
                    return o
                case .remove(offset: let o, element: _, associatedWith: _):
                    return o
                }
            }
        }
        var element: ChangeElement {
            get {
                switch self {
                case .insert(offset: _, element: let e, associatedWith: _):
                    return e
                case .remove(offset: _, element: let e, associatedWith: _):
                    return e
                }
            }
        }
        var associatedOffset: Int? {
            get {
                switch self {
                case .insert(offset: _, element: _, associatedWith: let o):
                    return o
                case .remove(offset: _, element: _, associatedWith: let o):
                    return o
                }
            }
        }
    }

    // The public initializer calls this function to ensure that its parameter
    // meets the conditions set in its documentation.
    private static func validateChanges<C>(_ changes : C) -> Bool where C:Collection, C.Element == Change {
        if changes.count == 0 { return true }

        var insertAssocToOffset = Dictionary<Int,Int>()
        var removeOffsetToAssoc = Dictionary<Int,Int>()
        var insertOffset = Set<Int>()
        var removeOffset = Set<Int>()

        for c in changes {
            let offset = c.offset
            if offset < 0 { return false }

            switch c {
            case .remove(_, _, _):
                if removeOffset.contains(offset) { return false }
                removeOffset.insert(offset)
            case .insert(_, _, _):
                if insertOffset.contains(offset) { return false }
                insertOffset.insert(offset)
            }

            if let assoc = c.associatedOffset {
                if assoc < 0 { return false }
                switch c {
                case .remove(_, _, _):
                    if removeOffsetToAssoc[offset] != nil { return false }
                    removeOffsetToAssoc[offset] = assoc
                case .insert(_, _, _):
                    if insertAssocToOffset[assoc] != nil { return false }
                    insertAssocToOffset[assoc] = offset
                }
            }
        }

        return removeOffsetToAssoc == insertAssocToOffset
    }

    /// Creates an instance from a collection of changes.
    ///
    /// For clients interested in the difference between two ordered
    /// collections, see `OrderedCollection.difference(from:)`.
    ///
    /// To guarantee that instances are unambiguous and safe for compatible base
    /// states, this initializer will fail unless its parameter meets to the
    /// following requirements:
    ///
    /// 1) All insertion offsets are unique
    /// 2) All removal offsets are unique
    /// 3) All offset associations between insertions and removals are symmetric
    ///
    /// - Parameter changes: A collection of changes that represent a transition
    ///   between two states.
    ///
    /// - Complexity: O(*n* * log(*n*)), where *n* is the length of the
    ///   parameter.
    public init?<C>(_ c: C) where C:Collection, C.Element == Change {
        if !OrderedCollectionDifference<ChangeElement>.validateChanges(c) {
            return nil
        }

        self.init(validatedChanges: c)
    }

    // Internal initializer for use by algorithms that cannot produce invalid
    // collections of changes. These include the Myers' diff algorithm and
    // the move inferencer.
    init<C>(validatedChanges c: C) where C:Collection, C.Element == Change {
        let changes = c.sorted { (a, b) -> Bool in
            switch (a, b) {
            case (.remove(_, _, _), .insert(_, _, _)):
                return true
            case (.insert(_, _, _), .remove(_, _, _)):
                return false
            default:
                return a.offset < b.offset
            }
        }

        // Find first insertion via binary search
        let firstInsertIndex: Int
        if changes.count == 0 {
            firstInsertIndex = 0
        } else {
            var range = 0...changes.count
            while range.lowerBound != range.upperBound {
                let i = (range.lowerBound + range.upperBound) / 2
                switch changes[i] {
                case .insert(_, _, _):
                    range = range.lowerBound...i
                case .remove(_, _, _):
                    range = (i + 1)...range.upperBound
                }
            }
            firstInsertIndex = range.lowerBound
        }

        removals = Array(changes[0..<firstInsertIndex])
        insertions = Array(changes[firstInsertIndex..<changes.count])
    }

    /// The `.insert` changes contained by this difference, from lowest offset to highest
    public let insertions: [Change]
    
    /// The `.remove` changes contained by this difference, from lowest offset to highest
    public let removals: [Change]
}

/// An OrderedCollectionDifference is itself a Collection.
///
/// The `Change` elements are ordered as:
///
/// 1. `.remove`s, from highest `offset` to lowest
/// 2. `.insert`s, from lowest `offset` to highest
///
/// This guarantees that applicators on compatible base states are safe when
/// written in the form:
///
/// ```
/// for c in diff {
///     switch c {
///     case .remove(offset: let o, element: _, associatedWith: _):
///         arr.remove(at: o)
///     case .insert(offset: let o, element: let e, associatedWith: _):
///         arr.insert(e, at: o)
///     }
/// }
/// ```
extension OrderedCollectionDifference : Collection {
    public typealias Element = OrderedCollectionDifference<ChangeElement>.Change

    // Opaque index type is isomorphic to Int
    public struct Index: Comparable, Hashable {
        public static func < (lhs: OrderedCollectionDifference<ChangeElement>.Index, rhs: OrderedCollectionDifference<ChangeElement>.Index) -> Bool {
            return lhs.i < rhs.i
        }
        
        let i: Int
        init(_ index: Int) {
            i = index
        }
    }

    public var startIndex: OrderedCollectionDifference<ChangeElement>.Index {
        return Index(0)
    }
    
    public var endIndex: OrderedCollectionDifference<ChangeElement>.Index {
        return Index(removals.count + insertions.count)
    }
    
    public func index(after index: OrderedCollectionDifference<ChangeElement>.Index) -> OrderedCollectionDifference<ChangeElement>.Index {
        return Index(index.i + 1)
    }
    
    public subscript(position: OrderedCollectionDifference<ChangeElement>.Index) -> Element {
        return position.i < removals.count ? removals[removals.count - (position.i + 1)] : insertions[position.i - removals.count]
    }
    
    public func index(before index: OrderedCollectionDifference<ChangeElement>.Index) -> OrderedCollectionDifference<ChangeElement>.Index {
        return Index(index.i - 1)
    }
    
    public func formIndex(_ index: inout OrderedCollectionDifference<ChangeElement>.Index, offsetBy distance: Int) {
        index = Index(index.i + distance)
    }
    
    public func distance(from start: OrderedCollectionDifference<ChangeElement>.Index, to end: OrderedCollectionDifference<ChangeElement>.Index) -> Int {
        return end.i - start.i
    }
}

extension OrderedCollectionDifference.Change: Equatable where ChangeElement: Equatable {}

extension OrderedCollectionDifference: Equatable where ChangeElement: Equatable {}

extension OrderedCollectionDifference.Change: Hashable where ChangeElement: Hashable {}

extension OrderedCollectionDifference: Hashable where ChangeElement: Hashable {

    /// Infers which `ChangeElement`s have been both inserted and removed only
    /// once and returns a new difference with those associations.
    ///
    /// - Returns: an instance with all possible moves inferred.
    ///
    /// - Complexity: O(*n*) where *n* is `self.count`
    public func inferringMoves() -> OrderedCollectionDifference<ChangeElement> {
        let removeDict: [ChangeElement:Int?] = {
            var res = [ChangeElement:Int?](minimumCapacity: Swift.min(removals.count, insertions.count))
            for r in removals {
                let element = r.element
                if res[element] != .none {
                    res[element] = .some(.none)
                } else {
                    res[element] = .some(r.offset)
                }
            }
            return res.filter { (_, v) -> Bool in v != .none }
        }()
        
        let insertDict: [ChangeElement:Int?] = {
            var res = [ChangeElement:Int?](minimumCapacity: Swift.min(removals.count, insertions.count))
            for i in insertions {
                let element = i.element
                if res[element] != .none {
                    res[element] = .some(.none)
                } else {
                    res[element] = .some(i.offset)
                }
            }
            return res.filter { (_, v) -> Bool in v != .none }
        }()

        return OrderedCollectionDifference.init(validatedChanges:map({ (c: OrderedCollectionDifference<ChangeElement>.Change) -> OrderedCollectionDifference<ChangeElement>.Change in
            switch c {
            case .remove(offset: let o, element: let e, associatedWith: _):
                if removeDict[e] == nil {
                    return c
                }
                if let assoc = insertDict[e] {
                    return .remove(offset: o, element: e, associatedWith: assoc)
                }
            case .insert(offset: let o, element: let e, associatedWith: _):
                if insertDict[e] == nil {
                    return c
                }
                if let assoc = removeDict[e] {
                    return .insert(offset: o, element: e, associatedWith: assoc)
                }
            }
            return c
        }))
    }
}

extension OrderedCollectionDifference.Change: Codable where ChangeElement: Codable {
    private enum CodingKeys: String, CodingKey {
        case offset
        case element
        case associatedOffset
        case isRemove
    }

    public init(from decoder: Decoder) throws {
        let values = try decoder.container(keyedBy: CodingKeys.self)
        let offset = try values.decode(Int.self, forKey: .offset)
        let element = try values.decode(ChangeElement.self, forKey: .element)
        let associatedOffset = try values.decode(Int?.self, forKey: .associatedOffset)
        let isRemove = try values.decode(Bool.self, forKey: .isRemove)
        if isRemove {
            self = .remove(offset: offset, element: element, associatedWith: associatedOffset)
        } else {
            self = .insert(offset: offset, element: element, associatedWith: associatedOffset)
        }
    }
    
    public func encode(to encoder: Encoder) throws {
        var container = encoder.container(keyedBy: CodingKeys.self)
        switch self {
        case .remove(_, _, _):
            try container.encode(true, forKey: .isRemove)
        case .insert(_, _, _):
            try container.encode(false, forKey: .isRemove)
        }
        
        try container.encode(offset, forKey: .offset)
        try container.encode(element, forKey: .element)
        try container.encode(associatedOffset, forKey: .associatedOffset)
    }
}

extension OrderedCollectionDifference: Codable where ChangeElement: Codable {}
