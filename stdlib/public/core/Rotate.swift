//===--- Rotate.swift - Collection rotation -------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

extension MutableCollection {
    /// Rotates the elements of the collection such that the value at the given
    /// index moves leftward to `startIndex`.
    ///
    /// Passing `startIndex` or `endIndex` as `middle` has no effect.
    ///
    /// The following example rotates the elements of an array of characters so
    /// the second character moves to be the first:
    ///
    ///     var café: [Character] = ["C", "a", "f", "é"]
    ///     café.rotate(toFirst: café.index(after: café.startIndex))
    ///     print(café)
    ///     // Prints "["a", "f", "é", "C"]"
    ///
    /// - Precondition: `middle` must be a valid index of this collection.
    ///
    /// - Parameter middle: The index of the element whose value will move
    ///   to `startIndex`, unless it's `endIndex` (then it's ignored).
    ///
    /// - Returns: The new index of the value originally at `startIndex`.
    ///
    /// - Postcondition: The collection is a left-rotation of its old state,
    ///   *i.e.* `newValue` equals `oldValue[middle...] + oldValue[..<middle]`.
    ///
    /// - Complexity: O(*n*), where *n* is the number of elements in the
    ///   collection.
    @discardableResult
    public mutating func rotate(toFirst middle: Index) -> Index {
        switch middle {
        case ...startIndex, endIndex...:
            // A base case, otherwise the true/false and false/true cases of the
            // switch below would trigger infinite recursion.
            return startIndex
        default:
            // With the given index as the pivot between partitions, switch the
            // order of said partitions.  Do it without testing the relative
            // lengths in advance.  If the partitions have different lengths,
            // the prefix/whole part of the originally-second partition is
            // finalized and the remainder (the suffix of the longer original
            // partition and the whole/prefix part of the originally-first
            // partition) needs to be rotated.
            var i = startIndex, j = middle
            while true {
                switch (i == middle, j == endIndex) {
                case (false, false):
                    // Move one step closer by doing the core swap.
                    swapAt(i, j)
                    formIndex(after: &i)
                    formIndex(after: &j)
                case (false, true):
                    // The first partition is longer than the second.
                    self[i...].rotate(toFirst: middle)
                    return i
                case (true, false):
                    // The first partition is shorter than the second.
                    return self[middle...].rotate(toFirst: j)
                case (true, true):
                    // The partitions have the same length.
                    return middle
                }
            }
        }
    }

    /// Rotates the elements of the collection such that the value at the given
    /// index moves rightward to just before `endIndex`.
    ///
    /// Passing `endIndex` or its immediate predecessor (if any) as `middle` has
    /// no effect.
    ///
    /// The following example rotates the elements of an array of characters so
    /// the second character moves to be the last:
    ///
    ///     var café: [Character] = ["C", "a", "f", "é"]
    ///     café.rotate(toLast: café.index(after: café.startIndex))
    ///     print(café)
    ///     // Prints "["f", "é", "C", "a"]"
    ///
    /// - Precondition: `middle` must be a valid index of this collection.
    ///
    /// - Parameter middle: The index of the element whose value will move to
    ///   just before `endIndex`, unless it's `endIndex` (then it's ignored).
    ///
    /// - Returns: The new index of the value originally at `startIndex`.
    ///
    /// - Postcondition: The collection is a right-rotation of its old state,
    ///   *i.e.* `newValue` equals `oldValue[middle>..] + oldValue[...middle]`.
    ///
    /// - Complexity: O(*n*), where *n* is the number of elements in the
    ///   collection.
    @inlinable
    @discardableResult
    public mutating func rotate(toLast middle: Index) -> Index {
        return rotate(toFirst: index(middle, offsetBy: +1, limitedBy: endIndex)
            ?? startIndex)
    }
}

/// A collection that presents the elements of its base collection
/// in rotated order.
///
/// - Note: This type is the result from either `x.rotated(toFirst:)` or
///   `x.rotated(toLast:)` for a given collection `x`.
///
/// The `rotated` methods are always lazy, but do not implicitly confer
/// laziness on algorithms applied to their results.  In other words, for
/// ordinary collections `c`:
///
/// * `c.rotated(toFirst: i)` does not create new storage
/// * `c.rotated(toFirst: i).map(f)` maps eagerly and returns a new array
/// * `c.lazy.rotated(toFirst: i).map(f)` maps lazily and returns a
///   `LazyMapCollection`
/// * (The above similarly apply to `rotated(toLast:)`.)
@_fixed_layout
public struct RotatedCollection<Base: Collection> {
    /// The underlying collection.
    public let _base: Base
    /// The underlying index for the rotated collection's first element.
    @usableFromInline
    internal var _origin: Base.Index

    /// Creates an instance that presents the elements of `base` in a rotated
    /// order with the given index as the new start-index.
    ///
    /// - Complexity: O(1)
    @inlinable
    internal init(_base: Base, asFirst middle: Base.Index) {
        self._base = _base
        _origin = middle < _base.endIndex ? middle : _base.startIndex
    }
}

extension RotatedCollection {
    // An iterator that can be much faster than the iterator of a rotated slice.
    @_fixed_layout
    public struct Iterator {
        @usableFromInline
        internal var _suffixIterator, _prefixIterator: Base.SubSequence.Iterator

        @inlinable
        @inline(__always)
        /// Creates an iterator over the given collection.
        public /// @testable
        init(_base: Base, asFirst middle: Base.Index) {
            _suffixIterator = _base[middle...].makeIterator()
            _prefixIterator = _base[..<middle].makeIterator()
        }
    }
}

extension RotatedCollection.Iterator: IteratorProtocol {
    public typealias Element = Base.Element

    @inlinable
    @inline(__always)
    public mutating func next() -> Element? {
        return _suffixIterator.next() ?? _prefixIterator.next()
    }
}

extension RotatedCollection: Sequence {
    /// A type that represents a valid position in the collection.
    ///
    /// Valid indices consist of the position of every element and a
    /// "past the end" position that's not valid for use as a subscript.
    public typealias Element = Base.Element

    @inlinable
    @inline(__always)
    public __consuming func makeIterator() -> Iterator {
        return Iterator(_base: _base, asFirst: _origin)
    }

    @inlinable
    @inline(__always)
    public var underestimatedCount: Int { return _base.underestimatedCount }
}

extension RotatedCollection {
    /// An index that traverses the same positions as an underlying index, in a
    /// rotated order.
    @_fixed_layout
    public struct Index {
        /// The position of the selected element in the underlying collection,
        /// or `nil` to indicate the past-the-end position.
        ///
        /// To translate in the other direction, pass a `Base.Index` value to
        /// the `RotatedCollection.rotation(of:)` method that is valid for its
        /// underlying collection to get a corresponding `Index` value.
        public let base: Base.Index?
        /// The position in the underlying collection of the source collection's
        /// `startIndex`.
        @usableFromInline
        internal let _origin: Base.Index

        /// Creates an index for the past-the-end position with the given index
        /// as the rotated starting index.
        ///
        /// - Parameter middle: The source collection's `startIndex`, relative
        ///   to the underlying collection.
        ///
        /// - Complexity: O(1)
        @inlinable
        @inline(__always)
        internal init(startingFrom middle: Base.Index) {
            base = nil
            _origin = middle
        }
        /// Creates an index into a rotated collection for the given base index,
        /// with the other given index as the starting position.
        ///
        /// - Precondition: `base` cannot be the underlying collection's
        ///   `endIndex`.
        ///
        /// - Parameter base: The selected element's index relative to the
        ///   underlying collection.
        /// - Parameter middle: The source collection's `startIndex`, relative
        ///   to the underlying collection.
        ///
        /// - Complexity: O(1)
        @inlinable
        @inline(__always)
        internal init(_ base: Base.Index, startingFrom middle: Base.Index) {
            self.base = base
            _origin = middle
        }
    }
}

extension RotatedCollection.Index: Equatable {
    @inlinable
    public static func == (lhs: RotatedCollection.Index,
                           rhs: RotatedCollection.Index) -> Bool {
        precondition(lhs._origin == rhs._origin)
        return lhs.base == rhs.base
    }
}

extension RotatedCollection.Index: Comparable {
    @inlinable
    public static func < (lhs: RotatedCollection.Index,
                          rhs: RotatedCollection.Index) -> Bool {
        precondition(lhs._origin == rhs._origin)

        // Resolve past-the-end states.
        switch (lhs.base, rhs.base) {
        case (nil, _):
            return false
        case (_?, nil):
            return true
        case let (lb?, rb?):
            switch (lb < lhs._origin, rb < rhs._origin) {
            case (true, false):
                // Recall that the prefix elements occur later in the rotated...
                return false
            case (false, true):
                // ...collection than suffix elements!
                return true
            case (false, false), (true, true):
                // Direct comparison works if they're in the same partition.
                return lb < rb
            }
        }
    }
}

extension RotatedCollection.Index: Hashable where Base.Index: Hashable {
    /// Hashes the essential components of this value by feeding them into the
    /// given hasher.
    ///
    /// - Parameter hasher: The hasher to use when combining the components
    ///   of this instance.
    @inlinable
    public func hash(into hasher: inout Hasher) {
        hasher.combine(base)
        hasher.combine(_origin)
    }
}

extension RotatedCollection {

    // TODO: Add examples for all methods in this block

    /// Translates the given index from the base collection to an index for this
    /// collection.
    ///
    /// Passing the base collection's `endIndex` always returns this
    /// collection's `endIndex`.
    ///
    /// For translations in the other direction, use the `Index` instance's
    /// `base` property.  A value of `nil` corresponds to the base collection's
    /// `endIndex`.
    ///
    /// - Precondition: `base` is a valid index for the underlying collection.
    ///
    /// - Parameter base: The index in the underlying collection to be rotated.
    ///
    /// - Returns: The index in this collection for the corresponding element
    ///   in the underlying collection selected by `base`.
    ///
    /// - Complexity: O(1)
    @inlinable
    func rotation(of base: Base.Index) -> Index {
        guard base < _base.endIndex else {
            return Index(startingFrom: _origin)
        }

        return Index(base, startingFrom: _origin)
    }

    /// Realigns the elements of the collection such that the value at the given
    /// index moves leftward to `startIndex`.
    ///
    /// Passing `startIndex` or `endIndex` as `middle` has no effect.  If there
    /// is an effect, all outstanding `Index` values are invalid.
    ///
    /// - Precondition: `middle` must be a valid index of this collection.
    ///
    /// - Parameter middle: The index of the element whose value will move
    ///   to `startIndex`, unless it's `endIndex` (then it's ignored).
    ///
    /// - Returns: The new index of the value originally at `startIndex`.
    ///
    /// - Postcondition: The collection is a left-rotation of its old state,
    ///   *i.e.* `newValue` equals `oldValue[middle...] + oldValue[..<middle]`.
    ///
    /// - Complexity: O(1)
    @discardableResult
    public mutating func reseat(asFirst middle: Index) -> Index {
        precondition(middle._origin == _origin)
        guard var newOrigin = middle.base else { return startIndex }

        swap(&_origin, &newOrigin)
        return rotation(of: newOrigin)
    }

    /// Realigns the elements of the collection such that the value at the given
    /// index moves rightward to just before `endIndex`.
    ///
    /// Passing `endIndex` or its immediate predecessor (if any) as `middle` has
    /// no effect.  If there is an effect, all outstanding `Index` values are
    /// invalid.
    ///
    /// - Precondition: `middle` must be a valid index of this collection.
    ///
    /// - Parameter middle: The index of the element whose value will move to
    ///   just before `endIndex`, unless it's `endIndex` (then it's ignored).
    ///
    /// - Returns: The new index of the value originally at `startIndex`.
    ///
    /// - Postcondition: The collection is a right-rotation of its old state,
    ///   *i.e.* `newValue` equals `oldValue[middle>..] + oldValue[...middle]`.
    ///
    /// - Complexity: O(1)
    @inlinable
    @discardableResult
    public mutating func reseat(asLast middle: Index) -> Index {
        return reseat(asFirst: index(middle, offsetBy: +1, limitedBy: endIndex)
            ?? startIndex)
    }
}

extension RotatedCollection: Collection {
    @inlinable
    @inline(__always)
    public var startIndex: Index { return rotation(of: _origin) }
    @inlinable
    @inline(__always)
    public var endIndex: Index { return Index(startingFrom: _origin) }

    @inlinable
    public subscript(position: Index) -> Element {
        precondition(position._origin == _origin)
        return _base[position.base!]
    }

    // Slice<Self> is used instead of RotatedCollection<SubSequence> because a
    // rotated range may correspond to a discontinuous underlying range.  Due to
    // this, the default definition of subscript(bounds:) is used.

    @inlinable
    @inline(__always)
    public var isEmpty: Bool { return _base.isEmpty }
    @inlinable
    @inline(__always)
    public var count: Int { return _base.count }

    // TODO: Should the underscored equatable-searching methods be forwarded?

    // TODO: Override index(_: offsetBy:) and index(_: offsetBy: limitedBy:)

    public func distance(from start: Index, to end: Index) -> Int {
        precondition(start._origin == _origin)
        precondition(end._origin == _origin)

        switch (start.base, end.base) {
        case (nil, nil):
            // Both past-the-end
            return 0
        case (let sb?, nil):
            // Measure to end
            if sb < _origin {
                // Start and past-the-end in same partition
                return _base.distance(from: sb, to: _origin)
            } else {
                // Wrap around, include the entire prefix partition
                return _base.distance(from: sb, to: _base.endIndex)
                    + _base.distance(from: _base.startIndex, to: _origin)
            }
        case (nil, let eb?):
            // Like the above case, but backwards
            if eb < _origin {
                return _base.distance(from: _origin, to: eb)
            } else {
                return _base.distance(from: _base.endIndex, to: eb)
                    + _base.distance(from: _origin, to: _base.startIndex)
            }
        case let (sb?, eb?):
            switch (sb < _origin, eb < _origin) {
            case (true, false):
                // Wrap around backwards
                return _base.distance(from: _origin, to: sb)
                    + _base.distance(from: _base.endIndex, to: eb)
            case (false, true):
                // Wrap around
                return _base.distance(from: sb, to: _base.endIndex)
                    + _base.distance(from: _base.startIndex, to: eb)
            case (false, false), (true, true):
                // Direct use works if they're in the same partition
                return _base.distance(from: sb, to: eb)
            }
        }
    }

    // TODO: Should the underscored range-checking methods be forwarded?

    public func index(after i: Index) -> Index {
        precondition(i._origin == _origin)

        let nextBaseIndex = _base.index(after: i.base!)
        switch nextBaseIndex {
        case _base.endIndex:
            return rotation(of: _base.startIndex)
        case _origin:
            return endIndex
        default:
            return rotation(of: nextBaseIndex)
        }
    }
    public func formIndex(after i: inout Index) {
        precondition(i._origin == _origin)

        var ib = i.base!
        _base.formIndex(after: &ib)
        switch ib {
        case _base.endIndex:
            i = rotation(of: _base.startIndex)
        case _origin:
            i = endIndex
        default:
            i = rotation(of: ib)
        }
    }
}

extension RotatedCollection: BidirectionalCollection
where Base: BidirectionalCollection {
    public func index(before i: Index) -> Index {
        precondition(i._origin == _origin)
        precondition(i.base != _origin)

        var ib = i.base ?? _origin
        if ib == _base.startIndex {
            ib = _base.endIndex
        }
        return rotation(of: _base.index(before: ib))
    }
    public func formIndex(before i: inout Index) {
        precondition(i._origin == _origin)
        precondition(i.base != _origin)

        var ib = i.base ?? _origin
        if ib == _base.startIndex {
            ib = _base.endIndex
        }
        _base.formIndex(before: &ib)
        i = rotation(of: ib)
    }

    // TODO: Override index(_: offsetBy:) and index(_: offsetBy: limitedBy:)
}

extension RotatedCollection: RandomAccessCollection
where Base: RandomAccessCollection {
    public func index(_ i: Index, offsetBy distance: Int) -> Index {
        //var d = distance
        precondition(i._origin == _origin)
        guard case var d = distance, d != 0 else { return i }
        precondition(!isEmpty)
        guard var ib = i.base else {
            // Easiest to reset the offsetting to be based on startIndex.
            return index(startIndex, offsetBy: d + count)
        }

        switch (distance < 0, ib < _origin) {
        case (false, false):
            // Positive movement in suffix partition
            guard let result = _base.index(ib, offsetBy: d,
                                           limitedBy: _base.endIndex) else {
                d -= _base.distance(from: ib, to: _base.endIndex)
                ib = _base.startIndex
                fallthrough
            }

            return rotation(of: result)
        case (false, true):
            // Positive movement in prefix partition
            return rotation(of: _base.index(ib, offsetBy: d,
                                            limitedBy: _origin)!)
        case (true, true):
            // Negative movement in prefix partition
            guard let result = _base.index(ib, offsetBy: d,
                                           limitedBy: _base.startIndex) else {
                d -= _base.distance(from: ib, to: _base.startIndex)
                ib = _base.endIndex
                fallthrough
            }

            return rotation(of: result)
        case (true, false):
            // Negative movement in suffix partition
            return rotation(of: _base.index(ib, offsetBy: d,
                                            limitedBy: _origin)!)
        }
    }

    // Punt to the default definition of index(_: offsetBy: limitedBy:).
}

extension RotatedCollection {
    /// Returns a view presenting the elements of the collection rotated
    /// leftward such that the element at the given index is now first.
    ///
    /// Passing `endIndex` (or `startIndex`) as `middle` produces a collection
    /// with its rotation amount unchanged.
    ///
    /// You can left-rotate a collection without allocating new space for its
    /// elements by calling `rotated(toFirst:)`. A `RotatedCollection`
    /// instance wraps an underlying collection and provides access to its
    /// elements in a rotated order. This example prints the characters of a
    /// string left-rotated twice:
    ///
    ///     let word = "Backwards"
    ///     let rotatedOnce = word.rotated(toFirst: word.index(after:
    ///      word.startIndex))
    ///     for char in rotatedOnce.rotated(toFirst: rotatedOnce.index(after:
    ///      rotatedOnce.startIndex)) {
    ///         print(char, terminator: "")
    ///     }
    ///     // Prints "ckwardsBa"
    ///
    /// - Precondition: `middle` must be a valid index of this collection.
    ///
    /// - Parameter middle: The index of the element that will be the new
    ///   `startIndex`, unless it's `endIndex` (then it's ignored).
    ///
    /// - Returns: A `RotatedCollection` with the same wrapped underlying
    ///   collection as `self`, but aligned to start at `middle.base`.
    ///
    /// - Complexity: O(1)
    @inlinable
    func rotated(toFirst middle: Index) -> RotatedCollection {
        return _base.rotated(toFirst: middle.base ?? _origin)
    }

    /// Returns a view presenting the elements of the collection rotated
    /// rightward such that the element at the given index is now last.
    ///
    /// Passing `endIndex` (or its immediate predecessor, if any) as `middle`
    /// produces a collection with its rotation amount unchanged.
    ///
    /// You can right-rotate a collection without allocating new space for its
    /// elements by calling `rotated(toLast:)`. A `RotatedCollection`
    /// instance wraps an underlying collection and provides access to its
    /// elements in a rotated order. This example prints the characters of a
    /// string right-rotated twice:
    ///
    ///     let word = "Backwards"
    ///     let rotatedOnce = word.rotated(toLast: word.index(word.endIndex,
    ///      offsetBy: -2))
    ///     for char in rotatedOnce.rotated(toLast:
    ///      rotatedOnce.index(rotatedOnce.endIndex, offsetBy: -2)) {
    ///         print(char, terminator: "")
    ///     }
    ///     // Prints "dsBackwar"
    ///
    /// - Precondition: `middle` must be a valid index of this collection.
    ///
    /// - Parameter middle: The index of the element that will be the new one
    ///   just before `endIndex`, unless it's `endIndex` (then it's ignored).
    ///
    /// - Returns: A `RotatedCollection` with the same wrapped underlying
    ///   collection as `self`, but aligned to end at `middle.base`.
    ///
    /// - Complexity: O(1)
    @inlinable
    func rotated(toLast middle: Index) -> RotatedCollection {
        guard let mb = middle.base else {
            return _base.rotated(toFirst: _origin)
        }
        return _base.rotated(toLast: mb)
    }
}

extension Collection {
    /// Returns a view presenting the elements of the collection rotated
    /// leftward such that the element at the given index is now first.
    ///
    /// Passing `endIndex` (or `startIndex`) as `middle` produces an unrotated
    /// collection.
    ///
    /// You can left-rotate a collection without allocating new space for its
    /// elements by calling `rotated(toFirst:)`. A `RotatedCollection`
    /// instance wraps an underlying collection and provides access to its
    /// elements in a rotated order. This example prints the characters of a
    /// string left-rotated once:
    ///
    ///     let word = "Backwards"
    ///     for char in word.rotated(toFirst: word.index(after:
    ///      word.startIndex)) {
    ///         print(char, terminator: "")
    ///     }
    ///     // Prints "ackwardsB"
    ///
    /// If you need a rotated collection of the same type, you may be able to
    /// use the collection's sequence-based or collection-based initializer. For
    /// example, to get the rotated version of a string, rotate its
    /// characters and initialize a new `String` instance from the result.
    ///
    ///     let rotatedWord = String(word.rotated(toFirst: word.index(after:
    ///      word.startIndex)))
    ///     print(rotatedWord)
    ///     // Prints "ackwardsB"
    ///
    /// - Precondition: `middle` must be a valid index of this collection.
    ///
    /// - Parameter middle: The index of the element that will be the new
    ///   `startIndex`, unless it's `endIndex` (then it's ignored).
    ///
    /// - Returns: A `RotatedCollection` wrapping (a copy of) `self`, but
    ///   aligned to start at `middle`.
    ///
    /// - Complexity: O(1)
    @inlinable
    public __consuming func rotated(toFirst middle: Index)
        -> RotatedCollection<Self> {
        return RotatedCollection(_base: self, asFirst: middle)
    }

    /// Returns a view presenting the elements of the collection rotated
    /// rightward such that the element at the given index is now last.
    ///
    /// Passing `endIndex` (or its immediate predecessor, if any) as `middle`
    /// produces an unrotated collection.
    ///
    /// You can right-rotate a collection without allocating new space for its
    /// elements by calling `rotated(toLast:)`. A `RotatedCollection`
    /// instance wraps an underlying collection and provides access to its
    /// elements in a rotated order. This example prints the characters of a
    /// string right-rotated once:
    ///
    ///     let word = "Backwards"
    ///     for char in word.rotated(toLast: word.index(word.endIndex,
    ///      offsetBy: -2)) {
    ///         print(char, terminator: "")
    ///     }
    ///     // Prints "sBackward"
    ///
    /// If you need a rotated collection of the same type, you may be able to
    /// use the collection's sequence-based or collection-based initializer. For
    /// example, to get the rotated version of a string, rotate its
    /// characters and initialize a new `String` instance from the result.
    ///
    ///     let rotatedWord = String(word.rotated(toLast:
    ///      word.index(word.endIndex, offsetBy: -2)))
    ///     print(rotatedWord)
    ///     // Prints "sBackward"
    ///
    /// - Precondition: `middle` must be a valid index of this collection.
    ///
    /// - Parameter middle: The index of the element that will be the new one
    ///   just before `endIndex`, unless it's `endIndex` (then it's ignored).
    ///
    /// - Returns: A `RotatedCollection` wrapping (a copy of) `self`, but
    ///   aligned to end at `middle`.
    ///
    /// - Complexity: O(1)
    @inlinable
    public __consuming func rotated(toLast middle: Index)
        -> RotatedCollection<Self> {
        return rotated(toFirst: index(middle, offsetBy: +1, limitedBy: endIndex)
            ?? startIndex)
    }
}
