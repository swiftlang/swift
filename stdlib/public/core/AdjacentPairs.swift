//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// A sequence of adjacent pairs of elements built from an underlying sequence.
///
/// In an `AdjacentPairsSequence`, the elements of the *i*th pair are the *i*th
/// and *(i+1)*th elements of the underlying sequence. The following example
/// uses the `adjacentPairs()` method to iterate over adjacent pairs of integers:
///
///    for pair in (1...5).adjacentPairs() {
///        print(pair)
///    }
///    // Prints "(1, 2)"
///    // Prints "(2, 3)"
///    // Prints "(3, 4)"
///    // Prints "(4, 5)"
@_fixed_layout
public struct AdjacentPairsSequence<Base: Sequence> {
    @usableFromInline
    internal let _base: Base

    /// Creates an instance that makes pairs of adjacent elements from `base`.
    @inlinable
    public init(_base: Base) {
        self._base = _base
    }
}

extension AdjacentPairsSequence {
    /// An iterator for `AdjacentPairsSequence`.
    @_fixed_layout
    public struct Iterator {
        @usableFromInline
        internal var _base: Base.Iterator

        @usableFromInline
        internal var _previousElement: Base.Element?

        /// Creates an instance around an underlying iterator.
        @inlinable
        internal init(_base: Base.Iterator) {
            self._base = _base
            self._previousElement = self._base.next()
        }
    }
}

extension AdjacentPairsSequence.Iterator: IteratorProtocol {
    /// The type of element returned by `next()`.
    public typealias Element = (Base.Element, Base.Element)

    /// Advances to the next element and returns it, or `nil` if no next element
    /// exists.
    ///
    /// Once `nil` has been returned, all subsequent calls return `nil`.
    @inlinable
    public mutating func next() -> Element? {
        guard let previous = _previousElement, let next = _base.next() else {
            return nil
        }
        _previousElement = next
        return (previous, next)
    }
}

extension AdjacentPairsSequence: Sequence {
    /// Returns an iterator over the elements of this sequence.
    @inlinable
    public func makeIterator() -> Iterator {
        return Iterator(_base: _base.makeIterator())
    }

    /// A value less than or equal to the number of elements in the sequence,
    /// calculated nondestructively.
    ///
    /// The default implementation returns 0. If you provide your own
    /// implementation, make sure to compute the value nondestructively.
    ///
    /// - Complexity: O(1), except if the sequence also conforms to `Collection`.
    ///   In this case, see the documentation of `Collection.underestimatedCount`.
    @inlinable
    public var underestimatedCount: Int {
        return Swift.max(0, _base.underestimatedCount - 1)
    }
}

extension Sequence {
    /// Creates a sequence of adjacent pairs of elements from this sequence.
    ///
    /// In the `AdjacentPairsSequence` instance returned by this method, the elements of
    /// the *i*th pair are the *i*th and *(i+1)*th elements of the underlying sequence.
    /// The following example uses the `adjacentPairs()` method to iterate over adjacent
    /// pairs of integers:
    ///
    ///    for pair in (1...5).adjacentPairs() {
    ///        print(pair)
    ///    }
    ///    // Prints "(1, 2)"
    ///    // Prints "(2, 3)"
    ///    // Prints "(3, 4)"
    ///    // Prints "(4, 5)"
    @inlinable
    public func adjacentPairs() -> AdjacentPairsSequence<Self> {
        return AdjacentPairsSequence(_base: self)
    }
}
