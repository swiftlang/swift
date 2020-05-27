////===--- _EitherSequence.swift - A sequence type-erasing two sequences -----===//
////
//// This source file is part of the Swift.org open source project
////
//// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
//// Licensed under Apache License v2.0 with Runtime Library Exception
////
//// See https://swift.org/LICENSE.txt for license information
//// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
////
////===----------------------------------------------------------------------===//

// Not public stdlib API, currently used in Mirror.children implementation.

internal enum _Either<Left, Right> {
  case left(Left), right(Right)
}

extension _Either {
  internal init(_ left: Left, or other: Right.Type) { self = .left(left) }
  internal init(_ left: Left) { self = .left(left) }
  internal init(_ right: Right) { self = .right(right) }
}

extension _Either: Equatable where Left: Equatable, Right: Equatable {
  internal static func == (lhs: Self, rhs: Self) -> Bool {
    switch (lhs, rhs) {
    case let (.left(l), .left(r)): return l == r
    case let (.right(l), .right(r)): return l == r
    case (.left, .right), (.right, .left): return false
    }
  }
}

extension _Either: Comparable where Left: Comparable, Right: Comparable {
  internal static func < (lhs: Self, rhs: Self) -> Bool {
    switch (lhs, rhs) {
    case let (.left(l), .left(r)): return l < r
    case let (.right(l), .right(r)): return l < r
    case (.left, .right): return true
    case (.right, .left): return false
    }
  }
}

/// A sequence that type erases two sequences. A lighter-weight alternative to
/// AnySequence when more can be statically known, and which is more easily
/// specialized.
///
/// If you  only know about one of the types, the second one can be
/// AnySequence, giving you a fast path for the known one.
///
/// If you have 3+ types to erase, you can nest them.
typealias _EitherSequence<L: Sequence, R: Sequence> =
  _Either<L,R> where L.Element == R.Element

extension _EitherSequence {
  internal struct Iterator {
    var left: Left.Iterator?
    var right: Right.Iterator?
  }
}

extension _Either.Iterator: IteratorProtocol {
  internal typealias Element = Left.Element

  internal mutating func next() -> Element? {
    left?.next() ?? right?.next()
  }
}

extension _EitherSequence: Sequence {
  internal typealias Element = Left.Element

  internal func makeIterator() -> Iterator {
    switch self {
    case let .left(l):
      return Iterator(left: l.makeIterator(), right: nil)
    case let .right(r):
      return Iterator(left: nil, right: r.makeIterator())
    }
  }
}

internal typealias _EitherCollection<
  T: Collection, U: Collection
> = _EitherSequence<T,U> where T.Element == U.Element

extension _EitherCollection: Collection {
  internal typealias Index = _Either<Left.Index, Right.Index>

  internal var startIndex: Index {
    switch self {
    case let .left(s): return .left(s.startIndex)
    case let .right(s): return .right(s.startIndex)
    }
  }

  internal var endIndex: Index {
    switch self {
    case let .left(s): return .left(s.endIndex)
    case let .right(s): return .right(s.endIndex)
    }
  }

  internal subscript(position: Index) -> Element {
    switch (self,position) {
    case let (.left(s),.left(i)): return s[i]
    case let (.right(s),.right(i)): return s[i]
    default: fatalError("_EitherCollecton: Sequence used with other index type")
    }
  }

  internal func index(after i: Index) -> Index {
    switch (self,i) {
    case let (.left(s),.left(i)): return .left(s.index(after: i))
    case let (.right(s),.right(i)): return .right(s.index(after: i))
    default: fatalError("_EitherCollecton: wrong type of index used")
    }
  }

  internal func index(
    _ i: Index,
    offsetBy distance: Int,
    limitedBy limit: Index
  ) -> Index? {
    switch (self,i,limit) {
    case let (.left(s),.left(i),.left(limit)):
      return s.index(i, offsetBy: distance, limitedBy: limit).map { .left($0) }
    case let (.right(s),.right(i),.right(limit)):
      return s.index(i, offsetBy: distance, limitedBy: limit).map { .right($0) }
    default: fatalError("_EitherCollecton: wrong type of index used")
    }
  }

  internal func index(_ i: Index, offsetBy distance: Int) -> Index {
    switch (self,i) {
    case let (.left(s),.left(i)): return .left(s.index(i, offsetBy: distance))
    case let (.right(s),.right(i)): return .right(s.index(i, offsetBy: distance))
    default: fatalError("_EitherCollecton: wrong type of index used")
    }
  }

  internal func distance(from start: Index, to end: Index) -> Int {
    switch (self,start,end) {
    case let (.left(s),.left(i),.left(j)):
      return s.distance(from: i, to: j)
    case let (.right(s),.right(i),.right(j)):
      return s.distance(from: i, to: j)
    default: fatalError("_EitherCollecton: wrong type of index used")
    }
  }
}

internal typealias _EitherBidirectionalCollection<
  L: BidirectionalCollection, R: BidirectionalCollection
> = _Either<L,R> where L.Element == R.Element

extension _EitherBidirectionalCollection: BidirectionalCollection {
  internal func index(before i: Index) -> Index {
    switch (self,i) {
    case let (.left(s),.left(i)): return .left(s.index(before: i))
    case let (.right(s),.right(i)): return .right(s.index(before: i))
    default: fatalError("_EitherCollecton: wrong type of index used")
    }
  }
}

internal typealias _EitherRandomAccessCollection<
  L: RandomAccessCollection, R: RandomAccessCollection
> = _Either<L,R> where L.Element == R.Element

extension _EitherRandomAccessCollection: RandomAccessCollection { }

extension _Either {
  init<T, C: Collection>(
    _ collection: C
  ) where Right == AnyCollection<T>, C.Element == T {
    self = .right(AnyCollection(collection))
  }
}

extension AnyCollection {
  init<L: Collection,R: Collection>(
    _ other: _Either<L,R>
  ) where L.Element == Element, R.Element == Element {
    // Strip away the Either and put the actual collection into the existential,
    // trying to use the custom initializer from another AnyCollection.
    switch other {
    case let .left(l as Self): self = .init(l)
    case let .right(r as Self): self = .init(r)
    case let .left(l): self = .init(l)
    case let .right(r): self = .init(r)
    }
  }
}

