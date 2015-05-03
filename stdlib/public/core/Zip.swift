//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// A sequence of pairs built out of two underlying sequences, where
/// the elements of the `i`th pair are the `i`th elements of each
/// underlying sequence.
public func zip<Sequence1 : SequenceType, Sequence2 : SequenceType>(
  sequence1: Sequence1, _ sequence2: Sequence2
) -> Zip2<Sequence1, Sequence2> {
  return Zip2(sequence1, sequence2)
}

/// A generator for the `Zip2` sequence
public struct ZipGenerator2<
  Generator1 : GeneratorType, Generator2 : GeneratorType
> : GeneratorType {
  /// The type of element returned by `next()`.
  public typealias Element = (Generator1.Element, Generator2.Element)

  /// Construct around a pair of underlying generators.
  public init(_ generator1: Generator1, _ generator2: Generator2) {
    _baseStreams = (generator1, generator2)
  }

  /// Advance to the next element and return it, or `nil` if no next
  /// element exists.
  ///
  /// Requires: `next()` has not been applied to a copy of `self`
  /// since the copy was made, and no preceding call to `self.next()`
  /// has returned `nil`.
  public mutating func next() -> Element? {
    // The next() function needs to track if it has reached the end.  If we
    // didn't, and the first sequence is shorter than the second, then, when we
    // have already exhausted the second sequence, on every subsequent call to
    // next() we would consume and discard one additional element from the
    // first sequence, even though next() return nil.

    if _reachedEnd {
      return nil
    }

    require let e0 = _baseStreams.0.next() else {
      _reachedEnd = true
      return nil
    }

    require let e1 = _baseStreams.1.next() else {
      _reachedEnd = true
      return nil
    }

    return .Some((e0, e1))
  }

  internal var _baseStreams: (Generator1, Generator2)
  internal var _reachedEnd: Bool = false
}

/// A sequence of pairs built out of two underlying sequences, where
/// the elements of the `i`th pair are the `i`th elements of each
/// underlying sequence.
public struct Zip2<Sequence1 : SequenceType, Sequence2 : SequenceType>
  : SequenceType {

  public typealias Stream1 = Sequence1.Generator
  public typealias Stream2 = Sequence2.Generator

  /// A type whose instances can produce the elements of this
  /// sequence, in order.
  public typealias Generator = ZipGenerator2<Stream1, Stream2>

  /// Construct an instance that makes pairs of elements from `sequence1` and
  /// `sequence2`.
  public init(_ sequence1: Sequence1, _ sequence2: Sequence2) {
    _sequences = (sequence1, sequence2)
  }

  /// Return a *generator* over the elements of this *sequence*.
  ///
  /// - complexity: O(1)
  public func generate() -> Generator {
    return Generator(
      _sequences.0.generate(),
      _sequences.1.generate())
  }

  internal let _sequences: (Sequence1, Sequence2)
}
