//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// A sequence of pairs built out of two underlying sequences, where
/// the elements of the `i`th pair are the `i`th elements of each
/// underlying sequence. Passes nil instead of element of underlying sequence
/// when there is no `i`th element.  Finishes when both of sequences finish.
/// For example: zipWithNilPadding(["one", "two", "three", "four"], (1, 2, 3))
/// returns a sequence ("one", 1), ("two", 2), ("three", 3), ("four", nil).
public func zipWithNilPadding<Sequence1 : SequenceType, Sequence2 : SequenceType>(
  sequence1: Sequence1, _ sequence2: Sequence2
) -> Zip2SequenceWithNilPadding<Sequence1, Sequence2> {
  return Zip2SequenceWithNilPadding(sequence1, sequence2)
}

/// A generator for `Zip2SequenceWithNilPadding`.
public struct Zip2GeneratorWithNilPadding<
  Generator1 : GeneratorType, Generator2 : GeneratorType
> : GeneratorType {
  /// The type of element returned by `next()`.
  public typealias Element = (Generator1.Element?, Generator2.Element?)

  /// Construct around a pair of underlying generators.
  public init(_ generator1: Generator1, _ generator2: Generator2) {
    (_baseStream1, _baseStream2) = (generator1, generator2)
  }

  /// Advance to the next element and return it, or `nil` if no next
  /// element exists.
  ///
  /// - Requires: `next()` has not been applied to a copy of `self`
  ///   since the copy was made, and no preceding call to `self.next()`
  ///   has returned `nil`.
  public mutating func next() -> Element? {
    let element1 = self._baseStream1.next()
    let element2 = self._baseStream2.next()

    if nil == element1 && nil == element2 {
        return nil
    } else {
        return (element1, element2)
    }
  }

  internal var _baseStream1: Generator1
  internal var _baseStream2: Generator2
}

/// A sequence of pairs built out of two underlying sequences, where
/// the elements of the `i`th pair are the `i`th elements of each
/// underlying sequence.  Finishes when both of sequences finish.
/// For example: Zip2SequenceWithNilPadding(["one", "two", "three", "four"], (1, 2, 3))
/// returns a sequence ("one", 1), ("two", 2), ("three", 3), ("four", nil).
public struct Zip2SequenceWithNilPadding<Sequence1 : SequenceType, Sequence2 : SequenceType>
  : SequenceType {

  public typealias Stream1 = Sequence1.Generator
  public typealias Stream2 = Sequence2.Generator

  /// A type whose instances can produce the elements of this
  /// sequence, in order.
  public typealias Generator = Zip2GeneratorWithNilPadding<Stream1, Stream2>

  /// Construct an instance that makes pairs of elements from `sequence1` and
  /// `sequence2`.
  public init(_ sequence1: Sequence1, _ sequence2: Sequence2) {
    (_sequence1, _sequence2) = (sequence1, sequence2)
  }

  /// Return a *generator* over the elements of this *sequence*.
  ///
  /// - Complexity: O(1).
  public func generate() -> Generator {
    return Generator(
      _sequence1.generate(),
      _sequence2.generate())
  }

  internal let _sequence1: Sequence1
  internal let _sequence2: Sequence2
}
