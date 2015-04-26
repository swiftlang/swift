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
public func zip<S0: SequenceType, S1: SequenceType>(
  s0: S0, _ s1: S1) -> Zip2<S0, S1> {
  // FIXME(prext): remove this function when protocol extensions land.
  return s0._prext_zip(s1)
}

/// A generator for the `Zip2` sequence
public struct ZipGenerator2<
  E0 : GeneratorType, E1 : GeneratorType
> : GeneratorType {
  /// The type of element returned by `next()`.
  public typealias Element = (E0.Element,E1.Element)

  /// Construct around a pair of underlying generators.
  public init(_ e0: E0, _ e1: E1) {
    baseStreams = (e0,e1)
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

    if reachedEnd {
      return nil
    }

    var e0 = baseStreams.0.next()
    if e0 == nil {
      reachedEnd = true
      return nil
    }
    var e1 = baseStreams.1.next()
    if e1 ==  nil {
      reachedEnd = true
      return nil
    }
    return .Some((e0!, e1!))
  }

  var baseStreams: (E0, E1)
  var reachedEnd: Bool = false
}

/// A sequence of pairs built out of two underlying sequences, where
/// the elements of the `i`th pair are the `i`th elements of each
/// underlying sequence.
public struct Zip2<S0: SequenceType, S1: SequenceType> : SequenceType
{
  public typealias Stream1 = S0.Generator
  public typealias Stream2 = S1.Generator
  
  /// A type whose instances can produce the elements of this
  /// sequence, in order.
  public typealias Generator = ZipGenerator2<Stream1, Stream2>

  /// Construct an instance that makes pairs of elements from `s0` and
  /// `s1`.
  public init(_ s0: S0, _ s1: S1) {
    sequences = (s0,s1)
  }

  /// Return a *generator* over the elements of this *sequence*.
  ///
  /// - complexity: O(1)
  public func generate() -> Generator {
    return Generator(
      sequences.0.generate(), 
      sequences.1.generate())
  }

  var sequences: (S0,S1)
}
