//===--- Join.swift - Protocol and Algorithm for concatenation ------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

internal enum _JoinIteratorState {
  case start
  case generatingElements
  case generatingSeparator
  case end
}

/// An iterator that presents the elements of the sequences traversed
/// by `Base`, concatenated using a given separator.
public struct JoinIterator<
  Base : IteratorProtocol where Base.Element : Sequence
> : IteratorProtocol {

  /// Creates an iterator that presents the elements of the sequences
  /// traversed by `base`, concatenated using `separator`.
  ///
  /// - Complexity: O(`separator.count`).
  public init<
    Separator : Sequence
    where
    Separator.Iterator.Element == Base.Element.Iterator.Element
  >(base: Base, separator: Separator) {
    self._base = base
    self._separatorData = ContiguousArray(separator)
  }

  /// Advance to the next element and return it, or `nil` if no next
  /// element exists.
  public mutating func next() -> Base.Element.Iterator.Element? {
    repeat {
      switch _state {
      case .start:
        if let nextSubSequence = _base.next() {
          _inner = nextSubSequence.iterator()
          _state = .generatingElements
        } else {
          _state = .end
          return nil
        }

      case .generatingElements:
        let result = _inner!.next()
        if _fastPath(result != nil) {
          return result
        }
        _inner = _base.next()?.iterator()
        if _inner == nil {
          _state = .end
          return nil
        }
        if !_separatorData.isEmpty {
          _separator = _separatorData.iterator()
          _state = .generatingSeparator
        }

      case .generatingSeparator:
        let result = _separator!.next()
        if _fastPath(result != nil) {
          return result
        }
        _state = .generatingElements

      case .end:
        return nil

      }
    }
    while true
  }

  internal var _base: Base
  internal var _inner: Base.Element.Iterator?
  internal var _separatorData: ContiguousArray<Base.Element.Iterator.Element>
  internal var _separator:
    ContiguousArray<Base.Element.Iterator.Element>.Iterator?
  internal var _state: _JoinIteratorState = .start
}

/// A sequence that presents the elements of the `Base` sequences
/// concatenated using a given separator.
public struct JoinSequence<
  Base : Sequence where Base.Iterator.Element : Sequence
> : Sequence {

  /// Creates a sequence that presents the elements of `base` sequences
  /// concatenated using `separator`.
  ///
  /// - Complexity: O(`separator.count`).
  public init<
    Separator : Sequence
    where
    Separator.Iterator.Element == Base.Iterator.Element.Iterator.Element
  >(base: Base, separator: Separator) {
    self._base = base
    self._separator = ContiguousArray(separator)
  }

  /// Return an iterator over the elements of this sequence.
  ///
  /// - Complexity: O(1).
  public func iterator() -> JoinIterator<Base.Iterator> {
    return JoinIterator(
      base: _base.iterator(),
      separator: _separator)
  }

  public func _copyToNativeArrayBuffer()
    -> _ContiguousArrayBuffer<Base.Iterator.Element.Iterator.Element> {
    var result = ContiguousArray<Iterator.Element>()
    let separatorSize: Int = numericCast(_separator.count)

    let reservation = _base._preprocessingPass {
      () -> Int in
      var r = 0
      for chunk in _base {
        r += separatorSize + chunk.underestimatedCount
      }
      return r - separatorSize
    }

    if let n = reservation {
      result.reserveCapacity(numericCast(n))
    }

    if separatorSize != 0 {
      var iter = _base.iterator()
      if let first = iter.next() {
        result.appendContentsOf(first)
        while let next = iter.next() {
          result.appendContentsOf(_separator)
          result.appendContentsOf(next)
        }
      }
    } else {
      for x in _base {
        result.appendContentsOf(x)
      }
    }

    return result._buffer
  }

  internal var _base: Base
  internal var _separator:
    ContiguousArray<Base.Iterator.Element.Iterator.Element>
}

extension Sequence where Iterator.Element : Sequence {
  /// Returns a view, whose elements are the result of interposing a given
  /// `separator` between the elements of the sequence `self`.
  ///
  /// For example,
  /// `[[1, 2, 3], [4, 5, 6], [7, 8, 9]].join(separator: [-1, -2])`
  /// yields `[1, 2, 3, -1, -2, 4, 5, 6, -1, -2, 7, 8, 9]`.
  @warn_unused_result
  public func join<
    Separator : Sequence
    where
    Separator.Iterator.Element == Iterator.Element.Iterator.Element
  >(separator separator: Separator) -> JoinSequence<Self> {
    return JoinSequence(base: self, separator: separator)
  }
}

@available(*, unavailable, renamed="JoinIterator")
public struct JoinGenerator<
  Base : IteratorProtocol where Base.Element : Sequence
> {}

extension JoinSequence {
  @available(*, unavailable, renamed="iterator")
  public func generate() -> JoinIterator<Base.Iterator> {
    fatalError("unavailable function can't be called")
  }
}

extension Sequence where Iterator.Element : Sequence {
  @available(*, unavailable, renamed="join")
  public func joinWithSeparator<
    Separator : Sequence
    where
    Separator.Iterator.Element == Iterator.Element.Iterator.Element
  >(separator: Separator) -> JoinSequence<Self> {
    fatalError("unavailable function can't be called")
  }
}

