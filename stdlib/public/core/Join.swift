//===--- Join.swift - Protocol and Algorithm for concatenation ------------===//
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

internal enum _JoinIteratorState {
  case Start
  case GeneratingElements
  case GeneratingSeparator
  case End
}

/// An iterator that presents the elements of the sequences traversed
/// by `Base`, concatenated using a given separator.
public struct JoinIterator<
  Base : IteratorProtocol where Base.Element : Sequence
> : IteratorProtocol {

  /// Creates an iterator that presents the elements of the sequences
  /// traversed by `base`, concatenated using `separator`.
  ///
  /// - Complexity: O(`separator.length`).
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
      case .Start:
        if let nextSubSequence = _base.next() {
          _inner = nextSubSequence.iterator()
          _state = .GeneratingElements
        } else {
          _state = .End
          return nil
        }

      case .GeneratingElements:
        let result = _inner!.next()
        if _fastPath(result != nil) {
          return result
        }
        if _separatorData.isEmpty {
          _inner = _base.next()?.iterator()
          if _inner == nil {
            _state = .End
            return nil
          }
        } else {
          _inner = _base.next()?.iterator()
          if _inner == nil {
            _state = .End
            return nil
          }
          _separator = _separatorData.iterator()
          _state = .GeneratingSeparator
        }

      case .GeneratingSeparator:
        let result = _separator!.next()
        if _fastPath(result != nil) {
          return result
        }
        _state = .GeneratingElements

      case .End:
        return nil

      }
    }
    while true
  }

  internal var _base: Base
  internal var _inner: Base.Element.Iterator? = nil
  internal var _separatorData: ContiguousArray<Base.Element.Iterator.Element>
  internal var _separator:
    ContiguousArray<Base.Element.Iterator.Element>.Iterator?
  internal var _state: _JoinIteratorState = .Start
}

/// A sequence that presents the elements of the `Base` sequences
/// concatenated using a given separator.
public struct JoinSequence<
  Base : Sequence where Base.Iterator.Element : Sequence
> : Sequence {

  /// Creates a sequence that presents the elements of `base` sequences
  /// concatenated using `separator`.
  ///
  /// - Complexity: O(`separator.length`).
  public init<
    Separator : Sequence
    where
    Separator.Iterator.Element == Base.Iterator.Element.Iterator.Element
  >(base: Base, separator: Separator) {
    self._base = base
    self._separator = ContiguousArray(separator)
  }

  /// Return an *iterator* over the elements of this *sequence*.
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
    let separatorSize: Int = numericCast(_separator.length)

    let reservation = _base._preprocessingPass {
      (s: Base) -> Int in
      var r = 0
      for chunk in s {
        r += separatorSize + chunk.underestimatedLength()
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
  /// `[[1, 2, 3], [4, 5, 6], [7, 8, 9]].joinWithSeparator([-1, -2])`
  /// yields `[1, 2, 3, -1, -2, 4, 5, 6, -1, -2, 7, 8, 9]`.
  @warn_unused_result
  public func joinWithSeparator<
    Separator : Sequence
    where
    Separator.Iterator.Element == Iterator.Element.Iterator.Element
  >(separator: Separator) -> JoinSequence<Self> {
    return JoinSequence(base: self, separator: separator)
  }
}

