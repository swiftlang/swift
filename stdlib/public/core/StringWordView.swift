//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
//
//===----------------------------------------------------------------------===//

extension String {
  internal struct _WordView {
    internal var _guts: _StringGuts

    internal init(_ _guts: _StringGuts) {
      self._guts = _guts
    }
  }
}

extension String._WordView: Collection {
  internal struct Index: Hashable {
    internal var _encodedOffset: Int
  }

  typealias Element = Substring

  var startIndex: Index {
    Index(_encodedOffset: _guts.startIndex._encodedOffset)
  }

  var endIndex: Index {
    Index(_encodedOffset: _guts.endIndex._encodedOffset)
  }

  func index(after i: Index) -> Index {
    _precondition(i < endIndex)

    let nextOffset = _guts.nextWordIndex(startingAt: i._encodedOffset)
    return Index(_encodedOffset: nextOffset)
  }

  subscript(position: Index) -> Element {
    let indexAfter = index(after: position)

    let l = String.Index(
      _encodedOffset: position._encodedOffset
    )._characterAligned
    let u = String.Index(
      _encodedOffset: indexAfter._encodedOffset
    )._characterAligned

    return String(_guts)[l ..< u]
  }
}

extension String._WordView.Index: Comparable {
  @inline(__always)
  static func < (lhs: Self, rhs: Self) -> Bool {
    lhs._encodedOffset < rhs._encodedOffset
  }
}

extension String._WordView.Index {
  internal init(_ i: String.Index) {
    _encodedOffset = i._encodedOffset
  }
}

extension String._WordView.Index: Sendable {}

extension String._WordView: BidirectionalCollection {
  func index(before i: Index) -> Index {
    _precondition(i > startIndex)
    _precondition(i <= endIndex)

    let previousOffset = _guts.previousWordIndex(endingAt: i._encodedOffset)

    return Index(_encodedOffset: previousOffset)
  }
}

extension String._WordView: Sendable {}

extension String {
  // FIXME: Figure out what to do with this if/when we actually release it as
  // public API.
  // Should this be:
  //    var words: WordView
  // or perhaps
  //    func words(_ level: ...) -> some BidirectionalCollection<Substring>
  //
  // This is only used for testing right now.
  @_spi(_Unicode)
  @available(SwiftStdlib 5.7, *)
  public func _words() -> [Substring] {
    Array(_WordView(_guts))
  }

  @_spi(_Unicode)
  @available(SwiftStdlib 5.7, *)
  public func _isOnWordBoundary(_ i: String.Index) -> Bool {
    guard i._encodedOffset != startIndex._encodedOffset,
          i._encodedOffset != endIndex._encodedOffset else {
      return true
    }

    let i = String._WordView.Index(i)
    let nearest = _guts.roundDownToNearestWord(i)
    return i == nearest
  }
}
