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
  @_spi(_Unicode)
  @available(SwiftStdlib 5.7, *)
  public struct _WordView {
    internal var _guts: _StringGuts

    internal init(_ _guts: _StringGuts) {
      self._guts = _guts
    }
  }
}

@available(SwiftStdlib 5.7, *)
extension String._WordView: Collection {
  @_spi(_Unicode)
  @available(SwiftStdlib 5.7, *)
  public typealias Index = String.Index

  @_spi(_Unicode)
  @available(SwiftStdlib 5.7, *)
  public typealias Element = Substring

  @_spi(_Unicode)
  @available(SwiftStdlib 5.7, *)
  public var startIndex: Index {
    _guts.startIndex
  }

  @_spi(_Unicode)
  @available(SwiftStdlib 5.7, *)
  public var endIndex: Index {
    _guts.endIndex
  }

  @_spi(_Unicode)
  @available(SwiftStdlib 5.7, *)
  public func index(after i: Index) -> Index {
    let i = _guts.validateWordIndex(i)
    return _uncheckedIndex(after: i)
  }

  internal func _uncheckedIndex(after i: Index) -> Index {
    _internalInvariant(_guts.hasMatchingEncoding(i))
    _internalInvariant(i < endIndex)
    _internalInvariant(i._isScalarAligned)

    if _slowPath(_guts.isForeign) {
      return _foreignIndex(after: i)
    }

    let nextOffset = _guts.withFastUTF8 { utf8 in
      nextBoundary(startingAt: i._encodedOffset) {
        _internalInvariant($0 >= 0)

        guard $0 < utf8.count else {
          return nil
        }

        let (scalar, len) = _decodeScalar(utf8, startingAt: $0)
        return (scalar, $0 &+ len)
      }
    }

    let nextIndex = String.Index(_encodedOffset: nextOffset)._scalarAligned
    return _guts.markEncoding(nextIndex)
  }

  @inline(never)
  internal func _foreignIndex(after i: Index) -> Index {
#if _runtime(_ObjC)
    let nextOffset = nextBoundary(startingAt: i._encodedOffset) {
      _internalInvariant($0 >= 0)

      guard $0 < _guts.count else {
        return nil
      }

      let scalars = String.UnicodeScalarView(_guts)
      let idx = String.Index(_encodedOffset: $0)

      let scalar = scalars[idx]
      let nextIndex = scalars.index(after: idx)

      return (scalar, nextIndex._encodedOffset)
    }

    let nextIndex = String.Index(_encodedOffset: nextOffset)._scalarAligned
    return _guts.markEncoding(nextIndex)
#else
    fatalError("No foreign strings on this platform in this version of Swift.")
#endif
  }

  @_spi(_Unicode)
  @available(SwiftStdlib 5.7, *)
  public subscript(position: Index) -> Element {
    let position = _guts.validateWordIndex(position)
    let indexAfter = _uncheckedIndex(after: position)

    return String(_guts)[position ..< indexAfter]
  }
}

@available(SwiftStdlib 5.7, *)
extension String._WordView: BidirectionalCollection {
  @_spi(_Unicode)
  @available(SwiftStdlib 5.7, *)
  public func index(before i: Index) -> Index {
    let i = _guts.validateInclusiveWordIndex(i)
    return _uncheckedIndex(before: i)
  }

  internal func _uncheckedIndex(before i: Index) -> Index {
    _internalInvariant(_guts.hasMatchingEncoding(i))
    _internalInvariant(i > startIndex)
    _internalInvariant(i._isScalarAligned)

    if _slowPath(_guts.isForeign) {
      return _foreignIndex(before: i)
    }

    let previousOffset = _guts.withFastUTF8 { utf8 in
      previousBoundary(endingAt: i._encodedOffset) {
        _internalInvariant($0 <= _guts.count)

        guard $0 > 0 else {
          return nil
        }

        let (scalar, len) = _decodeScalar(utf8, endingAt: $0)
        return (scalar, $0 &- len)
      }
    }

    let previousIndex = String.Index(
      _encodedOffset: previousOffset
    )._scalarAligned
    return _guts.markEncoding(previousIndex)
  }

  @inline(never)
  internal func _foreignIndex(before i: Index) -> Index {
#if _runtime(_ObjC)
    let previousOffset = previousBoundary(endingAt: i._encodedOffset) {
      _internalInvariant($0 <= _guts.count)

      guard $0 > 0 else {
        return nil
      }

      let scalars = String.UnicodeScalarView(_guts)
      let idx = String.Index(_encodedOffset: $0)

      let previousIndex = scalars.index(before: idx)
      let scalar = scalars[previousIndex]

      return (scalar, previousIndex._encodedOffset)
    }

    let previousIndex = String.Index(
      _encodedOffset: previousOffset
    )._scalarAligned
    return _guts.markEncoding(previousIndex)
#else
    fatalError("No foreign strings on this platform in this version of Swift.")
#endif
  }
}

extension String {
  @_spi(_Unicode)
  @available(SwiftStdlib 5.7, *)
  public var _words: _WordView {
    _WordView(_guts)
  }
}

extension String {
  @_spi(_Unicode)
  @available(SwiftStdlib 5.7, *)
  public func _isOnWordBoundary(_ i: String.Index) -> Bool {
    guard i != startIndex, i != endIndex else {
      return true
    }

    let after = _words.index(after: i)
    let before = _words.index(before: after)

    return i == before
  }
}
