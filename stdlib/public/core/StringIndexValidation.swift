//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// Index validation
extension _StringGuts {
  @_alwaysEmitIntoClient @inline(__always)
  internal func isFastScalarIndex(_ i: String.Index) -> Bool {
    hasMatchingEncoding(i) && i._isScalarAligned
  }

  @_alwaysEmitIntoClient @inline(__always)
  internal func isFastCharacterIndex(_ i: String.Index) -> Bool {
    hasMatchingEncoding(i) && i._isCharacterAligned
  }
}

// Subscalar index validation (UTF-8 & UTF-16 views)
extension _StringGuts {
  @_alwaysEmitIntoClient
  internal func validateSubscalarIndex(_ i: String.Index) -> String.Index {
    let i = ensureMatchingEncoding(i)
    _precondition(i._encodedOffset < count, "String index is out of bounds")
    return i
  }

  @_alwaysEmitIntoClient
  internal func validateSubscalarIndex(
    _ i: String.Index,
    in bounds: Range<String.Index>
  ) -> String.Index {
    _internalInvariant(bounds.upperBound <= endIndex)

    let i = ensureMatchingEncoding(i)
    _precondition(i >= bounds.lowerBound && i < bounds.upperBound,
      "Substring index is out of bounds")
    return i
  }

  @_alwaysEmitIntoClient
  internal func validateInclusiveSubscalarIndex(
    _ i: String.Index
  ) -> String.Index {
    let i = ensureMatchingEncoding(i)
    _precondition(i._encodedOffset <= count, "String index is out of bounds")
    return i
  }

  internal func validateInclusiveSubscalarIndex(
    _ i: String.Index,
    in bounds: Range<String.Index>
  ) -> String.Index {
    _internalInvariant(bounds.upperBound <= endIndex)

    let i = ensureMatchingEncoding(i)
    _precondition(i >= bounds.lowerBound && i <= bounds.upperBound,
      "Substring index is out of bounds")
    return i
  }

  @_alwaysEmitIntoClient
  internal func validateSubscalarRange(
    _ range: Range<String.Index>
  ) -> Range<String.Index> {
    let upper = ensureMatchingEncoding(range.upperBound)
    let lower = ensureMatchingEncoding(range.lowerBound)

    // Note: if only `lower` was miscoded, then the range invariant `lower <=
    // upper` may no longer hold after the above conversions, so we need to
    // re-check it here.
    _precondition(upper <= endIndex && lower <= upper,
      "String index range is out of bounds")

    return Range(_uncheckedBounds: (lower, upper))
  }

  @_alwaysEmitIntoClient
  internal func validateSubscalarRange(
    _ range: Range<String.Index>,
    in bounds: Range<String.Index>
  ) -> Range<String.Index> {
    _internalInvariant(bounds.upperBound <= endIndex)

    let upper = ensureMatchingEncoding(range.upperBound)
    let lower = ensureMatchingEncoding(range.lowerBound)

    // Note: if only `lower` was miscoded, then the range invariant `lower <=
    // upper` may no longer hold after the above conversions, so we need to
    // re-check it here.
    _precondition(
      lower >= bounds.lowerBound
      && lower <= upper
      && upper <= bounds.upperBound,
      "Substring index range is out of bounds")

    return Range(_uncheckedBounds: (lower, upper))
  }
}

// Scalar index validation (Unicode scalar views)
extension _StringGuts {
  /// Validate `i` and adjust its position toward the start, returning the
  /// resulting index or trapping as appropriate. If this function returns, then
  /// the returned value
  ///
  /// - has an encoding that matches this string,
  /// - is within the bounds of this string, and
  /// - is aligned on a scalar boundary.
  @_alwaysEmitIntoClient
  internal func validateScalarIndex(_ i: String.Index) -> String.Index {
    if isFastScalarIndex(i) {
      _precondition(i._encodedOffset < count, "String index is out of bounds")
      return i
    }

    return scalarAlign(validateSubscalarIndex(i))
  }

  /// Validate `i` and adjust its position toward the start, returning the
  /// resulting index or trapping as appropriate. If this function returns, then
  /// the returned value
  ///
  /// - has an encoding that matches this string,
  /// - is within `start ..< end`, and
  /// - is aligned on a scalar boundary.
  @_alwaysEmitIntoClient
  internal func validateScalarIndex(
    _ i: String.Index,
    in bounds: Range<String.Index>
  ) -> String.Index {
    _internalInvariant(bounds.upperBound <= endIndex)

    if isFastScalarIndex(i) {
      _precondition(i >= bounds.lowerBound && i < bounds.upperBound,
        "Substring index is out of bounds")
      return i
    }

    return scalarAlign(validateSubscalarIndex(i, in: bounds))
  }
}

extension _StringGuts {
  /// Validate `i` and adjust its position toward the start, returning the
  /// resulting index or trapping as appropriate. If this function returns, then
  /// the returned value
  ///
  /// - has an encoding that matches this string,
  /// - is within the bounds of this string (including the `endIndex`), and
  /// - is aligned on a scalar boundary.
  @_alwaysEmitIntoClient
  internal func validateInclusiveScalarIndex(
    _ i: String.Index
  ) -> String.Index {
    if isFastScalarIndex(i) {
      _precondition(i._encodedOffset <= count, "String index is out of bounds")
      return i
    }

    return scalarAlign(validateInclusiveSubscalarIndex(i))
  }

  /// Validate `i` and adjust its position toward the start, returning the
  /// resulting index or trapping as appropriate. If this function returns, then
  /// the returned value
  ///
  /// - has an encoding that matches this string,
  /// - is within the bounds of this string (including the `endIndex`), and
  /// - is aligned on a scalar boundary.
  internal func validateInclusiveScalarIndex(
    _ i: String.Index,
    in bounds: Range<String.Index>
  ) -> String.Index {
    _internalInvariant(bounds.upperBound <= endIndex)

    if isFastScalarIndex(i) {
      _precondition(i >= bounds.lowerBound && i <= bounds.upperBound,
        "Substring index is out of bounds")
      return i
    }

    return scalarAlign(validateInclusiveSubscalarIndex(i, in: bounds))
  }
}

extension _StringGuts {
  /// Validate `range` and adjust the position of its bounds, returning the
  /// resulting range or trapping as appropriate. If this function returns, then
  /// the bounds of the returned value
  ///
  /// - have an encoding that matches this string,
  /// - are within the bounds of this string, and
  /// - are aligned on a scalar boundary.
  internal func validateScalarRange(
    _ range: Range<String.Index>
  ) -> Range<String.Index> {
    if
      isFastScalarIndex(range.lowerBound), isFastScalarIndex(range.upperBound)
    {
      _precondition(range.upperBound._encodedOffset <= count,
        "String index range is out of bounds")
      return range
    }

    let r = validateSubscalarRange(range)
    return Range(
      _uncheckedBounds: (scalarAlign(r.lowerBound), scalarAlign(r.upperBound)))
  }

  /// Validate `range` and adjust the position of its bounds, returning the
  /// resulting range or trapping as appropriate. If this function returns, then
  /// the bounds of the returned value
  ///
  /// - have an encoding that matches this string,
  /// - are within `start ..< end`, and
  /// - are aligned on a scalar boundary.
  internal func validateScalarRange(
    _ range: Range<String.Index>,
    in bounds: Range<String.Index>
  ) -> Range<String.Index> {
    _internalInvariant(bounds.upperBound <= endIndex)

    if
      isFastScalarIndex(range.lowerBound), isFastScalarIndex(range.upperBound)
    {
      _precondition(
        range.lowerBound >= bounds.lowerBound
        && range.upperBound <= bounds.upperBound,
        "String index range is out of bounds")
      return range
    }

    let r = validateSubscalarRange(range, in: bounds)
    let upper = scalarAlign(r.upperBound)
    let lower = scalarAlign(r.lowerBound)
    return Range(_uncheckedBounds: (lower, upper))
  }
}

// Character index validation (String & Substring)
extension _StringGuts {
  internal func validateCharacterIndex(_ i: String.Index) -> String.Index {
    if isFastCharacterIndex(i) {
      _precondition(i._encodedOffset < count, "String index is out of bounds")
      return i
    }
    return roundDownToNearestCharacter(scalarAlign(validateSubscalarIndex(i)))
  }

  internal func validateCharacterIndex(
    _ i: String.Index,
    in bounds: Range<String.Index>
  ) -> String.Index {
    _internalInvariant(bounds.upperBound <= endIndex)

    if isFastCharacterIndex(i) {
      _precondition(i >= bounds.lowerBound && i < bounds.upperBound,
        "Substring index is out of bounds")
      return i
    }

    return roundDownToNearestCharacter(
      scalarAlign(validateSubscalarIndex(i, in: bounds)),
      in: bounds)
  }

  internal func validateInclusiveCharacterIndex(
    _ i: String.Index
  ) -> String.Index {
    if isFastCharacterIndex(i) {
      _precondition(i._encodedOffset <= count, "String index is out of bounds")
      return i
    }

    return roundDownToNearestCharacter(
      scalarAlign(validateInclusiveSubscalarIndex(i)))
  }

  internal func validateInclusiveCharacterIndex(
    _ i: String.Index,
    in bounds: Range<String.Index>
  ) -> String.Index {
    _internalInvariant(bounds.upperBound <= endIndex)

    if isFastCharacterIndex(i) {
      _precondition(i >= bounds.lowerBound && i <= bounds.upperBound,
        "Substring index is out of bounds")
      return i
    }

    return roundDownToNearestCharacter(
      scalarAlign(validateInclusiveSubscalarIndex(i, in: bounds)),
      in: bounds)
  }

  internal func validateCharacterRange(
    _ range: Range<String.Index>
  ) -> Range<String.Index> {
    if
      isFastCharacterIndex(range.lowerBound),
      isFastCharacterIndex(range.upperBound)
    {
      _precondition(range.upperBound._encodedOffset <= count,
        "String index range is out of bounds")
      return range
    }

    let r = validateSubscalarRange(range)
    let l = roundDownToNearestCharacter(scalarAlign(r.lowerBound))
    let u = roundDownToNearestCharacter(scalarAlign(r.upperBound))
    return Range(_uncheckedBounds: (l, u))
  }

  internal func validateCharacterRange(
    _ range: Range<String.Index>,
    in bounds: Range<String.Index>
  ) -> Range<String.Index> {
    _internalInvariant(bounds.upperBound <= endIndex)

    if
      isFastCharacterIndex(range.lowerBound),
      isFastCharacterIndex(range.upperBound)
    {
      _precondition(
        range.lowerBound >= bounds.lowerBound
        && range.upperBound <= bounds.upperBound,
        "String index range is out of bounds")
      return range
    }

    let r = validateSubscalarRange(range, in: bounds)
    let l = roundDownToNearestCharacter(scalarAlign(r.lowerBound), in: bounds)
    let u = roundDownToNearestCharacter(scalarAlign(r.upperBound), in: bounds)
    return Range(_uncheckedBounds: (l, u))
  }
}
