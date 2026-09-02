//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// A type that can provide a raw, native-width representation of itself for
/// interpolation into an `UncheckedString`.
///
/// Unlike `String`'s `CustomStringConvertible`, which produces Unicode text,
/// a `CustomUncheckedStringConvertible` type produces raw `Element` data
/// directly -- no implicit encoding, transcoding, or textual description is
/// involved. `UncheckedString` interpolation only accepts values that
/// conform to this protocol (rather than any `CustomStringConvertible`
/// value, the way `String` interpolation does), since nothing about
/// `UncheckedString` implies a text encoding that an arbitrary describable
/// value could be rendered through.
///
/// `UncheckedStringProtocol` (and therefore `UncheckedString` and
/// `UncheckedSubString`) conforms to this protocol automatically, producing
/// its own character data. Any other type can opt in by declaring the
/// conformance and implementing `withUncheckedStringRepresentation`.
@available(SwiftStdlib 9999, *)
public protocol CustomUncheckedStringConvertible {
  /// The element type of the raw representation this type provides.
  associatedtype UncheckedStringElement: FixedWidthInteger

  /// Calls the given closure with a buffer containing this value's raw
  /// `UncheckedStringElement` representation.
  func withUncheckedStringRepresentation<R, Failure>(
    _ body: (Span<UncheckedStringElement>) throws(Failure) -> R
  ) throws(Failure) -> R
}

/// A type that can represent a string as a collection of characters.
///
/// Unlike `StringProtocol`, no assumptions are made about the encoding or
/// type of the characters.
@available(SwiftStdlib 9999, *)
public protocol UncheckedStringProtocol
  : BidirectionalCollection, Equatable, Hashable, Comparable,
    CustomDebugStringConvertible, CustomUncheckedStringConvertible
  where Iterator.Element: FixedWidthInteger,
    Index == Int,
    SubSequence: UncheckedStringProtocol,
    UncheckedStringElement == Element
{
  typealias SubSequence = UncheckedSubString<Element>

  /// Calls the given closure with a buffer of `Element`s,
  /// which are *not* necessarily NUL-terminated.
  func withCharacterData<R, E>(
    _ body: (Span<Element>) throws(E) -> R
  ) throws(E) -> R
}

@available(SwiftStdlib 9999, *)
extension UncheckedStringProtocol {
  public func withUncheckedStringRepresentation<R, Failure>(
    _ body: (Span<Element>) throws(Failure) -> R
  ) throws(Failure) -> R {
    try withCharacterData(body)
  }
}

@available(SwiftStdlib 9999, *)
extension UncheckedStringProtocol {
  public static func == (lhs: Self, rhs: Self) -> Bool {
    if lhs.count != rhs.count {
      return false
    }

    return lhs.withCharacterData { lhsData in
      rhs.withCharacterData { rhsData in
        return lhsData._elementsEqual(to: rhsData)
      }
    }
  }
}

@available(SwiftStdlib 9999, *)
extension UncheckedStringProtocol {
  public func hash(into hasher: inout Hasher) {
    hasher.combine(count)
    withCharacterData {
      $0._hashContents(into: &hasher)
    }
  }
}

@available(SwiftStdlib 9999, *)
extension UncheckedStringProtocol {
  public static func < (lhs: Self, rhs: Self) -> Bool {
    return lhs.withCharacterData { lhsData in
      rhs.withCharacterData { rhsData in
        let minCount = Swift.min(lhsData.count, rhsData.count)
        var i = 0
        while i < minCount {
          let l = lhsData[i]
          let r = rhsData[i]
          if l != r {
            return l < r
          }
          i += 1
        }
        return lhsData.count < rhsData.count
      }
    }
  }
}

@available(SwiftStdlib 9999, *)
extension UncheckedStringProtocol {
  public var debugDescription: String {
    return withCharacterData { data in
      var result = "\""
      var i = 0
      while i < data.count {
        let ch = data[i]
        if ch == 92 {
          result += "\\\\"
        } else if ch == 34 {
          result += "\\\""
        } else if ch >= 32 && ch <= 127 {
          result += String(Unicode.Scalar(UInt32(ch))!)
        } else {
          result += "\\x{"
          result += String(ch, radix: 16)
          result += "}"
        }
        i += 1
      }
      result += "\""
      return result
    }
  }
}

@available(SwiftStdlib 9999, *)
extension UncheckedStringProtocol where Iterator == IndexingIterator<Self> {
  /// Bulk-copies this string's elements into `buffer`.
  ///
  /// Without this override, `Sequence`'s default implementation copies one
  /// element at a time via `makeIterator()`/`subscript(_:)` -- for `.small`
  /// storage, each such access re-unpacks the *entire* packed byte tuple
  /// just to extract one element. This is the hook that
  /// `Array(_:)`/`_copyCollectionToContiguousArray` actually calls.
  ///
  /// `UncheckedString`/`UncheckedSubString` additionally provide their own
  /// concrete override of this and of `withContiguousStorageIfAvailable`
  /// (in `UncheckedString.swift`) rather than relying solely on this
  /// protocol-extension default -- verified by testing that both are
  /// actually reached through `Array(_:)` for every storage case.
  @inlinable
  public __consuming func _copyContents(
    initializing buffer: UnsafeMutableBufferPointer<Element>
  ) -> (IndexingIterator<Self>, UnsafeMutableBufferPointer<Element>.Index) {
    return unsafe _uncheckedStringCopyContents(self, initializing: buffer)
  }
}

/// Provides bulk access to `str`'s contents as contiguous storage.
///
/// Shared implementation called from both `UncheckedString`'s and
/// `UncheckedSubString`'s own concrete `withContiguousStorageIfAvailable`
/// overrides, so the two types don't duplicate this body.
@available(SwiftStdlib 9999, *)
@usableFromInline
@inline(__always)
internal func _uncheckedStringWithContiguousStorage<S: UncheckedStringProtocol, R>(
  _ str: S,
  _ body: (UnsafeBufferPointer<S.Element>) throws -> R
) rethrows -> R? {
  return try str.withCharacterData { data in
    try data.withUnsafeBufferPointer(body)
  }
}

/// Bulk-copies `str`'s elements into `buffer`.
///
/// Shared implementation called from both `UncheckedString`'s and
/// `UncheckedSubString`'s own concrete `_copyContents` overrides, so the
/// two types don't duplicate this body.
@available(SwiftStdlib 9999, *)
@usableFromInline
internal func _uncheckedStringCopyContents<S: UncheckedStringProtocol>(
  _ str: S,
  initializing buffer: UnsafeMutableBufferPointer<S.Element>
) -> (IndexingIterator<S>, UnsafeMutableBufferPointer<S.Element>.Index) {
  let copied = str.withCharacterData { data in
    data.withUnsafeBufferPointer { source -> Int in
      let n = Swift.min(buffer.count, source.count)
      if n > 0 {
        unsafe buffer.baseAddress!.initialize(from: source.baseAddress!, count: n)
      }
      return n
    }
  }
  let newPosition = str.index(str.startIndex, offsetBy: copied)
  return (
    IndexingIterator(_elements: str, _position: newPosition),
    unsafe buffer.index(buffer.startIndex, offsetBy: copied)
  )
}

@available(SwiftStdlib 9999, *)
extension UncheckedStringProtocol {
  /// Returns a Boolean value indicating whether this string begins with the
  /// specified prefix.
  public func hasPrefix<Other: UncheckedStringProtocol>(
    _ prefix: Other
  ) -> Bool where Other.Element == Element {
    guard prefix.count <= count else { return false }
    return withCharacterData { selfData in
      prefix.withCharacterData { prefixData in
        selfData.extracting(first: prefixData.count)._elementsEqual(to: prefixData)
      }
    }
  }

  /// Returns a Boolean value indicating whether this string ends with the
  /// specified suffix.
  public func hasSuffix<Other: UncheckedStringProtocol>(
    _ suffix: Other
  ) -> Bool where Other.Element == Element {
    guard suffix.count <= count else { return false }
    return withCharacterData { selfData in
      suffix.withCharacterData { suffixData in
        selfData.extracting(last: suffixData.count)._elementsEqual(to: suffixData)
      }
    }
  }
}

