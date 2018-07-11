//===--- StringIndex.swift ------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
extension String {
  /// A position of a character or code unit in a string.
  @_fixed_layout // FIXME(sil-serialize-all)
  public struct Index {
    @usableFromInline
    internal typealias _UTF8Buffer = UTF8.EncodedScalar

    @usableFromInline // FIXME(sil-serialize-all)
    internal var _compoundOffset: UInt64

    @usableFromInline
    internal var _utf8Buffer = _UTF8Buffer()

    @usableFromInline
    internal var _graphemeStrideCache: UInt16 = 0
  }
}

/// Convenience accessors
extension String.Index {
  @inlinable // FIXME(sil-serialize-all)
  internal var utf8Buffer: String.Index._UTF8Buffer? {
    guard !_utf8Buffer.isEmpty else { return nil }
    return _utf8Buffer
  }

  @inlinable // FIXME(sil-serialize-all)
  internal var characterStride: Int? {
    guard _graphemeStrideCache > 0 else { return nil }
    return Int(truncatingIfNeeded: _graphemeStrideCache)
  }

  // TODO: Probably worth carving a bit for, or maybe a isSubScalar bit...
  @inlinable // FIXME(sil-serialize-all)
  internal var isUTF8: Bool {
    return self.utf8Buffer != nil || self.transcodedOffset > 0
  }
}

extension String.Index : Equatable {
  // A combined code unit and transcoded offset, for comparison purposes
  @inlinable // FIXME(sil-serialize-all)
  internal var _orderingValue: UInt64 {
    return _compoundOffset
  }

  @inlinable // FIXME(sil-serialize-all)
  public static func == (lhs: String.Index, rhs: String.Index) -> Bool {
    return lhs._orderingValue == rhs._orderingValue
  }
}

extension String.Index : Comparable {
  @inlinable // FIXME(sil-serialize-all)
  public static func < (lhs: String.Index, rhs: String.Index) -> Bool {
    return lhs._orderingValue < rhs._orderingValue
  }
}

extension String.Index : Hashable {
  /// Hashes the essential components of this value by feeding them into the
  /// given hasher.
  ///
  /// - Parameter hasher: The hasher to use when combining the components
  ///   of this instance.
  @inlinable
  public func hash(into hasher: inout Hasher) {
    hasher.combine(_orderingValue)
  }
}

extension String.Index {
  @inline(__always)
  @inlinable
  internal init(encodedOffset: Int, transcodedOffset: Int) {
    let cuOffset = UInt64(truncatingIfNeeded: encodedOffset)
    _sanityCheck(
      cuOffset & 0xFFFF_0000_0000_0000 == 0, "String length capped at 48bits")
    let transOffset = UInt64(truncatingIfNeeded: transcodedOffset)
    _sanityCheck(transOffset <= 4, "UTF-8 max transcoding is 4 code units")

    self._compoundOffset = cuOffset &<< 2 | transOffset
  }

  @inline(__always)
  @inlinable
  internal init(from other: String.Index, adjustingEncodedOffsetBy adj: Int) {
    self.init(
      encodedOffset: other.encodedOffset &+ adj,
      transcodedOffset: other.transcodedOffset)
    self._utf8Buffer = other._utf8Buffer
    self._graphemeStrideCache = other._graphemeStrideCache
  }

  /// Creates a new index at the specified UTF-16 offset.
  ///
  /// - Parameter offset: An offset in UTF-16 code units.
  @inlinable // FIXME(sil-serialize-all)
  public init(encodedOffset offset: Int) {
    self.init(encodedOffset: offset, transcodedOffset: 0)
  }

  @inlinable // FIXME(sil-serialize-all)
  internal init(
    encodedOffset offset: Int, transcodedOffset: Int, buffer: _UTF8Buffer
  ) {
    self.init(encodedOffset: offset, transcodedOffset: transcodedOffset)
    self._utf8Buffer = buffer
  }

  @inlinable
  internal init(encodedOffset: Int, characterStride: Int) {
    self.init(encodedOffset: encodedOffset, transcodedOffset: 0)
    if characterStride < UInt16.max {
      self._graphemeStrideCache = UInt16(truncatingIfNeeded: characterStride)
    }
  }

  /// The offset into a string's UTF-16 encoding for this index.
  @inlinable // FIXME(sil-serialize-all)
  public var encodedOffset : Int {
    return Int(truncatingIfNeeded: _compoundOffset &>> 2)
  }

  /// The offset of this index within whatever encoding this is being viewed as
  @inlinable // FIXME(sil-serialize-all)
  internal var transcodedOffset: Int {
    return Int(truncatingIfNeeded: _compoundOffset & 0x3)
  }
}
