//===----------------------------------------------------------------------===//
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

/// A single extended grapheme cluster that approximates a user-perceived
/// character.
///
/// The `Character` type represents a character made up of one or more Unicode
/// scalar values, grouped by a Unicode boundary algorithm. Generally, a
/// `Character` instance matches what the reader of a string will perceive as
/// a single character. Strings are collections of `Character` instances, so
/// the number of visible characters is generally the most natural way to
/// count the length of a string.
///
///     let greeting = "Hello! 🐥"
///     print("Length: \(greeting.count)")
///     // Prints "Length: 8"
///
/// Because each character in a string can be made up of one or more Unicode
/// scalar values, the number of characters in a string may not match the
/// length of the Unicode scalar value representation or the length of the
/// string in a particular binary representation.
///
///     print("Unicode scalar value count: \(greeting.unicodeScalars.count)")
///     // Prints "Unicode scalar value count: 15"
///
///     print("UTF-8 representation count: \(greeting.utf8.count)")
///     // Prints "UTF-8 representation count: 18"
///
/// Every `Character` instance is composed of one or more Unicode scalar values
/// that are grouped together as an *extended grapheme cluster*. The way these
/// scalar values are grouped is defined by a canonical, localized, or
/// otherwise tailored Unicode segmentation algorithm.
///
/// For example, a country's Unicode flag character is made up of two regional
/// indicator scalar values that correspond to that country's ISO 3166-1
/// alpha-2 code. The alpha-2 code for The United States is "US", so its flag
/// character is made up of the Unicode scalar values `"\u{1F1FA}"` (REGIONAL
/// INDICATOR SYMBOL LETTER U) and `"\u{1F1F8}"` (REGIONAL INDICATOR SYMBOL
/// LETTER S). When placed next to each other in a string literal, these two
/// scalar values are combined into a single grapheme cluster, represented by
/// a `Character` instance in Swift.
///
///     let usFlag: Character = "\u{1F1FA}\u{1F1F8}"
///     print(usFlag)
///     // Prints "🇺🇸"
///
/// For more information about the Unicode terms used in this discussion, see
/// the [Unicode.org glossary][glossary]. In particular, this discussion
/// mentions [extended grapheme clusters][clusters] and [Unicode scalar
/// values][scalars].
///
/// [glossary]: http://www.unicode.org/glossary/
/// [clusters]: http://www.unicode.org/glossary/#extended_grapheme_cluster
/// [scalars]: http://www.unicode.org/glossary/#unicode_scalar_value
@_fixed_layout
public struct Character {
  // Fundamentally, it is just a String, but it is optimized for the common case
  // where the UTF-16 representation fits in 63 bits.  The remaining bit is used
  // to discriminate between small and large representations.  Since a grapheme
  // cluster cannot have U+0000 anywhere but in its first scalar, we can store
  // zero in empty code units above the first one.
  @_frozen // FIXME(sil-serialize-all)
  @usableFromInline
  internal enum Representation {
    case smallUTF16(Builtin.Int63)
    case large(_UTF16StringStorage)
  }

  @usableFromInline
  internal var _representation: Representation

  // FIXME(sil-serialize-all): Should be @inlinable
  // <rdar://problem/34557187>
  internal static func _smallValue(_ value: Builtin.Int63) -> UInt64 {
    return UInt64(Builtin.zext_Int63_Int64(value))
  }

  typealias UTF16View = String.UTF16View
  @inlinable // FIXME(sil-serialize-all)
  internal var utf16: UTF16View {
    return String(self).utf16
  }

  @inlinable // FIXME(sil-serialize-all)
  internal init(_smallRepresentation b: _SmallUTF16) {
    _sanityCheck(Int64(b._storage) >= 0)
    _representation = .smallUTF16(
      Builtin.trunc_Int64_Int63(b._storage._value))
  }

  @inlinable // FIXME(sil-serialize-all)
  internal init(_largeRepresentation storage: _UTF16StringStorage) {
    _representation = .large(storage)
  }

  /// Creates a Character from a String that is already known to require the
  /// large representation.
  ///
  /// - Note: `s` should contain only a single grapheme, but we can't require
  ///   that formally because of grapheme cluster literals and the shifting
  ///   sands of Unicode.  https://bugs.swift.org/browse/SR-4955
  @inlinable // FIXME(sil-serialize-all)
  internal init(_largeRepresentationString s: String) {
    let storage = s._guts._extractNativeStorage(of: UTF16.CodeUnit.self)
    self.init(_largeRepresentation: storage)
  }
}

extension Character
 : _ExpressibleByBuiltinUTF16ExtendedGraphemeClusterLiteral,
   ExpressibleByExtendedGraphemeClusterLiteral
{
  /// Creates a character containing the given Unicode scalar value.
  ///
  /// - Parameter content: The Unicode scalar value to convert into a character.
  @inlinable // FIXME(sil-serialize-all)
  public init(_ content: Unicode.Scalar) {
    let content16 = UTF16.encode(content)._unsafelyUnwrappedUnchecked
    _representation = .smallUTF16(
      Builtin.zext_Int32_Int63(content16._storage._value))
  }

  @inlinable // FIXME(sil-serialize-all)
  @effects(readonly)
  public init(_builtinUnicodeScalarLiteral value: Builtin.Int32) {
    self.init(Unicode.Scalar(_builtinUnicodeScalarLiteral: value))
  }

  // Inlining ensures that the whole constructor can be folded away to a single
  // integer constant in case of small character literals.
  @inlinable // FIXME(sil-serialize-all)
  @inline(__always)
  @effects(readonly)
  public init(
    _builtinExtendedGraphemeClusterLiteral start: Builtin.RawPointer,
    utf8CodeUnitCount: Builtin.Word,
    isASCII: Builtin.Int1
  ) {
    let utf8 = UnsafeBufferPointer(
      start: UnsafePointer<Unicode.UTF8.CodeUnit>(start),
      count: Int(utf8CodeUnitCount))

    if utf8.count == 1 {
      _representation = .smallUTF16(
        Builtin.zext_Int8_Int63(utf8.first._unsafelyUnwrappedUnchecked._value))
      return
    }

  FastPath:
    repeat {
      var shift = 0
      let maxShift = 64 - 16
      var bits: UInt64 = 0

      for s8 in Unicode._ParsingIterator(
        codeUnits: utf8.makeIterator(), parser: UTF8.ForwardParser()) {

        let s16
          = UTF16.transcode(s8, from: UTF8.self)._unsafelyUnwrappedUnchecked

        for u16 in s16 {
          guard _fastPath(shift <= maxShift) else { break FastPath }
          bits |= UInt64(u16) &<< shift
          shift += 16
        }
      }
      guard _fastPath(Int64(truncatingIfNeeded: bits) >= 0) else {
        break FastPath
      }
      _representation = .smallUTF16(Builtin.trunc_Int64_Int63(bits._value))
      return
    }
    while false

    // For anything that doesn't fit in 63 bits, build the large
    // representation.
    self = Character(_largeRepresentationString:
      String(
        _builtinExtendedGraphemeClusterLiteral: start,
        utf8CodeUnitCount: utf8CodeUnitCount,
        isASCII: isASCII))
  }

  // Inlining ensures that the whole constructor can be folded away to a single
  // integer constant in case of small character literals.
  @inlinable // FIXME(sil-serialize-all)
  @inline(__always)
  @effects(readonly)
  public init(
    _builtinExtendedGraphemeClusterLiteral start: Builtin.RawPointer,
    utf16CodeUnitCount: Builtin.Word
  ) {
    let utf16 = _UnmanagedString<UTF16.CodeUnit>(
      start: UnsafePointer(start),
      count: Int(utf16CodeUnitCount))

    switch utf16.count {
    case 1:
      _representation = .smallUTF16(Builtin.zext_Int16_Int63(utf16[0]._value))
    case 2:
      let bits = UInt32(utf16[0]) | UInt32(utf16[1]) &<< 16
      _representation = .smallUTF16(Builtin.zext_Int32_Int63(bits._value))
    case 3:
      let bits = UInt64(utf16[0])
        | UInt64(utf16[1]) &<< 16
        | UInt64(utf16[2]) &<< 32
      _representation = .smallUTF16(Builtin.trunc_Int64_Int63(bits._value))
    case 4 where utf16[3] < 0x8000:
      let bits = UInt64(utf16[0])
        | UInt64(utf16[1]) &<< 16
        | UInt64(utf16[2]) &<< 32
        | UInt64(utf16[3]) &<< 48
      _representation = .smallUTF16(Builtin.trunc_Int64_Int63(bits._value))
    default:
      // TODO(SSO): small check
      _representation = .large(
        _StringGuts(_large: utf16)._extractNativeStorage())
    }
  }

  /// Creates a character with the specified value.
  ///
  /// Do not call this initalizer directly. It is used by the compiler when
  /// you use a string literal to initialize a `Character` instance. For
  /// example:
  ///
  ///     let oBreve: Character = "o\u{306}"
  ///     print(oBreve)
  ///     // Prints "ŏ"
  ///
  /// The assignment to the `oBreve` constant calls this initializer behind the
  /// scenes.
  @inlinable // FIXME(sil-serialize-all)
  public init(extendedGraphemeClusterLiteral value: Character) {
    self = value
  }

  /// Creates a character from a single-character string.
  ///
  /// The following example creates a new character from the uppercase version
  /// of a string that only holds one character.
  ///
  ///     let a = "a"
  ///     let capitalA = Character(a.uppercased())
  ///
  /// - Parameter s: The single-character string to convert to a `Character`
  ///   instance. `s` must contain exactly one extended grapheme cluster.
  @inlinable // FIXME(sil-serialize-all)
  public init(_ s: String) {
    let count = s._guts.count
    _precondition(count != 0,
      "Can't form a Character from an empty String")
    _debugPrecondition(s.index(after: s.startIndex) == s.endIndex,
      "Can't form a Character from a String containing more than one extended grapheme cluster")

    self.init(_unverified: s._guts)
  }

  /// Construct a Character from a _StringGuts, assuming it consists of exactly
  /// one extended grapheme cluster.
  @inlinable // FIXME(sil-serialize-all)
  internal init(_unverified guts: _StringGuts) {
    self = _visitGuts(guts,
      ascii: { ascii in
        if _fastPath(ascii.count == 1) {
          return Character(_singleCodeUnit: ascii[0])
        }
        // The only multi-scalar ASCII grapheme cluster is CR/LF.
        _sanityCheck(ascii.count == 2)
        _sanityCheck(ascii.start[0] == _CR)
        _sanityCheck(ascii.start[1] == _LF)
        return Character(_codeUnitPair: UInt16(_CR), UInt16(_LF))
      },
      utf16: { utf16 in return Character(_unverified: utf16) },
      opaque: { opaque in return Character(_unverified: opaque) })
  }

  /// Construct a Character from a slice of a _StringGuts, assuming
  /// the specified range covers exactly one extended grapheme cluster.
  @inlinable // FIXME(sil-serialize-all)
  internal init(_unverified guts: _StringGuts, range: Range<Int>) {
    self = _visitGuts(
      guts, range: (range, performBoundsCheck: true),
      ascii: { ascii in
        if _fastPath(ascii.count == 1) {
          return Character(_singleCodeUnit: ascii[0])
        }
        // The only multi-scalar ASCII grapheme cluster is CR/LF.
        _sanityCheck(ascii.count == 2)
        _sanityCheck(ascii.start[0] == _CR)
        _sanityCheck(ascii.start[1] == _LF)
        return Character(_codeUnitPair: UInt16(_CR), UInt16(_LF))
      },
      utf16: { utf16 in return Character(_unverified: utf16) },
      opaque: { opaque in return Character(_unverified: opaque) })
  }

  @inlinable
  internal
  init(_singleCodeUnit cu: UInt16) {
    _sanityCheck(UTF16._isScalar(cu))
    _representation = .smallUTF16(
      Builtin.zext_Int16_Int63(Builtin.reinterpretCast(cu)))
  }

  @inlinable
  internal
    init(_codeUnitPair first: UInt16, _ second: UInt16) {
    _sanityCheck(
      (UTF16._isScalar(first) && UTF16._isScalar(second)) ||
      (UTF16.isLeadSurrogate(first) && UTF16.isTrailSurrogate(second)))
    _representation = .smallUTF16(
      Builtin.zext_Int32_Int63(
        Builtin.reinterpretCast(
          UInt32(first) | UInt32(second) &<< 16)))
  }

  @inlinable
  internal
  init(_unverified storage: _SwiftStringStorage<Unicode.UTF16.CodeUnit>) {
    if _fastPath(storage.count <= 4) {
      _sanityCheck(storage.count > 0)
      let b = _SmallUTF16(storage.unmanagedView)
      if _fastPath(Int64(bitPattern: b._storage) >= 0) {
        self.init(_smallRepresentation: b)
        _fixLifetime(storage)
        return
      }
    }
    // FIXME: We may want to make a copy if storage.unusedCapacity > 0
    self.init(_largeRepresentation: storage)
  }

  @inlinable
  internal
  init<V: _StringVariant>(_unverified variant: V) {
    if _fastPath(variant.count <= 4) {
      _sanityCheck(variant.count > 0)
      let b = _SmallUTF16(variant)
      if _fastPath(Int64(bitPattern: b._storage) >= 0) {
        self.init(_smallRepresentation: b)
        return
      }
    }
    self.init(_largeRepresentation: variant._copyToNativeStorage())
  }
}

extension Character : CustomStringConvertible {
  @inlinable // FIXME(sil-serialize-all)
  public var description: String {
    return String(describing: self)
  }
}

extension Character : LosslessStringConvertible { }

extension Character : CustomDebugStringConvertible {
  /// A textual representation of the character, suitable for debugging.
  @inlinable // FIXME(sil-serialize-all)
  public var debugDescription: String {
    return String(self).debugDescription
  }
}

extension Character {
  internal typealias _SmallUTF16 = _UIntBuffer<UInt64, Unicode.UTF16.CodeUnit>

  @inlinable // FIXME(sil-serialize-all)
  internal var _smallUTF16 : _SmallUTF16? {
    guard case .smallUTF16(let _63bits) = _representation else { return nil }
    _onFastPath()
    let bits = UInt64(Builtin.zext_Int63_Int64(_63bits))
    let minBitWidth = type(of: bits).bitWidth - bits.leadingZeroBitCount
    return _SmallUTF16(
      _storage: bits,
      _bitCount: UInt8(
        truncatingIfNeeded: 16 * Swift.max(1, (minBitWidth + 15) / 16))
    )
  }

  @inlinable // FIXME(sil-serialize-all)
  internal var _largeUTF16 : _UTF16StringStorage? {
    guard case .large(let storage) = _representation else { return nil }
    return storage
  }
}

extension Character {
  @inlinable // FIXME(sil-serialize-all)
  internal var _count : Int {
    if let small = _smallUTF16 { return small.count }
    return _largeUTF16._unsafelyUnwrappedUnchecked.count
  }
}

extension String {
  /// Creates a string containing the given character.
  ///
  /// - Parameter c: The character to convert to a string.
  @inlinable // FIXME(sil-serialize-all)
  public init(_ c: Character) {
    if let utf16 = c._smallUTF16 {
      if let small = _SmallUTF8String(utf16) {
        self = String(_StringGuts(small))
      } else {
        // FIXME: Remove when we support UTF-8 in small string
        self = String(decoding: utf16, as: Unicode.UTF16.self)
      }
    }
    else {
      // TODO(SSO): small check. For now, since we only do ASCII, this won't hit
      self = String(_StringGuts(_large: c._largeUTF16!))
    }
  }
}

/// `.small` characters are stored in an Int63 with their UTF-8 representation,
/// with any unused bytes set to 0xFF. ASCII characters will have all bytes set
/// to 0xFF except for the lowest byte, which will store the ASCII value. Since
/// 0x7FFFFFFFFFFFFF80 or greater is an invalid UTF-8 sequence, we know if a
/// value is ASCII by checking if it is greater than or equal to
/// 0x7FFFFFFFFFFFFF00.
// FIXME(sil-serialize-all): Should be @inlinable
// <rdar://problem/34557187>
internal var _minASCIICharReprBuiltin: Builtin.Int63 {
  @inline(__always) get {
    let x: Int64 = 0x7FFFFFFFFFFFFF00
    return Builtin.truncOrBitCast_Int64_Int63(x._value)
  }
}

extension Character : Equatable {
  @inlinable
  @inline(__always)
  public static func == (lhs: Character, rhs: Character) -> Bool {
    let l0 = lhs._smallUTF16
    if _fastPath(l0 != nil), let l = l0?._storage {
      let r0 = rhs._smallUTF16
      if _fastPath(r0 != nil), let r = r0?._storage {
        if (l | r) < 0x300 { return l == r }
        if l == r { return true }
      }
    }

    // FIXME(performance): constructing two temporary strings is extremely
    // wasteful and inefficient.
    return String(lhs) == String(rhs)
  }
}

extension Character : Comparable {
  @inlinable
  @inline(__always)
  public static func < (lhs: Character, rhs: Character) -> Bool {
    let l0 = lhs._smallUTF16
    if _fastPath(l0 != nil), let l = l0?._storage {
      let r0 = rhs._smallUTF16
      if _fastPath(r0 != nil), let r = r0?._storage {
        if (l | r) < 0x80 { return l < r }
        if l == r { return false }
      }
    }
    // FIXME(performance): constructing two temporary strings is extremely
    // wasteful and inefficient.
    return String(lhs) < String(rhs)
  }
}

extension Character: Hashable {
  // not @inlinable (performance)
  @effects(releasenone)
  public func hash(into hasher: inout Hasher) {
    // FIXME(performance): constructing a temporary string is extremely
    // wasteful and inefficient.
    hasher.combine(String(self))
  }
}
