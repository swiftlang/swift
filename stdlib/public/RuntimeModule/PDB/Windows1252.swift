//===--- Windows1252.swift ------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  Windows CP1252 encoding support.
//
//===----------------------------------------------------------------------===//

import Swift

extension Unicode {
  enum Windows1252 {}
}

/// Windows-1252 encoding is the same as ISO8859-1, except for the range
/// 0x80..0x9f, which map to Unicode characters (in most cases) rather than
/// to C1 control codes.
extension Unicode.Windows1252: Unicode.Encoding {
  typealias CodeUnit = UInt8
  typealias EncodedScalar = CollectionOfOne<CodeUnit>

  @usableFromInline
  static let code2Unicode: [UInt16] = [
    0x20ac, 0x0081, 0x201a, 0x0192, 0x201e, 0x2026, 0x2020, 0x2021,
    0x02c6, 0x2030, 0x0160, 0x2039, 0x0152, 0x008d, 0x017d, 0x008f,
    0x0090, 0x2018, 0x2019, 0x201c, 0x201d, 0x2022, 0x2013, 0x2014,
    0x02dc, 0x2122, 0x0161, 0x20ea, 0x0153, 0x009d, 0x017e, 0x0178
  ]

  @inlinable
  static var encodedReplacementCharacter: EncodedScalar {
    return EncodedScalar(0x1a) // U+001A SUBSTITUTE; best we can do for ISO8859
  }

  @inline(__always)
  @inlinable
  static func _isScalar(_ x: CodeUnit) -> Bool {
    return true
  }

  @inline(__always)
  @inlinable
  static func decode(_ source: EncodedScalar) -> Unicode.Scalar {
    let codeUnit = source.first.unsafelyUnwrapped
    if codeUnit >= 0x80 && codeUnit < 0xa0 {
      return Unicode.Scalar(code2Unicode[Int(codeUnit - 0x80)])!
    }
    return Unicode.Scalar(UInt32(codeUnit))!
  }

  @inline(__always)
  @inlinable
  static func encode(
    _ source: Unicode.Scalar
  ) -> EncodedScalar? {
    if source.value < 0x100 && (source.value < 0x80 || source.value >= 0xa0) {
      return EncodedScalar(UInt8(truncatingIfNeeded: source.value))
    } else if source.value < 0x2200 {
      let codePoint16 = UInt16(truncatingIfNeeded: source.value)
      guard let ndx = code2Unicode.firstIndex(of: codePoint16) else {
        return nil
      }
      return EncodedScalar(UInt8(truncatingIfNeeded: ndx) + 0x80)
    } else {
      return nil
    }
  }

  @inline(__always)
  @inlinable
  static func transcode<FromEncoding: Unicode.Encoding>(
    _ content: FromEncoding.EncodedScalar, from _: FromEncoding.Type
  ) -> EncodedScalar? {
    if _fastPath(FromEncoding.self == UTF16.self) {
      let c = _identityCast(content, to: UTF16.EncodedScalar.self)
      if c._storage < 0x100 && (c._storage < 0x80 || c._storage >= 0xa0) {
        return EncodedScalar(CodeUnit(c._storage & 0x7f))
      }
    }
    else if _fastPath(FromEncoding.self == UTF8.self) {
      let c = _identityCast(content, to: UTF8.EncodedScalar.self)
      let first = unsafe c.first.unsafelyUnwrapped
      if first < 0x80 {
        return EncodedScalar(CodeUnit(first))
      }
    }
    return encode(FromEncoding.decode(content))
  }

  struct Parser {
    @inlinable
    init() { }
  }

  typealias ForwardParser = Parser
  typealias ReverseParser = Parser
}

extension Unicode.Windows1252.Parser: Unicode.Parser {
  typealias Encoding = Unicode.Windows1252

  /// Parses a single Unicode scalar value from `input`.
  @inlinable
  mutating func parseScalar<I: IteratorProtocol>(
    from input: inout I
  ) -> Unicode.ParseResult<Encoding.EncodedScalar>
  where I.Element == Encoding.CodeUnit {
    let n = input.next()
    if _fastPath(n != nil), let x = n {
      return .valid(Unicode.Windows1252.EncodedScalar(x))
    }
    return .emptyInput
  }
}

typealias Windows1252 = Unicode.Windows1252
