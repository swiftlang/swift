//===--- ISO8859-1.swift --------------------------------------------------===//
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
// ISO-8859-1 encoding support.
//
//===----------------------------------------------------------------------===//

import Swift

extension Unicode {
  enum ISO8859_1 {}
}

extension Unicode.ISO8859_1: Unicode.Encoding {
  typealias CodeUnit = UInt8
  typealias EncodedScalar = CollectionOfOne<CodeUnit>

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
    return Unicode.Scalar(UInt32(source.first.unsafelyUnwrapped))!
  }

  @inline(__always)
  @inlinable
  static func encode(
    _ source: Unicode.Scalar
  ) -> EncodedScalar? {
    guard source.value < (1&<<8) else { return nil }
    return EncodedScalar(UInt8(truncatingIfNeeded: source.value))
  }

  @inline(__always)
  @inlinable
  static func transcode<FromEncoding: Unicode.Encoding>(
    _ content: FromEncoding.EncodedScalar, from _: FromEncoding.Type
  ) -> EncodedScalar? {
    if _fastPath(FromEncoding.self == UTF16.self) {
      let c = _identityCast(content, to: UTF16.EncodedScalar.self)
      guard (c._storage & 0xFF00 == 0) else { return nil }
      return EncodedScalar(CodeUnit(c._storage & 0x7f))
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

extension Unicode.ISO8859_1.Parser: Unicode.Parser {
  typealias Encoding = Unicode.ISO8859_1

  /// Parses a single Unicode scalar value from `input`.
  @inlinable
  mutating func parseScalar<I: IteratorProtocol>(
    from input: inout I
  ) -> Unicode.ParseResult<Encoding.EncodedScalar>
  where I.Element == Encoding.CodeUnit {
    let n = input.next()
    if _fastPath(n != nil), let x = n {
      return .valid(Unicode.ISO8859_1.EncodedScalar(x))
    }
    return .emptyInput
  }
}

typealias ISO8859_1 = Unicode.ISO8859_1
