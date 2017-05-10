//===--- ASCII.swift ------------------------------------------------------===//
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
extension _Unicode {
  @_fixed_layout
  public enum ASCII {}
}

extension _Unicode.ASCII : UnicodeEncoding {
  public typealias CodeUnit = UInt8
  public typealias EncodedScalar = CollectionOfOne<CodeUnit>

  public static var encodedReplacementCharacter : EncodedScalar {
    return EncodedScalar(0x1a) // U+001A SUBSTITUTE; best we can do for ASCII
  }

  @inline(__always)
  @_inlineable
  public static func _isScalar(_ x: CodeUnit) -> Bool {
    return true
  }

  @inline(__always)
  @_inlineable
  public static func decode(_ source: EncodedScalar) -> UnicodeScalar {
    return UnicodeScalar(_unchecked: UInt32(
        source.first._unsafelyUnwrappedUnchecked))
  }
  
  @inline(__always)
  @_inlineable
  public static func encode(
    _ source: UnicodeScalar
  ) -> EncodedScalar? {
    guard source.value < (1&<<7) else { return nil }
    return EncodedScalar(UInt8(extendingOrTruncating: source.value))
  }

  @inline(__always)
  public static func transcode<FromEncoding : UnicodeEncoding>(
    _ content: FromEncoding.EncodedScalar, from _: FromEncoding.Type
  ) -> EncodedScalar? {
    if _fastPath(FromEncoding.self == UTF16.self) {
      let c = unsafeBitCast(content, to: UTF16.EncodedScalar.self)
      guard (c._storage & 0xFF80 == 0) else { return nil }
      return EncodedScalar(CodeUnit(c._storage & 0x7f))
    }
    else if _fastPath(FromEncoding.self == UTF8.self) {
      let c = unsafeBitCast(content, to: UTF8.EncodedScalar.self)
      guard (c._storage & 0x80 == 0) else { return nil }
      return EncodedScalar(CodeUnit(c._storage & 0x7f))
    }
    return encode(FromEncoding.decode(content))
  }

  public struct Parser {
    public init() { }
  }
  
  public typealias ForwardParser = Parser
  public typealias ReverseParser = Parser
}

extension _Unicode.ASCII.Parser : UnicodeParser {
  public typealias Encoding = _Unicode.ASCII

  /// Parses a single Unicode scalar value from `input`.
  public mutating func parseScalar<I : IteratorProtocol>(
    from input: inout I
  ) -> _Unicode.ParseResult<Encoding.EncodedScalar>
  where I.Element == Encoding.CodeUnit {
    let n = input.next()
    if _fastPath(n != nil), let x = n {
      guard _fastPath(Int8(extendingOrTruncating: x) >= 0)
      else { return .error(length: 1) }
      return .valid(_Unicode.ASCII.EncodedScalar(x))
    }
    return .emptyInput
  }
}
