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
extension Unicode {
  @frozen
  public enum ASCII {}
}

extension Unicode.ASCII: Unicode.Encoding {
  public typealias CodeUnit = UInt8
  public typealias EncodedScalar = CollectionOfOne<CodeUnit>

  @inlinable
  public static var encodedReplacementCharacter: EncodedScalar {
    return EncodedScalar(0x1a) // U+001A SUBSTITUTE; best we can do for ASCII
  }

  /// Returns whether the given code unit represents an ASCII scalar
  @_alwaysEmitIntoClient
  public static func isASCII(_ x: CodeUnit) -> Bool { return UTF8.isASCII(x) }

  @inline(__always)
  @inlinable
  public static func _isScalar(_ x: CodeUnit) -> Bool {
    return true
  }

  @inline(__always)
  @inlinable
  public static func decode(_ source: EncodedScalar) -> Unicode.Scalar {
    return Unicode.Scalar(_unchecked: UInt32(
        source.first._unsafelyUnwrappedUnchecked))
  }
  
  @inline(__always)
  @inlinable
  public static func encode(
    _ source: Unicode.Scalar
  ) -> EncodedScalar? {
    guard source.value < (1&<<7) else { return nil }
    return EncodedScalar(UInt8(truncatingIfNeeded: source.value))
  }

  @inline(__always)
  @inlinable
  public static func transcode<FromEncoding: Unicode.Encoding>(
    _ content: FromEncoding.EncodedScalar, from _: FromEncoding.Type
  ) -> EncodedScalar? {
    if _fastPath(FromEncoding.self == UTF16.self) {
      let c = _identityCast(content, to: UTF16.EncodedScalar.self)
      guard (c._storage & 0xFF80 == 0) else { return nil }
      return EncodedScalar(CodeUnit(c._storage & 0x7f))
    }
    else if _fastPath(FromEncoding.self == UTF8.self) {
      let c = _identityCast(content, to: UTF8.EncodedScalar.self)
      let first = unsafe c.first.unsafelyUnwrapped
      guard (first < 0x80) else { return nil }
      return EncodedScalar(CodeUnit(first))
    }
    return encode(FromEncoding.decode(content))
  }

  @frozen
  public struct Parser {
    @inlinable
    public init() { }
  }
  
  public typealias ForwardParser = Parser
  public typealias ReverseParser = Parser
}

extension Unicode.ASCII.Parser: Unicode.Parser {
  public typealias Encoding = Unicode.ASCII

  /// Parses a single Unicode scalar value from `input`.
  @inlinable
  public mutating func parseScalar<I: IteratorProtocol>(
    from input: inout I
  ) -> Unicode.ParseResult<Encoding.EncodedScalar>
  where I.Element == Encoding.CodeUnit {
    let n = input.next()
    if _fastPath(n != nil), let x = n {
      guard _fastPath(Int8(truncatingIfNeeded: x) >= 0)
      else { return .error(length: 1) }
      return .valid(Unicode.ASCII.EncodedScalar(x))
    }
    return .emptyInput
  }
}
