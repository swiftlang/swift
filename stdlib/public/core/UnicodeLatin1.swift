//===--- UnicodeLatin1.swift ----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *)
extension Unicode {

  @frozen
  public enum Latin1 {}
}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *)
extension Unicode.Latin1: Unicode.Encoding {

  @frozen
  public struct Parser {

    @inlinable
    public init() {}
  }

  public typealias CodeUnit = UInt8
  public typealias EncodedScalar = CollectionOfOne<CodeUnit>
  public typealias ForwardParser = Parser
  public typealias ReverseParser = Parser

  @inlinable
  public static var encodedReplacementCharacter: EncodedScalar {
    EncodedScalar(0x1A) // U+001A SUBSTITUTE
  }

  @inlinable
  public static func decode(_ encodedScalar: EncodedScalar) -> Unicode.Scalar {
    Unicode.Scalar(encodedScalar[0])
  }

  @inlinable
  public static func encode(_ unicodeScalar: Unicode.Scalar) -> EncodedScalar? {
    CodeUnit(exactly: unicodeScalar.value).map { EncodedScalar($0) }
  }

  @inlinable
  public static func _isScalar(_: CodeUnit) -> Bool { true }
}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *)
extension Unicode.Latin1.Parser: Unicode.Parser {

  public typealias Encoding = Unicode.Latin1

  @inlinable
  public mutating func parseScalar<I: IteratorProtocol>(
    from codeUnits: inout I
  ) -> Unicode.ParseResult<Encoding.EncodedScalar>
  where I.Element == Encoding.CodeUnit {
    codeUnits.next().map { .valid(Encoding.EncodedScalar($0)) } ?? .emptyInput
  }
}
