//===--- UnicodeEncoding.swift --------------------------------------------===//
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

public protocol _UnicodeEncoding {
  /// The basic unit of encoding
  associatedtype CodeUnit: UnsignedInteger, FixedWidthInteger
  
  /// A valid scalar value as represented in this encoding
  associatedtype EncodedScalar: BidirectionalCollection
    where EncodedScalar.Iterator.Element == CodeUnit

  /// A unicode scalar value to be used when repairing
  /// encoding/decoding errors, as represented in this encoding.
  ///
  /// If the Unicode replacement character U+FFFD is representable in this
  /// encoding, `encodedReplacementCharacter` encodes that scalar value.
  static var encodedReplacementCharacter: EncodedScalar { get }

  /// Converts from encoded to encoding-independent representation
  static func decode(_ content: EncodedScalar) -> Unicode.Scalar

  /// Converts from encoding-independent to encoded representation, returning
  /// `nil` if the scalar can't be represented in this encoding.
  static func encode(_ content: Unicode.Scalar) -> EncodedScalar?

  /// Converts a scalar from another encoding's representation, returning
  /// `nil` if the scalar can't be represented in this encoding.
  ///
  /// A default implementation of this method will be provided 
  /// automatically for any conforming type that does not implement one.
  static func transcode<FromEncoding: Unicode.Encoding>(
    _ content: FromEncoding.EncodedScalar, from _: FromEncoding.Type
  ) -> EncodedScalar?

  /// A type that can be used to parse `CodeUnits` into
  /// `EncodedScalar`s.
  associatedtype ForwardParser: Unicode.Parser
    where ForwardParser.Encoding == Self
  
  /// A type that can be used to parse a reversed sequence of
  /// `CodeUnits` into `EncodedScalar`s.
  associatedtype ReverseParser: Unicode.Parser
    where ReverseParser.Encoding == Self

  //===--------------------------------------------------------------------===//
  // FIXME: this requirement shouldn't be here and is mitigated by the default
  // implementation below.  Compiler bugs prevent it from being expressed in an
  // intermediate, underscored protocol.
  /// Returns true if `x` only appears in this encoding as the representation of
  /// a complete scalar value.
  static func _isScalar(_ x: CodeUnit) -> Bool
}

extension _UnicodeEncoding {
  // See note on declaration of requirement, above
  @inlinable
  public static func _isScalar(_ x: CodeUnit) -> Bool { return false }

  @inlinable
  public static func transcode<FromEncoding: Unicode.Encoding>(
    _ content: FromEncoding.EncodedScalar, from _: FromEncoding.Type
  ) -> EncodedScalar? {
    return encode(FromEncoding.decode(content))
  }

  /// Converts from encoding-independent to encoded representation, returning
  /// `encodedReplacementCharacter` if the scalar can't be represented in this
  /// encoding.
  @inlinable
  internal static func _encode(_ content: Unicode.Scalar) -> EncodedScalar {
    return encode(content) ?? encodedReplacementCharacter
  }

  /// Converts a scalar from another encoding's representation, returning
  /// `encodedReplacementCharacter` if the scalar can't be represented in this
  /// encoding.
  @inlinable
  internal static func _transcode<FromEncoding: Unicode.Encoding>(
    _ content: FromEncoding.EncodedScalar, from _: FromEncoding.Type
  ) -> EncodedScalar {
    return transcode(content, from: FromEncoding.self)
      ?? encodedReplacementCharacter
  }

  @inlinable
  internal static func _transcode<
  Source: Sequence, SourceEncoding: Unicode.Encoding>(
    _ source: Source,
    from sourceEncoding: SourceEncoding.Type,
    into processScalar: (EncodedScalar)->Void)
  where Source.Element == SourceEncoding.CodeUnit {
    var p = SourceEncoding.ForwardParser()
    var i = source.makeIterator()
    while true {
      switch p.parseScalar(from: &i) {
      case .valid(let e): processScalar(_transcode(e, from: sourceEncoding))
      case .error(_): processScalar(encodedReplacementCharacter)
      case .emptyInput: return
      }
    }
  }
}

extension Unicode {
  public typealias Encoding = _UnicodeEncoding
}

