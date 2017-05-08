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
  associatedtype CodeUnit : UnsignedInteger, FixedWidthInteger
  
  /// A valid scalar value as represented in this encoding
  associatedtype EncodedScalar : BidirectionalCollection
    where EncodedScalar.Iterator.Element == CodeUnit

  /// The replacement character U+FFFD as represented in this encoding
  static var encodedReplacementCharacter : EncodedScalar { get }

  /// Converts from encoded to encoding-independent representation
  static func decode(_ content: EncodedScalar) -> UnicodeScalar

  /// Converts from encoding-independent to encoded representation, returning
  /// `nil` if the scalar can't be represented in this encoding.
  static func encodeIfRepresentable(_ content: UnicodeScalar) -> EncodedScalar?

  /// Converts a scalar from another encoding's representation, returning
  /// `nil` if the scalar can't be represented in this encoding.
  static func transcodeIfRepresentable<FromEncoding : UnicodeEncoding>(
    _ content: FromEncoding.EncodedScalar, from _: FromEncoding.Type
  ) -> EncodedScalar?

  associatedtype ForwardParser : UnicodeParser
  associatedtype ReverseParser : UnicodeParser
  
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
  public static func _isScalar(_ x: CodeUnit) -> Bool { return false }
}

public protocol UnicodeEncoding : _UnicodeEncoding
where ForwardParser.Encoding == Self, ReverseParser.Encoding == Self {}

extension _UnicodeEncoding {
  public static func transcodeIfRepresentable<FromEncoding : UnicodeEncoding>(
    _ content: FromEncoding.EncodedScalar, from _: FromEncoding.Type
  ) -> EncodedScalar? {
    return encodeIfRepresentable(FromEncoding.decode(content))
  }

  /// Converts from encoding-independent to encoded representation, returning
  /// `nil` if the scalar can't be represented in this encoding.
  public static func encode(_ content: UnicodeScalar) -> EncodedScalar {
    return encodeIfRepresentable(content) ?? encodedReplacementCharacter
  }

  /// Converts a scalar from another encoding's representation, returning
  /// `nil` if the scalar can't be represented in this encoding.
  public static func transcode<FromEncoding : UnicodeEncoding>(
    _ content: FromEncoding.EncodedScalar, from _: FromEncoding.Type
  ) -> EncodedScalar {
    return encode(FromEncoding.decode(content))
  }
}
