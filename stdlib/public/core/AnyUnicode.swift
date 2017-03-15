//===--- AnyUnicode.swift -------------------------------------------------===//
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

//===--- TODO -------------------------------------------------------------===//
//
//===----------------------------------------------------------------------===//
public protocol _AnyUnicode {
  var encoding: AnyUnicodeEncoding.Type { get }
  
  var isKnownLatin1: Bool { get }
  var isKnownASCII: Bool { get }
  var isKnownValidEncoding: Bool { get }
  var isKnownFCCNormalized: Bool { get }
  var isKnownFCDForm: Bool { get }
  var isKnownNFDNormalized: Bool { get }
  var isKnownNFCNormalized: Bool { get }

  func isLatin1() -> Bool
  func isASCII() -> Bool
  func isValidEncoding() -> Bool
}

public protocol _FixedFormatUnicode : _AnyUnicode {
  associatedtype Encoding: UnicodeEncoding
  var encoding: Encoding.Type { get }
  
  // func isFCCNormalized() -> Bool
  
  associatedtype CodeUnits : RandomAccessCollection
  // where Iterator.Element == Encoding.CodeUnit
  
  var codeUnits : CodeUnits { get }
  
  /// A type that presents the string's UTF-16 code units without necessarily
  /// correcting encoding errors
  associatedtype RawUTF16View : BidirectionalCollection
  // where Iterator.Element == UInt16

  /// The string's UTF-16 code units, without necessarily correcting encoding
  /// errors.
  var rawUTF16: RawUTF16View { get }

  /// A type that presents an FCC-normalized view of the string
  associatedtype FCCNormalizedUTF16View : BidirectionalCollection
  // where Iterator.Element == UInt16

  /// An FCC-normalized view of the string
  var fccNormalizedUTF16: FCCNormalizedUTF16View { get }

  associatedtype CharacterView : BidirectionalCollection
  // where Iterator.Element == Character

  var characters: CharacterView { get }

  /// A type that presents the string's unicode scalar values
  associatedtype UnicodeScalarView : BidirectionalCollection
  // where Iterator.Element == UnicodeScalar

  var unicodeScalars: UnicodeScalarView { get }
  
  /// A type presenting ASCII unicode scalar values verbatim, and otherwise
  /// presenting values >= 128, which is outside the range of ASCII.
  associatedtype ExtendedASCIIView : BidirectionalCollection = CodeUnits
  // where Iterator.Element : UnsignedInteger
  
  var extendedASCII: ExtendedASCIIView { get }
}

/// Default views
public extension _FixedFormatUnicode
where
  CodeUnits.Iterator.Element  == Encoding.EncodedScalar.Iterator.Element,
  CodeUnits.Iterator.Element : UnsignedInteger,
  CodeUnits.SubSequence : RandomAccessCollection,
  CodeUnits.SubSequence.Index == CodeUnits.Index,
  CodeUnits.SubSequence.SubSequence == CodeUnits.SubSequence,
  CodeUnits.SubSequence.Iterator.Element == CodeUnits.Iterator.Element {
  var characters: UnicodeStorage<CodeUnits,Encoding>.CharacterView {
    return UnicodeStorage(codeUnits).characters
  }
}

// UTF32 gets a default UnicodeScalarView that injects replacement characters
// for illegal scalar values
public extension _FixedFormatUnicode
where
  Encoding == UTF32,
  CodeUnits.Iterator.Element  == Encoding.EncodedScalar.Iterator.Element,
  CodeUnits.Iterator.Element : UnsignedInteger,
  CodeUnits.SubSequence : RandomAccessCollection,
  CodeUnits.SubSequence.Index == CodeUnits.Index,
  CodeUnits.SubSequence.SubSequence == CodeUnits.SubSequence,
  CodeUnits.SubSequence.Iterator.Element == CodeUnits.Iterator.Element {

  var unicodeScalars: LazyMapCollection<CodeUnits, UnicodeScalar> {
    return codeUnits.lazy.map {
      UnicodeScalar($0)
      ?? UnicodeScalar(_unchecked: 0xFFFD)
    }
  }
}

// Everybody else gets a UnicodeScalarView based on transcoding to UTF32, which
// already makes any necessary corrections.
public extension _FixedFormatUnicode
where
  CodeUnits.Iterator.Element  == Encoding.EncodedScalar.Iterator.Element,
  CodeUnits.Iterator.Element : UnsignedInteger,
  CodeUnits.SubSequence : RandomAccessCollection,
  CodeUnits.SubSequence.Index == CodeUnits.Index,
  CodeUnits.SubSequence.SubSequence == CodeUnits.SubSequence,
  CodeUnits.SubSequence.Iterator.Element == CodeUnits.Iterator.Element {

  var unicodeScalars: LazyMapBidirectionalCollection<
    UnicodeStorage<CodeUnits,Encoding>.ScalarsTranscoded<UTF32>
  , UnicodeScalar
  > {
    return UnicodeStorage(codeUnits, Encoding.self)
      .scalarsTranscoded(to: UTF32.self)
      .lazy.map { UnicodeScalar($0) }
  }
}

public extension _FixedFormatUnicode {
  var encoding: AnyUnicodeEncoding.Type {
    return encoding as Encoding.Type
  }
}

public extension _FixedFormatUnicode where ExtendedASCIIView == CodeUnits {
  var extendedASCII: CodeUnits {
    return codeUnits
  }
}

/// Default implementations
public extension _FixedFormatUnicode {

  var isKnownLatin1: Bool { return false }
  var isKnownASCII: Bool { return false }
  var isKnownValidEncoding: Bool { return false }
  var isKnownFCCNormalized: Bool { return false }
  var isKnownFCDForm: Bool {
    return isKnownFCCNormalized || isKnownNFDNormalized
  }
}

public extension _FixedFormatUnicode
where RawUTF16View.Iterator.Element : UnsignedInteger {
  // FIXME: we'd like to put this up in the unconditional extension, but we are
  // forbidden.
  var encoding: Encoding.Type { return Encoding.self }
  
  func isLatin1() -> Bool {
    return isKnownLatin1 || !rawUTF16.contains { $0 > 0xFF }
  }
  
  func isASCII() -> Bool {
    return isKnownASCII || !rawUTF16.contains { $0 > 0x7f }
  }
}

public extension _FixedFormatUnicode
where RawUTF16View.Iterator.Element : UnsignedInteger,
  CodeUnits.SubSequence : Collection, 
  CodeUnits.SubSequence.SubSequence == CodeUnits.SubSequence,
  CodeUnits.SubSequence.Iterator.Element == Encoding.EncodedScalar.Iterator.Element
{
  func isValidEncoding() -> Bool {
    return Encoding.parseForward(
      codeUnits, repairingIllFormedSequences: false
    ) { _ in }.errorCount == 0
  }
}

//===--- Defaults for Latin-1 ---------------------------------------------===//
public extension _FixedFormatUnicode where Encoding == Latin1 {
  var isKnownLatin1: Bool { return true }
  var isKnownValidEncoding: Bool { return true }
  var isKnownFCCNormalized: Bool { return true }
}
  
public extension _FixedFormatUnicode
where Encoding == Latin1, 
  CodeUnits.Iterator.Element == Encoding.EncodedScalar.Iterator.Element,
  CodeUnits.Iterator.Element : UnsignedInteger,
  CodeUnits.SubSequence : RandomAccessCollection,
  CodeUnits.SubSequence.Index == CodeUnits.Index,
  CodeUnits.SubSequence.SubSequence == CodeUnits.SubSequence,
  CodeUnits.SubSequence.Iterator.Element == CodeUnits.Iterator.Element
{
  var rawUTF16: LazyMapRandomAccessCollection<CodeUnits, UInt16> {
    return fccNormalizedUTF16
  }

  /// An FCC-normalized view of the string
  var fccNormalizedUTF16: LazyMapRandomAccessCollection<CodeUnits, UInt16> {
    return codeUnits.lazy.map { numericCast($0) }
  }
  
  var characters: LazyMapRandomAccessCollection<CodeUnits, Character> {
    return codeUnits.lazy.map {
      Character(UnicodeScalar(_unchecked: numericCast($0)))
    }
  }

  var unicodeScalars: LazyMapRandomAccessCollection<CodeUnits, UnicodeScalar> {
    return codeUnits.lazy.map {
      UnicodeScalar(_unchecked: numericCast($0))
    }
  }
}
  
//===--- Defaults for UTF16 and ValidUTF16 --------------------------------===//
public extension _FixedFormatUnicode
where Encoding.EncodedScalar == UTF16.EncodedScalar,
  CodeUnits.Iterator.Element : UnsignedInteger,
  CodeUnits.Iterator.Element == UTF16.CodeUnit,
  CodeUnits.SubSequence : RandomAccessCollection,
  CodeUnits.SubSequence.Index == CodeUnits.Index,
  CodeUnits.SubSequence.SubSequence == CodeUnits.SubSequence,
  CodeUnits.SubSequence.Iterator.Element == CodeUnits.Iterator.Element {
  
  // FIXME: we should have a way to represent the validity of the encoding of
  // this result—and maybe other nice properties—in the type system.  So maybe
  // this thing should conform to _FixedFormatUnicode
  var fccNormalizedUTF16
  : UnicodeStorage<CodeUnits,Encoding>.FCCNormalizedUTF16View {
    return UnicodeStorage(codeUnits, Encoding.self).fccNormalizedUTF16
  }
  
  var rawUTF16 : CodeUnits {
    return codeUnits
  }
}

