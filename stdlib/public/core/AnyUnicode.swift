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

public extension _FixedFormatUnicode {
  var encoding: AnyUnicodeEncoding.Type {
    return encoding as Encoding.Type
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
where Encoding == Latin1, CodeUnits.Iterator.Element : UnsignedInteger {
  var rawUTF16: LazyMapRandomAccessCollection<CodeUnits, UInt16> {
    return fccNormalizedUTF16
  }

  /// An FCC-normalized view of the string
  var fccNormalizedUTF16: LazyMapRandomAccessCollection<CodeUnits, UInt16> {
    return codeUnits.lazy.map { numericCast($0) }
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

