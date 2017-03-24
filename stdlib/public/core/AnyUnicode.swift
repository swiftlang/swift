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

/// Types that present useful views and information about Unicode content.
///
/// Unicode and _UnicodeStorage refine this protocol.
public protocol _UnicodeContent {
  var isKnownLatin1: Bool { get }
  var isKnownASCII: Bool { get }
  var isKnownFCCNormalized: Bool { get }
  var isKnownFCDForm: Bool { get }
  var isKnownNFDNormalized: Bool { get }
  var isKnownNFCNormalized: Bool { get }

  func isLatin1() -> Bool
  func isASCII() -> Bool

  /// A type that presents the string's UTF-16 code units without necessarily
  /// correcting encoding errors
  associatedtype UTF16View : UnicodeView
  // where Iterator.Element == UInt16

  /// The string's UTF-16 code units, without necessarily correcting encoding
  /// errors.
  var utf16: UTF16View { get }

  /// A type that presents an FCC-normalized view of the string
  associatedtype FCCNormalizedUTF16View : UnicodeView
  // where Iterator.Element == UInt16

  /// An FCC-normalized view of the string
  var fccNormalizedUTF16: FCCNormalizedUTF16View { get }

  associatedtype CharacterView : UnicodeView
  // where Iterator.Element == Character

  var characters: CharacterView { get }

  /// A type that presents the string's unicode scalar values
  associatedtype UnicodeScalarView : UnicodeView
  // where Iterator.Element == UnicodeScalar

  var unicodeScalars: UnicodeScalarView { get }
  
#if false
  /// A type presenting ASCII-only extended grapheme clusters (`Character`s) as
  /// their (single) unicode scalar values, and presenting all other
  /// `Character`s as `nil`.
  associatedtype ASCIIOnlyView : UnicodeView
  // where Iterator.Element : UInt8?
  
  var asciiOnlyView: ASCIIOnlyView { get }
#endif
}

/// Types that present Unicode content in a given encoding.
///
/// Typical models are the buffers that provide storage for String
public protocol _UnicodeStorage : _UnicodeContent {
  associatedtype Encoding: UnicodeEncoding
  var encoding: Encoding.Type { get }
  
  associatedtype CodeUnits : RandomAccessCollection
  // where Iterator.Element == Encoding.CodeUnit

  var isKnownValidEncoding: Bool { get }
  func isValidEncoding() -> Bool
  
  var codeUnits : CodeUnits { get }
  // func isFCCNormalized() -> Bool
}

/// Default views
public extension _UnicodeStorage
where
  CodeUnits.Iterator.Element  == Encoding.EncodedScalar.Iterator.Element,
  CodeUnits.Iterator.Element : UnsignedInteger,
  CodeUnits.SubSequence : RandomAccessCollection,
  CodeUnits.SubSequence.Index == CodeUnits.Index,
  CodeUnits.SubSequence.SubSequence == CodeUnits.SubSequence,
  CodeUnits.SubSequence.Iterator.Element == CodeUnits.Iterator.Element {
  var characters: _UnicodeViews<CodeUnits,Encoding>.CharacterView {
    return _UnicodeViews(codeUnits).characters
  }
}

// UTF32 gets a default UnicodeScalarView that injects replacement characters
// for illegal scalar values
public extension _UnicodeStorage
where
  Encoding == UTF32,
  CodeUnits.Iterator.Element  == Encoding.EncodedScalar.Iterator.Element,
  CodeUnits.Iterator.Element : UnsignedInteger,
  CodeUnits.SubSequence : RandomAccessCollection,
  CodeUnits.SubSequence.Index == CodeUnits.Index,
  CodeUnits.SubSequence.SubSequence == CodeUnits.SubSequence,
  CodeUnits.SubSequence.Iterator.Element == CodeUnits.Iterator.Element {

  var unicodeScalars: RandomAccessUnicodeView<
    LazyMapRandomAccessCollection<CodeUnits, UnicodeScalar>
  > {
    return RandomAccessUnicodeView(
      codeUnits.lazy.map {
        UnicodeScalar($0)
        ?? UnicodeScalar(_unchecked: 0xFFFD)
      }
    )
  }
}

// Everybody else gets a UnicodeScalarView based on transcoding to UTF32, which
// already makes any necessary corrections.
public extension _UnicodeStorage
where
  CodeUnits.Iterator.Element  == Encoding.EncodedScalar.Iterator.Element,
  CodeUnits.Iterator.Element : UnsignedInteger,
  CodeUnits.SubSequence : RandomAccessCollection,
  CodeUnits.SubSequence.Index == CodeUnits.Index,
  CodeUnits.SubSequence.SubSequence == CodeUnits.SubSequence,
  CodeUnits.SubSequence.Iterator.Element == CodeUnits.Iterator.Element {

  var unicodeScalars: _UnicodeViews<CodeUnits,Encoding>.Scalars {
    return _UnicodeViews(codeUnits, Encoding.self).scalars
  }
}

/// Default implementations
public extension _UnicodeStorage {

  var isKnownLatin1: Bool { return false }
  var isKnownASCII: Bool { return false }
  var isKnownValidEncoding: Bool { return false }
  var isKnownFCCNormalized: Bool { return false }
  var isKnownFCDForm: Bool {
    return isKnownFCCNormalized || isKnownNFDNormalized
  }
}

public extension _UnicodeStorage
where UTF16View.Iterator.Element : UnsignedInteger {
  // FIXME: we'd like to put this up in the unconditional extension, but we are
  // forbidden.
  var encoding: Encoding.Type { return Encoding.self }
  
  func isLatin1() -> Bool {
    return isKnownLatin1 || !utf16.contains { $0 > 0xFF }
  }
  
  func isASCII() -> Bool {
    return isKnownASCII || !utf16.contains { $0 > 0x7f }
  }
}

public extension _UnicodeStorage
where UTF16View.Iterator.Element : UnsignedInteger,
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
public extension _UnicodeStorage where Encoding == Latin1 {
  var isKnownLatin1: Bool { return true }
  var isKnownValidEncoding: Bool { return true }
  var isKnownFCCNormalized: Bool { return true }
}
  
public extension _UnicodeStorage
where Encoding == Latin1, 
  CodeUnits.Iterator.Element == Encoding.EncodedScalar.Iterator.Element,
  CodeUnits.Iterator.Element : UnsignedInteger,
  CodeUnits.SubSequence : RandomAccessCollection,
  CodeUnits.SubSequence.Index == CodeUnits.Index,
  CodeUnits.SubSequence.SubSequence == CodeUnits.SubSequence,
  CodeUnits.SubSequence.Iterator.Element == CodeUnits.Iterator.Element
{
  var utf16: RandomAccessUnicodeView<
    LazyMapRandomAccessCollection<CodeUnits, UInt16>
  > {
    return fccNormalizedUTF16
  }

  /// An FCC-normalized view of the string
  var fccNormalizedUTF16: RandomAccessUnicodeView<
    LazyMapRandomAccessCollection<CodeUnits, UInt16>
  > {
    return RandomAccessUnicodeView(codeUnits.lazy.map { numericCast($0) })
  }
  
  var characters: RandomAccessUnicodeView<
    LazyMapRandomAccessCollection<CodeUnits, Character>
  > {
    return RandomAccessUnicodeView(
      codeUnits.lazy.map {
        Character(UnicodeScalar(_unchecked: numericCast($0)))
      })
  }

  var unicodeScalars: RandomAccessUnicodeView<
    LazyMapRandomAccessCollection<CodeUnits, UnicodeScalar>
  > {
    return RandomAccessUnicodeView(
      codeUnits.lazy.map {
      UnicodeScalar(_unchecked: numericCast($0))
    })
  }
}
  
//===--- Defaults for UTF16 and ValidUTF16 --------------------------------===//
public extension _UnicodeStorage
where Encoding.EncodedScalar == UTF16.EncodedScalar,
  CodeUnits.Iterator.Element == UTF16.CodeUnit,
  CodeUnits.SubSequence : RandomAccessCollection,
  CodeUnits.SubSequence.Index == CodeUnits.Index,
  CodeUnits.SubSequence.SubSequence == CodeUnits.SubSequence,
  CodeUnits.SubSequence.Iterator.Element == CodeUnits.Iterator.Element {
  
  // FIXME: we should have a way to represent the validity of the encoding of
  // this result—and maybe other nice properties—in the type system.  So maybe
  // this thing should conform to _UnicodeStorage
  var fccNormalizedUTF16
  : _UnicodeViews<CodeUnits,Encoding>.FCCNormalizedUTF16View {
    return _UnicodeViews(codeUnits, Encoding.self).fccNormalizedUTF16
  }
  
  var utf16 : RandomAccessUnicodeView<CodeUnits> {
    return RandomAccessUnicodeView(codeUnits)
  }
}

