// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest




enum UnicodeContentWidth: UInt32 {
case ASCII = 1 << 7
case Latin1 = 1 << 8
case UCS2 = 1 << 16
case Unlimited = 1 << 21
}

protocol FixedEncodingUnicodeStorage : RandomAccessCollection {
  associatedtype Encoding: UnicodeEncoding // where Encoding.EncodedScalar.Iterator.Element == Iterator.Element
}

protocol Unicode : BidirectionalCollection
  /* where Iterator.Element == UnicodeScalar */ {
  associatedtype Encoding : AnyUnicodeEncoding
  
  /// An underlying collection of code units
  associatedtype CodeUnits: RandomAccessCollection
  // where CodeUnits.Iterator.Element ==
  // Encoding.EncodedScalar.Iterator.Element,
  
  associatedtype ExtendedASCII: BidirectionalCollection
  // where Iterator.Element == UInt32
  
  var codeUnits: CodeUnits {get}

  func contentWidth(knownWithoutScanning: Bool = false) -> UInt32
  func isNormalizedNFC(knownWithoutScanning: Bool = false) -> Bool
  func isNormalizedNFD(knownWithoutScanning: Bool = false) -> Bool
  func isInFastCOrDForm(knownWithoutScanning: Bool = false) -> Bool
}

enum StringStorage : Unicode {
case small7Bit(UInt8,UInt8,UInt8,UInt8,UInt8,UInt8,UInt8),
  arbitrary()

  // This can be better; see https://bugs.swift.org/browse/SR-3740
  var codeUnits: AnyRandomAccessCollection<UInt32>
  
  // where Iterator.Element == UInt32
  var encoding: AnyUnicodeEncoding
}

struct StringFacade<U: Unicode> {
  var unicode: U
}
