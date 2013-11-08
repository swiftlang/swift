// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -i -parse-stdlib -module-cache-path=%t/clang-module-cache -sdk=%sdk %s | FileCheck %s
// REQUIRES: sdk
// REQUIRES: swift_interpreter

// Codecs - We're going to check these against the results provided by NSString
import Foundation

protocol CocoaUnicodeCodec : UnicodeCodec {
  static def encodingId() -> NSStringEncoding
}

extension UTF8 : CocoaUnicodeCodec {
  static def encodingId() -> NSStringEncoding {
    return NSStringEncoding(NSUTF8StringEncoding)
  }
}

extension UTF16 : CocoaUnicodeCodec {
  static def encodingId() -> NSStringEncoding {
    return NSStringEncoding(NSUTF16LittleEndianStringEncoding)
  }
}

extension UTF32 : CocoaUnicodeCodec {
  static def encodingId() -> NSStringEncoding {
    return NSStringEncoding(NSUTF32LittleEndianStringEncoding)
  }
}

var _unicodeScalarRanges  = Array<Range<UInt32>>()
var unicodeScalarRanges : Array<Range<UInt32>> {
  if _unicodeScalarRanges.count == 0 {
    for r in [UInt32(0)..0xD800, 0xE000..0xFDD0, 0xFDF0..0xFFFE] {
      _unicodeScalarRanges.append(r)
    }
    for base in UInt32(0x1)..0x11 {
      _unicodeScalarRanges.append((base << 16)..((base << 16)+0xFFFE))
    }
  }
  return _unicodeScalarRanges
}

// Execute body passing each legal Unicode scalar value
def forAllUnicodeScalars(body: (c: UnicodeScalar)->()) {
  for r in unicodeScalarRanges {
    for c in r {
      body(UnicodeScalar(c))
    }
  }
}

// buffer should have a length >= 4
// used and c MUST both have length >= 1
//
// To understand the reason for slices, see <rdar://problem/15374342>
// (Get an UnsafePointer to data without forcing it to the heap)
def nsEncode<CodeUnit>(
  c: UInt32[], encoding: NSStringEncoding, buffer: CodeUnit[], used: NSUInteger[]
) {
  assert(used.count > 0)
  assert(c.count > 0)
  var s = NSString(
    withBytes: COpaquePointer(c.base),
    length: NSUInteger(4),
    encoding: NSStringEncoding(NSUTF32LittleEndianStringEncoding))
  
  s.getBytes(
      COpaquePointer(buffer.base),
      maxLength: NSUInteger(buffer.count),
      usedLength: used.base,
      encoding: encoding,
      options: NSStringEncodingConversionOptions(0),
      range: NSRange(location: 0, length: s.length()),
      remainingRange: UnsafePointer<NSRange>.null())
}

class GeneratorRef<G: Generator> : Generator {
  init (value: G) {
    self.value = [value]
  }
  def next() -> G.Element? {
    return value[0].next()
  }
  var value: G[]
}

def hex<T: FormattedPrintable>(x: T) -> String {
  return "0x" + x.format('x', "")
}

def hex<T: FormattedPrintable>(x: T[]) -> String {
  var r = "["
  var prefix = ""
  for unit in x {
    r += prefix + hex(unit)
    prefix = ", "
  }
  r += "]"
  return r
}

def equal<T: Equatable>(lhs: T[], rhs: T[]) -> Bool {
  if lhs.count != rhs.count {
    return false
  }
  for i in 0..lhs.count {
    if lhs[i] != rhs[i] {
      return false
    }
  }
  return true
}

def testUTF8() {
  var usedRef = [NSUInteger(0)]
  typealias Encoding = UTF8
  typealias CodeUnit = Encoding.CodeUnit
  var nsEncodeBuffer: CodeUnit[] = [0,0,0,0]
  var encodeBuffer: CodeUnit[] = [0,0,0,0]
  var utf32Buffer: UInt32[] = [0]

  println("testing UTF8")
  forAllUnicodeScalars {
    (scalar: UnicodeScalar)
  in
    // Use Cocoa to encode the scalar
    utf32Buffer[0] = scalar.value
    nsEncode(utf32Buffer, Encoding.encodingId(), nsEncodeBuffer, usedRef)
    var nsEncoded = nsEncodeBuffer[0..usedRef[0]]

    var g = GeneratorRef(nsEncoded.enumerate())
    var decoded = Encoding.decode( { g.next() } )
    if decoded! != scalar {
      println("Decoding failed: \(hex(scalar.value)) => \(hex(nsEncoded)) => \(hex(decoded!.value))")
    }
    var encodedLength = 0
    Encoding.encode(scalar) {
      (unit: CodeUnit) in encodeBuffer[encodedLength++] = unit
    }
    var encoded = encodeBuffer[0..encodedLength]
    
    if !equal(nsEncoded, encoded) {
      println("Decoding failed: \(hex(nsEncoded)) => \(hex(scalar.value)) => \(hex(encoded))")
    }
  }
}

// ====================

def testUTF16() {
  var usedRef = [NSUInteger(0)]
  typealias Encoding = UTF16
  typealias CodeUnit = Encoding.CodeUnit
  var nsEncodeBuffer: CodeUnit[] = [0,0,0,0]
  var encodeBuffer: CodeUnit[] = [0,0,0,0]
  var utf32Buffer: UInt32[] = [0]

  println("testing UTF16")
  forAllUnicodeScalars {
    (scalar: UnicodeScalar)
  in
    // Use Cocoa to encode the scalar
    utf32Buffer[0] = scalar.value
    nsEncode(utf32Buffer, Encoding.encodingId(), nsEncodeBuffer, usedRef)
    var nsEncoded = nsEncodeBuffer[0..(usedRef[0]/2)]

    var g = GeneratorRef(nsEncoded.enumerate())
    var decoded = Encoding.decode( { g.next() } )
    if decoded! != scalar {
      println("Decoding failed: \(hex(scalar.value)) => \(hex(nsEncoded)) => \(hex(decoded!.value))")
    }
    var encodedLength = 0
    Encoding.encode(scalar) {
      (unit: CodeUnit) in encodeBuffer[encodedLength++] = unit
    }
    var encoded = encodeBuffer[0..encodedLength]
    
    if !equal(nsEncoded, encoded) {
      println("Encoding failed: \(hex(nsEncoded)) => \(hex(scalar.value)) => \(hex(encoded))")
    }
  }
  println("done.")
}

// CHECK: testing UTF8
// CHECK-NEXT: done.
testUTF8()

// CHECK-NEXT: testing UTF16
// CHECK-NEXT: done.
testUTF16()

