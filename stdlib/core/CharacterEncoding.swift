// Conversions between different Unicode encodings.  Note that these
// are *not* currently resilient to erroneous data.

import swift
typealias UnicodeScalar = Char


protocol CharacterEncoding {
  typealias CodeUnit

  static def create(value: CodeUnit) -> Self
  def value() -> CodeUnit
  
  static def decode(next: ()->CodeUnit?) -> UnicodeScalar?
  static def encode(input: UnicodeScalar, output: (CodeUnit)->())
}

struct UTF8 : CharacterEncoding {

  typealias CodeUnit = UInt8
  
  static def create(value: CodeUnit) -> UTF8 {
    return UTF8(value)
  }
  
  def value() -> CodeUnit {
    return self._value
  }
  
  static def decode(next: ()->CodeUnit?) -> UnicodeScalar? {
    
    var c = next()
    if !c {
      return .None
    }

    var c0 = c!

    // one octet (7 bits)
    if c0 < 0x80 {
      return Char(UInt32(c0))
    }
    
    var c1 = next()!
    
    // start with octet 1 (we'll mask off high bits later)
    var result = UInt32(c0)
    result = (result << 6) | UInt32(c1 & 0x3F)      // merge octet 2
    if c0 < 0xE0 {
      return UnicodeScalar(result & 0x000007FF)     // 11 bits
    }
    c1 = next()!                                    // prefetch octet 3
    result = (result << 6) | UInt32(c1 & 0x3F)      // merge octet 3
    if c0 < 0xF0 {
      return UnicodeScalar(result & 0x0000FFFF)     // 16 bits
    }
    c1 = next()!                                    // prefetch octet 4
    result = (result << 6) | UInt32(c1 & 0x3F)      // merge octet 4
    return UnicodeScalar(result & 0x001FFFFF)       // 21 bits
  }
  
  static def encode(input: UnicodeScalar, output: (CodeUnit)->()) {
    var c = UInt32(input)
    var buf3 = UInt8(c)
    
    if c >= UInt32(1<<7) {
      c >>= 6
      buf3 = (buf3 & 0x3F) | 0x80 // 10xxxxxx
      var buf2 = UInt8(c)
      if c < UInt32(1<<5) {
        buf2 |= 0xC0              // 110xxxxx
      }
      else {
        c >>= 6
        buf2 = (buf2 & 0x3F) | 0x80 // 10xxxxxx
        var buf1 = UInt8(c)
        if c < UInt32(1<<4) {
          buf1 |= 0xE0              // 1110xxxx
        }
        else {
          c >>= 6
          buf1 = (buf1 & 0x3F) | 0x80 // 10xxxxxx
          output(UInt8(c | 0xF0))     // 11110xxx
        }
        output(buf1)
      }
      output(buf2)
    }
    output(buf3)
  }

  var _value: UInt8
}

struct UTF16 : CharacterEncoding {
  typealias CodeUnit = UInt16
  
  static def create(value: CodeUnit) -> UTF16 {
    return UTF16(value)
  }
  
  def value() -> CodeUnit {
    return self._value
  }
  
  static def decode(next: ()->CodeUnit?) -> UnicodeScalar? {
    var first = next()
    if !first {
      return .None
    }
    
    var unit0 = UInt32(first!)
    if (unit0 >> 11) != 0x1B {
      return UnicodeScalar(unit0)
    }
    
    var unit1 = UInt32(next()!)
    return UnicodeScalar(
      0x10000
      + ((unit0 - 0xD800) << 10)
      + (unit1 - 0xDC00))
  }

  static def encode(input: UnicodeScalar, output: (UInt16)->()) {
    var scalarValue: UInt32 = UInt32(input)
    
    if UInt32(UInt16(scalarValue)) == scalarValue {
      output(UInt16(scalarValue))
    }
    else {
      var lead_offset = UInt32(0xD800) - (0x10000 >> 10)
      output(UInt16(lead_offset + (scalarValue >> 10)))
      output(UInt16(0xDC00 + (scalarValue & 0x3FF)))
    }
  }

  var _value: UInt16
}

struct UTF32 : CharacterEncoding {
  typealias CodeUnit = UInt32
  
  static def create(value: CodeUnit) -> UTF32 {
    return UTF32(value)
  }
  
  def value() -> CodeUnit {
    return self._value
  }
  
  static def decode(next: ()->CodeUnit?) -> UnicodeScalar? {
    var x = next()
    if x {
      return UnicodeScalar(x!)
    }
    return .None
  }

  static def encode(input: UnicodeScalar, output: (UInt32)->()) {
    output(UInt32(input))
  }

  var _value: UInt32
}

struct UnicodeScalars<
  Base: Generator where Base.Element: CharacterEncoding
> : Generator, Enumerable {
  typealias Encoding = Base.Element
  
  init(base: Base) {
    self._base = base
  }
  
  def next() -> UnicodeScalar? {
    return Encoding.decode() {
        var unit = _base.next()
        if !unit {
          return .None
        }
        return unit!.value()
      }
  }

  // Every Generator is a single-pass Enumerable
  def enumerate() -> UnicodeScalars {
    return self
  }

  var _base: Base
}

def unicodeCodeConvert<
Input: Enumerable, OutputEncoding: CharacterEncoding
  where Input.GeneratorType.Element: CharacterEncoding
>(
  input: Input,
  output: (UnicodeScalar)->()
) {
  for scalar in UnicodeScalars(input.enumerate()) {
    output(scalar)
  }
}

// ======= Testing ========

// Use NSString's code conversion capabilities to cross-check
import Foundation

protocol CocoaCharacterEncoding : CharacterEncoding {
  static def encodingId() -> NSStringEncoding
}

extension UTF8 : CocoaCharacterEncoding {
  static def encodingId() -> NSStringEncoding {
    return NSStringEncoding(NSUTF8StringEncoding)
  }
}

extension UTF16 : CocoaCharacterEncoding {
  static def encodingId() -> NSStringEncoding {
    return NSStringEncoding(NSUTF16LittleEndianStringEncoding)
  }
}

extension UTF32 : CocoaCharacterEncoding {
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
// used and c MUST have length >= 1
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

testUTF8()

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
}

testUTF16()

println("done.")
