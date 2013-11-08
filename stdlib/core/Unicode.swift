// Conversions between different Unicode encodings.  Note that these
// are *not* currently resilient to erroneous data.

import swift
typealias UnicodeScalar = Char

protocol UnicodeCodec {
  typealias CodeUnit
  
  static def decode(next: ()->CodeUnit?) -> UnicodeScalar?
  static def encode(input: UnicodeScalar, output: (CodeUnit)->())
}

struct UTF8 : UnicodeCodec {

  typealias CodeUnit = UInt8
  
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

struct UTF16 : UnicodeCodec {
  typealias CodeUnit = UInt16
  
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

struct UTF32 : UnicodeCodec {
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

