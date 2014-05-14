//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//


// Conversions between different Unicode encodings.  Note that these
// are *not* currently resilient to erroneous data.

protocol UnicodeCodec {
  typealias CodeUnit
  
  class func decode<G : Generator where G.Element == CodeUnit>(inout next:G) -> UnicodeScalar?
  class func encode<S : Sink where S.Element == CodeUnit>(input: UnicodeScalar,inout output: S)
}

struct UTF8 : UnicodeCodec {

  typealias CodeUnit = UInt8
  
  static func decode<G : Generator where G.Element == CodeUnit>(inout next:G) -> UnicodeScalar? {
    
    var c = next.next()
    if !c {
      return .None
    }

    var c0 = c!

    // one octet (7 bits)
    if c0 < 0x80 {
      return UnicodeScalar(UInt32(c0))
    }
    
    var c1 = next.next()!
    
    // start with octet 1 (we'll mask off high bits later)
    var result = UInt32(c0)
    result = (result << 6) | UInt32(c1 & 0x3F)      // merge octet 2
    if c0 < 0xE0 {
      return UnicodeScalar(result & 0x000007FF)     // 11 bits
    }
    c1 = next.next()!                                    // prefetch octet 3
    result = (result << 6) | UInt32(c1 & 0x3F)      // merge octet 3
    if c0 < 0xF0 {
      return UnicodeScalar(result & 0x0000FFFF)     // 16 bits
    }
    c1 = next.next()!                                    // prefetch octet 4
    result = (result << 6) | UInt32(c1 & 0x3F)      // merge octet 4
    return UnicodeScalar(result & 0x001FFFFF)       // 21 bits
  }
  
  static func encode<S : Sink where S.Element == CodeUnit>(
    input: UnicodeScalar,inout output: S) {
    var c = UInt32(input)
    var buf3 = UInt8(c & 0xFF)
    
    if c >= UInt32(1<<7) {
      c >>= 6
      buf3 = (buf3 & 0x3F) | 0x80 // 10xxxxxx
      var buf2 = UInt8(c & 0xFF)
      if c < UInt32(1<<5) {
        buf2 |= 0xC0              // 110xxxxx
      }
      else {
        c >>= 6
        buf2 = (buf2 & 0x3F) | 0x80 // 10xxxxxx
        var buf1 = UInt8(c & 0xFF)
        if c < UInt32(1<<4) {
          buf1 |= 0xE0              // 1110xxxx
        }
        else {
          c >>= 6
          buf1 = (buf1 & 0x3F) | 0x80 // 10xxxxxx
          output.put(UInt8(c | 0xF0)) // 11110xxx
        }
        output.put(buf1)
      }
      output.put(buf2)
    }
    output.put(buf3)
  }

  var _value =  UInt8()
}

struct UTF16 : UnicodeCodec {
  typealias CodeUnit = UInt16
  
  static func decode<G : Generator where G.Element == CodeUnit>(inout input:G) -> UnicodeScalar? {
    let first = input.next()
    if !first {
      return .None
    }
    
    let unit0 = UInt32(first!)
    if (unit0 >> 11) != 0x1B {
      return UnicodeScalar(unit0)
    }
    
    let unit1 = UInt32(input.next()!)

    // FIXME: Uglified due to type checker performance issues.
    var result : UInt32 = 0x10000
    result += ((unit0 - 0xD800) << 10)
    result += (unit1 - 0xDC00)
    return UnicodeScalar(result)
  }

  static func encode<S : Sink where S.Element == CodeUnit>(
    input: UnicodeScalar,inout output: S) {
    var scalarValue: UInt32 = UInt32(input)
    
    if scalarValue <= UInt32(UInt16.max) {
      output.put(UInt16(scalarValue))
    }
    else {
      var lead_offset = UInt32(0xD800) - (0x10000 >> 10)
      output.put(UInt16(lead_offset + (scalarValue >> 10)))
      output.put(UInt16(0xDC00 + (scalarValue & 0x3FF)))
    }
  }

  var _value = UInt16()
}

struct UTF32 : UnicodeCodec {
  typealias CodeUnit = UInt32
  
  init(_ _value: UInt32) {
    self._value = _value
  }

  static func create(value: CodeUnit) -> UTF32 {
    return UTF32(value)
  }
  
  func value() -> CodeUnit {
    return self._value
  }
  
  static func decode<G : Generator where G.Element == CodeUnit>(
    inout input:G) -> UnicodeScalar? {
    var x = input.next()
    if x {
      return UnicodeScalar(x!)
    }
    return .None
  }

  static func encode<S : Sink where S.Element == CodeUnit>(
    input: UnicodeScalar,inout output: S) {
    output.put(UInt32(input))
  }

  var _value = UInt32()
}

func transcode<
  Input: Generator,
Output: Sink,
  InputEncoding: UnicodeCodec,
  OutputEncoding: UnicodeCodec
where InputEncoding.CodeUnit == Input.Element,
       OutputEncoding.CodeUnit == Output.Element> (
  inputEncoding: InputEncoding.Type, outputEncoding: OutputEncoding.Type,
  var input: Input, var output: Output
) {
  for var scalar = InputEncoding.decode(&input);
          scalar;
          scalar = InputEncoding.decode(&input)
  {
    var s = scalar!
    OutputEncoding.encode(s, output: &output)
  }
}

// Trivial transcoder; I'm hoping this will kick in for common cases
func transcode<
  Input: Generator,
  Output: Sink,
  Encoding: UnicodeCodec
    where Encoding.CodeUnit == Input.Element, Encoding.CodeUnit == Output.Element
> (
  inputEncoding: Encoding.Type, outputEncoding: Encoding.Type,
  var input: Input, var output: Output
) {
  while true {
    var x = input.next()
    if !x {
      break
    }
    output.put(x!)
  }
}

protocol StringElement {
  class func toUTF16CodeUnit(_: Self) -> UTF16.CodeUnit
  class func fromUTF16CodeUnit(utf16: UTF16.CodeUnit) -> Self
}

extension UTF16.CodeUnit : StringElement {
  static func toUTF16CodeUnit(x: UTF16.CodeUnit) -> UTF16.CodeUnit {
    return x
  }
  static func fromUTF16CodeUnit(utf16: UTF16.CodeUnit) -> UTF16.CodeUnit {
    return utf16
  }
}

extension UTF8.CodeUnit : StringElement {
  static func toUTF16CodeUnit(x: UTF8.CodeUnit) -> UTF16.CodeUnit {
    return UTF16.CodeUnit(x)
  }
  static func fromUTF16CodeUnit(utf16: UTF16.CodeUnit) -> UTF8.CodeUnit {
    return UTF8.CodeUnit(utf16)
  }
}

extension UTF16 {
  static func width(x: UnicodeScalar) -> Int {
    return x.value <= 0xFFFF ? 1 : 2
  }

  static func leadSurrogate(x: UnicodeScalar) -> UTF16.CodeUnit {
    assert(width(x) == 2)
    return (UTF16.CodeUnit(x.value - 0x1_0000) >> 10) + 0xD800
  }
  
  static func trailSurrogate(x: UnicodeScalar) -> UTF16.CodeUnit {
    assert(width(x) == 2)
    return (UTF16.CodeUnit(x.value - 0x1_0000) & ((1 << 10) - 1)) + 0xDC00
  }

  static func copy<T: StringElement, U: StringElement>(
    source: UnsafePointer<T>, destination: UnsafePointer<U>, count: Int
  ) {
    if UWord(Builtin.strideof(T.self)) == UWord(Builtin.strideof(U.self)) {
      c_memcpy(
        dest: UnsafePointer(destination),
        src: UnsafePointer(source),
        size: UInt(count) * UInt(Builtin.strideof(U.self)))
    }
    else {
      for i in 0..count {
        let u16 = T.toUTF16CodeUnit((source + i).get())
        (destination + i).set(U.fromUTF16CodeUnit(u16))
      }
    }
  }

  static func measure<Encoding: UnicodeCodec, Input: Generator where Encoding.CodeUnit == Input.Element>(
    _: Encoding.Type, var input: Input
  ) -> (Int, Bool) {
    var count = 0
    var isAscii = true
    while true {
      if let x = Encoding.decode(&input) {
        if x.value > 0x7f {
          isAscii = false
        }
        count += width(x)
      }
      else {
        break
      }
    }
    return (count, isAscii)
  }
}
