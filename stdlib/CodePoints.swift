typealias CodePoint = Char

struct CodePoints {
  typealias Element = CodePoint

  constructor(s: String) {
    this.str_value = s.str_value
  }

  func first() -> Char {
    var p = str_value.base

    // one octet (7 bits)
    var c0 : UInt8 = p.get()

    // We know we have a null-terminated string, so there's at least
    // one more byte.  Let the CPU scheduler know that its fetch isn't
    // dependent on the results of the upcoming branch by starting the
    // fetch now
    ++p
    var c1 = p.get()

    if c0 < 0x80 {
      return Char(UInt32(c0))
    }
    
    // start with octet 1 (we'll mask off high bits later)
    var result = UInt32(c0)

    // merge octet 2
    result = (result << 6) | UInt32(c1 & 0x3F)

    // prefetch octet 3
    ++p
    c1 = p.get()

    if c0 < 0xE0 {
      return CodePoint(result & 0x000007FF) // 11 bits
    }

    // merge octet 3
    result = (result << 6) | UInt32(c1 & 0x3F)

    // prefetch octet 4
    ++p
    c1 = p.get()

    if c0 < 0xF0 {
      return CodePoint(result & 0x0000FFFF) // 16 bits
    }

    // merge octet 4
    result = (result << 6) | UInt32(c1 & 0x3F)
    return CodePoint(result & 0x001FFFFF) // 21 bits
  }

  var str_value : StringByteData
}

// 1 byte
var s7  = "\u007F"      //                     0000 0000 0111 1111
assert(CodePoints(s7).first() == Char(0x007F))

// 2 bytes
var s8  = "\u00A6"      //                     0000 0000 1010 0110
assert(CodePoints(s8).first() == Char(0x00A6))
var s11 = "\u0521"      //                     0000 0101 0010 0001
assert(CodePoints(s11).first() == Char(0x0521))

// 3 bytes
var s12 = "\u0D6F"      //                     0000 1101 0110 1111
assert(CodePoints(s12).first() == Char(0x0D6F))
var s16 = "\uB977"      //                     1011 1001 0111 0111
assert(CodePoints(s16).first() == Char(0xB977))

// 4 bytes
var s17 = "\U0001B9C7"  // 0000 0000 0000 0001 1011 1001 1100 0111
assert(CodePoints(s17).first() == Char(0x1B9C7))
var s21 = "\U0010B977"  // 0000 0000 0001 0000 1011 1001 0111 0111
assert(CodePoints(s21).first() == Char(0x10B977))

println("done.")
