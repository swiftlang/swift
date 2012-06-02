//===----------------------------------------------------------------------===//
// String Type
//===----------------------------------------------------------------------===//

func [asmname="swift_StringToNSString"]
convertStringToNSString(string : [byref] String) -> id

// FIXME:  How does one write a default constructor in Swift?  The compiler
//   supplied default constructor is breaking the String invariant that the
//   String always contains a valid UTF-8 null terminated string.
//   Instead I've propagated through String that a null value is a valid state
//   (which is wrong).
//   I tried this, but it did not work:
//      static func String() -> String {
//        print("Got Here!\n")
//       }
//   It does not get called.
struct String {
  var str_value : StringByte[]

  static func convertFromStringLiteral(val : Builtin.RawPointer) -> String {
    var isASCII = true
    var s : String
    var Ptr = UnsafePointerInt8(val)
    while Ptr.get() != 0 {
      if StringByte(Ptr.get()) >= 0x80 {
        isASCII = false
      }
      ++Ptr
    }
    s.str_value = new StringByte[Ptr-UnsafePointerInt8(val)]
    Ptr = UnsafePointerInt8(val)
    var i = 0
    var len = s.str_value.getLength()
    while i < len {
      s.str_value[i] = StringByte(Ptr.get())
      ++Ptr
      ++i
    }
    s.str_value.setASCII(isASCII)
    return s
  }

  func byteLength() -> Int {
    return str_value.getLength()
  }

  func [conversion] __conversion() -> NSString {
    var result : NSString
    result.value = convertStringToNSString(&this)
    return result
  }

  struct CharRange : Range {
    var value : String

    typealias Element = Char
    func isEmpty() -> Bool {
      return value.isEmpty()
    }
    func getFirstAndAdvance() -> Char {
      // string ranges do not end in the middle of a Char
      // one range check is sufficient.
      if isEmpty() {
        Builtin.trap()
      }
      var u8 = StringByte(value.str_value.base.get())
      if u8 < 0x80 {
        value.str_value.base += 1
        value.str_value.setLength(value.str_value.getLength() - 1)
        return Char(UInt32(u8))
      }
     
      return _getFirstAndAdvancedSlow(u8) 
    }
    
    func /*[noinline]*/ _getFirstAndAdvancedSlow(u8 : StringByte) -> Char {
      var u8_1 = StringByte((value.str_value.base + 1).get())
      if u8 < 0xE0 {
        value.str_value.base += 2
        value.str_value.setLength(value.str_value.getLength() - 2)
        return Char((UInt32(u8 & 0x1F) << 6) |
                     UInt32(u8_1 & 0x3F))
      }
      var u8_2 = StringByte((value.str_value.base + 2).get())
      if u8 < 0xF0 {
        value.str_value.base += 3
        value.str_value.setLength(value.str_value.getLength() - 3)
        return Char((UInt32(u8   & 0x0F) << 12) |
                    (UInt32(u8_1 & 0x3F) << 6)  |
                     UInt32(u8_2 & 0x3F))
      }
      var u8_3 = StringByte((value.str_value.base + 3).get())
      value.str_value.base += 4
      value.str_value.setLength(value.str_value.getLength() - 4)
      return Char((UInt32(u8   & 0x07) << 18) |
                  (UInt32(u8_1 & 0x3F) << 12) |
                  (UInt32(u8_2 & 0x3F) << 6)  |
                   UInt32(u8_3 & 0x3F))
    }
  }

  func getElements() -> CharRange {
    return CharRange(this)
  }

  func size() -> Int {
    if str_value.isASCII() {
      return byteLength()
    }
    var Result = 0
    for i in this {
      ++Result
    }
    return Result
  }

  func isEmpty() -> Bool {
    return byteLength() == 0
  }

  subscript (rng : IntRange) -> String {
    get {
      var len = rng.max - rng.min
      if str_value.isASCII() {
        if len >= byteLength() {
          Builtin.trap()
        }
        return String(SliceStringByte.convertFromHeapArray(
                      (str_value.base + rng.min).value,
                      str_value.owner, len.value))
      }
      return _subscriptNonASCII(rng)
    }
  }

  func _subscriptNonASCII (rng : IntRange) -> String {
    var len = rng.max - rng.min
    var start = rng.min
    var idx = 0
    while start > 0 {
      var tmp : StringByte = str_value[idx]
      if tmp < 0x80 {
        --start
      } else if tmp >= 0xC0 {
        --start
      }
      ++idx
    }
    var oldidx = idx;
    while len > 0 {
      var tmp = str_value[idx]
      if tmp < 0x80 {
        --len
      } else if tmp >= 0xC0 {
        --len
      }
      ++idx
    }
    return String(SliceStringByte.convertFromHeapArray(
                    (str_value.base + oldidx).value,
                    str_value.owner, (idx - oldidx).value))
  }

  subscript (idx : Int) -> Char {
    get {
      if str_value.isASCII() {
        if idx < byteLength() {
          return Char(UInt32((str_value.base + idx).get()))
        }
      }
      return subscriptNonASCII(idx)
    }
  }

  func subscriptNonASCII(idx : Int) -> Char {
    for c in this {
      if idx == 0 {
        return c;
      }
      --idx
    }
    Builtin.trap()
  }

  func replPrint() {
    print('"')
    for c in this {
      c.replPrintCharBody()
    }
    print('"')
  }
  
  // FIXME: Locales make this interesting
  func toUpper() -> String {
    var len = byteLength()
    var resultArray = new StringByte[len]
    var i : Int
    while i != len {
      var u8 = str_value[i]
      if u8 < 0x80 {
        if u8 >= 97 && u8 <= 122 {
          resultArray[i] = u8 - 32
        } else {
          resultArray[i] = u8
        }
        ++i
      } else if u8 < 0xE0 {
        resultArray[i] = u8
        var u8_1 = str_value[i + 1]
        if u8 == 0xC3 && u8_1 > 0xA0 && u8_1 < 0xBF && u8_1 != 0xB7 {
          resultArray[i+1] = u8_1 - 0x20
        } else {
          resultArray[i+1] = u8_1
        }
        i += 2
      } else if u8 < 0xF0 {
        resultArray[i] = u8
        resultArray[i+1] = str_value[i+1]
        resultArray[i+2] = str_value[i+2]
        i += 3
      } else {
        resultArray[i] = u8
        resultArray[i+1] = str_value[i+1]
        resultArray[i+2] = str_value[i+2]
        resultArray[i+3] = str_value[i+3]
        i += 4
      }
    }
    
    var result : String
    result.str_value = resultArray
    return result
  }
  
  // FIXME: Locales make this interesting
  func toLower() -> String {
    var len = byteLength()
    var resultArray = new StringByte[len]
    var i : Int
    while i != len {
      var u8 = str_value[i]
      if u8 < 0x80 {
        if u8 >= 65 && u8 <= 90 {
          resultArray[i] = u8 + 32
        } else {
          resultArray[i] = u8
        }
        ++i
      } else if u8 < 0xE0 {
        resultArray[i] = u8
        var u8_1 = str_value[i + 1]
        if u8 == 0xC3 && u8_1 > 0x80 && u8_1 < 0x9F && u8_1 != 0x97 {
          resultArray[i+1] = u8_1 + 0x20
        } else {
          resultArray[i+1] = u8_1
        }
        i += 2
      } else if u8 < 0xF0 {
        resultArray[i] = u8
        resultArray[i+1] = str_value[i+1]
        resultArray[i+2] = str_value[i+2]
        i += 3
      } else {
        resultArray[i] = u8
        resultArray[i+1] = str_value[i+1]
        resultArray[i+2] = str_value[i+2]
        resultArray[i+3] = str_value[i+3]
        i += 4
      }
    }
    
    var result : String
    result.str_value = resultArray
    return result
  }

  static func String(c : Char) -> String {
    var buf : StringByte[] = new StringByte[4]
    var len = 0
    if c <= Char(0x7F) {
      buf[0] = StringByte(UInt32(c))
      len = 1
    } else {
      var high = StringByte(((UInt32(c) >> 18)       ) | 0xF0)
      var midh = StringByte(((UInt32(c) >> 12) & 0x3F) | 0x80)
      var midl = StringByte(((UInt32(c) >>  6) & 0x3F) | 0x80)
      var low  = StringByte(((UInt32(c)      ) & 0x3F) | 0x80)
      if c <= Char(0x07FF) {
        buf[0] = midl | 0x40
        buf[1] = low
        len = 2
      } else if c <= Char(0xFFFF) {
        buf[0] = midh | 0x60
        buf[1] = midl
        buf[2] = low
        len = 3
      } else if c <= Char(0x1FFFFF) {
        buf[0] = high
        buf[1] = midh
        buf[2] = midl
        buf[3] = low
        len = 4
      } else {
        Builtin.trap()
      }
    }
    return String(SliceStringByte.convertFromHeapArray(buf.base.value, buf.owner, len.value))
  }
}

extension String : FormattedPrintable {
  func printFormatted(kind : Char, layout : String) {
    var toPrint = this
    if kind == 'u' { toPrint = toUpper() }
    else if kind == 'l' { toPrint = toLower() }
    Format(layout).printString(toPrint)
  }
}

// StringLiteralType specifies the default type to use for an string literal
// when the type isn't constrained.
typealias StringLiteralType = String


// Concatenation.
func [infix_left=190]+(lhs : String, rhs : String) -> String {
  var l_length = lhs.byteLength()
  var r_length = rhs.byteLength()
  var buf : StringByte[] = new StringByte[l_length + r_length]
  var i = 0
  for var j = 0; j < l_length; ++j {
    buf[i] = lhs.str_value[j]
    ++i
  }
  for var j = 0; j < r_length; ++j {
    buf[i] = rhs.str_value[j]
    ++i
  }
  var s = String(buf)
  s.str_value.setASCII(lhs.str_value.isASCII() && rhs.str_value.isASCII())
  return s
}
func [infix_left=190]+(lhs : String, rhs : Char) -> String {
  return lhs + String(rhs)
}
func [infix_left=190]+(lhs : Char, rhs : String) -> String {
  return String(lhs) + rhs
}
func [infix_left=190]+(lhs : Char, rhs : Char) -> String {
  return String(lhs) + String(rhs)
}


// String append
func [assignment,infix_left=90] += (lhs : [byref] String, rhs : String) {
  lhs = lhs + rhs
}
func [assignment,infix_left=90] += (lhs : [byref] String, rhs : Char) {
  lhs = lhs + rhs
}


// Comparison operators

func [infix=160] == (lhs : String, rhs : String) -> Bool {
  if lhs.str_value.getLength() != rhs.str_value.getLength() {
    return false
  }
  var len = lhs.byteLength();
  for var i = 0; i < len; ++i {
     if lhs.str_value[i] != rhs.str_value[i] {
       return false
     }
  }
  return true
}

func [infix=160] != (lhs : String, rhs : String) -> Bool {
  return !(lhs == rhs)
}

func [infix=170] < (lhs : String, rhs : String) -> Bool {
  var i = 0
  for c in rhs.str_value {
    if i == lhs.byteLength() || lhs.str_value[i] < c {
      return true
    }
    if c < lhs.str_value[i] {
      return false
    }
    ++i
  }
  return false
}

func [infix=170] > (lhs : String, rhs : String) -> Bool {
  return rhs < lhs
}

func [infix=170] <= (lhs : String, rhs : String) -> Bool {
  return !(rhs < lhs)
}

func [infix=170] >= (lhs : String, rhs : String) -> Bool {
  return !(lhs < rhs)
}

// Conversions to string from other types.
extension String {

  static func String(v : Int128, radix : Int = 10) -> String {
    var Buffer = new StringByte[128]
    var i = c_print_int(Buffer.base.value, 128, v, radix)
    var b = Buffer[0..Int(i)]
    return String(b)
  }

  static func String(v : Int8, radix : Int = 10) -> String {
    return String(Int128(v), radix)
  }
  static func String(v : UInt8, radix : Int = 10) -> String {
    return String(Int128(v), radix)
  }
  static func String(v : Int16, radix : Int = 10) -> String {
    return String(Int128(v), radix)
  }
  static func String(v : Int32, radix : Int = 10) -> String {
    return String(Int128(v), radix)
  }
  static func String(v : UInt32, radix : Int = 10) -> String {
    return String(Int128(v), radix)
  }
  static func String(v : Int64, radix : Int = 10) -> String {
    return String(Int128(v), radix)
  }
  static func String(v : UInt64, radix : Int = 10) -> String {
    return String(Int128(v), radix)
  }

  static func String(v : Double) -> String {
    var Buffer = new StringByte[256]
    var i = c_print_double(Buffer.base.value, v)
    return String(Buffer[0..Int(i)])
  }

  static func String(v : Float) -> String { return String(Double(v)) }
  static func String(b : Bool) -> String {
    if b {
      return "true"
    }
    return "false"
  }
}

extension String {
  /// \brief Produce a substring of the given string from the given character
  /// index to the end of the string.
  func substr(start : Int) -> String {
    var rng = getElements()
    while start > 0 {
      rng.getFirstAndAdvance()
      --start
    }
    var result = this
    var bytes = rng.value.str_value.base - str_value.base
    result.str_value.base += bytes
    result.str_value.setLength(result.str_value.getLength() - bytes)
    return result
  }

  /// \brief Split the given string at the given delimiter character, returning 
  /// the strings before and after that character (neither includes the character
  /// found) and a boolean value indicating whether the delimiter was found.
  // 
  func splitFirst(delim : Char) 
    -> (before: String, after: String, wasFound : Bool) 
  {
    var rng = getElements()
    while !rng.isEmpty() {
      var rngBefore = rng
      if rng.getFirstAndAdvance() == delim {
        var before = this
        before.str_value.setLength(rngBefore.value.str_value.base - str_value.base)
        before.str_value.setASCII(str_value.isASCII())

        var after = this
        after.str_value.base += rng.value.str_value.base - str_value.base
        after.str_value.setLength(after.str_value.getLength() -
                                  (rng.value.str_value.base - str_value.base))
        return (before, after, true)
      }
    }
    return (this, "", false)
  }

  /// \brief Split the given string at the first character for which the given
  /// predicate returns true. Returns the string before that character, the 
  /// character that matches, the string after that character, and a boolean value
  /// indicating whether any character was found.
  func splitFirstIf(pred : (Char) -> Bool) 
    -> (before: String, found: Char, after : String, wasFound: Bool) 
  {
    var rng = getElements()
    while !rng.isEmpty() {
      var rngBefore = rng
      var c = rng.getFirstAndAdvance()
      if pred(c) {
        var before = this
        before.str_value.setLength(rngBefore.value.str_value.base -
                                   str_value.base)
        before.str_value.setASCII(str_value.isASCII())

        var after = this
        after.str_value.base += rng.value.str_value.base - str_value.base
        after.str_value.setLength(after.str_value.getLength() -
                                  (rng.value.str_value.base - str_value.base))
        return (before, c, after, true)
      }
    }
    return (this, Char(0), "", false)
  }
}
