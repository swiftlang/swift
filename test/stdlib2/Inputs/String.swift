//===----------------------------------------------------------------------===//
// String Type
//===----------------------------------------------------------------------===//

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
struct String : Enumerable, Comparable, Hashable {
  var str_value : StringByteData

  static func convertFromStringLiteral(val : Builtin.RawPointer,
                                       byteSize : Builtin.Int64) -> String {
    var s : String
    var owner : Builtin.ObjectPointer
    s.str_value = StringByteData.convertFromHeapArray(val, owner, byteSize)
    s.str_value.setASCII(false)
    s.str_value.setCString(true)
    return s
  }

  static func convertFromASCIIStringLiteral(val : Builtin.RawPointer,
                                          byteSize : Builtin.Int64) -> String {
    var s : String
    var owner : Builtin.ObjectPointer
    s.str_value = StringByteData.convertFromHeapArray(val, owner, byteSize)
    s.str_value.setASCII(true)
    s.str_value.setCString(true)
    return s
  }

  constructor(sz : Int, c : Char) {
    var s = String(c)
    str_value = StringByteData.getNew(sz*s.byteLength())
    var k = 0
    for i in 0..sz {
      for j in 0..s.byteLength() {
        str_value[k] = s.str_value[j]
        ++k
      }
    }
  }

  func asUInt8() -> UInt8[] {
    var r : UInt8[]
    r.base = str_value.base
    r.length = str_value.length
    r.owner = str_value.owner
    return r
  }

  func byteLength() -> Int {
    return str_value.getLength()
  }

  struct CharEnumeratorType : Enumerator {
    var value : String

    typealias Element = Char
    func isEmpty() -> Bool {
      return value.isEmpty()
    }
    func next() -> Char {
      // string ranges do not end in the middle of a Char
      // one range check is sufficient.
      debugTrap(!isEmpty())

      var u8 = StringByte(value.str_value.base.get())
      if u8 < 0x80 {
        value.str_value.base += 1
        value.str_value.setLength(value.str_value.getLength() - 1)
        return Char(UInt32(u8))
      }

      return _nextSlow(u8)
    }

    func /*[noinline]*/ _nextSlow(u8 : StringByte) -> Char {
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

  typealias EnumeratorType = CharEnumeratorType
  func getEnumeratorType() -> CharEnumeratorType {
    return CharEnumeratorType(this)
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

  var length : Int {
    get { return size() }
  }

  func isEmpty() -> Bool {
    return byteLength() == 0
  }

  subscript (rng : IntEnumeratorType) -> String {
    get {
      var len = rng.max - rng.min
      if str_value.isASCII() {
        debugTrap(UInt(len) <= UInt(byteLength()))
        return String(StringByteData.convertFromHeapArray(
                      (str_value.base + rng.min).value,
                      str_value.owner, len.value))
      }
      return _subscriptNonASCII(rng)
    }
  }

  func _subscriptNonASCII (rng : IntEnumeratorType) -> String {
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
    var oldidx = idx
    while len > 0 {
      var tmp = str_value[idx]
      if tmp < 0x80 {
        --len
      } else if tmp >= 0xC0 {
        --len
      }
      ++idx
    }
    return String(StringByteData.convertFromHeapArray(
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
        return c
      }
      --idx
    }
    alwaysTrap()
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
    var resultArray = StringByteData.getNew(len)
    var i : Int
    while i != len {
      var u8 = str_value[i]
      if u8 < 0x80 {
        if (97 .. 123).contains(Int(u8)) {
          resultArray[i] = u8 - 32
        } else {
          resultArray[i] = u8
        }
        ++i
      } else if u8 < 0xE0 {
        resultArray[i] = u8
        var u8_1 = str_value[i + 1]
        if u8 == 0xC3 && (0xA0 .. 0xBF).contains(Int(u8_1)) && u8_1 != 0xB7 {
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
    var resultArray = StringByteData.getNew(len)
    var i : Int
    while i != len {
      var u8 = str_value[i]
      if u8 < 0x80 {
        if (65 .. 91).contains(Int(u8)) {
          resultArray[i] = u8 + 32
        } else {
          resultArray[i] = u8
        }
        ++i
      } else if u8 < 0xE0 {
        resultArray[i] = u8
        var u8_1 = str_value[i + 1]
        if u8 == 0xC3 && (0x80 .. 0x9F).contains(Int(u8_1)) && u8_1 != 0x97 {
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

  constructor(c : Char) {
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
        alwaysTrap()
      }
    }
    str_value = StringByteData.convertFromHeapArray(buf.base.value, buf.owner, len.value)
  }

  // Predicates

  // FIXME: Replace this with a generic isAll().
  func _isAll(predicate : (Char) -> Bool) -> Bool {
    for c in this { if !predicate(c) { return false } }

    return true
  }

  func startsWith(prefix : String) -> Bool {
    if prefix.size() > size() { return false }

    return this[0..prefix.size()] == prefix
  }

  func isAlpha() -> Bool { return _isAll({ $0.isAlpha() }) }
  func isDigit() -> Bool { return _isAll({ $0.isDigit() }) }
  func isSpace() -> Bool { return _isAll({ $0.isSpace() }) }
}

extension String : FormattedPrintable {
  func format(kind : Char, layout : String) -> String {
    var toPrint = this
    if kind == 'u' { toPrint = toUpper() }
    else if kind == 'l' { toPrint = toLower() }
    return Format(layout).printToString(toPrint)
  }
}

// StringLiteralType specifies the default type to use for an string literal
// when the type isn't constrained.
typealias StringLiteralType = String


// Concatenation.
func [infix_left=190]+(lhs : String, rhs : String) -> String {
  var l_length = lhs.byteLength()
  var r_length = rhs.byteLength()
  var buf = StringByteData.getNew(l_length + r_length)
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
  s.str_value.setCString(false) // XXX FIXME -- this should be true
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
  var len = lhs.byteLength()
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
  constructor(v : Int128, radix : Int = 10, uppercase : Bool = false) {
    var Buffer = StringByteData.getNew(128)
    var i = c_print_int(Buffer.base.value, 128, v, radix, uppercase)
    str_value = Buffer[0..Int(i)]
  }

  constructor(v : UInt128, radix : Int = 10, uppercase : Bool = false) {
    var Buffer = StringByteData.getNew(128)
    var i = c_print_uint(Buffer.base.value, 128, v, radix, uppercase)
    str_value = Buffer[0..Int(i)]
  }

  constructor(v : Int8, radix : Int = 10, uppercase : Bool = false) {
    this = String(Int128(v), radix, uppercase)
  }
  constructor(v : Int16, radix : Int = 10, uppercase : Bool = false) {
    this = String(Int128(v), radix, uppercase)
  }
  constructor(v : Int32, radix : Int = 10, uppercase : Bool = false) {
    this = String(Int128(v), radix, uppercase)
  }
  constructor(v : Int64, radix : Int = 10, uppercase : Bool = false) {
    this = String(Int128(v), radix, uppercase)
  }
  constructor(v : UInt8, radix : Int = 10, uppercase : Bool = false) {
    this = String(UInt128(v), radix, uppercase)
  }
  constructor(v : UInt16, radix : Int = 10, uppercase : Bool = false) {
    this = String(UInt128(v), radix, uppercase)
  }
  constructor(v : UInt32, radix : Int = 10, uppercase : Bool = false) {
    this = String(UInt128(v), radix, uppercase)
  }
  constructor(v : UInt64, radix : Int = 10, uppercase : Bool = false) {
    this = String(UInt128(v), radix, uppercase)
  }

  constructor(v : Double) {
    var Buffer = StringByteData.getNew(256)
    var i = c_print_double(Buffer.base.value, v)
    this = String(Buffer[0..Int(i)])
  }
  constructor(v : Float) {
    this = String(Double(v))
  }

  constructor(b : Bool) {
    if b {
      this = "true"
    } else {
      this = "false"
    }
  }
}

extension String {
  /// \brief Produce a substring of the given string from the given character
  /// index to the end of the string.
  func substr(start : Int) -> String {
    var rng = getEnumeratorType()
    while start > 0 {
      rng.next()
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
  func splitFirst(delim : Char)
    -> (before: String, after: String, wasFound : Bool)
  {
    var rng = getEnumeratorType()
    while !rng.isEmpty() {
      var rngBefore = rng
      if rng.next() == delim {
        var before = this
        before.str_value.setLength(rngBefore.value.str_value.base - str_value.base)
        before.str_value.setASCII(str_value.isASCII())
        before.str_value.setCString(false)

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
    var rng = getEnumeratorType()
    while !rng.isEmpty() {
      var rngBefore = rng
      var c = rng.next()
      if pred(c) {
        var before = this
        before.str_value.setLength(rngBefore.value.str_value.base -
                                   str_value.base)
        before.str_value.setASCII(str_value.isASCII())
        before.str_value.setCString(false)

        var after = this
        after.str_value.base += rng.value.str_value.base - str_value.base
        after.str_value.setLength(after.str_value.getLength() -
                                  (rng.value.str_value.base - str_value.base))
        return (before, c, after, true)
      }
    }
    return (this, Char(0), "", false)
  }

  /// \brief Split the given string at each occurrence of the
  /// delimiter character, returning an array of strings that
  /// before/between/after the delimiters.
  func split(arg : Char) -> String[] {
    // Create a vector to hold the result.
    var lines = new Vector<String>
    var contents = this
    while true {
      var split = contents.splitFirst(arg)
      lines.append(split.before)
      if !split.wasFound {
        break
      }
      contents = split.after
    }

    // Convert the vector to an array.
    //
    // FIXME: Remove this.
    var linesArray = new String[lines.length]
    for i in 0..lines.length {
      linesArray[i] = lines[i]
    }

    return linesArray
  }

  /// \brief Split the given string at each occurrence of a character for which
  /// the given predicate evaluates true, returning an array of strings that
  /// before/between/after those delimiters.
  func splitIf(pred : (Char) -> Bool) -> String[] {
    // Create a vector to hold the result.
    var lines = new Vector<String>
    var contents = this
    while true {
      var split = contents.splitFirstIf(pred)
      lines.append(split.before)
      if !split.wasFound {
        break
      }
      contents = split.after
    }

    // Convert the vector to an array.
    //
    // FIXME: Remove this.
    var linesArray = new String[lines.length]
    for i in 0..lines.length {
      linesArray[i] = lines[i]
    }

    return linesArray
  }
}

extension String {
  func hash() -> UInt {
    var r : UInt = 5381
    var len = byteLength();
    for var i = 0; i < len; ++i {
      r = ((r << 5) + r) + UInt(str_value[i])
    }
    return r
  }
}

//===----------------------------------------------------------------------===//
// Char Type
//===----------------------------------------------------------------------===//

struct Char : Comparable {
  var value : Builtin.Int32

  static func convertFromCharacterLiteral(val : Builtin.Int32) -> Char {
    return Char(val)
  }

  constructor(v : UInt32) {
    // FIXME:  Give Char an invariant that it can only hold valid UTF-32
    value = v.value
  }

  func replPrint() {
    print('\'')
    replPrintCharBody()
    print('\'')
  }

  func replPrintCharBody() {
    if this == '\\' {
      print('\\')
      print('\\')
    } else if this == '"' {
      print('\\')
      print('"')
    } else if this == '\'' {
      print('\\')
      print('\'')
    } else if isPrint() {
      print(this)
    } else if this == '\t' {
      print('\\')
      print('t')
    } else if this == '\n' {
      print('\\')
      print('n')
    } else if this == '\r' {
      print('\\')
      print('r')
    } else if UInt32(this) < 128 {
      print('\\')
      print('x')
      _printNibbleAsHex(UInt32(this) >> 4)
      _printNibbleAsHex(UInt32(this))
    } else if _isUTF8() {
      print(this)
    } else if UInt32(this) <= 0xFFFF {
      print('\\')
      print('u')
      _printNibbleAsHex(UInt32(this) >> 12)
      _printNibbleAsHex(UInt32(this) >> 8)
      _printNibbleAsHex(UInt32(this) >> 4)
      _printNibbleAsHex(UInt32(this))
    } else {
      print('\\')
      print('U')
      _printNibbleAsHex(UInt32(this) >> 28)
      _printNibbleAsHex(UInt32(this) >> 24)
      _printNibbleAsHex(UInt32(this) >> 20)
      _printNibbleAsHex(UInt32(this) >> 16)
      _printNibbleAsHex(UInt32(this) >> 12)
      _printNibbleAsHex(UInt32(this) >> 8)
      _printNibbleAsHex(UInt32(this) >> 4)
      _printNibbleAsHex(UInt32(this))
    }
  }

    // FIXME: Move to nested function when IRGen supports it.
  static func _printNibbleAsHex(v : UInt32) {
    v = v & 15
    if v < 10 {
      print(Char(v+48))    // 48 = '0'
    } else {
      print(Char(v-10+65)) // 65 = 'A'
    }
  }

  // FIXME: Locales make this interesting
  func isAlpha() -> Bool {
    return (this >= 'A' && this <= 'Z') || (this >= 'a' && this <= 'z')
  }

  // FIXME: Locales make this interesting
  func isDigit() -> Bool {
    return this >= '0' && this <= '9'
  }

  // FIXME: Locales make this interesting
  func toUpper() -> Char {
    if this >= 'a' && this <= 'z' {
      return Char(UInt32(this) - 32)
    } else if this >= 'à' && this <= 'þ' && this != '÷' {
      return Char(UInt32(this) - 32)
    }
    return this
  }

  // FIXME: Locales make this interesting
  func toLower() -> Char {
    if this >= 'A' && this <= 'Z' {
      return Char(UInt32(this) + 32)
    } else if this >= 'À' && this <= 'Þ' && this != '×' {
      return Char(UInt32(this) + 32)
    }
    return this
  }

  // FIXME: Locales make this interesting.
  func isSpace() -> Bool {
    // FIXME: The constraint-based type checker goes painfully exponential
    // when we turn this into one large expression. Break it up for now,
    // until we can optimize the constraint solver better.
    if this == ' '  || this == '\t' { return true }
    if this == '\n' || this == '\r' { return true }
    return this == '\x0B' || this == '\x0C'
  }
}

extension Char : FormattedPrintable {
  func format(kind : Char, layout : String) -> String {
    return String(this).format(kind, layout)
  }
}

extension Char {
  constructor(v : Int) {
    this = Char(UInt32(v))
  }
}

extension UInt8 {
  constructor(v : Char) {
    this = UInt8(UInt32(v.value))
  }
}
extension UInt32 {
  constructor(v : Char) {
    value = v.value
  }
}
extension UInt64 {
  constructor(v : Char) {
    this = UInt64(UInt32(v.value))
  }
}

func [infix_left=190] - (lhs: Char, rhs: Char) -> Int {
  return Int(Int32(Builtin.sub_Int32(lhs.value, rhs.value)))
}

func [infix_left=190] - (lhs: Char, rhs: Int) -> Char {
  return Char(UInt32(Builtin.sub_Int32(lhs.value, Int32(rhs).value)))
}

func [infix_left=190] + (lhs: Char, rhs: Int) -> Char {
  return Char(UInt32(Builtin.add_Int32(lhs.value, Int32(rhs).value)))
}

func [infix_left=190] + (lhs: Int, rhs: Char) -> Char {
  return Char(UInt32(Builtin.add_Int32(rhs.value, Int32(lhs).value)))
}

func [infix=170] < (lhs : Char, rhs : Char) -> Bool {
  return _getBool(Builtin.cmp_ult_Int32(lhs.value, rhs.value))
}

func [infix=170] > (lhs : Char, rhs : Char) -> Bool {
  return _getBool(Builtin.cmp_ugt_Int32(lhs.value, rhs.value))
}

func [infix=170] <= (lhs : Char, rhs : Char) -> Bool {
  return _getBool(Builtin.cmp_ule_Int32(lhs.value, rhs.value))
}

func [infix=170] >= (lhs : Char, rhs : Char) -> Bool {
  return _getBool(Builtin.cmp_uge_Int32(lhs.value, rhs.value))
}

func [infix=160] == (lhs : Char, rhs : Char) -> Bool {
  return _getBool(Builtin.cmp_eq_Int32(lhs.value, rhs.value))
}

func [infix=160] != (lhs : Char, rhs : Char) -> Bool {
  return _getBool(Builtin.cmp_ne_Int32(lhs.value, rhs.value))
}

extension Char {
  func isPrint() -> Bool {
    return (this >= Char(0o040) && this <= Char(0o176))
  }
}

// StringByte

struct StringByte : Comparable {
  var value : Builtin.Int8

  static func convertFromIntegerLiteral(val : Builtin.Int8) -> StringByte {
    return StringByte(val)
  }

  func replPrint() {
    print(UInt64(this))
  }
  // FIXME:
  // static var max : StringByte { get { return 0xFF } }
  // static var min : StringByte { get { return 0 } }
  static func max() -> StringByte { return 0xFF }
  static func min() -> StringByte { return 0 }
}

func [infix_left=190] + (lhs: StringByte, rhs: StringByte) -> StringByte {
  return StringByte(Builtin.add_Int8(lhs.value, rhs.value))
}

func [infix_left=190] - (lhs: StringByte, rhs: StringByte) -> StringByte {
  return StringByte(Builtin.sub_Int8(lhs.value, rhs.value))
}

func [infix=160] == (lhs : StringByte, rhs : StringByte) -> Bool {
  return _getBool(Builtin.cmp_eq_Int8(lhs.value, rhs.value))
}

func [infix=160] != (lhs : StringByte, rhs : StringByte) -> Bool {
  return !(lhs == rhs)
}

func [infix=170] < (lhs : StringByte, rhs : StringByte) -> Bool {
  return _getBool(Builtin.cmp_ult_Int8(lhs.value, rhs.value))
}

func [infix=170] > (lhs : StringByte, rhs : StringByte) -> Bool {
  return rhs < lhs
}

func [infix=170] <= (lhs : StringByte, rhs : StringByte) -> Bool {
  return !(rhs < lhs)
}

func [infix=170] >= (lhs : StringByte, rhs : StringByte) -> Bool {
  return !(lhs < rhs)
}

func [infix_left=150] & (lhs: StringByte, rhs: StringByte) -> StringByte {
  return StringByte(Builtin.and_Int8(lhs.value, rhs.value))
}

func [infix_left=130] | (lhs: StringByte, rhs: StringByte) -> StringByte {
  return StringByte(Builtin.or_Int8(lhs.value, rhs.value))
}

extension StringByte : FormattedPrintable {
  func format(kind : Char, layout : String) -> String {
    return UInt64(this).format(kind, layout)
  }
}

extension StringByte {
  constructor(v : Int8) {
    value = v.value
  }
  constructor(v : UInt8) {
    value = v.value
  }
  constructor(v : UInt32) {
    value = Builtin.trunc_Int32_Int8(v.value)
  }
}

extension Int8 {
  constructor(v : StringByte) {
    value = v.value
  }
}

extension UInt8 {
  constructor(v : StringByte) {
    value = v.value
  }
}

extension Int32 {
  constructor(v : StringByte) {
    value = Builtin.zext_Int8_Int32(v.value)
  }
}

extension UInt32 {
  constructor(v : StringByte) {
    value = Builtin.zext_Int8_Int32(v.value)
  }
}

extension UInt64 {
  constructor(v : StringByte) {
    value = Builtin.zext_Int8_Int64(v.value)
  }
}

extension Int64 {
  constructor(v : StringByte) {
    value = Builtin.zext_Int8_Int64(v.value)
  }
}

// StringByteData

struct StringByteData : Enumerable {
   var base : UnsafePointer<UInt8>,
   length : Int,
   owner : Builtin.ObjectPointer

   static func convertFromHeapArray(base : Builtin.RawPointer,
                                    owner : Builtin.ObjectPointer,
                                    length : Builtin.Int64) -> StringByteData {
     typealias UnsafePtr = UnsafePointer<UInt8>
     return StringByteData(UnsafePtr(base), Int(length), owner)
   }

   static func getNew(length : Int) -> StringByteData {
     var temp = new StringByte[length]
     return convertFromHeapArray(temp.base.value, temp.owner, temp.length.value)
   }

   func getLength() -> Int {
      return length & (Int.max() >> 1)
   }

   func setLength(len : Int) {
      length &= Int.min()
      length |= (len & Int.max())
   }

   func isASCII() -> Bool {
     return (length & Int64.min()) == 0
   }

   func isCString() -> Bool {
     return (length & (1 << 62)) == 0
   }

   func setASCII(b : Bool) {
     if b {
       length &= Int64.max()
     } else {
       length |= Int64.min()
     }
   }

   func setCString(b : Bool) {
     if b {
       length &= ~(1 << 62)
     } else {
       length |= (1 << 62)
     }
   }

  func getCString() -> String {
    if isCString() {
      return String(this)
    }
    var tmp : String = String(this) + Char(0)
    tmp.str_value.setCString(true)
    return tmp
  }

  func getRawCString() -> UnsafePointer<UInt8> {
    return getCString().str_value.base
  }

  subscript (i : Int) -> StringByte {
    get {
      debugTrap(UInt(i) < UInt(getLength()))
      return StringByte((base + i).get())
    }

    set {
      debugTrap(UInt(i) < UInt(getLength()))
      (base + i).set(UInt8(value))
    }
  }

  typealias EnumeratorType = StringByteData
  func getEnumeratorType() -> StringByteData { return this }

  func replPrint() {
    print('[')
    var first = true
    var total = 0
    for i in this {
      if first {
        first = false
      } else {
        print(", ")
      }
      i.replPrint()
      total = total + 1
      if total > 50 {
        print(" ...]")
        return
      }
    }
    print(']')
  }

  // Slicing via subscripting with a range.
  subscript (rng : IntEnumeratorType) -> StringByteData {
    get {
      var len = getLength()
      debugTrap(rng.min <= len && rng.max <= len)
      return StringByteData(base + rng.min, rng.max - rng.min, owner)
    }

    set {
      var len = value.getLength()
      debugTrap(len == rng.max - rng.min)

      // Common case: the elements were updated in place, so we do not have to
      // perform any updates.
      var destStart = base + rng.min
      if value.base == destStart {
        return
      }

      // If the start of the destination slice falls inside the source slice,
      // copy backwards.
      if destStart >= value.base && destStart < value.base + len {
        var destEnd = destStart + len
        for i in value {
          --destEnd
          destEnd.set(UInt8(i))
        }

        return
      }

      // Copy the data.
      for i in value {
        destStart.set(UInt8(i))
        ++destStart
      }
    }
  }

  func each(f : (StringByte) -> Void) {
    for i in this { f(i) }
  }

  func reduce(val : StringByte, f : (StringByte, StringByte) -> StringByte) -> StringByte {
    for i in this { val = f(val, i) }
    return val
  }

  func map(f : (StringByte) -> StringByte) -> StringByte[] {
    var len = getLength()
    var r = new StringByte[len]
    for i in 0 .. len { r[i] = f(this[i]) }
    return r
  }
}

extension StringByteData : Enumerator {
  typealias Element = StringByte

  func isEmpty() -> Bool { return getLength() == 0 }
  func next() -> StringByte {
    var prev = this[0]
    base = base + 1
    setLength(getLength() - 1)
    return prev
  }
}
