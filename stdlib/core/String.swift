//===----------------------------------------------------------------------===//
// String Type
//===----------------------------------------------------------------------===//

struct String : Hashable, BuiltinStringLiteralConvertible,
                StringLiteralConvertible {
  var str_value : StringByteData

  static func _convertFromBuiltinStringLiteral(value : Builtin.RawPointer,
                                               byteSize : Builtin.Int64,
                                               isASCII : Builtin.Int1) -> String {
    var s : String
    var owner : Builtin.ObjectPointer
    s.str_value = StringByteData.convertFromHeapArray(value, owner, byteSize)
    s.str_value.setASCII(_getBool(isASCII))
    s.str_value.setCString(true)
    return s
  }

  typealias StringLiteralType = String
  static func convertFromStringLiteral(value : String) -> String {
    return value
  }

  constructor() {
    this.str_value = StringByteData()
  }

  constructor(str_value : StringByteData) {
    this.str_value = str_value
  }

  constructor(sz : Int, c : Char) {
    var s = String(c)
    str_value = StringByteData.getNew(sz*s.byteLength())
    var k = 0
    for i in 0..sz {
      for j in 0..s.byteLength() {
        str_value[k++] = s.str_value[j]
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
    return str_value.length
  }

  func _makeNulTerminated() {
    if str_value.isCString() {
      return
    }

    this = str_value.getCString()
  }

  struct CharEnumeratorType : Enumerable, Enumerator {
    var value : String

    typealias EnumeratorType = CharEnumeratorType
    func getEnumeratorType() -> CharEnumeratorType {
      return this
    }

    typealias Element = Char
    func isEmpty() -> Bool {
      return value.isEmpty()
    }
    func next() -> Char {
      // string ranges do not end in the middle of a Char
      // one range check is sufficient.
      debugTrap(!isEmpty())
      debugTrap(value.str_value.length > 0)

      var u8 = StringByte(value.str_value.base.get())
      if u8 < 0x80 {
        value.str_value.base += 1
        value.str_value.length -= 1
        return Char(UInt32(u8))
      }

      return _nextSlow(u8)
    }

    func /*[noinline]*/ _nextSlow(u8 : StringByte) -> Char {
      var u8_1 = StringByte((value.str_value.base + 1).get())
      if u8 < 0xE0 {
        value.str_value.base += 2
        value.str_value.length -= 2
        return Char((UInt32(u8 & 0x1F) << 6) |
                     UInt32(u8_1 & 0x3F))
      }
      var u8_2 = StringByte((value.str_value.base + 2).get())
      if u8 < 0xF0 {
        value.str_value.base += 3
        value.str_value.length -= 3
        return Char((UInt32(u8   & 0x0F) << 12) |
                    (UInt32(u8_1 & 0x3F) << 6)  |
                     UInt32(u8_2 & 0x3F))
      }
      var u8_3 = StringByte((value.str_value.base + 3).get())
      value.str_value.base += 4
      value.str_value.length -= 4
      return Char((UInt32(u8   & 0x07) << 18) |
                  (UInt32(u8_1 & 0x3F) << 12) |
                  (UInt32(u8_2 & 0x3F) << 6)  |
                   UInt32(u8_3 & 0x3F))
    }
  }

  var chars : CharEnumeratorType {
    return CharEnumeratorType(this)
  }
  var lines : String[] {
    return split('\n')
  }
  var bytes : StringByteData {
    return str_value
  }

  func size() -> Int {
    if str_value.isASCII() {
      return byteLength()
    }
    var Result = 0
    for i in chars {
      ++Result
    }
    return Result
  }

  var length : Int {
    return size()
  }

  func isEmpty() -> Bool {
    return byteLength() == 0
  }

  subscript (rng : IntEnumeratorType) -> String {
    var len = rng.max - rng.min
    if str_value.isASCII() {
      debugTrap(UInt(len) <= UInt(byteLength()))
      return String(StringByteData.convertFromHeapArray(
                        (str_value.base + rng.min).value,
                        str_value.owner,
                        len.value))
    }
    return _subscriptNonASCII(rng)
  }

  func _subscriptNonASCII (rng : IntEnumeratorType) -> String {
    var len = rng.max - rng.min
    var start = rng.min
    var idx = 0
    while start > 0 {
      var tmp : StringByte = str_value[idx++]
      if tmp < 0x80 {
        --start
      } else if tmp >= 0xC0 {
        --start
      }
    }
    var oldidx = idx
    while len > 0 {
      var tmp = str_value[idx++]
      if tmp < 0x80 {
        --len
      } else if tmp >= 0xC0 {
        --len
      }
    }
    return String(StringByteData.convertFromHeapArray(
                    (str_value.base + oldidx).value,
                    str_value.owner, (idx - oldidx).value))
  }

  subscript (idx : Int) -> Char {
    if str_value.isASCII() {
      if idx < byteLength() {
        return Char(UInt32((str_value.base + idx).get()))
      }
    }
    return subscriptNonASCII(idx)
  }

  func subscriptNonASCII(idx : Int) -> Char {
    for c in chars {
      if idx-- == 0 {
        return c
      }
    }
    alwaysTrap()
  }

  func replPrint() {
    print('"')
    for c in chars {
      c.replPrintCharBody()
    }
    print('"')
  }

  // FIXME: Locales make this interesting
  var uppercase : String {
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
        i += 1
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
  var lowercase : String {
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
        i += 1
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
    str_value = StringByteData(4)
    _append(c)
  }

  // Predicates

  // FIXME: Replace this with a generic isAll().
  func _isAll(predicate : (Char) -> Bool) -> Bool {
    for c in chars { if !predicate(c) { return false } }

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
    if kind == 'u' { toPrint = uppercase }
    else if kind == 'l' { toPrint = lowercase }
    return Format(layout).printToString(toPrint)
  }
}

// StringLiteralType specifies the default type to use for an string literal
// when the type isn't constrained.
typealias StringLiteralType = String


// Concatenation.
extension String {
  func _append(bytes: UnsafePointer<UInt8>, count: Int) {
    str_value.appendBytes(bytes, count)
  }

  func _append(c_: Char) {
    var buf: (UInt8,UInt8,UInt8,UInt8)
    var count = 1

    var c = UInt32(c_)
    buf.3 = UInt8(c)
    if c >= UInt32(1<<7) {
      c >>= 6
      ++count
      buf.3 = (buf.3 & 0x3F) | 0x80 // 10xxxxxx
      buf.2 = UInt8(c)
      if c < UInt32(1<<5) {
        buf.2 |= 0xC0 // 110xxxxx
      }
      else {
        c >>= 6
        ++count
        buf.2 = (buf.2 & 0x3F) | 0x80 // 10xxxxxx
        buf.1 = UInt8(c)
        if c < UInt32(1<<4) {
          buf.1 |= 0xE0 // 1110xxxx
        }
        else {
          c >>= 6
          ++count
          buf.1 = (buf.1 & 0x3F) | 0x80 // 10xxxxxx
          buf.0 = UInt8(c | 0xF0) // 11110xxx
        }
      }
    }
    
    str_value.appendBytes(UnsafePointer.addressOf(&buf.0) + 4 - count, count)
  }
}

func +(lhs : String, rhs : String) -> String {
  if (lhs.isEmpty()) {
    return rhs
  }
  // FIXME: Consider further optimizations here, like using rhs as the
  // result if it's uniquely referenced and has the capacity
  lhs._append(rhs.str_value.base, rhs.str_value.length)
  return lhs
}
func +(lhs : String, rhs : Char) -> String {
  lhs._append(rhs)
  return lhs
}
func +(lhs : Char, rhs : String) -> String {
  var result = String(StringByteData(rhs.byteLength() + 4))
  result._append(lhs)
  result._append(rhs.str_value.base, rhs.byteLength())
  return result
}
func +(lhs : Char, rhs : Char) -> String {
  var result = String(StringByteData(8))
  result._append(lhs)
  result._append(rhs)
  return result
}


// String append
func [assignment] += (lhs : [byref] String, rhs : String) {
  if (lhs.isEmpty()) {
    lhs = rhs
  }
  else {
    lhs._append(rhs.str_value.base, rhs.str_value.length)
  }
}

func [assignment] += (lhs : [byref] String, rhs : Char) {
  lhs._append(rhs)
}


// Comparison operators
extension String : Comparable {
  func __equal__(rhs: String) -> Bool {
    if str_value.length != rhs.str_value.length {
      return false
    }
    if str_value.base == rhs.str_value.base {
      return true
    }
    var len = byteLength()
    for var i = 0; i < len; ++i {
       if str_value[i] != rhs.str_value[i] {
         return false
       }
    }
    return true
  }

  func __less__(rhs: String) -> Bool {
    var i = 0
    for c in rhs.str_value {
      if i == byteLength() || str_value[i] < c {
        return true
      }
      if c < str_value[i] {
        return false
      }
      ++i
    }
    return false
  }
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

// Conversions from string to other types.
extension String {
  /// \brief If the string represents an integer that fits into an Int, returns
  /// the corresponding integer.
  func toInt() -> Optional<Int> {
    if length <= 0 {
      return None
    }

    // Interpet '+' or '-' before the number.
    var startWithIndex = 0
    var negativeFactor = -1
    var firstC = this[0]
    if (firstC == '+') {
      startWithIndex = 1
    } else if (firstC == '-') {
      startWithIndex = 1
      negativeFactor = 1
    }

    // Interpret the string as an integer.
    // Since Int.min() has a larger absolute value, perform addition with
    // negative numbers; detect underflows before they happen. 
    var res : Int = 0
    for c in this[startWithIndex..length].chars {
      if !c.isDigit() {
        // Conversion failed if a non-digit is encountered.
        return None
      }

      // Underflow occurs if res * 10 < Int.min().
      if res < Int.min() / 10 {
        return None
      }
      res = res * 10

      var d : Int = (c - '0')
      // Underflow occurs if res - d < Int.min().
      if res < Int.min() + d {
        return None
      }
      res = res - d
    }

    // If res is Int.min() and the result should be positive, the next
    // operation will overflow.
    if negativeFactor == -1 && res == Int.min() {
      return None
    }

    return Optional(res * negativeFactor)
  }
}

extension String {
  /// \brief Produce a substring of the given string from the given character
  /// index to the end of the string.
  func substr(start : Int) -> String {
    var rng = chars
    while start-- > 0 {
      rng.next()
    }
    var result = this
    var bytes = rng.value.str_value.base - str_value.base
    result.str_value.base += bytes
    result.str_value.length -= bytes
    return result
  }

  /// \brief Split the given string at the given delimiter character, returning 
  /// the strings before and after that character (neither includes the character
  /// found) and a boolean value indicating whether the delimiter was found.
  func splitFirst(delim : Char)
    -> (before: String, after: String, wasFound : Bool)
  {
    var rng = chars
    while !rng.isEmpty() {
      var rngBefore = rng
      if rng.next() == delim {
        var before = this
        before.str_value.length = rngBefore.value.str_value.base - str_value.base
        before.str_value.setASCII(str_value.isASCII())
        before.str_value.setCString(false)

        var after = this
        after.str_value.base += rng.value.str_value.base - str_value.base
        after.str_value.length = after.str_value.length -
                                  (rng.value.str_value.base - str_value.base)
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
    var rng = chars
    while !rng.isEmpty() {
      var rngBefore = rng
      var c = rng.next()
      if pred(c) {
        var before = this
        before.str_value.length = rngBefore.value.str_value.base -
                                   str_value.base
        before.str_value.setASCII(str_value.isASCII())
        before.str_value.setCString(false)

        var after = this
        after.str_value.base += rng.value.str_value.base - str_value.base
        after.str_value.length = after.str_value.length -
                                  (rng.value.str_value.base - str_value.base)
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
    var lines = Vector<String>()
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
    var lines = Vector<String>()
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
  func hashValue() -> Int {
    var r : UInt = 5381
    for byte in str_value {
      r = ((r << 5) + r) + UInt(byte)
    }
    return Int(r)
  }
}

extension String : StringInterpolationConvertible {
  static func convertFromStringInterpolation(strings : String...) -> String {
    var result : String
    for str in strings {
      result += str
    }
    return result
  }
}
