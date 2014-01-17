// Interfaces with a questionable future that are needed in order to
// be a drop-in replacement for String
//
extension String {
  
  init<
    Encoding: UnicodeCodec, Input: Collection
  where Input.StreamType.Element == Encoding.CodeUnit
  >(
    encoding: Encoding.metatype, input: Input
  )
  {
    self = String(StringBuffer(encoding, input))
  }
  
  init(str_value : StringByteData) {
    self = String(UTF8, UnsafeArray(str_value.base, str_value.length))
  }

  init(sz: Int, c: UnicodeScalar) {
    self = String(UTF32, GenerateN<UTF32.CodeUnit>(sz, c.value))
  }

  var str_value: StringByteData {
    var utf8 = self.asUTF8()
    return StringByteData.convertFromHeapArray(
      utf8.base.value, utf8.owner,
      utf8.count.value)
  }
  
  func asUTF8() -> UTF8.CodeUnit[] {
    var result = new UTF8.CodeUnit[encodedLength(UTF8)]
    var len = 0
    encode(UTF8, SinkOf<UTF8.CodeUnit>({ result[len++] = $0 }))
    return result
  }

  func byteLength() -> Int {
    return encodedLength(UTF8)
  }

  func nulTerminatedUTF8() -> StringByteData {
    var buffer = str_value
    var nul: UInt8[] = [0]
    buffer.appendBytes(nul.base, 1)
    swift_keepAlive(nul.owner)
    return buffer
  }

  // FIXME: this typealias should die; it is only needed to satisfy
  // test/NameBinding/library.swift.  That test should be updated to
  // not depend on stdlib details
  typealias CharStreamType = UTF16Scalars.StreamType
  var chars : UTF16Scalars {
    return UTF16Scalars(_contiguous())
  }
  
  var lines : String[] {
    return split('\n')
  }
  
  func split(separator: UnicodeScalar) -> String[] {
    var scalarSlices = swift.split(chars, { $0 == separator })
    return scalarSlices.map { $0 as String }
  }
  
  var bytes : StringByteData {
    var result = StringByteData(byteLength())
    encode(
      UTF8, SinkOf<UTF8.CodeUnit>(
        {
          var tmp = $0
          result.appendBytes(
            UnsafePointer(Builtin.addressof(&tmp)), 1)
        }
        ))
    return result
  }
  
  func size() -> Int {
    var count = 0
    for c in chars {
      ++count
    }
    return count
  }

  var length: Int {
    return size()
  }
  
  func isEmpty() -> Bool {
    switch (representation) {
    case .Opaque(var rep):
      return rep.range.isEmpty()
    case .Contiguous(var rep):
      return rep.count == 0
    }
  }

  type func _from(utf8: StringByteData) -> String {
    return String(UTF8, UnsafeArray(utf8.base, utf8.length))
  }
  
  // FIXME: for some reason, making this function an actual overload
  // of subscript breaks tests.  Investigate later.
  func subscript_(rng : IntStreamType) -> String {
    return String._from(bytes[rng])
  }

  subscript (idx : Int) -> UnicodeScalar {
    for (i, c) in swift.enumerate(chars) {
      if i == idx {
        return c
      }
    }
    alwaysTrap()
  }
}

extension String : ReplPrintable {
  func replPrint() {
    print('"')
    for c in chars {
      print(c.escape())
    }
    print('"')
  }
}

extension String {
  // FIXME: Locales make this interesting
  var uppercase : String {
    var str_value = self.bytes
    var len = byteLength()
    var resultArray = StringByteData.getNew(len)
    var i = 0
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

    return String._from(resultArray)
  }

  // FIXME: Locales make this interesting
  var lowercase : String {
    var str_value = self.bytes
    var len = byteLength()
    var resultArray = StringByteData.getNew(len)
    var i = 0
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

    return String._from(resultArray)
  }

  init(c: UnicodeScalar) {
    self = String(1, c)
  }

  func _isAll(predicate: (UnicodeScalar) -> Bool) -> Bool {
    for c in chars { if !predicate(c) { return false } }

    return true
  }

  func startsWith(prefix: String) -> Bool {
    if prefix.size() > size() { return false }

    return self[0..prefix.size()] == prefix
  }

  func isAlpha() -> Bool { return _isAll({ $0.isAlpha() }) }
  func isDigit() -> Bool { return _isAll({ $0.isDigit() }) }
  func isSpace() -> Bool { return _isAll({ $0.isSpace() }) }
}

extension String : FormattedPrintable {
  func format(kind: UnicodeScalar, layout: String) -> String {
    var toPrint = self
    if kind == 'u' { toPrint = uppercase }
    else if kind == 'l' { toPrint = lowercase }
    return Format(layout).printToString(toPrint)
  }
}

/// \brief Represent a positive integer value in the given radix,
/// writing each UTF-16 code units into stream.  The value of `ten'
/// should be either 'A' or 'a', depending on whether you want upper-
/// or lower-case letters when radix > 10
func _formatPositiveInteger( 
  value: UInt64,
  radix: UInt64,
  ten: UnicodeScalar = 'a') (  stream: (UTF16.CodeUnit)->Void )
{

  if value == 0 {
    return
  }

  _formatPositiveInteger(value / radix, radix, ten)(stream)
  var digit = UInt32(value % radix)
  var baseCharOrd : UInt32 = digit <= 9 ? '0'.value : ten.value - 10
  stream(UTF16.CodeUnit(baseCharOrd + digit))
}

func _formatSignedInteger(
  value: Int64,
  radix: UInt64,
  ten: UnicodeScalar = 'a') (  stream: (UTF16.CodeUnit)->Void ) {
  
  if value == 0 {
    stream (UTF16.CodeUnit('0'.value))
  }
  else {
    if (value < 0) {
      stream(UTF16.CodeUnit('-'.value))
    }
    // Compute the absolute value without causing overflow when value
    // == Int64.min
    let absValue = value < 0 ? UInt64(~value) + 1 : UInt64(value)
    _formatPositiveInteger(absValue, radix, ten)(stream)
  }
}

// Conversions to string from other types.
extension String {
  init(v: Int64, radix: Int = 10, uppercase: Bool = false) {
    var format = _formatSignedInteger(v, UInt64(radix), uppercase ? 'A' : 'a')
    var utf16Count = 0
    format { _ in ++utf16Count;() }
    var buffer = StringBuffer(utf16Count)
    var used = buffer.used
    format { used++.set($0) }
    buffer.used = used
    self = String(buffer)
  }

  init(v : UInt64, radix: Int = 10, uppercase: Bool = false) {
    var format = _formatPositiveInteger(v, UInt64(radix), uppercase ? 'A' : 'a')
    var utf16Count = v == 0 ? 1 : 0
    format { _ in ++utf16Count;() }
    var buffer = StringBuffer(utf16Count)
    var used = buffer.used
    format { used++.set($0) }
    if v == 0 {
      used++.set(UTF16.CodeUnit('0'.value))
    }
    buffer.used = used
    self = String(buffer)
  }

  init(v : Int8, radix : Int = 10, uppercase : Bool = false) {
    self = String(Int64(v), radix, uppercase)
  }
  init(v : Int16, radix : Int = 10, uppercase : Bool = false) {
    self = String(Int64(v), radix, uppercase)
  }
  init(v : Int32, radix : Int = 10, uppercase : Bool = false) {
    self = String(Int64(v), radix, uppercase)
  }
  init(v : UInt8, radix : Int = 10, uppercase : Bool = false) {
    self = String(UInt64(v), radix, uppercase)
  }
  init(v : UInt16, radix : Int = 10, uppercase : Bool = false) {
    self = String(UInt64(v), radix, uppercase)
  }
  init(v : UInt32, radix : Int = 10, uppercase : Bool = false) {
    self = String(UInt64(v), radix, uppercase)
  }

  init(v : Double) {
    var cCharBuf = Array<UInt8>(256, 0)
    var n = Int(c_print_double(cCharBuf.base.value, v))
    var buffer = StringBuffer(n)
    var used = buffer.used
    for i in 0..n {
      used++.set(UTF16.CodeUnit(cCharBuf[i]))
    }
    buffer.used = used
    self = String(buffer)
  }
  
  init(v : Float) {
    self = String(Double(v))
  }

  init(b : Bool) {
    if b {
      self = "true"
    } else {
      self = "false"
    }
  }
}

// Conversions from string to other types.
extension String {
  /// \brief If the string represents an integer that fits into an Int, returns
  /// the corresponding integer.
  func toInt() -> Int? {
    var scalars = self.chars

    var start = scalars.startIndex()
    if start == scalars.endIndex() {
      return .None
    }
    
    // Interpet '+' or '-' before the number.
    var negativeFactor = -1
    var firstC = scalars[start]
    if (firstC == '+') {
      ++start
    } else if (firstC == '-') {
      ++start
      negativeFactor = 1
    }

    // Interpret the string as an integer.
    // Since Int.min has a larger absolute value, perform addition with
    // negative numbers; detect underflows before they happen. 
    var res : Int = 0
    for c in scalars[start..scalars.endIndex()] {
      if !c.isDigit() {
        // Conversion failed if a non-digit is encountered.
        return .None
      }

      // Underflow occurs if res * 10 < Int.min.
      if res < Int.min / 10 {
        return .None
      }
      res = res * 10

      var d : Int = (c - '0')
      // Underflow occurs if res - d < Int.min.
      if res < Int.min + d {
        return .None
      }
      res = res - d
    }

    // If res is Int.min and the result should be positive, the next
    // operation will overflow.
    if negativeFactor == -1 && res == Int.min {
      return .None
    }

    return .Some(res * negativeFactor)
  }
}

extension String {
  /// \brief Produce a substring of the given string from the given character
  /// index to the end of the string.
  func substr(start: Int) -> String {
    var rng = chars
    var startIndex = rng.startIndex()
    for i in 0..start {
      ++startIndex
    }
    return rng[startIndex..rng.endIndex()]
  }

  /// \brief Split the given string at the given delimiter character, returning 
  /// the strings before and after that character (neither includes the character
  /// found) and a boolean value indicating whether the delimiter was found.
  func splitFirst(delim: UnicodeScalar)
    -> (before: String, after: String, wasFound : Bool)
  {
    var rng = chars
    for i in indices(rng) {
      if rng[i] == delim {
        return (rng[rng.startIndex()..i], rng[i.succ()..rng.endIndex()], true)
      }
    }
    return (self, "", false)
  }

  /// \brief Split the given string at the first character for which the given
  /// predicate returns true. Returns the string before that character, the 
  /// character that matches, the string after that character, and a boolean value
  /// indicating whether any character was found.
  func splitFirstIf(pred: (UnicodeScalar) -> Bool)
    -> (before: String, found: UnicodeScalar, after: String, wasFound: Bool)
  {
    var rng = chars
    for i in indices(rng) {
      if pred(rng[i]) {
        return (rng[rng.startIndex()..i], rng[i], rng[i.succ()..rng.endIndex()], true)
      }
    }
    return (self, '🎃', String(), false)
  }

  /// \brief Split the given string at each occurrence of a character for which
  /// the given predicate evaluates true, returning an array of strings that
  /// before/between/after those delimiters.
  func splitIf(pred: (UnicodeScalar) -> Bool) -> String[] {
    var scalarSlices = swift.split(chars, pred)
    return scalarSlices.map { $0 as String }
  }
}

