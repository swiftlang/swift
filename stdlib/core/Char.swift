//===----------------------------------------------------------------------===//
// Char Type
//===----------------------------------------------------------------------===//

struct Char : BuiltinCharacterLiteralConvertible, CharacterLiteralConvertible {
  var _value : Builtin.Int32

  var value : UInt32 {
    get:
      return UInt32(_value)
  }

  static func _convertFromBuiltinCharacterLiteral(val : Builtin.Int32) -> Char {
    return Char(val)
  }

  typealias CharacterLiteralType = Char
  static func convertFromCharacterLiteral(value : Char) -> Char {
    return value
  }

  constructor() {
    this._value = Int32(0).value
  }

  constructor(value : Builtin.Int32) {
    this._value = value
  }

  constructor(v : UInt32) {
    var lowHalf = v & 0xFFFF
    // reserved in each plane
    debugTrap(lowHalf != 0xFFFE && lowHalf != 0xFFFF)
    // UTF-16 surrogate pair values are not valid code points
    debugTrap(v < 0xD800 || v > 0xDFFF)
    // U+FDD0...U+FDEF are also reserved
    debugTrap(v < 0xFDD0 || v > 0xFDEF)
    // beyond what is defined to be valid
    debugTrap(v < 0x10FFFF)

    this._value = v.value
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
    } else if this == '\0' {
      print('\\')
      print('0')
    } else if this == '\n' {
      print('\\')
      print('n')
    } else if this == '\r' {
      print('\\')
      print('r')
    } else if this == '\t' {
      print('\\')
      print('t')
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
  func _printNibbleAsHex(v : UInt32) {
    v = v & 15
    if v < 10 {
      print(Char(v+48))    // 48 = '0'
    } else {
      print(Char(v-10+65)) // 65 = 'A'
    }
  }

  /// \returns true if this is an ASCII character (code point 0 to 127
  /// inclusive).
  func isASCII() -> Bool {
    return value <= 127
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
  var uppercase : Char {
    if this >= 'a' && this <= 'z' {
      return Char(UInt32(this) - 32)
    } else if this >= 'à' && this <= 'þ' && this != '÷' {
      return Char(UInt32(this) - 32)
    }
    return this
  }

  // FIXME: Locales make this interesting
  var lowercase : Char {
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

extension Char : Hashable {
  func hashValue() -> Int {
    return Int(this.value)
  }
}

extension Char {
  constructor(v : Int) {
    this = Char(UInt32(v))
  }
}

extension UInt8 {
  constructor(v : Char) {
    debugTrap(v.value <= UInt32(UInt8.max()), "Code point value does not fit into UInt8")
    this = UInt8(v.value)
  }
}
extension UInt32 {
  constructor(v : Char) {
    this = v.value
  }
}
extension UInt64 {
  constructor(v : Char) {
    this = UInt64(v.value)
  }
}

func - (lhs: Char, rhs: Char) -> Int {
  return Int(lhs.value) - Int(rhs.value)
}

func - (lhs: Char, rhs: Int) -> Char {
  return Char(lhs.value - UInt32(rhs))
}

func + (lhs: Char, rhs: Int) -> Char {
  return Char(lhs.value + UInt32(rhs))
}

func + (lhs: Int, rhs: Char) -> Char {
  return rhs + lhs
}

extension Char : Comparable {
  func __equal__(rhs: Char) -> Bool {
    return this.value == rhs.value
  }
  func __less__(rhs: Char) -> Bool {
    return this.value < rhs.value
  }
}

extension Char {
  func isPrint() -> Bool {
    return (this >= Char(0o040) && this <= Char(0o176))
  }
}
