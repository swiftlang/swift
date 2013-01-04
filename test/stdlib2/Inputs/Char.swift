//===----------------------------------------------------------------------===//
// Char Type
//===----------------------------------------------------------------------===//

struct Char : Comparable, Nullable {
  var value : Builtin.Int32

  static func convertFromCharacterLiteral(val : Builtin.Int32) -> Char {
    return Char(val)
  }

  constructor(v : UInt32) {
    // FIXME:  Give Char an invariant that it can only hold valid UTF-32
    value = v.value
  }

  static func isNull(arg : Char) -> Bool {
    return _getBool(Builtin.cmp_eq_Int32(arg.value, Int32(0).value))
  }

  static func null() -> Char {
    var tmp : Char
    return tmp
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
