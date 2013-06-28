struct Word : BuiltinIntegerLiteralConvertible, IntegerLiteralConvertible, 
       ArrayBound {
  var value : Builtin.Int64

  static func _convertFromBuiltinIntegerLiteral(val : Builtin.Int128) -> Word {
    return Word(Builtin.trunc_Int128_Int64(val))
  }

  typealias IntegerLiteralType = Word
  static func convertFromIntegerLiteral(value : Word) -> Word {
    return value
  }

  func _getBuiltinArrayBoundValue() -> Builtin.Int64 {
    return value
  }

  typealias ArrayBoundType = Word
  func getArrayBoundValue() -> Word {
    return this
  }

  func replPrint() {
    print(Int64(this))
  }
  // FIXME:
  // static var max : Word { get: return 0x7FFFFFFFFFFFFFFF }
  // static var min : Word { get: return -0x7FFFFFFFFFFFFFFF-1 }
  static func max() -> Word { return 0x7FFFFFFFFFFFFFFF }
  static func min() -> Word { return -0x7FFFFFFFFFFFFFFF-1 }
  static func bitSize() -> Word { return 64 }
}

extension Word : RandomAccessIndex, BidirectionalIndex {
  func __equal__(rhs: Word) -> Bool {
    return _getBool(Builtin.cmp_eq_Int64(value, rhs.value))
  }
  func __less__(rhs: Word) -> Bool {
    return _getBool(Builtin.cmp_slt_Int64(value, rhs.value))
  }
  func hashValue() -> Int {
    return Int(Int64(value))
  }
  func succ() -> Word {
    return this + 1
  }
  func pred() -> Word {
    return this - 1
  }
  typealias DistanceType = Word
  func __sub__(rhs: Word) -> DistanceType {
    return Word(Builtin.sub_Int64(value, rhs.value))
  }
  func __add__(offset: DistanceType) -> Word {
    return Word(Builtin.add_Int64(value, offset.value))
  }
}

extension Word : SignedNumber {
  func __negate__() -> Word { return 0 - this }
  func isNegative() -> Bool { return this < 0 }
}

struct UWord : BuiltinIntegerLiteralConvertible, IntegerLiteralConvertible {
  var value : Builtin.Int64

  static func _convertFromBuiltinIntegerLiteral(val : Builtin.Int128) -> UWord {
    return UWord(Builtin.trunc_Int128_Int64(val))
  }

  typealias IntegerLiteralType = UWord
  static func convertFromIntegerLiteral(value : UWord) -> UWord {
    return value
  }

  func replPrint() {
    print(UInt64(this))
  }
  // FIXME:
  // static var max : UWord { get: return 0xFFFFFFFFFFFFFFFF }
  // static var min : UWord { get: return 0 }
  static func max() -> UWord { return 0xFFFFFFFFFFFFFFFF }
  static func min() -> UWord { return 0 }
  static func bitSize() -> UWord { return 64 }
}

extension UWord : RandomAccessIndex {
  func __equal__(rhs: UWord) -> Bool {
    return _getBool(Builtin.cmp_eq_Int64(value, rhs.value))
  }
  func __less__(rhs: UWord) -> Bool {
    return _getBool(Builtin.cmp_ult_Int64(value, rhs.value))
  }
  func succ() -> UWord {
    return this + 1
  }
  func pred() -> UWord {
    return this - 1
  }
  typealias DistanceType = UWord
  func __sub__(rhs: UWord) -> DistanceType {
    return UWord(Builtin.sub_Int64(value, rhs.value))
  }
  func __add__(offset: DistanceType) -> UWord {
    return UWord(Builtin.add_Int64(value, offset.value))
  }
}

//===----------------------------------------------------------------------===//
// Explicit conversions between types.
//===----------------------------------------------------------------------===//

extension Word {
  constructor(v : Int64) {
    value = v.value
  }
  constructor(v : UInt64) {
    value = v.value
  }
}

extension UWord {
  constructor(v : Int64) {
    value = v.value
  }
  constructor(v : UInt64) {
    value = v.value
  }
}

extension Int8 {
  constructor(v : Word) {
    value = Builtin.trunc_Int64_Int8(v.value)
  }
  constructor(v : UWord) {
    value = Builtin.trunc_Int64_Int8(v.value)
  }
}

extension UInt8 {
  constructor(v : Word) {
    value = Builtin.trunc_Int64_Int8(v.value)
  }
  constructor(v : UWord) {
    value = Builtin.trunc_Int64_Int8(v.value)
  }
}

extension Int16 {
  constructor(v : Word) {
    value = Builtin.trunc_Int64_Int16(v.value)
  }
  constructor(v : UWord) {
    value = Builtin.trunc_Int64_Int16(v.value)
  }
}

extension UInt16 {
  constructor(v : Word) {
    value = Builtin.trunc_Int64_Int16(v.value)
  }
  constructor(v : UWord) {
    value = Builtin.trunc_Int64_Int16(v.value)
  }
}

extension Int32 {
  constructor(v : Word) {
    value = Builtin.trunc_Int64_Int32(v.value)
  }
  constructor(v : UWord) {
    value = Builtin.trunc_Int64_Int32(v.value)
  }
}

extension UInt32 {
  constructor(v : Word) {
    value = Builtin.trunc_Int64_Int32(v.value)
  }
  constructor(v : UWord) {
    value = Builtin.trunc_Int64_Int32(v.value)
  }
}

extension Int64 {
  constructor(v : Word) {
    value = v.value
  }
  constructor(v : UWord) {
    value = v.value
  }
}

extension UInt64 {
  constructor(v : Word) {
    value = v.value
  }
  constructor(v : UWord) {
    value = v.value
  }
}

extension Int128 {
  constructor(v : Word) {
    value = Builtin.sext_Int64_Int128(v.value)
  }
  constructor(v : UWord) {
    value = Builtin.zext_Int64_Int128(v.value)
  }
}

extension UInt128 {
  constructor(v : Word) {
    value = Builtin.sext_Int64_Int128(v.value)
  }
  constructor(v : UWord) {
    value = Builtin.zext_Int64_Int128(v.value)
  }
}

//===----------------------------------------------------------------------===//
// Standard Operators
//===----------------------------------------------------------------------===//

func [prefix] +(a : Word)  -> Word   { return a }
func [prefix] +(a : UWord) -> UWord  { return a }
func [prefix] ~(a : Word)  -> Word  { return a ^ Word.min() }
func [prefix] ~(a : UWord) -> UWord { return a ^ UWord.max() }

func * (lhs : Word, rhs : Word) -> Word {
  return Word(Builtin.mul_Int64(lhs.value, rhs.value))
}
func * (lhs : UWord, rhs : UWord) -> UWord {
  return UWord(Builtin.mul_Int64(lhs.value, rhs.value))
}

func / (lhs : Word, rhs : Word) -> Word {
  return Word(Builtin.sdiv_Int64(lhs.value, rhs.value))
}
func / (lhs : UWord, rhs : UWord) -> UWord {
  return UWord(Builtin.udiv_Int64(lhs.value, rhs.value))
}

func % (lhs : Word, rhs : Word) -> Word {
  return Word(Builtin.srem_Int64(lhs.value, rhs.value))
}
func % (lhs : UWord, rhs : UWord) -> UWord {
  return UWord(Builtin.urem_Int64(lhs.value, rhs.value))
}

func + (lhs: Word, rhs: Word) -> Word {
  return Word(Builtin.add_Int64(lhs.value, rhs.value))
}
func + (lhs: UWord, rhs: UWord) -> UWord {
  return UWord(Builtin.add_Int64(lhs.value, rhs.value))
}

func - (lhs: Word, rhs: Word) -> Word {
  return Word(Builtin.sub_Int64(lhs.value, rhs.value))
}
func - (lhs: UWord, rhs: UWord) -> UWord {
  return UWord(Builtin.sub_Int64(lhs.value, rhs.value))
}

func << (lhs : Word, rhs : Word) -> Word {
  return Word(Builtin.shl_Int64(lhs.value, rhs.value))
}
func << (lhs : UWord, rhs : UWord) -> UWord {
  return UWord(Builtin.shl_Int64(lhs.value, rhs.value))
}

func >>(lhs : Word, rhs : Word) -> Word {
  return Word(Builtin.ashr_Int64(lhs.value, rhs.value))
}
func >>(lhs : UWord, rhs : UWord) -> UWord {
  return UWord(Builtin.lshr_Int64(lhs.value, rhs.value))
}

func & (lhs: Word, rhs: Word) -> Word {
  return Word(Builtin.and_Int64(lhs.value, rhs.value))
}
func & (lhs: UWord, rhs: UWord) -> UWord {
  return UWord(Builtin.and_Int64(lhs.value, rhs.value))
}

func ^ (lhs: Word, rhs: Word) -> Word {
  return Word(Builtin.xor_Int64(lhs.value, rhs.value))
}
func ^ (lhs: UWord, rhs: UWord) -> UWord {
  return UWord(Builtin.xor_Int64(lhs.value, rhs.value))
}

func | (lhs: Word, rhs: Word) -> Word {
  return Word(Builtin.or_Int64(lhs.value, rhs.value))
}
func | (lhs: UWord, rhs: UWord) -> UWord {
  return UWord(Builtin.or_Int64(lhs.value, rhs.value))
}

func [assignment] -= (lhs : [byref] UWord, rhs : UWord) {
  lhs = lhs - rhs
}

func [assignment] *= (lhs : [byref] Word, rhs : Word) {
  lhs = lhs * rhs
}
func [assignment] *= (lhs : [byref] UWord, rhs : UWord) {
  lhs = lhs * rhs
}

func [assignment] /= (lhs : [byref] Word, rhs : Word) {
  lhs = lhs / rhs
}
func [assignment] /= (lhs : [byref] UWord, rhs : UWord) {
  lhs = lhs / rhs
}

func [assignment] %= (lhs : [byref] Word, rhs : Word) {
  lhs = lhs % rhs
}
func [assignment] %= (lhs : [byref] UWord, rhs : UWord) {
  lhs = lhs % rhs
}

func [assignment] <<= (lhs : [byref] Word, rhs : Word) {
  lhs = lhs << rhs
}
func [assignment] <<= (lhs : [byref] UWord, rhs : UWord) {
  lhs = lhs << rhs
}

func [assignment] >>= (lhs : [byref] Word, rhs : Word) {
  lhs = lhs >> rhs
}
func [assignment] >>= (lhs : [byref] UWord, rhs : UWord) {
  lhs = lhs >> rhs
}

func [assignment] &= (lhs : [byref] Word, rhs : Word) {
  lhs = lhs & rhs
}
func [assignment] &= (lhs : [byref] UWord, rhs : UWord) {
  lhs = lhs & rhs
}

func [assignment] |= (lhs : [byref] Word, rhs : Word) {
  lhs = lhs | rhs
}
func [assignment] |= (lhs : [byref] UWord, rhs : UWord) {
  lhs = lhs | rhs
}

func [assignment] ^= (lhs : [byref] Word, rhs : Word) {
  lhs = lhs ^ rhs
}
func [assignment] ^= (lhs : [byref] UWord, rhs : UWord) {
  lhs = lhs ^ rhs
}
