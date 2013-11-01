struct Word : BuiltinIntegerLiteralConvertible, IntegerLiteralConvertible, 
              ArrayBound, ReplPrintable {
  var value : Builtin.Word

  @transparent
  static func _convertFromBuiltinIntegerLiteral(val : Builtin.Int2048) -> Word {
    return Word(Builtin.trunc_Int2048_Word(val))
  }

  typealias IntegerLiteralType = Word
  @transparent
  static func convertFromIntegerLiteral(value : Word) -> Word {
    return value
  }

  func _getBuiltinArrayBoundValue() -> Builtin.Word {
    return value
  }

  typealias ArrayBoundType = Word
  func getArrayBoundValue() -> Word {
    return self
  }

  func replPrint() {
    print(Int64(self))
  }
}

@transparent
extension Word : RandomAccessIndex, BidirectionalIndex {
  func __equal__(rhs: Word) -> Bool {
    return _getBool(Builtin.cmp_eq_Word(value, rhs.value))
  }
  func __less__(rhs: Word) -> Bool {
    return _getBool(Builtin.cmp_slt_Word(value, rhs.value))
  }
  func hashValue() -> Int {
    return Int(value)
  }
  func succ() -> Word {
    return self + 1
  }
  func pred() -> Word {
    return self - 1
  }
  typealias DistanceType = Word
  static func sub(lhs: Word, rhs: Word) -> (DistanceType, Bool) {
    var tmp = Builtin.int_ssub_with_overflow_Word(lhs.value, rhs.value)
    return (Word(tmp.0), _getBool(tmp.1))
  }
  static func add(lhs: Word, rhs: DistanceType) -> (Word, Bool) {
    var tmp = Builtin.int_sadd_with_overflow_Word(lhs.value, rhs.value)
    return (Word(tmp.0), _getBool(tmp.1))
  }
  static func mul(lhs: Word, rhs: Word) -> (Word, Bool) {
    var tmp = Builtin.int_smul_with_overflow_Word(lhs.value, rhs.value)
    return (Word(tmp.0), _getBool(tmp.1))
  }
}

extension Word : SignedNumber {
  static func negate(rhs: Word) -> (Word, Bool) { return Word.sub(0, rhs) }
  func isNegative() -> Bool { return self < 0 }
}

struct UWord : BuiltinIntegerLiteralConvertible, IntegerLiteralConvertible {
  var value : Builtin.Word

  @transparent
  static func _convertFromBuiltinIntegerLiteral(val : Builtin.Int2048) -> UWord {
    return UWord(Builtin.trunc_Int2048_Word(val))
  }

  typealias IntegerLiteralType = UWord
  @transparent
  static func convertFromIntegerLiteral(value : UWord) -> UWord {
    return value
  }

  func replPrint() {
    print(UInt64(self))
  }
}

@transparent
extension UWord : RandomAccessIndex {
  func __equal__(rhs: UWord) -> Bool {
    return _getBool(Builtin.cmp_eq_Word(value, rhs.value))
  }
  func __less__(rhs: UWord) -> Bool {
    return _getBool(Builtin.cmp_ult_Word(value, rhs.value))
  }
  func succ() -> UWord {
    return self + 1
  }
  func pred() -> UWord {
    return self - 1
  }
  typealias DistanceType = UWord
  static func sub(lhs: UWord, rhs: UWord) -> (DistanceType, Bool) {
    var tmp = Builtin.int_usub_with_overflow_Word(lhs.value, rhs.value)
    return (UWord(tmp.0), _getBool(tmp.1))
  }
  static func add(lhs: UWord, rhs: DistanceType) -> (UWord, Bool) {
    var tmp = Builtin.int_uadd_with_overflow_Word(lhs.value, rhs.value)
    return (UWord(tmp.0), _getBool(tmp.1))
  }
  static func mul(lhs: UWord, rhs: UWord) -> (UWord, Bool) {
    var tmp = Builtin.int_umul_with_overflow_Word(lhs.value, rhs.value)
    return (UWord(tmp.0), _getBool(tmp.1))
  }
}

//===----------------------------------------------------------------------===//
// Explicit conversions between types.
//===----------------------------------------------------------------------===//

@transparent
extension Word {
  init(v : Int32) {
    value = Builtin.sext_Int32_Word(v.value)
  }
  init(v : UInt32) {
    value = Builtin.zext_Int32_Word(v.value)
  }

  init(v : Int64) {
    value = Builtin.trunc_Int64_Word(v.value)
  }
  init(v : UInt64) {
    value = Builtin.trunc_Int64_Word(v.value)
  }
}

@transparent
extension UWord {
  init(v : Int32) {
    value = Builtin.sext_Int32_Word(v.value)
  }
  init(v : UInt32) {
    value = Builtin.zext_Int32_Word(v.value)
  }

  init(v : Int64) {
    value = Builtin.trunc_Int64_Word(v.value)
  }
  init(v : UInt64) {
    value = Builtin.trunc_Int64_Word(v.value)
  }
}

@transparent
extension Int8 {
  init(v : Word) {
    value = Builtin.trunc_Word_Int8(v.value)
  }
  init(v : UWord) {
    value = Builtin.trunc_Word_Int8(v.value)
  }
}

@transparent
extension UInt8 {
  init(v : Word) {
    value = Builtin.trunc_Word_Int8(v.value)
  }
  init(v : UWord) {
    value = Builtin.trunc_Word_Int8(v.value)
  }
}

@transparent
extension Int16 {
  init(v : Word) {
    value = Builtin.trunc_Word_Int16(v.value)
  }
  init(v : UWord) {
    value = Builtin.trunc_Word_Int16(v.value)
  }
}

@transparent
extension UInt16 {
  init(v : Word) {
    value = Builtin.trunc_Word_Int16(v.value)
  }
  init(v : UWord) {
    value = Builtin.trunc_Word_Int16(v.value)
  }
}

@transparent
extension Int32 {
  init(v : Word) {
    value = Builtin.trunc_Word_Int32(v.value)
  }
  init(v : UWord) {
    value = Builtin.trunc_Word_Int32(v.value)
  }
}

@transparent
extension UInt32 {
  init(v : Word) {
    value = Builtin.trunc_Word_Int32(v.value)
  }
  init(v : UWord) {
    value = Builtin.trunc_Word_Int32(v.value)
  }
}

@transparent
extension Int64 {
  init(v : Word) {
    value = Builtin.sext_Word_Int64(v.value)
  }
  init(v : UWord) {
    value = Builtin.zext_Word_Int64(v.value)
  }
}

@transparent
extension UInt64 {
  init(v : Word) {
    value = Builtin.sext_Word_Int64(v.value)
  }
  init(v : UWord) {
    value = Builtin.zext_Word_Int64(v.value)
  }
}

//===----------------------------------------------------------------------===//
// Standard Operators
//===----------------------------------------------------------------------===//

@prefix func +(a : Word)  -> Word   { return a }
@prefix func +(a : UWord) -> UWord  { return a }
@prefix func ~(a : Word)  -> Word  { return a ^ Word.min() }
@prefix func ~(a : UWord) -> UWord { return a ^ UWord.max() }

func * (lhs : Word, rhs : Word) -> Word {
  return Word(Builtin.mul_Word(lhs.value, rhs.value))
}
func * (lhs : UWord, rhs : UWord) -> UWord {
  return UWord(Builtin.mul_Word(lhs.value, rhs.value))
}

func / (lhs : Word, rhs : Word) -> Word {
  return Word(Builtin.sdiv_Word(lhs.value, rhs.value))
}
func / (lhs : UWord, rhs : UWord) -> UWord {
  return UWord(Builtin.udiv_Word(lhs.value, rhs.value))
}

func % (lhs : Word, rhs : Word) -> Word {
  return Word(Builtin.srem_Word(lhs.value, rhs.value))
}
func % (lhs : UWord, rhs : UWord) -> UWord {
  return UWord(Builtin.urem_Word(lhs.value, rhs.value))
}

func + (lhs: Word, rhs: Word) -> Word {
  return Word(Builtin.add_Word(lhs.value, rhs.value))
}
func + (lhs: UWord, rhs: UWord) -> UWord {
  return UWord(Builtin.add_Word(lhs.value, rhs.value))
}

func - (lhs: Word, rhs: Word) -> Word {
  return Word(Builtin.sub_Word(lhs.value, rhs.value))
}
func - (lhs: UWord, rhs: UWord) -> UWord {
  return UWord(Builtin.sub_Word(lhs.value, rhs.value))
}

func << (lhs : Word, rhs : Word) -> Word {
  return Word(Builtin.shl_Word(lhs.value, rhs.value))
}
func << (lhs : UWord, rhs : UWord) -> UWord {
  return UWord(Builtin.shl_Word(lhs.value, rhs.value))
}

func >>(lhs : Word, rhs : Word) -> Word {
  return Word(Builtin.ashr_Word(lhs.value, rhs.value))
}
func >>(lhs : UWord, rhs : UWord) -> UWord {
  return UWord(Builtin.lshr_Word(lhs.value, rhs.value))
}

func & (lhs: Word, rhs: Word) -> Word {
  return Word(Builtin.and_Word(lhs.value, rhs.value))
}
func & (lhs: UWord, rhs: UWord) -> UWord {
  return UWord(Builtin.and_Word(lhs.value, rhs.value))
}

func ^ (lhs: Word, rhs: Word) -> Word {
  return Word(Builtin.xor_Word(lhs.value, rhs.value))
}
func ^ (lhs: UWord, rhs: UWord) -> UWord {
  return UWord(Builtin.xor_Word(lhs.value, rhs.value))
}

func | (lhs: Word, rhs: Word) -> Word {
  return Word(Builtin.or_Word(lhs.value, rhs.value))
}
func | (lhs: UWord, rhs: UWord) -> UWord {
  return UWord(Builtin.or_Word(lhs.value, rhs.value))
}

@assignment func -= (lhs : @inout UWord, rhs : UWord) {
  lhs = lhs - rhs
}

@assignment func *= (lhs : @inout Word, rhs : Word) {
  lhs = lhs * rhs
}
@assignment func *= (lhs : @inout UWord, rhs : UWord) {
  lhs = lhs * rhs
}

@assignment func /= (lhs : @inout Word, rhs : Word) {
  lhs = lhs / rhs
}
@assignment func /= (lhs : @inout UWord, rhs : UWord) {
  lhs = lhs / rhs
}

@assignment func %= (lhs : @inout Word, rhs : Word) {
  lhs = lhs % rhs
}
@assignment func %= (lhs : @inout UWord, rhs : UWord) {
  lhs = lhs % rhs
}

@assignment func <<= (lhs : @inout Word, rhs : Word) {
  lhs = lhs << rhs
}
@assignment func <<= (lhs : @inout UWord, rhs : UWord) {
  lhs = lhs << rhs
}

@assignment func >>= (lhs : @inout Word, rhs : Word) {
  lhs = lhs >> rhs
}
@assignment func >>= (lhs : @inout UWord, rhs : UWord) {
  lhs = lhs >> rhs
}

@assignment func &= (lhs : @inout Word, rhs : Word) {
  lhs = lhs & rhs
}
@assignment func &= (lhs : @inout UWord, rhs : UWord) {
  lhs = lhs & rhs
}

@assignment func |= (lhs : @inout Word, rhs : Word) {
  lhs = lhs | rhs
}
@assignment func |= (lhs : @inout UWord, rhs : UWord) {
  lhs = lhs | rhs
}

@assignment func ^= (lhs : @inout Word, rhs : Word) {
  lhs = lhs ^ rhs
}
@assignment func ^= (lhs : @inout UWord, rhs : UWord) {
  lhs = lhs ^ rhs
}
