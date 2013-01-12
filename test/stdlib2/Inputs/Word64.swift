struct Word : Comparable {
  var value : Builtin.Int64

  static func convertFromIntegerLiteral(val : Builtin.Int64) -> Word {
    return Word(val)
  }
  func getArrayBoundValue() -> Builtin.Int64 {
    return value
  }

  func replPrint() {
    print(Int64(this))
  }
  // FIXME:
  // static var max : Word { get { return 0x7FFFFFFFFFFFFFFF } }
  // static var min : Word { get { return -0x7FFFFFFFFFFFFFFF-1 } }
  static func max() -> Word { return 0x7FFFFFFFFFFFFFFF }
  static func min() -> Word { return -0x7FFFFFFFFFFFFFFF-1 }
  static func bitSize() -> Word { return 64 }
}

struct UWord : Comparable {
  var value : Builtin.Int64

  static func convertFromIntegerLiteral(val : Builtin.Int64) -> UWord {
    return UWord(val)
  }

  func replPrint() {
    print(UInt64(this))
  }
  // FIXME:
  // static var max : UWord { get { return 0xFFFFFFFFFFFFFFFF } }
  // static var min : UWord { get { return 0 } }
  static func max() -> UWord { return 0xFFFFFFFFFFFFFFFF }
  static func min() -> UWord { return 0 }
  static func bitSize() -> UWord { return 64 }
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
  constructor(v : Word) {
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

func -(a : Word) -> Word   { return 0 - a }
func +(a : Word)  -> Word   { return a }
func +(a : UWord) -> UWord  { return a }
func ~(a : Word)  -> Word  { return a ^ Word.min() }
func ~(a : UWord) -> UWord { return a ^ UWord.max() }

func [assignment] ++(a : [byref] Word) { a += 1 }
func [assignment] ++(a : [byref] UWord) { a += 1 }
func [assignment] --(a : [byref] Word) { a -= 1 }
func [assignment] --(a : [byref] UWord) { a -= 1 }

func [infix_left=200] * (lhs : Word, rhs : Word) -> Word {
  return Word(Builtin.mul_Int64(lhs.value, rhs.value))
}
func [infix_left=200] * (lhs : UWord, rhs : UWord) -> UWord {
  return UWord(Builtin.mul_Int64(lhs.value, rhs.value))
}

func [infix_left=200] / (lhs : Word, rhs : Word) -> Word {
  return Word(Builtin.sdiv_Int64(lhs.value, rhs.value))
}
func [infix_left=200] / (lhs : UWord, rhs : UWord) -> UWord {
  return UWord(Builtin.udiv_Int64(lhs.value, rhs.value))
}

func [infix_left=200] % (lhs : Word, rhs : Word) -> Word {
  return Word(Builtin.srem_Int64(lhs.value, rhs.value))
}
func [infix_left=200] % (lhs : UWord, rhs : UWord) -> UWord {
  return UWord(Builtin.urem_Int64(lhs.value, rhs.value))
}

func [infix_left=190] + (lhs: Word, rhs: Word) -> Word {
  return Word(Builtin.add_Int64(lhs.value, rhs.value))
}
func [infix_left=190] + (lhs: UWord, rhs: UWord) -> UWord {
  return UWord(Builtin.add_Int64(lhs.value, rhs.value))
}

func [infix_left=190] - (lhs: Word, rhs: Word) -> Word {
  return Word(Builtin.sub_Int64(lhs.value, rhs.value))
}
func [infix_left=190] - (lhs: UWord, rhs: UWord) -> UWord {
  return UWord(Builtin.sub_Int64(lhs.value, rhs.value))
}

func [infix=180] << (lhs : Word, rhs : Word) -> Word {
  return Word(Builtin.shl_Int64(lhs.value, rhs.value))
}
func [infix=180] << (lhs : UWord, rhs : UWord) -> UWord {
  return UWord(Builtin.shl_Int64(lhs.value, rhs.value))
}

func [infix=180] >>(lhs : Word, rhs : Word) -> Word {
  return Word(Builtin.ashr_Int64(lhs.value, rhs.value))
}
func [infix=180] >>(lhs : UWord, rhs : UWord) -> UWord {
  return UWord(Builtin.lshr_Int64(lhs.value, rhs.value))
}

// Less-Than Comparison.
func [infix=170] < (lhs : Word, rhs : Word) -> Bool {
  return _getBool(Builtin.cmp_slt_Int64(lhs.value, rhs.value))
}
func [infix=170] < (lhs : UWord, rhs : UWord) -> Bool {
  return _getBool(Builtin.cmp_ult_Int64(lhs.value, rhs.value))
}

// Greater-Than Comparison.
func [infix=170] > (lhs : Word, rhs : Word) -> Bool {
  return _getBool(Builtin.cmp_sgt_Int64(lhs.value, rhs.value))
}
func [infix=170] > (lhs : UWord, rhs : UWord) -> Bool {
  return _getBool(Builtin.cmp_ugt_Int64(lhs.value, rhs.value))
}

// Less-Than-Or-Equal Comparison.
func [infix=170] <= (lhs : Word, rhs : Word) -> Bool {
  return _getBool(Builtin.cmp_sle_Int64(lhs.value, rhs.value))
}
func [infix=170] <= (lhs : UWord, rhs : UWord) -> Bool {
  return _getBool(Builtin.cmp_ule_Int64(lhs.value, rhs.value))
}

func [infix=170] >= (lhs : Word, rhs : Word) -> Bool {
  return _getBool(Builtin.cmp_sge_Int64(lhs.value, rhs.value))
}
func [infix=170] >= (lhs : UWord, rhs : UWord) -> Bool {
  return _getBool(Builtin.cmp_uge_Int64(lhs.value, rhs.value))
}

func [infix=160] == (lhs : Word, rhs : Word) -> Bool {
  return _getBool(Builtin.cmp_eq_Int64(lhs.value, rhs.value))
}
func [infix=160] == (lhs : UWord, rhs : UWord) -> Bool {
  return _getBool(Builtin.cmp_eq_Int64(lhs.value, rhs.value))
}

func [infix=160] != (lhs : Word, rhs : Word) -> Bool {
  return _getBool(Builtin.cmp_ne_Int64(lhs.value, rhs.value))
}
func [infix=160] != (lhs : UWord, rhs : UWord) -> Bool {
  return _getBool(Builtin.cmp_ne_Int64(lhs.value, rhs.value))
}

func [infix_left=150] & (lhs: Word, rhs: Word) -> Word {
  return Word(Builtin.and_Int64(lhs.value, rhs.value))
}
func [infix_left=150] & (lhs: UWord, rhs: UWord) -> UWord {
  return UWord(Builtin.and_Int64(lhs.value, rhs.value))
}

func [infix_left=140] ^ (lhs: Word, rhs: Word) -> Word {
  return Word(Builtin.xor_Int64(lhs.value, rhs.value))
}
func [infix_left=140] ^ (lhs: UWord, rhs: UWord) -> UWord {
  return UWord(Builtin.xor_Int64(lhs.value, rhs.value))
}

func [infix_left=130] | (lhs: Word, rhs: Word) -> Word {
  return Word(Builtin.or_Int64(lhs.value, rhs.value))
}
func [infix_left=130] | (lhs: UWord, rhs: UWord) -> UWord {
  return UWord(Builtin.or_Int64(lhs.value, rhs.value))
}

func [assignment,infix=90] += (lhs : [byref] Word, rhs : Word) {
  lhs = lhs + rhs
}
func [assignment,infix=90] += (lhs : [byref] UWord, rhs : UWord) {
  lhs = lhs + rhs
}

func [assignment,infix=90] -= (lhs : [byref] Word, rhs : Word) {
  lhs = lhs - rhs
}
func [assignment,infix=90] -= (lhs : [byref] UWord, rhs : UWord) {
  lhs = lhs - rhs
}

func [assignment,infix=90] *= (lhs : [byref] Word, rhs : Word) {
  lhs = lhs * rhs
}
func [assignment,infix=90] *= (lhs : [byref] UWord, rhs : UWord) {
  lhs = lhs * rhs
}

func [assignment,infix=90] /= (lhs : [byref] Word, rhs : Word) {
  lhs = lhs / rhs
}
func [assignment,infix=90] /= (lhs : [byref] UWord, rhs : UWord) {
  lhs = lhs / rhs
}

func [assignment,infix=90] %= (lhs : [byref] Word, rhs : Word) {
  lhs = lhs % rhs
}
func [assignment,infix=90] %= (lhs : [byref] UWord, rhs : UWord) {
  lhs = lhs % rhs
}

func [assignment,infix=90] <<= (lhs : [byref] Word, rhs : Word) {
  lhs = lhs << rhs
}
func [assignment,infix=90] <<= (lhs : [byref] UWord, rhs : UWord) {
  lhs = lhs << rhs
}

func [assignment,infix=90] >>= (lhs : [byref] Word, rhs : Word) {
  lhs = lhs >> rhs
}
func [assignment,infix=90] >>= (lhs : [byref] UWord, rhs : UWord) {
  lhs = lhs >> rhs
}

func [assignment,infix=90] &= (lhs : [byref] Word, rhs : Word) {
  lhs = lhs & rhs
}
func [assignment,infix=90] &= (lhs : [byref] UWord, rhs : UWord) {
  lhs = lhs & rhs
}

func [assignment,infix=90] |= (lhs : [byref] Word, rhs : Word) {
  lhs = lhs | rhs
}
func [assignment,infix=90] |= (lhs : [byref] UWord, rhs : UWord) {
  lhs = lhs | rhs
}

func [assignment,infix=90] ^= (lhs : [byref] Word, rhs : Word) {
  lhs = lhs ^ rhs
}
func [assignment,infix=90] ^= (lhs : [byref] UWord, rhs : UWord) {
  lhs = lhs ^ rhs
}
