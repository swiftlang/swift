
//// Automatically Generated From FixedPoint.gyb.  Do Not Edit Directly ////
//// To regenerate:                                                     ////
//// ../../utils/gyb -DWORD=32 FixedPoint.gyb -o FixedPoint32.swift ////
//// ../../utils/gyb -DWORD=64 FixedPoint.gyb -o FixedPoint64.swift ////


struct UInt8 : BuiltinIntegerLiteralConvertible, IntegerLiteralConvertible,
                ArrayBound, ReplPrintable {
  var value: Builtin.Int8

  @transparent
  init() {
    var maxWidthZero: Int64 = 0
    value = Builtin.truncOrBitCast_Int64_Int8(maxWidthZero.value)
  }

  @transparent
  init(v: Builtin.Int8) {
    value = v
  }

  @transparent
  type func _convertFromBuiltinIntegerLiteral(val: Builtin.Int2048) -> UInt8 {
    return UInt8(Builtin.s_to_u_checked_trunc_Int2048_Int8(val).0)
  }

  @transparent
  type func convertFromIntegerLiteral(value: UInt8) -> UInt8 {
    return value
  }
  @transparent
  func _getBuiltinArrayBoundValue() -> Builtin.Word {
    return Builtin.zextOrBitCast_Int8_Word(self.value)
  }

  typealias ArrayBoundType = UInt8
  func getArrayBoundValue() -> UInt8 {
    return self
  }

  func replPrint() {
    print(UInt64(self))
  }

  @transparent
  type var max: UInt8 { return 0xFF }
  @transparent
  type var min: UInt8 { return 0 }
}

extension UInt8 : Hashable {
  func hashValue() -> Int {
    return Int(Builtin.sextOrBitCast_Int8_Word(self.value))
  }
}

@transparent
extension UInt8 : RandomAccessIndex {
  @transparent
  func succ() -> UInt8 {
    return self + 1
  }
  @transparent
  func pred() -> UInt8 {
    return self - 1
  }
  typealias DistanceType = UInt8
  @transparent
  type func sub(lhs: UInt8, rhs: UInt8, reportOverflow: Bool) -> (DistanceType, Bool) {
    var tmp = Builtin.usub_with_overflow_Int8(lhs.value, rhs.value, reportOverflow.value)
    return (UInt8(tmp.0), Bool(tmp.1))
  }
  @transparent
  type func add(lhs: UInt8, rhs: DistanceType, reportOverflow: Bool) -> (UInt8, Bool) {
    var tmp = Builtin.uadd_with_overflow_Int8(lhs.value, rhs.value, reportOverflow.value)
    return (UInt8(tmp.0), Bool(tmp.1))
  }
  @transparent
  type func mul(lhs: UInt8, rhs: UInt8, reportOverflow: Bool) -> (UInt8, Bool) {
    var tmp = Builtin.umul_with_overflow_Int8(lhs.value, rhs.value, reportOverflow.value)
    return (UInt8(tmp.0), Bool(tmp.1))
  }
  @transparent
  type func div(lhs: UInt8, rhs: UInt8, reportOverflow: Bool) -> (UInt8, Bool) {
    if rhs == 0 {
      return (0, true)
    }
    var tmp = Builtin.udiv_Int8(lhs.value, rhs.value)
    return (UInt8(tmp), false)
  }
  @transparent
  type func rem(lhs: UInt8, rhs: UInt8, reportOverflow: Bool) -> (UInt8, Bool) {
    if rhs == 0 {
      return (0, true)
    }
    var tmp = Builtin.urem_Int8(lhs.value, rhs.value)
    return (UInt8(tmp), false)
  }

  @transparent
  type func sub(lhs: UInt8, rhs: UInt8) -> (DistanceType, Bool) {
    return sub(lhs, rhs, true)
  }
  @transparent
  type func add(lhs: UInt8, rhs: DistanceType) -> (UInt8, Bool) {
    return add(lhs, rhs, true)
  }
  @transparent
  type func mul(lhs: UInt8, rhs: UInt8) -> (UInt8, Bool) {
    return mul(lhs, rhs, true)
  }
  @transparent
  type func div(lhs: UInt8, rhs: UInt8) -> (UInt8, Bool) {
    return div(lhs, rhs, true)
  }
  @transparent
  type func rem(lhs: UInt8, rhs: UInt8) -> (UInt8, Bool) {
    return rem(lhs, rhs, true)
  }
}


// construction from other integer types
@transparent
extension UInt8 {
  init(v: Int8) {
    var srcNotWord = v.value
    var tmp = Builtin.s_to_u_checked_conversion_Int8(srcNotWord)
    Builtin.condfail(tmp.1)
    var dstNotWord = tmp.0
    value = dstNotWord
  }

  init(v: UInt16) {
    var srcNotWord = v.value
    var tmp = Builtin.u_to_u_checked_trunc_Int16_Int8(srcNotWord)
    Builtin.condfail(tmp.1)
    var dstNotWord = tmp.0
    value = dstNotWord
  }

  init(v: Int16) {
    var srcNotWord = v.value
    var tmp = Builtin.s_to_u_checked_trunc_Int16_Int8(srcNotWord)
    Builtin.condfail(tmp.1)
    var dstNotWord = tmp.0
    value = dstNotWord
  }

  init(v: UInt32) {
    var srcNotWord = v.value
    var tmp = Builtin.u_to_u_checked_trunc_Int32_Int8(srcNotWord)
    Builtin.condfail(tmp.1)
    var dstNotWord = tmp.0
    value = dstNotWord
  }

  init(v: Int32) {
    var srcNotWord = v.value
    var tmp = Builtin.s_to_u_checked_trunc_Int32_Int8(srcNotWord)
    Builtin.condfail(tmp.1)
    var dstNotWord = tmp.0
    value = dstNotWord
  }

  init(v: UInt64) {
    var srcNotWord = v.value
    var tmp = Builtin.u_to_u_checked_trunc_Int64_Int8(srcNotWord)
    Builtin.condfail(tmp.1)
    var dstNotWord = tmp.0
    value = dstNotWord
  }

  init(v: Int64) {
    var srcNotWord = v.value
    var tmp = Builtin.s_to_u_checked_trunc_Int64_Int8(srcNotWord)
    Builtin.condfail(tmp.1)
    var dstNotWord = tmp.0
    value = dstNotWord
  }

  init(v: UInt) {
    var srcNotWord = Builtin.truncOrBitCast_Word_Int32(v.value)
    var tmp = Builtin.u_to_u_checked_trunc_Int32_Int8(srcNotWord)
    Builtin.condfail(tmp.1)
    var dstNotWord = tmp.0
    value = dstNotWord
  }

  init(v: Int) {
    var srcNotWord = Builtin.truncOrBitCast_Word_Int32(v.value)
    var tmp = Builtin.s_to_u_checked_trunc_Int32_Int8(srcNotWord)
    Builtin.condfail(tmp.1)
    var dstNotWord = tmp.0
    value = dstNotWord
  }

  func asSigned() -> Int8 {
    return Int8(value)
  }
}

// Operations with masking and non-masking versions
@transparent
func &+ (lhs: UInt8, rhs: UInt8) -> UInt8 {
  return UInt8.add(lhs, rhs, false).0
}
@transparent
func + (lhs: UInt8, rhs: UInt8) -> UInt8 {
  var tmp = UInt8.add(lhs, rhs)
  Builtin.condfail(tmp.1.value)
  return tmp.0
}
@transparent
func &* (lhs: UInt8, rhs: UInt8) -> UInt8 {
  return UInt8.mul(lhs, rhs, false).0
}
@transparent
func * (lhs: UInt8, rhs: UInt8) -> UInt8 {
  var tmp = UInt8.mul(lhs, rhs)
  Builtin.condfail(tmp.1.value)
  return tmp.0
}
@transparent
func &- (lhs: UInt8, rhs: UInt8) -> UInt8 {
  return UInt8.sub(lhs, rhs, false).0
}
@transparent
func - (lhs: UInt8, rhs: UInt8) -> UInt8 {
  var tmp = UInt8.sub(lhs, rhs)
  Builtin.condfail(tmp.1.value)
  return tmp.0
}

// Bitwise negate
@transparent @prefix
func ~(rhs: UInt8) -> UInt8 {
  let mask = UInt8.sub(0, 1).0
  return UInt8(Builtin.xor_Int8(rhs.value, mask.value))
}

@transparent
func == (lhs: UInt8, rhs: UInt8) -> Bool {
  return Bool(Builtin.cmp_eq_Int8(lhs.value, rhs.value))
}
@transparent
func != (lhs: UInt8, rhs: UInt8) -> Bool {
  return Bool(Builtin.cmp_ne_Int8(lhs.value, rhs.value))
}
@transparent
func < (lhs: UInt8, rhs: UInt8) -> Bool {
  return Bool(Builtin.cmp_ult_Int8(lhs.value, rhs.value))
}
@transparent
func <= (lhs: UInt8, rhs: UInt8) -> Bool {
  return Bool(Builtin.cmp_ule_Int8(lhs.value, rhs.value))
}
@transparent
func > (lhs: UInt8, rhs: UInt8) -> Bool {
  return Bool(Builtin.cmp_ugt_Int8(lhs.value, rhs.value))
}
@transparent
func >= (lhs: UInt8, rhs: UInt8) -> Bool {
  return Bool(Builtin.cmp_uge_Int8(lhs.value, rhs.value))
}

@transparent
func << (lhs: UInt8, rhs: UInt8) -> UInt8 {
  return UInt8(Builtin.shl_Int8(lhs.value, rhs.value))
}
@transparent
func >> (lhs: UInt8, rhs: UInt8) -> UInt8 {
  return UInt8(Builtin.lshr_Int8(lhs.value, rhs.value))
}
@transparent
func & (lhs: UInt8, rhs: UInt8) -> UInt8 {
  return UInt8(Builtin.and_Int8(lhs.value, rhs.value))
}
@transparent
func ^ (lhs: UInt8, rhs: UInt8) -> UInt8 {
  return UInt8(Builtin.xor_Int8(lhs.value, rhs.value))
}
@transparent
func | (lhs: UInt8, rhs: UInt8) -> UInt8 {
  return UInt8(Builtin.or_Int8(lhs.value, rhs.value))
}

// bitwise operations
@transparent
extension UInt8 : BitwiseOperations {
  type func allZeros() -> UInt8 { return 0 }
}

// Compound assignments
@transparent @assignment
func +=(lhs: @inout UInt8, rhs: UInt8) {
  lhs = lhs + rhs
}
@transparent @assignment
func -=(lhs: @inout UInt8, rhs: UInt8) {
  lhs = lhs - rhs
}
@transparent @assignment
func *=(lhs: @inout UInt8, rhs: UInt8) {
  lhs = lhs * rhs
}
@transparent @assignment
func <<=(lhs: @inout UInt8, rhs: UInt8) {
  lhs = lhs << rhs
}
@transparent @assignment
func >>=(lhs: @inout UInt8, rhs: UInt8) {
  lhs = lhs >> rhs
}
@transparent @assignment
func &=(lhs: @inout UInt8, rhs: UInt8) {
  lhs = lhs & rhs
}
@transparent @assignment
func |=(lhs: @inout UInt8, rhs: UInt8) {
  lhs = lhs | rhs
}
@transparent @assignment
func ^=(lhs: @inout UInt8, rhs: UInt8) {
  lhs = lhs ^ rhs
}

struct Int8 : BuiltinIntegerLiteralConvertible, IntegerLiteralConvertible,
                ArrayBound, ReplPrintable {
  var value: Builtin.Int8

  @transparent
  init() {
    var maxWidthZero: Int64 = 0
    value = Builtin.truncOrBitCast_Int64_Int8(maxWidthZero.value)
  }

  @transparent
  init(v: Builtin.Int8) {
    value = v
  }

  @transparent
  type func _convertFromBuiltinIntegerLiteral(val: Builtin.Int2048) -> Int8 {
    return Int8(Builtin.s_to_s_checked_trunc_Int2048_Int8(val).0)
  }

  @transparent
  type func convertFromIntegerLiteral(value: Int8) -> Int8 {
    return value
  }
  @transparent
  func _getBuiltinArrayBoundValue() -> Builtin.Word {
    return Builtin.sextOrBitCast_Int8_Word(self.value)
  }

  typealias ArrayBoundType = Int8
  func getArrayBoundValue() -> Int8 {
    return self
  }

  func replPrint() {
    print(Int64(self))
  }

  @transparent
  type var max: Int8 { return 0x7F }
  @transparent
  type var min: Int8 { return -0x7F-1 }
}

extension Int8 : Hashable {
  func hashValue() -> Int {
    return Int(Builtin.sextOrBitCast_Int8_Word(self.value))
  }
}

@transparent
extension Int8 : RandomAccessIndex {
  @transparent
  func succ() -> Int8 {
    return self + 1
  }
  @transparent
  func pred() -> Int8 {
    return self - 1
  }
  typealias DistanceType = Int8
  @transparent
  type func sub(lhs: Int8, rhs: Int8, reportOverflow: Bool) -> (DistanceType, Bool) {
    var tmp = Builtin.ssub_with_overflow_Int8(lhs.value, rhs.value, reportOverflow.value)
    return (Int8(tmp.0), Bool(tmp.1))
  }
  @transparent
  type func add(lhs: Int8, rhs: DistanceType, reportOverflow: Bool) -> (Int8, Bool) {
    var tmp = Builtin.sadd_with_overflow_Int8(lhs.value, rhs.value, reportOverflow.value)
    return (Int8(tmp.0), Bool(tmp.1))
  }
  @transparent
  type func mul(lhs: Int8, rhs: Int8, reportOverflow: Bool) -> (Int8, Bool) {
    var tmp = Builtin.smul_with_overflow_Int8(lhs.value, rhs.value, reportOverflow.value)
    return (Int8(tmp.0), Bool(tmp.1))
  }
  @transparent
  type func div(lhs: Int8, rhs: Int8, reportOverflow: Bool) -> (Int8, Bool) {
    if rhs == 0 {
      return (0, true)
    }
    if lhs == Int8.min && rhs == -1 {
      return (lhs, true)
    }
    var tmp = Builtin.sdiv_Int8(lhs.value, rhs.value)
    return (Int8(tmp), false)
  }
  @transparent
  type func rem(lhs: Int8, rhs: Int8, reportOverflow: Bool) -> (Int8, Bool) {
    if rhs == 0 {
      return (0, true)
    }
    if lhs == Int8.min && rhs == -1 {
      return (0, true)
    }
    var tmp = Builtin.srem_Int8(lhs.value, rhs.value)
    return (Int8(tmp), false)
  }

  @transparent
  type func sub(lhs: Int8, rhs: Int8) -> (DistanceType, Bool) {
    return sub(lhs, rhs, true)
  }
  @transparent
  type func add(lhs: Int8, rhs: DistanceType) -> (Int8, Bool) {
    return add(lhs, rhs, true)
  }
  @transparent
  type func mul(lhs: Int8, rhs: Int8) -> (Int8, Bool) {
    return mul(lhs, rhs, true)
  }
  @transparent
  type func div(lhs: Int8, rhs: Int8) -> (Int8, Bool) {
    return div(lhs, rhs, true)
  }
  @transparent
  type func rem(lhs: Int8, rhs: Int8) -> (Int8, Bool) {
    return rem(lhs, rhs, true)
  }
}

@transparent
extension Int8 : SignedNumber {
  @transparent
  type func negate(rhs: Int8) -> (Int8, Bool) {
    return Int8.sub(0, rhs)
  }
  @transparent
  type func abs(rhs: Int8) -> (Int8, Bool) {
    return rhs.isNegative() ? Int8.negate(rhs) : (rhs, false)
  }
  @transparent
  func isNegative() -> Bool { return self < 0 }
}

// construction from other integer types
@transparent
extension Int8 {
  init(v: UInt8) {
    var srcNotWord = v.value
    var tmp = Builtin.u_to_s_checked_conversion_Int8(srcNotWord)
    Builtin.condfail(tmp.1)
    var dstNotWord = tmp.0
    value = dstNotWord
  }

  init(v: UInt16) {
    var srcNotWord = v.value
    var tmp = Builtin.u_to_s_checked_trunc_Int16_Int8(srcNotWord)
    Builtin.condfail(tmp.1)
    var dstNotWord = tmp.0
    value = dstNotWord
  }

  init(v: Int16) {
    var srcNotWord = v.value
    var tmp = Builtin.s_to_s_checked_trunc_Int16_Int8(srcNotWord)
    Builtin.condfail(tmp.1)
    var dstNotWord = tmp.0
    value = dstNotWord
  }

  init(v: UInt32) {
    var srcNotWord = v.value
    var tmp = Builtin.u_to_s_checked_trunc_Int32_Int8(srcNotWord)
    Builtin.condfail(tmp.1)
    var dstNotWord = tmp.0
    value = dstNotWord
  }

  init(v: Int32) {
    var srcNotWord = v.value
    var tmp = Builtin.s_to_s_checked_trunc_Int32_Int8(srcNotWord)
    Builtin.condfail(tmp.1)
    var dstNotWord = tmp.0
    value = dstNotWord
  }

  init(v: UInt64) {
    var srcNotWord = v.value
    var tmp = Builtin.u_to_s_checked_trunc_Int64_Int8(srcNotWord)
    Builtin.condfail(tmp.1)
    var dstNotWord = tmp.0
    value = dstNotWord
  }

  init(v: Int64) {
    var srcNotWord = v.value
    var tmp = Builtin.s_to_s_checked_trunc_Int64_Int8(srcNotWord)
    Builtin.condfail(tmp.1)
    var dstNotWord = tmp.0
    value = dstNotWord
  }

  init(v: UInt) {
    var srcNotWord = Builtin.truncOrBitCast_Word_Int32(v.value)
    var tmp = Builtin.u_to_s_checked_trunc_Int32_Int8(srcNotWord)
    Builtin.condfail(tmp.1)
    var dstNotWord = tmp.0
    value = dstNotWord
  }

  init(v: Int) {
    var srcNotWord = Builtin.truncOrBitCast_Word_Int32(v.value)
    var tmp = Builtin.s_to_s_checked_trunc_Int32_Int8(srcNotWord)
    Builtin.condfail(tmp.1)
    var dstNotWord = tmp.0
    value = dstNotWord
  }

  func asUnsigned() -> UInt8 {
    return UInt8(value)
  }
}

// Operations with masking and non-masking versions
@transparent
func &+ (lhs: Int8, rhs: Int8) -> Int8 {
  return Int8.add(lhs, rhs, false).0
}
@transparent
func + (lhs: Int8, rhs: Int8) -> Int8 {
  var tmp = Int8.add(lhs, rhs)
  Builtin.condfail(tmp.1.value)
  return tmp.0
}
@transparent
func &* (lhs: Int8, rhs: Int8) -> Int8 {
  return Int8.mul(lhs, rhs, false).0
}
@transparent
func * (lhs: Int8, rhs: Int8) -> Int8 {
  var tmp = Int8.mul(lhs, rhs)
  Builtin.condfail(tmp.1.value)
  return tmp.0
}
@transparent
func &- (lhs: Int8, rhs: Int8) -> Int8 {
  return Int8.sub(lhs, rhs, false).0
}
@transparent
func - (lhs: Int8, rhs: Int8) -> Int8 {
  var tmp = Int8.sub(lhs, rhs)
  Builtin.condfail(tmp.1.value)
  return tmp.0
}

// Bitwise negate
@transparent @prefix
func ~(rhs: Int8) -> Int8 {
  let mask = Int8.sub(0, 1).0
  return Int8(Builtin.xor_Int8(rhs.value, mask.value))
}

@transparent
func == (lhs: Int8, rhs: Int8) -> Bool {
  return Bool(Builtin.cmp_eq_Int8(lhs.value, rhs.value))
}
@transparent
func != (lhs: Int8, rhs: Int8) -> Bool {
  return Bool(Builtin.cmp_ne_Int8(lhs.value, rhs.value))
}
@transparent
func < (lhs: Int8, rhs: Int8) -> Bool {
  return Bool(Builtin.cmp_slt_Int8(lhs.value, rhs.value))
}
@transparent
func <= (lhs: Int8, rhs: Int8) -> Bool {
  return Bool(Builtin.cmp_sle_Int8(lhs.value, rhs.value))
}
@transparent
func > (lhs: Int8, rhs: Int8) -> Bool {
  return Bool(Builtin.cmp_sgt_Int8(lhs.value, rhs.value))
}
@transparent
func >= (lhs: Int8, rhs: Int8) -> Bool {
  return Bool(Builtin.cmp_sge_Int8(lhs.value, rhs.value))
}

@transparent
func << (lhs: Int8, rhs: Int8) -> Int8 {
  return Int8(Builtin.shl_Int8(lhs.value, rhs.value))
}
@transparent
func >> (lhs: Int8, rhs: Int8) -> Int8 {
  return Int8(Builtin.ashr_Int8(lhs.value, rhs.value))
}
@transparent
func & (lhs: Int8, rhs: Int8) -> Int8 {
  return Int8(Builtin.and_Int8(lhs.value, rhs.value))
}
@transparent
func ^ (lhs: Int8, rhs: Int8) -> Int8 {
  return Int8(Builtin.xor_Int8(lhs.value, rhs.value))
}
@transparent
func | (lhs: Int8, rhs: Int8) -> Int8 {
  return Int8(Builtin.or_Int8(lhs.value, rhs.value))
}

// bitwise operations
@transparent
extension Int8 : BitwiseOperations {
  type func allZeros() -> Int8 { return 0 }
}

// Compound assignments
@transparent @assignment
func +=(lhs: @inout Int8, rhs: Int8) {
  lhs = lhs + rhs
}
@transparent @assignment
func -=(lhs: @inout Int8, rhs: Int8) {
  lhs = lhs - rhs
}
@transparent @assignment
func *=(lhs: @inout Int8, rhs: Int8) {
  lhs = lhs * rhs
}
@transparent @assignment
func <<=(lhs: @inout Int8, rhs: Int8) {
  lhs = lhs << rhs
}
@transparent @assignment
func >>=(lhs: @inout Int8, rhs: Int8) {
  lhs = lhs >> rhs
}
@transparent @assignment
func &=(lhs: @inout Int8, rhs: Int8) {
  lhs = lhs & rhs
}
@transparent @assignment
func |=(lhs: @inout Int8, rhs: Int8) {
  lhs = lhs | rhs
}
@transparent @assignment
func ^=(lhs: @inout Int8, rhs: Int8) {
  lhs = lhs ^ rhs
}

struct UInt16 : BuiltinIntegerLiteralConvertible, IntegerLiteralConvertible,
                ArrayBound, ReplPrintable {
  var value: Builtin.Int16

  @transparent
  init() {
    var maxWidthZero: Int64 = 0
    value = Builtin.truncOrBitCast_Int64_Int16(maxWidthZero.value)
  }

  @transparent
  init(v: Builtin.Int16) {
    value = v
  }

  @transparent
  type func _convertFromBuiltinIntegerLiteral(val: Builtin.Int2048) -> UInt16 {
    return UInt16(Builtin.s_to_u_checked_trunc_Int2048_Int16(val).0)
  }

  @transparent
  type func convertFromIntegerLiteral(value: UInt16) -> UInt16 {
    return value
  }
  @transparent
  func _getBuiltinArrayBoundValue() -> Builtin.Word {
    return Builtin.zextOrBitCast_Int16_Word(self.value)
  }

  typealias ArrayBoundType = UInt16
  func getArrayBoundValue() -> UInt16 {
    return self
  }

  func replPrint() {
    print(UInt64(self))
  }

  @transparent
  type var max: UInt16 { return 0xFFFF }
  @transparent
  type var min: UInt16 { return 0 }
}

extension UInt16 : Hashable {
  func hashValue() -> Int {
    return Int(Builtin.sextOrBitCast_Int16_Word(self.value))
  }
}

@transparent
extension UInt16 : RandomAccessIndex {
  @transparent
  func succ() -> UInt16 {
    return self + 1
  }
  @transparent
  func pred() -> UInt16 {
    return self - 1
  }
  typealias DistanceType = UInt16
  @transparent
  type func sub(lhs: UInt16, rhs: UInt16, reportOverflow: Bool) -> (DistanceType, Bool) {
    var tmp = Builtin.usub_with_overflow_Int16(lhs.value, rhs.value, reportOverflow.value)
    return (UInt16(tmp.0), Bool(tmp.1))
  }
  @transparent
  type func add(lhs: UInt16, rhs: DistanceType, reportOverflow: Bool) -> (UInt16, Bool) {
    var tmp = Builtin.uadd_with_overflow_Int16(lhs.value, rhs.value, reportOverflow.value)
    return (UInt16(tmp.0), Bool(tmp.1))
  }
  @transparent
  type func mul(lhs: UInt16, rhs: UInt16, reportOverflow: Bool) -> (UInt16, Bool) {
    var tmp = Builtin.umul_with_overflow_Int16(lhs.value, rhs.value, reportOverflow.value)
    return (UInt16(tmp.0), Bool(tmp.1))
  }
  @transparent
  type func div(lhs: UInt16, rhs: UInt16, reportOverflow: Bool) -> (UInt16, Bool) {
    if rhs == 0 {
      return (0, true)
    }
    var tmp = Builtin.udiv_Int16(lhs.value, rhs.value)
    return (UInt16(tmp), false)
  }
  @transparent
  type func rem(lhs: UInt16, rhs: UInt16, reportOverflow: Bool) -> (UInt16, Bool) {
    if rhs == 0 {
      return (0, true)
    }
    var tmp = Builtin.urem_Int16(lhs.value, rhs.value)
    return (UInt16(tmp), false)
  }

  @transparent
  type func sub(lhs: UInt16, rhs: UInt16) -> (DistanceType, Bool) {
    return sub(lhs, rhs, true)
  }
  @transparent
  type func add(lhs: UInt16, rhs: DistanceType) -> (UInt16, Bool) {
    return add(lhs, rhs, true)
  }
  @transparent
  type func mul(lhs: UInt16, rhs: UInt16) -> (UInt16, Bool) {
    return mul(lhs, rhs, true)
  }
  @transparent
  type func div(lhs: UInt16, rhs: UInt16) -> (UInt16, Bool) {
    return div(lhs, rhs, true)
  }
  @transparent
  type func rem(lhs: UInt16, rhs: UInt16) -> (UInt16, Bool) {
    return rem(lhs, rhs, true)
  }
}


// construction from other integer types
@transparent
extension UInt16 {
  init(v: UInt8) {
    var srcNotWord = v.value
    var dstNotWord = Builtin.zext_Int8_Int16(srcNotWord)
    value = dstNotWord
  }

  init(v: Int8) {
    var srcNotWord = v.value
    var tmp = Builtin.s_to_u_checked_conversion_Int8(srcNotWord)
    Builtin.condfail(tmp.1)
    var dstNotWord = Builtin.sext_Int8_Int16(tmp.0)
    value = dstNotWord
  }

  init(v: Int16) {
    var srcNotWord = v.value
    var tmp = Builtin.s_to_u_checked_conversion_Int16(srcNotWord)
    Builtin.condfail(tmp.1)
    var dstNotWord = tmp.0
    value = dstNotWord
  }

  init(v: UInt32) {
    var srcNotWord = v.value
    var tmp = Builtin.u_to_u_checked_trunc_Int32_Int16(srcNotWord)
    Builtin.condfail(tmp.1)
    var dstNotWord = tmp.0
    value = dstNotWord
  }

  init(v: Int32) {
    var srcNotWord = v.value
    var tmp = Builtin.s_to_u_checked_trunc_Int32_Int16(srcNotWord)
    Builtin.condfail(tmp.1)
    var dstNotWord = tmp.0
    value = dstNotWord
  }

  init(v: UInt64) {
    var srcNotWord = v.value
    var tmp = Builtin.u_to_u_checked_trunc_Int64_Int16(srcNotWord)
    Builtin.condfail(tmp.1)
    var dstNotWord = tmp.0
    value = dstNotWord
  }

  init(v: Int64) {
    var srcNotWord = v.value
    var tmp = Builtin.s_to_u_checked_trunc_Int64_Int16(srcNotWord)
    Builtin.condfail(tmp.1)
    var dstNotWord = tmp.0
    value = dstNotWord
  }

  init(v: UInt) {
    var srcNotWord = Builtin.truncOrBitCast_Word_Int32(v.value)
    var tmp = Builtin.u_to_u_checked_trunc_Int32_Int16(srcNotWord)
    Builtin.condfail(tmp.1)
    var dstNotWord = tmp.0
    value = dstNotWord
  }

  init(v: Int) {
    var srcNotWord = Builtin.truncOrBitCast_Word_Int32(v.value)
    var tmp = Builtin.s_to_u_checked_trunc_Int32_Int16(srcNotWord)
    Builtin.condfail(tmp.1)
    var dstNotWord = tmp.0
    value = dstNotWord
  }

  func asSigned() -> Int16 {
    return Int16(value)
  }
}

// Operations with masking and non-masking versions
@transparent
func &+ (lhs: UInt16, rhs: UInt16) -> UInt16 {
  return UInt16.add(lhs, rhs, false).0
}
@transparent
func + (lhs: UInt16, rhs: UInt16) -> UInt16 {
  var tmp = UInt16.add(lhs, rhs)
  Builtin.condfail(tmp.1.value)
  return tmp.0
}
@transparent
func &* (lhs: UInt16, rhs: UInt16) -> UInt16 {
  return UInt16.mul(lhs, rhs, false).0
}
@transparent
func * (lhs: UInt16, rhs: UInt16) -> UInt16 {
  var tmp = UInt16.mul(lhs, rhs)
  Builtin.condfail(tmp.1.value)
  return tmp.0
}
@transparent
func &- (lhs: UInt16, rhs: UInt16) -> UInt16 {
  return UInt16.sub(lhs, rhs, false).0
}
@transparent
func - (lhs: UInt16, rhs: UInt16) -> UInt16 {
  var tmp = UInt16.sub(lhs, rhs)
  Builtin.condfail(tmp.1.value)
  return tmp.0
}

// Bitwise negate
@transparent @prefix
func ~(rhs: UInt16) -> UInt16 {
  let mask = UInt16.sub(0, 1).0
  return UInt16(Builtin.xor_Int16(rhs.value, mask.value))
}

@transparent
func == (lhs: UInt16, rhs: UInt16) -> Bool {
  return Bool(Builtin.cmp_eq_Int16(lhs.value, rhs.value))
}
@transparent
func != (lhs: UInt16, rhs: UInt16) -> Bool {
  return Bool(Builtin.cmp_ne_Int16(lhs.value, rhs.value))
}
@transparent
func < (lhs: UInt16, rhs: UInt16) -> Bool {
  return Bool(Builtin.cmp_ult_Int16(lhs.value, rhs.value))
}
@transparent
func <= (lhs: UInt16, rhs: UInt16) -> Bool {
  return Bool(Builtin.cmp_ule_Int16(lhs.value, rhs.value))
}
@transparent
func > (lhs: UInt16, rhs: UInt16) -> Bool {
  return Bool(Builtin.cmp_ugt_Int16(lhs.value, rhs.value))
}
@transparent
func >= (lhs: UInt16, rhs: UInt16) -> Bool {
  return Bool(Builtin.cmp_uge_Int16(lhs.value, rhs.value))
}

@transparent
func << (lhs: UInt16, rhs: UInt16) -> UInt16 {
  return UInt16(Builtin.shl_Int16(lhs.value, rhs.value))
}
@transparent
func >> (lhs: UInt16, rhs: UInt16) -> UInt16 {
  return UInt16(Builtin.lshr_Int16(lhs.value, rhs.value))
}
@transparent
func & (lhs: UInt16, rhs: UInt16) -> UInt16 {
  return UInt16(Builtin.and_Int16(lhs.value, rhs.value))
}
@transparent
func ^ (lhs: UInt16, rhs: UInt16) -> UInt16 {
  return UInt16(Builtin.xor_Int16(lhs.value, rhs.value))
}
@transparent
func | (lhs: UInt16, rhs: UInt16) -> UInt16 {
  return UInt16(Builtin.or_Int16(lhs.value, rhs.value))
}

// bitwise operations
@transparent
extension UInt16 : BitwiseOperations {
  type func allZeros() -> UInt16 { return 0 }
}

// Compound assignments
@transparent @assignment
func +=(lhs: @inout UInt16, rhs: UInt16) {
  lhs = lhs + rhs
}
@transparent @assignment
func -=(lhs: @inout UInt16, rhs: UInt16) {
  lhs = lhs - rhs
}
@transparent @assignment
func *=(lhs: @inout UInt16, rhs: UInt16) {
  lhs = lhs * rhs
}
@transparent @assignment
func <<=(lhs: @inout UInt16, rhs: UInt16) {
  lhs = lhs << rhs
}
@transparent @assignment
func >>=(lhs: @inout UInt16, rhs: UInt16) {
  lhs = lhs >> rhs
}
@transparent @assignment
func &=(lhs: @inout UInt16, rhs: UInt16) {
  lhs = lhs & rhs
}
@transparent @assignment
func |=(lhs: @inout UInt16, rhs: UInt16) {
  lhs = lhs | rhs
}
@transparent @assignment
func ^=(lhs: @inout UInt16, rhs: UInt16) {
  lhs = lhs ^ rhs
}

struct Int16 : BuiltinIntegerLiteralConvertible, IntegerLiteralConvertible,
                ArrayBound, ReplPrintable {
  var value: Builtin.Int16

  @transparent
  init() {
    var maxWidthZero: Int64 = 0
    value = Builtin.truncOrBitCast_Int64_Int16(maxWidthZero.value)
  }

  @transparent
  init(v: Builtin.Int16) {
    value = v
  }

  @transparent
  type func _convertFromBuiltinIntegerLiteral(val: Builtin.Int2048) -> Int16 {
    return Int16(Builtin.s_to_s_checked_trunc_Int2048_Int16(val).0)
  }

  @transparent
  type func convertFromIntegerLiteral(value: Int16) -> Int16 {
    return value
  }
  @transparent
  func _getBuiltinArrayBoundValue() -> Builtin.Word {
    return Builtin.sextOrBitCast_Int16_Word(self.value)
  }

  typealias ArrayBoundType = Int16
  func getArrayBoundValue() -> Int16 {
    return self
  }

  func replPrint() {
    print(Int64(self))
  }

  @transparent
  type var max: Int16 { return 0x7FFF }
  @transparent
  type var min: Int16 { return -0x7FFF-1 }
}

extension Int16 : Hashable {
  func hashValue() -> Int {
    return Int(Builtin.sextOrBitCast_Int16_Word(self.value))
  }
}

@transparent
extension Int16 : RandomAccessIndex {
  @transparent
  func succ() -> Int16 {
    return self + 1
  }
  @transparent
  func pred() -> Int16 {
    return self - 1
  }
  typealias DistanceType = Int16
  @transparent
  type func sub(lhs: Int16, rhs: Int16, reportOverflow: Bool) -> (DistanceType, Bool) {
    var tmp = Builtin.ssub_with_overflow_Int16(lhs.value, rhs.value, reportOverflow.value)
    return (Int16(tmp.0), Bool(tmp.1))
  }
  @transparent
  type func add(lhs: Int16, rhs: DistanceType, reportOverflow: Bool) -> (Int16, Bool) {
    var tmp = Builtin.sadd_with_overflow_Int16(lhs.value, rhs.value, reportOverflow.value)
    return (Int16(tmp.0), Bool(tmp.1))
  }
  @transparent
  type func mul(lhs: Int16, rhs: Int16, reportOverflow: Bool) -> (Int16, Bool) {
    var tmp = Builtin.smul_with_overflow_Int16(lhs.value, rhs.value, reportOverflow.value)
    return (Int16(tmp.0), Bool(tmp.1))
  }
  @transparent
  type func div(lhs: Int16, rhs: Int16, reportOverflow: Bool) -> (Int16, Bool) {
    if rhs == 0 {
      return (0, true)
    }
    if lhs == Int16.min && rhs == -1 {
      return (lhs, true)
    }
    var tmp = Builtin.sdiv_Int16(lhs.value, rhs.value)
    return (Int16(tmp), false)
  }
  @transparent
  type func rem(lhs: Int16, rhs: Int16, reportOverflow: Bool) -> (Int16, Bool) {
    if rhs == 0 {
      return (0, true)
    }
    if lhs == Int16.min && rhs == -1 {
      return (0, true)
    }
    var tmp = Builtin.srem_Int16(lhs.value, rhs.value)
    return (Int16(tmp), false)
  }

  @transparent
  type func sub(lhs: Int16, rhs: Int16) -> (DistanceType, Bool) {
    return sub(lhs, rhs, true)
  }
  @transparent
  type func add(lhs: Int16, rhs: DistanceType) -> (Int16, Bool) {
    return add(lhs, rhs, true)
  }
  @transparent
  type func mul(lhs: Int16, rhs: Int16) -> (Int16, Bool) {
    return mul(lhs, rhs, true)
  }
  @transparent
  type func div(lhs: Int16, rhs: Int16) -> (Int16, Bool) {
    return div(lhs, rhs, true)
  }
  @transparent
  type func rem(lhs: Int16, rhs: Int16) -> (Int16, Bool) {
    return rem(lhs, rhs, true)
  }
}

@transparent
extension Int16 : SignedNumber {
  @transparent
  type func negate(rhs: Int16) -> (Int16, Bool) {
    return Int16.sub(0, rhs)
  }
  @transparent
  type func abs(rhs: Int16) -> (Int16, Bool) {
    return rhs.isNegative() ? Int16.negate(rhs) : (rhs, false)
  }
  @transparent
  func isNegative() -> Bool { return self < 0 }
}

// construction from other integer types
@transparent
extension Int16 {
  init(v: UInt8) {
    var srcNotWord = v.value
    var dstNotWord = Builtin.zext_Int8_Int16(srcNotWord)
    value = dstNotWord
  }

  init(v: Int8) {
    var srcNotWord = v.value
    var dstNotWord = Builtin.sext_Int8_Int16(srcNotWord)
    value = dstNotWord
  }

  init(v: UInt16) {
    var srcNotWord = v.value
    var tmp = Builtin.u_to_s_checked_conversion_Int16(srcNotWord)
    Builtin.condfail(tmp.1)
    var dstNotWord = tmp.0
    value = dstNotWord
  }

  init(v: UInt32) {
    var srcNotWord = v.value
    var tmp = Builtin.u_to_s_checked_trunc_Int32_Int16(srcNotWord)
    Builtin.condfail(tmp.1)
    var dstNotWord = tmp.0
    value = dstNotWord
  }

  init(v: Int32) {
    var srcNotWord = v.value
    var tmp = Builtin.s_to_s_checked_trunc_Int32_Int16(srcNotWord)
    Builtin.condfail(tmp.1)
    var dstNotWord = tmp.0
    value = dstNotWord
  }

  init(v: UInt64) {
    var srcNotWord = v.value
    var tmp = Builtin.u_to_s_checked_trunc_Int64_Int16(srcNotWord)
    Builtin.condfail(tmp.1)
    var dstNotWord = tmp.0
    value = dstNotWord
  }

  init(v: Int64) {
    var srcNotWord = v.value
    var tmp = Builtin.s_to_s_checked_trunc_Int64_Int16(srcNotWord)
    Builtin.condfail(tmp.1)
    var dstNotWord = tmp.0
    value = dstNotWord
  }

  init(v: UInt) {
    var srcNotWord = Builtin.truncOrBitCast_Word_Int32(v.value)
    var tmp = Builtin.u_to_s_checked_trunc_Int32_Int16(srcNotWord)
    Builtin.condfail(tmp.1)
    var dstNotWord = tmp.0
    value = dstNotWord
  }

  init(v: Int) {
    var srcNotWord = Builtin.truncOrBitCast_Word_Int32(v.value)
    var tmp = Builtin.s_to_s_checked_trunc_Int32_Int16(srcNotWord)
    Builtin.condfail(tmp.1)
    var dstNotWord = tmp.0
    value = dstNotWord
  }

  func asUnsigned() -> UInt16 {
    return UInt16(value)
  }
}

// Operations with masking and non-masking versions
@transparent
func &+ (lhs: Int16, rhs: Int16) -> Int16 {
  return Int16.add(lhs, rhs, false).0
}
@transparent
func + (lhs: Int16, rhs: Int16) -> Int16 {
  var tmp = Int16.add(lhs, rhs)
  Builtin.condfail(tmp.1.value)
  return tmp.0
}
@transparent
func &* (lhs: Int16, rhs: Int16) -> Int16 {
  return Int16.mul(lhs, rhs, false).0
}
@transparent
func * (lhs: Int16, rhs: Int16) -> Int16 {
  var tmp = Int16.mul(lhs, rhs)
  Builtin.condfail(tmp.1.value)
  return tmp.0
}
@transparent
func &- (lhs: Int16, rhs: Int16) -> Int16 {
  return Int16.sub(lhs, rhs, false).0
}
@transparent
func - (lhs: Int16, rhs: Int16) -> Int16 {
  var tmp = Int16.sub(lhs, rhs)
  Builtin.condfail(tmp.1.value)
  return tmp.0
}

// Bitwise negate
@transparent @prefix
func ~(rhs: Int16) -> Int16 {
  let mask = Int16.sub(0, 1).0
  return Int16(Builtin.xor_Int16(rhs.value, mask.value))
}

@transparent
func == (lhs: Int16, rhs: Int16) -> Bool {
  return Bool(Builtin.cmp_eq_Int16(lhs.value, rhs.value))
}
@transparent
func != (lhs: Int16, rhs: Int16) -> Bool {
  return Bool(Builtin.cmp_ne_Int16(lhs.value, rhs.value))
}
@transparent
func < (lhs: Int16, rhs: Int16) -> Bool {
  return Bool(Builtin.cmp_slt_Int16(lhs.value, rhs.value))
}
@transparent
func <= (lhs: Int16, rhs: Int16) -> Bool {
  return Bool(Builtin.cmp_sle_Int16(lhs.value, rhs.value))
}
@transparent
func > (lhs: Int16, rhs: Int16) -> Bool {
  return Bool(Builtin.cmp_sgt_Int16(lhs.value, rhs.value))
}
@transparent
func >= (lhs: Int16, rhs: Int16) -> Bool {
  return Bool(Builtin.cmp_sge_Int16(lhs.value, rhs.value))
}

@transparent
func << (lhs: Int16, rhs: Int16) -> Int16 {
  return Int16(Builtin.shl_Int16(lhs.value, rhs.value))
}
@transparent
func >> (lhs: Int16, rhs: Int16) -> Int16 {
  return Int16(Builtin.ashr_Int16(lhs.value, rhs.value))
}
@transparent
func & (lhs: Int16, rhs: Int16) -> Int16 {
  return Int16(Builtin.and_Int16(lhs.value, rhs.value))
}
@transparent
func ^ (lhs: Int16, rhs: Int16) -> Int16 {
  return Int16(Builtin.xor_Int16(lhs.value, rhs.value))
}
@transparent
func | (lhs: Int16, rhs: Int16) -> Int16 {
  return Int16(Builtin.or_Int16(lhs.value, rhs.value))
}

// bitwise operations
@transparent
extension Int16 : BitwiseOperations {
  type func allZeros() -> Int16 { return 0 }
}

// Compound assignments
@transparent @assignment
func +=(lhs: @inout Int16, rhs: Int16) {
  lhs = lhs + rhs
}
@transparent @assignment
func -=(lhs: @inout Int16, rhs: Int16) {
  lhs = lhs - rhs
}
@transparent @assignment
func *=(lhs: @inout Int16, rhs: Int16) {
  lhs = lhs * rhs
}
@transparent @assignment
func <<=(lhs: @inout Int16, rhs: Int16) {
  lhs = lhs << rhs
}
@transparent @assignment
func >>=(lhs: @inout Int16, rhs: Int16) {
  lhs = lhs >> rhs
}
@transparent @assignment
func &=(lhs: @inout Int16, rhs: Int16) {
  lhs = lhs & rhs
}
@transparent @assignment
func |=(lhs: @inout Int16, rhs: Int16) {
  lhs = lhs | rhs
}
@transparent @assignment
func ^=(lhs: @inout Int16, rhs: Int16) {
  lhs = lhs ^ rhs
}

struct UInt32 : BuiltinIntegerLiteralConvertible, IntegerLiteralConvertible,
                ArrayBound, ReplPrintable {
  var value: Builtin.Int32

  @transparent
  init() {
    var maxWidthZero: Int64 = 0
    value = Builtin.truncOrBitCast_Int64_Int32(maxWidthZero.value)
  }

  @transparent
  init(v: Builtin.Int32) {
    value = v
  }

  @transparent
  type func _convertFromBuiltinIntegerLiteral(val: Builtin.Int2048) -> UInt32 {
    return UInt32(Builtin.s_to_u_checked_trunc_Int2048_Int32(val).0)
  }

  @transparent
  type func convertFromIntegerLiteral(value: UInt32) -> UInt32 {
    return value
  }
  @transparent
  func _getBuiltinArrayBoundValue() -> Builtin.Word {
    return Builtin.zextOrBitCast_Int32_Word(self.value)
  }

  typealias ArrayBoundType = UInt32
  func getArrayBoundValue() -> UInt32 {
    return self
  }

  func replPrint() {
    print(UInt64(self))
  }

  @transparent
  type var max: UInt32 { return 0xFFFF_FFFF }
  @transparent
  type var min: UInt32 { return 0 }
}

extension UInt32 : Hashable {
  func hashValue() -> Int {
    return Int(Builtin.zextOrBitCast_Int32_Word(self.value))
  }
}

@transparent
extension UInt32 : RandomAccessIndex {
  @transparent
  func succ() -> UInt32 {
    return self + 1
  }
  @transparent
  func pred() -> UInt32 {
    return self - 1
  }
  typealias DistanceType = UInt32
  @transparent
  type func sub(lhs: UInt32, rhs: UInt32, reportOverflow: Bool) -> (DistanceType, Bool) {
    var tmp = Builtin.usub_with_overflow_Int32(lhs.value, rhs.value, reportOverflow.value)
    return (UInt32(tmp.0), Bool(tmp.1))
  }
  @transparent
  type func add(lhs: UInt32, rhs: DistanceType, reportOverflow: Bool) -> (UInt32, Bool) {
    var tmp = Builtin.uadd_with_overflow_Int32(lhs.value, rhs.value, reportOverflow.value)
    return (UInt32(tmp.0), Bool(tmp.1))
  }
  @transparent
  type func mul(lhs: UInt32, rhs: UInt32, reportOverflow: Bool) -> (UInt32, Bool) {
    var tmp = Builtin.umul_with_overflow_Int32(lhs.value, rhs.value, reportOverflow.value)
    return (UInt32(tmp.0), Bool(tmp.1))
  }
  @transparent
  type func div(lhs: UInt32, rhs: UInt32, reportOverflow: Bool) -> (UInt32, Bool) {
    if rhs == 0 {
      return (0, true)
    }
    var tmp = Builtin.udiv_Int32(lhs.value, rhs.value)
    return (UInt32(tmp), false)
  }
  @transparent
  type func rem(lhs: UInt32, rhs: UInt32, reportOverflow: Bool) -> (UInt32, Bool) {
    if rhs == 0 {
      return (0, true)
    }
    var tmp = Builtin.urem_Int32(lhs.value, rhs.value)
    return (UInt32(tmp), false)
  }

  @transparent
  type func sub(lhs: UInt32, rhs: UInt32) -> (DistanceType, Bool) {
    return sub(lhs, rhs, true)
  }
  @transparent
  type func add(lhs: UInt32, rhs: DistanceType) -> (UInt32, Bool) {
    return add(lhs, rhs, true)
  }
  @transparent
  type func mul(lhs: UInt32, rhs: UInt32) -> (UInt32, Bool) {
    return mul(lhs, rhs, true)
  }
  @transparent
  type func div(lhs: UInt32, rhs: UInt32) -> (UInt32, Bool) {
    return div(lhs, rhs, true)
  }
  @transparent
  type func rem(lhs: UInt32, rhs: UInt32) -> (UInt32, Bool) {
    return rem(lhs, rhs, true)
  }
}


// construction from other integer types
@transparent
extension UInt32 {
  init(v: UInt8) {
    var srcNotWord = v.value
    var dstNotWord = Builtin.zext_Int8_Int32(srcNotWord)
    value = dstNotWord
  }

  init(v: Int8) {
    var srcNotWord = v.value
    var tmp = Builtin.s_to_u_checked_conversion_Int8(srcNotWord)
    Builtin.condfail(tmp.1)
    var dstNotWord = Builtin.sext_Int8_Int32(tmp.0)
    value = dstNotWord
  }

  init(v: UInt16) {
    var srcNotWord = v.value
    var dstNotWord = Builtin.zext_Int16_Int32(srcNotWord)
    value = dstNotWord
  }

  init(v: Int16) {
    var srcNotWord = v.value
    var tmp = Builtin.s_to_u_checked_conversion_Int16(srcNotWord)
    Builtin.condfail(tmp.1)
    var dstNotWord = Builtin.sext_Int16_Int32(tmp.0)
    value = dstNotWord
  }

  init(v: Int32) {
    var srcNotWord = v.value
    var tmp = Builtin.s_to_u_checked_conversion_Int32(srcNotWord)
    Builtin.condfail(tmp.1)
    var dstNotWord = tmp.0
    value = dstNotWord
  }

  init(v: UInt64) {
    var srcNotWord = v.value
    var tmp = Builtin.u_to_u_checked_trunc_Int64_Int32(srcNotWord)
    Builtin.condfail(tmp.1)
    var dstNotWord = tmp.0
    value = dstNotWord
  }

  init(v: Int64) {
    var srcNotWord = v.value
    var tmp = Builtin.s_to_u_checked_trunc_Int64_Int32(srcNotWord)
    Builtin.condfail(tmp.1)
    var dstNotWord = tmp.0
    value = dstNotWord
  }

  init(v: UInt) {
    var srcNotWord = Builtin.truncOrBitCast_Word_Int32(v.value)
    var dstNotWord = srcNotWord
    value = dstNotWord
  }

  init(v: Int) {
    var srcNotWord = Builtin.truncOrBitCast_Word_Int32(v.value)
    var tmp = Builtin.s_to_u_checked_conversion_Int32(srcNotWord)
    Builtin.condfail(tmp.1)
    var dstNotWord = tmp.0
    value = dstNotWord
  }

  func asSigned() -> Int32 {
    return Int32(value)
  }
}

// Operations with masking and non-masking versions
@transparent
func &+ (lhs: UInt32, rhs: UInt32) -> UInt32 {
  return UInt32.add(lhs, rhs, false).0
}
@transparent
func + (lhs: UInt32, rhs: UInt32) -> UInt32 {
  var tmp = UInt32.add(lhs, rhs)
  Builtin.condfail(tmp.1.value)
  return tmp.0
}
@transparent
func &* (lhs: UInt32, rhs: UInt32) -> UInt32 {
  return UInt32.mul(lhs, rhs, false).0
}
@transparent
func * (lhs: UInt32, rhs: UInt32) -> UInt32 {
  var tmp = UInt32.mul(lhs, rhs)
  Builtin.condfail(tmp.1.value)
  return tmp.0
}
@transparent
func &- (lhs: UInt32, rhs: UInt32) -> UInt32 {
  return UInt32.sub(lhs, rhs, false).0
}
@transparent
func - (lhs: UInt32, rhs: UInt32) -> UInt32 {
  var tmp = UInt32.sub(lhs, rhs)
  Builtin.condfail(tmp.1.value)
  return tmp.0
}

// Bitwise negate
@transparent @prefix
func ~(rhs: UInt32) -> UInt32 {
  let mask = UInt32.sub(0, 1).0
  return UInt32(Builtin.xor_Int32(rhs.value, mask.value))
}

@transparent
func == (lhs: UInt32, rhs: UInt32) -> Bool {
  return Bool(Builtin.cmp_eq_Int32(lhs.value, rhs.value))
}
@transparent
func != (lhs: UInt32, rhs: UInt32) -> Bool {
  return Bool(Builtin.cmp_ne_Int32(lhs.value, rhs.value))
}
@transparent
func < (lhs: UInt32, rhs: UInt32) -> Bool {
  return Bool(Builtin.cmp_ult_Int32(lhs.value, rhs.value))
}
@transparent
func <= (lhs: UInt32, rhs: UInt32) -> Bool {
  return Bool(Builtin.cmp_ule_Int32(lhs.value, rhs.value))
}
@transparent
func > (lhs: UInt32, rhs: UInt32) -> Bool {
  return Bool(Builtin.cmp_ugt_Int32(lhs.value, rhs.value))
}
@transparent
func >= (lhs: UInt32, rhs: UInt32) -> Bool {
  return Bool(Builtin.cmp_uge_Int32(lhs.value, rhs.value))
}

@transparent
func << (lhs: UInt32, rhs: UInt32) -> UInt32 {
  return UInt32(Builtin.shl_Int32(lhs.value, rhs.value))
}
@transparent
func >> (lhs: UInt32, rhs: UInt32) -> UInt32 {
  return UInt32(Builtin.lshr_Int32(lhs.value, rhs.value))
}
@transparent
func & (lhs: UInt32, rhs: UInt32) -> UInt32 {
  return UInt32(Builtin.and_Int32(lhs.value, rhs.value))
}
@transparent
func ^ (lhs: UInt32, rhs: UInt32) -> UInt32 {
  return UInt32(Builtin.xor_Int32(lhs.value, rhs.value))
}
@transparent
func | (lhs: UInt32, rhs: UInt32) -> UInt32 {
  return UInt32(Builtin.or_Int32(lhs.value, rhs.value))
}

// bitwise operations
@transparent
extension UInt32 : BitwiseOperations {
  type func allZeros() -> UInt32 { return 0 }
}

// Compound assignments
@transparent @assignment
func +=(lhs: @inout UInt32, rhs: UInt32) {
  lhs = lhs + rhs
}
@transparent @assignment
func -=(lhs: @inout UInt32, rhs: UInt32) {
  lhs = lhs - rhs
}
@transparent @assignment
func *=(lhs: @inout UInt32, rhs: UInt32) {
  lhs = lhs * rhs
}
@transparent @assignment
func <<=(lhs: @inout UInt32, rhs: UInt32) {
  lhs = lhs << rhs
}
@transparent @assignment
func >>=(lhs: @inout UInt32, rhs: UInt32) {
  lhs = lhs >> rhs
}
@transparent @assignment
func &=(lhs: @inout UInt32, rhs: UInt32) {
  lhs = lhs & rhs
}
@transparent @assignment
func |=(lhs: @inout UInt32, rhs: UInt32) {
  lhs = lhs | rhs
}
@transparent @assignment
func ^=(lhs: @inout UInt32, rhs: UInt32) {
  lhs = lhs ^ rhs
}

struct Int32 : BuiltinIntegerLiteralConvertible, IntegerLiteralConvertible,
                ArrayBound, ReplPrintable {
  var value: Builtin.Int32

  @transparent
  init() {
    var maxWidthZero: Int64 = 0
    value = Builtin.truncOrBitCast_Int64_Int32(maxWidthZero.value)
  }

  @transparent
  init(v: Builtin.Int32) {
    value = v
  }

  @transparent
  type func _convertFromBuiltinIntegerLiteral(val: Builtin.Int2048) -> Int32 {
    return Int32(Builtin.s_to_s_checked_trunc_Int2048_Int32(val).0)
  }

  @transparent
  type func convertFromIntegerLiteral(value: Int32) -> Int32 {
    return value
  }
  @transparent
  func _getBuiltinArrayBoundValue() -> Builtin.Word {
    return Builtin.zextOrBitCast_Int32_Word(self.value)
  }

  typealias ArrayBoundType = Int32
  func getArrayBoundValue() -> Int32 {
    return self
  }

  func replPrint() {
    print(Int64(self))
  }

  @transparent
  type var max: Int32 { return 0x7FFF_FFFF }
  @transparent
  type var min: Int32 { return -0x7FFF_FFFF-1 }
}

extension Int32 : Hashable {
  func hashValue() -> Int {
    return Int(Builtin.zextOrBitCast_Int32_Word(self.value))
  }
}

@transparent
extension Int32 : RandomAccessIndex {
  @transparent
  func succ() -> Int32 {
    return self + 1
  }
  @transparent
  func pred() -> Int32 {
    return self - 1
  }
  typealias DistanceType = Int32
  @transparent
  type func sub(lhs: Int32, rhs: Int32, reportOverflow: Bool) -> (DistanceType, Bool) {
    var tmp = Builtin.ssub_with_overflow_Int32(lhs.value, rhs.value, reportOverflow.value)
    return (Int32(tmp.0), Bool(tmp.1))
  }
  @transparent
  type func add(lhs: Int32, rhs: DistanceType, reportOverflow: Bool) -> (Int32, Bool) {
    var tmp = Builtin.sadd_with_overflow_Int32(lhs.value, rhs.value, reportOverflow.value)
    return (Int32(tmp.0), Bool(tmp.1))
  }
  @transparent
  type func mul(lhs: Int32, rhs: Int32, reportOverflow: Bool) -> (Int32, Bool) {
    var tmp = Builtin.smul_with_overflow_Int32(lhs.value, rhs.value, reportOverflow.value)
    return (Int32(tmp.0), Bool(tmp.1))
  }
  @transparent
  type func div(lhs: Int32, rhs: Int32, reportOverflow: Bool) -> (Int32, Bool) {
    if rhs == 0 {
      return (0, true)
    }
    if lhs == Int32.min && rhs == -1 {
      return (lhs, true)
    }
    var tmp = Builtin.sdiv_Int32(lhs.value, rhs.value)
    return (Int32(tmp), false)
  }
  @transparent
  type func rem(lhs: Int32, rhs: Int32, reportOverflow: Bool) -> (Int32, Bool) {
    if rhs == 0 {
      return (0, true)
    }
    if lhs == Int32.min && rhs == -1 {
      return (0, true)
    }
    var tmp = Builtin.srem_Int32(lhs.value, rhs.value)
    return (Int32(tmp), false)
  }

  @transparent
  type func sub(lhs: Int32, rhs: Int32) -> (DistanceType, Bool) {
    return sub(lhs, rhs, true)
  }
  @transparent
  type func add(lhs: Int32, rhs: DistanceType) -> (Int32, Bool) {
    return add(lhs, rhs, true)
  }
  @transparent
  type func mul(lhs: Int32, rhs: Int32) -> (Int32, Bool) {
    return mul(lhs, rhs, true)
  }
  @transparent
  type func div(lhs: Int32, rhs: Int32) -> (Int32, Bool) {
    return div(lhs, rhs, true)
  }
  @transparent
  type func rem(lhs: Int32, rhs: Int32) -> (Int32, Bool) {
    return rem(lhs, rhs, true)
  }
}

@transparent
extension Int32 : SignedNumber {
  @transparent
  type func negate(rhs: Int32) -> (Int32, Bool) {
    return Int32.sub(0, rhs)
  }
  @transparent
  type func abs(rhs: Int32) -> (Int32, Bool) {
    return rhs.isNegative() ? Int32.negate(rhs) : (rhs, false)
  }
  @transparent
  func isNegative() -> Bool { return self < 0 }
}

// construction from other integer types
@transparent
extension Int32 {
  init(v: UInt8) {
    var srcNotWord = v.value
    var dstNotWord = Builtin.zext_Int8_Int32(srcNotWord)
    value = dstNotWord
  }

  init(v: Int8) {
    var srcNotWord = v.value
    var dstNotWord = Builtin.sext_Int8_Int32(srcNotWord)
    value = dstNotWord
  }

  init(v: UInt16) {
    var srcNotWord = v.value
    var dstNotWord = Builtin.zext_Int16_Int32(srcNotWord)
    value = dstNotWord
  }

  init(v: Int16) {
    var srcNotWord = v.value
    var dstNotWord = Builtin.sext_Int16_Int32(srcNotWord)
    value = dstNotWord
  }

  init(v: UInt32) {
    var srcNotWord = v.value
    var tmp = Builtin.u_to_s_checked_conversion_Int32(srcNotWord)
    Builtin.condfail(tmp.1)
    var dstNotWord = tmp.0
    value = dstNotWord
  }

  init(v: UInt64) {
    var srcNotWord = v.value
    var tmp = Builtin.u_to_s_checked_trunc_Int64_Int32(srcNotWord)
    Builtin.condfail(tmp.1)
    var dstNotWord = tmp.0
    value = dstNotWord
  }

  init(v: Int64) {
    var srcNotWord = v.value
    var tmp = Builtin.s_to_s_checked_trunc_Int64_Int32(srcNotWord)
    Builtin.condfail(tmp.1)
    var dstNotWord = tmp.0
    value = dstNotWord
  }

  init(v: UInt) {
    var srcNotWord = Builtin.truncOrBitCast_Word_Int32(v.value)
    var tmp = Builtin.u_to_s_checked_conversion_Int32(srcNotWord)
    Builtin.condfail(tmp.1)
    var dstNotWord = tmp.0
    value = dstNotWord
  }

  init(v: Int) {
    var srcNotWord = Builtin.truncOrBitCast_Word_Int32(v.value)
    var dstNotWord = srcNotWord
    value = dstNotWord
  }

  func asUnsigned() -> UInt32 {
    return UInt32(value)
  }
}

// Operations with masking and non-masking versions
@transparent
func &+ (lhs: Int32, rhs: Int32) -> Int32 {
  return Int32.add(lhs, rhs, false).0
}
@transparent
func + (lhs: Int32, rhs: Int32) -> Int32 {
  var tmp = Int32.add(lhs, rhs)
  Builtin.condfail(tmp.1.value)
  return tmp.0
}
@transparent
func &* (lhs: Int32, rhs: Int32) -> Int32 {
  return Int32.mul(lhs, rhs, false).0
}
@transparent
func * (lhs: Int32, rhs: Int32) -> Int32 {
  var tmp = Int32.mul(lhs, rhs)
  Builtin.condfail(tmp.1.value)
  return tmp.0
}
@transparent
func &- (lhs: Int32, rhs: Int32) -> Int32 {
  return Int32.sub(lhs, rhs, false).0
}
@transparent
func - (lhs: Int32, rhs: Int32) -> Int32 {
  var tmp = Int32.sub(lhs, rhs)
  Builtin.condfail(tmp.1.value)
  return tmp.0
}

// Bitwise negate
@transparent @prefix
func ~(rhs: Int32) -> Int32 {
  let mask = Int32.sub(0, 1).0
  return Int32(Builtin.xor_Int32(rhs.value, mask.value))
}

@transparent
func == (lhs: Int32, rhs: Int32) -> Bool {
  return Bool(Builtin.cmp_eq_Int32(lhs.value, rhs.value))
}
@transparent
func != (lhs: Int32, rhs: Int32) -> Bool {
  return Bool(Builtin.cmp_ne_Int32(lhs.value, rhs.value))
}
@transparent
func < (lhs: Int32, rhs: Int32) -> Bool {
  return Bool(Builtin.cmp_slt_Int32(lhs.value, rhs.value))
}
@transparent
func <= (lhs: Int32, rhs: Int32) -> Bool {
  return Bool(Builtin.cmp_sle_Int32(lhs.value, rhs.value))
}
@transparent
func > (lhs: Int32, rhs: Int32) -> Bool {
  return Bool(Builtin.cmp_sgt_Int32(lhs.value, rhs.value))
}
@transparent
func >= (lhs: Int32, rhs: Int32) -> Bool {
  return Bool(Builtin.cmp_sge_Int32(lhs.value, rhs.value))
}

@transparent
func << (lhs: Int32, rhs: Int32) -> Int32 {
  return Int32(Builtin.shl_Int32(lhs.value, rhs.value))
}
@transparent
func >> (lhs: Int32, rhs: Int32) -> Int32 {
  return Int32(Builtin.ashr_Int32(lhs.value, rhs.value))
}
@transparent
func & (lhs: Int32, rhs: Int32) -> Int32 {
  return Int32(Builtin.and_Int32(lhs.value, rhs.value))
}
@transparent
func ^ (lhs: Int32, rhs: Int32) -> Int32 {
  return Int32(Builtin.xor_Int32(lhs.value, rhs.value))
}
@transparent
func | (lhs: Int32, rhs: Int32) -> Int32 {
  return Int32(Builtin.or_Int32(lhs.value, rhs.value))
}

// bitwise operations
@transparent
extension Int32 : BitwiseOperations {
  type func allZeros() -> Int32 { return 0 }
}

// Compound assignments
@transparent @assignment
func +=(lhs: @inout Int32, rhs: Int32) {
  lhs = lhs + rhs
}
@transparent @assignment
func -=(lhs: @inout Int32, rhs: Int32) {
  lhs = lhs - rhs
}
@transparent @assignment
func *=(lhs: @inout Int32, rhs: Int32) {
  lhs = lhs * rhs
}
@transparent @assignment
func <<=(lhs: @inout Int32, rhs: Int32) {
  lhs = lhs << rhs
}
@transparent @assignment
func >>=(lhs: @inout Int32, rhs: Int32) {
  lhs = lhs >> rhs
}
@transparent @assignment
func &=(lhs: @inout Int32, rhs: Int32) {
  lhs = lhs & rhs
}
@transparent @assignment
func |=(lhs: @inout Int32, rhs: Int32) {
  lhs = lhs | rhs
}
@transparent @assignment
func ^=(lhs: @inout Int32, rhs: Int32) {
  lhs = lhs ^ rhs
}

struct UInt64 : BuiltinIntegerLiteralConvertible, IntegerLiteralConvertible,
                ArrayBound, ReplPrintable {
  var value: Builtin.Int64

  @transparent
  init() {
    var maxWidthZero: Int64 = 0
    value = Builtin.truncOrBitCast_Int64_Int64(maxWidthZero.value)
  }

  @transparent
  init(v: Builtin.Int64) {
    value = v
  }

  @transparent
  type func _convertFromBuiltinIntegerLiteral(val: Builtin.Int2048) -> UInt64 {
    return UInt64(Builtin.s_to_u_checked_trunc_Int2048_Int64(val).0)
  }

  @transparent
  type func convertFromIntegerLiteral(value: UInt64) -> UInt64 {
    return value
  }
  @transparent
  func _getBuiltinArrayBoundValue() -> Builtin.Word {
    return Builtin.truncOrBitCast_Int64_Word(self.value)
  }

  typealias ArrayBoundType = UInt64
  func getArrayBoundValue() -> UInt64 {
    return self
  }

  func replPrint() {
    print(self)
  }

  @transparent
  type var max: UInt64 { return 0xFFFF_FFFF_FFFF_FFFF }
  @transparent
  type var min: UInt64 { return 0 }
}

extension UInt64 : Hashable {
  func hashValue() -> Int {
    var result: Int = 0
    for var i = 0; i < (sizeof(self) * 8); i += sizeof(Int) * 8 {
      result ^= Int(self >> UInt64(i)) & ~0
    }
    return result
  }
}

@transparent
extension UInt64 : RandomAccessIndex {
  @transparent
  func succ() -> UInt64 {
    return self + 1
  }
  @transparent
  func pred() -> UInt64 {
    return self - 1
  }
  typealias DistanceType = UInt64
  @transparent
  type func sub(lhs: UInt64, rhs: UInt64, reportOverflow: Bool) -> (DistanceType, Bool) {
    var tmp = Builtin.usub_with_overflow_Int64(lhs.value, rhs.value, reportOverflow.value)
    return (UInt64(tmp.0), Bool(tmp.1))
  }
  @transparent
  type func add(lhs: UInt64, rhs: DistanceType, reportOverflow: Bool) -> (UInt64, Bool) {
    var tmp = Builtin.uadd_with_overflow_Int64(lhs.value, rhs.value, reportOverflow.value)
    return (UInt64(tmp.0), Bool(tmp.1))
  }
  @transparent
  type func mul(lhs: UInt64, rhs: UInt64, reportOverflow: Bool) -> (UInt64, Bool) {
    var tmp = Builtin.umul_with_overflow_Int64(lhs.value, rhs.value, reportOverflow.value)
    return (UInt64(tmp.0), Bool(tmp.1))
  }
  @transparent
  type func div(lhs: UInt64, rhs: UInt64, reportOverflow: Bool) -> (UInt64, Bool) {
    if rhs == 0 {
      return (0, true)
    }
    var tmp = Builtin.udiv_Int64(lhs.value, rhs.value)
    return (UInt64(tmp), false)
  }
  @transparent
  type func rem(lhs: UInt64, rhs: UInt64, reportOverflow: Bool) -> (UInt64, Bool) {
    if rhs == 0 {
      return (0, true)
    }
    var tmp = Builtin.urem_Int64(lhs.value, rhs.value)
    return (UInt64(tmp), false)
  }

  @transparent
  type func sub(lhs: UInt64, rhs: UInt64) -> (DistanceType, Bool) {
    return sub(lhs, rhs, true)
  }
  @transparent
  type func add(lhs: UInt64, rhs: DistanceType) -> (UInt64, Bool) {
    return add(lhs, rhs, true)
  }
  @transparent
  type func mul(lhs: UInt64, rhs: UInt64) -> (UInt64, Bool) {
    return mul(lhs, rhs, true)
  }
  @transparent
  type func div(lhs: UInt64, rhs: UInt64) -> (UInt64, Bool) {
    return div(lhs, rhs, true)
  }
  @transparent
  type func rem(lhs: UInt64, rhs: UInt64) -> (UInt64, Bool) {
    return rem(lhs, rhs, true)
  }
}


// construction from other integer types
@transparent
extension UInt64 {
  init(v: UInt8) {
    var srcNotWord = v.value
    var dstNotWord = Builtin.zext_Int8_Int64(srcNotWord)
    value = dstNotWord
  }

  init(v: Int8) {
    var srcNotWord = v.value
    var tmp = Builtin.s_to_u_checked_conversion_Int8(srcNotWord)
    Builtin.condfail(tmp.1)
    var dstNotWord = Builtin.sext_Int8_Int64(tmp.0)
    value = dstNotWord
  }

  init(v: UInt16) {
    var srcNotWord = v.value
    var dstNotWord = Builtin.zext_Int16_Int64(srcNotWord)
    value = dstNotWord
  }

  init(v: Int16) {
    var srcNotWord = v.value
    var tmp = Builtin.s_to_u_checked_conversion_Int16(srcNotWord)
    Builtin.condfail(tmp.1)
    var dstNotWord = Builtin.sext_Int16_Int64(tmp.0)
    value = dstNotWord
  }

  init(v: UInt32) {
    var srcNotWord = v.value
    var dstNotWord = Builtin.zext_Int32_Int64(srcNotWord)
    value = dstNotWord
  }

  init(v: Int32) {
    var srcNotWord = v.value
    var tmp = Builtin.s_to_u_checked_conversion_Int32(srcNotWord)
    Builtin.condfail(tmp.1)
    var dstNotWord = Builtin.sext_Int32_Int64(tmp.0)
    value = dstNotWord
  }

  init(v: Int64) {
    var srcNotWord = v.value
    var tmp = Builtin.s_to_u_checked_conversion_Int64(srcNotWord)
    Builtin.condfail(tmp.1)
    var dstNotWord = tmp.0
    value = dstNotWord
  }

  init(v: UInt) {
    var srcNotWord = Builtin.truncOrBitCast_Word_Int32(v.value)
    var dstNotWord = Builtin.zext_Int32_Int64(srcNotWord)
    value = dstNotWord
  }

  init(v: Int) {
    var srcNotWord = Builtin.truncOrBitCast_Word_Int32(v.value)
    var tmp = Builtin.s_to_u_checked_conversion_Int32(srcNotWord)
    Builtin.condfail(tmp.1)
    var dstNotWord = Builtin.sext_Int32_Int64(tmp.0)
    value = dstNotWord
  }

  func asSigned() -> Int64 {
    return Int64(value)
  }
}

// Operations with masking and non-masking versions
@transparent
func &+ (lhs: UInt64, rhs: UInt64) -> UInt64 {
  return UInt64.add(lhs, rhs, false).0
}
@transparent
func + (lhs: UInt64, rhs: UInt64) -> UInt64 {
  var tmp = UInt64.add(lhs, rhs)
  Builtin.condfail(tmp.1.value)
  return tmp.0
}
@transparent
func &* (lhs: UInt64, rhs: UInt64) -> UInt64 {
  return UInt64.mul(lhs, rhs, false).0
}
@transparent
func * (lhs: UInt64, rhs: UInt64) -> UInt64 {
  var tmp = UInt64.mul(lhs, rhs)
  Builtin.condfail(tmp.1.value)
  return tmp.0
}
@transparent
func &- (lhs: UInt64, rhs: UInt64) -> UInt64 {
  return UInt64.sub(lhs, rhs, false).0
}
@transparent
func - (lhs: UInt64, rhs: UInt64) -> UInt64 {
  var tmp = UInt64.sub(lhs, rhs)
  Builtin.condfail(tmp.1.value)
  return tmp.0
}

// Bitwise negate
@transparent @prefix
func ~(rhs: UInt64) -> UInt64 {
  let mask = UInt64.sub(0, 1).0
  return UInt64(Builtin.xor_Int64(rhs.value, mask.value))
}

@transparent
func == (lhs: UInt64, rhs: UInt64) -> Bool {
  return Bool(Builtin.cmp_eq_Int64(lhs.value, rhs.value))
}
@transparent
func != (lhs: UInt64, rhs: UInt64) -> Bool {
  return Bool(Builtin.cmp_ne_Int64(lhs.value, rhs.value))
}
@transparent
func < (lhs: UInt64, rhs: UInt64) -> Bool {
  return Bool(Builtin.cmp_ult_Int64(lhs.value, rhs.value))
}
@transparent
func <= (lhs: UInt64, rhs: UInt64) -> Bool {
  return Bool(Builtin.cmp_ule_Int64(lhs.value, rhs.value))
}
@transparent
func > (lhs: UInt64, rhs: UInt64) -> Bool {
  return Bool(Builtin.cmp_ugt_Int64(lhs.value, rhs.value))
}
@transparent
func >= (lhs: UInt64, rhs: UInt64) -> Bool {
  return Bool(Builtin.cmp_uge_Int64(lhs.value, rhs.value))
}

@transparent
func << (lhs: UInt64, rhs: UInt64) -> UInt64 {
  return UInt64(Builtin.shl_Int64(lhs.value, rhs.value))
}
@transparent
func >> (lhs: UInt64, rhs: UInt64) -> UInt64 {
  return UInt64(Builtin.lshr_Int64(lhs.value, rhs.value))
}
@transparent
func & (lhs: UInt64, rhs: UInt64) -> UInt64 {
  return UInt64(Builtin.and_Int64(lhs.value, rhs.value))
}
@transparent
func ^ (lhs: UInt64, rhs: UInt64) -> UInt64 {
  return UInt64(Builtin.xor_Int64(lhs.value, rhs.value))
}
@transparent
func | (lhs: UInt64, rhs: UInt64) -> UInt64 {
  return UInt64(Builtin.or_Int64(lhs.value, rhs.value))
}

// bitwise operations
@transparent
extension UInt64 : BitwiseOperations {
  type func allZeros() -> UInt64 { return 0 }
}

// Compound assignments
@transparent @assignment
func +=(lhs: @inout UInt64, rhs: UInt64) {
  lhs = lhs + rhs
}
@transparent @assignment
func -=(lhs: @inout UInt64, rhs: UInt64) {
  lhs = lhs - rhs
}
@transparent @assignment
func *=(lhs: @inout UInt64, rhs: UInt64) {
  lhs = lhs * rhs
}
@transparent @assignment
func <<=(lhs: @inout UInt64, rhs: UInt64) {
  lhs = lhs << rhs
}
@transparent @assignment
func >>=(lhs: @inout UInt64, rhs: UInt64) {
  lhs = lhs >> rhs
}
@transparent @assignment
func &=(lhs: @inout UInt64, rhs: UInt64) {
  lhs = lhs & rhs
}
@transparent @assignment
func |=(lhs: @inout UInt64, rhs: UInt64) {
  lhs = lhs | rhs
}
@transparent @assignment
func ^=(lhs: @inout UInt64, rhs: UInt64) {
  lhs = lhs ^ rhs
}

struct Int64 : BuiltinIntegerLiteralConvertible, IntegerLiteralConvertible,
                ArrayBound, ReplPrintable {
  var value: Builtin.Int64

  @transparent
  init() {
    var maxWidthZero: Int64 = 0
    value = Builtin.truncOrBitCast_Int64_Int64(maxWidthZero.value)
  }

  @transparent
  init(v: Builtin.Int64) {
    value = v
  }

  @transparent
  type func _convertFromBuiltinIntegerLiteral(val: Builtin.Int2048) -> Int64 {
    return Int64(Builtin.s_to_s_checked_trunc_Int2048_Int64(val).0)
  }

  @transparent
  type func convertFromIntegerLiteral(value: Int64) -> Int64 {
    return value
  }
  @transparent
  func _getBuiltinArrayBoundValue() -> Builtin.Word {
    return Builtin.truncOrBitCast_Int64_Word(self.value)
  }

  typealias ArrayBoundType = Int64
  func getArrayBoundValue() -> Int64 {
    return self
  }

  func replPrint() {
    print(self)
  }

  @transparent
  type var max: Int64 { return 0x7FFF_FFFF_FFFF_FFFF }
  @transparent
  type var min: Int64 { return -0x7FFF_FFFF_FFFF_FFFF-1 }
}

extension Int64 : Hashable {
  func hashValue() -> Int {
    var result: Int = 0
    for var i = 0; i < (sizeof(self) * 8); i += sizeof(Int) * 8 {
      result ^= Int(self >> Int64(i)) & ~0
    }
    return result
  }
}

@transparent
extension Int64 : RandomAccessIndex {
  @transparent
  func succ() -> Int64 {
    return self + 1
  }
  @transparent
  func pred() -> Int64 {
    return self - 1
  }
  typealias DistanceType = Int64
  @transparent
  type func sub(lhs: Int64, rhs: Int64, reportOverflow: Bool) -> (DistanceType, Bool) {
    var tmp = Builtin.ssub_with_overflow_Int64(lhs.value, rhs.value, reportOverflow.value)
    return (Int64(tmp.0), Bool(tmp.1))
  }
  @transparent
  type func add(lhs: Int64, rhs: DistanceType, reportOverflow: Bool) -> (Int64, Bool) {
    var tmp = Builtin.sadd_with_overflow_Int64(lhs.value, rhs.value, reportOverflow.value)
    return (Int64(tmp.0), Bool(tmp.1))
  }
  @transparent
  type func mul(lhs: Int64, rhs: Int64, reportOverflow: Bool) -> (Int64, Bool) {
    var tmp = Builtin.smul_with_overflow_Int64(lhs.value, rhs.value, reportOverflow.value)
    return (Int64(tmp.0), Bool(tmp.1))
  }
  @transparent
  type func div(lhs: Int64, rhs: Int64, reportOverflow: Bool) -> (Int64, Bool) {
    if rhs == 0 {
      return (0, true)
    }
    if lhs == Int64.min && rhs == -1 {
      return (lhs, true)
    }
    var tmp = Builtin.sdiv_Int64(lhs.value, rhs.value)
    return (Int64(tmp), false)
  }
  @transparent
  type func rem(lhs: Int64, rhs: Int64, reportOverflow: Bool) -> (Int64, Bool) {
    if rhs == 0 {
      return (0, true)
    }
    if lhs == Int64.min && rhs == -1 {
      return (0, true)
    }
    var tmp = Builtin.srem_Int64(lhs.value, rhs.value)
    return (Int64(tmp), false)
  }

  @transparent
  type func sub(lhs: Int64, rhs: Int64) -> (DistanceType, Bool) {
    return sub(lhs, rhs, true)
  }
  @transparent
  type func add(lhs: Int64, rhs: DistanceType) -> (Int64, Bool) {
    return add(lhs, rhs, true)
  }
  @transparent
  type func mul(lhs: Int64, rhs: Int64) -> (Int64, Bool) {
    return mul(lhs, rhs, true)
  }
  @transparent
  type func div(lhs: Int64, rhs: Int64) -> (Int64, Bool) {
    return div(lhs, rhs, true)
  }
  @transparent
  type func rem(lhs: Int64, rhs: Int64) -> (Int64, Bool) {
    return rem(lhs, rhs, true)
  }
}

@transparent
extension Int64 : SignedNumber {
  @transparent
  type func negate(rhs: Int64) -> (Int64, Bool) {
    return Int64.sub(0, rhs)
  }
  @transparent
  type func abs(rhs: Int64) -> (Int64, Bool) {
    return rhs.isNegative() ? Int64.negate(rhs) : (rhs, false)
  }
  @transparent
  func isNegative() -> Bool { return self < 0 }
}

// construction from other integer types
@transparent
extension Int64 {
  init(v: UInt8) {
    var srcNotWord = v.value
    var dstNotWord = Builtin.zext_Int8_Int64(srcNotWord)
    value = dstNotWord
  }

  init(v: Int8) {
    var srcNotWord = v.value
    var dstNotWord = Builtin.sext_Int8_Int64(srcNotWord)
    value = dstNotWord
  }

  init(v: UInt16) {
    var srcNotWord = v.value
    var dstNotWord = Builtin.zext_Int16_Int64(srcNotWord)
    value = dstNotWord
  }

  init(v: Int16) {
    var srcNotWord = v.value
    var dstNotWord = Builtin.sext_Int16_Int64(srcNotWord)
    value = dstNotWord
  }

  init(v: UInt32) {
    var srcNotWord = v.value
    var dstNotWord = Builtin.zext_Int32_Int64(srcNotWord)
    value = dstNotWord
  }

  init(v: Int32) {
    var srcNotWord = v.value
    var dstNotWord = Builtin.sext_Int32_Int64(srcNotWord)
    value = dstNotWord
  }

  init(v: UInt64) {
    var srcNotWord = v.value
    var tmp = Builtin.u_to_s_checked_conversion_Int64(srcNotWord)
    Builtin.condfail(tmp.1)
    var dstNotWord = tmp.0
    value = dstNotWord
  }

  init(v: UInt) {
    var srcNotWord = Builtin.truncOrBitCast_Word_Int32(v.value)
    var dstNotWord = Builtin.zext_Int32_Int64(srcNotWord)
    value = dstNotWord
  }

  init(v: Int) {
    var srcNotWord = Builtin.truncOrBitCast_Word_Int32(v.value)
    var dstNotWord = Builtin.sext_Int32_Int64(srcNotWord)
    value = dstNotWord
  }

  func asUnsigned() -> UInt64 {
    return UInt64(value)
  }
}

// Operations with masking and non-masking versions
@transparent
func &+ (lhs: Int64, rhs: Int64) -> Int64 {
  return Int64.add(lhs, rhs, false).0
}
@transparent
func + (lhs: Int64, rhs: Int64) -> Int64 {
  var tmp = Int64.add(lhs, rhs)
  Builtin.condfail(tmp.1.value)
  return tmp.0
}
@transparent
func &* (lhs: Int64, rhs: Int64) -> Int64 {
  return Int64.mul(lhs, rhs, false).0
}
@transparent
func * (lhs: Int64, rhs: Int64) -> Int64 {
  var tmp = Int64.mul(lhs, rhs)
  Builtin.condfail(tmp.1.value)
  return tmp.0
}
@transparent
func &- (lhs: Int64, rhs: Int64) -> Int64 {
  return Int64.sub(lhs, rhs, false).0
}
@transparent
func - (lhs: Int64, rhs: Int64) -> Int64 {
  var tmp = Int64.sub(lhs, rhs)
  Builtin.condfail(tmp.1.value)
  return tmp.0
}

// Bitwise negate
@transparent @prefix
func ~(rhs: Int64) -> Int64 {
  let mask = Int64.sub(0, 1).0
  return Int64(Builtin.xor_Int64(rhs.value, mask.value))
}

@transparent
func == (lhs: Int64, rhs: Int64) -> Bool {
  return Bool(Builtin.cmp_eq_Int64(lhs.value, rhs.value))
}
@transparent
func != (lhs: Int64, rhs: Int64) -> Bool {
  return Bool(Builtin.cmp_ne_Int64(lhs.value, rhs.value))
}
@transparent
func < (lhs: Int64, rhs: Int64) -> Bool {
  return Bool(Builtin.cmp_slt_Int64(lhs.value, rhs.value))
}
@transparent
func <= (lhs: Int64, rhs: Int64) -> Bool {
  return Bool(Builtin.cmp_sle_Int64(lhs.value, rhs.value))
}
@transparent
func > (lhs: Int64, rhs: Int64) -> Bool {
  return Bool(Builtin.cmp_sgt_Int64(lhs.value, rhs.value))
}
@transparent
func >= (lhs: Int64, rhs: Int64) -> Bool {
  return Bool(Builtin.cmp_sge_Int64(lhs.value, rhs.value))
}

@transparent
func << (lhs: Int64, rhs: Int64) -> Int64 {
  return Int64(Builtin.shl_Int64(lhs.value, rhs.value))
}
@transparent
func >> (lhs: Int64, rhs: Int64) -> Int64 {
  return Int64(Builtin.ashr_Int64(lhs.value, rhs.value))
}
@transparent
func & (lhs: Int64, rhs: Int64) -> Int64 {
  return Int64(Builtin.and_Int64(lhs.value, rhs.value))
}
@transparent
func ^ (lhs: Int64, rhs: Int64) -> Int64 {
  return Int64(Builtin.xor_Int64(lhs.value, rhs.value))
}
@transparent
func | (lhs: Int64, rhs: Int64) -> Int64 {
  return Int64(Builtin.or_Int64(lhs.value, rhs.value))
}

// bitwise operations
@transparent
extension Int64 : BitwiseOperations {
  type func allZeros() -> Int64 { return 0 }
}

// Compound assignments
@transparent @assignment
func +=(lhs: @inout Int64, rhs: Int64) {
  lhs = lhs + rhs
}
@transparent @assignment
func -=(lhs: @inout Int64, rhs: Int64) {
  lhs = lhs - rhs
}
@transparent @assignment
func *=(lhs: @inout Int64, rhs: Int64) {
  lhs = lhs * rhs
}
@transparent @assignment
func <<=(lhs: @inout Int64, rhs: Int64) {
  lhs = lhs << rhs
}
@transparent @assignment
func >>=(lhs: @inout Int64, rhs: Int64) {
  lhs = lhs >> rhs
}
@transparent @assignment
func &=(lhs: @inout Int64, rhs: Int64) {
  lhs = lhs & rhs
}
@transparent @assignment
func |=(lhs: @inout Int64, rhs: Int64) {
  lhs = lhs | rhs
}
@transparent @assignment
func ^=(lhs: @inout Int64, rhs: Int64) {
  lhs = lhs ^ rhs
}

struct UInt : BuiltinIntegerLiteralConvertible, IntegerLiteralConvertible,
                ArrayBound, ReplPrintable {
  var value: Builtin.Word

  @transparent
  init() {
    var maxWidthZero: Int64 = 0
    value = Builtin.truncOrBitCast_Int64_Word(maxWidthZero.value)
  }

  @transparent
  init(v: Builtin.Word) {
    value = v
  }

  @transparent
  type func _convertFromBuiltinIntegerLiteral(val: Builtin.Int2048) -> UInt {
    return UInt(Builtin.s_to_u_checked_trunc_Int2048_Word(val).0)
  }

  @transparent
  type func convertFromIntegerLiteral(value: UInt) -> UInt {
    return value
  }
  @transparent
  func _getBuiltinArrayBoundValue() -> Builtin.Word {
    return self.value
  }

  typealias ArrayBoundType = UInt
  func getArrayBoundValue() -> UInt {
    return self
  }

  func replPrint() {
    print(UInt64(self))
  }

  @transparent
  type var max: UInt { return 0xFFFF_FFFF }
  @transparent
  type var min: UInt { return 0 }
}

extension UInt : Hashable {
  func hashValue() -> Int {
    return Int(self.value)
  }
}

@transparent
extension UInt : RandomAccessIndex {
  @transparent
  func succ() -> UInt {
    return self + 1
  }
  @transparent
  func pred() -> UInt {
    return self - 1
  }
  typealias DistanceType = UInt
  @transparent
  type func sub(lhs: UInt, rhs: UInt, reportOverflow: Bool) -> (DistanceType, Bool) {
    var tmp = Builtin.usub_with_overflow_Word(lhs.value, rhs.value, reportOverflow.value)
    return (UInt(tmp.0), Bool(tmp.1))
  }
  @transparent
  type func add(lhs: UInt, rhs: DistanceType, reportOverflow: Bool) -> (UInt, Bool) {
    var tmp = Builtin.uadd_with_overflow_Word(lhs.value, rhs.value, reportOverflow.value)
    return (UInt(tmp.0), Bool(tmp.1))
  }
  @transparent
  type func mul(lhs: UInt, rhs: UInt, reportOverflow: Bool) -> (UInt, Bool) {
    var tmp = Builtin.umul_with_overflow_Word(lhs.value, rhs.value, reportOverflow.value)
    return (UInt(tmp.0), Bool(tmp.1))
  }
  @transparent
  type func div(lhs: UInt, rhs: UInt, reportOverflow: Bool) -> (UInt, Bool) {
    if rhs == 0 {
      return (0, true)
    }
    var tmp = Builtin.udiv_Word(lhs.value, rhs.value)
    return (UInt(tmp), false)
  }
  @transparent
  type func rem(lhs: UInt, rhs: UInt, reportOverflow: Bool) -> (UInt, Bool) {
    if rhs == 0 {
      return (0, true)
    }
    var tmp = Builtin.urem_Word(lhs.value, rhs.value)
    return (UInt(tmp), false)
  }

  @transparent
  type func sub(lhs: UInt, rhs: UInt) -> (DistanceType, Bool) {
    return sub(lhs, rhs, true)
  }
  @transparent
  type func add(lhs: UInt, rhs: DistanceType) -> (UInt, Bool) {
    return add(lhs, rhs, true)
  }
  @transparent
  type func mul(lhs: UInt, rhs: UInt) -> (UInt, Bool) {
    return mul(lhs, rhs, true)
  }
  @transparent
  type func div(lhs: UInt, rhs: UInt) -> (UInt, Bool) {
    return div(lhs, rhs, true)
  }
  @transparent
  type func rem(lhs: UInt, rhs: UInt) -> (UInt, Bool) {
    return rem(lhs, rhs, true)
  }
}


// construction from other integer types
@transparent
extension UInt {
  init(v: UInt8) {
    var srcNotWord = v.value
    var dstNotWord = Builtin.zext_Int8_Int32(srcNotWord)
    value = Builtin.zextOrBitCast_Int32_Word(dstNotWord)
  }

  init(v: Int8) {
    var srcNotWord = v.value
    var tmp = Builtin.s_to_u_checked_conversion_Int8(srcNotWord)
    Builtin.condfail(tmp.1)
    var dstNotWord = Builtin.sext_Int8_Int32(tmp.0)
    value = Builtin.zextOrBitCast_Int32_Word(dstNotWord)
  }

  init(v: UInt16) {
    var srcNotWord = v.value
    var dstNotWord = Builtin.zext_Int16_Int32(srcNotWord)
    value = Builtin.zextOrBitCast_Int32_Word(dstNotWord)
  }

  init(v: Int16) {
    var srcNotWord = v.value
    var tmp = Builtin.s_to_u_checked_conversion_Int16(srcNotWord)
    Builtin.condfail(tmp.1)
    var dstNotWord = Builtin.sext_Int16_Int32(tmp.0)
    value = Builtin.zextOrBitCast_Int32_Word(dstNotWord)
  }

  init(v: UInt32) {
    var srcNotWord = v.value
    var dstNotWord = srcNotWord
    value = Builtin.zextOrBitCast_Int32_Word(dstNotWord)
  }

  init(v: Int32) {
    var srcNotWord = v.value
    var tmp = Builtin.s_to_u_checked_conversion_Int32(srcNotWord)
    Builtin.condfail(tmp.1)
    var dstNotWord = tmp.0
    value = Builtin.zextOrBitCast_Int32_Word(dstNotWord)
  }

  init(v: UInt64) {
    var srcNotWord = v.value
    var tmp = Builtin.u_to_u_checked_trunc_Int64_Int32(srcNotWord)
    Builtin.condfail(tmp.1)
    var dstNotWord = tmp.0
    value = Builtin.zextOrBitCast_Int32_Word(dstNotWord)
  }

  init(v: Int64) {
    var srcNotWord = v.value
    var tmp = Builtin.s_to_u_checked_trunc_Int64_Int32(srcNotWord)
    Builtin.condfail(tmp.1)
    var dstNotWord = tmp.0
    value = Builtin.zextOrBitCast_Int32_Word(dstNotWord)
  }

  init(v: Int) {
    var srcNotWord = Builtin.truncOrBitCast_Word_Int32(v.value)
    var tmp = Builtin.s_to_u_checked_conversion_Int32(srcNotWord)
    Builtin.condfail(tmp.1)
    var dstNotWord = tmp.0
    value = Builtin.zextOrBitCast_Int32_Word(dstNotWord)
  }

  func asSigned() -> Int {
    return Int(value)
  }
}

// Operations with masking and non-masking versions
@transparent
func &+ (lhs: UInt, rhs: UInt) -> UInt {
  return UInt.add(lhs, rhs, false).0
}
@transparent
func + (lhs: UInt, rhs: UInt) -> UInt {
  var tmp = UInt.add(lhs, rhs)
  Builtin.condfail(tmp.1.value)
  return tmp.0
}
@transparent
func &* (lhs: UInt, rhs: UInt) -> UInt {
  return UInt.mul(lhs, rhs, false).0
}
@transparent
func * (lhs: UInt, rhs: UInt) -> UInt {
  var tmp = UInt.mul(lhs, rhs)
  Builtin.condfail(tmp.1.value)
  return tmp.0
}
@transparent
func &- (lhs: UInt, rhs: UInt) -> UInt {
  return UInt.sub(lhs, rhs, false).0
}
@transparent
func - (lhs: UInt, rhs: UInt) -> UInt {
  var tmp = UInt.sub(lhs, rhs)
  Builtin.condfail(tmp.1.value)
  return tmp.0
}

// Bitwise negate
@transparent @prefix
func ~(rhs: UInt) -> UInt {
  let mask = UInt.sub(0, 1).0
  return UInt(Builtin.xor_Word(rhs.value, mask.value))
}

@transparent
func == (lhs: UInt, rhs: UInt) -> Bool {
  return Bool(Builtin.cmp_eq_Word(lhs.value, rhs.value))
}
@transparent
func != (lhs: UInt, rhs: UInt) -> Bool {
  return Bool(Builtin.cmp_ne_Word(lhs.value, rhs.value))
}
@transparent
func < (lhs: UInt, rhs: UInt) -> Bool {
  return Bool(Builtin.cmp_ult_Word(lhs.value, rhs.value))
}
@transparent
func <= (lhs: UInt, rhs: UInt) -> Bool {
  return Bool(Builtin.cmp_ule_Word(lhs.value, rhs.value))
}
@transparent
func > (lhs: UInt, rhs: UInt) -> Bool {
  return Bool(Builtin.cmp_ugt_Word(lhs.value, rhs.value))
}
@transparent
func >= (lhs: UInt, rhs: UInt) -> Bool {
  return Bool(Builtin.cmp_uge_Word(lhs.value, rhs.value))
}

@transparent
func << (lhs: UInt, rhs: UInt) -> UInt {
  return UInt(Builtin.shl_Word(lhs.value, rhs.value))
}
@transparent
func >> (lhs: UInt, rhs: UInt) -> UInt {
  return UInt(Builtin.lshr_Word(lhs.value, rhs.value))
}
@transparent
func & (lhs: UInt, rhs: UInt) -> UInt {
  return UInt(Builtin.and_Word(lhs.value, rhs.value))
}
@transparent
func ^ (lhs: UInt, rhs: UInt) -> UInt {
  return UInt(Builtin.xor_Word(lhs.value, rhs.value))
}
@transparent
func | (lhs: UInt, rhs: UInt) -> UInt {
  return UInt(Builtin.or_Word(lhs.value, rhs.value))
}

// bitwise operations
@transparent
extension UInt : BitwiseOperations {
  type func allZeros() -> UInt { return 0 }
}

// Compound assignments
@transparent @assignment
func +=(lhs: @inout UInt, rhs: UInt) {
  lhs = lhs + rhs
}
@transparent @assignment
func -=(lhs: @inout UInt, rhs: UInt) {
  lhs = lhs - rhs
}
@transparent @assignment
func *=(lhs: @inout UInt, rhs: UInt) {
  lhs = lhs * rhs
}
@transparent @assignment
func <<=(lhs: @inout UInt, rhs: UInt) {
  lhs = lhs << rhs
}
@transparent @assignment
func >>=(lhs: @inout UInt, rhs: UInt) {
  lhs = lhs >> rhs
}
@transparent @assignment
func &=(lhs: @inout UInt, rhs: UInt) {
  lhs = lhs & rhs
}
@transparent @assignment
func |=(lhs: @inout UInt, rhs: UInt) {
  lhs = lhs | rhs
}
@transparent @assignment
func ^=(lhs: @inout UInt, rhs: UInt) {
  lhs = lhs ^ rhs
}

struct Int : BuiltinIntegerLiteralConvertible, IntegerLiteralConvertible,
                ArrayBound, ReplPrintable {
  var value: Builtin.Word

  @transparent
  init() {
    var maxWidthZero: Int64 = 0
    value = Builtin.truncOrBitCast_Int64_Word(maxWidthZero.value)
  }

  @transparent
  init(v: Builtin.Word) {
    value = v
  }

  @transparent
  type func _convertFromBuiltinIntegerLiteral(val: Builtin.Int2048) -> Int {
    return Int(Builtin.s_to_s_checked_trunc_Int2048_Word(val).0)
  }

  @transparent
  type func convertFromIntegerLiteral(value: Int) -> Int {
    return value
  }
  @transparent
  func _getBuiltinArrayBoundValue() -> Builtin.Word {
    return self.value
  }

  typealias ArrayBoundType = Int
  func getArrayBoundValue() -> Int {
    return self
  }

  func replPrint() {
    print(Int64(self))
  }

  @transparent
  type var max: Int { return 0x7FFF_FFFF }
  @transparent
  type var min: Int { return -0x7FFF_FFFF-1 }
}

extension Int : Hashable {
  func hashValue() -> Int {
    return Int(self.value)
  }
}

@transparent
extension Int : RandomAccessIndex {
  @transparent
  func succ() -> Int {
    return self + 1
  }
  @transparent
  func pred() -> Int {
    return self - 1
  }
  typealias DistanceType = Int
  @transparent
  type func sub(lhs: Int, rhs: Int, reportOverflow: Bool) -> (DistanceType, Bool) {
    var tmp = Builtin.ssub_with_overflow_Word(lhs.value, rhs.value, reportOverflow.value)
    return (Int(tmp.0), Bool(tmp.1))
  }
  @transparent
  type func add(lhs: Int, rhs: DistanceType, reportOverflow: Bool) -> (Int, Bool) {
    var tmp = Builtin.sadd_with_overflow_Word(lhs.value, rhs.value, reportOverflow.value)
    return (Int(tmp.0), Bool(tmp.1))
  }
  @transparent
  type func mul(lhs: Int, rhs: Int, reportOverflow: Bool) -> (Int, Bool) {
    var tmp = Builtin.smul_with_overflow_Word(lhs.value, rhs.value, reportOverflow.value)
    return (Int(tmp.0), Bool(tmp.1))
  }
  @transparent
  type func div(lhs: Int, rhs: Int, reportOverflow: Bool) -> (Int, Bool) {
    if rhs == 0 {
      return (0, true)
    }
    if lhs == Int.min && rhs == -1 {
      return (lhs, true)
    }
    var tmp = Builtin.sdiv_Word(lhs.value, rhs.value)
    return (Int(tmp), false)
  }
  @transparent
  type func rem(lhs: Int, rhs: Int, reportOverflow: Bool) -> (Int, Bool) {
    if rhs == 0 {
      return (0, true)
    }
    if lhs == Int.min && rhs == -1 {
      return (0, true)
    }
    var tmp = Builtin.srem_Word(lhs.value, rhs.value)
    return (Int(tmp), false)
  }

  @transparent
  type func sub(lhs: Int, rhs: Int) -> (DistanceType, Bool) {
    return sub(lhs, rhs, true)
  }
  @transparent
  type func add(lhs: Int, rhs: DistanceType) -> (Int, Bool) {
    return add(lhs, rhs, true)
  }
  @transparent
  type func mul(lhs: Int, rhs: Int) -> (Int, Bool) {
    return mul(lhs, rhs, true)
  }
  @transparent
  type func div(lhs: Int, rhs: Int) -> (Int, Bool) {
    return div(lhs, rhs, true)
  }
  @transparent
  type func rem(lhs: Int, rhs: Int) -> (Int, Bool) {
    return rem(lhs, rhs, true)
  }
}

@transparent
extension Int : SignedNumber {
  @transparent
  type func negate(rhs: Int) -> (Int, Bool) {
    return Int.sub(0, rhs)
  }
  @transparent
  type func abs(rhs: Int) -> (Int, Bool) {
    return rhs.isNegative() ? Int.negate(rhs) : (rhs, false)
  }
  @transparent
  func isNegative() -> Bool { return self < 0 }
}

// construction from other integer types
@transparent
extension Int {
  init(v: UInt8) {
    var srcNotWord = v.value
    var dstNotWord = Builtin.zext_Int8_Int32(srcNotWord)
    value = Builtin.zextOrBitCast_Int32_Word(dstNotWord)
  }

  init(v: Int8) {
    var srcNotWord = v.value
    var dstNotWord = Builtin.sext_Int8_Int32(srcNotWord)
    value = Builtin.zextOrBitCast_Int32_Word(dstNotWord)
  }

  init(v: UInt16) {
    var srcNotWord = v.value
    var dstNotWord = Builtin.zext_Int16_Int32(srcNotWord)
    value = Builtin.zextOrBitCast_Int32_Word(dstNotWord)
  }

  init(v: Int16) {
    var srcNotWord = v.value
    var dstNotWord = Builtin.sext_Int16_Int32(srcNotWord)
    value = Builtin.zextOrBitCast_Int32_Word(dstNotWord)
  }

  init(v: UInt32) {
    var srcNotWord = v.value
    var tmp = Builtin.u_to_s_checked_conversion_Int32(srcNotWord)
    Builtin.condfail(tmp.1)
    var dstNotWord = tmp.0
    value = Builtin.zextOrBitCast_Int32_Word(dstNotWord)
  }

  init(v: Int32) {
    var srcNotWord = v.value
    var dstNotWord = srcNotWord
    value = Builtin.zextOrBitCast_Int32_Word(dstNotWord)
  }

  init(v: UInt64) {
    var srcNotWord = v.value
    var tmp = Builtin.u_to_s_checked_trunc_Int64_Int32(srcNotWord)
    Builtin.condfail(tmp.1)
    var dstNotWord = tmp.0
    value = Builtin.zextOrBitCast_Int32_Word(dstNotWord)
  }

  init(v: Int64) {
    var srcNotWord = v.value
    var tmp = Builtin.s_to_s_checked_trunc_Int64_Int32(srcNotWord)
    Builtin.condfail(tmp.1)
    var dstNotWord = tmp.0
    value = Builtin.zextOrBitCast_Int32_Word(dstNotWord)
  }

  init(v: UInt) {
    var srcNotWord = Builtin.truncOrBitCast_Word_Int32(v.value)
    var tmp = Builtin.u_to_s_checked_conversion_Int32(srcNotWord)
    Builtin.condfail(tmp.1)
    var dstNotWord = tmp.0
    value = Builtin.zextOrBitCast_Int32_Word(dstNotWord)
  }

  func asUnsigned() -> UInt {
    return UInt(value)
  }
}

// Operations with masking and non-masking versions
@transparent
func &+ (lhs: Int, rhs: Int) -> Int {
  return Int.add(lhs, rhs, false).0
}
@transparent
func + (lhs: Int, rhs: Int) -> Int {
  var tmp = Int.add(lhs, rhs)
  Builtin.condfail(tmp.1.value)
  return tmp.0
}
@transparent
func &* (lhs: Int, rhs: Int) -> Int {
  return Int.mul(lhs, rhs, false).0
}
@transparent
func * (lhs: Int, rhs: Int) -> Int {
  var tmp = Int.mul(lhs, rhs)
  Builtin.condfail(tmp.1.value)
  return tmp.0
}
@transparent
func &- (lhs: Int, rhs: Int) -> Int {
  return Int.sub(lhs, rhs, false).0
}
@transparent
func - (lhs: Int, rhs: Int) -> Int {
  var tmp = Int.sub(lhs, rhs)
  Builtin.condfail(tmp.1.value)
  return tmp.0
}

// Bitwise negate
@transparent @prefix
func ~(rhs: Int) -> Int {
  let mask = Int.sub(0, 1).0
  return Int(Builtin.xor_Word(rhs.value, mask.value))
}

@transparent
func == (lhs: Int, rhs: Int) -> Bool {
  return Bool(Builtin.cmp_eq_Word(lhs.value, rhs.value))
}
@transparent
func != (lhs: Int, rhs: Int) -> Bool {
  return Bool(Builtin.cmp_ne_Word(lhs.value, rhs.value))
}
@transparent
func < (lhs: Int, rhs: Int) -> Bool {
  return Bool(Builtin.cmp_slt_Word(lhs.value, rhs.value))
}
@transparent
func <= (lhs: Int, rhs: Int) -> Bool {
  return Bool(Builtin.cmp_sle_Word(lhs.value, rhs.value))
}
@transparent
func > (lhs: Int, rhs: Int) -> Bool {
  return Bool(Builtin.cmp_sgt_Word(lhs.value, rhs.value))
}
@transparent
func >= (lhs: Int, rhs: Int) -> Bool {
  return Bool(Builtin.cmp_sge_Word(lhs.value, rhs.value))
}

@transparent
func << (lhs: Int, rhs: Int) -> Int {
  return Int(Builtin.shl_Word(lhs.value, rhs.value))
}
@transparent
func >> (lhs: Int, rhs: Int) -> Int {
  return Int(Builtin.ashr_Word(lhs.value, rhs.value))
}
@transparent
func & (lhs: Int, rhs: Int) -> Int {
  return Int(Builtin.and_Word(lhs.value, rhs.value))
}
@transparent
func ^ (lhs: Int, rhs: Int) -> Int {
  return Int(Builtin.xor_Word(lhs.value, rhs.value))
}
@transparent
func | (lhs: Int, rhs: Int) -> Int {
  return Int(Builtin.or_Word(lhs.value, rhs.value))
}

// bitwise operations
@transparent
extension Int : BitwiseOperations {
  type func allZeros() -> Int { return 0 }
}

// Compound assignments
@transparent @assignment
func +=(lhs: @inout Int, rhs: Int) {
  lhs = lhs + rhs
}
@transparent @assignment
func -=(lhs: @inout Int, rhs: Int) {
  lhs = lhs - rhs
}
@transparent @assignment
func *=(lhs: @inout Int, rhs: Int) {
  lhs = lhs * rhs
}
@transparent @assignment
func <<=(lhs: @inout Int, rhs: Int) {
  lhs = lhs << rhs
}
@transparent @assignment
func >>=(lhs: @inout Int, rhs: Int) {
  lhs = lhs >> rhs
}
@transparent @assignment
func &=(lhs: @inout Int, rhs: Int) {
  lhs = lhs & rhs
}
@transparent @assignment
func |=(lhs: @inout Int, rhs: Int) {
  lhs = lhs | rhs
}
@transparent @assignment
func ^=(lhs: @inout Int, rhs: Int) {
  lhs = lhs ^ rhs
}

// Word size lookup

@transparent
extension Int {
  type func bitSize() -> Int {
    return 32
  }
}

@transparent
extension UInt {
  type func bitSize() -> UInt {
    return 32
  }
}


typealias Word = Int
typealias UWord = UInt
