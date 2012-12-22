struct Int8 : Comparable {
  var value : Builtin.Int8

  static func convertFromIntegerLiteral(val : Builtin.Int8) -> Int8 {
    return Int8(val)
  }

  func replPrint() {
    print(Int64(this))
  }
  // FIXME:
  // static var max : Int8 { get { return 0x7F } }
  // static var min : Int8 { get { return -0x7F-1 } }
  static func max() -> Int8 { return 0x7F }
  static func min() -> Int8 { return -0x7F-1 }
}

struct UInt8 : Comparable {
  var value : Builtin.Int8

  static func convertFromIntegerLiteral(val : Builtin.Int8) -> UInt8 {
    return UInt8(val)
  }

  func replPrint() {
    print(UInt64(this))
  }
  // FIXME:
  // static var max : UInt8 { get { return 0xFF } }
  // static var min : UInt8 { get { return 0 } }
  static func max() -> UInt8 { return 0xFF }
  static func min() -> UInt8 { return 0 }
}

struct Int16 : Comparable {
  var value : Builtin.Int16

  static func convertFromIntegerLiteral(val : Builtin.Int16) -> Int16 {
    return Int16(val)
  }

  func replPrint() {
    print(Int64(this))
  }
  // FIXME:
  // static var max : Int16 { get { return 0x7FFF } }
  // static var min : Int16 { get { return -0x7FFF-1 } }
  static func max() -> Int16 { return 0x7FFF }
  static func min() -> Int16 { return -0x7FFF-1 }
}

struct UInt16 : Comparable {
  var value : Builtin.Int16

  static func convertFromIntegerLiteral(val : Builtin.Int16) -> UInt16 {
    return UInt16(val)
  }

  func replPrint() {
    print(UInt64(this))
  }
  // FIXME:
  // static var max : UInt16 { get { return 0xFFFF } }
  // static var min : UInt16 { get { return 0 } }
  static func max() -> UInt16 { return 0xFFFF }
  static func min() -> UInt16 { return 0 }
}

struct Int32 : Comparable {
  var value : Builtin.Int32

  static func convertFromIntegerLiteral(val : Builtin.Int32) -> Int32 {
    return Int32(val)
  }
  func getArrayBoundValue() -> Builtin.Int32 {
    return value
  }

  func replPrint() {
    print(Int64(this))
  }
  // FIXME:
  // static var max : Int32 { get { return 0x7FFFFFFF } }
  // static var min : Int32 { get { return -0x7FFFFFFF-1 } }
  static func max() -> Int32 { return 0x7FFFFFFF }
  static func min() -> Int32 { return -0x7FFFFFFF-1 }
}

struct UInt32 : Comparable {
  var value : Builtin.Int32

  static func convertFromIntegerLiteral(val : Builtin.Int32) -> UInt32 {
    return UInt32(val)
  }

  func replPrint() {
    print(UInt64(this))
  }
  // FIXME:
  // static var max : UInt32 { get { return 0xFFFFFFFF } }
  // static var min : UInt32 { get { return 0 } }
  static func max() -> UInt32 { return 0xFFFFFFFF }
  static func min() -> UInt32 { return 0 }
}

struct Int64 : Comparable {
  var value : Builtin.Int64

  static func convertFromIntegerLiteral(val : Builtin.Int64) -> Int {
    return Int64(val)
  }
  func getArrayBoundValue() -> Builtin.Int64 {
    return value
  }

  func replPrint() {
    print(this)
  }
  // FIXME:
  // static var max : Int64 { get { return 0x7FFFFFFFFFFFFFFF } }
  // static var min : Int64 { get { return -0x7FFFFFFFFFFFFFFF-1 } }
  static func max() -> Int64 { return 0x7FFFFFFFFFFFFFFF }
  static func min() -> Int64 { return -0x7FFFFFFFFFFFFFFF-1 }
}

struct UInt64 : Comparable {
  var value : Builtin.Int64

  static func convertFromIntegerLiteral(val : Builtin.Int64) -> UInt64 {
    return UInt64(val)
  }
  func getArrayBoundValue() -> Builtin.Int64 {
    return value
  }

  func replPrint() {
    print(this)
  }
  // FIXME:
  // static var max : UInt64 { get { return 0xFFFFFFFFFFFFFFFF } }
  // static var min : UInt64 { get { return 0 } }
  static func max() -> UInt64 { return 0xFFFFFFFFFFFFFFFF }
  static func min() -> UInt64 { return 0 }
}

struct Int128 : Comparable {
  var value : Builtin.Int128

  static func convertFromIntegerLiteral(val : Builtin.Int128) -> Int128 {
    return Int128(val)
  }

  func replPrint() {
    print(String(this))
  }
  // FIXME:
  // static var max : Int128 { get { return 0x7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF } }
  // static var min : Int128 { get { return -0x7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF-1 } }
  static func max() -> Int128 { return 0x7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF }
  static func min() -> Int128 { return -0x7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF-1 }
}

struct UInt128 : Comparable {
  var value : Builtin.Int128

  static func convertFromIntegerLiteral(val : Builtin.Int128) -> UInt128 {
    return UInt128(val)
  }

  func replPrint() {
    print(String(this))
  }
  // FIXME:
  // static var max : UInt128 { get { return 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF } }
  // static var min : UInt128 { get { return 0 } }
  static func max() -> UInt128 { return 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF }
  static func min() -> UInt128 { return 0 }
}

//===----------------------------------------------------------------------===//
// Explicit conversions between types.
//===----------------------------------------------------------------------===//

extension Int8 {
  constructor(v : UInt8) {
    value = v.value
  }
  constructor(v : Int16) {
    value = Builtin.trunc_Int16_Int8(v.value)
  }
  constructor(v : UInt16) {
    value = Builtin.trunc_Int16_Int8(v.value)
  }
  constructor(v : Int32) {
    value = Builtin.trunc_Int32_Int8(v.value)
  }
  constructor(v : UInt32) {
    value = Builtin.trunc_Int32_Int8(v.value)
  }
  constructor(v : Int64) {
    value = Builtin.trunc_Int64_Int8(v.value)
  }
  constructor(v : UInt64) {
    value = Builtin.trunc_Int64_Int8(v.value)
  }
  constructor(v : Int128) {
    value = Builtin.trunc_Int128_Int8(v.value)
  }
  constructor(v : UInt128) {
    value = Builtin.trunc_Int128_Int8(v.value)
  }
}

extension UInt8 {
  constructor(v : Int8) {
    value = v.value
  }
  constructor(v : Int16) {
    value = Builtin.trunc_Int16_Int8(v.value)
  }
  constructor(v : UInt16) {
    value = Builtin.trunc_Int16_Int8(v.value)
  }
  constructor(v : Int32) {
    value = Builtin.trunc_Int32_Int8(v.value)
  }
  constructor(v : UInt32) {
    value = Builtin.trunc_Int32_Int8(v.value)
  }
  constructor(v : Int64) {
    value = Builtin.trunc_Int64_Int8(v.value)
  }
  constructor(v : UInt64) {
    value = Builtin.trunc_Int64_Int8(v.value)
  }
  constructor(v : Int128) {
    value = Builtin.trunc_Int128_Int8(v.value)
  }
  constructor(v : UInt128) {
    value = Builtin.trunc_Int128_Int8(v.value)
  }
}

extension Int16 {
  constructor(v : Int8) {
    value = Builtin.sext_Int8_Int16(v.value)
  }
  constructor(v : UInt8) {
    value = Builtin.zext_Int8_Int16(v.value)
  }
  constructor(v : UInt16) {
    value = v.value
  }
  constructor(v : Int32) {
    value = Builtin.trunc_Int32_Int16(v.value)
  }
  constructor(v : UInt32) {
    value = Builtin.trunc_Int32_Int16(v.value)
  }
  constructor(v : Int64) {
    value = Builtin.trunc_Int64_Int16(v.value)
  }
  constructor(v : UInt64) {
    value = Builtin.trunc_Int64_Int16(v.value)
  }
  constructor(v : Int128) {
    value = Builtin.trunc_Int128_Int16(v.value)
  }
  constructor(v : UInt128) {
    value = Builtin.trunc_Int128_Int16(v.value)
  }
}

extension UInt16 {
  constructor(v : Int8) {
    value = Builtin.sext_Int8_Int16(v.value)
  }
  constructor(v : UInt8) {
    value = Builtin.zext_Int8_Int16(v.value)
  }
  constructor(v : Int16) {
    value = v.value
  }
  constructor(v : Int32) {
    value = Builtin.trunc_Int32_Int16(v.value)
  }
  constructor(v : UInt32) {
    value = Builtin.trunc_Int32_Int16(v.value)
  }
  constructor(v : Int64) {
    value = Builtin.trunc_Int64_Int16(v.value)
  }
  constructor(v : UInt64) {
    value = Builtin.trunc_Int64_Int16(v.value)
  }
  constructor(v : Int128) {
    value = Builtin.trunc_Int128_Int16(v.value)
  }
  constructor(v : UInt128) {
    value = Builtin.trunc_Int128_Int16(v.value)
  }
}

extension Int32 {
  constructor(v : Int8) {
    value = Builtin.sext_Int8_Int32(v.value)
  }
  constructor(v : UInt8) {
    value = Builtin.zext_Int8_Int32(v.value)
  }
  constructor(v : Int16) {
    value = Builtin.sext_Int16_Int32(v.value)
  }
  constructor(v : UInt16) {
    value = Builtin.zext_Int16_Int32(v.value)
  }
  constructor(v : UInt32) {
    value = v.value
  }
  constructor(v : Int64) {
    value = Builtin.trunc_Int64_Int32(v.value)
  }
  constructor(v : UInt64) {
    value = Builtin.trunc_Int64_Int32(v.value)
  }
  constructor(v : Int128) {
    value = Builtin.trunc_Int128_Int32(v.value)
  }
  constructor(v : UInt128) {
    value = Builtin.trunc_Int128_Int32(v.value)
  }
}

extension UInt32 {
  constructor(v : Int8) {
    value = Builtin.sext_Int8_Int32(v.value)
  }
  constructor(v : UInt8) {
    value = Builtin.zext_Int8_Int32(v.value)
  }
  constructor(v : Int16) {
    value = Builtin.sext_Int16_Int32(v.value)
  }
  constructor(v : UInt16) {
    value = Builtin.zext_Int16_Int32(v.value)
  }
  constructor(v : Int32) {
    value = v.value
  }
  constructor(v : Int64) {
    value = Builtin.trunc_Int64_Int32(v.value)
  }
  constructor(v : UInt64) {
    value = Builtin.trunc_Int64_Int32(v.value)
  }
  constructor(v : Int128) {
    value = Builtin.trunc_Int128_Int32(v.value)
  }
  constructor(v : UInt128) {
    value = Builtin.trunc_Int128_Int32(v.value)
  }
}

extension Int64 {
  constructor(v : Int8) {
    value = Builtin.sext_Int8_Int64(v.value)
  }
  constructor(v : UInt8) {
    value = Builtin.zext_Int8_Int64(v.value)
  }
  constructor(v : Int16) {
    value = Builtin.sext_Int16_Int64(v.value)
  }
  constructor(v : UInt16) {
    value = Builtin.zext_Int16_Int64(v.value)
  }
  constructor(v : Int32) {
    value = Builtin.sext_Int32_Int64(v.value)
  }
  constructor(v : UInt32) {
    value = Builtin.zext_Int32_Int64(v.value)
  }
  constructor(v : UInt64) {
    value = v.value
  }
  constructor(v : Int128) {
    value = Builtin.trunc_Int128_Int64(v.value)
  }
  constructor(v : UInt128) {
    value = Builtin.trunc_Int128_Int64(v.value)
  }
}

extension UInt64 {
  constructor(v : Int8) {
    value = Builtin.sext_Int8_Int64(v.value)
  }
  constructor(v : UInt8) {
    value = Builtin.zext_Int8_Int64(v.value)
  }
  constructor(v : Int16) {
    value = Builtin.sext_Int16_Int64(v.value)
  }
  constructor(v : UInt16) {
    value = Builtin.zext_Int16_Int64(v.value)
  }
  constructor(v : Int32) {
    value = Builtin.sext_Int32_Int64(v.value)
  }
  constructor(v : UInt32) {
    value = Builtin.zext_Int32_Int64(v.value)
  }
  constructor(v : Int64) {
    value = v.value
  }
  constructor(v : Int128) {
    value = Builtin.trunc_Int128_Int64(v.value)
  }
  constructor(v : UInt128) {
    value = Builtin.trunc_Int128_Int64(v.value)
  }
}

extension Int128 {
  constructor(v : Int8) {
    value = Builtin.sext_Int8_Int128(v.value)
  }
  constructor(v : UInt8) {
    value = Builtin.zext_Int8_Int128(v.value)
  }
  constructor(v : Int16) {
    value = Builtin.sext_Int16_Int128(v.value)
  }
  constructor(v : UInt16) {
    value = Builtin.zext_Int16_Int128(v.value)
  }
  constructor(v : Int32) {
    value = Builtin.sext_Int32_Int128(v.value)
  }
  constructor(v : UInt32) {
    value = Builtin.zext_Int32_Int128(v.value)
  }
  constructor(v : Int64) {
    value = Builtin.sext_Int64_Int128(v.value)
  }
  constructor(v : UInt64) {
    value = Builtin.zext_Int64_Int128(v.value)
  }
  constructor(v : UInt128) {
    value = v.value
  }
}

extension UInt128 {
  constructor(v : Int8) {
    value = Builtin.sext_Int8_Int128(v.value)
  }
  constructor(v : UInt8) {
    value = Builtin.zext_Int8_Int128(v.value)
  }
  constructor(v : Int16) {
    value = Builtin.sext_Int16_Int128(v.value)
  }
  constructor(v : UInt16) {
    value = Builtin.zext_Int16_Int128(v.value)
  }
  constructor(v : Int32) {
    value = Builtin.sext_Int32_Int128(v.value)
  }
  constructor(v : UInt32) {
    value = Builtin.zext_Int32_Int128(v.value)
  }
  constructor(v : Int64) {
    value = Builtin.sext_Int64_Int128(v.value)
  }
  constructor(v : UInt64) {
    value = Builtin.zext_Int64_Int128(v.value)
  }
  constructor(v : Int128) {
    value = v.value
  }
}

//===----------------------------------------------------------------------===//
// Standard Operators
//===----------------------------------------------------------------------===//

// Unary negation operators.
func -(a : Int8)   -> Int8   { return 0 - a }
func -(a : Int16)  -> Int16  { return 0 - a }
func -(a : Int32)  -> Int32  { return 0 - a }
func -(a : Int64)  -> Int64  { return 0 - a }
func -(a : Int128) -> Int128 { return 0 - a }

// Unary addition operators.
func +(a : Int8)    -> Int8   { return a }
func +(a : UInt8)   -> UInt8  { return a }
func +(a : Int16)   -> Int16  { return a }
func +(a : UInt16)  -> UInt16 { return a }
func +(a : Int32)   -> Int32  { return a }
func +(a : UInt32)  -> UInt32 { return a }
func +(a : Int64)   -> Int64  { return a }
func +(a : UInt64)  -> UInt64 { return a }
func +(a : Int128)  -> Int128 { return a }
func +(a : UInt128) -> UInt128 { return a }

// Bitwise negation operators.
func ~(a : Int8)    -> Int8   { return a ^ -1 }
func ~(a : UInt8)   -> UInt8  { return a ^ 0xFF }
func ~(a : Int16)   -> Int16  { return a ^ -1 }
func ~(a : UInt16)  -> UInt16 { return a ^ 0xFFFF }
func ~(a : Int32)   -> Int32  { return a ^ -1 }
func ~(a : UInt32)  -> UInt32 { return a ^ 0xFFFFFFFF }
func ~(a : Int64)   -> Int64  { return a ^ -1 }
func ~(a : UInt64)  -> UInt64 { return a ^ 0xFFFFFFFFFFFFFFFF }
func ~(a : Int128)  -> Int128 { return a ^ -1 }
func ~(a : UInt128) -> UInt128 { return a ^ 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF }

// Unary increment and decrement operators.  These intentionally return nothing.
func [assignment] ++(a : [byref] Int8) { a += 1 }
func [assignment] ++(a : [byref] UInt8) { a += 1 }
func [assignment] ++(a : [byref] Int16) { a += 1 }
func [assignment] ++(a : [byref] UInt16) { a += 1 }
func [assignment] ++(a : [byref] Int32) { a += 1 }
func [assignment] ++(a : [byref] UInt32) { a += 1 }
func [assignment] ++(a : [byref] Int64) { a += 1 }
func [assignment] ++(a : [byref] UInt64) { a += 1 }
func [assignment] ++(a : [byref] Int128) { a += 1 }
func [assignment] ++(a : [byref] UInt128) { a += 1 }
func [assignment] --(a : [byref] Int8) { a -= 1 }
func [assignment] --(a : [byref] UInt8) { a -= 1 }
func [assignment] --(a : [byref] Int16) { a -= 1 }
func [assignment] --(a : [byref] UInt16) { a -= 1 }
func [assignment] --(a : [byref] Int32) { a -= 1 }
func [assignment] --(a : [byref] UInt32) { a -= 1 }
func [assignment] --(a : [byref] Int64) { a -= 1 }
func [assignment] --(a : [byref] UInt64) { a -= 1 }
func [assignment] --(a : [byref] Int128) { a -= 1 }
func [assignment] --(a : [byref] UInt128) { a -= 1 }

// Binary Multiplication.
func [infix_left=200] * (lhs : Int8, rhs : Int8) -> Int8 {
  return Int8(Builtin.mul_Int8(lhs.value, rhs.value))
}
func [infix_left=200] * (lhs : UInt8, rhs : UInt8) -> UInt8 {
  return UInt8(Builtin.mul_Int8(lhs.value, rhs.value))
}
func [infix_left=200] * (lhs : Int16, rhs : Int16) -> Int16 {
  return Int16(Builtin.mul_Int16(lhs.value, rhs.value))
}
func [infix_left=200] * (lhs : UInt16, rhs : UInt16) -> UInt16 {
  return UInt16(Builtin.mul_Int16(lhs.value, rhs.value))
}
func [infix_left=200] * (lhs : Int32, rhs : Int32) -> Int32 {
  return Int32(Builtin.mul_Int32(lhs.value, rhs.value))
}
func [infix_left=200] * (lhs : UInt32, rhs : UInt32) -> UInt32 {
  return UInt32(Builtin.mul_Int32(lhs.value, rhs.value))
}
func [infix_left=200] * (lhs : Int64, rhs : Int64) -> Int64 {
  return Int64(Builtin.mul_Int64(lhs.value, rhs.value))
}
func [infix_left=200] * (lhs : UInt64, rhs : UInt64) -> UInt64 {
  return UInt64(Builtin.mul_Int64(lhs.value, rhs.value))
}
func [infix_left=200] * (lhs : Int128, rhs : Int128) -> Int128 {
  return Int128(Builtin.mul_Int128(lhs.value, rhs.value))
}
func [infix_left=200] * (lhs : UInt128, rhs : UInt128) -> UInt128 {
  return UInt128(Builtin.mul_Int128(lhs.value, rhs.value))
}


// Binary Division.
func [infix_left=200] / (lhs : Int8, rhs : Int8) -> Int8 {
  return Int8(Builtin.sdiv_Int8(lhs.value, rhs.value))
}
func [infix_left=200] / (lhs : UInt8, rhs : UInt8) -> UInt8 {
  return UInt8(Builtin.udiv_Int8(lhs.value, rhs.value))
}
func [infix_left=200] / (lhs : Int16, rhs : Int16) -> Int16 {
  return Int16(Builtin.sdiv_Int16(lhs.value, rhs.value))
}
func [infix_left=200] / (lhs : UInt16, rhs : UInt16) -> UInt16 {
  return UInt16(Builtin.udiv_Int16(lhs.value, rhs.value))
}
func [infix_left=200] / (lhs : Int32, rhs : Int32) -> Int32 {
  return Int32(Builtin.sdiv_Int32(lhs.value, rhs.value))
}
func [infix_left=200] / (lhs : UInt32, rhs : UInt32) -> UInt32 {
  return UInt32(Builtin.udiv_Int32(lhs.value, rhs.value))
}
func [infix_left=200] / (lhs : Int64, rhs : Int64) -> Int64 {
  return Int64(Builtin.sdiv_Int64(lhs.value, rhs.value))
}
func [infix_left=200] / (lhs : UInt64, rhs : UInt64) -> UInt64 {
  return UInt64(Builtin.udiv_Int64(lhs.value, rhs.value))
}
func [infix_left=200] / (lhs : Int128, rhs : Int128) -> Int128 {
  return Int128(Builtin.sdiv_Int128(lhs.value, rhs.value))
}
func [infix_left=200] / (lhs : UInt128, rhs : UInt128) -> UInt128 {
  return UInt128(Builtin.udiv_Int128(lhs.value, rhs.value))
}

// Binary Remainder.
func [infix_left=200] % (lhs : Int8, rhs : Int8) -> Int8 {
  return Int8(Builtin.srem_Int8(lhs.value, rhs.value))
}
func [infix_left=200] % (lhs : UInt8, rhs : UInt8) -> UInt8 {
  return UInt8(Builtin.urem_Int8(lhs.value, rhs.value))
}
func [infix_left=200] % (lhs : Int16, rhs : Int16) -> Int16 {
  return Int16(Builtin.srem_Int16(lhs.value, rhs.value))
}
func [infix_left=200] % (lhs : UInt16, rhs : UInt16) -> UInt16 {
  return UInt16(Builtin.urem_Int16(lhs.value, rhs.value))
}
func [infix_left=200] % (lhs : Int32, rhs : Int32) -> Int32 {
  return Int32(Builtin.srem_Int32(lhs.value, rhs.value))
}
func [infix_left=200] % (lhs : UInt32, rhs : UInt32) -> UInt32 {
  return UInt32(Builtin.urem_Int32(lhs.value, rhs.value))
}
func [infix_left=200] % (lhs : Int64, rhs : Int64) -> Int64 {
  return Int64(Builtin.srem_Int64(lhs.value, rhs.value))
}
func [infix_left=200] % (lhs : UInt64, rhs : UInt64) -> UInt64 {
  return UInt64(Builtin.urem_Int64(lhs.value, rhs.value))
}
func [infix_left=200] % (lhs : Int128, rhs : Int128) -> Int128 {
  return Int128(Builtin.srem_Int128(lhs.value, rhs.value))
}
func [infix_left=200] % (lhs : UInt128, rhs : UInt128) -> UInt128 {
  return UInt128(Builtin.urem_Int128(lhs.value, rhs.value))
}


// Binary Addition.
func [infix_left=190] + (lhs: Int8, rhs: Int8) -> Int8 {
  return Int8(Builtin.add_Int8(lhs.value, rhs.value))
}
func [infix_left=190] + (lhs: UInt8, rhs: UInt8) -> UInt8 {
  return UInt8(Builtin.add_Int8(lhs.value, rhs.value))
}
func [infix_left=190] + (lhs: Int16, rhs: Int16) -> Int16 {
  return Int16(Builtin.add_Int16(lhs.value, rhs.value))
}
func [infix_left=190] + (lhs: UInt16, rhs: UInt16) -> UInt16 {
  return UInt16(Builtin.add_Int16(lhs.value, rhs.value))
}
func [infix_left=190] + (lhs: Int32, rhs: Int32) -> Int32 {
  return Int32(Builtin.add_Int32(lhs.value, rhs.value))
}
func [infix_left=190] + (lhs: UInt32, rhs: UInt32) -> UInt32 {
  return UInt32(Builtin.add_Int32(lhs.value, rhs.value))
}
func [infix_left=190] + (lhs: Int64, rhs: Int64) -> Int64 {
  return Int64(Builtin.add_Int64(lhs.value, rhs.value))
}
func [infix_left=190] + (lhs: UInt64, rhs: UInt64) -> UInt64 {
  return UInt64(Builtin.add_Int64(lhs.value, rhs.value))
}
func [infix_left=190] + (lhs: Int128, rhs: Int128) -> Int128 {
  return Int128(Builtin.add_Int128(lhs.value, rhs.value))
}
func [infix_left=190] + (lhs: UInt128, rhs: UInt128) -> UInt128 {
  return UInt128(Builtin.add_Int128(lhs.value, rhs.value))
}


// Binary Subtraction.
func [infix_left=190] - (lhs: Int8, rhs: Int8) -> Int8 {
  return Int8(Builtin.sub_Int8(lhs.value, rhs.value))
}
func [infix_left=190] - (lhs: UInt8, rhs: UInt8) -> UInt8 {
  return UInt8(Builtin.sub_Int8(lhs.value, rhs.value))
}
func [infix_left=190] - (lhs: Int16, rhs: Int16) -> Int16 {
  return Int16(Builtin.sub_Int16(lhs.value, rhs.value))
}
func [infix_left=190] - (lhs: UInt16, rhs: UInt16) -> UInt16 {
  return UInt16(Builtin.sub_Int16(lhs.value, rhs.value))
}
func [infix_left=190] - (lhs: Int32, rhs: Int32) -> Int32 {
  return Int32(Builtin.sub_Int32(lhs.value, rhs.value))
}
func [infix_left=190] - (lhs: UInt32, rhs: UInt32) -> UInt32 {
  return UInt32(Builtin.sub_Int32(lhs.value, rhs.value))
}
func [infix_left=190] - (lhs: Int64, rhs: Int64) -> Int64 {
  return Int64(Builtin.sub_Int64(lhs.value, rhs.value))
}
func [infix_left=190] - (lhs: UInt64, rhs: UInt64) -> UInt64 {
  return UInt64(Builtin.sub_Int64(lhs.value, rhs.value))
}
func [infix_left=190] - (lhs: Int128, rhs: Int128) -> Int128 {
  return Int128(Builtin.sub_Int128(lhs.value, rhs.value))
}
func [infix_left=190] - (lhs: UInt128, rhs: UInt128) -> UInt128 {
  return UInt128(Builtin.sub_Int128(lhs.value, rhs.value))
}

// Left Shift.
func [infix=180] << (lhs : Int8, rhs : Int8) -> Int8 {
  return Int8(Builtin.shl_Int8(lhs.value, rhs.value))
}
func [infix=180] << (lhs : UInt8, rhs : UInt8) -> UInt8 {
  return UInt8(Builtin.shl_Int8(lhs.value, rhs.value))
}
func [infix=180] << (lhs : Int16, rhs : Int16) -> Int16 {
  return Int16(Builtin.shl_Int16(lhs.value, rhs.value))
}
func [infix=180] << (lhs : UInt16, rhs : UInt16) -> UInt16 {
  return UInt16(Builtin.shl_Int16(lhs.value, rhs.value))
}
func [infix=180] << (lhs : Int32, rhs : Int32) -> Int32 {
  return Int32(Builtin.shl_Int32(lhs.value, rhs.value))
}
func [infix=180] << (lhs : UInt32, rhs : UInt32) -> UInt32 {
  return UInt32(Builtin.shl_Int32(lhs.value, rhs.value))
}
func [infix=180] << (lhs : Int64, rhs : Int64) -> Int64 {
  return Int64(Builtin.shl_Int64(lhs.value, rhs.value))
}
func [infix=180] << (lhs : UInt64, rhs : UInt64) -> UInt64 {
  return UInt64(Builtin.shl_Int64(lhs.value, rhs.value))
}
func [infix=180] << (lhs : Int128, rhs : Int128) -> Int128 {
  return Int128(Builtin.shl_Int128(lhs.value, rhs.value))
}
func [infix=180] << (lhs : UInt128, rhs : UInt128) -> UInt128 {
  return UInt128(Builtin.shl_Int128(lhs.value, rhs.value))
}

// Right Shift.
func [infix=180] >>(lhs : Int8, rhs : Int8) -> Int8 {
  return Int8(Builtin.ashr_Int8(lhs.value, rhs.value))
}
func [infix=180] >>(lhs : UInt8, rhs : UInt8) -> UInt8 {
  return UInt8(Builtin.lshr_Int8(lhs.value, rhs.value))
}
func [infix=180] >>(lhs : Int16, rhs : Int16) -> Int16 {
  return Int16(Builtin.ashr_Int16(lhs.value, rhs.value))
}
func [infix=180] >>(lhs : UInt16, rhs : UInt16) -> UInt16 {
  return UInt16(Builtin.lshr_Int16(lhs.value, rhs.value))
}
func [infix=180] >>(lhs : Int32, rhs : Int32) -> Int32 {
  return Int32(Builtin.ashr_Int32(lhs.value, rhs.value))
}
func [infix=180] >>(lhs : UInt32, rhs : UInt32) -> UInt32 {
  return UInt32(Builtin.lshr_Int32(lhs.value, rhs.value))
}
func [infix=180] >>(lhs : Int64, rhs : Int64) -> Int64 {
  return Int64(Builtin.ashr_Int64(lhs.value, rhs.value))
}
func [infix=180] >>(lhs : UInt64, rhs : UInt64) -> UInt64 {
  return UInt64(Builtin.lshr_Int64(lhs.value, rhs.value))
}
func [infix=180] >>(lhs : Int128, rhs : Int128) -> Int128 {
  return Int128(Builtin.ashr_Int128(lhs.value, rhs.value))
}
func [infix=180] >>(lhs : UInt128, rhs : UInt128) -> UInt128 {
  return UInt128(Builtin.lshr_Int128(lhs.value, rhs.value))
}

// Less-Than Comparison.
func [infix=170] < (lhs : Int8, rhs : Int8) -> Bool {
  return _getBool(Builtin.cmp_slt_Int8(lhs.value, rhs.value))
}
func [infix=170] < (lhs : UInt8, rhs : UInt8) -> Bool {
  return _getBool(Builtin.cmp_ult_Int8(lhs.value, rhs.value))
}
func [infix=170] < (lhs : Int16, rhs : Int16) -> Bool {
  return _getBool(Builtin.cmp_slt_Int16(lhs.value, rhs.value))
}
func [infix=170] < (lhs : UInt16, rhs : UInt16) -> Bool {
  return _getBool(Builtin.cmp_ult_Int16(lhs.value, rhs.value))
}
func [infix=170] < (lhs : Int32, rhs : Int32) -> Bool {
  return _getBool(Builtin.cmp_slt_Int32(lhs.value, rhs.value))
}
func [infix=170] < (lhs : UInt32, rhs : UInt32) -> Bool {
  return _getBool(Builtin.cmp_ult_Int32(lhs.value, rhs.value))
}
func [infix=170] < (lhs : Int64, rhs : Int64) -> Bool {
  return _getBool(Builtin.cmp_slt_Int64(lhs.value, rhs.value))
}
func [infix=170] < (lhs : UInt64, rhs : UInt64) -> Bool {
  return _getBool(Builtin.cmp_ult_Int64(lhs.value, rhs.value))
}
func [infix=170] < (lhs : Int128, rhs : Int128) -> Bool {
  return _getBool(Builtin.cmp_slt_Int128(lhs.value, rhs.value))
}
func [infix=170] < (lhs : UInt128, rhs : UInt128) -> Bool {
  return _getBool(Builtin.cmp_ult_Int128(lhs.value, rhs.value))
}

// Greater-Than Comparison.
func [infix=170] > (lhs : Int8, rhs : Int8) -> Bool {
  return _getBool(Builtin.cmp_sgt_Int8(lhs.value, rhs.value))
}
func [infix=170] > (lhs : UInt8, rhs : UInt8) -> Bool {
  return _getBool(Builtin.cmp_ugt_Int8(lhs.value, rhs.value))
}
func [infix=170] > (lhs : Int16, rhs : Int16) -> Bool {
  return _getBool(Builtin.cmp_sgt_Int16(lhs.value, rhs.value))
}
func [infix=170] > (lhs : UInt16, rhs : UInt16) -> Bool {
  return _getBool(Builtin.cmp_ugt_Int16(lhs.value, rhs.value))
}
func [infix=170] > (lhs : Int32, rhs : Int32) -> Bool {
  return _getBool(Builtin.cmp_sgt_Int32(lhs.value, rhs.value))
}
func [infix=170] > (lhs : UInt32, rhs : UInt32) -> Bool {
  return _getBool(Builtin.cmp_ugt_Int32(lhs.value, rhs.value))
}
func [infix=170] > (lhs : Int64, rhs : Int64) -> Bool {
  return _getBool(Builtin.cmp_sgt_Int64(lhs.value, rhs.value))
}
func [infix=170] > (lhs : UInt64, rhs : UInt64) -> Bool {
  return _getBool(Builtin.cmp_ugt_Int64(lhs.value, rhs.value))
}
func [infix=170] > (lhs : Int128, rhs : Int128) -> Bool {
  return _getBool(Builtin.cmp_sgt_Int128(lhs.value, rhs.value))
}
func [infix=170] > (lhs : UInt128, rhs : UInt128) -> Bool {
  return _getBool(Builtin.cmp_ugt_Int128(lhs.value, rhs.value))
}

// Less-Than-Or-Equal Comparison.
func [infix=170] <= (lhs : Int8, rhs : Int8) -> Bool {
  return _getBool(Builtin.cmp_sle_Int8(lhs.value, rhs.value))
}
func [infix=170] <= (lhs : UInt8, rhs : UInt8) -> Bool {
  return _getBool(Builtin.cmp_ule_Int8(lhs.value, rhs.value))
}
func [infix=170] <= (lhs : Int16, rhs : Int16) -> Bool {
  return _getBool(Builtin.cmp_sle_Int16(lhs.value, rhs.value))
}
func [infix=170] <= (lhs : UInt16, rhs : UInt16) -> Bool {
  return _getBool(Builtin.cmp_ule_Int16(lhs.value, rhs.value))
}
func [infix=170] <= (lhs : Int32, rhs : Int32) -> Bool {
  return _getBool(Builtin.cmp_sle_Int32(lhs.value, rhs.value))
}
func [infix=170] <= (lhs : UInt32, rhs : UInt32) -> Bool {
  return _getBool(Builtin.cmp_ule_Int32(lhs.value, rhs.value))
}
func [infix=170] <= (lhs : Int64, rhs : Int64) -> Bool {
  return _getBool(Builtin.cmp_sle_Int64(lhs.value, rhs.value))
}
func [infix=170] <= (lhs : UInt64, rhs : UInt64) -> Bool {
  return _getBool(Builtin.cmp_ule_Int64(lhs.value, rhs.value))
}
func [infix=170] <= (lhs : Int128, rhs : Int128) -> Bool {
  return _getBool(Builtin.cmp_sle_Int128(lhs.value, rhs.value))
}
func [infix=170] <= (lhs : UInt128, rhs : UInt128) -> Bool {
  return _getBool(Builtin.cmp_ule_Int128(lhs.value, rhs.value))
}

// Greater-Than-Or-Equal Comparison.
func [infix=170] >= (lhs : Int8, rhs : Int8) -> Bool {
  return _getBool(Builtin.cmp_sge_Int8(lhs.value, rhs.value))
}
func [infix=170] >= (lhs : UInt8, rhs : UInt8) -> Bool {
  return _getBool(Builtin.cmp_uge_Int8(lhs.value, rhs.value))
}
func [infix=170] >= (lhs : Int16, rhs : Int16) -> Bool {
  return _getBool(Builtin.cmp_sge_Int16(lhs.value, rhs.value))
}
func [infix=170] >= (lhs : UInt16, rhs : UInt16) -> Bool {
  return _getBool(Builtin.cmp_uge_Int16(lhs.value, rhs.value))
}
func [infix=170] >= (lhs : Int32, rhs : Int32) -> Bool {
  return _getBool(Builtin.cmp_sge_Int32(lhs.value, rhs.value))
}
func [infix=170] >= (lhs : UInt32, rhs : UInt32) -> Bool {
  return _getBool(Builtin.cmp_uge_Int32(lhs.value, rhs.value))
}
func [infix=170] >= (lhs : Int64, rhs : Int64) -> Bool {
  return _getBool(Builtin.cmp_sge_Int64(lhs.value, rhs.value))
}
func [infix=170] >= (lhs : UInt64, rhs : UInt64) -> Bool {
  return _getBool(Builtin.cmp_uge_Int64(lhs.value, rhs.value))
}
func [infix=170] >= (lhs : Int128, rhs : Int128) -> Bool {
  return _getBool(Builtin.cmp_sge_Int128(lhs.value, rhs.value))
}
func [infix=170] >= (lhs : UInt128, rhs : UInt128) -> Bool {
  return _getBool(Builtin.cmp_uge_Int128(lhs.value, rhs.value))
}

// Equality Comparison.
func [infix=160] == (lhs : Int8, rhs : Int8) -> Bool {
  return _getBool(Builtin.cmp_eq_Int8(lhs.value, rhs.value))
}
func [infix=160] == (lhs : UInt8, rhs : UInt8) -> Bool {
  return _getBool(Builtin.cmp_eq_Int8(lhs.value, rhs.value))
}
func [infix=160] == (lhs : Int16, rhs : Int16) -> Bool {
  return _getBool(Builtin.cmp_eq_Int16(lhs.value, rhs.value))
}
func [infix=160] == (lhs : UInt16, rhs : UInt16) -> Bool {
  return _getBool(Builtin.cmp_eq_Int16(lhs.value, rhs.value))
}
func [infix=160] == (lhs : Int32, rhs : Int32) -> Bool {
  return _getBool(Builtin.cmp_eq_Int32(lhs.value, rhs.value))
}
func [infix=160] == (lhs : UInt32, rhs : UInt32) -> Bool {
  return _getBool(Builtin.cmp_eq_Int32(lhs.value, rhs.value))
}
func [infix=160] == (lhs : Int64, rhs : Int64) -> Bool {
  return _getBool(Builtin.cmp_eq_Int64(lhs.value, rhs.value))
}
func [infix=160] == (lhs : UInt64, rhs : UInt64) -> Bool {
  return _getBool(Builtin.cmp_eq_Int64(lhs.value, rhs.value))
}
func [infix=160] == (lhs : Int128, rhs : Int128) -> Bool {
  return _getBool(Builtin.cmp_eq_Int128(lhs.value, rhs.value))
}
func [infix=160] == (lhs : UInt128, rhs : UInt128) -> Bool {
  return _getBool(Builtin.cmp_eq_Int128(lhs.value, rhs.value))
}

// Not-Equality Comparison.
func [infix=160] != (lhs : Int8, rhs : Int8) -> Bool {
  return _getBool(Builtin.cmp_ne_Int8(lhs.value, rhs.value))
}
func [infix=160] != (lhs : UInt8, rhs : UInt8) -> Bool {
  return _getBool(Builtin.cmp_ne_Int8(lhs.value, rhs.value))
}
func [infix=160] != (lhs : Int16, rhs : Int16) -> Bool {
  return _getBool(Builtin.cmp_ne_Int16(lhs.value, rhs.value))
}
func [infix=160] != (lhs : UInt16, rhs : UInt16) -> Bool {
  return _getBool(Builtin.cmp_ne_Int16(lhs.value, rhs.value))
}
func [infix=160] != (lhs : Int32, rhs : Int32) -> Bool {
  return _getBool(Builtin.cmp_ne_Int32(lhs.value, rhs.value))
}
func [infix=160] != (lhs : UInt32, rhs : UInt32) -> Bool {
  return _getBool(Builtin.cmp_ne_Int32(lhs.value, rhs.value))
}
func [infix=160] != (lhs : Int64, rhs : Int64) -> Bool {
  return _getBool(Builtin.cmp_ne_Int64(lhs.value, rhs.value))
}
func [infix=160] != (lhs : UInt64, rhs : UInt64) -> Bool {
  return _getBool(Builtin.cmp_ne_Int64(lhs.value, rhs.value))
}
func [infix=160] != (lhs : Int128, rhs : Int128) -> Bool {
  return _getBool(Builtin.cmp_ne_Int128(lhs.value, rhs.value))
}
func [infix=160] != (lhs : UInt128, rhs : UInt128) -> Bool {
  return _getBool(Builtin.cmp_ne_Int128(lhs.value, rhs.value))
}

// Bitwise 'and'.
func [infix_left=150] & (lhs: Int8, rhs: Int8) -> Int8 {
  return Int8(Builtin.and_Int8(lhs.value, rhs.value))
}
func [infix_left=150] & (lhs: UInt8, rhs: UInt8) -> UInt8 {
  return UInt8(Builtin.and_Int8(lhs.value, rhs.value))
}
func [infix_left=150] & (lhs: Int16, rhs: Int16) -> Int16 {
  return Int16(Builtin.and_Int16(lhs.value, rhs.value))
}
func [infix_left=150] & (lhs: UInt16, rhs: UInt16) -> UInt16 {
  return UInt16(Builtin.and_Int16(lhs.value, rhs.value))
}
func [infix_left=150] & (lhs: Int32, rhs: Int32) -> Int32 {
  return Int32(Builtin.and_Int32(lhs.value, rhs.value))
}
func [infix_left=150] & (lhs: UInt32, rhs: UInt32) -> UInt32 {
  return UInt32(Builtin.and_Int32(lhs.value, rhs.value))
}
func [infix_left=150] & (lhs: Int64, rhs: Int64) -> Int64 {
  return Int64(Builtin.and_Int64(lhs.value, rhs.value))
}
func [infix_left=150] & (lhs: UInt64, rhs: UInt64) -> UInt64 {
  return UInt64(Builtin.and_Int64(lhs.value, rhs.value))
}
func [infix_left=150] & (lhs: Int128, rhs: Int128) -> Int128 {
  return Int128(Builtin.and_Int128(lhs.value, rhs.value))
}
func [infix_left=150] & (lhs: UInt128, rhs: UInt128) -> UInt128 {
  return UInt128(Builtin.and_Int128(lhs.value, rhs.value))
}

func [infix_left=140] ^ (lhs: Int8, rhs: Int8) -> Int8 {
  return Int8(Builtin.xor_Int8(lhs.value, rhs.value))
}
func [infix_left=140] ^ (lhs: UInt8, rhs: UInt8) -> UInt8 {
  return UInt8(Builtin.xor_Int8(lhs.value, rhs.value))
}
func [infix_left=140] ^ (lhs: Int16, rhs: Int16) -> Int16 {
  return Int16(Builtin.xor_Int16(lhs.value, rhs.value))
}
func [infix_left=140] ^ (lhs: UInt16, rhs: UInt16) -> UInt16 {
  return UInt16(Builtin.xor_Int16(lhs.value, rhs.value))
}
func [infix_left=140] ^ (lhs: Int32, rhs: Int32) -> Int32 {
  return Int32(Builtin.xor_Int32(lhs.value, rhs.value))
}
func [infix_left=140] ^ (lhs: UInt32, rhs: UInt32) -> UInt32 {
  return UInt32(Builtin.xor_Int32(lhs.value, rhs.value))
}
func [infix_left=140] ^ (lhs: Int64, rhs: Int64) -> Int64 {
  return Int64(Builtin.xor_Int64(lhs.value, rhs.value))
}
func [infix_left=140] ^ (lhs: UInt64, rhs: UInt64) -> UInt64 {
  return UInt64(Builtin.xor_Int64(lhs.value, rhs.value))
}
func [infix_left=140] ^ (lhs: Int128, rhs: Int128) -> Int128 {
  return Int128(Builtin.xor_Int128(lhs.value, rhs.value))
}
func [infix_left=140] ^ (lhs: UInt128, rhs: UInt128) -> UInt128 {
  return UInt128(Builtin.xor_Int128(lhs.value, rhs.value))
}

func [infix_left=130] | (lhs: Int8, rhs: Int8) -> Int8 {
  return Int8(Builtin.or_Int8(lhs.value, rhs.value))
}
func [infix_left=130] | (lhs: UInt8, rhs: UInt8) -> UInt8 {
  return UInt8(Builtin.or_Int8(lhs.value, rhs.value))
}
func [infix_left=130] | (lhs: Int16, rhs: Int16) -> Int16 {
  return Int16(Builtin.or_Int16(lhs.value, rhs.value))
}
func [infix_left=130] | (lhs: UInt16, rhs: UInt16) -> UInt16 {
  return UInt16(Builtin.or_Int16(lhs.value, rhs.value))
}
func [infix_left=130] | (lhs: Int32, rhs: Int32) -> Int32 {
  return Int32(Builtin.or_Int32(lhs.value, rhs.value))
}
func [infix_left=130] | (lhs: UInt32, rhs: UInt32) -> UInt32 {
  return UInt32(Builtin.or_Int32(lhs.value, rhs.value))
}
func [infix_left=130] | (lhs: Int64, rhs: Int64) -> Int64 {
  return Int64(Builtin.or_Int64(lhs.value, rhs.value))
}
func [infix_left=130] | (lhs: UInt64, rhs: UInt64) -> UInt64 {
  return UInt64(Builtin.or_Int64(lhs.value, rhs.value))
}
func [infix_left=130] | (lhs: Int128, rhs: Int128) -> Int128 {
  return Int128(Builtin.or_Int128(lhs.value, rhs.value))
}
func [infix_left=130] | (lhs: UInt128, rhs: UInt128) -> UInt128 {
  return UInt128(Builtin.or_Int128(lhs.value, rhs.value))
}

// See Bool.swift for && and ||
// In C, 120 is &&
// In C, 110 is ||

// In C, 100 is ?:
// In C, 90 is =, *=, += etc.

// Compound assignment (with addition)
func [assignment,infix_left=90] += (lhs : [byref] Int8, rhs : Int8) {
  lhs = lhs + rhs
}
func [assignment,infix_left=90] += (lhs : [byref] UInt8, rhs : UInt8) {
  lhs = lhs + rhs
}
func [assignment,infix_left=90] += (lhs : [byref] Int16, rhs : Int16) {
  lhs = lhs + rhs
}
func [assignment,infix_left=90] += (lhs : [byref] UInt16, rhs : UInt16) {
  lhs = lhs + rhs
}
func [assignment,infix_left=90] += (lhs : [byref] Int32, rhs : Int32) {
  lhs = lhs + rhs
}
func [assignment,infix_left=90] += (lhs : [byref] UInt32, rhs : UInt32) {
  lhs = lhs + rhs
}
func [assignment,infix_left=90] += (lhs : [byref] Int64, rhs : Int64) {
  lhs = lhs + rhs
}
func [assignment,infix_left=90] += (lhs : [byref] UInt64, rhs : UInt64) {
  lhs = lhs + rhs
}
func [assignment,infix_left=90] += (lhs : [byref] Int128, rhs : Int128) {
  lhs = lhs + rhs
}
func [assignment,infix_left=90] += (lhs : [byref] UInt128, rhs : UInt128) {
  lhs = lhs + rhs
}

// Compound assignment (with subtraction)
func [assignment,infix_left=90] -= (lhs : [byref] Int8, rhs : Int8) {
  lhs = lhs - rhs
}
func [assignment,infix_left=90] -= (lhs : [byref] UInt8, rhs : UInt8) {
  lhs = lhs - rhs
}
func [assignment,infix_left=90] -= (lhs : [byref] Int16, rhs : Int16) {
  lhs = lhs - rhs
}
func [assignment,infix_left=90] -= (lhs : [byref] UInt16, rhs : UInt16) {
  lhs = lhs - rhs
}
func [assignment,infix_left=90] -= (lhs : [byref] Int32, rhs : Int32) {
  lhs = lhs - rhs
}
func [assignment,infix_left=90] -= (lhs : [byref] UInt32, rhs : UInt32) {
  lhs = lhs - rhs
}
func [assignment,infix_left=90] -= (lhs : [byref] Int64, rhs : Int64) {
  lhs = lhs - rhs
}
func [assignment,infix_left=90] -= (lhs : [byref] UInt64, rhs : UInt64) {
  lhs = lhs - rhs
}
func [assignment,infix_left=90] -= (lhs : [byref] Int128, rhs : Int128) {
  lhs = lhs - rhs
}
func [assignment,infix_left=90] -= (lhs : [byref] UInt128, rhs : UInt128) {
  lhs = lhs - rhs
}

// Compound assignment (with multiplication)
func [assignment,infix_left=90] *= (lhs : [byref] Int8, rhs : Int8) {
  lhs = lhs * rhs
}
func [assignment,infix_left=90] *= (lhs : [byref] UInt8, rhs : UInt8) {
  lhs = lhs * rhs
}
func [assignment,infix_left=90] *= (lhs : [byref] Int16, rhs : Int16) {
  lhs = lhs * rhs
}
func [assignment,infix_left=90] *= (lhs : [byref] UInt16, rhs : UInt16) {
  lhs = lhs * rhs
}
func [assignment,infix_left=90] *= (lhs : [byref] Int32, rhs : Int32) {
  lhs = lhs * rhs
}
func [assignment,infix_left=90] *= (lhs : [byref] UInt32, rhs : UInt32) {
  lhs = lhs * rhs
}
func [assignment,infix_left=90] *= (lhs : [byref] Int64, rhs : Int64) {
  lhs = lhs * rhs
}
func [assignment,infix_left=90] *= (lhs : [byref] UInt64, rhs : UInt64) {
  lhs = lhs * rhs
}
func [assignment,infix_left=90] *= (lhs : [byref] Int128, rhs : Int128) {
  lhs = lhs * rhs
}
func [assignment,infix_left=90] *= (lhs : [byref] UInt128, rhs : UInt128) {
  lhs = lhs * rhs
}

// Compound assignment (with division)
func [assignment,infix_left=90] /= (lhs : [byref] Int8, rhs : Int8) {
  lhs = lhs / rhs
}
func [assignment,infix_left=90] /= (lhs : [byref] UInt8, rhs : UInt8) {
  lhs = lhs / rhs
}
func [assignment,infix_left=90] /= (lhs : [byref] Int16, rhs : Int16) {
  lhs = lhs / rhs
}
func [assignment,infix_left=90] /= (lhs : [byref] UInt16, rhs : UInt16) {
  lhs = lhs / rhs
}
func [assignment,infix_left=90] /= (lhs : [byref] Int32, rhs : Int32) {
  lhs = lhs / rhs
}
func [assignment,infix_left=90] /= (lhs : [byref] UInt32, rhs : UInt32) {
  lhs = lhs / rhs
}
func [assignment,infix_left=90] /= (lhs : [byref] Int64, rhs : Int64) {
  lhs = lhs / rhs
}
func [assignment,infix_left=90] /= (lhs : [byref] UInt64, rhs : UInt64) {
  lhs = lhs / rhs
}
func [assignment,infix_left=90] /= (lhs : [byref] Int128, rhs : Int128) {
  lhs = lhs / rhs
}
func [assignment,infix_left=90] /= (lhs : [byref] UInt128, rhs : UInt128) {
  lhs = lhs / rhs
}

// Compound assignment (with remainder)
func [assignment,infix_left=90] %= (lhs : [byref] Int8, rhs : Int8) {
  lhs = lhs % rhs
}
func [assignment,infix_left=90] %= (lhs : [byref] UInt8, rhs : UInt8) {
  lhs = lhs % rhs
}
func [assignment,infix_left=90] %= (lhs : [byref] Int16, rhs : Int16) {
  lhs = lhs % rhs
}
func [assignment,infix_left=90] %= (lhs : [byref] UInt16, rhs : UInt16) {
  lhs = lhs % rhs
}
func [assignment,infix_left=90] %= (lhs : [byref] Int32, rhs : Int32) {
  lhs = lhs % rhs
}
func [assignment,infix_left=90] %= (lhs : [byref] UInt32, rhs : UInt32) {
  lhs = lhs % rhs
}
func [assignment,infix_left=90] %= (lhs : [byref] Int64, rhs : Int64) {
  lhs = lhs % rhs
}
func [assignment,infix_left=90] %= (lhs : [byref] UInt64, rhs : UInt64) {
  lhs = lhs % rhs
}
func [assignment,infix_left=90] %= (lhs : [byref] Int128, rhs : Int128) {
  lhs = lhs % rhs
}
func [assignment,infix_left=90] %= (lhs : [byref] UInt128, rhs : UInt128) {
  lhs = lhs % rhs
}

// Compound assignment (with left shift)
func [assignment,infix_left=90] <<= (lhs : [byref] Int8, rhs : Int8) {
  lhs = lhs << rhs
}
func [assignment,infix_left=90] <<= (lhs : [byref] UInt8, rhs : UInt8) {
  lhs = lhs << rhs
}
func [assignment,infix_left=90] <<= (lhs : [byref] Int16, rhs : Int16) {
  lhs = lhs << rhs
}
func [assignment,infix_left=90] <<= (lhs : [byref] UInt16, rhs : UInt16) {
  lhs = lhs << rhs
}
func [assignment,infix_left=90] <<= (lhs : [byref] Int32, rhs : Int32) {
  lhs = lhs << rhs
}
func [assignment,infix_left=90] <<= (lhs : [byref] UInt32, rhs : UInt32) {
  lhs = lhs << rhs
}
func [assignment,infix_left=90] <<= (lhs : [byref] Int64, rhs : Int64) {
  lhs = lhs << rhs
}
func [assignment,infix_left=90] <<= (lhs : [byref] UInt64, rhs : UInt64) {
  lhs = lhs << rhs
}
func [assignment,infix_left=90] <<= (lhs : [byref] Int128, rhs : Int128) {
  lhs = lhs << rhs
}
func [assignment,infix_left=90] <<= (lhs : [byref] UInt128, rhs : UInt128) {
  lhs = lhs << rhs
}

// Compound assignment (with right shift)
func [assignment,infix_left=90] >>= (lhs : [byref] Int8, rhs : Int8) {
  lhs = lhs >> rhs
}
func [assignment,infix_left=90] >>= (lhs : [byref] UInt8, rhs : UInt8) {
  lhs = lhs >> rhs
}
func [assignment,infix_left=90] >>= (lhs : [byref] Int16, rhs : Int16) {
  lhs = lhs >> rhs
}
func [assignment,infix_left=90] >>= (lhs : [byref] UInt16, rhs : UInt16) {
  lhs = lhs >> rhs
}
func [assignment,infix_left=90] >>= (lhs : [byref] Int32, rhs : Int32) {
  lhs = lhs >> rhs
}
func [assignment,infix_left=90] >>= (lhs : [byref] UInt32, rhs : UInt32) {
  lhs = lhs >> rhs
}
func [assignment,infix_left=90] >>= (lhs : [byref] Int64, rhs : Int64) {
  lhs = lhs >> rhs
}
func [assignment,infix_left=90] >>= (lhs : [byref] UInt64, rhs : UInt64) {
  lhs = lhs >> rhs
}
func [assignment,infix_left=90] >>= (lhs : [byref] Int128, rhs : Int128) {
  lhs = lhs >> rhs
}
func [assignment,infix_left=90] >>= (lhs : [byref] UInt128, rhs : UInt128) {
  lhs = lhs >> rhs
}

// Compound assignment (with bitwise and)
func [assignment,infix_left=90] &= (lhs : [byref] Int8, rhs : Int8) {
  lhs = lhs & rhs
}
func [assignment,infix_left=90] &= (lhs : [byref] UInt8, rhs : UInt8) {
  lhs = lhs & rhs
}
func [assignment,infix_left=90] &= (lhs : [byref] Int16, rhs : Int16) {
  lhs = lhs & rhs
}
func [assignment,infix_left=90] &= (lhs : [byref] UInt16, rhs : UInt16) {
  lhs = lhs & rhs
}
func [assignment,infix_left=90] &= (lhs : [byref] Int32, rhs : Int32) {
  lhs = lhs & rhs
}
func [assignment,infix_left=90] &= (lhs : [byref] UInt32, rhs : UInt32) {
  lhs = lhs & rhs
}
func [assignment,infix_left=90] &= (lhs : [byref] Int64, rhs : Int64) {
  lhs = lhs & rhs
}
func [assignment,infix_left=90] &= (lhs : [byref] UInt64, rhs : UInt64) {
  lhs = lhs & rhs
}
func [assignment,infix_left=90] &= (lhs : [byref] Int128, rhs : Int128) {
  lhs = lhs & rhs
}
func [assignment,infix_left=90] &= (lhs : [byref] UInt128, rhs : UInt128) {
  lhs = lhs & rhs
}

// Compound assignment (with bitwise or)
func [assignment,infix_left=90] |= (lhs : [byref] Int8, rhs : Int8) {
  lhs = lhs | rhs
}
func [assignment,infix_left=90] |= (lhs : [byref] UInt8, rhs : UInt8) {
  lhs = lhs | rhs
}
func [assignment,infix_left=90] |= (lhs : [byref] Int16, rhs : Int16) {
  lhs = lhs | rhs
}
func [assignment,infix_left=90] |= (lhs : [byref] UInt16, rhs : UInt16) {
  lhs = lhs | rhs
}
func [assignment,infix_left=90] |= (lhs : [byref] Int32, rhs : Int32) {
  lhs = lhs | rhs
}
func [assignment,infix_left=90] |= (lhs : [byref] UInt32, rhs : UInt32) {
  lhs = lhs | rhs
}
func [assignment,infix_left=90] |= (lhs : [byref] Int64, rhs : Int64) {
  lhs = lhs | rhs
}
func [assignment,infix_left=90] |= (lhs : [byref] UInt64, rhs : UInt64) {
  lhs = lhs | rhs
}
func [assignment,infix_left=90] |= (lhs : [byref] Int128, rhs : Int128) {
  lhs = lhs | rhs
}
func [assignment,infix_left=90] |= (lhs : [byref] UInt128, rhs : UInt128) {
  lhs = lhs | rhs
}

// Compound assignment (with bitwise exclusive or)
func [assignment,infix_left=90] ^= (lhs : [byref] Int8, rhs : Int8) {
  lhs = lhs ^ rhs
}
func [assignment,infix_left=90] ^= (lhs : [byref] UInt8, rhs : UInt8) {
  lhs = lhs ^ rhs
}
func [assignment,infix_left=90] ^= (lhs : [byref] Int16, rhs : Int16) {
  lhs = lhs ^ rhs
}
func [assignment,infix_left=90] ^= (lhs : [byref] UInt16, rhs : UInt16) {
  lhs = lhs ^ rhs
}
func [assignment,infix_left=90] ^= (lhs : [byref] Int32, rhs : Int32) {
  lhs = lhs ^ rhs
}
func [assignment,infix_left=90] ^= (lhs : [byref] UInt32, rhs : UInt32) {
  lhs = lhs ^ rhs
}
func [assignment,infix_left=90] ^= (lhs : [byref] Int64, rhs : Int64) {
  lhs = lhs ^ rhs
}
func [assignment,infix_left=90] ^= (lhs : [byref] UInt64, rhs : UInt64) {
  lhs = lhs ^ rhs
}
func [assignment,infix_left=90] ^= (lhs : [byref] Int128, rhs : Int128) {
  lhs = lhs ^ rhs
}
func [assignment,infix_left=90] ^= (lhs : [byref] UInt128, rhs : UInt128) {
  lhs = lhs ^ rhs
}
