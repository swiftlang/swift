struct Float32 {
  var value : Builtin.FPIEEE32

  func replPrint() {
    print(Float64(this))
  }
}

extension Float32 : BuiltinIntegerLiteralConvertible, IntegerLiteralConvertible {
  static func _convertFromBuiltinIntegerLiteral(val : Builtin.Int128) -> Float32 {
    return Float32(Builtin.uitofp_Int128_FPIEEE32(val))
  }

  typealias IntegerLiteralType = Int128
  static func convertFromIntegerLiteral(value : Int128) -> Float32 {
    return Float32(Builtin.uitofp_Int128_FPIEEE32(value.value))
  }
}

extension Float32 : BuiltinFloatLiteralConvertible {
  static func _convertFromBuiltinFloatLiteral(
                value : Builtin.FPIEEE64) -> Float32 {
    return Float32(Builtin.fptrunc_FPIEEE64_FPIEEE32(value))
  }
}

extension Float32 : FloatLiteralConvertible {
  typealias FloatLiteralType = Float32
  static func convertFromFloatLiteral(value : Float32) -> Float32 {
    return value
  }
}

extension Float32 : Comparable, Hashable {
  func __equal__(rhs: Float32) -> Bool {
    return _getBool(Builtin.fcmp_oeq_FPIEEE32(value, rhs.value))
  }
  func __less__(rhs: Float32) -> Bool {
    return _getBool(Builtin.fcmp_olt_FPIEEE32(value, rhs.value))
  }
  func hashValue() -> Int {
    return Int(Int32(Builtin.bitcast_FPIEEE32_Int32(value)))
  }
}

extension Float32 : SignedNumber {
  func __negate__() -> Float32 { return Float32(Builtin.fneg_FPIEEE32(value)) }
  func isNegative() -> Bool { return this < 0 }
}

//===----------------------------------------------------------------------===//
// Explicit conversions between types.
//===----------------------------------------------------------------------===//

extension Int8 {
  constructor(v : Float32) {
    value = Builtin.fptosi_FPIEEE32_Int8(v.value)
  }
}

extension UInt8 {
  constructor(v : Float32) {
    value = Builtin.fptoui_FPIEEE32_Int8(v.value)
  }
}

extension Int16 {
  constructor(v : Float32) {
    value = Builtin.fptosi_FPIEEE32_Int16(v.value)
  }
}

extension Int32 {
  constructor(v : Float32) {
    value = Builtin.fptosi_FPIEEE32_Int32(v.value)
  }
}

extension UInt32 {
  constructor(v : Float32) {
    value = Builtin.fptoui_FPIEEE32_Int32(v.value)
  }
}

extension Int64 {
  constructor(v : Float32) {
    value = Builtin.fptosi_FPIEEE32_Int64(v.value)
  }
}

extension UInt64 {
  constructor(v : Float32) {
    value = Builtin.fptosi_FPIEEE32_Int64(v.value)
  }
}

extension Int128 {
  constructor(v : Float32) {
    value = Builtin.fptosi_FPIEEE32_Int128(v.value)
  }
}

extension Float32 {
  constructor(v : Int8) {
    value = Builtin.sitofp_Int8_FPIEEE32(v.value)
  }
  constructor(v : UInt8) {
    value = Builtin.uitofp_Int8_FPIEEE32(v.value)
  }
  constructor(v : Int16) {
    value = Builtin.sitofp_Int16_FPIEEE32(v.value)
  }
  constructor(v : UInt16) {
    value = Builtin.uitofp_Int16_FPIEEE32(v.value)
  }
  constructor(v : Int32) {
    value = Builtin.sitofp_Int32_FPIEEE32(v.value)
  }
  constructor(v : UInt32) {
    value = Builtin.uitofp_Int32_FPIEEE32(v.value)
  }
  constructor(v : Int64) {
    value = Builtin.sitofp_Int64_FPIEEE32(v.value)
  }
  constructor(v : Int128) {
    value = Builtin.sitofp_Int128_FPIEEE32(v.value)
  }
}

//===----------------------------------------------------------------------===//
// Standard Operators
//===----------------------------------------------------------------------===//

// Unary addition operators.
func [prefix] +(a : Float32) -> Float32 { return a }

// Binary Multiplication.
func * (lhs : Float32, rhs : Float32) -> Float32 {
  return Float32(Builtin.fmul_FPIEEE32(lhs.value, rhs.value))
}

// Binary Division.
func / (lhs : Float32, rhs : Float32) -> Float32 {
  return Float32(Builtin.fdiv_FPIEEE32(lhs.value, rhs.value))
}

// Binary Remainder.
// The sign of the result matches the sign of the dividend.
// 1) This is consistent with '%' in C#, D, Java, and JavaScript
// 2) C99 requires this behavior for fmod*()
// 3) C++11 requires this behavior for std::fmod*()
func [asmname="fmodf"] % (lhs: Float32, rhs: Float32) -> Float32

// Binary Addition.
func + (lhs: Float32, rhs: Float32) -> Float32 {
  return Float32(Builtin.fadd_FPIEEE32(lhs.value, rhs.value))
}

// Binary Subtraction.
func - (lhs: Float32, rhs: Float32) -> Float32 {
  return Float32(Builtin.fsub_FPIEEE32(lhs.value, rhs.value))
}

// See Bool.swift for && and ||
// In C, 120 is &&
// In C, 110 is ||

// In C, 100 is ?:
// In C, 90 is =, *=, += etc.

// Compound assignment (with addition)
func [assignment] += (lhs : [byref] Float32, rhs : Float32) {
  lhs = lhs + rhs
}

// Compound assignment (with subtraction)
func [assignment] -= (lhs : [byref] Float32, rhs : Float32) {
  lhs = lhs - rhs
}

// Compound assignment (with multiplication)
func [assignment] *= (lhs : [byref] Float32, rhs : Float32) {
  lhs = lhs * rhs
}

// Compound assignment (with division)
func [assignment] /= (lhs : [byref] Float32, rhs : Float32) {
  lhs = lhs / rhs
}

// Compound assignment (with remainder)
func [assignment] %= (lhs : [byref] Float32, rhs : Float32) {
  lhs = lhs % rhs
}

// math
func [asmname="sinf"]    sin(x : Float32) -> Float32
func [asmname="cosf"]    cos(x : Float32) -> Float32
func [asmname="tanf"]    tan(x : Float32) -> Float32
func [asmname="atanf"]  atan(x : Float32) -> Float32
func [asmname="atan2f"] atan(y : Float32, x : Float32) -> Float32
func [asmname="sqrtf"]  sqrt(x : Float32) -> Float32
