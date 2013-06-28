struct Float64 {
  var value : Builtin.FPIEEE64

  func replPrint() {
    print(this)
  }
}

extension Float64 : BuiltinIntegerLiteralConvertible, IntegerLiteralConvertible {
  static func _convertFromBuiltinIntegerLiteral(val : Builtin.Int128) -> Float64 {
    return Float64(Builtin.uitofp_Int128_FPIEEE64(val))
  }

  typealias IntegerLiteralType = Int128
  static func convertFromIntegerLiteral(value : Int128) -> Float64 {
    return Float64(Builtin.uitofp_Int128_FPIEEE64(value.value))
  }
}

extension Float64 : BuiltinFloatLiteralConvertible {
  static func _convertFromBuiltinFloatLiteral(
                value : Builtin.FPIEEE64) -> Float64 {
    return Float64(value)
  }
}

extension Float64 : FloatLiteralConvertible {
  typealias FloatLiteralType = Float64
  static func convertFromFloatLiteral(value : Float64) -> Float64 {
    return value
  }
}

extension Float64 : Comparable, Hashable {
  func __equal__(rhs: Float64) -> Bool {
    return _getBool(Builtin.fcmp_oeq_FPIEEE64(value, rhs.value))
  }
  func __less__(rhs: Float64) -> Bool {
    return _getBool(Builtin.fcmp_olt_FPIEEE64(value, rhs.value))
  }
  func hashValue() -> Int {
    return Int(Builtin.bitcast_FPIEEE64_Int64(value))
  }
}

extension Float64 : SignedNumber {
  func __negate__() -> Float64 { return Float64(Builtin.fneg_FPIEEE64(value)) }
  func isNegative() -> Bool { return this < 0 }
}

//===----------------------------------------------------------------------===//
// Explicit conversions between types.
//===----------------------------------------------------------------------===//

extension Int8 {
  constructor(v : Float64) {
    value = Builtin.fptosi_FPIEEE64_Int8(v.value)
  }
}

extension UInt8 {
  constructor(v : Float64) {
    value = Builtin.fptoui_FPIEEE64_Int8(v.value)
  }
}

extension Int16 {
  constructor(v : Float64) {
    value = Builtin.fptosi_FPIEEE64_Int16(v.value)
  }
}

extension Int32 {
  constructor(v : Float64) {
    value = Builtin.fptosi_FPIEEE64_Int32(v.value)
  }
}

extension UInt32 {
  constructor(v : Float64) {
    value = Builtin.fptoui_FPIEEE64_Int32(v.value)
  }
}

extension Int64 {
  constructor(v : Float64) {
    value = Builtin.fptosi_FPIEEE64_Int64(v.value)
  }
}

extension UInt64 {
  constructor(v : Float64) {
    value = Builtin.fptosi_FPIEEE64_Int64(v.value)
  }
}

extension Int128 {
  constructor(v : Float64) {
    value = Builtin.fptosi_FPIEEE64_Int128(v.value)
  }
}

extension Float32 {
  constructor(v : Float64) {
    value = Builtin.fptrunc_FPIEEE64_FPIEEE32(v.value)
  }
}

extension Float64 {
  constructor(v : Int8) {
    value = Builtin.sitofp_Int8_FPIEEE64(v.value)
  }
  constructor(v : UInt8) {
    value = Builtin.uitofp_Int8_FPIEEE64(v.value)
  }
  constructor(v : Int16) {
    value = Builtin.sitofp_Int16_FPIEEE64(v.value)
  }
  constructor(v : UInt16) {
    value = Builtin.uitofp_Int16_FPIEEE64(v.value)
  }
  constructor(v : Int32) {
    value = Builtin.sitofp_Int32_FPIEEE64(v.value)
  }
  constructor(v : UInt32) {
    value = Builtin.uitofp_Int32_FPIEEE64(v.value)
  }
  constructor(v : UInt64) {
    value = Builtin.uitofp_Int64_FPIEEE64(v.value)
  }
  constructor(v : Int64) {
    value = Builtin.sitofp_Int64_FPIEEE64(v.value)
  }
  constructor(v : Int128) {
    value = Builtin.sitofp_Int128_FPIEEE64(v.value)
  }
  constructor(v : Float32) {
    value = Builtin.fpext_FPIEEE32_FPIEEE64(v.value)
  }
}

//===----------------------------------------------------------------------===//
// Standard Operators
//===----------------------------------------------------------------------===//

// Unary addition operators.
func [prefix] +(a : Float64) -> Float64 { return a }

// Binary Multiplication.
func * (lhs : Float64, rhs : Float64) -> Float64 {
  return Float64(Builtin.fmul_FPIEEE64(lhs.value, rhs.value))
}

// Binary Division.
func / (lhs: Float64, rhs: Float64) -> Float64 {
  return Float64(Builtin.fdiv_FPIEEE64(lhs.value, rhs.value))
}

// Binary Remainder.
// The sign of the result matches the sign of the dividend.
// 1) This is consistent with '%' in C#, D, Java, and JavaScript
// 2) C99 requires this behavior for fmod*()
// 3) C++11 requires this behavior for std::fmod*()
func [asmname="fmod"] % (lhs: Float64, rhs: Float64) -> Float64

// Binary Addition.
func + (lhs: Float64, rhs: Float64) -> Float64 {
  return Float64(Builtin.fadd_FPIEEE64(lhs.value, rhs.value))
}

// Binary Subtraction.
func - (lhs: Float64, rhs: Float64) -> Float64 {
  return Float64(Builtin.fsub_FPIEEE64(lhs.value, rhs.value))
}

// See Bool.swift for && and ||
// In C, 120 is &&
// In C, 110 is ||

// In C, 100 is ?:
// In C, 90 is =, *=, += etc.

// Compound assignment (with addition)
func [assignment] += (lhs : [byref] Float64, rhs : Float64) {
  lhs = lhs + rhs
}

// Compound assignment (with subtraction)
func [assignment] -= (lhs : [byref] Float64, rhs : Float64) {
  lhs = lhs - rhs
}

// Compound assignment (with multiplication)
func [assignment] *= (lhs : [byref] Float64, rhs : Float64) {
  lhs = lhs * rhs
}

// Compound assignment (with division)
func [assignment] /= (lhs : [byref] Float64, rhs : Float64) {
  lhs = lhs / rhs
}

// Compound assignment (with remainder)
func [assignment] %= (lhs : [byref] Float64, rhs : Float64) {
  lhs = lhs % rhs
}

// math
func [asmname="sin"]    sin(x : Float64) -> Float64
func [asmname="cos"]    cos(x : Float64) -> Float64
func [asmname="tan"]    tan(x : Float64) -> Float64
func [asmname="atan"]  atan(x : Float64) -> Float64
func [asmname="atan2"] atan(y : Float64, x : Float64) -> Float64
func [asmname="sqrt"]  sqrt(x : Float64) -> Float64
