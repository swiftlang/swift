struct Float80 {
  var value : Builtin.FPIEEE80

  static func convertFromIntegerLiteral(v : Builtin.Int128) -> Float80 {
  }

  func replPrint() {
    print(Float64(this))
  }
}

extension Float80 : BuiltinIntegerLiteralConvertible, IntegerLiteralConvertible {
  static func _convertFromBuiltinIntegerLiteral(val : Builtin.Int128) -> Float80 {
    return Float80(Builtin.uitofp_Int128_FPIEEE80(val))
  }

  typealias IntegerLiteralType = Int128
  static func convertFromIntegerLiteral(value : Int128) -> Float80 {
    return Float80(Builtin.uitofp_Int128_FPIEEE80(value.value))
  }
}

extension Float80 : BuiltinFloatLiteralConvertible {
  // FIXME: This is actually losing precision <rdar://problem/14073102>.
  static func _convertFromBuiltinFloatLiteral(
                value : Builtin.FPIEEE64) -> Float80 {
    return Float80(Builtin.fpext_FPIEEE64_FPIEEE80(value))
  }
}

extension Float80 : FloatLiteralConvertible {
  typealias FloatLiteralType = Float80
  static func convertFromFloatLiteral(value : Float80) -> Float80 {
    return value
  }
}

extension Float80 : Comparable, Hashable {
  func __equal__(rhs: Float80) -> Bool {
    return _getBool(Builtin.fcmp_oeq_FPIEEE80(value, rhs.value))
  }
  func __less__(rhs: Float80) -> Bool {
    return _getBool(Builtin.fcmp_olt_FPIEEE80(value, rhs.value))
  }
  func hashValue() -> Int {
    var asInt = Int128(Builtin.zext_Int80_Int128(
                    Builtin.bitcast_FPIEEE80_Int80(value)))
    return asInt.hashValue()
  }
}

extension Float80 : SignedNumber {
  func __negate__() -> Float80 { return Float80(Builtin.fneg_FPIEEE80(value)) }
  func isNegative() -> Bool { return this < 0 }
}

//===----------------------------------------------------------------------===//
// Explicit conversions between types.
//===----------------------------------------------------------------------===//

extension Int8 {
  constructor(v : Float80) {
    value = Builtin.fptosi_FPIEEE80_Int8(v.value)
  }
}

extension UInt8 {
  constructor(v : Float80) {
    value = Builtin.fptoui_FPIEEE80_Int8(v.value)
  }
}

extension Int16 {
  constructor(v : Float80) {
    value = Builtin.fptosi_FPIEEE80_Int16(v.value)
  }
}

extension Int32 {
  constructor(v : Float80) {
    value = Builtin.fptosi_FPIEEE80_Int32(v.value)
  }
}

extension UInt32 {
  constructor(v : Float80) {
    value = Builtin.fptoui_FPIEEE80_Int32(v.value)
  }
}

extension Int64 {
  constructor(v : Float80) {
    value = Builtin.fptosi_FPIEEE80_Int64(v.value)
  }
}

extension UInt64 {
  constructor(v : Float80) {
    value = Builtin.fptosi_FPIEEE80_Int64(v.value)
  }
}

extension Int128 {
  constructor(v : Float80) {
    value = Builtin.fptosi_FPIEEE80_Int128(v.value)
  }
}

extension Float32 {
  constructor(v : Float80) {
    value = Builtin.fptrunc_FPIEEE80_FPIEEE32(v.value)
  }
}

extension Float64 {
  constructor(v : Float80) {
    value = Builtin.fptrunc_FPIEEE80_FPIEEE64(v.value)
  }
}

extension Float80 {
  constructor(v : Int8) {
    value = Builtin.sitofp_Int8_FPIEEE80(v.value)
  }
  constructor(v : UInt8) {
    value = Builtin.uitofp_Int8_FPIEEE80(v.value)
  }
  constructor(v : Int16) {
    value = Builtin.sitofp_Int16_FPIEEE80(v.value)
  }
  constructor(v : UInt16) {
    value = Builtin.uitofp_Int16_FPIEEE80(v.value)
  }
  constructor(v : Int32) {
    value = Builtin.sitofp_Int32_FPIEEE80(v.value)
  }
  constructor(v : UInt32) {
    value = Builtin.uitofp_Int32_FPIEEE80(v.value)
  }
  constructor(v : UInt64) {
    value = Builtin.uitofp_Int64_FPIEEE80(v.value)
  }
  constructor(v : Int64) {
    value = Builtin.sitofp_Int64_FPIEEE80(v.value)
  }
  constructor(v : Int128) {
    value = Builtin.sitofp_Int128_FPIEEE80(v.value)
  }
  constructor(v : Float32) {
    value = Builtin.fpext_FPIEEE32_FPIEEE80(v.value)
  }
  constructor(v : Float64) {
    value = Builtin.fpext_FPIEEE64_FPIEEE80(v.value)
  }
}

//===----------------------------------------------------------------------===//
// Standard Operators
//===----------------------------------------------------------------------===//

// Unary addition operators.
func [prefix] +(a : Float80) -> Float80 { return a }

// Binary Multiplication.
func * (lhs : Float80, rhs : Float80) -> Float80 {
  return Float80(Builtin.fmul_FPIEEE80(lhs.value, rhs.value))
}

// Binary Division.
func / (lhs: Float80, rhs: Float80) -> Float80 {
  return Float80(Builtin.fdiv_FPIEEE80(lhs.value, rhs.value))
}

// Binary Remainder.
// The sign of the result matches the sign of the dividend.
// 1) This is consistent with '%' in C#, D, Java, and JavaScript
// 2) C99 requires this behavior for fmod*()
// 3) C++11 requires this behavior for std::fmod*()
func [asmname="fmodl"] % (lhs: Float80, rhs: Float80) -> Float80

// Binary Addition.
func + (lhs: Float80, rhs: Float80) -> Float80 {
  return Float80(Builtin.fadd_FPIEEE80(lhs.value, rhs.value))
}

// Binary Subtraction.
func - (lhs: Float80, rhs: Float80) -> Float80 {
  return Float80(Builtin.fsub_FPIEEE80(lhs.value, rhs.value))
}

// See Bool.swift for && and ||
// In C, 120 is &&
// In C, 110 is ||

// In C, 100 is ?:
// In C, 90 is =, *=, += etc.

// Compound assignment (with addition)
func [assignment] += (lhs : [byref] Float80, rhs : Float80) {
  lhs = lhs + rhs
}

// Compound assignment (with subtraction)
func [assignment] -= (lhs : [byref] Float80, rhs : Float80) {
  lhs = lhs - rhs
}

// Compound assignment (with multiplication)
func [assignment] *= (lhs : [byref] Float80, rhs : Float80) {
  lhs = lhs * rhs
}

// Compound assignment (with division)
func [assignment] /= (lhs : [byref] Float80, rhs : Float80) {
  lhs = lhs / rhs
}

// Compound assignment (with remainder)
func [assignment] %= (lhs : [byref] Float80, rhs : Float80) {
  lhs = lhs % rhs
}

// math
func [asmname="sinl"]    sin(x : Float80) -> Float80
func [asmname="cosl"]    cos(x : Float80) -> Float80
func [asmname="tanl"]    tan(x : Float80) -> Float80
func [asmname="atanl"]  atan(x : Float80) -> Float80
func [asmname="atan2l"] atan(y : Float80, x : Float80) -> Float80
func [asmname="sqrtl"]  sqrt(x : Float80) -> Float80
