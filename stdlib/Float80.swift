struct Float80 : Comparable {
  var value : Builtin.FPIEEE80

  static func convertFromFloatLiteral(val : Builtin.FPIEEE80) -> Float80 {
    return Float80(val)
  }

  static func convertFromIntegerLiteral(v : Builtin.Int128) -> Float80 {
    return Float80(Builtin.uitofp_Int128_FPIEEE80(v))
  }

  func replPrint() {
    print(Float64(this))
  }
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

// Unary negation operators.
func -(a : Float80) -> Float80 { return 0.0 - a }

// Unary addition operators.
func +(a : Float80) -> Float80 { return a }

// Binary Multiplication.
func [infix_left=200] * (lhs : Float80, rhs : Float80) -> Float80 {
  return Float80(Builtin.fmul_FPIEEE80(lhs.value, rhs.value))
}

// Binary Division.
func [infix_left=200] / (lhs: Float80, rhs: Float80) -> Float80 {
  return Float80(Builtin.fdiv_FPIEEE80(lhs.value, rhs.value))
}

// Binary Remainder.
// The sign of the result matches the sign of the dividend.
// 1) This is consistent with '%' in C#, D, Java, and JavaScript
// 2) C99 requires this behavior for fmod*()
// 3) C++11 requires this behavior for std::fmod*()
func [asmname="fmodl",infix_left=200] % (lhs: Float80, rhs: Float80) -> Float80

// Binary Addition.
func [infix_left=190] + (lhs: Float80, rhs: Float80) -> Float80 {
  return Float80(Builtin.fadd_FPIEEE80(lhs.value, rhs.value))
}

// Binary Subtraction.
func [infix_left=190] - (lhs: Float80, rhs: Float80) -> Float80 {
  return Float80(Builtin.fsub_FPIEEE80(lhs.value, rhs.value))
}

// Less-Than Comparison.
func [infix=170] < (lhs : Float80, rhs : Float80) -> Bool {
  return _getBool(Builtin.fcmp_olt_FPIEEE80(lhs.value, rhs.value))
}

// Greater-Than Comparison.
func [infix=170] > (lhs : Float80, rhs : Float80) -> Bool {
  return _getBool(Builtin.fcmp_ogt_FPIEEE80(lhs.value, rhs.value))
}

// Less-Than-Or-Equal Comparison.
func [infix=170] <= (lhs : Float80, rhs : Float80) -> Bool {
  return _getBool(Builtin.fcmp_ole_FPIEEE80(lhs.value, rhs.value))
}

// Greater-Than-Or-Equal Comparison.
func [infix=170] >= (lhs : Float80, rhs : Float80) -> Bool {
  return _getBool(Builtin.fcmp_oge_FPIEEE80(lhs.value, rhs.value))
}

// Equality Comparison.
func [infix=160] == (lhs : Float80, rhs : Float80) -> Bool {
  return _getBool(Builtin.fcmp_oeq_FPIEEE80(lhs.value, rhs.value))
}

// Not-Equality Comparison.
func [infix=160] != (lhs : Float80, rhs : Float80) -> Bool {
  return _getBool(Builtin.fcmp_une_FPIEEE80(lhs.value, rhs.value))
}

// See Bool.swift for && and ||
// In C, 120 is &&
// In C, 110 is ||

// In C, 100 is ?:
// In C, 90 is =, *=, += etc.

// Compound assignment (with addition)
func [assignment,infix=90] += (lhs : [byref] Float80, rhs : Float80) {
  lhs = lhs + rhs
}

// Compound assignment (with subtraction)
func [assignment,infix=90] -= (lhs : [byref] Float80, rhs : Float80) {
  lhs = lhs - rhs
}

// Compound assignment (with multiplication)
func [assignment,infix=90] *= (lhs : [byref] Float80, rhs : Float80) {
  lhs = lhs * rhs
}

// Compound assignment (with division)
func [assignment,infix=90] /= (lhs : [byref] Float80, rhs : Float80) {
  lhs = lhs / rhs
}

// Compound assignment (with remainder)
func [assignment,infix=90] %= (lhs : [byref] Float80, rhs : Float80) {
  lhs = lhs % rhs
}
