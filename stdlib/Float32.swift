struct Float32 : Comparable {
  var value : Builtin.FPIEEE32

  static func convertFromFloatLiteral(val : Builtin.FPIEEE32) -> Float32 {
    return Float32(val)
  }

  // Allow converting integer literals to floating point types.
  static func convertFromIntegerLiteral(v : Builtin.Int128) -> Float32 {
    return Float32(Builtin.uitofp_Int128_FPIEEE32(v))
  }

  func replPrint() {
    print(Float64(this))
  }
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

// Unary negation operators.
func -(a : Float32) -> Float32 { return 0.0 - a }

// Unary addition operators.
func +(a : Float32) -> Float32 { return a }

// Binary Multiplication.
func [infix_left=200] * (lhs : Float32, rhs : Float32) -> Float32 {
  return Float32(Builtin.fmul_FPIEEE32(lhs.value, rhs.value))
}

// Binary Division.
func [infix_left=200] / (lhs : Float32, rhs : Float32) -> Float32 {
  return Float32(Builtin.fdiv_FPIEEE32(lhs.value, rhs.value))
}

// Binary Remainder.
// The sign of the result matches the sign of the dividend.
// 1) This is consistent with '%' in C#, D, Java, and JavaScript
// 2) C99 requires this behavior for fmod*()
// 3) C++11 requires this behavior for std::fmod*()
func [asmname="fmodf",infix_left=200] % (lhs: Float32, rhs: Float32) -> Float32

// Binary Addition.
func [infix_left=190] + (lhs: Float32, rhs: Float32) -> Float32 {
  return Float32(Builtin.fadd_FPIEEE32(lhs.value, rhs.value))
}

// Binary Subtraction.
func [infix_left=190] - (lhs: Float32, rhs: Float32) -> Float32 {
  return Float32(Builtin.fsub_FPIEEE32(lhs.value, rhs.value))
}

// Less-Than Comparison.
func [infix=170] < (lhs : Float32, rhs : Float32) -> Bool {
  return _getBool(Builtin.fcmp_olt_FPIEEE32(lhs.value, rhs.value))
}

// Greater-Than Comparison.
func [infix=170] > (lhs : Float32, rhs : Float32) -> Bool {
  return _getBool(Builtin.fcmp_ogt_FPIEEE32(lhs.value, rhs.value))
}

// Less-Than-Or-Equal Comparison.
func [infix=170] <= (lhs : Float32, rhs : Float32) -> Bool {
  return _getBool(Builtin.fcmp_ole_FPIEEE32(lhs.value, rhs.value))
}

// Greater-Than-Or-Equal Comparison.
func [infix=170] >= (lhs : Float32, rhs : Float32) -> Bool {
  return _getBool(Builtin.fcmp_oge_FPIEEE32(lhs.value, rhs.value))
}

// Equality Comparison.
func [infix=160] == (lhs : Float32, rhs : Float32) -> Bool {
  return _getBool(Builtin.fcmp_oeq_FPIEEE32(lhs.value, rhs.value))
}

// Not-Equality Comparison.
func [infix=160] != (lhs : Float32, rhs : Float32) -> Bool {
  return _getBool(Builtin.fcmp_une_FPIEEE32(lhs.value, rhs.value))
}

// See Bool.swift for && and ||
// In C, 120 is &&
// In C, 110 is ||

// In C, 100 is ?:
// In C, 90 is =, *=, += etc.

// Compound assignment (with addition)
func [assignment,infix=90] += (lhs : [byref] Float32, rhs : Float32) {
  lhs = lhs + rhs
}

// Compound assignment (with subtraction)
func [assignment,infix=90] -= (lhs : [byref] Float32, rhs : Float32) {
  lhs = lhs - rhs
}

// Compound assignment (with multiplication)
func [assignment,infix=90] *= (lhs : [byref] Float32, rhs : Float32) {
  lhs = lhs * rhs
}

// Compound assignment (with division)
func [assignment,infix=90] /= (lhs : [byref] Float32, rhs : Float32) {
  lhs = lhs / rhs
}

// Compound assignment (with remainder)
func [assignment,infix=90] %= (lhs : [byref] Float32, rhs : Float32) {
  lhs = lhs % rhs
}
