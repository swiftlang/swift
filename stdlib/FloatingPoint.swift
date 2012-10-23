struct Float {
  var value : Builtin.FPIEEE32

  static func convertFromFloatLiteral(val : Builtin.FPIEEE32) -> Float {
    return Float(val)
  }

  // Allow converting integer literals to floating point types.
  static func convertFromIntegerLiteral(v : Builtin.Int128) -> Float {
    return Float(Builtin.uitofp_Int128_FPIEEE32(v))
  }

  func replPrint() {
    print(Double(this))
  }
}

struct Double {
  var value : Builtin.FPIEEE64 
  
  static func convertFromFloatLiteral(val : Builtin.FPIEEE64) -> Double {
    return Double(val)
  }
  
  static func convertFromIntegerLiteral(v : Builtin.Int128) -> Double {
    return Double(Builtin.uitofp_Int128_FPIEEE64(v))
  }

  func replPrint() {
    print(this)
  }
}

//===----------------------------------------------------------------------===//
// Explicit conversions between types.
//===----------------------------------------------------------------------===//

extension Int8 {
  constructor(v : Float) {
    value = Builtin.fptosi_FPIEEE32_Int8(v.value)
  }
  constructor(v : Double) {
    value = Builtin.fptosi_FPIEEE64_Int8(v.value)
  }
}

extension UInt8 {
  constructor(v : Float) {
    value = Builtin.fptoui_FPIEEE32_Int8(v.value)
  }
  constructor(v : Double) {
    value = Builtin.fptoui_FPIEEE64_Int8(v.value)
  }
}

extension Int16 {
  constructor(v : Float) {
    value = Builtin.fptosi_FPIEEE32_Int16(v.value)
  }
  constructor(v : Double) {
    value = Builtin.fptosi_FPIEEE64_Int16(v.value)
  }
}

extension Int32 {
  constructor(v : Float) {
    value = Builtin.fptosi_FPIEEE32_Int32(v.value)
  }
  constructor(v : Double) {
    value = Builtin.fptosi_FPIEEE64_Int32(v.value)
  }
}

extension UInt32 {
  constructor(v : Float) {
    value = Builtin.fptoui_FPIEEE32_Int32(v.value)
  }
  constructor(v : Double) {
    value = Builtin.fptoui_FPIEEE64_Int32(v.value)
  }
}

extension Int64 {
  constructor(v : Float) {
    value = Builtin.fptosi_FPIEEE32_Int64(v.value)
  }
  constructor(v : Double) {
    value = Builtin.fptosi_FPIEEE64_Int64(v.value)
  }
}

extension UInt64 {
  constructor(v : Float) {
    value = Builtin.fptosi_FPIEEE32_Int64(v.value)
  }
  constructor(v : Double) {
    value = Builtin.fptosi_FPIEEE64_Int64(v.value)
  }
}

extension Int128 {
  constructor(v : Float) {
    value = Builtin.fptosi_FPIEEE32_Int128(v.value)
  }
  constructor(v : Double) {
    value = Builtin.fptosi_FPIEEE64_Int128(v.value)
  }
}

extension Float {
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
  constructor(v : Double) {
    value = Builtin.fptrunc_FPIEEE64_FPIEEE32(v.value)
  }
}

extension Double {
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
  constructor(v : Float) {
    value = Builtin.fpext_FPIEEE32_FPIEEE64(v.value)
  }
}

//===----------------------------------------------------------------------===//
// Standard Operators
//===----------------------------------------------------------------------===//

// Unary negation operators.
func -(a : Float)  -> Float  { return 0.0 - a }
func -(a : Double) -> Double { return 0.0 - a }

// Unary addition operators.
func +(a : Float)   -> Float  { return a }
func +(a : Double)  -> Double { return a }

// Binary Multiplication.
func [infix_left=200] * (lhs : Float, rhs : Float) -> Float {
  return Float(Builtin.fmul_FPIEEE32(lhs.value, rhs.value))
}
func [infix_left=200] * (lhs : Double, rhs : Double) -> Double {
  return Double(Builtin.fmul_FPIEEE64(lhs.value, rhs.value))
}

// Binary Division.
func [infix_left=200] / (lhs : Float, rhs : Float) -> Float {
  return Float(Builtin.fdiv_FPIEEE32(lhs.value, rhs.value))
}
func [infix_left=200] / (lhs: Double, rhs: Double) -> Double {
  return Double(Builtin.fdiv_FPIEEE64(lhs.value, rhs.value))
}

// Binary Remainder.
// FIXME: Should we support % on floating point types?

// Binary Addition.
func [infix_left=190] + (lhs: Float, rhs: Float) -> Float {
  return Float(Builtin.fadd_FPIEEE32(lhs.value, rhs.value))
}
func [infix_left=190] + (lhs: Double, rhs: Double) -> Double {
  return Double(Builtin.fadd_FPIEEE64(lhs.value, rhs.value))
}

// Binary Subtraction.
func [infix_left=190] - (lhs: Float, rhs: Float) -> Float {
  return Float(Builtin.fsub_FPIEEE32(lhs.value, rhs.value))
}
func [infix_left=190] - (lhs: Double, rhs: Double) -> Double {
  return Double(Builtin.fsub_FPIEEE64(lhs.value, rhs.value))
}

// Less-Than Comparison.
func [infix=170] < (lhs : Float, rhs : Float) -> Bool {
  return _getBool(Builtin.fcmp_olt_FPIEEE32(lhs.value, rhs.value))
}
func [infix=170] < (lhs : Double, rhs : Double) -> Bool {
  return _getBool(Builtin.fcmp_olt_FPIEEE64(lhs.value, rhs.value))
}

// Greater-Than Comparison.
func [infix=170] > (lhs : Float, rhs : Float) -> Bool {
  return _getBool(Builtin.fcmp_ogt_FPIEEE32(lhs.value, rhs.value))
}
func [infix=170] > (lhs : Double, rhs : Double) -> Bool {
  return _getBool(Builtin.fcmp_ogt_FPIEEE64(lhs.value, rhs.value))
}

// Less-Than-Or-Equal Comparison.
func [infix=170] <= (lhs : Float, rhs : Float) -> Bool {
  return _getBool(Builtin.fcmp_ole_FPIEEE32(lhs.value, rhs.value))
}
func [infix=170] <= (lhs : Double, rhs : Double) -> Bool {
  return _getBool(Builtin.fcmp_ole_FPIEEE64(lhs.value, rhs.value))
}

// Greater-Than-Or-Equal Comparison.
func [infix=170] >= (lhs : Float, rhs : Float) -> Bool {
  return _getBool(Builtin.fcmp_oge_FPIEEE32(lhs.value, rhs.value))
}
func [infix=170] >= (lhs : Double, rhs : Double) -> Bool {
  return _getBool(Builtin.fcmp_oge_FPIEEE64(lhs.value, rhs.value))
}

// Equality Comparison.
func [infix=160] == (lhs : Float, rhs : Float) -> Bool {
  return _getBool(Builtin.fcmp_oeq_FPIEEE32(lhs.value, rhs.value))
}
func [infix=160] == (lhs : Double, rhs : Double) -> Bool {
  return _getBool(Builtin.fcmp_oeq_FPIEEE64(lhs.value, rhs.value))
}

// Not-Equality Comparison.
func [infix=160] != (lhs : Float, rhs : Float) -> Bool {
  return _getBool(Builtin.fcmp_une_FPIEEE32(lhs.value, rhs.value))
}
func [infix=160] != (lhs : Double, rhs : Double) -> Bool {
  return _getBool(Builtin.fcmp_une_FPIEEE64(lhs.value, rhs.value))
}

// See Bool.swift for && and ||
// In C, 120 is &&
// In C, 110 is ||

// In C, 100 is ?:
// In C, 90 is =, *=, += etc.

// Compound assignment (with addition)
func [assignment,infix_left=90] += (lhs : [byref] Float, rhs : Float) {
  lhs = lhs + rhs
}
func [assignment,infix_left=90] += (lhs : [byref] Double, rhs : Double) {
  lhs = lhs + rhs
}

// Compound assignment (with subtraction)
func [assignment,infix_left=90] -= (lhs : [byref] Float, rhs : Float) {
  lhs = lhs - rhs
}
func [assignment,infix_left=90] -= (lhs : [byref] Double, rhs : Double) {
  lhs = lhs - rhs
}

// Compound assignment (with multiplication)
func [assignment,infix_left=90] *= (lhs : [byref] Float, rhs : Float) {
  lhs = lhs * rhs
}
func [assignment,infix_left=90] *= (lhs : [byref] Double, rhs : Double) {
  lhs = lhs * rhs
}

// Compound assignment (with division)
func [assignment,infix_left=90] /= (lhs : [byref] Float, rhs : Float) {
  lhs = lhs / rhs
}
func [assignment,infix_left=90] /= (lhs : [byref] Double, rhs : Double) {
  lhs = lhs / rhs
}

// Compound assignment (with remainder)
// FIXME -- do we want to support this?
