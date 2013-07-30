//===----------------------------------------------------------------------===//
// Vectors of Floats Type
//===----------------------------------------------------------------------===//

// Vector of 2 32-bit floats.
struct Vec2f {
  var value : Builtin.Vec2xFPIEEE32

  var length : Int { return 2 }

  constructor() { }

  constructor(value : Builtin.Vec2xFPIEEE32) { 
    this.value = value 
  }

  constructor(a : Float, b : Float) {
    this[0] = a
    this[1] = b
  }

  constructor(a : Float) {
    this[0] = a
    this[1] = a
  }

  subscript (index : Int) -> Float {
  get:
    assert(index >= 0 && index < 2, "out-of-bounds vector access")
    return Float(Builtin.extractelement_Vec2xFPIEEE32_Int32(value, Int32(index).value))

  set(newValue):
    assert(index >= 0 && index < 2, "out-of-bounds vector access")
    value = Builtin.insertelement_Vec2xFPIEEE32_FPIEEE32_Int32(value, newValue.value, Int32(index).value)
  }

  func replPrint() {
    print("Vec2f(\(this[0]), \(this[1]))");
  }
 }

//===----------------------------------------------------------------------===//
// Standard Operators
//===----------------------------------------------------------------------===//

// Unary addition operators.
func [prefix] +(vec : Vec2f) -> Vec2f {
  return vec
}

// Unary subtraction operator.
func [prefix] -(vec : Vec2f) -> Vec2f {
  return Vec2f(Builtin.fneg_Vec2xFPIEEE32(vec.value))
}

// Binary Multiplication.
func * (lhs : Vec2f, rhs : Vec2f) -> Vec2f {
  return Vec2f(Builtin.fmul_Vec2xFPIEEE32(lhs.value, rhs.value))
}

// Binary Division.
func / (lhs : Vec2f, rhs : Vec2f) -> Vec2f {
  return Vec2f(Builtin.fdiv_Vec2xFPIEEE32(lhs.value, rhs.value))
}

// Arithmetic operations.
func + (lhs : Vec2f, rhs : Vec2f) -> Vec2f {
  return Vec2f(Builtin.fadd_Vec2xFPIEEE32(lhs.value, rhs.value))
}

func - (lhs : Vec2f, rhs : Vec2f) -> Vec2f {
  return Vec2f(Builtin.fsub_Vec2xFPIEEE32(lhs.value, rhs.value))
}

// Compound assignment (with addition)
func [assignment] += (lhs : [byref] Vec2f, rhs : Vec2f) {
  lhs = lhs + rhs
}

// Compound assignment (with subtraction)
func [assignment] -= (lhs : [byref] Vec2f, rhs : Vec2f) {
  lhs = lhs - rhs
}

// Compound assignment (with multiplication)
func [assignment] *= (lhs : [byref] Vec2f, rhs : Vec2f) {
  lhs = lhs * rhs
}

// Compound assignment (with division)
func [assignment] /= (lhs : [byref] Vec2f, rhs : Vec2f) {
  lhs = lhs / rhs
}

// Compare equal.
func ==(lhs : Vec2f, rhs : Vec2f) -> Vec2b {
  return Vec2b(Builtin.fcmp_oeq_Vec2xFPIEEE32(lhs.value, rhs.value))
}

// Compare not equal.
func != (lhs : Vec2f, rhs : Vec2f) -> Vec2b {
  return Vec2b(Builtin.fcmp_one_Vec2xFPIEEE32(lhs.value, rhs.value))
}

// Compare less.
func < (lhs : Vec2f, rhs : Vec2f) -> Vec2b {
  return Vec2b(Builtin.fcmp_olt_Vec2xFPIEEE32(lhs.value, rhs.value))
}

// Compare less than.
func <= (lhs : Vec2f, rhs : Vec2f) -> Vec2b {
  return Vec2b(Builtin.fcmp_ole_Vec2xFPIEEE32(lhs.value, rhs.value))
}

// Compare greater.
func > (lhs : Vec2f, rhs : Vec2f) -> Vec2b {
  return Vec2b(Builtin.fcmp_ogt_Vec2xFPIEEE32(lhs.value, rhs.value))
}

// Compare greater than.
func >= (lhs : Vec2f, rhs : Vec2f) -> Vec2b {
  return Vec2b(Builtin.fcmp_oge_Vec2xFPIEEE32(lhs.value, rhs.value))
}
