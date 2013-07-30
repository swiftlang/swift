//===----------------------------------------------------------------------===//
// Vectors of Int Type
//===----------------------------------------------------------------------===//

// Vec2i8 - Vector of two 8 bit signed integers
struct Vec2i8 {
  var value : Builtin.Vec2xInt8

  var length : Int { return 2 }

  constructor() { }

  constructor(value : Builtin.Vec2xInt8) {
    this.value = value 
  }

  constructor(a : Int8, b : Int8) {
    this[0] = a
    this[1] = b
  }

  constructor(a : Int8) {
    this[0] = a
    this[1] = a
  }

  subscript (index : Int) -> Int8 {
  get:
    assert(index >= 0 && index < 2, "out-of-bounds vector access")
    return Int8(Builtin.extractelement_Vec2xInt8_Int32(value, Int32(index).value))

  set(newValue):
    assert(index >= 0 && index < 2, "out-of-bounds vector access")
    value = Builtin.insertelement_Vec2xInt8_Int8_Int32(value, newValue.value, Int32(index).value)
  }

  func replPrint() {
    print("Vec2i8(\(this[0]), \(this[1]))");
  }
 }

//===----------------------------------------------------------------------===//
// Standard Operators
//===----------------------------------------------------------------------===//

// Unary addition operators.
func [prefix] +(vec : Vec2i8) -> Vec2i8 {
  return vec
}

// Unary subtraction operator.
func [prefix] -(vec : Vec2i8) -> Vec2i8 {
  return Vec2i8(Builtin.sub_Vec2xInt8(Vec2i8(0).value, vec.value))
}

// Binary Multiplication.
func * (lhs : Vec2i8, rhs : Vec2i8) -> Vec2i8 {
  return Vec2i8(Builtin.mul_Vec2xInt8(lhs.value, rhs.value))
}

// Binary Division.
func / (lhs : Vec2i8, rhs : Vec2i8) -> Vec2i8 {
  return Vec2i8(Builtin.sdiv_Vec2xInt8(lhs.value, rhs.value))
}

// Binary Remainder.
func % (lhs : Vec2i8, rhs : Vec2i8) -> Vec2i8 {
  return Vec2i8(Builtin.srem_Vec2xInt8(lhs.value, rhs.value))
}

// Binary Addition.
func + ( lhs : Vec2i8, rhs : Vec2i8) -> Vec2i8 {
  return Vec2i8(Builtin.add_Vec2xInt8(lhs.value, rhs.value))
}

// Binary Subtraction.
func - (lhs : Vec2i8, rhs : Vec2i8) -> Vec2i8 {
  return Vec2i8(Builtin.sub_Vec2xInt8(lhs.value, rhs.value))
}

// Compound assignment (with addition)
func [assignment] += (lhs : [byref] Vec2i8, rhs : Vec2i8) {
  lhs = lhs + rhs
}

// Compound assignment (with subtraction)
func [assignment] -= (lhs : [byref] Vec2i8, rhs : Vec2i8) {
  lhs = lhs - rhs
}

// Compound assignment (with multiplication)
func [assignment] *= (lhs : [byref] Vec2i8, rhs : Vec2i8) {
  lhs = lhs * rhs
}

// Compound assignment (with division)
func [assignment] /= (lhs : [byref] Vec2i8, rhs : Vec2i8) {
  lhs = lhs / rhs
}

// Compound assignment (with remainder)
func [assignment] %= (lhs : [byref] Vec2i8, rhs : Vec2i8) {
  lhs = lhs % rhs
}

// Unary bitwise complement.
func [prefix] ~ (vec : Vec2i8) -> Vec2i8 {
  return Vec2i8(Builtin.xor_Vec2xInt8(vec.value, Vec2i8(-1).value))
}

// Unary logical complement.
func [prefix] ! (vec : Vec2i8) -> Vec2i8 {
  return ~vec
}

// Bitwise 'and'.
func & (lhs : Vec2i8, rhs : Vec2i8) -> Vec2i8 {
  return Vec2i8(Builtin.and_Vec2xInt8(lhs.value, rhs.value))
}

// Bitwise 'xor'.
func ^ (lhs : Vec2i8, rhs : Vec2i8) -> Vec2i8 {
  return Vec2i8(Builtin.xor_Vec2xInt8(lhs.value, rhs.value))
}

// Bitwise 'or'.
func | (lhs : Vec2i8, rhs : Vec2i8) -> Vec2i8 {
  return Vec2i8(Builtin.or_Vec2xInt8(lhs.value, rhs.value))
}

// Shift left.
func << (lhs : Vec2i8, rhs : Vec2i8) -> Vec2i8 {
  return Vec2i8(Builtin.shl_Vec2xInt8(lhs.value, rhs.value))
}

// Shift right.
func >> (lhs : Vec2i8, rhs : Vec2i8) -> Vec2i8 {
  return Vec2i8(Builtin.lshr_Vec2xInt8(lhs.value, rhs.value))
}

// Compare equal.
func == (lhs : Vec2i8, rhs : Vec2i8) -> Vec2b {
  return Vec2b(Builtin.cmp_eq_Vec2xInt8(lhs.value, rhs.value))
}

// Compare not equal.
func != (lhs : Vec2i8, rhs : Vec2i8) -> Vec2b {
  return Vec2b(Builtin.cmp_ne_Vec2xInt8(lhs.value, rhs.value))
}

// Compare less.
func < (lhs : Vec2i8, rhs : Vec2i8) -> Vec2b {
  return Vec2b(Builtin.cmp_slt_Vec2xInt8(lhs.value, rhs.value))
}

// Compare less than.
func <= (lhs : Vec2i8, rhs : Vec2i8) -> Vec2b {
  return Vec2b(Builtin.cmp_sle_Vec2xInt8(lhs.value, rhs.value))
}

// Compare greater.
func > (lhs : Vec2i8, rhs : Vec2i8) -> Vec2b {
  return Vec2b(Builtin.cmp_sgt_Vec2xInt8(lhs.value, rhs.value))
}

// Compare greater than.
func >= (lhs : Vec2i8, rhs : Vec2i8) -> Vec2b {
  return Vec2b(Builtin.cmp_sge_Vec2xInt8(lhs.value, rhs.value))
}
