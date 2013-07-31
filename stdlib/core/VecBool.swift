//===----------------------------------------------------------------------===//
// Vectors of Bool Type
//===----------------------------------------------------------------------===//

struct Vec2b {
  var value : Builtin.Vec2xInt8

  var length : Int { return 2 }

  constructor() { }

  constructor(value : Builtin.Vec2xInt1) { 
    this.value = Builtin.zext_Vec2xInt1_Vec2xInt8(value)
  }

  constructor(value : Builtin.Vec2xInt8) { 
    this.value = value 
  }

  constructor(a : Bool, b : Bool) {
    this[0] = a
    this[1] = b
  }

 constructor(a : Bool) {
    this[0] = a
    this[1] = a
  }

  subscript (index : Int) -> Bool {
  get:
    assert(index >= 0 && index < 2, "out-of-bounds vector access")
    return Int8(Builtin.extractelement_Vec2xInt8_Int32(value, Int32(index).value)) != 0

  set(newValue):
    assert(index >= 0 && index < 2, "out-of-bounds vector access")
    value = Builtin.insertelement_Vec2xInt8_Int8_Int32(
               value,
               (newValue? 1 as Int8 : 0 as Int8).value,
               Int32(index).value)
  }

  func replPrint() {
    print("Vec2b(\(this[0]), \(this[1]))");
  }
}

//===----------------------------------------------------------------------===//
// Standard Operators
//===----------------------------------------------------------------------===//

// Bitwise 'and'.
func & (lhs : Vec2b, rhs : Vec2b) -> Vec2b {
  return Vec2b(Builtin.and_Vec2xInt8(lhs.value, rhs.value))
}

// Bitwise 'xor'.
func ^ (lhs : Vec2b, rhs : Vec2b) -> Vec2b {
  return Vec2b(Builtin.xor_Vec2xInt8(lhs.value, rhs.value))
}

// Bitwise 'or'.
func | (lhs : Vec2b, rhs : Vec2b) -> Vec2b {
  return Vec2b(Builtin.or_Vec2xInt8(lhs.value, rhs.value))
}
