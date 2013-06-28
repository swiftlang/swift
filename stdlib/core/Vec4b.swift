struct Vec4b {
  var _value : Builtin.Vec4xInt8

  var length : Int { return 4 }

  constructor() { }

  constructor(value : Builtin.Vec4xInt1) { 
    this._value = Builtin.zext_Vec4xInt1_Vec4xInt8(value)
  }

  constructor(value : Builtin.Vec4xInt8) { 
    this._value = value 
  }

  constructor(a : Bool, b : Bool, c : Bool, d : Bool) {
    this[0] = a
    this[1] = b
    this[2] = c
    this[3] = d
  }

  subscript (index : Int) -> Bool {
  get:
    assert(index >= 0 && index < 4, "out-of-bounds vector access")
    return Int8(Builtin.extractelement_Vec4xInt8_Int32(_value, Int32(index).value)) != 0

  set(newValue):
    assert(index >= 0 && index < 4, "out-of-bounds vector access")
    _value = Builtin.insertelement_Vec4xInt8_Int8_Int32(
               _value, 
               (newValue? 1 as Int8 : 0 as Int8).value,
               Int32(index).value)
  }

  func replPrint() {
    print("Vec4b(\(this[0]), \(this[1]), \(this[2]), \(this[3]))");
  }
}

// Bitwise operations.
func & (lhs : Vec4b, rhs : Vec4b) -> Vec4b {
  return Vec4b(Builtin.and_Vec4xInt8(lhs._value, rhs._value))
}

func | (lhs : Vec4b, rhs : Vec4b) -> Vec4b {
  return Vec4b(Builtin.or_Vec4xInt8(lhs._value, rhs._value))
}

func ^ (lhs : Vec4b, rhs : Vec4b) -> Vec4b {
  return Vec4b(Builtin.xor_Vec4xInt8(lhs._value, rhs._value))
}
