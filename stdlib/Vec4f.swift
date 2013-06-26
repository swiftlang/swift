struct Vec4f {
  var value : Builtin.Vec4xFPIEEE32

  var length : Int { return 4 }

  constructor() { }

  constructor(value : Builtin.Vec4xFPIEEE32) { 
    this.value = value 
  }

  constructor(a : Float, b : Float, c : Float, d : Float) {
    this[0] = a
    this[1] = b
    this[2] = c
    this[3] = d
  }

  subscript (index : Int) -> Float {
  get:
    assert(index >= 0 && index < 4, "out-of-bounds vector access")
    return Float(Builtin.extractelement_Vec4xFPIEEE32_Int32(value, Int32(index).value))

  set(newValue):
    assert(index >= 0 && index < 4, "out-of-bounds vector access")
    value = Builtin.insertelement_Vec4xFPIEEE32_FPIEEE32_Int32(value, newValue.value, 
                                                               Int32(index).value)
  }

  func replPrint() {
    print("Vec4f(\(this[0]), \(this[1]), \(this[2]), \(this[3]))");
  }
}

// Arithmetic operations
func +(lhs : Vec4f, rhs : Vec4f) -> Vec4f {
  return Vec4f(Builtin.fadd_Vec4xFPIEEE32(lhs.value, rhs.value))
}

func -(lhs : Vec4f, rhs : Vec4f) -> Vec4f {
  return Vec4f(Builtin.fsub_Vec4xFPIEEE32(lhs.value, rhs.value))
}

func *(lhs : Vec4f, rhs : Vec4f) -> Vec4f {
  return Vec4f(Builtin.fmul_Vec4xFPIEEE32(lhs.value, rhs.value))
}

func /(lhs : Vec4f, rhs : Vec4f) -> Vec4f {
  return Vec4f(Builtin.fdiv_Vec4xFPIEEE32(lhs.value, rhs.value))
}

func [prefix] +(vec : Vec4f) -> Vec4f {
  return vec
}

func [prefix] -(vec : Vec4f) -> Vec4f {
  return Vec4f(Builtin.fneg_Vec4xFPIEEE32(vec.value))
}

// Comparisons
func ==(lhs : Vec4f, rhs : Vec4f) -> Vec4b {
  return Vec4b(Builtin.fcmp_oeq_Vec4xFPIEEE32(lhs.value, rhs.value))
}

func !=(lhs : Vec4f, rhs : Vec4f) -> Vec4b {
  return Vec4b(Builtin.fcmp_one_Vec4xFPIEEE32(lhs.value, rhs.value))
}

func <(lhs : Vec4f, rhs : Vec4f) -> Vec4b {
  return Vec4b(Builtin.fcmp_olt_Vec4xFPIEEE32(lhs.value, rhs.value))
}

func <=(lhs : Vec4f, rhs : Vec4f) -> Vec4b {
  return Vec4b(Builtin.fcmp_ole_Vec4xFPIEEE32(lhs.value, rhs.value))
}

func >(lhs : Vec4f, rhs : Vec4f) -> Vec4b {
  return Vec4b(Builtin.fcmp_ogt_Vec4xFPIEEE32(lhs.value, rhs.value))
}

func >=(lhs : Vec4f, rhs : Vec4f) -> Vec4b {
  return Vec4b(Builtin.fcmp_oge_Vec4xFPIEEE32(lhs.value, rhs.value))
}

