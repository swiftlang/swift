//===----------------------------------------------------------------------===//
// Bool Datatype and Supporting Operators
//===----------------------------------------------------------------------===//

// Bool is the standard way to reason about truth values.
oneof Bool {
  false, true

  /// \brief Default-initialize Boolean value to \c false.
  constructor() { this = .false }
}

// FIXME: Convert these to immutable vars when we have them.
var true : Bool {
  return Bool.true
}
var false : Bool {
  return Bool.false
}


// *private* helper function for forming Bools
func [asmname="swift_getBool"] _getBool(v : Builtin.Int1) -> Bool;


extension Bool : LogicValue {
  // FIXME: Implement pattern matching or equality testing to implement this.
  func [asmname="_TSb13getLogicValuefRSbFT_Bi1_"] _getBuiltinLogicValue() -> Builtin.Int1

  func getLogicValue() -> Bool { return this }

  func replPrint() {
    if this {
      print("true")
    } else {
      print("false")
    }
  }
  
  // Bool can be constructed from LogicValue
  constructor(v : LogicValue) {
    this = v.getLogicValue()
  }
}

//===----------------------------------------------------------------------===//
// Standard Operators
//===----------------------------------------------------------------------===//

// Unary bitwise complement.
func [prefix] ~(a : Bool) -> Bool {
  return a ^ true
}

// Unary logical complement.
func [prefix] !(a : Bool) -> Bool {
  return ~a
}

extension Bool : Equatable, Hashable {
  func __equal__(rhs: Bool) -> Bool {
    return _getBool(Builtin.cmp_eq_Int1(_getBuiltinLogicValue(), 
                                        rhs._getBuiltinLogicValue()))
  }
  func hashValue() -> Int {
    return this? 1 : 0
  }
}

// Bitwise 'and'.
func & (lhs : Bool, rhs : Bool) -> Bool {
  return _getBool(Builtin.and_Int1(lhs._getBuiltinLogicValue(), 
                                   rhs._getBuiltinLogicValue()))
}

// Bitwise 'xor'.
func ^ (lhs : Bool, rhs : Bool) -> Bool {
  return _getBool(Builtin.xor_Int1(lhs._getBuiltinLogicValue(),
                                   rhs._getBuiltinLogicValue()))
}

// Bitwise 'or'.
func | (lhs : Bool, rhs : Bool) -> Bool {
  return _getBool(Builtin.or_Int1(lhs._getBuiltinLogicValue(),
                                  rhs._getBuiltinLogicValue()))
}

// Short circuiting logical operators.
func && (lhs: Bool, rhs: [auto_closure] ()->Bool) -> Bool {
  if lhs {
    return rhs()
  }

  return false
}
func || (lhs: Bool, rhs: [auto_closure] ()->Bool) -> Bool {
  if lhs {
    return true
  }

  return rhs()
}

// Compound assignment (with bitwise and)
func [assignment] &= (lhs : [byref] Bool, rhs : Bool) {
  lhs = lhs & rhs
}

// Compound assignment (with bitwise or)
func [assignment] |= (lhs : [byref] Bool, rhs : Bool) {
  lhs = lhs | rhs
}

// Compound assignment (with bitwise xor)
func [assignment] ^= (lhs : [byref] Bool, rhs : Bool) {
  lhs = lhs ^ rhs
}
