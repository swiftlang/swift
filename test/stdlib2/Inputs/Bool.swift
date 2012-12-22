//===----------------------------------------------------------------------===//
// Bool Datatype and Supporting Operators
//===----------------------------------------------------------------------===//

// Bool is the standard way to reason about truth values.
oneof Bool {
  false, true
}
// FIXME: Convert these to immutable vars when we have them.
var true : Bool {
  get { return Bool.true }
}
var false : Bool {
  get { return Bool.false }
}


// *private* helper function for forming Bools
func _getBool(v : Builtin.Int1) -> Bool {
  if v {
    return true
  }
  return false
}


extension Bool {
  // FIXME: Implement pattern matching or equality testing to implement this.
  func getLogicValue() -> Builtin.Int1

  func replPrint() {
    if this {
      print("true")
    } else {
      print("false")
    }
  }
}

// Bitwise complement.
func ~(a : Bool) -> Bool {
  return _getBool(Builtin.xor_Int1(a.getLogicValue(), true.getLogicValue()))
}

// Logical complement.
func !(a : Bool) -> Bool {
  return ~a
}


// Not-Equality Comparison.
func [infix=160] != (lhs : Bool, rhs : Bool) -> Bool {
  return _getBool(Builtin.xor_Int1(lhs.getLogicValue(), rhs.getLogicValue()))
}

func [infix=160] == (lhs : Bool, rhs : Bool) -> Bool {
  return ~(lhs != rhs)
}

//===----------------------------------------------------------------------===//
// Standard Operators
//===----------------------------------------------------------------------===//

// Short circuiting logical operators.
func [infix_left=120] && (lhs: Bool, rhs: [auto_closure] ()->Bool) -> Bool {
  if lhs {
    return rhs()
  }
  
  return false
}
func [infix_left=110] || (lhs: Bool, rhs: [auto_closure] ()->Bool) -> Bool {
  if lhs {
    return true
  }
  
  return rhs()
}
