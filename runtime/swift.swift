// RUN: %swift %s -verify -parse-as-library

//===----------------------------------------------------------------------===//
// Swift Standard Prolog Library.
//===----------------------------------------------------------------------===//

import Builtin

// Void is just a type alias for the empty tuple.
typealias Void : ()

//===----------------------------------------------------------------------===//
// Standard Arithmetic Types
//===----------------------------------------------------------------------===//

struct Int8 { 
  value : Builtin.Int8
  
  static func convertFromIntegerLiteral(val : Builtin.Int8) -> Int8 {
    return Int8(val)
  }

  func replPrint() {
    print(Int64(this))
  }
}

struct UInt8 { 
  value : Builtin.Int8
  
  static func convertFromIntegerLiteral(val : Builtin.Int8) -> UInt8 {
    return UInt8(val)
  }

  func replPrint() {
    print(Int64(this))
  }
}

struct Int16 { 
  value : Builtin.Int16 
  
  static func convertFromIntegerLiteral(val : Builtin.Int16) -> Int16 {
    return Int16(val)
  }

  func replPrint() {
    print(Int64(this))
  }
}

struct Int32 {
  value : Builtin.Int32

  static func convertFromIntegerLiteral(val : Builtin.Int32) -> Int32 {
    return Int32(val)
  }
  func getArrayBoundValue() -> Builtin.Int32 {
    return value
  }

  func replPrint() {
    print(Int64(this))
  }
}

struct UInt32 { 
  value : Builtin.Int32
  
  static func convertFromIntegerLiteral(val : Builtin.Int32) -> UInt32 {
    return UInt32(val)
  }

  func replPrint() {
    print(Int64(this))
  }
}

struct Int64 { 
  value : Builtin.Int64

  static func convertFromIntegerLiteral(val : Builtin.Int64) -> Int {
    return Int64(val)
  }
  func getArrayBoundValue() -> Builtin.Int64 {
    return value
  }

  func replPrint() {
    print(this)
  }
}

struct Int128 { 
  value : Builtin.Int128

  static func convertFromIntegerLiteral(val : Builtin.Int128) -> Int128 {
    return Int128(val)
  }

  func replPrint() {
    print(String(this))
  }
}

struct Float {
  value : Builtin.FPIEEE32

  static func convertFromFloatLiteral(val : Builtin.FPIEEE32) -> Float {
    return Float(val)
  }

  // Allow converting integer literals to floating point types.
  static func convertFromIntegerLiteral(v : Builtin.Int128) -> Float {
    return Float(Builtin.sitofp_Int128_FPIEEE32(v))
  }

  func replPrint() {
    print(Double(this))
  }
}

struct Double {
  value : Builtin.FPIEEE64 
  
  static func convertFromFloatLiteral(val : Builtin.FPIEEE64) -> Double {
    return Double(val)
  }
  
  static func convertFromIntegerLiteral(v : Builtin.Int128) -> Double {
    return Double(Builtin.sitofp_Int128_FPIEEE64(v))
  }

  func replPrint() {
    print(this)
  }
}

//===----------------------------------------------------------------------===//
// Some useful type aliases
//===----------------------------------------------------------------------===//

// Int is just a typealias for Int64.
typealias Int : Int64

// IntegerLiteralType specifies the default type to use for an integer literal
// when the type isn't constrained.
typealias IntegerLiteralType : Int

// FloatLiteralType specifies the default type to use for a floating point
// literal when the type isn't constrained.
typealias FloatLiteralType : Double


// FIXME: Consider adding "int", "double", etc as aliases for Int/Double.  They
// violate the naming convention but lower the barrier to entry.


//===----------------------------------------------------------------------===//
// Explicit conversions between types.
//===----------------------------------------------------------------------===//

extension Int8 {
  static func Int8(v : UInt8) -> Int8 {
    return Int8(v.value)
  }
  static func Int8(v : Int16) -> Int8 {
    return Int8(Builtin.trunc_Int16_Int8(v.value))
  }
  static func Int8(v : Int32) -> Int8 {
    return Int8(Builtin.trunc_Int32_Int8(v.value))
  }
  static func Int8(v : UInt32) -> Int8 {
    return Int8(Builtin.trunc_Int32_Int8(v.value))
  }
  static func Int8(v : Int64) -> Int8 {
    return Int8(Builtin.trunc_Int64_Int8(v.value))
  }
  static func Int8(v : Int128) -> Int8 {
    return Int8(Builtin.trunc_Int128_Int8(v.value))
  }
  static func Int8(v : Float) -> Int8 {
    return Int8(Builtin.fptosi_FPIEEE32_Int8(v.value))
  }
  static func Int8(v : Double) -> Int8 {
    return Int8(Builtin.fptosi_FPIEEE64_Int8(v.value))
  }
}

extension UInt8 {
  static func UInt8(v : Int8) -> UInt8 {
    return UInt8(v.value)
  }
  static func UInt8(v : Int16) -> UInt8 {
    return UInt8(Builtin.trunc_Int16_Int8(v.value))
  }
  static func UInt8(v : Int32) -> UInt8 {
    return UInt8(Builtin.trunc_Int32_Int8(v.value))
  }
  static func UInt8(v : UInt32) -> UInt8 {
    return UInt8(Builtin.trunc_Int32_Int8(v.value))
  }
  static func UInt8(v : Int64) -> UInt8 {
    return UInt8(Builtin.trunc_Int64_Int8(v.value))
  }
  static func UInt8(v : Int128) -> UInt8 {
    return UInt8(Builtin.trunc_Int128_Int8(v.value))
  }
  static func UInt8(v : Float) -> UInt8 {
    return UInt8(Builtin.fptoui_FPIEEE32_Int8(v.value))
  }
  static func UInt8(v : Double) -> UInt8 {
    return UInt8(Builtin.fptoui_FPIEEE64_Int8(v.value))
  }
}

extension Int16 {
  static func Int16(v : Int8) -> Int16 {
    return Int16(Builtin.sext_Int8_Int16(v.value))
  }
  static func Int16(v : UInt8) -> Int16 {
    return Int16(Builtin.zext_Int8_Int16(v.value))
  }
  static func Int16(v : Int32) -> Int16 {
    return Int16(Builtin.trunc_Int32_Int16(v.value))
  }
  static func Int16(v : UInt32) -> Int16 {
    return Int16(Builtin.trunc_Int32_Int16(v.value))
  }
  static func Int16(v : Int64) -> Int16 {
    return Int16(Builtin.trunc_Int64_Int16(v.value))
  }
  static func Int16(v : Int128) -> Int16 {
    return Int16(Builtin.trunc_Int128_Int16(v.value))
  }
  static func Int16(v : Float) -> Int16 {
    return Int16(Builtin.fptosi_FPIEEE32_Int16(v.value))
  }
  static func Int16(v : Double) -> Int16 {
    return Int16(Builtin.fptosi_FPIEEE64_Int16(v.value))
  }
}

extension Int32 {
  static func Int32(v : Int8) -> Int32 {
    return Int32(Builtin.sext_Int8_Int32(v.value))
  }
  static func Int32(v : UInt8) -> Int32 {
    return Int32(Builtin.zext_Int8_Int32(v.value))
  }
  static func Int32(v : Int16) -> Int32 {
    return Int32(Builtin.sext_Int16_Int32(v.value))
  }
  static func Int32(v : UInt32) -> Int32 {
    return Int32(v.value)
  }
  static func Int32(v : Int64) -> Int32 {
    return Int32(Builtin.trunc_Int64_Int32(v.value))
  }
  static func Int32(v : Int128) -> Int32 {
    return Int32(Builtin.trunc_Int128_Int32(v.value))
  }
  static func Int32(v : Float) -> Int32 {
    return Int32(Builtin.fptosi_FPIEEE32_Int32(v.value))
  }
  static func Int32(v : Double) -> Int32 {
    return Int32(Builtin.fptosi_FPIEEE64_Int32(v.value))
  }
}

extension UInt32 {
  static func UInt32(v : Int8) -> UInt32 {
    return UInt32(Builtin.sext_Int8_Int32(v.value))
  }
  static func UInt32(v : UInt8) -> UInt32 {
    return UInt32(Builtin.zext_Int8_Int32(v.value))
  }
  static func UInt32(v : Int16) -> UInt32 {
    return UInt32(Builtin.sext_Int16_Int32(v.value))
  }
  static func UInt32(v : Int32) -> UInt32 {
    return UInt32(v.value)
  }
  static func UInt32(v : Int64) -> UInt32 {
    return UInt32(Builtin.trunc_Int64_Int32(v.value))
  }
  static func UInt32(v : Int128) -> UInt32 {
    return UInt32(Builtin.trunc_Int128_Int32(v.value))
  }
  static func UInt32(v : Float) -> UInt32 {
    return UInt32(Builtin.fptoui_FPIEEE32_Int32(v.value))
  }
  static func UInt32(v : Double) -> UInt32 {
    return UInt32(Builtin.fptoui_FPIEEE64_Int32(v.value))
  }
}

extension Int64 {
  static func Int64(v : Int8) -> Int64 {
    return Int64(Builtin.sext_Int8_Int64(v.value))
  }
  static func Int64(v : UInt8) -> Int64 {
    return Int64(Builtin.zext_Int8_Int64(v.value))
  }
  static func Int64(v : Int16) -> Int64 {
    return Int64(Builtin.sext_Int16_Int64(v.value))
  }
  static func Int64(v : Int32) -> Int64 {
    return Int64(Builtin.sext_Int32_Int64(v.value))
  }
  static func Int64(v : UInt32) -> Int64 {
    return Int64(Builtin.zext_Int32_Int64(v.value))
  }
  static func Int64(v : Int128) -> Int64 {
    return Int64(Builtin.trunc_Int128_Int64(v.value))
  }
  static func Int64(v : Float) -> Int64 {
    return Int64(Builtin.fptosi_FPIEEE32_Int64(v.value))
  }
  static func Int64(v : Double) -> Int64 {
    return Int64(Builtin.fptosi_FPIEEE64_Int64(v.value))
  }
  static func Int(v : Double) -> Int64 {
    return Int64(v)
  }
}

extension Int128 {
  static func Int128(v : Int8) -> Int128 {
    return Int128(Builtin.sext_Int8_Int128(v.value))
  }
  static func Int128(v : UInt8) -> Int128 {
    return Int128(Builtin.zext_Int8_Int128(v.value))
  }
  static func Int128(v : Int16) -> Int128 {
    return Int128(Builtin.sext_Int16_Int128(v.value))
  }
  static func Int128(v : Int32) -> Int128 {
    return Int128(Builtin.sext_Int32_Int128(v.value))
  }
  static func Int128(v : UInt32) -> Int128 {
    return Int128(Builtin.zext_Int32_Int128(v.value))
  }
  static func Int128(v : Int64) -> Int128 {
    return Int128(Builtin.sext_Int64_Int128(v.value))
  }
  static func Int128(v : Float) -> Int128 {
    return Int128(Builtin.fptosi_FPIEEE32_Int128(v.value))
  }
  static func Int128(v : Double) -> Int128 {
    return Int128(Builtin.fptosi_FPIEEE64_Int128(v.value))
  }
}

extension Float {
  static func Float(v : Int8) -> Float {
    return Float(Builtin.sitofp_Int8_FPIEEE32(v.value))
  }
  static func Float(v : UInt8) -> Float {
    return Float(Builtin.uitofp_Int8_FPIEEE32(v.value))
  }
  static func Float(v : Int16) -> Float {
    return Float(Builtin.sitofp_Int16_FPIEEE32(v.value))
  }
  static func Float(v : Int32) -> Float {
    return Float(Builtin.sitofp_Int32_FPIEEE32(v.value))
  }
  static func Float(v : UInt32) -> Float {
    return Float(Builtin.uitofp_Int32_FPIEEE32(v.value))
  }
  static func Float(v : Int64) -> Float {
    return Float(Builtin.sitofp_Int64_FPIEEE32(v.value))
  }
  static func Float(v : Int128) -> Float {
    return Float(Builtin.sitofp_Int128_FPIEEE32(v.value))
  }
  static func Float(v : Double) -> Float {
    return Float(Builtin.fptrunc_FPIEEE64_FPIEEE32(v.value))
  }
}

extension Double {
  static func Double(v : Int8) -> Double {
    return Double(Builtin.sitofp_Int8_FPIEEE64(v.value))
  }
  static func Double(v : UInt8) -> Double {
    return Double(Builtin.uitofp_Int8_FPIEEE64(v.value))
  }
  static func Double(v : Int16) -> Double {
    return Double(Builtin.sitofp_Int16_FPIEEE64(v.value))
  }
  static func Double(v : Int32) -> Double {
    return Double(Builtin.sitofp_Int32_FPIEEE64(v.value))
  }
  static func Double(v : UInt32) -> Double {
    return Double(Builtin.uitofp_Int32_FPIEEE64(v.value))
  }
  static func Double(v : Int64) -> Double {
    return Double(Builtin.sitofp_Int64_FPIEEE64(v.value))
  }
  static func Double(v : Int128) -> Double {
    return Double(Builtin.sitofp_Int128_FPIEEE64(v.value))
  }
  static func Double(v : Float) -> Double {
    return Double(Builtin.fpext_FPIEEE32_FPIEEE64(v.value))
  }
}

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
func getBool(v : Builtin.Int1) -> Bool {
  if (v) {
    return true
  }
  return false
}


extension Bool {
  // FIXME: Implement pattern matching or equality testing to implement this.
  func getLogicValue() -> Builtin.Int1

  func replPrint() {
    if (this) {
      print("true")
    } else {
      print("false")
    }
  }
}

// Bitwise complement.
func ~(a : Bool) -> Bool {
  return getBool(Builtin.xor_Int1(a.getLogicValue(), true.getLogicValue()))
}

// Logical complement.
func !(a : Bool) -> Bool {
  return ~a
}


// Not-Equality Comparison.
func [infix=160] != (lhs : Bool, rhs : Bool) -> Bool {
  return getBool(Builtin.xor_Int1(lhs.getLogicValue(), rhs.getLogicValue()))
}

func [infix=160] == (lhs : Bool, rhs : Bool) -> Bool {
  return ~(lhs != rhs)
}



//===----------------------------------------------------------------------===//
// Standard Operators
//===----------------------------------------------------------------------===//

// Unary negation operators.
func -(a : Int8)   -> Int8   { return 0 - a }
func -(a : Int16)  -> Int16  { return 0 - a }
func -(a : Int32)  -> Int32  { return 0 - a }
func -(a : Int64)  -> Int64  { return 0 - a }
func -(a : Int128) -> Int128 { return 0 - a }
func -(a : Float)  -> Float  { return 0.0 - a }
func -(a : Double) -> Double { return 0.0 - a }

// Unary addition operators.
func +(a : Int8)    -> Int8   { return a }
func +(a : UInt8)   -> UInt8  { return a }
func +(a : Int16)   -> Int16  { return a }
func +(a : Int32)   -> Int32  { return a }
func +(a : UInt32)  -> UInt32 { return a }
func +(a : Int64)   -> Int64  { return a }
func +(a : Int128)  -> Int128 { return a }
func +(a : Float)   -> Float  { return a }
func +(a : Double)  -> Double { return a }

// Bitwise negation operators.
func ~(a : Int8)    -> Int8   { return a ^ -1 }
func ~(a : UInt8)   -> UInt8  { return a ^ 0xFF }
func ~(a : Int16)   -> Int16  { return a ^ -1 }
func ~(a : Int32)   -> Int32  { return a ^ -1 }
func ~(a : UInt32)  -> UInt32 { return a ^ 0xFFFFFFFF }
func ~(a : Int64)   -> Int64  { return a ^ -1 }
func ~(a : Int128)  -> Int128 { return a ^ -1 }

// Unary increment and decrement operators.  These intentionally return nothing.
func ++(a : [byref(implicit)] Int8) { a = a + 1 }
func ++(a : [byref(implicit)] UInt8) { a = a + 1 }
func ++(a : [byref(implicit)] Int16) { a = a + 1 }
func ++(a : [byref(implicit)] Int32) { a = a + 1 }
func ++(a : [byref(implicit)] UInt32) { a = a + 1 }
func ++(a : [byref(implicit)] Int64) { a = a + 1 }
func ++(a : [byref(implicit)] Int128) { a = a + 1 }
func ++(a : [byref(implicit)] Float) { a = a + 1.0 }
func ++(a : [byref(implicit)] Double) { a = a + 1.0 }
func --(a : [byref(implicit)] Int8) { a = a - 1 }
func --(a : [byref(implicit)] UInt8) { a = a - 1 }
func --(a : [byref(implicit)] Int16) { a = a - 1 }
func --(a : [byref(implicit)] Int32) { a = a - 1 }
func --(a : [byref(implicit)] UInt32) { a = a - 1 }
func --(a : [byref(implicit)] Int64) { a = a - 1 }
func --(a : [byref(implicit)] Int128) { a = a - 1 }
func --(a : [byref(implicit)] Float) { a = a - 1.0 }
func --(a : [byref(implicit)] Double) { a = a - 1.0 }

// Binary Multiplication.
func [infix_left=200] * (lhs : Int8, rhs : Int8) -> Int8 {
  return Int8(Builtin.mul_Int8(lhs.value, rhs.value))
}
func [infix_left=200] * (lhs : UInt8, rhs : UInt8) -> UInt8 {
  return UInt8(Builtin.mul_Int8(lhs.value, rhs.value))
}
func [infix_left=200] * (lhs : Int16, rhs : Int16) -> Int16 {
  return Int16(Builtin.mul_Int16(lhs.value, rhs.value))
}
func [infix_left=200] * (lhs : Int32, rhs : Int32) -> Int32 {
  return Int32(Builtin.mul_Int32(lhs.value, rhs.value))
}
func [infix_left=200] * (lhs : UInt32, rhs : UInt32) -> UInt32 {
  return UInt32(Builtin.mul_Int32(lhs.value, rhs.value))
}
func [infix_left=200] * (lhs : Int64, rhs : Int64) -> Int64 {
  return Int64(Builtin.mul_Int64(lhs.value, rhs.value))
}
func [infix_left=200] * (lhs : Int128, rhs : Int128) -> Int128 {
  return Int128(Builtin.mul_Int128(lhs.value, rhs.value))
}
func [infix_left=200] * (lhs : Float, rhs : Float) -> Float {
  return Float(Builtin.mul_FPIEEE32(lhs.value, rhs.value))
}
func [infix_left=200] * (lhs : Double, rhs : Double) -> Double {
  return Double(Builtin.mul_FPIEEE64(lhs.value, rhs.value))
}


// Binary Division.
func [infix_left=200] / (lhs : Int8, rhs : Int8) -> Int8 {
  return Int8(Builtin.sdiv_Int8(lhs.value, rhs.value))
}
func [infix_left=200] / (lhs : UInt8, rhs : UInt8) -> UInt8 {
  return UInt8(Builtin.udiv_Int8(lhs.value, rhs.value))
}
func [infix_left=200] / (lhs : Int16, rhs : Int16) -> Int16 {
  return Int16(Builtin.sdiv_Int16(lhs.value, rhs.value))
}
func [infix_left=200] / (lhs : Int32, rhs : Int32) -> Int32 {
  return Int32(Builtin.sdiv_Int32(lhs.value, rhs.value))
}
func [infix_left=200] / (lhs : UInt32, rhs : UInt32) -> UInt32 {
  return UInt32(Builtin.udiv_Int32(lhs.value, rhs.value))
}
func [infix_left=200] / (lhs : Int64, rhs : Int64) -> Int64 {
  return Int64(Builtin.sdiv_Int64(lhs.value, rhs.value))
}
func [infix_left=200] / (lhs : Int128, rhs : Int128) -> Int128 {
  return Int128(Builtin.sdiv_Int128(lhs.value, rhs.value))
}
func [infix_left=200] / (lhs : Float, rhs : Float) -> Float {
  return Float(Builtin.fdiv_FPIEEE32(lhs.value, rhs.value))
}
func [infix_left=200] / (lhs: Double, rhs: Double) -> Double {
  return Double(Builtin.fdiv_FPIEEE64(lhs.value, rhs.value))
}

// Binary Remainder.
func [infix_left=200] % (lhs : Int8, rhs : Int8) -> Int8 {
  return Int8(Builtin.srem_Int8(lhs.value, rhs.value))
}
func [infix_left=200] % (lhs : UInt8, rhs : UInt8) -> UInt8 {
  return UInt8(Builtin.urem_Int8(lhs.value, rhs.value))
}
func [infix_left=200] % (lhs : Int16, rhs : Int16) -> Int16 {
  return Int16(Builtin.srem_Int16(lhs.value, rhs.value))
}
func [infix_left=200] % (lhs : Int32, rhs : Int32) -> Int32 {
  return Int32(Builtin.srem_Int32(lhs.value, rhs.value))
}
func [infix_left=200] % (lhs : UInt32, rhs : UInt32) -> UInt32 {
  return UInt32(Builtin.urem_Int32(lhs.value, rhs.value))
}
func [infix_left=200] % (lhs : Int64, rhs : Int64) -> Int64 {
  return Int64(Builtin.srem_Int64(lhs.value, rhs.value))
}
func [infix_left=200] % (lhs : Int128, rhs : Int128) -> Int128 {
  return Int128(Builtin.srem_Int128(lhs.value, rhs.value))
}
// FIXME: Should we support % on floating point types?


// Binary Addition.
func [infix_left=190] + (lhs: Int8, rhs: Int8) -> Int8 {
  return Int8(Builtin.add_Int8(lhs.value, rhs.value))
}
func [infix_left=190] + (lhs: UInt8, rhs: UInt8) -> UInt8 {
  return UInt8(Builtin.add_Int8(lhs.value, rhs.value))
}
func [infix_left=190] + (lhs: Int16, rhs: Int16) -> Int16 {
  return Int16(Builtin.add_Int16(lhs.value, rhs.value))
}
func [infix_left=190] + (lhs: Int32, rhs: Int32) -> Int32 {
  return Int32(Builtin.add_Int32(lhs.value, rhs.value))
}
func [infix_left=190] + (lhs: UInt32, rhs: UInt32) -> UInt32 {
  return UInt32(Builtin.add_Int32(lhs.value, rhs.value))
}
func [infix_left=190] + (lhs: Int64, rhs: Int64) -> Int64 {
  return Int64(Builtin.add_Int64(lhs.value, rhs.value))
}
func [infix_left=190] + (lhs: Int128, rhs: Int128) -> Int128 {
  return Int128(Builtin.add_Int128(lhs.value, rhs.value))
}
func [infix_left=190] + (lhs: Float, rhs: Float) -> Float {
  return Float(Builtin.add_FPIEEE32(lhs.value, rhs.value))
}
func [infix_left=190] + (lhs: Double, rhs: Double) -> Double {
  return Double(Builtin.add_FPIEEE64(lhs.value, rhs.value))
}


// Binary Subtraction.
func [infix_left=190] - (lhs: Int8, rhs: Int8) -> Int8 {
  return Int8(Builtin.sub_Int8(lhs.value, rhs.value))
}
func [infix_left=190] - (lhs: UInt8, rhs: UInt8) -> UInt8 {
  return UInt8(Builtin.sub_Int8(lhs.value, rhs.value))
}
func [infix_left=190] - (lhs: Int16, rhs: Int16) -> Int16 {
  return Int16(Builtin.sub_Int16(lhs.value, rhs.value))
}
func [infix_left=190] - (lhs: Int32, rhs: Int32) -> Int32 {
  return Int32(Builtin.sub_Int32(lhs.value, rhs.value))
}
func [infix_left=190] - (lhs: UInt32, rhs: UInt32) -> UInt32 {
  return UInt32(Builtin.sub_Int32(lhs.value, rhs.value))
}
func [infix_left=190] - (lhs: Int64, rhs: Int64) -> Int64 {
  return Int64(Builtin.sub_Int64(lhs.value, rhs.value))
}
func [infix_left=190] - (lhs: Int128, rhs: Int128) -> Int128 {
  return Int128(Builtin.sub_Int128(lhs.value, rhs.value))
}
func [infix_left=190] - (lhs: Float, rhs: Float) -> Float {
  return Float(Builtin.sub_FPIEEE32(lhs.value, rhs.value))
}
func [infix_left=190] - (lhs: Double, rhs: Double) -> Double {
  return Double(Builtin.sub_FPIEEE64(lhs.value, rhs.value))
}

// Left Shift.
func [infix=180] << (lhs : Int8, rhs : Int8) -> Int8 {
  return Int8(Builtin.shl_Int8(lhs.value, rhs.value))
}
func [infix=180] << (lhs : UInt8, rhs : UInt8) -> UInt8 {
  return UInt8(Builtin.shl_Int8(lhs.value, rhs.value))
}
func [infix=180] << (lhs : Int16, rhs : Int16) -> Int16 {
  return Int16(Builtin.shl_Int16(lhs.value, rhs.value))
}
func [infix=180] << (lhs : Int32, rhs : Int32) -> Int32 {
  return Int32(Builtin.shl_Int32(lhs.value, rhs.value))
}
func [infix=180] << (lhs : UInt32, rhs : UInt32) -> UInt32 {
  return UInt32(Builtin.shl_Int32(lhs.value, rhs.value))
}
func [infix=180] << (lhs : Int64, rhs : Int64) -> Int64 {
  return Int64(Builtin.shl_Int64(lhs.value, rhs.value))
}
func [infix=180] << (lhs : Int128, rhs : Int128) -> Int128 {
  return Int128(Builtin.shl_Int128(lhs.value, rhs.value))
}

// Right Shift.
func [infix=180] >>(lhs : Int8, rhs : Int8) -> Int8 {
  return Int8(Builtin.ashr_Int8(lhs.value, rhs.value))
}
func [infix=180] >>(lhs : UInt8, rhs : UInt8) -> UInt8 {
  return UInt8(Builtin.lshr_Int8(lhs.value, rhs.value))
}
func [infix=180] >>(lhs : Int16, rhs : Int16) -> Int16 {
  return Int16(Builtin.ashr_Int16(lhs.value, rhs.value))
}
func [infix=180] >>(lhs : Int32, rhs : Int32) -> Int32 {
  return Int32(Builtin.ashr_Int32(lhs.value, rhs.value))
}
func [infix=180] >>(lhs : UInt32, rhs : UInt32) -> UInt32 {
  return UInt32(Builtin.lshr_Int32(lhs.value, rhs.value))
}
func [infix=180] >>(lhs : Int64, rhs : Int64) -> Int64 {
  return Int64(Builtin.ashr_Int64(lhs.value, rhs.value))
}
func [infix=180] >>(lhs : Int128, rhs : Int128) -> Int128 {
  return Int128(Builtin.ashr_Int128(lhs.value, rhs.value))
}

// Less-Than Comparison.
func [infix=170] < (lhs : Int8, rhs : Int8) -> Bool {
  return getBool(Builtin.cmp_slt_Int8(lhs.value, rhs.value))
}
func [infix=170] < (lhs : UInt8, rhs : UInt8) -> Bool {
  return getBool(Builtin.cmp_ult_Int8(lhs.value, rhs.value))
}
func [infix=170] < (lhs : Int16, rhs : Int16) -> Bool {
  return getBool(Builtin.cmp_slt_Int16(lhs.value, rhs.value))
}
func [infix=170] < (lhs : Int32, rhs : Int32) -> Bool {
  return getBool(Builtin.cmp_slt_Int32(lhs.value, rhs.value))
}
func [infix=170] < (lhs : UInt32, rhs : UInt32) -> Bool {
  return getBool(Builtin.cmp_ult_Int32(lhs.value, rhs.value))
}
func [infix=170] < (lhs : Int64, rhs : Int64) -> Bool {
  return getBool(Builtin.cmp_slt_Int64(lhs.value, rhs.value))
}
func [infix=170] < (lhs : Int128, rhs : Int128) -> Bool {
  return getBool(Builtin.cmp_slt_Int128(lhs.value, rhs.value))
}
func [infix=170] < (lhs : Float, rhs : Float) -> Bool {
  return getBool(Builtin.fcmp_olt_FPIEEE32(lhs.value, rhs.value))
}
func [infix=170] < (lhs : Double, rhs : Double) -> Bool {
  return getBool(Builtin.fcmp_olt_FPIEEE64(lhs.value, rhs.value))
}

// Greater-Than Comparison.
func [infix=170] > (lhs : Int8, rhs : Int8) -> Bool {
  return getBool(Builtin.cmp_sgt_Int8(lhs.value, rhs.value))
}
func [infix=170] > (lhs : UInt8, rhs : UInt8) -> Bool {
  return getBool(Builtin.cmp_ugt_Int8(lhs.value, rhs.value))
}
func [infix=170] > (lhs : Int16, rhs : Int16) -> Bool {
  return getBool(Builtin.cmp_sgt_Int16(lhs.value, rhs.value))
}
func [infix=170] > (lhs : Int32, rhs : Int32) -> Bool {
  return getBool(Builtin.cmp_sgt_Int32(lhs.value, rhs.value))
}
func [infix=170] > (lhs : UInt32, rhs : UInt32) -> Bool {
  return getBool(Builtin.cmp_ugt_Int32(lhs.value, rhs.value))
}
func [infix=170] > (lhs : Int64, rhs : Int64) -> Bool {
  return getBool(Builtin.cmp_sgt_Int64(lhs.value, rhs.value))
}
func [infix=170] > (lhs : Int128, rhs : Int128) -> Bool {
  return getBool(Builtin.cmp_sgt_Int128(lhs.value, rhs.value))
}
func [infix=170] > (lhs : Float, rhs : Float) -> Bool {
  return getBool(Builtin.fcmp_ogt_FPIEEE32(lhs.value, rhs.value))
}
func [infix=170] > (lhs : Double, rhs : Double) -> Bool {
  return getBool(Builtin.fcmp_ogt_FPIEEE64(lhs.value, rhs.value))
}

// Less-Than-Or-Equal Comparison.
func [infix=170] <= (lhs : Int8, rhs : Int8) -> Bool {
  return getBool(Builtin.cmp_sle_Int8(lhs.value, rhs.value))
}
func [infix=170] <= (lhs : UInt8, rhs : UInt8) -> Bool {
  return getBool(Builtin.cmp_ule_Int8(lhs.value, rhs.value))
}
func [infix=170] <= (lhs : Int16, rhs : Int16) -> Bool {
  return getBool(Builtin.cmp_sle_Int16(lhs.value, rhs.value))
}
func [infix=170] <= (lhs : Int32, rhs : Int32) -> Bool {
  return getBool(Builtin.cmp_sle_Int32(lhs.value, rhs.value))
}
func [infix=170] <= (lhs : UInt32, rhs : UInt32) -> Bool {
  return getBool(Builtin.cmp_ule_Int32(lhs.value, rhs.value))
}
func [infix=170] <= (lhs : Int64, rhs : Int64) -> Bool {
  return getBool(Builtin.cmp_sle_Int64(lhs.value, rhs.value))
}
func [infix=170] <= (lhs : Int128, rhs : Int128) -> Bool {
  return getBool(Builtin.cmp_sle_Int128(lhs.value, rhs.value))
}
func [infix=170] <= (lhs : Float, rhs : Float) -> Bool {
  return getBool(Builtin.fcmp_ole_FPIEEE32(lhs.value, rhs.value))
}
func [infix=170] <= (lhs : Double, rhs : Double) -> Bool {
  return getBool(Builtin.fcmp_ole_FPIEEE64(lhs.value, rhs.value))
}

// Greater-Than-Or-Equal Comparison.
func [infix=170] >= (lhs : Int8, rhs : Int8) -> Bool {
  return getBool(Builtin.cmp_sge_Int8(lhs.value, rhs.value))
}
func [infix=170] >= (lhs : UInt8, rhs : UInt8) -> Bool {
  return getBool(Builtin.cmp_uge_Int8(lhs.value, rhs.value))
}
func [infix=170] >= (lhs : Int16, rhs : Int16) -> Bool {
  return getBool(Builtin.cmp_sge_Int16(lhs.value, rhs.value))
}
func [infix=170] >= (lhs : Int32, rhs : Int32) -> Bool {
  return getBool(Builtin.cmp_sge_Int32(lhs.value, rhs.value))
}
func [infix=170] >= (lhs : UInt32, rhs : UInt32) -> Bool {
  return getBool(Builtin.cmp_uge_Int32(lhs.value, rhs.value))
}
func [infix=170] >= (lhs : Int64, rhs : Int64) -> Bool {
  return getBool(Builtin.cmp_sge_Int64(lhs.value, rhs.value))
}
func [infix=170] >= (lhs : Int128, rhs : Int128) -> Bool {
  return getBool(Builtin.cmp_sge_Int128(lhs.value, rhs.value))
}
func [infix=170] >= (lhs : Float, rhs : Float) -> Bool {
  return getBool(Builtin.fcmp_oge_FPIEEE32(lhs.value, rhs.value))
}
func [infix=170] >= (lhs : Double, rhs : Double) -> Bool {
  return getBool(Builtin.fcmp_oge_FPIEEE64(lhs.value, rhs.value))
}

// Equality Comparison.
func [infix=160] == (lhs : Int8, rhs : Int8) -> Bool {
  return getBool(Builtin.cmp_eq_Int8(lhs.value, rhs.value))
}
func [infix=160] == (lhs : UInt8, rhs : UInt8) -> Bool {
  return getBool(Builtin.cmp_eq_Int8(lhs.value, rhs.value))
}
func [infix=160] == (lhs : Int16, rhs : Int16) -> Bool {
  return getBool(Builtin.cmp_eq_Int16(lhs.value, rhs.value))
}
func [infix=160] == (lhs : Int32, rhs : Int32) -> Bool {
  return getBool(Builtin.cmp_eq_Int32(lhs.value, rhs.value))
}
func [infix=160] == (lhs : UInt32, rhs : UInt32) -> Bool {
  return getBool(Builtin.cmp_eq_Int32(lhs.value, rhs.value))
}
func [infix=160] == (lhs : Int64, rhs : Int64) -> Bool {
  return getBool(Builtin.cmp_eq_Int64(lhs.value, rhs.value))
}
func [infix=160] == (lhs : Int128, rhs : Int128) -> Bool {
  return getBool(Builtin.cmp_eq_Int128(lhs.value, rhs.value))
}
func [infix=160] == (lhs : Float, rhs : Float) -> Bool {
  return getBool(Builtin.fcmp_oeq_FPIEEE32(lhs.value, rhs.value))
}
func [infix=160] == (lhs : Double, rhs : Double) -> Bool {
  return getBool(Builtin.fcmp_oeq_FPIEEE64(lhs.value, rhs.value))
}

// Not-Equality Comparison.
func [infix=160] != (lhs : Int8, rhs : Int8) -> Bool {
  return getBool(Builtin.cmp_ne_Int8(lhs.value, rhs.value))
}
func [infix=160] != (lhs : UInt8, rhs : UInt8) -> Bool {
  return getBool(Builtin.cmp_ne_Int8(lhs.value, rhs.value))
}
func [infix=160] != (lhs : Int16, rhs : Int16) -> Bool {
  return getBool(Builtin.cmp_ne_Int16(lhs.value, rhs.value))
}
func [infix=160] != (lhs : Int32, rhs : Int32) -> Bool {
  return getBool(Builtin.cmp_ne_Int32(lhs.value, rhs.value))
}
func [infix=160] != (lhs : UInt32, rhs : UInt32) -> Bool {
  return getBool(Builtin.cmp_ne_Int32(lhs.value, rhs.value))
}
func [infix=160] != (lhs : Int64, rhs : Int64) -> Bool {
  return getBool(Builtin.cmp_ne_Int64(lhs.value, rhs.value))
}
func [infix=160] != (lhs : Int128, rhs : Int128) -> Bool {
  return getBool(Builtin.cmp_ne_Int128(lhs.value, rhs.value))
}
func [infix=160] != (lhs : Float, rhs : Float) -> Bool {
  return getBool(Builtin.fcmp_une_FPIEEE32(lhs.value, rhs.value))
}
func [infix=160] != (lhs : Double, rhs : Double) -> Bool {
  return getBool(Builtin.fcmp_une_FPIEEE64(lhs.value, rhs.value))
}

// Bitwise 'and'.
func [infix_left=150] & (lhs: Int8, rhs: Int8) -> Int8 {
  return Int8(Builtin.and_Int8(lhs.value, rhs.value))
}
func [infix_left=150] & (lhs: UInt8, rhs: UInt8) -> UInt8 {
  return UInt8(Builtin.and_Int8(lhs.value, rhs.value))
}
func [infix_left=150] & (lhs: Int16, rhs: Int16) -> Int16 {
  return Int16(Builtin.and_Int16(lhs.value, rhs.value))
}
func [infix_left=150] & (lhs: Int32, rhs: Int32) -> Int32 {
  return Int32(Builtin.and_Int32(lhs.value, rhs.value))
}
func [infix_left=150] & (lhs: UInt32, rhs: UInt32) -> UInt32 {
  return UInt32(Builtin.and_Int32(lhs.value, rhs.value))
}
func [infix_left=150] & (lhs: Int64, rhs: Int64) -> Int64 {
  return Int64(Builtin.and_Int64(lhs.value, rhs.value))
}
func [infix_left=150] & (lhs: Int128, rhs: Int128) -> Int128 {
  return Int128(Builtin.and_Int128(lhs.value, rhs.value))
}

func [infix_left=140] ^ (lhs: Int8, rhs: Int8) -> Int8 {
  return Int8(Builtin.xor_Int8(lhs.value, rhs.value))
}
func [infix_left=140] ^ (lhs: UInt8, rhs: UInt8) -> UInt8 {
  return UInt8(Builtin.xor_Int8(lhs.value, rhs.value))
}
func [infix_left=140] ^ (lhs: Int16, rhs: Int16) -> Int16 {
  return Int16(Builtin.xor_Int16(lhs.value, rhs.value))
}
func [infix_left=140] ^ (lhs: Int32, rhs: Int32) -> Int32 {
  return Int32(Builtin.xor_Int32(lhs.value, rhs.value))
}
func [infix_left=140] ^ (lhs: UInt32, rhs: UInt32) -> UInt32 {
  return UInt32(Builtin.xor_Int32(lhs.value, rhs.value))
}
func [infix_left=140] ^ (lhs: Int64, rhs: Int64) -> Int64 {
  return Int64(Builtin.xor_Int64(lhs.value, rhs.value))
}
func [infix_left=140] ^ (lhs: Int128, rhs: Int128) -> Int128 {
  return Int128(Builtin.xor_Int128(lhs.value, rhs.value))
}

func [infix_left=130] | (lhs: Int8, rhs: Int8) -> Int8 {
  return Int8(Builtin.or_Int8(lhs.value, rhs.value))
}
func [infix_left=130] | (lhs: UInt8, rhs: UInt8) -> UInt8 {
  return UInt8(Builtin.or_Int8(lhs.value, rhs.value))
}
func [infix_left=130] | (lhs: Int16, rhs: Int16) -> Int16 {
  return Int16(Builtin.or_Int16(lhs.value, rhs.value))
}
func [infix_left=130] | (lhs: Int32, rhs: Int32) -> Int32 {
  return Int32(Builtin.or_Int32(lhs.value, rhs.value))
}
func [infix_left=130] | (lhs: UInt32, rhs: UInt32) -> UInt32 {
  return UInt32(Builtin.or_Int32(lhs.value, rhs.value))
}
func [infix_left=130] | (lhs: Int64, rhs: Int64) -> Int64 {
  return Int64(Builtin.or_Int64(lhs.value, rhs.value))
}
func [infix_left=130] | (lhs: Int128, rhs: Int128) -> Int128 {
  return Int128(Builtin.or_Int128(lhs.value, rhs.value))
}

// Short circuiting logical operators.
func [infix_left=120] && (lhs: Bool, rhs: [auto_closure] ()->Bool) -> Bool {
  if (lhs) {
    return rhs()
  }
  
  return false
}
func [infix_left=110] || (lhs: Bool, rhs: [auto_closure] ()->Bool) -> Bool {
  if (lhs) {
    return true
  }
  
  return rhs()
}

// In C, 100 is ?:

// In C, 90 is =, *=, += etc.

//===----------------------------------------------------------------------===//
// Char Type
//===----------------------------------------------------------------------===//

// CharLiteralType specifies the default type to use for a character literal
// when the type isn't constrained.
typealias CharacterLiteralType : Char

struct Char { 
  value : Builtin.Int32

  static func convertFromCharacterLiteral(val : Builtin.Int32) -> Char {
    return Char(val)
  }
  
  static func Char(v : UInt32) -> Char {
    // FIXME:  Give Char an invariant that it can only hold valid UTF-32
    return Char(v.value)
  }

  func replPrint() {
    print('\'')
    replPrintCharBody()
    print('\'')
  }
  
  func replPrintCharBody() {
    if (this == '\\') {
      print('\\')
      print('\\')
    } else if (this == '"') {
      print('\\')
      print('"')
    } else if (this == '\'') {
      print('\\')
      print('\'')
    } else if (isPrint()) {
      print(this)
    } else if (this == '\t') {
      print('\\')
      print('t')
    } else if (this == '\n') {
      print('\\')
      print('n')
    } else if (this == '\r') {
      print('\\')
      print('r')
    } else if (UInt32(this) < 128) {
      print('\\')
      print('x')
      _printNibbleAsHex(UInt32(this) >> 4)
      _printNibbleAsHex(UInt32(this))
    } else {
      // FIXME: Implement \U escapes when strings support them.
      print("<unprintable character so far>")
    }
  }
  
    // FIXME: Move to nested function when IRGen supports it.
  static func _printNibbleAsHex(v : UInt32) {
    v = v & 15
    if (v < 10) {
      print(Char(v+48))    // 48 = '0'
    } else {
      print(Char(v-10+65)) // 65 = 'A'
    }
  }
}

extension UInt32 {
  static func UInt32(v : Char) -> UInt32 {
    return UInt32(v.value)
  }
}

func [infix_left=190] - (lhs: Char, rhs: Char) -> Int {
  return Int(Int32(Builtin.sub_Int32(lhs.value, rhs.value)))
}

func [infix_left=190] - (lhs: Char, rhs: Int) -> Char {
  return Char(UInt32(Builtin.sub_Int32(lhs.value, Int32(rhs).value)))
}

func [infix_left=190] + (lhs: Char, rhs: Int) -> Char {
  return Char(UInt32(Builtin.add_Int32(lhs.value, Int32(rhs).value)))
}

func [infix_left=190] + (lhs: Int, rhs: Char) -> Char {
  return Char(UInt32(Builtin.add_Int32(rhs.value, Int32(lhs).value)))
}

func [infix=170] < (lhs : Char, rhs : Char) -> Bool {
  return getBool(Builtin.cmp_ult_Int32(lhs.value, rhs.value))
}

func [infix=170] > (lhs : Char, rhs : Char) -> Bool {
  return getBool(Builtin.cmp_ugt_Int32(lhs.value, rhs.value))
}

func [infix=170] <= (lhs : Char, rhs : Char) -> Bool {
  return getBool(Builtin.cmp_ule_Int32(lhs.value, rhs.value))
}

func [infix=170] >= (lhs : Char, rhs : Char) -> Bool {
  return getBool(Builtin.cmp_uge_Int32(lhs.value, rhs.value))
}

func [infix=160] == (lhs : Char, rhs : Char) -> Bool {
  return getBool(Builtin.cmp_eq_Int32(lhs.value, rhs.value))
}

func [infix=160] != (lhs : Char, rhs : Char) -> Bool {
  return getBool(Builtin.cmp_ne_Int32(lhs.value, rhs.value))
}

extension Char {
  func isPrint() -> Bool {
    return (this >= Char(0o040) && this <= Char(0o077)) ||
           (this >= Char(0o100) && this <= Char(0o176))
  }
}

//===----------------------------------------------------------------------===//
// String Type
//===----------------------------------------------------------------------===//

struct StringCharRange {
  min : Builtin.RawPointer,
  max : Builtin.RawPointer

  func isEmpty() -> Bool { return Int64(Builtin.ptrtoint_Int64(min)) ==
                                  Int64(Builtin.ptrtoint_Int64(max)) }
  func getFirst() -> Char {
    var u32 : UInt32
    var u8  : UInt8
    var p : Builtin.RawPointer
    p = min
    u8 = UInt8(Builtin.load_Int8(p))
    if u8 < 0x80 {
        return Char(UInt32(u8))
    }
    else if u8 < 0xE0 {
      p = Builtin.gep_Int8(p, Int8(1).value)
      var u8_1 : UInt8
      u8_1 = UInt8(Builtin.load_Int8(p))
      return Char((UInt32(u8 & 0x1F) << 6) |
                   UInt32(u8_1 & 0x3F))
    }
    else if u8 < 0xF0 {
      p = Builtin.gep_Int8(p, Int8(1).value)
      var u8_1 : UInt8
      u8_1 = UInt8(Builtin.load_Int8(p))
      p = Builtin.gep_Int8(p, Int8(1).value)
      var u8_2 : UInt8
      u8_2 = UInt8(Builtin.load_Int8(p))
      return Char((UInt32(u8   & 0x0F) << 12) |
                  (UInt32(u8_1 & 0x3F) << 6)  |
                   UInt32(u8_2 & 0x3F))
    }
    p = Builtin.gep_Int8(p, Int8(1).value)
    var u8_1 : UInt8
    u8_1 = UInt8(Builtin.load_Int8(p))
    p = Builtin.gep_Int8(p, Int8(1).value)
    var u8_2 : UInt8
    u8_2 = UInt8(Builtin.load_Int8(p))
    p = Builtin.gep_Int8(p, Int8(1).value)
    var u8_3 : UInt8
    u8_3 = UInt8(Builtin.load_Int8(p))
    return Char((UInt32(u8   & 0x07) << 18) |
                (UInt32(u8_1 & 0x3F) << 12) |
                (UInt32(u8_2 & 0x3F) << 6)  |
                 UInt32(u8_3 & 0x3F))
  }

  func dropFirst() {
    var u8  : UInt8
    u8 = UInt8(Builtin.load_Int8(min))
    if u8 < 0x80 {
      min = Builtin.gep_Int8(min, Int8(1).value)
    }
    else if u8 < 0xE0 {
      min = Builtin.gep_Int8(min, Int8(2).value)
    }
    else if u8 < 0xF0 {
      min = Builtin.gep_Int8(min, Int8(3).value)
    }
    else {
      min = Builtin.gep_Int8(min, Int8(4).value)
    }
  }
}

// FIXME:  How does one write a default constructor in Swift?  The compiler
//   supplied default constructor is breaking the String invariant that the
//   String always contains a valid UTF-8 null terminated string.
//   Instead I've propagated through String that a null value is a valid state
//   (which is wrong).
//   I tried this, but it did not work:
//      static func String() -> String {
//        print("Got Here!\n")
//       }
//   It does not get called.
struct String {
  value : Builtin.RawPointer

  static func convertFromStringLiteral(val : Builtin.RawPointer) -> String {
    return String(val)
  }

  func getElements() -> StringCharRange {
    if Int64(Builtin.ptrtoint_Int64(value)) == 0 {
      return StringCharRange(value, value)
    }
    var Ptr = UnsafePointerInt8(value)
    while Ptr.get() != 0 {
      ++Ptr
    }
    return StringCharRange(value, Ptr.value)
  }

  func size() -> Int {
    var Result = 0
    for i in this {
      ++Result
    }
    return Result
  }
  
  func replPrint() {
    print('"')
    for c in this {
      c.replPrintCharBody()
    }
    print('"')
  }
}

// StringLiteralType specifies the default type to use for an string literal
// when the type isn't constrained.
typealias StringLiteralType : String


// Concatenation.
func [infix_left=190]+(lhs : String, rhs : String) -> String

// Conversions to string from other types.
extension String {
  static func String(v : Int128) -> String
  static func String(v : Double) -> String

  static func String(v : Int8) -> String {
    return String(Int128(v))
  }
  static func String(v : UInt8) -> String {
    return String(Int128(v))
  }
  static func String(v : Int16) -> String {
    return String(Int128(v))
  }
  static func String(v : Int32) -> String {
    return String(Int128(v))
  }
  static func String(v : UInt32) -> String {
    return String(Int128(v))
  }
  static func String(v : Int64) -> String {
    return String(Int128(v))
  }
  static func String(v : Float) -> String {
    return String(Double(v))
  }
  static func String(b : Bool) -> String {
    if b {
      return "true"
    }
    return "false"
  }
}





//===----------------------------------------------------------------------===//
// UnsafePointer<Int> Type
//===----------------------------------------------------------------------===//

/// FIXME: This should eventually be a generic UnsafePointer<T> type.
struct UnsafePointerInt {
  value : Builtin.RawPointer

  func get() -> Int64 {
    return Int64(Builtin.load_Int64(value))
  }

  func set(newvalue : Int64) -> () {
    Builtin.assign_Int64(newvalue.value, value)
  }
}

func [infix_left=190] + (lhs : UnsafePointerInt,
                         rhs : Int64) -> UnsafePointerInt {
  return UnsafePointerInt(Builtin.gep_Int64(lhs.value, (rhs * 8).value))
}

func [infix_left=190] + (lhs : Int64,
                         rhs : UnsafePointerInt) -> UnsafePointerInt {
  return rhs + lhs
}

func [infix_left=190] - (lhs : UnsafePointerInt,
                         rhs : Int64) -> UnsafePointerInt {
  return lhs + -rhs
}

func ++(a : [byref(implicit)] UnsafePointerInt) { a = a + 1 }
func --(a : [byref(implicit)] UnsafePointerInt) { a = a - 1 }

func == (lhs : UnsafePointerInt, rhs : UnsafePointerInt) -> Bool {
  return getBool(Builtin.cmp_eq_RawPointer(lhs.value, rhs.value))
}

func != (lhs : UnsafePointerInt, rhs : UnsafePointerInt) -> Bool {
  return getBool(Builtin.cmp_ne_RawPointer(lhs.value, rhs.value))
}

func < (lhs : UnsafePointerInt, rhs : UnsafePointerInt) -> Bool {
  return getBool(Builtin.cmp_ult_RawPointer(lhs.value, rhs.value))
}

func <= (lhs : UnsafePointerInt, rhs : UnsafePointerInt) -> Bool {
  return getBool(Builtin.cmp_ule_RawPointer(lhs.value, rhs.value))
}

func > (lhs : UnsafePointerInt, rhs : UnsafePointerInt) -> Bool {
  return getBool(Builtin.cmp_ugt_RawPointer(lhs.value, rhs.value))
}

func >= (lhs : UnsafePointerInt, rhs : UnsafePointerInt) -> Bool {
  return getBool(Builtin.cmp_uge_RawPointer(lhs.value, rhs.value))
}

//===----------------------------------------------------------------------===//
// UnsafePointer<Int8> Type
//===----------------------------------------------------------------------===//

/// FIXME: This should eventually be a generic UnsafePointer<T> type.
struct UnsafePointerInt8 {
  value : Builtin.RawPointer

  func get() -> Int8 {
    return Int8(Builtin.load_Int8(value))
  }

  func set(newvalue : Int8) -> () {
    Builtin.assign_Int8(newvalue.value, value)
  }
}

func [infix_left=190] + (lhs : UnsafePointerInt8,
                         rhs : Int64) -> UnsafePointerInt8 {
  return UnsafePointerInt8(Builtin.gep_Int64(lhs.value, rhs.value))
}

func [infix_left=190] + (lhs : Int64,
                         rhs : UnsafePointerInt8) -> UnsafePointerInt8 {
  return rhs + lhs
}
func [infix_left=190] - (lhs : UnsafePointerInt8,
                         rhs : Int64) -> UnsafePointerInt8 {
  return lhs + -rhs
}

func ++(a : [byref(implicit)] UnsafePointerInt8) { a = a + 1 }
func --(a : [byref(implicit)] UnsafePointerInt8) { a = a - 1 }


//===----------------------------------------------------------------------===//
// Slice<Int64> Type
//===----------------------------------------------------------------------===//

/// FIXME: This should eventually be a generic Slice<T> type.
struct SliceInt64 {
   base : UnsafePointerInt,
   length : Int,
   owner : Builtin.ObjectPointer

   static func convertFromHeapArray(base : Builtin.RawPointer,
                                    owner : Builtin.ObjectPointer,
                                    length : Builtin.Int64) -> SliceInt64 {
     return SliceInt64(UnsafePointerInt(base), Int(length), owner)
   }

   subscript (i : Int) -> Int64 {
     get {
       check(i < length, String(i) +
              " is out of range for array slice of length " + String(length))
       return (base + i).get()
     }

     set {
       check(i < length, String(i) +
              " is out of range for array slice of length " + String(length))
       (base + i).set(value)
     }
   }

   // Informal range protocol
   func getElements() -> SliceInt64 { return this }
   func isEmpty() -> Bool { return length == 0 }
   func getFirst() -> Int {
     return base.get()
   }
   func dropFirst() {
     base = base + 1
     length = length - 1
   }
   func replPrint() {
     print("[")
     var first = true
     var total = 0
     for i in this { 
       if first {
         first = false
       } else {
         print(", ")
       }
       i.replPrint()
       total = total + 1
       if (total > 50) {
         print(" ...]")
         return
       }
     }
     print("]")
   }

  // Slicing via subscripting with a range.
  subscript (rng : Range) -> Int[] {
    get {
      check(rng.min <= length && rng.max <= length, "invalid array slice")
      return SliceInt64(base + rng.min, rng.max - rng.min, owner)
    }

    set {
      check(value.length == rng.max - rng.min, "array slice of wrong length")

      // Common case: the elements were updated in place, so we do not have to
      // perform any updates.
      var destStart = base + rng.min
      if value.base == destStart {
        return
      }

      // If the start of the destination slice falls inside the source slice,
      // copy backwards.
      if destStart >= value.base && destStart < value.base + value.length {
        var destEnd = destStart + value.length
        for i in value {
          --destEnd
          destEnd.set(i)
        }

        return
      }

      // Copy the data.
      for i in value {
        destStart.set(i)
        ++destStart
      }
    }
  }

  func each(f : (Int) -> Void) {
    for i in this { f(i) }
  }

  func reduce(val : Int, f : (Int, Int) -> Int) -> Int {
    for i in this { val = f(val, i) }
    return val
  }

  func map(f : (Int) -> Int) -> Int[] {
    var r = new Int[length]
    for i in 0 .. length { r[i] = f(this[i]) }
    return r
  }
}
struct SliceSliceInt64 {
   base : UnsafePointerInt,
   length : Int,
   owner : Builtin.ObjectPointer

   static func convertFromHeapArray(base : Builtin.RawPointer,
                                    owner : Builtin.ObjectPointer,
                                    length : Builtin.Int64) -> SliceSliceInt64 {
     return SliceSliceInt64(UnsafePointerInt(base), Int(length), owner)
   }

   subscript (i : Int) -> SliceInt64 {
     get {
       check(i < length, String(i) +
              " is out of range for array slice of length " + String(length))
       var ptr = base + 3 * i // Each SliceInt64 is 3 64-bit values
       return SliceInt64.convertFromHeapArray(
                Builtin.load_RawPointer(ptr.value),
                Builtin.load_ObjectPointer((ptr + 2).value),
                Builtin.load_Int64((ptr + 1).value))
     }

     set {
       check(i < length, String(i) +
              " is out of range for array slice of length " + String(length))
       var ptr = base + 3 * i // Each SliceInt64 is 3 64-bit values
       Builtin.assign_RawPointer(value.base.value, ptr.value)
       Builtin.assign_Int64(value.length.value, (ptr + 1).value)
       Builtin.assign_ObjectPointer(value.owner, (ptr + 2).value)
     }
   }

   func getElements() -> SliceSliceInt64 { return this }
   func isEmpty() ->Bool { return length == 0 }
   func getFirst() -> SliceInt64 { 
       return SliceInt64.convertFromHeapArray(
                Builtin.load_RawPointer(base.value),
                Builtin.load_ObjectPointer((base + 2).value),
                Builtin.load_Int64((base + 1).value))
   }
   func dropFirst() {
     base = base + 3
     length = length - 1
   }

   func replPrint() {
     var total = 0
     print("[")
     var first = true
     for i in this { 
       if first {
         first = false
       } else {
         print(", ")
       }
       print("[")
       var innerfirst = true
       var x = 0
       // FIXME: Using for-each here causes an IRGen crash
       while x < i.length { 
         if innerfirst {
           innerfirst = false
         } else {
           print(", ")
         }
         i[x].replPrint()
         total = total + 1
         if (total > 50) {
           print("...], ...]")
           return ()
         }
         x = x + 1
       }
       print("]")
     }
     print("]")
   }

  // Slicing via subscripting with a range.
  subscript (rng : Range) -> Int[][] {
    get {
      check(rng.min <= length && rng.max <= length, "invalid array slice")
      return SliceSliceInt64(base + rng.min*3, rng.max - rng.min, owner)
    }

    set {
      check(value.length == rng.max - rng.min, "array slice of wrong length")

      // Common case: the elements were updated in place, so we do not have to
      // perform any updates.
      var destStart = base + rng.min * 3
      if value.base == destStart {
        return
      }

      // If the start of the destination slice falls inside the source slice,
      // copy backwards.
      if destStart >= value.base && destStart < value.base + value.length * 3 {
        var destEnd = destStart + value.length * 3
        for i in value {
          --destEnd
          Builtin.assign_ObjectPointer(i.owner, destEnd.value)
          --destEnd
          Builtin.assign_Int64(i.length.value, destEnd.value)
          --destEnd
          Builtin.assign_RawPointer(i.base.value, destEnd.value)
        }

        return
      }

      // Copy the data.
      for i in value {
        Builtin.assign_RawPointer(i.base.value, destStart.value)
        ++destStart
        Builtin.assign_Int64(i.length.value, destStart.value)
        ++destStart
        Builtin.assign_ObjectPointer(i.owner, destStart.value)
        ++destStart
      }
    }
  }

  func each(f : (Int[]) -> Void) {
    for i in this { f(i) }
  }

  func reduce(val : Int[], f : (Int[], Int[]) -> Int[]) -> Int[] {
    for i in this { val = f(val, i) }
    return val
  }
  
  func map(f : (Int[]) -> Int[]) -> Int[][] {
    var r = new Int[length][]
    for i in 0 .. length { r[i] = f(this[i]) }
    return r
  }
}

struct SliceDouble {
   base : UnsafePointerInt,
   length : Int,
   owner : Builtin.ObjectPointer

   static func convertFromHeapArray(base : Builtin.RawPointer,
                                    owner : Builtin.ObjectPointer,
                                    length : Builtin.Int64) -> SliceDouble {
     return SliceDouble(UnsafePointerInt(base), Int(length), owner)
   }

   subscript (i : Int) -> Double {
     get {
       check(i < length, String(i) +
              " is out of range for array slice of length " + String(length))

       // GROSS.
       return Double(Builtin.load_FPIEEE64((base + i).value))
      }

     set {
       check(i < length, String(i) +
              " is out of range for array slice of length " + String(length))
       Builtin.assign_FPIEEE64(value.value, (base + i).value)
     }
   }

   // Informal range protocol
   func getElements() -> SliceDouble { return this }
   func isEmpty() -> Bool { return length == 0 }
   func getFirst() -> Double {
     return this[0]
   }
   func dropFirst() {
     base = base + 1
     length = length - 1
   }
   func replPrint() {
     print("[")
     var first = true
     var total = 0
     for i in this { 
       if first {
         first = false
       } else {
         print(", ")
       }
       i.replPrint()
       total = total + 1
       if (total > 50) {
         print(" ...]")
         return
       }
     }
     print("]")
   }
   
  // TODO: Support range slicing ala, a[x..y] = a[a..b]

  func each(f : (Double) -> Void) {
    for i in this { f(i) }
  }

  func reduce(val : Double, f : (Double, Double) -> Double) -> Double {
    for i in this { val = f(val, i) }
    return val
  }

  func map(f : (Double) -> Double) -> Double[] {
    var r = new Double[length]
    for i in 0 .. length { r[i] = f(this[i]) }
    return r
  }
}

//===----------------------------------------------------------------------===//
// Range Type
//===----------------------------------------------------------------------===//

struct Range {
  min : Int,
  max : Int

  // FIXME: each/reduce should be moved out to generic methods, or methods that
  // take the range as a protocol'd "enumeration/iterator" value.
  func each(f : (Int) -> Void) {
    for i in this { f(i) }
  }

  func reduce(val : Int, f : (Int, Int) -> Int) -> Int {
    for i in this { val = f(val, i) }
    return val
  }

  func isEmpty() -> Bool { return min >= max }
  func getFirst() -> Int { return min }
  func dropFirst() { ++min }

  func getElements() -> Range { return this }

  func replPrint() {
    print(min)
    print("..")
    print(max)
  }
}

func [infix_left=110] .. (min : Int, max : Int) -> Range {
  return Range(min, max)
}

struct DoubleRange {
  min : Double,
  max : Double,
  stride : Double

  // FIXME: each/reduce should be moved out to generic methods, or methods that
  // take the range as a protocol'd "enumeration/iterator" value.
  func each(f : (Double) -> Void) {
    for i in this { f(i) }
  }

  func reduce(val : Double, f : (Double, Double) -> Double) -> Double {
    for i in this { val = f(val, i) }
    return val
  }
  
  // by - To be used as (0.0 .. 10.0).by(.25)
  func by(s : Double) -> DoubleRange {
    var result = this
    result.stride = s
    return result
  }

  func isEmpty() -> Bool { return min >= max }
  func getFirst() -> Double { return min }
  func dropFirst() { min = min + stride }

  func getElements() -> DoubleRange { return this }

  func replPrint() {
    print(min)
    print("..")
    print(max)

    if stride != 1.0 {
      print(" by ")
      print(stride)
    }
  }

}

func [infix_left=110] .. (min : Double, max : Double) -> DoubleRange {
  return DoubleRange(min, max, 1.0)
}


//===----------------------------------------------------------------------===//
// Misc utilities
//===----------------------------------------------------------------------===//

func swap(a : [byref] Int64, b : [byref] Int64) {
  var c = a
  a = b
  b = c
}

func assert(fn : [auto_closure] () -> Bool) {
  if (!fn()) {
    abort()
  }
}

func check(fn : [auto_closure] () -> Bool, msg : [auto_closure] () -> String) {
  if (!fn()) {
    println(msg())
    exit(1)
  }
}

// Some very basic output functions.
func print(val : Int)
func print(val : Double)
func print(val : String) {
  if Int64(Builtin.ptrtoint_Int64(val.value)) == 0 {
    return
  }
  var Ptr = UnsafePointerInt8(val.value)
  for ; Ptr.get() != 0; ++Ptr {
    c_putchar(Int32(UInt8(Ptr.get())))
  }
}
func print(val : Char) {
  var wc = UInt32(val)
  if (wc < 0x000080) {
    c_putchar(Int32(wc))
    return
  } else if (wc < 0x000800) {
    c_putchar(Int32(0xC0 | (wc >> 6)))
    c_putchar(Int32(0x80 | (wc & 0x03F)))
    return
  } else if (wc < 0x010000) {
    if (!(0x00D800 <= wc && wc < 0x00E000)) {
      c_putchar(Int32(0xE0 |  (wc >> 12)))
      c_putchar(Int32(0x80 | ((wc & 0x0FC0) >> 6)))
      c_putchar(Int32(0x80 |  (wc & 0x003F)))
      return
    }
  } else if (wc < 0x110000) {
    c_putchar(Int32(0xF0 |  (wc >> 18)))
    c_putchar(Int32(0x80 | ((wc & 0x03F000) >> 12)))
    c_putchar(Int32(0x80 | ((wc & 0x000FC0) >> 6)))
    c_putchar(Int32(0x80 |  (wc & 0x00003F)))
    return
  }
  print(Char(0xFFFD))
}

// Some derived output functions.
func println(val : Int) {
  print(val)
  print(Char(10))
}
func println(val : Double) {
  print(val)
  print(Char(10))
}
func println(val : String) {
  print(val)
  print(Char(10))
}

//===----------------------------------------------------------------------===//
// Extern C functions
//===----------------------------------------------------------------------===//

// FIXME: Once we have an FFI interface, make these have proper function bodies

// The C "exit" and "abort" functions
func [asmname="exit"] exit(exitCode : Int32)
func [asmname="abort"] abort()

func [asmname="putchar"] c_putchar(val : Int32)

// Some math stuff.
func [asmname="sqrtf"] sqrt(a : Float) -> Float
func [asmname="sqrt"] sqrt(a : Double) -> Double
func [asmname="sinf"] sin(a : Float) -> Float
func [asmname="sin"] sin(a : Double) -> Double
func [asmname="cosf"] cos(a : Float) -> Float
func [asmname="cos"] cos(a : Double) -> Double
func [asmname="atan2f"] atan2(y : Float, x : Float) -> Float
func [asmname="atan2"] atan2(y : Double, x : Double) -> Double


