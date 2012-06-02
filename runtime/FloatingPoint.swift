struct Float {
  var value : Builtin.FPIEEE32

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
  var value : Builtin.FPIEEE64 
  
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
// Explicit conversions between types.
//===----------------------------------------------------------------------===//

extension Int8 {
  static func Int8(v : Float) -> Int8 {
    return Int8(Builtin.fptosi_FPIEEE32_Int8(v.value))
  }
  static func Int8(v : Double) -> Int8 {
    return Int8(Builtin.fptosi_FPIEEE64_Int8(v.value))
  }
}

extension UInt8 {
  static func UInt8(v : Float) -> UInt8 {
    return UInt8(Builtin.fptoui_FPIEEE32_Int8(v.value))
  }
  static func UInt8(v : Double) -> UInt8 {
    return UInt8(Builtin.fptoui_FPIEEE64_Int8(v.value))
  }
}

extension Int16 {
  static func Int16(v : Float) -> Int16 {
    return Int16(Builtin.fptosi_FPIEEE32_Int16(v.value))
  }
  static func Int16(v : Double) -> Int16 {
    return Int16(Builtin.fptosi_FPIEEE64_Int16(v.value))
  }
}

extension Int32 {
  static func Int32(v : Float) -> Int32 {
    return Int32(Builtin.fptosi_FPIEEE32_Int32(v.value))
  }
  static func Int32(v : Double) -> Int32 {
    return Int32(Builtin.fptosi_FPIEEE64_Int32(v.value))
  }
}

extension UInt32 {
  static func UInt32(v : Float) -> UInt32 {
    return UInt32(Builtin.fptoui_FPIEEE32_Int32(v.value))
  }
  static func UInt32(v : Double) -> UInt32 {
    return UInt32(Builtin.fptoui_FPIEEE64_Int32(v.value))
  }
}

extension Int64 {
  static func Int64(v : Float) -> Int64 {
    return Int64(Builtin.fptosi_FPIEEE32_Int64(v.value))
  }
  static func Int64(v : Double) -> Int64 {
    return Int64(Builtin.fptosi_FPIEEE64_Int64(v.value))
  }
}

extension UInt64 {
  static func UInt64(v : Float) -> UInt64 {
    return UInt64(Builtin.fptosi_FPIEEE32_Int64(v.value))
  }
  static func UInt64(v : Double) -> UInt64 {
    return UInt64(Builtin.fptosi_FPIEEE64_Int64(v.value))
  }
}

extension Int128 {
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
  static func Float(v : UInt16) -> Float {
    return Float(Builtin.uitofp_Int16_FPIEEE32(v.value))
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
  static func Double(v : UInt16) -> Double {
    return Double(Builtin.uitofp_Int16_FPIEEE64(v.value))
  }
  static func Double(v : Int32) -> Double {
    return Double(Builtin.sitofp_Int32_FPIEEE64(v.value))
  }
  static func Double(v : UInt32) -> Double {
    return Double(Builtin.uitofp_Int32_FPIEEE64(v.value))
  }
  static func Double(v : UInt64) -> Double {
    return Double(Builtin.uitofp_Int64_FPIEEE64(v.value))
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


// Slice<Double>
struct SliceDouble {
   var base : UnsafePointerInt,
   length : Int,
   owner : Builtin.ObjectPointer

   static func convertFromHeapArray(base : Builtin.RawPointer,
                                    owner : Builtin.ObjectPointer,
                                    length : Builtin.Int64) -> SliceDouble {
     return SliceDouble(UnsafePointerInt(base), Int(length), owner)
   }

   subscript (i : Int) -> Double {
     get {
       if i >= length {
         Builtin.trap()
       }

       // GROSS.
       return Double(Builtin.load_FPIEEE64((base + i).value))
      }

     set {
       if i >= length {
         Builtin.trap()
       }

       Builtin.assign_FPIEEE64(value.value, (base + i).value)
     }
   }

   func getElements() -> SliceDouble { return this }

   func replPrint() {
     print('[')
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
       if total > 50 {
         print(" ...]")
         return
       }
     }
     print(']')
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

extension SliceDouble : Range {
  typealias Element = Double
  func isEmpty() -> Bool { return length == 0 }
  func getFirstAndAdvance() -> Double {
    var prev = this[0]
    base = base + 1
    length = length - 1
    return prev
  }
}
