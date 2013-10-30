
//// Automatically Generated From FloatingPoint.gyb.  Do Not Edit Directly ////
//// To regenerate:                                                        ////
////     ../../utils/gyb FloatingPoint.gyb -DwithExtendedPrecision=1 -o FloatAll.swift    ////
////     ../../utils/gyb FloatingPoint.gyb -DwithExtendedPrecision=0 -o FloatDouble.swift ////


struct Float32 : ReplPrintable {
  var value: Builtin.FPIEEE32

  @transparent
  init() {
    var zero: Int64 = 0
    value = Builtin.uitofp_Int64_FPIEEE32(zero.value)
  }

  @transparent
  init(v: Builtin.FPIEEE32) {
    value = v
  }

  func replPrint() {
    print(Double(self))
  }
}

@transparent
extension Float32 : BuiltinIntegerLiteralConvertible, IntegerLiteralConvertible {
  static func _convertFromBuiltinIntegerLiteral(val: Builtin.Int2048) -> Float32 {
    return Float32(Builtin.itofp_with_overflow_Int2048_FPIEEE32(val))
  }

  static func convertFromIntegerLiteral(value: Int64) -> Float32 {
    return Float32(Builtin.uitofp_Int64_FPIEEE32(value.value))
  }
}

@transparent
extension Float32 : BuiltinFloatLiteralConvertible {
  static func _convertFromBuiltinFloatLiteral(value: Builtin.FPIEEE64) -> Float32 {
    return Float32(Builtin.fptrunc_FPIEEE64_FPIEEE32(value))
  }
}

@transparent
extension Float32 : FloatLiteralConvertible {
  static func convertFromFloatLiteral(value: Float32) -> Float32 {
    return value
  }
}

@transparent
extension Float32 : Comparable {
  func __equal__(rhs: Float32) -> Bool {
    return _getBool(Builtin.fcmp_oeq_FPIEEE32(value, rhs.value))
  }
  func __less__(rhs: Float32) -> Bool {
    return _getBool(Builtin.fcmp_olt_FPIEEE32(value, rhs.value))
  }
}
extension Float32 : Hashable {
  func hashValue() -> Int {
    return Int(Int32(Builtin.bitcast_FPIEEE32_Int32(value)))
  }
}

extension Float32 : SignedNumber {
  static func negate(rhs: Float32) -> (Float32, Bool) {
    return (Float32(Builtin.fneg_FPIEEE32(rhs.value)), false)
  }
  func isNegative() -> Bool { return self < 0 }
}

//===----------------------------------------------------------------------===//
// Explicit conversions between types.
//===----------------------------------------------------------------------===//

// Construction from integers.
@transparent
extension Float32 {
  init(v: UInt8) {
    value = Builtin.uitofp_Int8_FPIEEE32(v.value)
  }
  init(v: Int8) {
    value = Builtin.sitofp_Int8_FPIEEE32(v.value)
  }
  init(v: UInt16) {
    value = Builtin.uitofp_Int16_FPIEEE32(v.value)
  }
  init(v: Int16) {
    value = Builtin.sitofp_Int16_FPIEEE32(v.value)
  }
  init(v: UInt32) {
    value = Builtin.uitofp_Int32_FPIEEE32(v.value)
  }
  init(v: Int32) {
    value = Builtin.sitofp_Int32_FPIEEE32(v.value)
  }
  init(v: UInt64) {
    value = Builtin.uitofp_Int64_FPIEEE32(v.value)
  }
  init(v: Int64) {
    value = Builtin.sitofp_Int64_FPIEEE32(v.value)
  }
}

// Construction from other floating point numbers.
@transparent
extension Float32 {
  init(v: Float64) {
  value = Builtin.fptrunc_FPIEEE64_FPIEEE32(v.value)
  }
  init(v: Float80) {
  value = Builtin.fptrunc_FPIEEE80_FPIEEE32(v.value)
  }
}

//===----------------------------------------------------------------------===//
// Standard Operator Table
//===----------------------------------------------------------------------===//

// Unary plus
@transparent @prefix
func +(rhs: Float32) -> Float32 { return rhs }

@transparent
func + (lhs: Float32, rhs: Float32) -> Float32 {
  return Float32(Builtin.fadd_FPIEEE32(lhs.value, rhs.value))
}
@transparent
func - (lhs: Float32, rhs: Float32) -> Float32 {
  return Float32(Builtin.fsub_FPIEEE32(lhs.value, rhs.value))
}
@transparent
func * (lhs: Float32, rhs: Float32) -> Float32 {
  return Float32(Builtin.fmul_FPIEEE32(lhs.value, rhs.value))
}
@transparent
func / (lhs: Float32, rhs: Float32) -> Float32 {
  return Float32(Builtin.fdiv_FPIEEE32(lhs.value, rhs.value))
}

// Binary Remainder.
// The sign of the result matches the sign of the dividend.
// 1) This is consistent with '%' in C#, D, Java, and JavaScript
// 2) C99 requires this behavior for fmod*()
// 3) C++11 requires this behavior for std::fmod*()
@asmname="fmodf"
func % (lhs: Float32, rhs: Float32) -> Float32

// See Bool.swift for && and ||
// In C, 120 is &&
// In C, 110 is ||

// In C, 100 is ?:
// In C, 90 is =, *=, += etc.

@transparent @assignment
func += (lhs: @inout Float32, rhs: Float32) { lhs = lhs + rhs }
@transparent @assignment
func -= (lhs: @inout Float32, rhs: Float32) { lhs = lhs - rhs }
@transparent @assignment
func *= (lhs: @inout Float32, rhs: Float32) { lhs = lhs * rhs }
@transparent @assignment
func /= (lhs: @inout Float32, rhs: Float32) { lhs = lhs / rhs }
@transparent @assignment
func %= (lhs: @inout Float32, rhs: Float32) { lhs = lhs % rhs }

@asmname="sinf"   func  sin(x: Float32) -> Float32
@asmname="cosf"   func  cos(x: Float32) -> Float32
@asmname="tanf"   func  tan(x: Float32) -> Float32
@asmname="atanf"  func atan(x: Float32) -> Float32
@asmname="atan2f" func atan(y: Float32, x: Float32) -> Float32
@asmname="sqrtf"  func sqrt(x: Float32) -> Float32


struct Float64 : ReplPrintable {
  var value: Builtin.FPIEEE64

  @transparent
  init() {
    var zero: Int64 = 0
    value = Builtin.uitofp_Int64_FPIEEE64(zero.value)
  }

  @transparent
  init(v: Builtin.FPIEEE64) {
    value = v
  }

  func replPrint() {
    print(Double(self))
  }
}

@transparent
extension Float64 : BuiltinIntegerLiteralConvertible, IntegerLiteralConvertible {
  static func _convertFromBuiltinIntegerLiteral(val: Builtin.Int2048) -> Float64 {
    return Float64(Builtin.itofp_with_overflow_Int2048_FPIEEE64(val))
  }

  static func convertFromIntegerLiteral(value: Int64) -> Float64 {
    return Float64(Builtin.uitofp_Int64_FPIEEE64(value.value))
  }
}

@transparent
extension Float64 : BuiltinFloatLiteralConvertible {
  static func _convertFromBuiltinFloatLiteral(value: Builtin.FPIEEE64) -> Float64 {
    return Float64(value)
  }
}

@transparent
extension Float64 : FloatLiteralConvertible {
  static func convertFromFloatLiteral(value: Float64) -> Float64 {
    return value
  }
}

@transparent
extension Float64 : Comparable {
  func __equal__(rhs: Float64) -> Bool {
    return _getBool(Builtin.fcmp_oeq_FPIEEE64(value, rhs.value))
  }
  func __less__(rhs: Float64) -> Bool {
    return _getBool(Builtin.fcmp_olt_FPIEEE64(value, rhs.value))
  }
}
extension Float64 : Hashable {
  func hashValue() -> Int {
    return Int(Int64(Builtin.bitcast_FPIEEE64_Int64(value)))
  }
}

extension Float64 : SignedNumber {
  static func negate(rhs: Float64) -> (Float64, Bool) {
    return (Float64(Builtin.fneg_FPIEEE64(rhs.value)), false)
  }
  func isNegative() -> Bool { return self < 0 }
}

//===----------------------------------------------------------------------===//
// Explicit conversions between types.
//===----------------------------------------------------------------------===//

// Construction from integers.
@transparent
extension Float64 {
  init(v: UInt8) {
    value = Builtin.uitofp_Int8_FPIEEE64(v.value)
  }
  init(v: Int8) {
    value = Builtin.sitofp_Int8_FPIEEE64(v.value)
  }
  init(v: UInt16) {
    value = Builtin.uitofp_Int16_FPIEEE64(v.value)
  }
  init(v: Int16) {
    value = Builtin.sitofp_Int16_FPIEEE64(v.value)
  }
  init(v: UInt32) {
    value = Builtin.uitofp_Int32_FPIEEE64(v.value)
  }
  init(v: Int32) {
    value = Builtin.sitofp_Int32_FPIEEE64(v.value)
  }
  init(v: UInt64) {
    value = Builtin.uitofp_Int64_FPIEEE64(v.value)
  }
  init(v: Int64) {
    value = Builtin.sitofp_Int64_FPIEEE64(v.value)
  }
}

// Construction from other floating point numbers.
@transparent
extension Float64 {
  init(v: Float32) {
  value = Builtin.fpext_FPIEEE32_FPIEEE64(v.value)
  }
  init(v: Float80) {
  value = Builtin.fptrunc_FPIEEE80_FPIEEE64(v.value)
  }
}

//===----------------------------------------------------------------------===//
// Standard Operator Table
//===----------------------------------------------------------------------===//

// Unary plus
@transparent @prefix
func +(rhs: Float64) -> Float64 { return rhs }

@transparent
func + (lhs: Float64, rhs: Float64) -> Float64 {
  return Float64(Builtin.fadd_FPIEEE64(lhs.value, rhs.value))
}
@transparent
func - (lhs: Float64, rhs: Float64) -> Float64 {
  return Float64(Builtin.fsub_FPIEEE64(lhs.value, rhs.value))
}
@transparent
func * (lhs: Float64, rhs: Float64) -> Float64 {
  return Float64(Builtin.fmul_FPIEEE64(lhs.value, rhs.value))
}
@transparent
func / (lhs: Float64, rhs: Float64) -> Float64 {
  return Float64(Builtin.fdiv_FPIEEE64(lhs.value, rhs.value))
}

// Binary Remainder.
// The sign of the result matches the sign of the dividend.
// 1) This is consistent with '%' in C#, D, Java, and JavaScript
// 2) C99 requires this behavior for fmod*()
// 3) C++11 requires this behavior for std::fmod*()
@asmname="fmod"
func % (lhs: Float64, rhs: Float64) -> Float64

// See Bool.swift for && and ||
// In C, 120 is &&
// In C, 110 is ||

// In C, 100 is ?:
// In C, 90 is =, *=, += etc.

@transparent @assignment
func += (lhs: @inout Float64, rhs: Float64) { lhs = lhs + rhs }
@transparent @assignment
func -= (lhs: @inout Float64, rhs: Float64) { lhs = lhs - rhs }
@transparent @assignment
func *= (lhs: @inout Float64, rhs: Float64) { lhs = lhs * rhs }
@transparent @assignment
func /= (lhs: @inout Float64, rhs: Float64) { lhs = lhs / rhs }
@transparent @assignment
func %= (lhs: @inout Float64, rhs: Float64) { lhs = lhs % rhs }

@asmname="sin"   func  sin(x: Float64) -> Float64
@asmname="cos"   func  cos(x: Float64) -> Float64
@asmname="tan"   func  tan(x: Float64) -> Float64
@asmname="atan"  func atan(x: Float64) -> Float64
@asmname="atan2" func atan(y: Float64, x: Float64) -> Float64
@asmname="sqrt"  func sqrt(x: Float64) -> Float64


struct Float80 : ReplPrintable {
  var value: Builtin.FPIEEE80

  @transparent
  init() {
    var zero: Int64 = 0
    value = Builtin.uitofp_Int64_FPIEEE80(zero.value)
  }

  @transparent
  init(v: Builtin.FPIEEE80) {
    value = v
  }

  func replPrint() {
    print(Double(self))
  }
}

@transparent
extension Float80 : BuiltinIntegerLiteralConvertible, IntegerLiteralConvertible {
  static func _convertFromBuiltinIntegerLiteral(val: Builtin.Int2048) -> Float80 {
    return Float80(Builtin.itofp_with_overflow_Int2048_FPIEEE80(val))
  }

  static func convertFromIntegerLiteral(value: Int64) -> Float80 {
    return Float80(Builtin.uitofp_Int64_FPIEEE80(value.value))
  }
}

@transparent
extension Float80 : BuiltinFloatLiteralConvertible {
  static func _convertFromBuiltinFloatLiteral(value: Builtin.FPIEEE64) -> Float80 {
    // FIXME: This is actually losing precision <rdar://problem/14073102>.
    return Float80(Builtin.fpext_FPIEEE64_FPIEEE80(value))
  }
}

@transparent
extension Float80 : FloatLiteralConvertible {
  static func convertFromFloatLiteral(value: Float80) -> Float80 {
    return value
  }
}

@transparent
extension Float80 : Comparable {
  func __equal__(rhs: Float80) -> Bool {
    return _getBool(Builtin.fcmp_oeq_FPIEEE80(value, rhs.value))
  }
  func __less__(rhs: Float80) -> Bool {
    return _getBool(Builtin.fcmp_olt_FPIEEE80(value, rhs.value))
  }
}
extension Float80 : Hashable {
  func hashValue() -> Int {
    var asBuiltinInt = Builtin.bitcast_FPIEEE80_Int80(value)
    return Int(Builtin.trunc_Int80_Int64(asBuiltinInt))
  }
}

extension Float80 : SignedNumber {
  static func negate(rhs: Float80) -> (Float80, Bool) {
    return (Float80(Builtin.fneg_FPIEEE80(rhs.value)), false)
  }
  func isNegative() -> Bool { return self < 0 }
}

//===----------------------------------------------------------------------===//
// Explicit conversions between types.
//===----------------------------------------------------------------------===//

// Construction from integers.
@transparent
extension Float80 {
  init(v: UInt8) {
    value = Builtin.uitofp_Int8_FPIEEE80(v.value)
  }
  init(v: Int8) {
    value = Builtin.sitofp_Int8_FPIEEE80(v.value)
  }
  init(v: UInt16) {
    value = Builtin.uitofp_Int16_FPIEEE80(v.value)
  }
  init(v: Int16) {
    value = Builtin.sitofp_Int16_FPIEEE80(v.value)
  }
  init(v: UInt32) {
    value = Builtin.uitofp_Int32_FPIEEE80(v.value)
  }
  init(v: Int32) {
    value = Builtin.sitofp_Int32_FPIEEE80(v.value)
  }
  init(v: UInt64) {
    value = Builtin.uitofp_Int64_FPIEEE80(v.value)
  }
  init(v: Int64) {
    value = Builtin.sitofp_Int64_FPIEEE80(v.value)
  }
}

// Construction from other floating point numbers.
@transparent
extension Float80 {
  init(v: Float32) {
  value = Builtin.fpext_FPIEEE32_FPIEEE80(v.value)
  }
  init(v: Float64) {
  value = Builtin.fpext_FPIEEE64_FPIEEE80(v.value)
  }
}

//===----------------------------------------------------------------------===//
// Standard Operator Table
//===----------------------------------------------------------------------===//

// Unary plus
@transparent @prefix
func +(rhs: Float80) -> Float80 { return rhs }

@transparent
func + (lhs: Float80, rhs: Float80) -> Float80 {
  return Float80(Builtin.fadd_FPIEEE80(lhs.value, rhs.value))
}
@transparent
func - (lhs: Float80, rhs: Float80) -> Float80 {
  return Float80(Builtin.fsub_FPIEEE80(lhs.value, rhs.value))
}
@transparent
func * (lhs: Float80, rhs: Float80) -> Float80 {
  return Float80(Builtin.fmul_FPIEEE80(lhs.value, rhs.value))
}
@transparent
func / (lhs: Float80, rhs: Float80) -> Float80 {
  return Float80(Builtin.fdiv_FPIEEE80(lhs.value, rhs.value))
}

// Binary Remainder.
// The sign of the result matches the sign of the dividend.
// 1) This is consistent with '%' in C#, D, Java, and JavaScript
// 2) C99 requires this behavior for fmod*()
// 3) C++11 requires this behavior for std::fmod*()
@asmname="fmodl"
func % (lhs: Float80, rhs: Float80) -> Float80

// See Bool.swift for && and ||
// In C, 120 is &&
// In C, 110 is ||

// In C, 100 is ?:
// In C, 90 is =, *=, += etc.

@transparent @assignment
func += (lhs: @inout Float80, rhs: Float80) { lhs = lhs + rhs }
@transparent @assignment
func -= (lhs: @inout Float80, rhs: Float80) { lhs = lhs - rhs }
@transparent @assignment
func *= (lhs: @inout Float80, rhs: Float80) { lhs = lhs * rhs }
@transparent @assignment
func /= (lhs: @inout Float80, rhs: Float80) { lhs = lhs / rhs }
@transparent @assignment
func %= (lhs: @inout Float80, rhs: Float80) { lhs = lhs % rhs }

@asmname="sinl"   func  sin(x: Float80) -> Float80
@asmname="cosl"   func  cos(x: Float80) -> Float80
@asmname="tanl"   func  tan(x: Float80) -> Float80
@asmname="atanl"  func atan(x: Float80) -> Float80
@asmname="atan2l" func atan(y: Float80, x: Float80) -> Float80
@asmname="sqrtl"  func sqrt(x: Float80) -> Float80


// Construction of integers from floating point numbers.
@transparent
extension UInt8 {
  init(v: Float32) {
    value = Builtin.fptoui_FPIEEE32_Int8(v.value)
  }
  init(v: Float64) {
    value = Builtin.fptoui_FPIEEE64_Int8(v.value)
  }
  init(v: Float80) {
    value = Builtin.fptoui_FPIEEE80_Int8(v.value)
  }
}
@transparent
extension Int8 {
  init(v: Float32) {
    value = Builtin.fptosi_FPIEEE32_Int8(v.value)
  }
  init(v: Float64) {
    value = Builtin.fptosi_FPIEEE64_Int8(v.value)
  }
  init(v: Float80) {
    value = Builtin.fptosi_FPIEEE80_Int8(v.value)
  }
}
@transparent
extension UInt16 {
  init(v: Float32) {
    value = Builtin.fptoui_FPIEEE32_Int16(v.value)
  }
  init(v: Float64) {
    value = Builtin.fptoui_FPIEEE64_Int16(v.value)
  }
  init(v: Float80) {
    value = Builtin.fptoui_FPIEEE80_Int16(v.value)
  }
}
@transparent
extension Int16 {
  init(v: Float32) {
    value = Builtin.fptosi_FPIEEE32_Int16(v.value)
  }
  init(v: Float64) {
    value = Builtin.fptosi_FPIEEE64_Int16(v.value)
  }
  init(v: Float80) {
    value = Builtin.fptosi_FPIEEE80_Int16(v.value)
  }
}
@transparent
extension UInt32 {
  init(v: Float32) {
    value = Builtin.fptoui_FPIEEE32_Int32(v.value)
  }
  init(v: Float64) {
    value = Builtin.fptoui_FPIEEE64_Int32(v.value)
  }
  init(v: Float80) {
    value = Builtin.fptoui_FPIEEE80_Int32(v.value)
  }
}
@transparent
extension Int32 {
  init(v: Float32) {
    value = Builtin.fptosi_FPIEEE32_Int32(v.value)
  }
  init(v: Float64) {
    value = Builtin.fptosi_FPIEEE64_Int32(v.value)
  }
  init(v: Float80) {
    value = Builtin.fptosi_FPIEEE80_Int32(v.value)
  }
}
@transparent
extension UInt64 {
  init(v: Float32) {
    value = Builtin.fptoui_FPIEEE32_Int64(v.value)
  }
  init(v: Float64) {
    value = Builtin.fptoui_FPIEEE64_Int64(v.value)
  }
  init(v: Float80) {
    value = Builtin.fptoui_FPIEEE80_Int64(v.value)
  }
}
@transparent
extension Int64 {
  init(v: Float32) {
    value = Builtin.fptosi_FPIEEE32_Int64(v.value)
  }
  init(v: Float64) {
    value = Builtin.fptosi_FPIEEE64_Int64(v.value)
  }
  init(v: Float80) {
    value = Builtin.fptosi_FPIEEE80_Int64(v.value)
  }
}


