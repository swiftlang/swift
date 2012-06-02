// RUN: %swift %s -verify -parse-as-library

//===----------------------------------------------------------------------===//
// Swift Standard Prolog Library.
//===----------------------------------------------------------------------===//

import Builtin

// Void is just a type alias for the empty tuple.
typealias Void = ()

//===----------------------------------------------------------------------===//
// Standard Arithmetic Types
//===----------------------------------------------------------------------===//

struct Int8 {
  var value : Builtin.Int8
  
  static func convertFromIntegerLiteral(val : Builtin.Int8) -> Int8 {
    return Int8(val)
  }

  func replPrint() {
    print(Int64(this))
  }
  // FIXME:
  // static var max : Int8 { get { return 0x7F } }
  // static var min : Int8 { get { return -0x7F-1 } }
  static func max() -> Int8 { return 0x7F }
  static func min() -> Int8 { return -0x7F-1 }
}

extension Int8 : FormattedPrintable {
  func printFormatted(kind : Char, layout : String) {
    Int64(this).printFormatted(kind, layout)
  }
}

struct UInt8 {
  var value : Builtin.Int8
  
  static func convertFromIntegerLiteral(val : Builtin.Int8) -> UInt8 {
    return UInt8(val)
  }

  func replPrint() {
    print(UInt64(this))
  }
  // FIXME:
  // static var max : UInt8 { get { return 0xFF } }
  // static var min : UInt8 { get { return 0 } }
  static func max() -> UInt8 { return 0xFF }
  static func min() -> UInt8 { return 0 }
}

extension UInt8 : FormattedPrintable {
  func printFormatted(kind : Char, layout : String) {
    UInt64(this).printFormatted(kind, layout)
  }
}

struct Int16 {
  var value : Builtin.Int16

  static func convertFromIntegerLiteral(val : Builtin.Int16) -> Int16 {
    return Int16(val)
  }

  func replPrint() {
    print(Int64(this))
  }
  // FIXME:
  // static var max : Int16 { get { return 0x7FFF } }
  // static var min : Int16 { get { return -0x7FFF-1 } }
  static func max() -> Int16 { return 0x7FFF }
  static func min() -> Int16 { return -0x7FFF-1 }
}

extension Int16 : FormattedPrintable {
  func printFormatted(kind : Char, layout : String) {
    UInt64(this).printFormatted(kind, layout)
  }
}

struct UInt16 {
  var value : Builtin.Int16

  static func convertFromIntegerLiteral(val : Builtin.Int16) -> UInt16 {
    return UInt16(val)
  }

  func replPrint() {
    print(UInt64(this))
  }
  // FIXME:
  // static var max : UInt16 { get { return 0xFFFF } }
  // static var min : UInt16 { get { return 0 } }
  static func max() -> UInt16 { return 0xFFFF }
  static func min() -> UInt16 { return 0 }
}

extension UInt16 : FormattedPrintable {
  func printFormatted(kind : Char, layout : String) {
    UInt64(this).printFormatted(kind, layout)
  }
}

struct Int32 {
  var value : Builtin.Int32

  static func convertFromIntegerLiteral(val : Builtin.Int32) -> Int32 {
    return Int32(val)
  }
  func getArrayBoundValue() -> Builtin.Int32 {
    return value
  }

  func replPrint() {
    print(Int64(this))
  }
  // FIXME:
  // static var max : Int32 { get { return 0x7FFFFFFF } }
  // static var min : Int32 { get { return -0x7FFFFFFF-1 } }
  static func max() -> Int32 { return 0x7FFFFFFF }
  static func min() -> Int32 { return -0x7FFFFFFF-1 }
}

extension Int32 : FormattedPrintable {
  func printFormatted(kind : Char, layout : String) {
    Int64(this).printFormatted(kind, layout)
  }
}

struct UInt32 {
  var value : Builtin.Int32

  static func convertFromIntegerLiteral(val : Builtin.Int32) -> UInt32 {
    return UInt32(val)
  }

  func replPrint() {
    print(UInt64(this))
  }
  // FIXME:
  // static var max : UInt32 { get { return 0xFFFFFFFF } }
  // static var min : UInt32 { get { return 0 } }
  static func max() -> UInt32 { return 0xFFFFFFFF }
  static func min() -> UInt32 { return 0 }
}

extension UInt32 : FormattedPrintable {
  func printFormatted(kind : Char, layout : String) {
    UInt64(this).printFormatted(kind, layout)
  }
}

struct Int64 {
  var value : Builtin.Int64

  static func convertFromIntegerLiteral(val : Builtin.Int64) -> Int {
    return Int64(val)
  }
  func getArrayBoundValue() -> Builtin.Int64 {
    return value
  }

  func replPrint() {
    print(this)
  }
  // FIXME:
  // static var max : Int64 { get { return 0x7FFFFFFFFFFFFFFF } }
  // static var min : Int64 { get { return -0x7FFFFFFFFFFFFFFF-1 } }
  static func max() -> Int64 { return 0x7FFFFFFFFFFFFFFF }
  static func min() -> Int64 { return -0x7FFFFFFFFFFFFFFF-1 }
}

extension Int64 : FormattedPrintable {
  func printFormatted(kind : Char, layout : String) {
    var radix = 10
    if kind == 'x' { radix = 16 }
    else if kind == 'o' { radix = 8 }
    Format(layout).printString(String(this, radix = radix))
  }
}

struct UInt64 {
  var value : Builtin.Int64

  static func convertFromIntegerLiteral(val : Builtin.Int64) -> UInt64 {
    return UInt64(val)
  }
  func getArrayBoundValue() -> Builtin.Int64 {
    return value
  }

  func replPrint() {
    print(this)
  }
  // FIXME:
  // static var max : UInt64 { get { return 0xFFFFFFFFFFFFFFFF } }
  // static var min : UInt64 { get { return 0 } }
  static func max() -> UInt64 { return 0xFFFFFFFFFFFFFFFF }
  static func min() -> UInt64 { return 0 }
}

extension UInt64 : FormattedPrintable {
  func printFormatted(kind : Char, layout : String) {
    var radix = 10
    if kind == 'x' { radix = 16 }
    else if kind == 'o' { radix = 8 }
    Format(layout).printString(String(this, radix = radix))
  }
}

struct Int128 {
  var value : Builtin.Int128

  static func convertFromIntegerLiteral(val : Builtin.Int128) -> Int128 {
    return Int128(val)
  }

  func replPrint() {
    print(String(this))
  }
  // FIXME:
  // static var max : Int128 { get { return 0x7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF } }
  // static var min : Int128 { get { return -0x7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF-1 } }
  static func max() -> Int128 { return 0x7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF }
  static func min() -> Int128 { return -0x7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF-1 }
}

extension Int128 : FormattedPrintable {
  func printFormatted(kind : Char, layout : String) {
    var radix = 10
    if kind == 'x' { radix = 16 }
    else if kind == 'o' { radix = 8 }
    Format(layout).printString(String(this, radix = radix))
  }
}

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

extension Float : FormattedPrintable {
  func printFormatted(kind : Char, layout : String) {
    Format(layout).printString(String(this))
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

extension Double : FormattedPrintable {
  func printFormatted(kind : Char, layout : String) {
    Format(layout).printString(String(this))
  }
}

//===----------------------------------------------------------------------===//
// Some useful type aliases
//===----------------------------------------------------------------------===//

// Int is just a typealias for Int64.
typealias Int = Int64

// IntegerLiteralType specifies the default type to use for an integer literal
// when the type isn't constrained.
typealias IntegerLiteralType = Int

// FloatLiteralType specifies the default type to use for a floating point
// literal when the type isn't constrained.
typealias FloatLiteralType = Double


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
  static func Int8(v : UInt16) -> Int8 {
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
  static func Int8(v : UInt64) -> Int8 {
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
  static func UInt8(v : UInt16) -> UInt8 {
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
  static func UInt8(v : UInt64) -> UInt8 {
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
  static func Int16(v : UInt16) -> Int16 {
    return Int16(v.value)
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
  static func Int16(v : UInt64) -> Int16 {
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
  static func Int32(v : UInt16) -> Int32 {
    return Int32(Builtin.zext_Int16_Int32(v.value))
  }
  static func Int32(v : UInt32) -> Int32 {
    return Int32(v.value)
  }
  static func Int32(v : Int64) -> Int32 {
    return Int32(Builtin.trunc_Int64_Int32(v.value))
  }
  static func Int32(v : UInt64) -> Int32 {
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
  static func UInt32(v : UInt16) -> UInt32 {
    return UInt32(Builtin.zext_Int16_Int32(v.value))
  }
  static func UInt32(v : Int32) -> UInt32 {
    return UInt32(v.value)
  }
  static func UInt32(v : Int64) -> UInt32 {
    return UInt32(Builtin.trunc_Int64_Int32(v.value))
  }
  static func UInt32(v : UInt64) -> UInt32 {
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
  static func Int64(v : UInt16) -> Int64 {
    return Int64(Builtin.zext_Int16_Int64(v.value))
  }
  static func Int64(v : Int32) -> Int64 {
    return Int64(Builtin.sext_Int32_Int64(v.value))
  }
  static func Int64(v : UInt32) -> Int64 {
    return Int64(Builtin.zext_Int32_Int64(v.value))
  }
  static func Int64(v : UInt64) -> Int64 {
    return Int64(v.value)
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
}

extension UInt64 {
  static func UInt64(v : Int8) -> UInt64 {
    return UInt64(Builtin.sext_Int8_Int64(v.value))
  }
  static func UInt64(v : UInt8) -> UInt64 {
    return UInt64(Builtin.zext_Int8_Int64(v.value))
  }
  static func UInt64(v : Int16) -> UInt64 {
    return UInt64(Builtin.sext_Int16_Int64(v.value))
  }
  static func UInt64(v : UInt16) -> UInt64 {
    return UInt64(Builtin.zext_Int16_Int64(v.value))
  }
  static func UInt64(v : Int32) -> UInt64 {
    return UInt64(Builtin.sext_Int32_Int64(v.value))
  }
  static func UInt64(v : UInt32) -> UInt64 {
    return UInt64(Builtin.zext_Int32_Int64(v.value))
  }
  static func UInt64(v : Int64) -> UInt64 {
    return UInt64(v.value)
  }
  static func UInt64(v : Int128) -> UInt64 {
    return UInt64(Builtin.trunc_Int128_Int64(v.value))
  }
  static func UInt64(v : Float) -> UInt64 {
    return UInt64(Builtin.fptosi_FPIEEE32_Int64(v.value))
  }
  static func UInt64(v : Double) -> UInt64 {
    return UInt64(Builtin.fptosi_FPIEEE64_Int64(v.value))
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
  static func Int128(v : UInt16) -> Int128 {
    return Int128(Builtin.zext_Int16_Int128(v.value))
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
  static func Int128(v : UInt64) -> Int128 {
    return Int128(Builtin.zext_Int64_Int128(v.value))
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

extension Bool : FormattedPrintable {
  func printFormatted(kind : Char, layout : String) {
    Format(layout).printString(String(this))
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
func +(a : UInt16)  -> UInt16  { return a }
func +(a : Int32)   -> Int32  { return a }
func +(a : UInt32)  -> UInt32 { return a }
func +(a : Int64)   -> Int64  { return a }
func +(a : UInt64)  -> UInt64 { return a }
func +(a : Int128)  -> Int128 { return a }
func +(a : Float)   -> Float  { return a }
func +(a : Double)  -> Double { return a }

// Bitwise negation operators.
func ~(a : Int8)    -> Int8   { return a ^ -1 }
func ~(a : UInt8)   -> UInt8  { return a ^ 0xFF }
func ~(a : Int16)   -> Int16  { return a ^ -1 }
func ~(a : UInt16)  -> UInt16 { return a ^ 0xFFFF }
func ~(a : Int32)   -> Int32  { return a ^ -1 }
func ~(a : UInt32)  -> UInt32 { return a ^ 0xFFFFFFFF }
func ~(a : Int64)   -> Int64  { return a ^ -1 }
func ~(a : UInt64)  -> UInt64 { return a ^ 0xFFFFFFFFFFFFFFFF }
func ~(a : Int128)  -> Int128 { return a ^ -1 }

// Unary increment and decrement operators.  These intentionally return nothing.
func [assignment] ++(a : [byref] Int8) { a += 1 }
func [assignment] ++(a : [byref] UInt8) { a += 1 }
func [assignment] ++(a : [byref] Int16) { a += 1 }
func [assignment] ++(a : [byref] UInt16) { a += 1 }
func [assignment] ++(a : [byref] Int32) { a += 1 }
func [assignment] ++(a : [byref] UInt32) { a += 1 }
func [assignment] ++(a : [byref] Int64) { a += 1 }
func [assignment] ++(a : [byref] UInt64) { a += 1 }
func [assignment] ++(a : [byref] Int128) { a += 1 }
func [assignment] --(a : [byref] Int8) { a -= 1 }
func [assignment] --(a : [byref] UInt8) { a -= 1 }
func [assignment] --(a : [byref] Int16) { a -= 1 }
func [assignment] --(a : [byref] UInt16) { a -= 1 }
func [assignment] --(a : [byref] Int32) { a -= 1 }
func [assignment] --(a : [byref] UInt32) { a -= 1 }
func [assignment] --(a : [byref] Int64) { a -= 1 }
func [assignment] --(a : [byref] UInt64) { a -= 1 }
func [assignment] --(a : [byref] Int128) { a -= 1 }

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
func [infix_left=200] * (lhs : UInt16, rhs : UInt16) -> UInt16 {
  return UInt16(Builtin.mul_Int16(lhs.value, rhs.value))
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
func [infix_left=200] * (lhs : UInt64, rhs : UInt64) -> UInt64 {
  return UInt64(Builtin.mul_Int64(lhs.value, rhs.value))
}
func [infix_left=200] * (lhs : Int128, rhs : Int128) -> Int128 {
  return Int128(Builtin.mul_Int128(lhs.value, rhs.value))
}
func [infix_left=200] * (lhs : Float, rhs : Float) -> Float {
  return Float(Builtin.fmul_FPIEEE32(lhs.value, rhs.value))
}
func [infix_left=200] * (lhs : Double, rhs : Double) -> Double {
  return Double(Builtin.fmul_FPIEEE64(lhs.value, rhs.value))
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
func [infix_left=200] / (lhs : UInt16, rhs : UInt16) -> UInt16 {
  return UInt16(Builtin.udiv_Int16(lhs.value, rhs.value))
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
func [infix_left=200] / (lhs : UInt64, rhs : UInt64) -> UInt64 {
  return UInt64(Builtin.udiv_Int64(lhs.value, rhs.value))
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
func [infix_left=200] % (lhs : UInt16, rhs : UInt16) -> UInt16 {
  return UInt16(Builtin.urem_Int16(lhs.value, rhs.value))
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
func [infix_left=200] % (lhs : UInt64, rhs : UInt64) -> UInt64 {
  return UInt64(Builtin.urem_Int64(lhs.value, rhs.value))
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
func [infix_left=190] + (lhs: UInt16, rhs: UInt16) -> UInt16 {
  return UInt16(Builtin.add_Int16(lhs.value, rhs.value))
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
func [infix_left=190] + (lhs: UInt64, rhs: UInt64) -> UInt64 {
  return UInt64(Builtin.add_Int64(lhs.value, rhs.value))
}
func [infix_left=190] + (lhs: Int128, rhs: Int128) -> Int128 {
  return Int128(Builtin.add_Int128(lhs.value, rhs.value))
}
func [infix_left=190] + (lhs: Float, rhs: Float) -> Float {
  return Float(Builtin.fadd_FPIEEE32(lhs.value, rhs.value))
}
func [infix_left=190] + (lhs: Double, rhs: Double) -> Double {
  return Double(Builtin.fadd_FPIEEE64(lhs.value, rhs.value))
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
func [infix_left=190] - (lhs: UInt16, rhs: UInt16) -> UInt16 {
  return UInt16(Builtin.sub_Int16(lhs.value, rhs.value))
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
func [infix_left=190] - (lhs: UInt64, rhs: UInt64) -> UInt64 {
  return UInt64(Builtin.sub_Int64(lhs.value, rhs.value))
}
func [infix_left=190] - (lhs: Int128, rhs: Int128) -> Int128 {
  return Int128(Builtin.sub_Int128(lhs.value, rhs.value))
}
func [infix_left=190] - (lhs: Float, rhs: Float) -> Float {
  return Float(Builtin.fsub_FPIEEE32(lhs.value, rhs.value))
}
func [infix_left=190] - (lhs: Double, rhs: Double) -> Double {
  return Double(Builtin.fsub_FPIEEE64(lhs.value, rhs.value))
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
func [infix=180] << (lhs : UInt16, rhs : UInt16) -> UInt16 {
  return UInt16(Builtin.shl_Int16(lhs.value, rhs.value))
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
func [infix=180] << (lhs : UInt64, rhs : UInt64) -> UInt64 {
  return UInt64(Builtin.shl_Int64(lhs.value, rhs.value))
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
func [infix=180] >>(lhs : UInt16, rhs : UInt16) -> UInt16 {
  return UInt16(Builtin.lshr_Int16(lhs.value, rhs.value))
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
func [infix=180] >>(lhs : UInt64, rhs : UInt64) -> UInt64 {
  return UInt64(Builtin.lshr_Int64(lhs.value, rhs.value))
}
func [infix=180] >>(lhs : Int128, rhs : Int128) -> Int128 {
  return Int128(Builtin.ashr_Int128(lhs.value, rhs.value))
}

// Less-Than Comparison.
func [infix=170] < (lhs : Int8, rhs : Int8) -> Bool {
  return _getBool(Builtin.cmp_slt_Int8(lhs.value, rhs.value))
}
func [infix=170] < (lhs : UInt8, rhs : UInt8) -> Bool {
  return _getBool(Builtin.cmp_ult_Int8(lhs.value, rhs.value))
}
func [infix=170] < (lhs : Int16, rhs : Int16) -> Bool {
  return _getBool(Builtin.cmp_slt_Int16(lhs.value, rhs.value))
}
func [infix=170] < (lhs : UInt16, rhs : UInt16) -> Bool {
  return _getBool(Builtin.cmp_ult_Int16(lhs.value, rhs.value))
}
func [infix=170] < (lhs : Int32, rhs : Int32) -> Bool {
  return _getBool(Builtin.cmp_slt_Int32(lhs.value, rhs.value))
}
func [infix=170] < (lhs : UInt32, rhs : UInt32) -> Bool {
  return _getBool(Builtin.cmp_ult_Int32(lhs.value, rhs.value))
}
func [infix=170] < (lhs : Int64, rhs : Int64) -> Bool {
  return _getBool(Builtin.cmp_slt_Int64(lhs.value, rhs.value))
}
func [infix=170] < (lhs : UInt64, rhs : UInt64) -> Bool {
  return _getBool(Builtin.cmp_ult_Int64(lhs.value, rhs.value))
}
func [infix=170] < (lhs : Int128, rhs : Int128) -> Bool {
  return _getBool(Builtin.cmp_slt_Int128(lhs.value, rhs.value))
}
func [infix=170] < (lhs : Float, rhs : Float) -> Bool {
  return _getBool(Builtin.fcmp_olt_FPIEEE32(lhs.value, rhs.value))
}
func [infix=170] < (lhs : Double, rhs : Double) -> Bool {
  return _getBool(Builtin.fcmp_olt_FPIEEE64(lhs.value, rhs.value))
}

// Greater-Than Comparison.
func [infix=170] > (lhs : Int8, rhs : Int8) -> Bool {
  return _getBool(Builtin.cmp_sgt_Int8(lhs.value, rhs.value))
}
func [infix=170] > (lhs : UInt8, rhs : UInt8) -> Bool {
  return _getBool(Builtin.cmp_ugt_Int8(lhs.value, rhs.value))
}
func [infix=170] > (lhs : Int16, rhs : Int16) -> Bool {
  return _getBool(Builtin.cmp_sgt_Int16(lhs.value, rhs.value))
}
func [infix=170] > (lhs : UInt16, rhs : UInt16) -> Bool {
  return _getBool(Builtin.cmp_ugt_Int16(lhs.value, rhs.value))
}
func [infix=170] > (lhs : Int32, rhs : Int32) -> Bool {
  return _getBool(Builtin.cmp_sgt_Int32(lhs.value, rhs.value))
}
func [infix=170] > (lhs : UInt32, rhs : UInt32) -> Bool {
  return _getBool(Builtin.cmp_ugt_Int32(lhs.value, rhs.value))
}
func [infix=170] > (lhs : Int64, rhs : Int64) -> Bool {
  return _getBool(Builtin.cmp_sgt_Int64(lhs.value, rhs.value))
}
func [infix=170] > (lhs : UInt64, rhs : UInt64) -> Bool {
  return _getBool(Builtin.cmp_ugt_Int64(lhs.value, rhs.value))
}
func [infix=170] > (lhs : Int128, rhs : Int128) -> Bool {
  return _getBool(Builtin.cmp_sgt_Int128(lhs.value, rhs.value))
}
func [infix=170] > (lhs : Float, rhs : Float) -> Bool {
  return _getBool(Builtin.fcmp_ogt_FPIEEE32(lhs.value, rhs.value))
}
func [infix=170] > (lhs : Double, rhs : Double) -> Bool {
  return _getBool(Builtin.fcmp_ogt_FPIEEE64(lhs.value, rhs.value))
}

// Less-Than-Or-Equal Comparison.
func [infix=170] <= (lhs : Int8, rhs : Int8) -> Bool {
  return _getBool(Builtin.cmp_sle_Int8(lhs.value, rhs.value))
}
func [infix=170] <= (lhs : UInt8, rhs : UInt8) -> Bool {
  return _getBool(Builtin.cmp_ule_Int8(lhs.value, rhs.value))
}
func [infix=170] <= (lhs : Int16, rhs : Int16) -> Bool {
  return _getBool(Builtin.cmp_sle_Int16(lhs.value, rhs.value))
}
func [infix=170] <= (lhs : UInt16, rhs : UInt16) -> Bool {
  return _getBool(Builtin.cmp_ule_Int16(lhs.value, rhs.value))
}
func [infix=170] <= (lhs : Int32, rhs : Int32) -> Bool {
  return _getBool(Builtin.cmp_sle_Int32(lhs.value, rhs.value))
}
func [infix=170] <= (lhs : UInt32, rhs : UInt32) -> Bool {
  return _getBool(Builtin.cmp_ule_Int32(lhs.value, rhs.value))
}
func [infix=170] <= (lhs : Int64, rhs : Int64) -> Bool {
  return _getBool(Builtin.cmp_sle_Int64(lhs.value, rhs.value))
}
func [infix=170] <= (lhs : UInt64, rhs : UInt64) -> Bool {
  return _getBool(Builtin.cmp_ule_Int64(lhs.value, rhs.value))
}
func [infix=170] <= (lhs : Int128, rhs : Int128) -> Bool {
  return _getBool(Builtin.cmp_sle_Int128(lhs.value, rhs.value))
}
func [infix=170] <= (lhs : Float, rhs : Float) -> Bool {
  return _getBool(Builtin.fcmp_ole_FPIEEE32(lhs.value, rhs.value))
}
func [infix=170] <= (lhs : Double, rhs : Double) -> Bool {
  return _getBool(Builtin.fcmp_ole_FPIEEE64(lhs.value, rhs.value))
}

// Greater-Than-Or-Equal Comparison.
func [infix=170] >= (lhs : Int8, rhs : Int8) -> Bool {
  return _getBool(Builtin.cmp_sge_Int8(lhs.value, rhs.value))
}
func [infix=170] >= (lhs : UInt8, rhs : UInt8) -> Bool {
  return _getBool(Builtin.cmp_uge_Int8(lhs.value, rhs.value))
}
func [infix=170] >= (lhs : Int16, rhs : Int16) -> Bool {
  return _getBool(Builtin.cmp_sge_Int16(lhs.value, rhs.value))
}
func [infix=170] >= (lhs : UInt16, rhs : UInt16) -> Bool {
  return _getBool(Builtin.cmp_uge_Int16(lhs.value, rhs.value))
}
func [infix=170] >= (lhs : Int32, rhs : Int32) -> Bool {
  return _getBool(Builtin.cmp_sge_Int32(lhs.value, rhs.value))
}
func [infix=170] >= (lhs : UInt32, rhs : UInt32) -> Bool {
  return _getBool(Builtin.cmp_uge_Int32(lhs.value, rhs.value))
}
func [infix=170] >= (lhs : Int64, rhs : Int64) -> Bool {
  return _getBool(Builtin.cmp_sge_Int64(lhs.value, rhs.value))
}
func [infix=170] >= (lhs : UInt64, rhs : UInt64) -> Bool {
  return _getBool(Builtin.cmp_uge_Int64(lhs.value, rhs.value))
}
func [infix=170] >= (lhs : Int128, rhs : Int128) -> Bool {
  return _getBool(Builtin.cmp_sge_Int128(lhs.value, rhs.value))
}
func [infix=170] >= (lhs : Float, rhs : Float) -> Bool {
  return _getBool(Builtin.fcmp_oge_FPIEEE32(lhs.value, rhs.value))
}
func [infix=170] >= (lhs : Double, rhs : Double) -> Bool {
  return _getBool(Builtin.fcmp_oge_FPIEEE64(lhs.value, rhs.value))
}

// Equality Comparison.
func [infix=160] == (lhs : Int8, rhs : Int8) -> Bool {
  return _getBool(Builtin.cmp_eq_Int8(lhs.value, rhs.value))
}
func [infix=160] == (lhs : UInt8, rhs : UInt8) -> Bool {
  return _getBool(Builtin.cmp_eq_Int8(lhs.value, rhs.value))
}
func [infix=160] == (lhs : Int16, rhs : Int16) -> Bool {
  return _getBool(Builtin.cmp_eq_Int16(lhs.value, rhs.value))
}
func [infix=160] == (lhs : UInt16, rhs : UInt16) -> Bool {
  return _getBool(Builtin.cmp_eq_Int16(lhs.value, rhs.value))
}
func [infix=160] == (lhs : Int32, rhs : Int32) -> Bool {
  return _getBool(Builtin.cmp_eq_Int32(lhs.value, rhs.value))
}
func [infix=160] == (lhs : UInt32, rhs : UInt32) -> Bool {
  return _getBool(Builtin.cmp_eq_Int32(lhs.value, rhs.value))
}
func [infix=160] == (lhs : Int64, rhs : Int64) -> Bool {
  return _getBool(Builtin.cmp_eq_Int64(lhs.value, rhs.value))
}
func [infix=160] == (lhs : UInt64, rhs : UInt64) -> Bool {
  return _getBool(Builtin.cmp_eq_Int64(lhs.value, rhs.value))
}
func [infix=160] == (lhs : Int128, rhs : Int128) -> Bool {
  return _getBool(Builtin.cmp_eq_Int128(lhs.value, rhs.value))
}
func [infix=160] == (lhs : Float, rhs : Float) -> Bool {
  return _getBool(Builtin.fcmp_oeq_FPIEEE32(lhs.value, rhs.value))
}
func [infix=160] == (lhs : Double, rhs : Double) -> Bool {
  return _getBool(Builtin.fcmp_oeq_FPIEEE64(lhs.value, rhs.value))
}

// Not-Equality Comparison.
func [infix=160] != (lhs : Int8, rhs : Int8) -> Bool {
  return _getBool(Builtin.cmp_ne_Int8(lhs.value, rhs.value))
}
func [infix=160] != (lhs : UInt8, rhs : UInt8) -> Bool {
  return _getBool(Builtin.cmp_ne_Int8(lhs.value, rhs.value))
}
func [infix=160] != (lhs : Int16, rhs : Int16) -> Bool {
  return _getBool(Builtin.cmp_ne_Int16(lhs.value, rhs.value))
}
func [infix=160] != (lhs : UInt16, rhs : UInt16) -> Bool {
  return _getBool(Builtin.cmp_ne_Int16(lhs.value, rhs.value))
}
func [infix=160] != (lhs : Int32, rhs : Int32) -> Bool {
  return _getBool(Builtin.cmp_ne_Int32(lhs.value, rhs.value))
}
func [infix=160] != (lhs : UInt32, rhs : UInt32) -> Bool {
  return _getBool(Builtin.cmp_ne_Int32(lhs.value, rhs.value))
}
func [infix=160] != (lhs : Int64, rhs : Int64) -> Bool {
  return _getBool(Builtin.cmp_ne_Int64(lhs.value, rhs.value))
}
func [infix=160] != (lhs : UInt64, rhs : UInt64) -> Bool {
  return _getBool(Builtin.cmp_ne_Int64(lhs.value, rhs.value))
}
func [infix=160] != (lhs : Int128, rhs : Int128) -> Bool {
  return _getBool(Builtin.cmp_ne_Int128(lhs.value, rhs.value))
}
func [infix=160] != (lhs : Float, rhs : Float) -> Bool {
  return _getBool(Builtin.fcmp_une_FPIEEE32(lhs.value, rhs.value))
}
func [infix=160] != (lhs : Double, rhs : Double) -> Bool {
  return _getBool(Builtin.fcmp_une_FPIEEE64(lhs.value, rhs.value))
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
func [infix_left=150] & (lhs: UInt16, rhs: UInt16) -> UInt16 {
  return UInt16(Builtin.and_Int16(lhs.value, rhs.value))
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
func [infix_left=150] & (lhs: UInt64, rhs: UInt64) -> UInt64 {
  return UInt64(Builtin.and_Int64(lhs.value, rhs.value))
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
func [infix_left=140] ^ (lhs: UInt16, rhs: UInt16) -> UInt16 {
  return UInt16(Builtin.xor_Int16(lhs.value, rhs.value))
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
func [infix_left=140] ^ (lhs: UInt64, rhs: UInt64) -> UInt64 {
  return UInt64(Builtin.xor_Int64(lhs.value, rhs.value))
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
func [infix_left=130] | (lhs: UInt16, rhs: UInt16) -> UInt16 {
  return UInt16(Builtin.or_Int16(lhs.value, rhs.value))
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
func [infix_left=130] | (lhs: UInt64, rhs: UInt64) -> UInt64 {
  return UInt64(Builtin.or_Int64(lhs.value, rhs.value))
}
func [infix_left=130] | (lhs: Int128, rhs: Int128) -> Int128 {
  return Int128(Builtin.or_Int128(lhs.value, rhs.value))
}

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

// In C, 100 is ?:

// In C, 90 is =, *=, += etc.

// Compound assignment (with addition)
func [assignment,infix_left=90] += (lhs : [byref] Int8, rhs : Int8) {
  lhs = lhs + rhs
}
func [assignment,infix_left=90] += (lhs : [byref] UInt8, rhs : UInt8) {
  lhs = lhs + rhs
}
func [assignment,infix_left=90] += (lhs : [byref] Int16, rhs : Int16) {
  lhs = lhs + rhs
}
func [assignment,infix_left=90] += (lhs : [byref] UInt16, rhs : UInt16) {
  lhs = lhs + rhs
}
func [assignment,infix_left=90] += (lhs : [byref] Int32, rhs : Int32) {
  lhs = lhs + rhs
}
func [assignment,infix_left=90] += (lhs : [byref] UInt32, rhs : UInt32) {
  lhs = lhs + rhs
}
func [assignment,infix_left=90] += (lhs : [byref] Int64, rhs : Int64) {
  lhs = lhs + rhs
}
func [assignment,infix_left=90] += (lhs : [byref] UInt64, rhs : UInt64) {
  lhs = lhs + rhs
}
func [assignment,infix_left=90] += (lhs : [byref] Int128, rhs : Int128) {
  lhs = lhs + rhs
}
func [assignment,infix_left=90] += (lhs : [byref] Float, rhs : Float) {
  lhs = lhs + rhs
}
func [assignment,infix_left=90] += (lhs : [byref] Double, rhs : Double) {
  lhs = lhs + rhs
}

// Compound assignment (with subtraction)
func [assignment,infix_left=90] -= (lhs : [byref] Int8, rhs : Int8) {
  lhs = lhs - rhs
}
func [assignment,infix_left=90] -= (lhs : [byref] UInt8, rhs : UInt8) {
  lhs = lhs - rhs
}
func [assignment,infix_left=90] -= (lhs : [byref] Int16, rhs : Int16) {
  lhs = lhs - rhs
}
func [assignment,infix_left=90] -= (lhs : [byref] UInt16, rhs : UInt16) {
  lhs = lhs - rhs
}
func [assignment,infix_left=90] -= (lhs : [byref] Int32, rhs : Int32) {
  lhs = lhs - rhs
}
func [assignment,infix_left=90] -= (lhs : [byref] UInt32, rhs : UInt32) {
  lhs = lhs - rhs
}
func [assignment,infix_left=90] -= (lhs : [byref] Int64, rhs : Int64) {
  lhs = lhs - rhs
}
func [assignment,infix_left=90] -= (lhs : [byref] UInt64, rhs : UInt64) {
  lhs = lhs - rhs
}
func [assignment,infix_left=90] -= (lhs : [byref] Int128, rhs : Int128) {
  lhs = lhs - rhs
}
func [assignment,infix_left=90] -= (lhs : [byref] Float, rhs : Float) {
  lhs = lhs - rhs
}
func [assignment,infix_left=90] -= (lhs : [byref] Double, rhs : Double) {
  lhs = lhs - rhs
}

// Compound assignment (with multiplication)
func [assignment,infix_left=90] *= (lhs : [byref] Int8, rhs : Int8) {
  lhs = lhs * rhs
}
func [assignment,infix_left=90] *= (lhs : [byref] UInt8, rhs : UInt8) {
  lhs = lhs * rhs
}
func [assignment,infix_left=90] *= (lhs : [byref] Int16, rhs : Int16) {
  lhs = lhs * rhs
}
func [assignment,infix_left=90] *= (lhs : [byref] UInt16, rhs : UInt16) {
  lhs = lhs * rhs
}
func [assignment,infix_left=90] *= (lhs : [byref] Int32, rhs : Int32) {
  lhs = lhs * rhs
}
func [assignment,infix_left=90] *= (lhs : [byref] UInt32, rhs : UInt32) {
  lhs = lhs * rhs
}
func [assignment,infix_left=90] *= (lhs : [byref] Int64, rhs : Int64) {
  lhs = lhs * rhs
}
func [assignment,infix_left=90] *= (lhs : [byref] UInt64, rhs : UInt64) {
  lhs = lhs * rhs
}
func [assignment,infix_left=90] *= (lhs : [byref] Int128, rhs : Int128) {
  lhs = lhs * rhs
}
func [assignment,infix_left=90] *= (lhs : [byref] Float, rhs : Float) {
  lhs = lhs * rhs
}
func [assignment,infix_left=90] *= (lhs : [byref] Double, rhs : Double) {
  lhs = lhs * rhs
}

// Compound assignment (with division)
func [assignment,infix_left=90] /= (lhs : [byref] Int8, rhs : Int8) {
  lhs = lhs / rhs
}
func [assignment,infix_left=90] /= (lhs : [byref] UInt8, rhs : UInt8) {
  lhs = lhs / rhs
}
func [assignment,infix_left=90] /= (lhs : [byref] Int16, rhs : Int16) {
  lhs = lhs / rhs
}
func [assignment,infix_left=90] /= (lhs : [byref] UInt16, rhs : UInt16) {
  lhs = lhs / rhs
}
func [assignment,infix_left=90] /= (lhs : [byref] Int32, rhs : Int32) {
  lhs = lhs / rhs
}
func [assignment,infix_left=90] /= (lhs : [byref] UInt32, rhs : UInt32) {
  lhs = lhs / rhs
}
func [assignment,infix_left=90] /= (lhs : [byref] Int64, rhs : Int64) {
  lhs = lhs / rhs
}
func [assignment,infix_left=90] /= (lhs : [byref] UInt64, rhs : UInt64) {
  lhs = lhs / rhs
}
func [assignment,infix_left=90] /= (lhs : [byref] Int128, rhs : Int128) {
  lhs = lhs / rhs
}
func [assignment,infix_left=90] /= (lhs : [byref] Float, rhs : Float) {
  lhs = lhs / rhs
}
func [assignment,infix_left=90] /= (lhs : [byref] Double, rhs : Double) {
  lhs = lhs / rhs
}

// Compound assignment (with remainder)
func [assignment,infix_left=90] %= (lhs : [byref] Int8, rhs : Int8) {
  lhs = lhs % rhs
}
func [assignment,infix_left=90] %= (lhs : [byref] UInt8, rhs : UInt8) {
  lhs = lhs % rhs
}
func [assignment,infix_left=90] %= (lhs : [byref] Int16, rhs : Int16) {
  lhs = lhs % rhs
}
func [assignment,infix_left=90] %= (lhs : [byref] UInt16, rhs : UInt16) {
  lhs = lhs % rhs
}
func [assignment,infix_left=90] %= (lhs : [byref] Int32, rhs : Int32) {
  lhs = lhs % rhs
}
func [assignment,infix_left=90] %= (lhs : [byref] UInt32, rhs : UInt32) {
  lhs = lhs % rhs
}
func [assignment,infix_left=90] %= (lhs : [byref] Int64, rhs : Int64) {
  lhs = lhs % rhs
}
func [assignment,infix_left=90] %= (lhs : [byref] UInt64, rhs : UInt64) {
  lhs = lhs % rhs
}
func [assignment,infix_left=90] %= (lhs : [byref] Int128, rhs : Int128) {
  lhs = lhs % rhs
}

// Compound assignment (with left shift)
func [assignment,infix_left=90] <<= (lhs : [byref] Int8, rhs : Int8) {
  lhs = lhs << rhs
}
func [assignment,infix_left=90] <<= (lhs : [byref] UInt8, rhs : UInt8) {
  lhs = lhs << rhs
}
func [assignment,infix_left=90] <<= (lhs : [byref] Int16, rhs : Int16) {
  lhs = lhs << rhs
}
func [assignment,infix_left=90] <<= (lhs : [byref] UInt16, rhs : UInt16) {
  lhs = lhs << rhs
}
func [assignment,infix_left=90] <<= (lhs : [byref] Int32, rhs : Int32) {
  lhs = lhs << rhs
}
func [assignment,infix_left=90] <<= (lhs : [byref] UInt32, rhs : UInt32) {
  lhs = lhs << rhs
}
func [assignment,infix_left=90] <<= (lhs : [byref] Int64, rhs : Int64) {
  lhs = lhs << rhs
}
func [assignment,infix_left=90] <<= (lhs : [byref] UInt64, rhs : UInt64) {
  lhs = lhs << rhs
}
func [assignment,infix_left=90] <<= (lhs : [byref] Int128, rhs : Int128) {
  lhs = lhs << rhs
}

// Compound assignment (with right shift)
func [assignment,infix_left=90] >>= (lhs : [byref] Int8, rhs : Int8) {
  lhs = lhs >> rhs
}
func [assignment,infix_left=90] >>= (lhs : [byref] UInt8, rhs : UInt8) {
  lhs = lhs >> rhs
}
func [assignment,infix_left=90] >>= (lhs : [byref] Int16, rhs : Int16) {
  lhs = lhs >> rhs
}
func [assignment,infix_left=90] >>= (lhs : [byref] UInt16, rhs : UInt16) {
  lhs = lhs >> rhs
}
func [assignment,infix_left=90] >>= (lhs : [byref] Int32, rhs : Int32) {
  lhs = lhs >> rhs
}
func [assignment,infix_left=90] >>= (lhs : [byref] UInt32, rhs : UInt32) {
  lhs = lhs >> rhs
}
func [assignment,infix_left=90] >>= (lhs : [byref] Int64, rhs : Int64) {
  lhs = lhs >> rhs
}
func [assignment,infix_left=90] >>= (lhs : [byref] UInt64, rhs : UInt64) {
  lhs = lhs >> rhs
}
func [assignment,infix_left=90] >>= (lhs : [byref] Int128, rhs : Int128) {
  lhs = lhs >> rhs
}

// Compound assignment (with bitwise and)
func [assignment,infix_left=90] &= (lhs : [byref] Int8, rhs : Int8) {
  lhs = lhs & rhs
}
func [assignment,infix_left=90] &= (lhs : [byref] UInt8, rhs : UInt8) {
  lhs = lhs & rhs
}
func [assignment,infix_left=90] &= (lhs : [byref] Int16, rhs : Int16) {
  lhs = lhs & rhs
}
func [assignment,infix_left=90] &= (lhs : [byref] UInt16, rhs : UInt16) {
  lhs = lhs & rhs
}
func [assignment,infix_left=90] &= (lhs : [byref] Int32, rhs : Int32) {
  lhs = lhs & rhs
}
func [assignment,infix_left=90] &= (lhs : [byref] UInt32, rhs : UInt32) {
  lhs = lhs & rhs
}
func [assignment,infix_left=90] &= (lhs : [byref] Int64, rhs : Int64) {
  lhs = lhs & rhs
}
func [assignment,infix_left=90] &= (lhs : [byref] UInt64, rhs : UInt64) {
  lhs = lhs & rhs
}
func [assignment,infix_left=90] &= (lhs : [byref] Int128, rhs : Int128) {
  lhs = lhs & rhs
}

// Compound assignment (with bitwise or)
func [assignment,infix_left=90] |= (lhs : [byref] Int8, rhs : Int8) {
  lhs = lhs | rhs
}
func [assignment,infix_left=90] |= (lhs : [byref] UInt8, rhs : UInt8) {
  lhs = lhs | rhs
}
func [assignment,infix_left=90] |= (lhs : [byref] Int16, rhs : Int16) {
  lhs = lhs | rhs
}
func [assignment,infix_left=90] |= (lhs : [byref] UInt16, rhs : UInt16) {
  lhs = lhs | rhs
}
func [assignment,infix_left=90] |= (lhs : [byref] Int32, rhs : Int32) {
  lhs = lhs | rhs
}
func [assignment,infix_left=90] |= (lhs : [byref] UInt32, rhs : UInt32) {
  lhs = lhs | rhs
}
func [assignment,infix_left=90] |= (lhs : [byref] Int64, rhs : Int64) {
  lhs = lhs | rhs
}
func [assignment,infix_left=90] |= (lhs : [byref] UInt64, rhs : UInt64) {
  lhs = lhs | rhs
}
func [assignment,infix_left=90] |= (lhs : [byref] Int128, rhs : Int128) {
  lhs = lhs | rhs
}

// Compound assignment (with bitwise exclusive or)
func [assignment,infix_left=90] ^= (lhs : [byref] Int8, rhs : Int8) {
  lhs = lhs ^ rhs
}
func [assignment,infix_left=90] ^= (lhs : [byref] UInt8, rhs : UInt8) {
  lhs = lhs ^ rhs
}
func [assignment,infix_left=90] ^= (lhs : [byref] Int16, rhs : Int16) {
  lhs = lhs ^ rhs
}
func [assignment,infix_left=90] ^= (lhs : [byref] UInt16, rhs : UInt16) {
  lhs = lhs ^ rhs
}
func [assignment,infix_left=90] ^= (lhs : [byref] Int32, rhs : Int32) {
  lhs = lhs ^ rhs
}
func [assignment,infix_left=90] ^= (lhs : [byref] UInt32, rhs : UInt32) {
  lhs = lhs ^ rhs
}
func [assignment,infix_left=90] ^= (lhs : [byref] Int64, rhs : Int64) {
  lhs = lhs ^ rhs
}
func [assignment,infix_left=90] ^= (lhs : [byref] UInt64, rhs : UInt64) {
  lhs = lhs ^ rhs
}
func [assignment,infix_left=90] ^= (lhs : [byref] Int128, rhs : Int128) {
  lhs = lhs ^ rhs
}

//===----------------------------------------------------------------------===//
// Char Type
//===----------------------------------------------------------------------===//

// CharLiteralType specifies the default type to use for a character literal
// when the type isn't constrained.
typealias CharacterLiteralType = Char

struct Char {
  var value : Builtin.Int32

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
    if this == '\\' {
      print('\\')
      print('\\')
    } else if this == '"' {
      print('\\')
      print('"')
    } else if this == '\'' {
      print('\\')
      print('\'')
    } else if isPrint() {
      print(this)
    } else if this == '\t' {
      print('\\')
      print('t')
    } else if this == '\n' {
      print('\\')
      print('n')
    } else if this == '\r' {
      print('\\')
      print('r')
    } else if UInt32(this) < 128 {
      print('\\')
      print('x')
      _printNibbleAsHex(UInt32(this) >> 4)
      _printNibbleAsHex(UInt32(this))
    } else if _isUTF8() {
      print(this)
    } else if UInt32(this) <= 0xFFFF {
      print('\\')
      print('u')
      _printNibbleAsHex(UInt32(this) >> 12)
      _printNibbleAsHex(UInt32(this) >> 8)
      _printNibbleAsHex(UInt32(this) >> 4)
      _printNibbleAsHex(UInt32(this))
    } else {
      print('\\')
      print('U')
      _printNibbleAsHex(UInt32(this) >> 28)
      _printNibbleAsHex(UInt32(this) >> 24)
      _printNibbleAsHex(UInt32(this) >> 20)
      _printNibbleAsHex(UInt32(this) >> 16)
      _printNibbleAsHex(UInt32(this) >> 12)
      _printNibbleAsHex(UInt32(this) >> 8)
      _printNibbleAsHex(UInt32(this) >> 4)
      _printNibbleAsHex(UInt32(this))
    }
  }
  
    // FIXME: Move to nested function when IRGen supports it.
  static func _printNibbleAsHex(v : UInt32) {
    v = v & 15
    if v < 10 {
      print(Char(v+48))    // 48 = '0'
    } else {
      print(Char(v-10+65)) // 65 = 'A'
    }
  }

  // FIXME: Locales make this interesting
  func isAlpha() -> Bool { 
    return (this >= 'A' && this <= 'Z') || (this >= 'a' && this <= 'z')
  }

  // FIXME: Locales make this interesting
  func isDigit() -> Bool {
    return this >= '0' && this <= '9'
  }

  // FIXME: Locales make this interesting
  func toUpper() -> Char {
    if this >= 'a' && this <= 'z' {
      return Char(UInt32(this) - 32)
    } else if this >= 'à' && this <= 'þ' && this != '÷' {
      return Char(UInt32(this) - 32)
    }
    return this;
  }

  // FIXME: Locales make this interesting
  func toLower() -> Char {
    if this >= 'A' && this <= 'Z' {
      return Char(UInt32(this) + 32)
    } else if this >= 'À' && this <= 'Þ' && this != '×' {
      return Char(UInt32(this) + 32)
    }
    return this;
  }
}

extension Char : FormattedPrintable {
  func printFormatted(kind : Char, layout : String) {
    // FIXME: We need to be able to convert a character to a string.
    // Duplicating the logic for the 'Format' class is lame!
    var toPrint = this;
    if kind == 'u' { toPrint = toUpper() }
    else if kind == 'l' { toPrint = toLower() }
    
    var format = Format(layout)
    if !format.isValid {
      print(toPrint);
    }
    
    var padding = max(format.width - 1, 0)
      if !format.leftJustify {
      for i in 0..padding { print(' ') }
    }
    print(toPrint)
    if format.leftJustify {
      for i in 0..padding { print(' ') }
    }
  }
}

extension UInt32 {
  static func UInt32(v : Char) -> UInt32 {
    return UInt32(v.value)
  }
}
extension UInt64 {
  static func UInt64(v : Char) -> UInt64 {
    return UInt64(UInt32(v.value))
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
  return _getBool(Builtin.cmp_ult_Int32(lhs.value, rhs.value))
}

func [infix=170] > (lhs : Char, rhs : Char) -> Bool {
  return _getBool(Builtin.cmp_ugt_Int32(lhs.value, rhs.value))
}

func [infix=170] <= (lhs : Char, rhs : Char) -> Bool {
  return _getBool(Builtin.cmp_ule_Int32(lhs.value, rhs.value))
}

func [infix=170] >= (lhs : Char, rhs : Char) -> Bool {
  return _getBool(Builtin.cmp_uge_Int32(lhs.value, rhs.value))
}

func [infix=160] == (lhs : Char, rhs : Char) -> Bool {
  return _getBool(Builtin.cmp_eq_Int32(lhs.value, rhs.value))
}

func [infix=160] != (lhs : Char, rhs : Char) -> Bool {
  return _getBool(Builtin.cmp_ne_Int32(lhs.value, rhs.value))
}

extension Char {
  func isPrint() -> Bool {
    return (this >= Char(0o040) && this <= Char(0o176))
  }
}

//===----------------------------------------------------------------------===//
// String Type
//===----------------------------------------------------------------------===//

func [asmname="swift_StringToNSString"]
convertStringToNSString(string : [byref] String) -> id

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
  var str_value : StringByte[]

  static func convertFromStringLiteral(val : Builtin.RawPointer) -> String {
    var isASCII = true
    var s : String
    var Ptr = UnsafePointerInt8(val)
    while Ptr.get() != 0 {
      if StringByte(Ptr.get()) >= 0x80 {
        isASCII = false
      }
      ++Ptr
    }
    s.str_value = new StringByte[Ptr-UnsafePointerInt8(val)]
    Ptr = UnsafePointerInt8(val)
    var i = 0
    var len = s.str_value.getLength()
    while i < len {
      s.str_value[i] = StringByte(Ptr.get())
      ++Ptr
      ++i
    }
    s.str_value.setASCII(isASCII)
    return s
  }

  func byteLength() -> Int {
    return str_value.getLength()
  }

  func [conversion] __conversion() -> NSString {
    var result : NSString
    result.value = convertStringToNSString(&this)
    return result
  }

  struct CharRange : Range {
    var value : String

    typealias Element = Char
    func isEmpty() -> Bool {
      return value.isEmpty()
    }
    func getFirstAndAdvance() -> Char {
      // string ranges do not end in the middle of a Char
      // one range check is sufficient.
      if isEmpty() {
        Builtin.trap()
      }
      var u8 = StringByte(value.str_value.base.get())
      if u8 < 0x80 {
        value.str_value.base += 1
        value.str_value.setLength(value.str_value.getLength() - 1)
        return Char(UInt32(u8))
      }
     
      return _getFirstAndAdvancedSlow(u8) 
    }
    
    func /*[noinline]*/ _getFirstAndAdvancedSlow(u8 : StringByte) -> Char {
      var u8_1 = StringByte((value.str_value.base + 1).get())
      if u8 < 0xE0 {
        value.str_value.base += 2
        value.str_value.setLength(value.str_value.getLength() - 2)
        return Char((UInt32(u8 & 0x1F) << 6) |
                     UInt32(u8_1 & 0x3F))
      }
      var u8_2 = StringByte((value.str_value.base + 2).get())
      if u8 < 0xF0 {
        value.str_value.base += 3
        value.str_value.setLength(value.str_value.getLength() - 3)
        return Char((UInt32(u8   & 0x0F) << 12) |
                    (UInt32(u8_1 & 0x3F) << 6)  |
                     UInt32(u8_2 & 0x3F))
      }
      var u8_3 = StringByte((value.str_value.base + 3).get())
      value.str_value.base += 4
      value.str_value.setLength(value.str_value.getLength() - 4)
      return Char((UInt32(u8   & 0x07) << 18) |
                  (UInt32(u8_1 & 0x3F) << 12) |
                  (UInt32(u8_2 & 0x3F) << 6)  |
                   UInt32(u8_3 & 0x3F))
    }
  }

  func getElements() -> CharRange {
    return CharRange(this)
  }

  func size() -> Int {
    if str_value.isASCII() {
      return byteLength()
    }
    var Result = 0
    for i in this {
      ++Result
    }
    return Result
  }

  func isEmpty() -> Bool {
    return byteLength() == 0
  }

  subscript (rng : IntRange) -> String {
    get {
      var len = rng.max - rng.min
      if str_value.isASCII() {
        if len >= byteLength() {
          Builtin.trap()
        }
        return String(SliceStringByte.convertFromHeapArray(
                      (str_value.base + rng.min).value,
                      str_value.owner, len.value))
      }
      return _subscriptNonASCII(rng)
    }
  }

  func _subscriptNonASCII (rng : IntRange) -> String {
    var len = rng.max - rng.min
    var start = rng.min
    var idx = 0
    while start > 0 {
      var tmp : StringByte = str_value[idx]
      if tmp < 0x80 {
        --start
      } else if tmp >= 0xC0 {
        --start
      }
      ++idx
    }
    var oldidx = idx;
    while len > 0 {
      var tmp = str_value[idx]
      if tmp < 0x80 {
        --len
      } else if tmp >= 0xC0 {
        --len
      }
      ++idx
    }
    return String(SliceStringByte.convertFromHeapArray(
                    (str_value.base + oldidx).value,
                    str_value.owner, (idx - oldidx).value))
  }

  subscript (idx : Int) -> Char {
    get {
      if str_value.isASCII() {
        if idx < byteLength() {
          return Char(UInt32((str_value.base + idx).get()))
        }
      }
      return subscriptNonASCII(idx)
    }
  }

  func subscriptNonASCII(idx : Int) -> Char {
    for c in this {
      if idx == 0 {
        return c;
      }
      --idx
    }
    Builtin.trap()
  }

  func replPrint() {
    print('"')
    for c in this {
      c.replPrintCharBody()
    }
    print('"')
  }
  
  // FIXME: Locales make this interesting
  func toUpper() -> String {
    var len = byteLength()
    var resultArray = new StringByte[len]
    var i : Int
    while i != len {
      var u8 = str_value[i]
      if u8 < 0x80 {
        if u8 >= 97 && u8 <= 122 {
          resultArray[i] = u8 - 32
        } else {
          resultArray[i] = u8
        }
        ++i
      } else if u8 < 0xE0 {
        resultArray[i] = u8
        var u8_1 = str_value[i + 1]
        if u8 == 0xC3 && u8_1 > 0xA0 && u8_1 < 0xBF && u8_1 != 0xB7 {
          resultArray[i+1] = u8_1 - 0x20
        } else {
          resultArray[i+1] = u8_1
        }
        i += 2
      } else if u8 < 0xF0 {
        resultArray[i] = u8
        resultArray[i+1] = str_value[i+1]
        resultArray[i+2] = str_value[i+2]
        i += 3
      } else {
        resultArray[i] = u8
        resultArray[i+1] = str_value[i+1]
        resultArray[i+2] = str_value[i+2]
        resultArray[i+3] = str_value[i+3]
        i += 4
      }
    }
    
    var result : String
    result.str_value = resultArray
    return result
  }
  
  // FIXME: Locales make this interesting
  func toLower() -> String {
    var len = byteLength()
    var resultArray = new StringByte[len]
    var i : Int
    while i != len {
      var u8 = str_value[i]
      if u8 < 0x80 {
        if u8 >= 65 && u8 <= 90 {
          resultArray[i] = u8 + 32
        } else {
          resultArray[i] = u8
        }
        ++i
      } else if u8 < 0xE0 {
        resultArray[i] = u8
        var u8_1 = str_value[i + 1]
        if u8 == 0xC3 && u8_1 > 0x80 && u8_1 < 0x9F && u8_1 != 0x97 {
          resultArray[i+1] = u8_1 + 0x20
        } else {
          resultArray[i+1] = u8_1
        }
        i += 2
      } else if u8 < 0xF0 {
        resultArray[i] = u8
        resultArray[i+1] = str_value[i+1]
        resultArray[i+2] = str_value[i+2]
        i += 3
      } else {
        resultArray[i] = u8
        resultArray[i+1] = str_value[i+1]
        resultArray[i+2] = str_value[i+2]
        resultArray[i+3] = str_value[i+3]
        i += 4
      }
    }
    
    var result : String
    result.str_value = resultArray
    return result
  }

  static func String(c : Char) -> String {
    var buf : StringByte[] = new StringByte[4]
    var len = 0
    if c <= Char(0x7F) {
      buf[0] = StringByte(UInt32(c))
      len = 1
    } else {
      var high = StringByte(((UInt32(c) >> 18)       ) | 0xF0)
      var midh = StringByte(((UInt32(c) >> 12) & 0x3F) | 0x80)
      var midl = StringByte(((UInt32(c) >>  6) & 0x3F) | 0x80)
      var low  = StringByte(((UInt32(c)      ) & 0x3F) | 0x80)
      if c <= Char(0x07FF) {
        buf[0] = midl | 0x40
        buf[1] = low
        len = 2
      } else if c <= Char(0xFFFF) {
        buf[0] = midh | 0x60
        buf[1] = midl
        buf[2] = low
        len = 3
      } else if c <= Char(0x1FFFFF) {
        buf[0] = high
        buf[1] = midh
        buf[2] = midl
        buf[3] = low
        len = 4
      } else {
        Builtin.trap()
      }
    }
    return String(SliceStringByte.convertFromHeapArray(buf.base.value, buf.owner, len.value))
  }
}

extension String : FormattedPrintable {
  func printFormatted(kind : Char, layout : String) {
    var toPrint = this
    if kind == 'u' { toPrint = toUpper() }
    else if kind == 'l' { toPrint = toLower() }
    Format(layout).printString(toPrint)
  }
}

// StringLiteralType specifies the default type to use for an string literal
// when the type isn't constrained.
typealias StringLiteralType = String


// Concatenation.
func [infix_left=190]+(lhs : String, rhs : String) -> String {
  var l_length = lhs.byteLength()
  var r_length = rhs.byteLength()
  var buf : StringByte[] = new StringByte[l_length + r_length]
  var i = 0
  for var j = 0; j < l_length; ++j {
    buf[i] = lhs.str_value[j]
    ++i
  }
  for var j = 0; j < r_length; ++j {
    buf[i] = rhs.str_value[j]
    ++i
  }
  var s = String(buf)
  s.str_value.setASCII(lhs.str_value.isASCII() && rhs.str_value.isASCII())
  return s
}
func [infix_left=190]+(lhs : String, rhs : Char) -> String {
  return lhs + String(rhs)
}
func [infix_left=190]+(lhs : Char, rhs : String) -> String {
  return String(lhs) + rhs
}
func [infix_left=190]+(lhs : Char, rhs : Char) -> String {
  return String(lhs) + String(rhs)
}


// String append
func [assignment,infix_left=90] += (lhs : [byref] String, rhs : String) {
  lhs = lhs + rhs
}
func [assignment,infix_left=90] += (lhs : [byref] String, rhs : Char) {
  lhs = lhs + rhs
}


// Comparison operators

func [infix=160] == (lhs : String, rhs : String) -> Bool {
  if lhs.str_value.getLength() != rhs.str_value.getLength() {
    return false
  }
  var len = lhs.byteLength();
  for var i = 0; i < len; ++i {
     if lhs.str_value[i] != rhs.str_value[i] {
       return false
     }
  }
  return true
}

func [infix=160] != (lhs : String, rhs : String) -> Bool {
  return !(lhs == rhs)
}

func [infix=170] < (lhs : String, rhs : String) -> Bool {
  var i = 0
  for c in rhs.str_value {
    if i == lhs.byteLength() || lhs.str_value[i] < c {
      return true
    }
    if c < lhs.str_value[i] {
      return false
    }
    ++i
  }
  return false
}

func [infix=170] > (lhs : String, rhs : String) -> Bool {
  return rhs < lhs
}

func [infix=170] <= (lhs : String, rhs : String) -> Bool {
  return !(rhs < lhs)
}

func [infix=170] >= (lhs : String, rhs : String) -> Bool {
  return !(lhs < rhs)
}

// Conversions to string from other types.
extension String {

  static func String(v : Int128, radix : Int = 10) -> String {
    var Buffer = new StringByte[128]
    var i = c_print_int(Buffer.base.value, 128, v, radix)
    var b = Buffer[0..Int(i)]
    return String(b)
  }

  static func String(v : Int8, radix : Int = 10) -> String {
    return String(Int128(v), radix)
  }
  static func String(v : UInt8, radix : Int = 10) -> String {
    return String(Int128(v), radix)
  }
  static func String(v : Int16, radix : Int = 10) -> String {
    return String(Int128(v), radix)
  }
  static func String(v : Int32, radix : Int = 10) -> String {
    return String(Int128(v), radix)
  }
  static func String(v : UInt32, radix : Int = 10) -> String {
    return String(Int128(v), radix)
  }
  static func String(v : Int64, radix : Int = 10) -> String {
    return String(Int128(v), radix)
  }
  static func String(v : UInt64, radix : Int = 10) -> String {
    return String(Int128(v), radix)
  }

  static func String(v : Double) -> String {
    var Buffer = new StringByte[256]
    var i = c_print_double(Buffer.base.value, v)
    return String(Buffer[0..Int(i)])
  }

  static func String(v : Float) -> String { return String(Double(v)) }
  static func String(b : Bool) -> String {
    if b {
      return "true"
    }
    return "false"
  }
}

extension String {
  /// \brief Produce a substring of the given string from the given character
  /// index to the end of the string.
  func substr(start : Int) -> String {
    var rng = getElements()
    while start > 0 {
      rng.getFirstAndAdvance()
      --start
    }
    var result = this
    var bytes = rng.value.str_value.base - str_value.base
    result.str_value.base += bytes
    result.str_value.setLength(result.str_value.getLength() - bytes)
    return result
  }

  /// \brief Split the given string at the given delimiter character, returning 
  /// the strings before and after that character (neither includes the character
  /// found) and a boolean value indicating whether the delimiter was found.
  // 
  func splitFirst(delim : Char) 
    -> (before: String, after: String, wasFound : Bool) 
  {
    var rng = getElements()
    while !rng.isEmpty() {
      var rngBefore = rng
      if rng.getFirstAndAdvance() == delim {
        var before = this
        before.str_value.setLength(rngBefore.value.str_value.base - str_value.base)
        before.str_value.setASCII(str_value.isASCII())

        var after = this
        after.str_value.base += rng.value.str_value.base - str_value.base
        after.str_value.setLength(after.str_value.getLength() -
                                  (rng.value.str_value.base - str_value.base))
        return (before, after, true)
      }
    }
    return (this, "", false)
  }

  /// \brief Split the given string at the first character for which the given
  /// predicate returns true. Returns the string before that character, the 
  /// character that matches, the string after that character, and a boolean value
  /// indicating whether any character was found.
  func splitFirstIf(pred : (Char) -> Bool) 
    -> (before: String, found: Char, after : String, wasFound: Bool) 
  {
    var rng = getElements()
    while !rng.isEmpty() {
      var rngBefore = rng
      var c = rng.getFirstAndAdvance()
      if pred(c) {
        var before = this
        before.str_value.setLength(rngBefore.value.str_value.base -
                                   str_value.base)
        before.str_value.setASCII(str_value.isASCII())

        var after = this
        after.str_value.base += rng.value.str_value.base - str_value.base
        after.str_value.setLength(after.str_value.getLength() -
                                  (rng.value.str_value.base - str_value.base))
        return (before, c, after, true)
      }
    }
    return (this, Char(0), "", false)
  }
}

//===----------------------------------------------------------------------===//
// Formatted Printing
//===----------------------------------------------------------------------===//

struct Format {
  var width : Int
  var isValid : Bool
  var leftJustify : Bool

  static func Format(layout : String) -> Format {
    var format : Format
    format.isValid = true
    format.leftJustify = false
    format.width = 0

    if layout.isEmpty() { return format }
    if layout[0] == '-' {
      format.leftJustify = true
      layout = layout.substr(1)
    }

    if !layout.isEmpty() {
      for c in layout {
        if !c.isDigit() { 
          format.isValid = false
          return format
        }
        format.width = format.width * 10 + Int(UInt32(c) - UInt32('0'))
      }
      if !layout.isEmpty() { 
        format.isValid = false
        return format
      }
    }

    return format
  }

  func printString(s : String) {

/*  Causes problems due to <rdar://problem/11529601>
    if !isValid { 
      print(s)
      return
    }
*/
    var padding = max(width - s.size(), 0)
    if !leftJustify {
      for i in 0..padding { print(' ') }
    }
    print(s)
    if leftJustify {
      for i in 0..padding { print(' ') }
    }
  }

  func replPrint() {
    print("valid = \(isValid), leftJustify = \(leftJustify), width=\(width)")
  }
}

protocol FormattedPrintable {
  func printFormatted(kind : Char, layout : String)
}

func splitFormat(format : String) -> (String, String, Char, String) {
  var (before, afterPercent, foundPercent) = format.splitFirst('%')
  if !foundPercent {
    return (before, "", Char(0), afterPercent)
  }

  var (layout, kind, after, found) = afterPercent.splitFirstIf({ $0.isAlpha() })
  if !found {
    return (before, "", Char(0), afterPercent)    
  }

  return (before, layout, kind, after)
}

func printf(format : String, args : FormattedPrintable...) {
  var index = 0
  while !format.isEmpty() {
    var (before, layout, kind, after) = splitFormat(format)
    print(before)
    if kind != Char(0) {
      args[index].printFormatted(kind, layout)
      ++index
    }
    format = after
  }
}

//===----------------------------------------------------------------------===//
// UnsafePointer<Int> Type
//===----------------------------------------------------------------------===//

/// FIXME: This should eventually be a generic UnsafePointer<T> type.
struct UnsafePointerInt {
  var value : Builtin.RawPointer

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

func [infix_left=90,assignment] += (lhs : [byref] UnsafePointerInt, rhs : Int64) {
  lhs = lhs + rhs
}

func [infix_left=90,assignment] -= (lhs : [byref] UnsafePointerInt, rhs : Int64) {
  lhs = lhs - rhs
}

func [assignment] ++(a : [byref] UnsafePointerInt) { a += 1 }
func [assignment] --(a : [byref] UnsafePointerInt) { a -= 1 }

func == (lhs : UnsafePointerInt, rhs : UnsafePointerInt) -> Bool {
  return _getBool(Builtin.cmp_eq_RawPointer(lhs.value, rhs.value))
}

func != (lhs : UnsafePointerInt, rhs : UnsafePointerInt) -> Bool {
  return _getBool(Builtin.cmp_ne_RawPointer(lhs.value, rhs.value))
}

func < (lhs : UnsafePointerInt, rhs : UnsafePointerInt) -> Bool {
  return _getBool(Builtin.cmp_ult_RawPointer(lhs.value, rhs.value))
}

func <= (lhs : UnsafePointerInt, rhs : UnsafePointerInt) -> Bool {
  return _getBool(Builtin.cmp_ule_RawPointer(lhs.value, rhs.value))
}

func > (lhs : UnsafePointerInt, rhs : UnsafePointerInt) -> Bool {
  return _getBool(Builtin.cmp_ugt_RawPointer(lhs.value, rhs.value))
}

func >= (lhs : UnsafePointerInt, rhs : UnsafePointerInt) -> Bool {
  return _getBool(Builtin.cmp_uge_RawPointer(lhs.value, rhs.value))
}

//===----------------------------------------------------------------------===//
// UnsafePointer<Int8> Type
//===----------------------------------------------------------------------===//

/// FIXME: This should eventually be a generic UnsafePointer<T> type.
struct UnsafePointerInt8 {
  var value : Builtin.RawPointer

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

func [infix_left=190] - (lhs : UnsafePointerInt8,
                         rhs : UnsafePointerInt8) -> Int {
  return Int(Builtin.sub_Int64(Builtin.ptrtoint_Int64(lhs.value),
                               Builtin.ptrtoint_Int64(rhs.value)))
}

func [assignment] ++(a : [byref] UnsafePointerInt8) { a += 1 }
func [assignment] --(a : [byref] UnsafePointerInt8) { a -= 1 }

func [infix_left=90,assignment] += (lhs : [byref] UnsafePointerInt8, 
                                    rhs : Int64) {
  lhs = lhs + rhs
}

func [infix_left=90,assignment] -= (lhs : [byref] UnsafePointerInt8,
                                    rhs : Int64) {
  lhs = lhs - rhs
}

func == (lhs : UnsafePointerInt8, rhs : UnsafePointerInt8) -> Bool {
  return _getBool(Builtin.cmp_eq_RawPointer(lhs.value, rhs.value))
}

func != (lhs : UnsafePointerInt8, rhs : UnsafePointerInt8) -> Bool {
  return _getBool(Builtin.cmp_ne_RawPointer(lhs.value, rhs.value))
}

func < (lhs : UnsafePointerInt8, rhs : UnsafePointerInt8) -> Bool {
  return _getBool(Builtin.cmp_ult_RawPointer(lhs.value, rhs.value))
}

func <= (lhs : UnsafePointerInt8, rhs : UnsafePointerInt8) -> Bool {
  return _getBool(Builtin.cmp_ule_RawPointer(lhs.value, rhs.value))
}

func > (lhs : UnsafePointerInt8, rhs : UnsafePointerInt8) -> Bool {
  return _getBool(Builtin.cmp_ugt_RawPointer(lhs.value, rhs.value))
}

func >= (lhs : UnsafePointerInt8, rhs : UnsafePointerInt8) -> Bool {
  return _getBool(Builtin.cmp_uge_RawPointer(lhs.value, rhs.value))
}

//===----------------------------------------------------------------------===//
// Slice<Int64> Type
//===----------------------------------------------------------------------===//

/// FIXME: This should eventually be a generic Slice<T> type.
struct SliceInt64 {
   var base : UnsafePointerInt,
   length : Int,
   owner : Builtin.ObjectPointer

   static func convertFromHeapArray(base : Builtin.RawPointer,
                                    owner : Builtin.ObjectPointer,
                                    length : Builtin.Int64) -> SliceInt64 {
     return SliceInt64(UnsafePointerInt(base), Int(length), owner)
   }

  func getElements() -> SliceInt64 { return this }

   subscript (i : Int) -> Int64 {
     get {
       if i >= length {
         Builtin.trap()
       }

       return (base + i).get()
     }

     set {
       if i >= length {
         Builtin.trap()
       }

       (base + i).set(value)
     }
   }

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

  // Slicing via subscripting with a range.
  subscript (rng : IntRange) -> Int[] {
    get {
      if !(rng.min <= length && rng.max <= length) {
        Builtin.trap()
      }
      return SliceInt64(base + rng.min, rng.max - rng.min, owner)
    }

    set {
      if !(value.length == rng.max - rng.min) {
        Builtin.trap()
      }

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

/// \brief SliceInt64 is a range.
extension SliceInt64 : Range {
  typealias Element = Int

  func isEmpty() -> Bool { return length == 0 }
  func getFirstAndAdvance() -> Int {
    var prev = base.get()
    base = base + 1
    length = length - 1
    return prev
  }
}

//===----------------------------------------------------------------------===//
// Slice<UInt64> Type
//===----------------------------------------------------------------------===//

/// FIXME: This should eventually be a generic Slice<T> type.
struct SliceUInt64 {
   var base : UnsafePointerInt,
   length : Int,
   owner : Builtin.ObjectPointer

   static func convertFromHeapArray(base : Builtin.RawPointer,
                                    owner : Builtin.ObjectPointer,
                                    length : Builtin.Int64) -> SliceUInt64 {
     return SliceUInt64(UnsafePointerInt(base), Int(length), owner)
   }

  func getElements() -> SliceUInt64 { return this }

   subscript (i : Int) -> UInt64 {
     get {
       if i >= length {
         Builtin.trap()
       }

       return UInt64((base + i).get())
     }

     set {
       if i >= length {
         Builtin.trap()
       }

       (base + i).set(Int64(value))
     }
   }

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

  // Slicing via subscripting with a range.
  subscript (rng : IntRange) -> UInt64[] {
    get {
      if !(rng.min <= length && rng.max <= length) {
        Builtin.trap()
      }
      return SliceUInt64(base + rng.min, rng.max - rng.min, owner)
    }

    set {
      if !(value.length == rng.max - rng.min) {
        Builtin.trap()
      }

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
          destEnd.set(Int64(i))
        }

        return
      }

      // Copy the data.
      for i in value {
        destStart.set(Int64(i))
        ++destStart
      }
    }
  }

  func each(f : (UInt64) -> Void) {
    for i in this { f(i) }
  }

  func reduce(val : Int, f : (Int, UInt64) -> Int) -> Int {
    for i in this { val = f(val, i) }
    return val
  }

  func map(f : (UInt64) -> UInt64) -> UInt64[] {
    var r = new UInt64[length]
    for i in 0 .. length { r[i] = f(this[i]) }
    return r
  }
}

/// \brief SliceUInt64 is a range.
extension SliceUInt64 : Range {
  typealias Element = UInt64

  func isEmpty() -> Bool { return length == 0 }
  func getFirstAndAdvance() -> Element {
    var prev = base.get()
    base = base + 1
    length = length - 1
    return UInt64(prev)
  }
}

struct SliceSliceInt64 {
   var base : UnsafePointerInt,
   length : Int,
   owner : Builtin.ObjectPointer

   static func convertFromHeapArray(base : Builtin.RawPointer,
                                    owner : Builtin.ObjectPointer,
                                    length : Builtin.Int64) -> SliceSliceInt64 {
     return SliceSliceInt64(UnsafePointerInt(base), Int(length), owner)
   }

   subscript (i : Int) -> SliceInt64 {
     get {
       if i >= length {
         Builtin.trap()
       }

       var ptr = base + 3 * i // Each SliceInt64 is 3 64-bit values
       return SliceInt64.convertFromHeapArray(
                Builtin.load_RawPointer(ptr.value),
                Builtin.load_ObjectPointer((ptr + 2).value),
                Builtin.load_Int64((ptr + 1).value))
     }

     set {
       if i >= length {
         Builtin.trap()
       }

       var ptr = base + 3 * i // Each SliceInt64 is 3 64-bit values
       Builtin.assign_RawPointer(value.base.value, ptr.value)
       Builtin.assign_Int64(value.length.value, (ptr + 1).value)
       Builtin.assign_ObjectPointer(value.owner, (ptr + 2).value)
     }
   }

   func getElements() -> SliceSliceInt64 { return this }

   func replPrint() {
     var total = 0
     print('[')
     var first = true
     for i in this { 
       if first {
         first = false
       } else {
         print(", ")
       }
       print('[')
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
         if total > 50 {
           print("...], ...]")
           return ()
         }
         x = x + 1
       }
       print(']')
     }
     print(']')
   }

  // Slicing via subscripting with a range.
  subscript (rng : IntRange) -> Int[][] {
    get {
      if !(rng.min <= length && rng.max <= length) {
        Builtin.trap()
      }
      return SliceSliceInt64(base + rng.min*3, rng.max - rng.min, owner)
    }

    set {
      if !(value.length == rng.max - rng.min) {
        Builtin.trap()
      }

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

extension SliceSliceInt64 : Range {
  typealias Element = SliceInt64
  func isEmpty() ->Bool { return length == 0 }
  func getFirstAndAdvance() -> SliceInt64 { 
    var prev = base
    base = base + 3
    length = length - 1
    return SliceInt64.convertFromHeapArray(
             Builtin.load_RawPointer(prev.value),
             Builtin.load_ObjectPointer((prev + 2).value),
             Builtin.load_Int64((prev + 1).value))
  }
}

struct SliceString {
   var base : UnsafePointerInt,
   length : Int,
   owner : Builtin.ObjectPointer

   static func convertFromHeapArray(base : Builtin.RawPointer,
                                    owner : Builtin.ObjectPointer,
                                    length : Builtin.Int64) -> SliceString {
     return SliceString(UnsafePointerInt(base), Int(length) & Int64.max(), owner)
   }

   subscript (i : Int) -> String {
     get {
       if i >= length {
         Builtin.trap()
       }

       var ptr = base + 3 * i // Each String is 3 64-bit values
       return String(SliceStringByte.convertFromHeapArray(
                Builtin.load_RawPointer(ptr.value),
                Builtin.load_ObjectPointer((ptr + 2).value),
                Builtin.load_Int64((ptr + 1).value)))
     }

     set {
       if i >= length {
         Builtin.trap()
       }

       var ptr = base + 3 * i // Each String is 3 64-bit values
       Builtin.assign_RawPointer(value.str_value.base.value, ptr.value)
       Builtin.assign_Int64(value.str_value.getLength().value, (ptr + 1).value)
       Builtin.assign_ObjectPointer(value.str_value.owner, (ptr + 2).value)
     }
   }

   func getElements() -> SliceString { return this }

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

  func each(f : (String) -> Void) {
    for i in this { f(i) }
  }

  func reduce(val : String, f : (String, String) -> String) -> String {
    for i in this { val = f(val, i) }
    return val
  }
  
  func map(f : (String) -> String) -> String[] {
    var r = new String[length]
    for i in 0 .. length { r[i] = f(this[i]) }
    return r
  }
}

extension SliceString : Range {
  typealias Element = String

  func isEmpty() ->Bool { return length == 0 }
  func getFirstAndAdvance() -> String {
    var prev = base
    base = base + 3
    length = length - 1
    return String(SliceStringByte.convertFromHeapArray(
             Builtin.load_RawPointer(prev.value),
             Builtin.load_ObjectPointer((prev + 2).value),
             Builtin.load_Int64((prev + 1).value)))
  }
}

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

struct SliceUInt8 {
   var base : UnsafePointerInt8,
   length : Int,
   owner : Builtin.ObjectPointer

   static func convertFromHeapArray(base : Builtin.RawPointer,
                                    owner : Builtin.ObjectPointer,
                                    length : Builtin.Int64) -> SliceUInt8 {
     return SliceUInt8(UnsafePointerInt8(base), Int(length), owner)
   }

   subscript (i : Int) -> UInt8 {
     get {
       if i >= length {
         Builtin.trap()
       }
       return UInt8((base + i).get())
      }

     set {
       if i >= length {
         Builtin.trap()
       }
       (base + i).set(Int8(value))
     }
   }

   func getElements() -> SliceUInt8 { return this }

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
   
  // Slicing via subscripting with a range.
  subscript (rng : IntRange) -> UInt8[] {
    get {
      if !(rng.min <= length && rng.max <= length) {
        Builtin.trap()
      }
      return SliceUInt8(base + rng.min, rng.max - rng.min, owner)
    }

    set {
      if !(value.length == rng.max - rng.min) {
        Builtin.trap()
      }

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
          destEnd.set(Int8(i))
        }

        return
      }

      // Copy the data.
      for i in value {
        destStart.set(Int8(i))
        ++destStart
      }
    }
  }

  func each(f : (UInt8) -> Void) {
    for i in this { f(i) }
  }

  func reduce(val : UInt8, f : (UInt8, UInt8) -> UInt8) -> UInt8 {
    for i in this { val = f(val, i) }
    return val
  }

  func map(f : (UInt8) -> UInt8) -> UInt8[] {
    var r = new UInt8[length]
    for i in 0 .. length { r[i] = f(this[i]) }
    return r
  }
}

extension SliceUInt8 : Range {
  typealias Element = UInt8

  func isEmpty() -> Bool { return length == 0 }
  func getFirstAndAdvance() -> UInt8 {
    var prev = this[0]
    base = base + 1
    length = length - 1
    return prev
  }
}

// StringByte

struct StringByte {
  var value : Builtin.Int8
  
  static func convertFromIntegerLiteral(val : Builtin.Int8) -> StringByte {
    return StringByte(val)
  }

  func replPrint() {
    print(UInt64(this))
  }
  // FIXME:
  // static var max : StringByte { get { return 0xFF } }
  // static var min : StringByte { get { return 0 } }
  static func max() -> StringByte { return 0xFF }
  static func min() -> StringByte { return 0 }
}

func [infix_left=190] + (lhs: StringByte, rhs: StringByte) -> StringByte {
  return StringByte(Builtin.add_Int8(lhs.value, rhs.value))
}

func [infix_left=190] - (lhs: StringByte, rhs: StringByte) -> StringByte {
  return StringByte(Builtin.sub_Int8(lhs.value, rhs.value))
}

func [infix=160] == (lhs : StringByte, rhs : StringByte) -> Bool {
  return _getBool(Builtin.cmp_eq_Int8(lhs.value, rhs.value))
}

func [infix=160] != (lhs : StringByte, rhs : StringByte) -> Bool {
  return !(lhs == rhs)
}

func [infix=170] < (lhs : StringByte, rhs : StringByte) -> Bool {
  return _getBool(Builtin.cmp_ult_Int8(lhs.value, rhs.value))
}

func [infix=170] > (lhs : StringByte, rhs : StringByte) -> Bool {
  return rhs < lhs
}

func [infix=170] <= (lhs : StringByte, rhs : StringByte) -> Bool {
  return !(rhs < lhs)
}

func [infix=170] >= (lhs : StringByte, rhs : StringByte) -> Bool {
  return !(lhs < rhs)
}

func [infix_left=150] & (lhs: StringByte, rhs: StringByte) -> StringByte {
  return StringByte(Builtin.and_Int8(lhs.value, rhs.value))
}

func [infix_left=130] | (lhs: StringByte, rhs: StringByte) -> StringByte {
  return StringByte(Builtin.or_Int8(lhs.value, rhs.value))
}

extension StringByte : FormattedPrintable {
  func printFormatted(kind : Char, layout : String) {
    UInt64(this).printFormatted(kind, layout)
  }
}

extension StringByte {
  static func StringByte(v : Int8) -> StringByte {
    return StringByte(v.value)
  }
  static func StringByte(v : UInt32) -> StringByte {
    return StringByte(Builtin.trunc_Int32_Int8(v.value))
  }
}

extension Int8 {
  static func Int8(v : StringByte) -> Int8 {
    return Int8(v.value)
  }
}

extension Int32 {
  static func Int32(v : StringByte) -> Int32 {
    return Int32(Builtin.zext_Int8_Int32(v.value))
  }
}

extension UInt32 {
  static func UInt32(v : StringByte) -> UInt32 {
    return UInt32(Builtin.zext_Int8_Int32(v.value))
  }
}

extension UInt64 {
  static func UInt64(v : StringByte) -> UInt64 {
    return UInt64(Builtin.zext_Int8_Int64(v.value))
  }
}

// SliceStringByte

struct SliceStringByte {
   var base : UnsafePointerInt8,
   length : Int,
   owner : Builtin.ObjectPointer

   static func convertFromHeapArray(base : Builtin.RawPointer,
                                    owner : Builtin.ObjectPointer,
                                    length : Builtin.Int64) -> SliceStringByte {
     return SliceStringByte(UnsafePointerInt8(base), Int(length), owner)
   }

   func getLength() -> Int {
      return length & Int.max()
   }

   func setLength(len : Int) {
      length &= Int.min()
      length |= (len & Int.max())
   }

   func isASCII() -> Bool {
     return (length & Int64.min()) == 0
   }

   func setASCII(b : Bool) {
     if b {
       length &= Int64.max()
     } else {
       length |= Int64.min()
     }
   }

   subscript (i : Int) -> StringByte {
     get {
       if i >= getLength() {
         Builtin.trap()
       }
       return StringByte((base + i).get())
      }

     set {
       if i >= getLength() {
         Builtin.trap()
       }
       (base + i).set(Int8(value))
     }
   }

   func getElements() -> SliceStringByte { return this }

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
   
  // Slicing via subscripting with a range.
  subscript (rng : IntRange) -> StringByte[] {
    get {
      var len = getLength()
      if !(rng.min <= len && rng.max <= len) {
        Builtin.trap()
      }
      return SliceStringByte(base + rng.min, rng.max - rng.min, owner)
    }

    set {
      var len = value.getLength()
      if !(len == rng.max - rng.min) {
        Builtin.trap()
      }

      // Common case: the elements were updated in place, so we do not have to
      // perform any updates.
      var destStart = base + rng.min
      if value.base == destStart {
        return
      }

      // If the start of the destination slice falls inside the source slice,
      // copy backwards.
      if destStart >= value.base && destStart < value.base + len {
        var destEnd = destStart + len
        for i in value {
          --destEnd
          destEnd.set(Int8(i))
        }

        return
      }

      // Copy the data.
      for i in value {
        destStart.set(Int8(i))
        ++destStart
      }
    }
  }

  func each(f : (StringByte) -> Void) {
    for i in this { f(i) }
  }

  func reduce(val : StringByte, f : (StringByte, StringByte) -> StringByte) -> StringByte {
    for i in this { val = f(val, i) }
    return val
  }

  func map(f : (StringByte) -> StringByte) -> StringByte[] {
    var len = getLength()
    var r = new StringByte[len]
    for i in 0 .. len { r[i] = f(this[i]) }
    return r
  }
}

extension SliceStringByte : Range {
  typealias Element = StringByte

  func isEmpty() -> Bool { return getLength() == 0 }
  func getFirstAndAdvance() -> StringByte {
    var prev = this[0]
    base = base + 1
    setLength(getLength() - 1)
    return prev
  }
}

struct SliceFormattedPrintable {
  var base : UnsafePointerInt
  var length : Int
  var owner : Builtin.ObjectPointer

  static func convertFromHeapArray(base : Builtin.RawPointer,
                                   owner : Builtin.ObjectPointer,
                                   length : Builtin.Int64) 
                -> SliceFormattedPrintable {
    return SliceFormattedPrintable(UnsafePointerInt(base), Int(length), owner)
  }

  func getElements() -> SliceFormattedPrintable { return this }

  subscript (i : Int) -> FormattedPrintable {
    get {
      if i >= length {
        Builtin.trap()
      }

      var ptr = base + 3 * i // Each FormattedPrintable is 3 pointers
      return load_FormattedPrintable(ptr)
    }

    set {
      if i >= length {
        Builtin.trap()
      }

      var ptr = base + 3 * i // Each FormattedPrintable is 3 pointers
      store_FormattedPrintable(value, ptr)
    }
  }
}

extension SliceFormattedPrintable : Range {
  typealias Element = FormattedPrintable
  func isEmpty() -> Bool { return length == 0 }
  func getFirstAndAdvance() -> FormattedPrintable {
      var prev = base
      base = base + 3  // Each FormattedPrintable is 3 pointers
      length = length - 1
      return load_FormattedPrintable(prev)
  }
}

//===----------------------------------------------------------------------===//
// Range protocol and types
//===----------------------------------------------------------------------===//

/// \brief Describes iteration over a 
protocol Range {
  typealias Element
  func isEmpty() -> Bool
  func getFirstAndAdvance() -> Element
}

struct IntRange : Range {
  typealias Element = Int
  var min : Int,
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
  func getFirstAndAdvance() -> Int {
    var prev = min
    ++min
    return prev
  }

  func getElements() -> IntRange { return this }

  func replPrint() {
    print(min)
    print("..")
    print(max)
  }
}

struct ReverseIntRange : Range {
  typealias Element = Int

  var min : Int,
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
  func getFirstAndAdvance() -> Int { 
    --max
    return max
  }

  func getElements() -> ReverseIntRange { return this }

  func replPrint() {
    print("reverse(")
    print(min)
    print("..")
    print(max)
    print(')')
  }
}

func reverse(rng : IntRange) -> ReverseIntRange {
  return ReverseIntRange(rng.min, rng.max)
}

func reverse(rng : ReverseIntRange) -> IntRange {
  return IntRange(rng.min, rng.max)
}

func [infix_left=110] .. (min : Int, max : Int) -> IntRange {
  return IntRange(min, max)
}

struct DoubleRange : Range {
  typealias Element = Double

  var min : Double,
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
  func getFirstAndAdvance() -> Double { 
    var prev = min
    min = min + stride
    return prev
  }

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

func swap(a : [byref] UInt64, b : [byref] UInt64) {
  var c = a
  a = b
  b = c
}

func assert(fn : [auto_closure] () -> Bool) {
  if !fn() {
    abort()
  }
}

// Some very basic output functions.
func print(val : Int)
func print(val : UInt64)
func print(val : Double)
func print(val : String) {
  var len = val.byteLength()
  for var i = 0; i < len; ++i {
    c_putchar(Int32(val.str_value[i]))
  }
}
func print(val : Char) {
  var wc = UInt32(val)
  if wc < 0x000080 {
    c_putchar(Int32(wc))
    return
  } else if wc < 0x000800 {
    c_putchar(Int32(0xC0 | (wc >> 6)))
    c_putchar(Int32(0x80 | (wc & 0x03F)))
    return
  } else if wc < 0x010000 {
    if !(0x00D800 <= wc && wc < 0x00E000) {
      c_putchar(Int32(0xE0 |  (wc >> 12)))
      c_putchar(Int32(0x80 | ((wc & 0x0FC0) >> 6)))
      c_putchar(Int32(0x80 |  (wc & 0x003F)))
      return
    }
  } else if wc < 0x110000 {
    c_putchar(Int32(0xF0 |  (wc >> 18)))
    c_putchar(Int32(0x80 | ((wc & 0x03F000) >> 12)))
    c_putchar(Int32(0x80 | ((wc & 0x000FC0) >> 6)))
    c_putchar(Int32(0x80 |  (wc & 0x00003F)))
    return
  }
  print(Char(0xFFFD))
}

func print(val : Bool) {
  if val {
    print("true")
  } else {
    print("false")
  }
}

// Some derived output functions.
func println(val : Bool) {
  print(val)
  print('\n')
}
func println(val : Int) {
  print(val)
  print('\n')
}
func println(val : UInt64) {
  print(val)
  print('\n')
}
func println(val : Double) {
  print(val)
  print('\n')
}
func println(val : String) {
  print(val)
  print('\n')
}

func println(val : Char) {
  print(val)
  print('\n')
}

func min(x : Int, y : Int, rest : Int...) -> Int {
  var r = x
  if (y < x) {
    r = y
  }
  for z in rest {
    if z < r {
      r = z
    }
  }
  return r;
}

func max(x : Int, y : Int, rest : Int...) -> Int {
  var r = y
  if (y < x) {
    r = x
  }
  for z in rest {
    if z >= r {
      r = z
    }
  }
  return r;
}

//===----------------------------------------------------------------------===//
// Standard protocols
//===----------------------------------------------------------------------===//

typealias Any = protocol<>

func [asmname="load_protocol"] 
load_FormattedPrintable(p : UnsafePointerInt) -> FormattedPrintable

func [asmname="store_protocol"] 
store_FormattedPrintable(value : FormattedPrintable, p : UnsafePointerInt)

//===----------------------------------------------------------------------===//
// Objective-C interactions
//===----------------------------------------------------------------------===//

// This violates the naming convention but looks really wrong as Id.
struct id {
  var value : Builtin.ObjCPointer
}

func [asmname="NSLog"]
NSLog(str : NSString, obj : NSString)

func [asmname="swift_NSStringToString"]
convertNSStringToString(object : id, string : [byref] String)

// NSString type for experiments with bridging.  It will eventually be replaced
// with a class type.
struct NSString {
  var value : id

  func [conversion] __conversion() -> String {
    var result : String
    convertNSStringToString(value, &result)
    return result
  }

  // FIXME: Fast-path in compiler/runtime to build a CFConstantString
  static func convertFromStringLiteral(val : Builtin.RawPointer) -> NSString {
    return String.convertFromStringLiteral(val)
  }

  func replPrint() {
    print(String(this))
  }
}

//===----------------------------------------------------------------------===//
// Extern C functions
//===----------------------------------------------------------------------===//

// FIXME: Once we have an FFI interface, make these have proper function bodies

// The C "exit" and "abort" functions
func [asmname="exit"] exit(exitCode : Int32)
func [asmname="abort"] abort()

func [asmname="putchar"] c_putchar(val : Int32)
func [asmname="print_int"] c_print_int(p : Builtin.RawPointer, buf_len : Int,
                                       x : Int128, Radix : Int) -> UInt64
func [asmname="print_double"] c_print_double(p : Builtin.RawPointer, x : Double)
                                                                       -> UInt64

// Some math stuff.
func [asmname="sqrtf"] sqrt(a : Float) -> Float
func [asmname="sqrt"] sqrt(a : Double) -> Double
func [asmname="sinf"] sin(a : Float) -> Float
func [asmname="sin"] sin(a : Double) -> Double
func [asmname="cosf"] cos(a : Float) -> Float
func [asmname="cos"] cos(a : Double) -> Double
func [asmname="atan2f"] atan2(y : Float, x : Float) -> Float
func [asmname="atan2"] atan2(y : Double, x : Double) -> Double


func [asmname="mach_absolute_time"] mach_absolute_time() -> UInt64

func [asmname="swift_replOutputIsUTF8"] _isUTF8() -> Bool
