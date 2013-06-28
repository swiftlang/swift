struct Int8 : BuiltinIntegerLiteralConvertible, IntegerLiteralConvertible {
  var value : Builtin.Int8

  static func _convertFromBuiltinIntegerLiteral(val : Builtin.Int128) -> Int8 {
    return Int8(Builtin.trunc_Int128_Int8(val))
  }

  typealias IntegerLiteralType = Int8
  static func convertFromIntegerLiteral(value : Int8) -> Int8 {
    return value
  }

  func replPrint() {
    print(Int64(this))
  }
  // FIXME:
  // static var max : Int8 { get: return 0x7F }
  // static var min : Int8 { get: return -0x7F-1 }
  static func max() -> Int8 { return 0x7F }
  static func min() -> Int8 { return -0x7F-1 }
}

extension Int8 : RandomAccessIndex, Hashable {
  func __equal__(rhs: Int8) -> Bool {
    return _getBool(Builtin.cmp_eq_Int8(value, rhs.value))
  }
  func __less__(rhs: Int8) -> Bool {
    return _getBool(Builtin.cmp_slt_Int8(value, rhs.value))
  }
  func succ() -> Int8 {
    return __add__(1)
  }
  func pred() -> Int8 {
    return __sub__(1)
  }
  typealias DistanceType = Int8
  func __sub__(rhs: Int8) -> DistanceType {
    return Int8(Builtin.sub_Int8(value, rhs.value))
  }
  func __add__(offset: DistanceType) -> Int8 {
    return Int8(Builtin.add_Int8(value, offset.value))
  }
  func hashValue() -> Int {
    return Int(this)
  }
}

extension Int8 : SignedNumber {
  func __negate__() -> Int8 { return 0 - this }
  func isNegative() -> Bool { return this < 0 }
}

struct UInt8 : BuiltinIntegerLiteralConvertible, IntegerLiteralConvertible {
  var value : Builtin.Int8

  static func _convertFromBuiltinIntegerLiteral(val : Builtin.Int128) -> UInt8 {
    return UInt8(Builtin.trunc_Int128_Int8(val))
  }

  typealias IntegerLiteralType = UInt8
  static func convertFromIntegerLiteral(value : UInt8) -> UInt8 {
    return value
  }

  func replPrint() {
    print(UInt64(this))
  }
  // FIXME:
  // static var max : UInt8 { get: return 0xFF }
  // static var min : UInt8 { get: return 0 }
  static func max() -> UInt8 { return 0xFF }
  static func min() -> UInt8 { return 0 }
}

extension UInt8 : RandomAccessIndex, Hashable {
  func __equal__(rhs: UInt8) -> Bool {
    return _getBool(Builtin.cmp_eq_Int8(value, rhs.value))
  }
  func __less__(rhs: UInt8) -> Bool {
    return _getBool(Builtin.cmp_ult_Int8(value, rhs.value))
  }
  func succ() -> UInt8 {
    return this + 1
  }
  func pred() -> UInt8 {
    return this - 1
  }
  func hashValue() -> Int {
    return Int(UInt(this).value)
  }
  typealias DistanceType = UInt8
  func __sub__(rhs: UInt8) -> DistanceType {
    return UInt8(Builtin.sub_Int8(value, rhs.value))
  }
  func __add__(offset: DistanceType) -> UInt8 {
    return UInt8(Builtin.add_Int8(value, offset.value))
  }
}

struct Int16 : BuiltinIntegerLiteralConvertible, IntegerLiteralConvertible {
  var value : Builtin.Int16

  static func _convertFromBuiltinIntegerLiteral(val : Builtin.Int128) -> Int16 {
    return Int16(Builtin.trunc_Int128_Int16(val))
  }

  typealias IntegerLiteralType = Int16
  static func convertFromIntegerLiteral(value : Int16) -> Int16 {
    return value
  }

  func replPrint() {
    print(Int64(this))
  }
  // FIXME:
  // static var max : Int16 { get: return 0x7FFF }
  // static var min : Int16 { get: return -0x7FFF-1 }
  static func max() -> Int16 { return 0x7FFF }
  static func min() -> Int16 { return -0x7FFF-1 }
}

extension Int16 : RandomAccessIndex, Hashable {
  func __equal__(rhs: Int16) -> Bool {
    return _getBool(Builtin.cmp_eq_Int16(value, rhs.value))
  }
  func __less__(rhs: Int16) -> Bool {
    return _getBool(Builtin.cmp_slt_Int16(value, rhs.value))
  }
  func succ() -> Int16 {
    return this + 1
  }
  func pred() -> Int16 {
    return this - 1
  }
  func hashValue() -> Int {
    return Int(this)
  }
  typealias DistanceType = Int16
  func __sub__(rhs: Int16) -> DistanceType {
    return Int16(Builtin.sub_Int16(value, rhs.value))
  }
  func __add__(offset: DistanceType) -> Int16 {
    return Int16(Builtin.add_Int16(value, offset.value))
  }
}

extension Int16 : SignedNumber {
  func __negate__() -> Int16 { return 0 - this }
  func isNegative() -> Bool { return this < 0 }
}

struct UInt16 : BuiltinIntegerLiteralConvertible, IntegerLiteralConvertible {
  var value : Builtin.Int16

  static func _convertFromBuiltinIntegerLiteral(val : Builtin.Int128) -> UInt16 {
    return UInt16(Builtin.trunc_Int128_Int16(val))
  }

  typealias IntegerLiteralType = UInt16
  static func convertFromIntegerLiteral(value : UInt16) -> UInt16 {
    return value
  }

  func replPrint() {
    print(UInt64(this))
  }
  // FIXME:
  // static var max : UInt16 { get: return 0xFFFF }
  // static var min : UInt16 { get: return 0 }
  static func max() -> UInt16 { return 0xFFFF }
  static func min() -> UInt16 { return 0 }
}

extension UInt16 : RandomAccessIndex, Hashable {
  func __equal__(rhs: UInt16) -> Bool {
    return _getBool(Builtin.cmp_eq_Int16(value, rhs.value))
  }
  func __less__(rhs: UInt16) -> Bool {
    return _getBool(Builtin.cmp_ult_Int16(value, rhs.value))
  }
  func succ() -> UInt16 {
    return this + 1
  }
  func pred() -> UInt16 {
    return this - 1
  }
  func hashValue() -> Int {
    return Int(UInt(this).value)
  }
  typealias DistanceType = UInt16
  func __sub__(rhs: UInt16) -> DistanceType {
    return UInt16(Builtin.sub_Int16(value, rhs.value))
  }
  func __add__(offset: DistanceType) -> UInt16 {
    return UInt16(Builtin.add_Int16(value, offset.value))
  }
}

struct Int32 : BuiltinIntegerLiteralConvertible, IntegerLiteralConvertible, ArrayBound {
  var value : Builtin.Int32

  static func _convertFromBuiltinIntegerLiteral(val : Builtin.Int128) -> Int32 {
    return Int32(Builtin.trunc_Int128_Int32(val))
  }

  typealias IntegerLiteralType = Int32
  static func convertFromIntegerLiteral(value : Int32) -> Int32 {
    return value
  }

  func _getBuiltinArrayBoundValue() -> Builtin.Int32 {
    return value
  }

  typealias ArrayBoundType = Int32
  func getArrayBoundValue() -> Int32 {
    return this
  }

  func replPrint() {
    print(Int64(this))
  }
  // FIXME:
  // static var max : Int32 { get: return 0x7FFFFFFF }
  // static var min : Int32 { get: return -0x7FFFFFFF-1 }
  static func max() -> Int32 { return 0x7FFFFFFF }
  static func min() -> Int32 { return -0x7FFFFFFF-1 }
}

extension Int32 : RandomAccessIndex, Hashable {
  func __equal__(rhs: Int32) -> Bool {
    return _getBool(Builtin.cmp_eq_Int32(value, rhs.value))
  }
  func __less__(rhs: Int32) -> Bool {
    return _getBool(Builtin.cmp_slt_Int32(value, rhs.value))
  }
  func succ() -> Int32 {
    return this + 1
  }
  func pred() -> Int32 {
    return this - 1
  }
  func hashValue() -> Int {
    return Int(this)
  }
  typealias DistanceType = Int32
  func __sub__(rhs: Int32) -> DistanceType {
    return Int32(Builtin.sub_Int32(value, rhs.value))
  }
  func __add__(offset: DistanceType) -> Int32 {
    return Int32(Builtin.add_Int32(value, offset.value))
  }
}

extension Int32 : SignedNumber {
  func __negate__() -> Int32 { return 0 - this }
  func isNegative() -> Bool { return this < 0 }
}

struct UInt32 : BuiltinIntegerLiteralConvertible, IntegerLiteralConvertible {
  var value : Builtin.Int32

  static func _convertFromBuiltinIntegerLiteral(val : Builtin.Int128) -> UInt32 {
    return UInt32(Builtin.trunc_Int128_Int32(val))
  }

  typealias IntegerLiteralType = UInt32
  static func convertFromIntegerLiteral(value : UInt32) -> UInt32 {
    return value
  }

  func replPrint() {
    print(UInt64(this))
  }
  // FIXME:
  // static var max : UInt32 { get: return 0xFFFFFFFF }
  // static var min : UInt32 { get: return 0 }
  static func max() -> UInt32 { return 0xFFFFFFFF }
  static func min() -> UInt32 { return 0 }
}

extension UInt32 : RandomAccessIndex, Hashable {
  func __equal__(rhs: UInt32) -> Bool {
    return _getBool(Builtin.cmp_eq_Int32(value, rhs.value))
  }
  func __less__(rhs: UInt32) -> Bool {
    return _getBool(Builtin.cmp_ult_Int32(value, rhs.value))
  }
  func succ() -> UInt32 {
    return this + 1
  }
  func pred() -> UInt32 {
    return this - 1
  }
  func hashValue() -> Int {
    return Int(UInt(this).value)
  }
  typealias DistanceType = UInt32
  func __sub__(rhs: UInt32) -> DistanceType {
    return UInt32(Builtin.sub_Int32(value, rhs.value))
  }
  func __add__(offset: DistanceType) -> UInt32 {
    return UInt32(Builtin.add_Int32(value, offset.value))
  }
}

struct Int64 : BuiltinIntegerLiteralConvertible, IntegerLiteralConvertible, ArrayBound {
  var value : Builtin.Int64

  static func _convertFromBuiltinIntegerLiteral(val : Builtin.Int128) -> Int64 {
    return Int64(Builtin.trunc_Int128_Int64(val))
  }

  typealias IntegerLiteralType = Int64
  static func convertFromIntegerLiteral(value : Int64) -> Int64 {
    return value
  }

  func _getBuiltinArrayBoundValue() -> Builtin.Int64 {
    return value
  }

  typealias ArrayBoundType = Int64
  func getArrayBoundValue() -> Int64 {
    return this
  }

  func replPrint() {
    print(this)
  }
  // FIXME:
  // static var max : Int64 { get: return 0x7FFFFFFFFFFFFFFF }
  // static var min : Int64 { get: return -0x7FFFFFFFFFFFFFFF-1 }
  static func max() -> Int64 { return 0x7FFFFFFFFFFFFFFF }
  static func min() -> Int64 { return -0x7FFFFFFFFFFFFFFF-1 }
}

extension Int64 : RandomAccessIndex, Hashable {
  func __equal__(rhs: Int64) -> Bool {
    return _getBool(Builtin.cmp_eq_Int64(value, rhs.value))
  }
  func __less__(rhs: Int64) -> Bool {
    return _getBool(Builtin.cmp_slt_Int64(value, rhs.value))
  }
  func succ() -> Int64 {
    return __add__(1)
  }
  func pred() -> Int64 {
    return __sub__(1)
  }
  typealias DistanceType = Int64
  func __sub__(rhs: Int64) -> DistanceType {
    return Int64(Builtin.sub_Int64(value, rhs.value))
  }
  func __add__(offset: DistanceType) -> Int64 {
    return Int64(Builtin.add_Int64(value, offset.value))
  }
  func hashValue() -> Int {
    return this
  }
}

extension Int64 : SignedNumber {
  func __negate__() -> Int64 { return 0 - this }
  func isNegative() -> Bool { return this < 0 }
}

struct UInt64 : BuiltinIntegerLiteralConvertible, IntegerLiteralConvertible {
  var value : Builtin.Int64

  static func _convertFromBuiltinIntegerLiteral(val : Builtin.Int128) -> UInt64 {
    return UInt64(Builtin.trunc_Int128_Int64(val))
  }

  typealias IntegerLiteralType = UInt64
  static func convertFromIntegerLiteral(value : UInt64) -> UInt64 {
    return value
  }

  func _getBuiltinArrayBoundValue() -> Builtin.Int64 {
    return value
  }

  typealias ArrayBoundType = UInt64
  func getArrayBoundValue() -> UInt64 {
    return this
  }

  func replPrint() {
    print(this)
  }
  // FIXME:
  // static var max : UInt64 { get: return 0xFFFFFFFFFFFFFFFF }
  // static var min : UInt64 { get: return 0 }
  static func max() -> UInt64 { return 0xFFFFFFFFFFFFFFFF }
  static func min() -> UInt64 { return 0 }
}

extension UInt64 : RandomAccessIndex, Hashable {
  func __equal__(rhs: UInt64) -> Bool {
    return _getBool(Builtin.cmp_eq_Int64(value, rhs.value))
  }
  func __less__(rhs: UInt64) -> Bool {
    return _getBool(Builtin.cmp_ult_Int64(value, rhs.value))
  }
  func succ() -> UInt64 {
    return this + 1
  }
  func pred() -> UInt64 {
    return this - 1
  }
  func hashValue() -> Int {
    return Int(value)
  }
  typealias DistanceType = UInt64
  func __sub__(rhs: UInt64) -> DistanceType {
    return UInt64(Builtin.sub_Int64(value, rhs.value))
  }
  func __add__(offset: DistanceType) -> UInt64 {
    return UInt64(Builtin.add_Int64(value, offset.value))
  }
}

struct Int128 : BuiltinIntegerLiteralConvertible, IntegerLiteralConvertible {
  var value : Builtin.Int128

  static func _convertFromBuiltinIntegerLiteral(val : Builtin.Int128) -> Int128 {
    return Int128(val)
  }

  typealias IntegerLiteralType = Int128
  static func convertFromIntegerLiteral(value : Int128) -> Int128 {
    return value
  }

  func replPrint() {
    print(String(this))
  }
  // FIXME:
  // static var max : Int128 { get: return 0x7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF }
  // static var min : Int128 { get: return -0x7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF-1 }
  static func max() -> Int128 { return 0x7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF }
  static func min() -> Int128 { return -0x7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF-1 }
}

extension Int128 : RandomAccessIndex, Hashable {
  func __equal__(rhs: Int128) -> Bool {
    return _getBool(Builtin.cmp_eq_Int128(value, rhs.value))
  }
  func __less__(rhs: Int128) -> Bool {
    return _getBool(Builtin.cmp_slt_Int128(value, rhs.value))
  }
  func succ() -> Int128 {
    return this + 1
  }
  func pred() -> Int128 {
    return this - 1
  }
  func hashValue() -> Int {
    return Int(this >> 64) ^ Int(this & 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)
  }
  typealias DistanceType = Int128
  func __sub__(rhs: Int128) -> DistanceType {
    return Int128(Builtin.sub_Int128(value, rhs.value))
  }
  func __add__(offset: DistanceType) -> Int128 {
    return Int128(Builtin.add_Int128(value, offset.value))
  }
}

extension Int128 : SignedNumber {
  func __negate__() -> Int128 { return 0 - this }
  func isNegative() -> Bool { return this < 0 }
}

struct UInt128 : BuiltinIntegerLiteralConvertible, IntegerLiteralConvertible {
  var value : Builtin.Int128

  static func _convertFromBuiltinIntegerLiteral(val : Builtin.Int128) -> UInt128 {
    return UInt128(val)
  }

  typealias IntegerLiteralType = UInt128
  static func convertFromIntegerLiteral(value : UInt128) -> UInt128 {
    return value
  }

  func replPrint() {
    print(String(this))
  }
  // FIXME:
  // static var max : UInt128 { get: return 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF }
  // static var min : UInt128 { get: return 0 }
  static func max() -> UInt128 { return 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF }
  static func min() -> UInt128 { return 0 }
}

extension UInt128 : RandomAccessIndex, Hashable {
  func __equal__(rhs: UInt128) -> Bool {
    return _getBool(Builtin.cmp_eq_Int128(value, rhs.value))
  }
  func __less__(rhs: UInt128) -> Bool {
    return _getBool(Builtin.cmp_ult_Int128(value, rhs.value))
  }
  func succ() -> UInt128 {
    return this + 1
  }
  func pred() -> UInt128 {
    return this - 1
  }
  func hashValue() -> Int {
    return Int(this >> 64) ^ Int(this & 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)
  }
  typealias DistanceType = UInt128
  func __sub__(rhs: UInt128) -> DistanceType {
    return UInt128(Builtin.sub_Int128(value, rhs.value))
  }
  func __add__(offset: DistanceType) -> UInt128 {
    return UInt128(Builtin.add_Int128(value, offset.value))
  }
}

//===----------------------------------------------------------------------===//
// Explicit conversions between types.
//===----------------------------------------------------------------------===//

extension Int8 {
  constructor(v : UInt8) {
    value = v.value
  }
  constructor(v : Int16) {
    value = Builtin.trunc_Int16_Int8(v.value)
  }
  constructor(v : UInt16) {
    value = Builtin.trunc_Int16_Int8(v.value)
  }
  constructor(v : Int32) {
    value = Builtin.trunc_Int32_Int8(v.value)
  }
  constructor(v : UInt32) {
    value = Builtin.trunc_Int32_Int8(v.value)
  }
  constructor(v : Int64) {
    value = Builtin.trunc_Int64_Int8(v.value)
  }
  constructor(v : UInt64) {
    value = Builtin.trunc_Int64_Int8(v.value)
  }
  constructor(v : Int128) {
    value = Builtin.trunc_Int128_Int8(v.value)
  }
  constructor(v : UInt128) {
    value = Builtin.trunc_Int128_Int8(v.value)
  }
}

extension UInt8 {
  constructor(v : Int8) {
    value = v.value
  }
  constructor(v : Int16) {
    value = Builtin.trunc_Int16_Int8(v.value)
  }
  constructor(v : UInt16) {
    value = Builtin.trunc_Int16_Int8(v.value)
  }
  constructor(v : Int32) {
    value = Builtin.trunc_Int32_Int8(v.value)
  }
  constructor(v : UInt32) {
    value = Builtin.trunc_Int32_Int8(v.value)
  }
  constructor(v : Int64) {
    value = Builtin.trunc_Int64_Int8(v.value)
  }
  constructor(v : UInt64) {
    value = Builtin.trunc_Int64_Int8(v.value)
  }
  constructor(v : Int128) {
    value = Builtin.trunc_Int128_Int8(v.value)
  }
  constructor(v : UInt128) {
    value = Builtin.trunc_Int128_Int8(v.value)
  }
}

extension Int16 {
  constructor(v : Int8) {
    value = Builtin.sext_Int8_Int16(v.value)
  }
  constructor(v : UInt8) {
    value = Builtin.zext_Int8_Int16(v.value)
  }
  constructor(v : UInt16) {
    value = v.value
  }
  constructor(v : Int32) {
    value = Builtin.trunc_Int32_Int16(v.value)
  }
  constructor(v : UInt32) {
    value = Builtin.trunc_Int32_Int16(v.value)
  }
  constructor(v : Int64) {
    value = Builtin.trunc_Int64_Int16(v.value)
  }
  constructor(v : UInt64) {
    value = Builtin.trunc_Int64_Int16(v.value)
  }
  constructor(v : Int128) {
    value = Builtin.trunc_Int128_Int16(v.value)
  }
  constructor(v : UInt128) {
    value = Builtin.trunc_Int128_Int16(v.value)
  }
}

extension UInt16 {
  constructor(v : Int8) {
    value = Builtin.sext_Int8_Int16(v.value)
  }
  constructor(v : UInt8) {
    value = Builtin.zext_Int8_Int16(v.value)
  }
  constructor(v : Int16) {
    value = v.value
  }
  constructor(v : Int32) {
    value = Builtin.trunc_Int32_Int16(v.value)
  }
  constructor(v : UInt32) {
    value = Builtin.trunc_Int32_Int16(v.value)
  }
  constructor(v : Int64) {
    value = Builtin.trunc_Int64_Int16(v.value)
  }
  constructor(v : UInt64) {
    value = Builtin.trunc_Int64_Int16(v.value)
  }
  constructor(v : Int128) {
    value = Builtin.trunc_Int128_Int16(v.value)
  }
  constructor(v : UInt128) {
    value = Builtin.trunc_Int128_Int16(v.value)
  }
}

extension Int32 {
  constructor(v : Int8) {
    value = Builtin.sext_Int8_Int32(v.value)
  }
  constructor(v : UInt8) {
    value = Builtin.zext_Int8_Int32(v.value)
  }
  constructor(v : Int16) {
    value = Builtin.sext_Int16_Int32(v.value)
  }
  constructor(v : UInt16) {
    value = Builtin.zext_Int16_Int32(v.value)
  }
  constructor(v : UInt32) {
    value = v.value
  }
  constructor(v : Int64) {
    value = Builtin.trunc_Int64_Int32(v.value)
  }
  constructor(v : UInt64) {
    value = Builtin.trunc_Int64_Int32(v.value)
  }
  constructor(v : Int128) {
    value = Builtin.trunc_Int128_Int32(v.value)
  }
  constructor(v : UInt128) {
    value = Builtin.trunc_Int128_Int32(v.value)
  }
}

extension UInt32 {
  constructor(v : Int8) {
    value = Builtin.sext_Int8_Int32(v.value)
  }
  constructor(v : UInt8) {
    value = Builtin.zext_Int8_Int32(v.value)
  }
  constructor(v : Int16) {
    value = Builtin.sext_Int16_Int32(v.value)
  }
  constructor(v : UInt16) {
    value = Builtin.zext_Int16_Int32(v.value)
  }
  constructor(v : Int32) {
    value = v.value
  }
  constructor(v : Int64) {
    value = Builtin.trunc_Int64_Int32(v.value)
  }
  constructor(v : UInt64) {
    value = Builtin.trunc_Int64_Int32(v.value)
  }
  constructor(v : Int128) {
    value = Builtin.trunc_Int128_Int32(v.value)
  }
  constructor(v : UInt128) {
    value = Builtin.trunc_Int128_Int32(v.value)
  }
}

extension Int64 {
  constructor(v : Int8) {
    value = Builtin.sext_Int8_Int64(v.value)
  }
  constructor(v : UInt8) {
    value = Builtin.zext_Int8_Int64(v.value)
  }
  constructor(v : Int16) {
    value = Builtin.sext_Int16_Int64(v.value)
  }
  constructor(v : UInt16) {
    value = Builtin.zext_Int16_Int64(v.value)
  }
  constructor(v : Int32) {
    value = Builtin.sext_Int32_Int64(v.value)
  }
  constructor(v : UInt32) {
    value = Builtin.zext_Int32_Int64(v.value)
  }
  constructor(v : UInt64) {
    value = v.value
  }
  constructor(v : Int128) {
    value = Builtin.trunc_Int128_Int64(v.value)
  }
  constructor(v : UInt128) {
    value = Builtin.trunc_Int128_Int64(v.value)
  }
}

extension UInt64 {
  constructor(v : Int8) {
    value = Builtin.sext_Int8_Int64(v.value)
  }
  constructor(v : UInt8) {
    value = Builtin.zext_Int8_Int64(v.value)
  }
  constructor(v : Int16) {
    value = Builtin.sext_Int16_Int64(v.value)
  }
  constructor(v : UInt16) {
    value = Builtin.zext_Int16_Int64(v.value)
  }
  constructor(v : Int32) {
    value = Builtin.sext_Int32_Int64(v.value)
  }
  constructor(v : UInt32) {
    value = Builtin.zext_Int32_Int64(v.value)
  }
  constructor(v : Int64) {
    value = v.value
  }
  constructor(v : Int128) {
    value = Builtin.trunc_Int128_Int64(v.value)
  }
  constructor(v : UInt128) {
    value = Builtin.trunc_Int128_Int64(v.value)
  }
}

extension Int128 {
  constructor(v : Int8) {
    value = Builtin.sext_Int8_Int128(v.value)
  }
  constructor(v : UInt8) {
    value = Builtin.zext_Int8_Int128(v.value)
  }
  constructor(v : Int16) {
    value = Builtin.sext_Int16_Int128(v.value)
  }
  constructor(v : UInt16) {
    value = Builtin.zext_Int16_Int128(v.value)
  }
  constructor(v : Int32) {
    value = Builtin.sext_Int32_Int128(v.value)
  }
  constructor(v : UInt32) {
    value = Builtin.zext_Int32_Int128(v.value)
  }
  constructor(v : Int64) {
    value = Builtin.sext_Int64_Int128(v.value)
  }
  constructor(v : UInt64) {
    value = Builtin.zext_Int64_Int128(v.value)
  }
  constructor(v : UInt128) {
    value = v.value
  }
}

extension UInt128 {
  constructor(v : Int8) {
    value = Builtin.sext_Int8_Int128(v.value)
  }
  constructor(v : UInt8) {
    value = Builtin.zext_Int8_Int128(v.value)
  }
  constructor(v : Int16) {
    value = Builtin.sext_Int16_Int128(v.value)
  }
  constructor(v : UInt16) {
    value = Builtin.zext_Int16_Int128(v.value)
  }
  constructor(v : Int32) {
    value = Builtin.sext_Int32_Int128(v.value)
  }
  constructor(v : UInt32) {
    value = Builtin.zext_Int32_Int128(v.value)
  }
  constructor(v : Int64) {
    value = Builtin.sext_Int64_Int128(v.value)
  }
  constructor(v : UInt64) {
    value = Builtin.zext_Int64_Int128(v.value)
  }
  constructor(v : Int128) {
    value = v.value
  }
}

//===----------------------------------------------------------------------===//
// Standard Operators
//===----------------------------------------------------------------------===//

// Unary addition operators.
func [prefix] +(a : Int8)    -> Int8   { return a }
func [prefix] +(a : UInt8)   -> UInt8  { return a }
func [prefix] +(a : Int16)   -> Int16  { return a }
func [prefix] +(a : UInt16)  -> UInt16 { return a }
func [prefix] +(a : Int32)   -> Int32  { return a }
func [prefix] +(a : UInt32)  -> UInt32 { return a }
func [prefix] +(a : Int64)   -> Int64  { return a }
func [prefix] +(a : UInt64)  -> UInt64 { return a }
func [prefix] +(a : Int128)  -> Int128 { return a }
func [prefix] +(a : UInt128) -> UInt128 { return a }

// Bitwise negation operators.
func [prefix] ~(a : Int8)    -> Int8   { return a ^ -1 }
func [prefix] ~(a : UInt8)   -> UInt8  { return a ^ 0xFF }
func [prefix] ~(a : Int16)   -> Int16  { return a ^ -1 }
func [prefix] ~(a : UInt16)  -> UInt16 { return a ^ 0xFFFF }
func [prefix] ~(a : Int32)   -> Int32  { return a ^ -1 }
func [prefix] ~(a : UInt32)  -> UInt32 { return a ^ 0xFFFFFFFF }
func [prefix] ~(a : Int64)   -> Int64  { return a ^ -1 }
func [prefix] ~(a : UInt64)  -> UInt64 { return a ^ 0xFFFFFFFFFFFFFFFF }
func [prefix] ~(a : Int128)  -> Int128 { return a ^ -1 }
func [prefix] ~(a : UInt128) -> UInt128 { return a ^ 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF }

// Binary Multiplication.
func * (lhs : Int8, rhs : Int8) -> Int8 {
  return Int8(Builtin.mul_Int8(lhs.value, rhs.value))
}
func * (lhs : UInt8, rhs : UInt8) -> UInt8 {
  return UInt8(Builtin.mul_Int8(lhs.value, rhs.value))
}
func  * (lhs : Int16, rhs : Int16) -> Int16 {
  return Int16(Builtin.mul_Int16(lhs.value, rhs.value))
}
func * (lhs : UInt16, rhs : UInt16) -> UInt16 {
  return UInt16(Builtin.mul_Int16(lhs.value, rhs.value))
}
func * (lhs : Int32, rhs : Int32) -> Int32 {
  return Int32(Builtin.mul_Int32(lhs.value, rhs.value))
}
func * (lhs : UInt32, rhs : UInt32) -> UInt32 {
  return UInt32(Builtin.mul_Int32(lhs.value, rhs.value))
}
func * (lhs : Int64, rhs : Int64) -> Int64 {
  return Int64(Builtin.mul_Int64(lhs.value, rhs.value))
}
func * (lhs : UInt64, rhs : UInt64) -> UInt64 {
  return UInt64(Builtin.mul_Int64(lhs.value, rhs.value))
}
func * (lhs : Int128, rhs : Int128) -> Int128 {
  return Int128(Builtin.mul_Int128(lhs.value, rhs.value))
}
func * (lhs : UInt128, rhs : UInt128) -> UInt128 {
  return UInt128(Builtin.mul_Int128(lhs.value, rhs.value))
}


// Binary Division.
func / (lhs : Int8, rhs : Int8) -> Int8 {
  return Int8(Builtin.sdiv_Int8(lhs.value, rhs.value))
}
func / (lhs : UInt8, rhs : UInt8) -> UInt8 {
  return UInt8(Builtin.udiv_Int8(lhs.value, rhs.value))
}
func / (lhs : Int16, rhs : Int16) -> Int16 {
  return Int16(Builtin.sdiv_Int16(lhs.value, rhs.value))
}
func / (lhs : UInt16, rhs : UInt16) -> UInt16 {
  return UInt16(Builtin.udiv_Int16(lhs.value, rhs.value))
}
func / (lhs : Int32, rhs : Int32) -> Int32 {
  return Int32(Builtin.sdiv_Int32(lhs.value, rhs.value))
}
func / (lhs : UInt32, rhs : UInt32) -> UInt32 {
  return UInt32(Builtin.udiv_Int32(lhs.value, rhs.value))
}
func / (lhs : Int64, rhs : Int64) -> Int64 {
  return Int64(Builtin.sdiv_Int64(lhs.value, rhs.value))
}
func / (lhs : UInt64, rhs : UInt64) -> UInt64 {
  return UInt64(Builtin.udiv_Int64(lhs.value, rhs.value))
}
func / (lhs : Int128, rhs : Int128) -> Int128 {
  return Int128(Builtin.sdiv_Int128(lhs.value, rhs.value))
}
func / (lhs : UInt128, rhs : UInt128) -> UInt128 {
  return UInt128(Builtin.udiv_Int128(lhs.value, rhs.value))
}

// Binary Remainder.
func % (lhs : Int8, rhs : Int8) -> Int8 {
  return Int8(Builtin.srem_Int8(lhs.value, rhs.value))
}
func % (lhs : UInt8, rhs : UInt8) -> UInt8 {
  return UInt8(Builtin.urem_Int8(lhs.value, rhs.value))
}
func % (lhs : Int16, rhs : Int16) -> Int16 {
  return Int16(Builtin.srem_Int16(lhs.value, rhs.value))
}
func % (lhs : UInt16, rhs : UInt16) -> UInt16 {
  return UInt16(Builtin.urem_Int16(lhs.value, rhs.value))
}
func % (lhs : Int32, rhs : Int32) -> Int32 {
  return Int32(Builtin.srem_Int32(lhs.value, rhs.value))
}
func % (lhs : UInt32, rhs : UInt32) -> UInt32 {
  return UInt32(Builtin.urem_Int32(lhs.value, rhs.value))
}
func % (lhs : Int64, rhs : Int64) -> Int64 {
  return Int64(Builtin.srem_Int64(lhs.value, rhs.value))
}
func % (lhs : UInt64, rhs : UInt64) -> UInt64 {
  return UInt64(Builtin.urem_Int64(lhs.value, rhs.value))
}
func % (lhs : Int128, rhs : Int128) -> Int128 {
  return Int128(Builtin.srem_Int128(lhs.value, rhs.value))
}
func % (lhs : UInt128, rhs : UInt128) -> UInt128 {
  return UInt128(Builtin.urem_Int128(lhs.value, rhs.value))
}


// Binary Addition.
func + (lhs: Int8, rhs: Int8) -> Int8 {
  return Int8(Builtin.add_Int8(lhs.value, rhs.value))
}
func + (lhs: UInt8, rhs: UInt8) -> UInt8 {
  return UInt8(Builtin.add_Int8(lhs.value, rhs.value))
}
func + (lhs: Int16, rhs: Int16) -> Int16 {
  return Int16(Builtin.add_Int16(lhs.value, rhs.value))
}
func + (lhs: UInt16, rhs: UInt16) -> UInt16 {
  return UInt16(Builtin.add_Int16(lhs.value, rhs.value))
}
func + (lhs: Int32, rhs: Int32) -> Int32 {
  return Int32(Builtin.add_Int32(lhs.value, rhs.value))
}
func + (lhs: UInt32, rhs: UInt32) -> UInt32 {
  return UInt32(Builtin.add_Int32(lhs.value, rhs.value))
}
func + (lhs: Int64, rhs: Int64) -> Int64 {
  return Int64(Builtin.add_Int64(lhs.value, rhs.value))
}
func + (lhs: UInt64, rhs: UInt64) -> UInt64 {
  return UInt64(Builtin.add_Int64(lhs.value, rhs.value))
}
func + (lhs: Int128, rhs: Int128) -> Int128 {
  return Int128(Builtin.add_Int128(lhs.value, rhs.value))
}
func + (lhs: UInt128, rhs: UInt128) -> UInt128 {
  return UInt128(Builtin.add_Int128(lhs.value, rhs.value))
}


// Binary Subtraction.
func - (lhs: Int8, rhs: Int8) -> Int8 {
  return Int8(Builtin.sub_Int8(lhs.value, rhs.value))
}
func - (lhs: UInt8, rhs: UInt8) -> UInt8 {
  return UInt8(Builtin.sub_Int8(lhs.value, rhs.value))
}
func - (lhs: Int16, rhs: Int16) -> Int16 {
  return Int16(Builtin.sub_Int16(lhs.value, rhs.value))
}
func - (lhs: UInt16, rhs: UInt16) -> UInt16 {
  return UInt16(Builtin.sub_Int16(lhs.value, rhs.value))
}
func - (lhs: Int32, rhs: Int32) -> Int32 {
  return Int32(Builtin.sub_Int32(lhs.value, rhs.value))
}
func - (lhs: UInt32, rhs: UInt32) -> UInt32 {
  return UInt32(Builtin.sub_Int32(lhs.value, rhs.value))
}
func - (lhs: Int64, rhs: Int64) -> Int64 {
  return Int64(Builtin.sub_Int64(lhs.value, rhs.value))
}
func - (lhs: UInt64, rhs: UInt64) -> UInt64 {
  return UInt64(Builtin.sub_Int64(lhs.value, rhs.value))
}
func - (lhs: Int128, rhs: Int128) -> Int128 {
  return Int128(Builtin.sub_Int128(lhs.value, rhs.value))
}
func - (lhs: UInt128, rhs: UInt128) -> UInt128 {
  return UInt128(Builtin.sub_Int128(lhs.value, rhs.value))
}

// Left Shift.
func << (lhs : Int8, rhs : Int8) -> Int8 {
  return Int8(Builtin.shl_Int8(lhs.value, rhs.value))
}
func << (lhs : UInt8, rhs : UInt8) -> UInt8 {
  return UInt8(Builtin.shl_Int8(lhs.value, rhs.value))
}
func << (lhs : Int16, rhs : Int16) -> Int16 {
  return Int16(Builtin.shl_Int16(lhs.value, rhs.value))
}
func << (lhs : UInt16, rhs : UInt16) -> UInt16 {
  return UInt16(Builtin.shl_Int16(lhs.value, rhs.value))
}
func << (lhs : Int32, rhs : Int32) -> Int32 {
  return Int32(Builtin.shl_Int32(lhs.value, rhs.value))
}
func << (lhs : UInt32, rhs : UInt32) -> UInt32 {
  return UInt32(Builtin.shl_Int32(lhs.value, rhs.value))
}
func << (lhs : Int64, rhs : Int64) -> Int64 {
  return Int64(Builtin.shl_Int64(lhs.value, rhs.value))
}
func << (lhs : UInt64, rhs : UInt64) -> UInt64 {
  return UInt64(Builtin.shl_Int64(lhs.value, rhs.value))
}
func << (lhs : Int128, rhs : Int128) -> Int128 {
  return Int128(Builtin.shl_Int128(lhs.value, rhs.value))
}
func << (lhs : UInt128, rhs : UInt128) -> UInt128 {
  return UInt128(Builtin.shl_Int128(lhs.value, rhs.value))
}

// Right Shift.
func >>(lhs : Int8, rhs : Int8) -> Int8 {
  return Int8(Builtin.ashr_Int8(lhs.value, rhs.value))
}
func >>(lhs : UInt8, rhs : UInt8) -> UInt8 {
  return UInt8(Builtin.lshr_Int8(lhs.value, rhs.value))
}
func >>(lhs : Int16, rhs : Int16) -> Int16 {
  return Int16(Builtin.ashr_Int16(lhs.value, rhs.value))
}
func >>(lhs : UInt16, rhs : UInt16) -> UInt16 {
  return UInt16(Builtin.lshr_Int16(lhs.value, rhs.value))
}
func >>(lhs : Int32, rhs : Int32) -> Int32 {
  return Int32(Builtin.ashr_Int32(lhs.value, rhs.value))
}
func >>(lhs : UInt32, rhs : UInt32) -> UInt32 {
  return UInt32(Builtin.lshr_Int32(lhs.value, rhs.value))
}
func >>(lhs : Int64, rhs : Int64) -> Int64 {
  return Int64(Builtin.ashr_Int64(lhs.value, rhs.value))
}
func >>(lhs : UInt64, rhs : UInt64) -> UInt64 {
  return UInt64(Builtin.lshr_Int64(lhs.value, rhs.value))
}
func >>(lhs : Int128, rhs : Int128) -> Int128 {
  return Int128(Builtin.ashr_Int128(lhs.value, rhs.value))
}
func >>(lhs : UInt128, rhs : UInt128) -> UInt128 {
  return UInt128(Builtin.lshr_Int128(lhs.value, rhs.value))
}

// Bitwise 'and'.
func & (lhs: Int8, rhs: Int8) -> Int8 {
  return Int8(Builtin.and_Int8(lhs.value, rhs.value))
}
func & (lhs: UInt8, rhs: UInt8) -> UInt8 {
  return UInt8(Builtin.and_Int8(lhs.value, rhs.value))
}
func & (lhs: Int16, rhs: Int16) -> Int16 {
  return Int16(Builtin.and_Int16(lhs.value, rhs.value))
}
func & (lhs: UInt16, rhs: UInt16) -> UInt16 {
  return UInt16(Builtin.and_Int16(lhs.value, rhs.value))
}
func & (lhs: Int32, rhs: Int32) -> Int32 {
  return Int32(Builtin.and_Int32(lhs.value, rhs.value))
}
func & (lhs: UInt32, rhs: UInt32) -> UInt32 {
  return UInt32(Builtin.and_Int32(lhs.value, rhs.value))
}
func & (lhs: Int64, rhs: Int64) -> Int64 {
  return Int64(Builtin.and_Int64(lhs.value, rhs.value))
}
func & (lhs: UInt64, rhs: UInt64) -> UInt64 {
  return UInt64(Builtin.and_Int64(lhs.value, rhs.value))
}
func & (lhs: Int128, rhs: Int128) -> Int128 {
  return Int128(Builtin.and_Int128(lhs.value, rhs.value))
}
func & (lhs: UInt128, rhs: UInt128) -> UInt128 {
  return UInt128(Builtin.and_Int128(lhs.value, rhs.value))
}

func ^ (lhs: Int8, rhs: Int8) -> Int8 {
  return Int8(Builtin.xor_Int8(lhs.value, rhs.value))
}
func ^ (lhs: UInt8, rhs: UInt8) -> UInt8 {
  return UInt8(Builtin.xor_Int8(lhs.value, rhs.value))
}
func ^ (lhs: Int16, rhs: Int16) -> Int16 {
  return Int16(Builtin.xor_Int16(lhs.value, rhs.value))
}
func ^ (lhs: UInt16, rhs: UInt16) -> UInt16 {
  return UInt16(Builtin.xor_Int16(lhs.value, rhs.value))
}
func ^ (lhs: Int32, rhs: Int32) -> Int32 {
  return Int32(Builtin.xor_Int32(lhs.value, rhs.value))
}
func ^ (lhs: UInt32, rhs: UInt32) -> UInt32 {
  return UInt32(Builtin.xor_Int32(lhs.value, rhs.value))
}
func ^ (lhs: Int64, rhs: Int64) -> Int64 {
  return Int64(Builtin.xor_Int64(lhs.value, rhs.value))
}
func ^ (lhs: UInt64, rhs: UInt64) -> UInt64 {
  return UInt64(Builtin.xor_Int64(lhs.value, rhs.value))
}
func ^ (lhs: Int128, rhs: Int128) -> Int128 {
  return Int128(Builtin.xor_Int128(lhs.value, rhs.value))
}
func ^ (lhs: UInt128, rhs: UInt128) -> UInt128 {
  return UInt128(Builtin.xor_Int128(lhs.value, rhs.value))
}

func | (lhs: Int8, rhs: Int8) -> Int8 {
  return Int8(Builtin.or_Int8(lhs.value, rhs.value))
}
func | (lhs: UInt8, rhs: UInt8) -> UInt8 {
  return UInt8(Builtin.or_Int8(lhs.value, rhs.value))
}
func | (lhs: Int16, rhs: Int16) -> Int16 {
  return Int16(Builtin.or_Int16(lhs.value, rhs.value))
}
func | (lhs: UInt16, rhs: UInt16) -> UInt16 {
  return UInt16(Builtin.or_Int16(lhs.value, rhs.value))
}
func | (lhs: Int32, rhs: Int32) -> Int32 {
  return Int32(Builtin.or_Int32(lhs.value, rhs.value))
}
func | (lhs: UInt32, rhs: UInt32) -> UInt32 {
  return UInt32(Builtin.or_Int32(lhs.value, rhs.value))
}
func | (lhs: Int64, rhs: Int64) -> Int64 {
  return Int64(Builtin.or_Int64(lhs.value, rhs.value))
}
func | (lhs: UInt64, rhs: UInt64) -> UInt64 {
  return UInt64(Builtin.or_Int64(lhs.value, rhs.value))
}
func | (lhs: Int128, rhs: Int128) -> Int128 {
  return Int128(Builtin.or_Int128(lhs.value, rhs.value))
}
func | (lhs: UInt128, rhs: UInt128) -> UInt128 {
  return UInt128(Builtin.or_Int128(lhs.value, rhs.value))
}

// See Bool.swift for && and ||
// In C, 120 is &&
// In C, 110 is ||

// In C, 100 is ?:
// In C, 90 is =, *=, += etc.

// Compound assignment (with subtraction)
func [assignment] -= (lhs : [byref] UInt8, rhs : UInt8) {
  lhs = lhs - rhs
}
func [assignment] -= (lhs : [byref] UInt16, rhs : UInt16) {
  lhs = lhs - rhs
}
func [assignment] -= (lhs : [byref] UInt32, rhs : UInt32) {
  lhs = lhs - rhs
}
func [assignment] -= (lhs : [byref] UInt64, rhs : UInt64) {
  lhs = lhs - rhs
}
func [assignment] -= (lhs : [byref] UInt128, rhs : UInt128) {
  lhs = lhs - rhs
}

// Compound assignment (with multiplication)
func [assignment] *= (lhs : [byref] Int8, rhs : Int8) {
  lhs = lhs * rhs
}
func [assignment] *= (lhs : [byref] UInt8, rhs : UInt8) {
  lhs = lhs * rhs
}
func [assignment] *= (lhs : [byref] Int16, rhs : Int16) {
  lhs = lhs * rhs
}
func [assignment] *= (lhs : [byref] UInt16, rhs : UInt16) {
  lhs = lhs * rhs
}
func [assignment] *= (lhs : [byref] Int32, rhs : Int32) {
  lhs = lhs * rhs
}
func [assignment] *= (lhs : [byref] UInt32, rhs : UInt32) {
  lhs = lhs * rhs
}
func [assignment] *= (lhs : [byref] Int64, rhs : Int64) {
  lhs = lhs * rhs
}
func [assignment] *= (lhs : [byref] UInt64, rhs : UInt64) {
  lhs = lhs * rhs
}
func [assignment] *= (lhs : [byref] Int128, rhs : Int128) {
  lhs = lhs * rhs
}
func [assignment] *= (lhs : [byref] UInt128, rhs : UInt128) {
  lhs = lhs * rhs
}

// Compound assignment (with division)
func [assignment] /= (lhs : [byref] Int8, rhs : Int8) {
  lhs = lhs / rhs
}
func [assignment] /= (lhs : [byref] UInt8, rhs : UInt8) {
  lhs = lhs / rhs
}
func [assignment] /= (lhs : [byref] Int16, rhs : Int16) {
  lhs = lhs / rhs
}
func [assignment] /= (lhs : [byref] UInt16, rhs : UInt16) {
  lhs = lhs / rhs
}
func [assignment] /= (lhs : [byref] Int32, rhs : Int32) {
  lhs = lhs / rhs
}
func [assignment] /= (lhs : [byref] UInt32, rhs : UInt32) {
  lhs = lhs / rhs
}
func [assignment] /= (lhs : [byref] Int64, rhs : Int64) {
  lhs = lhs / rhs
}
func [assignment] /= (lhs : [byref] UInt64, rhs : UInt64) {
  lhs = lhs / rhs
}
func [assignment] /= (lhs : [byref] Int128, rhs : Int128) {
  lhs = lhs / rhs
}
func [assignment] /= (lhs : [byref] UInt128, rhs : UInt128) {
  lhs = lhs / rhs
}

// Compound assignment (with remainder)
func [assignment] %= (lhs : [byref] Int8, rhs : Int8) {
  lhs = lhs % rhs
}
func [assignment] %= (lhs : [byref] UInt8, rhs : UInt8) {
  lhs = lhs % rhs
}
func [assignment] %= (lhs : [byref] Int16, rhs : Int16) {
  lhs = lhs % rhs
}
func [assignment] %= (lhs : [byref] UInt16, rhs : UInt16) {
  lhs = lhs % rhs
}
func [assignment] %= (lhs : [byref] Int32, rhs : Int32) {
  lhs = lhs % rhs
}
func [assignment] %= (lhs : [byref] UInt32, rhs : UInt32) {
  lhs = lhs % rhs
}
func [assignment] %= (lhs : [byref] Int64, rhs : Int64) {
  lhs = lhs % rhs
}
func [assignment] %= (lhs : [byref] UInt64, rhs : UInt64) {
  lhs = lhs % rhs
}
func [assignment] %= (lhs : [byref] Int128, rhs : Int128) {
  lhs = lhs % rhs
}
func [assignment] %= (lhs : [byref] UInt128, rhs : UInt128) {
  lhs = lhs % rhs
}

// Compound assignment (with left shift)
func [assignment] <<= (lhs : [byref] Int8, rhs : Int8) {
  lhs = lhs << rhs
}
func [assignment] <<= (lhs : [byref] UInt8, rhs : UInt8) {
  lhs = lhs << rhs
}
func [assignment] <<= (lhs : [byref] Int16, rhs : Int16) {
  lhs = lhs << rhs
}
func [assignment] <<= (lhs : [byref] UInt16, rhs : UInt16) {
  lhs = lhs << rhs
}
func [assignment] <<= (lhs : [byref] Int32, rhs : Int32) {
  lhs = lhs << rhs
}
func [assignment] <<= (lhs : [byref] UInt32, rhs : UInt32) {
  lhs = lhs << rhs
}
func [assignment] <<= (lhs : [byref] Int64, rhs : Int64) {
  lhs = lhs << rhs
}
func [assignment] <<= (lhs : [byref] UInt64, rhs : UInt64) {
  lhs = lhs << rhs
}
func [assignment] <<= (lhs : [byref] Int128, rhs : Int128) {
  lhs = lhs << rhs
}
func [assignment] <<= (lhs : [byref] UInt128, rhs : UInt128) {
  lhs = lhs << rhs
}

// Compound assignment (with right shift)
func [assignment] >>= (lhs : [byref] Int8, rhs : Int8) {
  lhs = lhs >> rhs
}
func [assignment] >>= (lhs : [byref] UInt8, rhs : UInt8) {
  lhs = lhs >> rhs
}
func [assignment] >>= (lhs : [byref] Int16, rhs : Int16) {
  lhs = lhs >> rhs
}
func [assignment] >>= (lhs : [byref] UInt16, rhs : UInt16) {
  lhs = lhs >> rhs
}
func [assignment] >>= (lhs : [byref] Int32, rhs : Int32) {
  lhs = lhs >> rhs
}
func [assignment] >>= (lhs : [byref] UInt32, rhs : UInt32) {
  lhs = lhs >> rhs
}
func [assignment] >>= (lhs : [byref] Int64, rhs : Int64) {
  lhs = lhs >> rhs
}
func [assignment] >>= (lhs : [byref] UInt64, rhs : UInt64) {
  lhs = lhs >> rhs
}
func [assignment] >>= (lhs : [byref] Int128, rhs : Int128) {
  lhs = lhs >> rhs
}
func [assignment] >>= (lhs : [byref] UInt128, rhs : UInt128) {
  lhs = lhs >> rhs
}

// Compound assignment (with bitwise and)
func [assignment] &= (lhs : [byref] Int8, rhs : Int8) {
  lhs = lhs & rhs
}
func [assignment] &= (lhs : [byref] UInt8, rhs : UInt8) {
  lhs = lhs & rhs
}
func [assignment] &= (lhs : [byref] Int16, rhs : Int16) {
  lhs = lhs & rhs
}
func [assignment] &= (lhs : [byref] UInt16, rhs : UInt16) {
  lhs = lhs & rhs
}
func [assignment] &= (lhs : [byref] Int32, rhs : Int32) {
  lhs = lhs & rhs
}
func [assignment] &= (lhs : [byref] UInt32, rhs : UInt32) {
  lhs = lhs & rhs
}
func [assignment] &= (lhs : [byref] Int64, rhs : Int64) {
  lhs = lhs & rhs
}
func [assignment] &= (lhs : [byref] UInt64, rhs : UInt64) {
  lhs = lhs & rhs
}
func [assignment] &= (lhs : [byref] Int128, rhs : Int128) {
  lhs = lhs & rhs
}
func [assignment] &= (lhs : [byref] UInt128, rhs : UInt128) {
  lhs = lhs & rhs
}

// Compound assignment (with bitwise or)
func [assignment] |= (lhs : [byref] Int8, rhs : Int8) {
  lhs = lhs | rhs
}
func [assignment] |= (lhs : [byref] UInt8, rhs : UInt8) {
  lhs = lhs | rhs
}
func [assignment] |= (lhs : [byref] Int16, rhs : Int16) {
  lhs = lhs | rhs
}
func [assignment] |= (lhs : [byref] UInt16, rhs : UInt16) {
  lhs = lhs | rhs
}
func [assignment] |= (lhs : [byref] Int32, rhs : Int32) {
  lhs = lhs | rhs
}
func [assignment] |= (lhs : [byref] UInt32, rhs : UInt32) {
  lhs = lhs | rhs
}
func [assignment] |= (lhs : [byref] Int64, rhs : Int64) {
  lhs = lhs | rhs
}
func [assignment] |= (lhs : [byref] UInt64, rhs : UInt64) {
  lhs = lhs | rhs
}
func [assignment] |= (lhs : [byref] Int128, rhs : Int128) {
  lhs = lhs | rhs
}
func [assignment] |= (lhs : [byref] UInt128, rhs : UInt128) {
  lhs = lhs | rhs
}

// Compound assignment (with bitwise exclusive or)
func [assignment] ^= (lhs : [byref] Int8, rhs : Int8) {
  lhs = lhs ^ rhs
}
func [assignment] ^= (lhs : [byref] UInt8, rhs : UInt8) {
  lhs = lhs ^ rhs
}
func [assignment] ^= (lhs : [byref] Int16, rhs : Int16) {
  lhs = lhs ^ rhs
}
func [assignment] ^= (lhs : [byref] UInt16, rhs : UInt16) {
  lhs = lhs ^ rhs
}
func [assignment] ^= (lhs : [byref] Int32, rhs : Int32) {
  lhs = lhs ^ rhs
}
func [assignment] ^= (lhs : [byref] UInt32, rhs : UInt32) {
  lhs = lhs ^ rhs
}
func [assignment] ^= (lhs : [byref] Int64, rhs : Int64) {
  lhs = lhs ^ rhs
}
func [assignment] ^= (lhs : [byref] UInt64, rhs : UInt64) {
  lhs = lhs ^ rhs
}
func [assignment] ^= (lhs : [byref] Int128, rhs : Int128) {
  lhs = lhs ^ rhs
}
func [assignment] ^= (lhs : [byref] UInt128, rhs : UInt128) {
  lhs = lhs ^ rhs
}
