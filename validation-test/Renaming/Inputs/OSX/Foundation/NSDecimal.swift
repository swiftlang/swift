
enum NSRoundingMode : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case roundPlain
  case roundDown
  case roundUp
  case roundBankers
}
enum NSCalculationError : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case noError
  case lossOfPrecision
  case underflow
  case overflow
  case divideByZero
}
var NSDecimalMaxSize: Int32 { get }
var NSDecimalNoScale: Int32 { get }
struct NSDecimal {
  var _exponent: Int32
  var _length: UInt32
  var _isNegative: UInt32
  var _isCompact: UInt32
  var _reserved: UInt32
  var _mantissa: (UInt16, UInt16, UInt16, UInt16, UInt16, UInt16, UInt16, UInt16)
  init()
  init(_exponent _exponent: Int32, _length _length: UInt32, _isNegative _isNegative: UInt32, _isCompact _isCompact: UInt32, _reserved _reserved: UInt32, _mantissa _mantissa: (UInt16, UInt16, UInt16, UInt16, UInt16, UInt16, UInt16, UInt16))
}
@discardableResult
func NSDecimalIsNotANumber(_ dcm: UnsafePointer<NSDecimal>) -> Bool
func NSDecimalCopy(_ destination: UnsafeMutablePointer<NSDecimal>, _ source: UnsafePointer<NSDecimal>)
func NSDecimalCompact(_ number: UnsafeMutablePointer<NSDecimal>)
@discardableResult
func NSDecimalCompare(_ leftOperand: UnsafePointer<NSDecimal>, _ rightOperand: UnsafePointer<NSDecimal>) -> NSComparisonResult
func NSDecimalRound(_ result: UnsafeMutablePointer<NSDecimal>, _ number: UnsafePointer<NSDecimal>, _ scale: Int, _ roundingMode: NSRoundingMode)
@discardableResult
func NSDecimalNormalize(_ number1: UnsafeMutablePointer<NSDecimal>, _ number2: UnsafeMutablePointer<NSDecimal>, _ roundingMode: NSRoundingMode) -> NSCalculationError
@discardableResult
func NSDecimalAdd(_ result: UnsafeMutablePointer<NSDecimal>, _ leftOperand: UnsafePointer<NSDecimal>, _ rightOperand: UnsafePointer<NSDecimal>, _ roundingMode: NSRoundingMode) -> NSCalculationError
@discardableResult
func NSDecimalSubtract(_ result: UnsafeMutablePointer<NSDecimal>, _ leftOperand: UnsafePointer<NSDecimal>, _ rightOperand: UnsafePointer<NSDecimal>, _ roundingMode: NSRoundingMode) -> NSCalculationError
@discardableResult
func NSDecimalMultiply(_ result: UnsafeMutablePointer<NSDecimal>, _ leftOperand: UnsafePointer<NSDecimal>, _ rightOperand: UnsafePointer<NSDecimal>, _ roundingMode: NSRoundingMode) -> NSCalculationError
@discardableResult
func NSDecimalDivide(_ result: UnsafeMutablePointer<NSDecimal>, _ leftOperand: UnsafePointer<NSDecimal>, _ rightOperand: UnsafePointer<NSDecimal>, _ roundingMode: NSRoundingMode) -> NSCalculationError
@discardableResult
func NSDecimalPower(_ result: UnsafeMutablePointer<NSDecimal>, _ number: UnsafePointer<NSDecimal>, _ power: Int, _ roundingMode: NSRoundingMode) -> NSCalculationError
@discardableResult
func NSDecimalMultiplyByPowerOf10(_ result: UnsafeMutablePointer<NSDecimal>, _ number: UnsafePointer<NSDecimal>, _ power: Int16, _ roundingMode: NSRoundingMode) -> NSCalculationError
@discardableResult
func NSDecimalString(_ dcm: UnsafePointer<NSDecimal>, _ locale: AnyObject?) -> String
