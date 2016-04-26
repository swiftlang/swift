
let NSDecimalNumberExactnessException: String
let NSDecimalNumberOverflowException: String
let NSDecimalNumberUnderflowException: String
let NSDecimalNumberDivideByZeroException: String
protocol NSDecimalNumberBehaviors {
  @discardableResult
  func roundingMode() -> NSRoundingMode
  @discardableResult
  func scale() -> Int16
  @discardableResult
  func exceptionDuringOperation(_ operation: Selector, error error: NSCalculationError, leftOperand leftOperand: NSDecimalNumber, rightOperand rightOperand: NSDecimalNumber?) -> NSDecimalNumber?
}
class NSDecimalNumber : NSNumber {
  convenience init(mantissa mantissa: UInt64, exponent exponent: Int16, isNegative flag: Bool)
  init(decimal dcm: NSDecimal)
  convenience init(string numberValue: String?)
  convenience init(string numberValue: String?, locale locale: AnyObject?)
  @discardableResult
  class func zero() -> NSDecimalNumber
  @discardableResult
  class func one() -> NSDecimalNumber
  @discardableResult
  class func minimum() -> NSDecimalNumber
  @discardableResult
  class func maximum() -> NSDecimalNumber
  @discardableResult
  class func notANumber() -> NSDecimalNumber
  @discardableResult
  func adding(_ decimalNumber: NSDecimalNumber) -> NSDecimalNumber
  @discardableResult
  func adding(_ decimalNumber: NSDecimalNumber, withBehavior behavior: NSDecimalNumberBehaviors?) -> NSDecimalNumber
  @discardableResult
  func subtracting(_ decimalNumber: NSDecimalNumber) -> NSDecimalNumber
  @discardableResult
  func subtracting(_ decimalNumber: NSDecimalNumber, withBehavior behavior: NSDecimalNumberBehaviors?) -> NSDecimalNumber
  @discardableResult
  func multiplying(by decimalNumber: NSDecimalNumber) -> NSDecimalNumber
  @discardableResult
  func multiplying(by decimalNumber: NSDecimalNumber, withBehavior behavior: NSDecimalNumberBehaviors?) -> NSDecimalNumber
  @discardableResult
  func dividing(by decimalNumber: NSDecimalNumber) -> NSDecimalNumber
  @discardableResult
  func dividing(by decimalNumber: NSDecimalNumber, withBehavior behavior: NSDecimalNumberBehaviors?) -> NSDecimalNumber
  @discardableResult
  func raising(toPower power: Int) -> NSDecimalNumber
  @discardableResult
  func raising(toPower power: Int, withBehavior behavior: NSDecimalNumberBehaviors?) -> NSDecimalNumber
  @discardableResult
  func multiplying(byPowerOf10 power: Int16) -> NSDecimalNumber
  @discardableResult
  func multiplying(byPowerOf10 power: Int16, withBehavior behavior: NSDecimalNumberBehaviors?) -> NSDecimalNumber
  @discardableResult
  func rounding(accordingToBehavior behavior: NSDecimalNumberBehaviors?) -> NSDecimalNumber
  class func setDefaultBehavior(_ behavior: NSDecimalNumberBehaviors)
  @discardableResult
  class func defaultBehavior() -> NSDecimalNumberBehaviors
}
class NSDecimalNumberHandler : NSObject, NSDecimalNumberBehaviors, NSCoding {
  @discardableResult
  class func defaultDecimalNumberHandler() -> NSDecimalNumberHandler
  init(roundingMode roundingMode: NSRoundingMode, scale scale: Int16, raiseOnExactness exact: Bool, raiseOnOverflow overflow: Bool, raiseOnUnderflow underflow: Bool, raiseOnDivideByZero divideByZero: Bool)
}
extension NSNumber {
  var decimalValue: NSDecimal { get }
}
extension NSScanner {
  @discardableResult
  func scanDecimal(_ dcm: UnsafeMutablePointer<NSDecimal>?) -> Bool
}
