
@available(iOS 8.0, *)
class HKUnit : NSObject, NSSecureCoding, NSCopying {
  var unitString: String { get }
  convenience init(from string: String)
  convenience init(from massFormatterUnit: NSMassFormatterUnit)
  @discardableResult
  class func massFormatterUnit(from unit: HKUnit) -> NSMassFormatterUnit
  convenience init(from lengthFormatterUnit: NSLengthFormatterUnit)
  @discardableResult
  class func lengthFormatterUnit(from unit: HKUnit) -> NSLengthFormatterUnit
  convenience init(from energyFormatterUnit: NSEnergyFormatterUnit)
  @discardableResult
  class func energyFormatterUnit(from unit: HKUnit) -> NSEnergyFormatterUnit
  @discardableResult
  func isNull() -> Bool
}
@available(iOS 8.0, *)
enum HKMetricPrefix : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case none
  case pico
  case nano
  case micro
  case milli
  case centi
  case deci
  case deca
  case hecto
  case kilo
  case mega
  case giga
  case tera
}
extension HKUnit {
  @discardableResult
  class func gramUnit(with prefix: HKMetricPrefix) -> Self
  @discardableResult
  class func gram() -> Self
  @discardableResult
  class func ounce() -> Self
  @discardableResult
  class func pound() -> Self
  @discardableResult
  class func stone() -> Self
  @discardableResult
  class func moleUnit(with prefix: HKMetricPrefix, molarMass gramsPerMole: Double) -> Self
  @discardableResult
  class func moleUnit(withMolarMass gramsPerMole: Double) -> Self
}
extension HKUnit {
  @discardableResult
  class func meterUnit(with prefix: HKMetricPrefix) -> Self
  @discardableResult
  class func meter() -> Self
  @discardableResult
  class func inch() -> Self
  @discardableResult
  class func foot() -> Self
  @available(iOS 9.0, *)
  @discardableResult
  class func yard() -> Self
  @discardableResult
  class func mile() -> Self
}
extension HKUnit {
  @discardableResult
  class func literUnit(with prefix: HKMetricPrefix) -> Self
  @discardableResult
  class func liter() -> Self
  @discardableResult
  class func fluidOunceUS() -> Self
  @discardableResult
  class func fluidOunceImperial() -> Self
  @discardableResult
  class func pintUS() -> Self
  @discardableResult
  class func pintImperial() -> Self
  @available(iOS 9.0, *)
  @discardableResult
  class func cupUS() -> Self
  @available(iOS 9.0, *)
  @discardableResult
  class func cupImperial() -> Self
}
extension HKUnit {
  @discardableResult
  class func pascalUnit(with prefix: HKMetricPrefix) -> Self
  @discardableResult
  class func pascal() -> Self
  @discardableResult
  class func millimeterOfMercury() -> Self
  @discardableResult
  class func centimeterOfWater() -> Self
  @discardableResult
  class func atmosphere() -> Self
}
extension HKUnit {
  @discardableResult
  class func secondUnit(with prefix: HKMetricPrefix) -> Self
  @discardableResult
  class func second() -> Self
  @discardableResult
  class func minute() -> Self
  @discardableResult
  class func hour() -> Self
  @discardableResult
  class func day() -> Self
}
extension HKUnit {
  @discardableResult
  class func jouleUnit(with prefix: HKMetricPrefix) -> Self
  @discardableResult
  class func joule() -> Self
  @discardableResult
  class func calorie() -> Self
  @discardableResult
  class func kilocalorie() -> Self
}
extension HKUnit {
  @discardableResult
  class func degreeCelsius() -> Self
  @discardableResult
  class func degreeFahrenheit() -> Self
  @discardableResult
  class func kelvin() -> Self
}
extension HKUnit {
  @discardableResult
  class func siemenUnit(with prefix: HKMetricPrefix) -> Self
  @discardableResult
  class func siemen() -> Self
}
extension HKUnit {
  @discardableResult
  class func count() -> Self
  @discardableResult
  class func percent() -> Self
}
extension HKUnit {
  @discardableResult
  func unitMultiplied(by unit: HKUnit) -> HKUnit
  @discardableResult
  func unitDivided(by unit: HKUnit) -> HKUnit
  @discardableResult
  func unitRaised(toPower power: Int) -> HKUnit
  @discardableResult
  func reciprocal() -> HKUnit
}
var HKUnitMolarMassBloodGlucose: Double { get }
