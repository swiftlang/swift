
@available(tvOS 8.0, *)
enum NSMassFormatterUnit : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case gram
  case kilogram
  case ounce
  case pound
  case stone
}
@available(tvOS 8.0, *)
class NSMassFormatter : NSFormatter {
  @NSCopying var numberFormatter: NSNumberFormatter!
  var unitStyle: NSFormattingUnitStyle
  var isForPersonMassUse: Bool
  @discardableResult
  func string(fromValue value: Double, unit unit: NSMassFormatterUnit) -> String
  @discardableResult
  func string(fromKilograms numberInKilograms: Double) -> String
  @discardableResult
  func unitString(fromValue value: Double, unit unit: NSMassFormatterUnit) -> String
  @discardableResult
  func unitString(fromKilograms numberInKilograms: Double, usedUnit unitp: UnsafeMutablePointer<NSMassFormatterUnit>?) -> String
}
