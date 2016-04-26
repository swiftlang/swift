
@available(watchOS 2.0, *)
enum NSEnergyFormatterUnit : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case joule
  case kilojoule
  case calorie
  case kilocalorie
}
@available(watchOS 2.0, *)
class NSEnergyFormatter : NSFormatter {
  @NSCopying var numberFormatter: NSNumberFormatter!
  var unitStyle: NSFormattingUnitStyle
  var isForFoodEnergyUse: Bool
  @discardableResult
  func string(fromValue value: Double, unit unit: NSEnergyFormatterUnit) -> String
  @discardableResult
  func string(fromJoules numberInJoules: Double) -> String
  @discardableResult
  func unitString(fromValue value: Double, unit unit: NSEnergyFormatterUnit) -> String
  @discardableResult
  func unitString(fromJoules numberInJoules: Double, usedUnit unitp: UnsafeMutablePointer<NSEnergyFormatterUnit>?) -> String
}
