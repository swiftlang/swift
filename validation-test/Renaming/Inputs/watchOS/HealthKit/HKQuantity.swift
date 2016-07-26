
@available(watchOS 2.0, *)
class HKQuantity : NSObject, NSSecureCoding, NSCopying {
  convenience init(unit unit: HKUnit, doubleValue value: Double)
  @discardableResult
  func isCompatibleWith(_ unit: HKUnit) -> Bool
  @discardableResult
  func doubleValue(for unit: HKUnit) -> Double
  @discardableResult
  func compare(_ quantity: HKQuantity) -> NSComparisonResult
}
