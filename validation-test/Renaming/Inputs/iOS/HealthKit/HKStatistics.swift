
@available(iOS 8.0, *)
struct HKStatisticsOptions : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var separateBySource: HKStatisticsOptions { get }
  static var discreteAverage: HKStatisticsOptions { get }
  static var discreteMin: HKStatisticsOptions { get }
  static var discreteMax: HKStatisticsOptions { get }
  static var cumulativeSum: HKStatisticsOptions { get }
}
@available(iOS 8.0, *)
class HKStatistics : NSObject, NSSecureCoding, NSCopying {
  var quantityType: HKQuantityType { get }
  var startDate: NSDate { get }
  var endDate: NSDate { get }
  var sources: [HKSource]? { get }
  @discardableResult
  func averageQuantity(for source: HKSource) -> HKQuantity?
  @discardableResult
  func averageQuantity() -> HKQuantity?
  @discardableResult
  func minimumQuantity(for source: HKSource) -> HKQuantity?
  @discardableResult
  func minimumQuantity() -> HKQuantity?
  @discardableResult
  func maximumQuantity(for source: HKSource) -> HKQuantity?
  @discardableResult
  func maximumQuantity() -> HKQuantity?
  @discardableResult
  func sumQuantity(for source: HKSource) -> HKQuantity?
  @discardableResult
  func sumQuantity() -> HKQuantity?
}
