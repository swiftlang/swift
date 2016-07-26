
@available(watchOS 2.0, *)
class HKObjectType : NSObject, NSSecureCoding, NSCopying {
  var identifier: String { get }
  @discardableResult
  class func quantityType(forIdentifier identifier: String) -> HKQuantityType?
  @discardableResult
  class func categoryType(forIdentifier identifier: String) -> HKCategoryType?
  @discardableResult
  class func characteristicType(forIdentifier identifier: String) -> HKCharacteristicType?
  @discardableResult
  class func correlationType(forIdentifier identifier: String) -> HKCorrelationType?
  @discardableResult
  class func workoutType() -> HKWorkoutType
  @available(watchOS 2.2, *)
  @discardableResult
  class func activitySummaryType() -> HKActivitySummaryType
}
@available(watchOS 2.0, *)
class HKCharacteristicType : HKObjectType {
}
@available(watchOS 2.0, *)
class HKSampleType : HKObjectType {
}
@available(watchOS 2.0, *)
class HKCategoryType : HKSampleType {
}
@available(watchOS 2.0, *)
class HKCorrelationType : HKSampleType {
}
@available(watchOS 2.0, *)
enum HKQuantityAggregationStyle : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case cumulative
  case discrete
}
@available(watchOS 2.0, *)
class HKQuantityType : HKSampleType {
  var aggregationStyle: HKQuantityAggregationStyle { get }
  @discardableResult
  func isCompatibleWith(_ unit: HKUnit) -> Bool
}
@available(watchOS 2.0, *)
class HKWorkoutType : HKSampleType {
}
@available(watchOS 2.2, *)
class HKActivitySummaryType : HKObjectType {
}
