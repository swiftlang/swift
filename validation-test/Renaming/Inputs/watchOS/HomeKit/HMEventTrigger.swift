
@available(watchOS 20000, *)
let HMSignificantEventSunrise: String
@available(watchOS 20000, *)
let HMSignificantEventSunset: String
@available(watchOS 20000, *)
let HMCharacteristicKeyPath: String
@available(watchOS 20000, *)
let HMCharacteristicValueKeyPath: String
@available(watchOS 20000, *)
class HMEventTrigger : HMTrigger {
  var events: [HMEvent] { get }
  @NSCopying var predicate: NSPredicate? { get }
  @discardableResult
  class func predicateForEvaluatingTriggerOccurring(beforeSignificantEvent significantEvent: String, applyingOffset offset: NSDateComponents?) -> NSPredicate
  @discardableResult
  class func predicateForEvaluatingTriggerOccurring(afterSignificantEvent significantEvent: String, applyingOffset offset: NSDateComponents?) -> NSPredicate
  @discardableResult
  class func predicateForEvaluatingTriggerOccurringBeforeDate(with dateComponents: NSDateComponents) -> NSPredicate
  @discardableResult
  class func predicateForEvaluatingTriggerOccurringOnDate(with dateComponents: NSDateComponents) -> NSPredicate
  @discardableResult
  class func predicateForEvaluatingTriggerOccurringAfterDate(with dateComponents: NSDateComponents) -> NSPredicate
  @discardableResult
  class func predicateForEvaluatingTrigger(with characteristic: HMCharacteristic, relatedBy operatorType: NSPredicateOperatorType, toValue value: AnyObject) -> NSPredicate
}
