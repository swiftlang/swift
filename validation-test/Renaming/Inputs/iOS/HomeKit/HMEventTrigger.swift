
@available(iOS 9.0, *)
let HMSignificantEventSunrise: String
@available(iOS 9.0, *)
let HMSignificantEventSunset: String
@available(iOS 9.0, *)
let HMCharacteristicKeyPath: String
@available(iOS 9.0, *)
let HMCharacteristicValueKeyPath: String
@available(iOS 9.0, *)
class HMEventTrigger : HMTrigger {
  init(name name: String, events events: [HMEvent], predicate predicate: NSPredicate?)
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
  func addEvent(_ event: HMEvent, completionHandler completion: (NSError?) -> Void)
  func removeEvent(_ event: HMEvent, completionHandler completion: (NSError?) -> Void)
  func updatePredicate(_ predicate: NSPredicate?, completionHandler completion: (NSError?) -> Void)
}
